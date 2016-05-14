(ns fulabdicts.main
  (:require
    [clojure.core.async :as async :refer [<! >! timeout chan alt! alts! go go-loop put!
                                          sliding-buffer
                                          ]]
    [clojure.java.io :as io]
    [clojure-watch.core :refer [start-watch]]
    [fulab.zarnidict.fulabdsl.core :as fulabdsl]
    [regexpforobj.core :as regexpforobj]
    [regexpforobj.main :as regexpforobj-main]

    [io.aviso.ansi :as font]

    ;[clj-http.client :as client]
    )
  )
(use 'aprint.core)
(require '[fipp.edn :refer (pprint) :rename {pprint fipp}])
(require 'fulabdicts.tokenizers)
(require 'fulabdicts.grammars) ; TODO: split file


(defn ifipp [x]
  (do
    (fipp x)
    x)
  )

(defmacro with-ns
  "Evaluates body in another namespace.  ns is either a namespace
  object or a symbol.  This makes it possible to define functions in
  namespaces other than the current one."
  [ns & body]
  `(binding [*ns* (the-ns ~ns)]
     ~@(map (fn [form] `(eval '~form)) body)))

(defn truncate
  [s n]
  (apply str (take n s)))

(defn prn2 [data]
  (str (truncate (prn-str data) 150) "... (" (-> data class str) ")")
  )

(defmacro watch-file-for-changes-depreciated [filename output-channel & [payload]]
  `(do
  (let [~'file-ch (chan
    (sliding-buffer 1)
  )
        ~'file (io/file ~filename)]
    (do
#_(println "go-loop" filename)
(go-loop [~'old-filename nil]
         (let [[[~'event ~'filename :as ~'v] ~'port] (alts! [~'file-ch
                                (timeout 500)
                                ]
                               :priority true
                               )]
           (when-not (nil? ~'old-filename)
             #_(println
                 (str "!!!!!!!!!!!!!!!!!!!!!!!!!!!!" ~'v ~'port)
                 (System/currentTimeMillis)
               )
              (when
                  (nil? ~'v)
                  (println font/blue-font "File" (str \" ~'old-filename \") "changed" font/reset-font)
                  (put! ~output-channel [~'old-filename ~payload])
                )
               )
             (recur ~'filename)
             )
           )


  (put! ~'file-ch [:initial ~filename])
  (start-watch [{:path (.getParent ~'file)
                 :event-types [:create :modify
                               ;:delete
                               ]
                 :bootstrap (fn [~'path] (println font/blue-font "Starting to watch " ~'path font/reset-font))
                 :callback (fn [~'event ~'filename]
                             (when (= (.getName ~'file) (.getName (io/file ~'filename)))
                               (put! ~'file-ch [~'event ~'filename])
                             )
                             )
                 :options {:recursive false}}])
    )
     ))
  )

(def input-file-ch (chan))
(def params-file-ch (chan))

(def input-file-data-ch (chan))
(def params-file-data-ch (chan))

(def result-ch (chan))

(go-loop []
         (let [
               [filename structure-name] (<! input-file-ch)
            data
                 (try
                  (let [data
                    (with-open [rdr (clojure.java.io/reader filename)]
                      (doall (line-seq rdr))
                      )]

                    (apply
                      fulabdsl/parse-fulabdsl-lines-short
                      data
                      (get fulabdicts.tokenizers/data structure-name)
                      )

                    )
                 (catch Throwable e (do (println e)
                                        [] ; XXX: empty data
                                        ))
                 )
               ]
           (println "Sending data off the channel"
                    ;(prn2 data)
                    )
           (put! input-file-data-ch data)
           )
         (recur)
         )


(go-loop []
         (let [
               [params-filename structure-name] (<! params-file-ch)

               data (do
                      (require 'fulabdicts.grammars :reload)
                      (get fulabdicts.grammars/data structure-name)
                   )
               
               ]
           (println "Sending grammar off the channel"
                    (prn2 data)
                    )
           (put! params-file-data-ch data)
           )
         (recur)
         )

(go-loop
  [
   input nil
   params nil

   current-input nil
   result []

   await-for-changes false
   ]
  (let
    [
     [value port]
     (if (or (empty? input) (empty? params) await-for-changes)
       (do
         ;(println "waiting for changes")
         ;(println input)
         ;(println params)
         (alts! [input-file-data-ch params-file-data-ch])
         )
       (do
         ;(println "just checking if there's something new")
         (alts! [input-file-data-ch params-file-data-ch] :default nil)
         )
       )
     ]
    (cond
      (= port input-file-data-ch)
      (recur
        value
        params

        value
        []
        false
        )

      (= port params-file-data-ch)
      (recur
        input
        value

        input
        []
        false
        )

      (empty? current-input)
      (let [result (map #(update-in % [1] regexpforobj/grammar_pretty) result)]
        (println "Result")
        (put! result-ch result)
        )

      :else
      (let
        [
         _ (println
             "will apply" 
             )
         _ (when (regexpforobj/is_parsing_error? current-input)
             (println font/bold-red-font "Tokenizer error" font/reset-font)
              (aprint current-input)
             )
         article (first current-input)
         [word body] article
         
         grammar-applied 
         (binding [regexpforobj/*regexpforobj-debug1* false]
         (time (regexpforobj/run params body))
           )
         ]
        (if (regexpforobj/is_parsing_error? grammar-applied)
          (do
            _ (clojure.pprint/pprint
                (vec body)
                )
            (println font/bold-red-font word "Error")
            (aprint (update-in grammar-applied [:context :tail] regexpforobj/grammar_pretty))
            (recur
              input
              params
  
              input
              []
              true
              )
            )
          (do
            (println font/bold-black-font word "OK" font/reset-font)
            (recur
              input
              params

              (rest current-input)
              (conj result [word grammar-applied])
              false
              )
            )
          )
        )

      )
    )
  )

; (aprint (grammar_pretty (Or (reduce (fn [a b] (concat a (cons (Seq [ b examples ])  (map #(Star (Seq [b %])) a) ))) [] (reverse [trn1 trn2 trn])))))


(go-loop []
         (let [result (<! result-ch)]
           (doseq [[word body] result]
             (println word)
             )
           )
         (recur)
         )

(defn -main [& [input-file structure-name :as args]]
  ; check exists (.exists (io/file "filename.txt"))
  (let [
        ]
    (watch-file-for-changes-depreciated
      input-file
      input-file-ch
      structure-name
      )
    (watch-file-for-changes-depreciated
      "src/fulabdicts/grammars.clj"
      params-file-ch
      structure-name
      )
    )
  )
