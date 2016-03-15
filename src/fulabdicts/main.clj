(ns fulabdicts.main
  (:require
    [clojure.core.async :as async :refer [<! >! timeout chan alt! alts! go go-loop put!
                                          sliding-buffer
                                          ]]
    [clojure.java.io :as io]
    [clojure-watch.core :refer [start-watch]]
    [fulab.zarnidict.fulabdsl.core :as fulabdsl]
    [regexpforobj.core :as regexpforobj]

    [io.aviso.ansi :as font]
    )
  )
(use 'aprint.core)

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

(defmacro watch-file-for-changes-depreciated [filename output-channel]
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
                  (put! ~output-channel ~'old-filename)
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
               filename (<! input-file-ch)
            data
                 (try
                  (->
                    (with-open [rdr (clojure.java.io/reader filename)]
                      (doall (line-seq rdr))
                      )

(fulabdsl/parse-fulabdsl-lines-short
        :tag-compare-fn
      #(if-not
       (or (= %1 %2) (and (= %1 "m1") (= %2 "m")))
       {:error :tags-mismatch :context [%1 %2]}
       )
:line-first-level-process-fn (comp list (fn [x]
                                          (cond
                                            (= x ", ")
                                            {:tag "COMMA" :value x}

                                            (contains? 
                                             (set (map (comp str char) (range (int \Ⅰ) (+ (int \Ⅰ) 16))))
                                              x)
                                            {:tag "R" :value x}

                                            (contains? 
                                             #{"А." "Б."}
                                              x)
                                            {:tag "R" :value x}

                                            :else
                                            x
                                            )
                                          ))
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
               params-filename (<! params-file-ch)

               data  (try
                       (let [s
                  (->
                    (slurp params-filename)
                    read-string
                    )]
(eval `(with-ns 'regexpforobj.core
  (eval ~s)
  ))
                         )
                 (catch Throwable e (do (println e)
                                        {} ; XXX: empty grammar
                                        ))
                       )
               ]
           (println "Sending grammar off the channel"
                    ;(prn2 data)
                    )
           (put! params-file-data-ch data)
           )
         (recur)
         )

#_(go-loop [prev-full-input nil
          prev-input nil
          prev-params nil
          result []
          ]
         (let [
               not-default (partial not= :default)
               ;_ (println "Will wait for input" (empty? prev-full-input))
               [new-input input-port] (if (empty? prev-full-input)
                                        (alts! [input-file-data-ch]) ; TODO probably should be one alts!
                                        (alts! [input-file-data-ch] :default prev-input :priority true)
                                        )
               ;_ (println "Input OK. Will wait for params" (empty? prev-params))
               [new-params params-port] (if (empty? prev-params)
                                          (alts! [params-file-data-ch])
                                          (alts! [params-file-data-ch] :default prev-params :priority true)
                                        )
               ;_ (println "Params OK")

               prev-full-input (if (not-default input-port) new-input prev-full-input)
               new-input (if (not-default params-port) prev-full-input prev-input)

               something-new (some not-default (hash-set input-port params-port))
               result (if something-new [] result)

               article (first new-input)
               [word body] article
               grammar-applied (regexpforobj/run new-params body)
               grammar-applied (if (regexpforobj/is_parsing_error? grammar-applied) grammar-applied [word grammar-applied])
               ]
           (if (empty? new-input)
             (do
               (println "empty prev-input")
               (when-not (empty? result)
                (put! result-ch result)
                 )
              (recur
                nil
                nil
                nil
                []
                )
               )
             (if-not (regexpforobj/is_parsing_error? grammar-applied)
               (do
                 (println font/bold-black-font word "OK")
                (recur
                  prev-full-input
                  (rest new-input)
                  new-params
                  (conj result grammar-applied)
                  )
                 )
               (do
                 (println font/bold-black-font word)
                 (println font/green-font grammar-applied font/reset-font)
                 (recur
                  prev-full-input
                  (rest new-input)
                  new-params
                  result
                   )
                 )
               )
             )
           )
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
      (println "result")

      :else
      (let
        [
         article (first current-input)
         [word body] article
         grammar-applied (regexpforobj/run params body)
         ]
        (if (regexpforobj/is_parsing_error? grammar-applied)
          (do
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
    #_(recur
      input
      params

      current-input
      result
      )
    )
  )


(go-loop []
         (let [result (<! result-ch)]
           (doseq [[word body] result]
             (println word)
             )
           )
         (recur)
         )

(defn -main [& [input-file params-file :as args]]
  ; check exists (.exists (io/file "filename.txt"))
  (let [
        ]
    (watch-file-for-changes-depreciated
      input-file
      input-file-ch
      )
    (watch-file-for-changes-depreciated
      params-file
      params-file-ch
      )
    )
  )
