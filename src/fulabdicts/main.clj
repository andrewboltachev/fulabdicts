(ns fulabdicts.main
  (:require
    [clojure.core.async :as async :refer [<! >! timeout chan alt! alts! go go-loop put!
                                          sliding-buffer
                                          ]]
    [clojure.java.io :as io]
    [clojure-watch.core :refer [start-watch]]
    [fulab.zarnidict.fulabdsl.core :as fulabdsl]
    [regexpforobj.core :as regexpforobj]
    )
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
                  (println "File" (str \" ~'old-filename \") "changed")
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
                 :bootstrap (fn [~'path] (println "Starting to watch " ~'path))
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
                      (doseq [line (line-seq rdr)]
                        line
                        )
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
           (put! input-file-data-ch data)
           )
         (recur)
         )


(go-loop []
         (let [
               params-filename (<! params-file-ch)

               data  (try
                  (->
                    (slurp params-filename)
                    read-string
                    )
                 (catch Throwable e (do (println e)
                                        {} ; XXX: empty grammar
                                        ))
                       )
               ]
           (put! params-file-data-ch data)
           )
         (recur)
         )

(go-loop [prev-full-input nil
          prev-input nil
          prev-params nil
          result []
          ]
         (let [
               not-default (partial not= :default)
               [new-input input-port] (alts! [input-file-data-ch] :default prev-input :priority true) ; TODO probably should be one alts!
               [new-params params-port] (alts! [params-file-data-ch] :default prev-params :priority true)

               prev-full-input (if (not-default input-port) new-input prev-full-input)
               new-input (if (not-default params-port) prev-full-input prev-input)

               something-new (some not-default (hash-set input-port params-port) )
               result (if something-new [] result)

               article (first prev-input)
               [word body] article
               grammar-applied (regexpforobj/run new-params body)
               grammar-applied (if (regexpforobj/is_parsing_error? grammar-applied) grammar-applied [word grammar-applied])
               ]
           (if (empty? prev-input)
             (do
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
               (recur
                 prev-full-input
                 (rest new-input)
                 new-params
                 (conj result grammar-applied)
                 )
               (do
                 (println "Erorr")
                 (println word)
                 (println grammar-applied)
                 (recur
                   nil
                   nil
                   nil
                   []
                   )
                 )
               )
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
