(ns fulabdicts.main
  (:require
    [clojure.core.async :as async :refer [<! >! timeout chan alt! alts! go go-loop put!
                                          sliding-buffer
                                          ]]
    [clojure.java.io :as io]
    [clojure-watch.core :refer [start-watch]]
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

(defn -main [& [input-file params-file :as args]]
  ; check exists (.exists (io/file "filename.txt"))
  (let [input-file-ch (chan)
        params-file-ch (chan)
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
