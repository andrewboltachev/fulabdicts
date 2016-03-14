(ns fulabdicts.main
  (:require
    [clojure.core.async :as async :refer [<! >! timeout chan alt! alts! go go-loop put!
                                          sliding-buffer
                                          ]]
    [clojure.java.io :as io]
    [clojure-watch.core :refer [start-watch]]
    )
  )

#_(def input-file-ch (chan
         (sliding-buffer 1)
         ))

(defmacro watch-file-for-changes-depreciated [filename output-channel]
  `(do
  (def file-ch#
(chan
    (sliding-buffer 1)
  )
    )
  (let [~'file (io/file ~filename)]

(go-loop [~'old-filename nil]
         (let [[[~'event ~'filename :as ~'v] ~'port] (alts! [file-ch#
                                (timeout 200)
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
                               (put! file-ch# [~'event ~'filename])
                             )
                             )
                 :options {:recursive false}}])
    )
     )
  )

(defn -main [& [input-file params-file :as args]]
  ; check exists (.exists (io/file "filename.txt"))
  (watch-file-for-changes-depreciated
    input-file
    (chan)
    )
  )
