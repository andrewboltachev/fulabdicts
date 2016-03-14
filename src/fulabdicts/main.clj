(ns fulabdicts.main
  (:require
    [clojure.core.async :as async :refer [<! >! timeout chan alt! alts! go go-loop put!
                                          sliding-buffer
                                          ]]
    [clojure.java.io :as io]
    [clojure-watch.core :refer [start-watch]]
    )
  )

(def input-file-ch (chan
         (sliding-buffer 1)
         ))

(go-loop [standby true]
         (let [[v port] (alts! [input-file-ch
                                (timeout 200)
                                ]
                               :priority true
                               )]
           (when (not standby)
             #_(println
                 (str "!!!!!!!!!!!!!!!!!!!!!!!!!!!!" v port)
                 (System/currentTimeMillis)
               )
              (when
                  (nil? v)
                  (println "Input file changed")
                )
             )
           (recur (nil? v))
           )
  )


(defn -main [& [input-file params-file :as args]]
  ; check exists (.exists (io/file "filename.txt"))
  (let [file (io/file input-file)]
  (start-watch [{:path (.getParent file)
                 :event-types [:create :modify
                               ;:delete
                               ]
                 :bootstrap (fn [path] (println "Starting to watch " path))
                 :callback (fn [event filename]
                             (when (= (.getName file) (.getName (io/file filename)))
                               (put! input-file-ch [event filename])
                             )
                             )
                 :options {:recursive false}}])
    )
  )
