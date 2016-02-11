(ns fulabdicts.main
  (:require
    [clojure.core.async :as async :refer [<! >! timeout chan alt! alts! go go-loop put!
                                          sliding-buffer
                                          ]]
    [clojure.java.io :as io]
    [clojure-watch.core :refer [start-watch]]
    )
  )

(def c (chan
         (sliding-buffer 10)
         ))

(go-loop []
         (let [v (<! c)]
           (println v (quot (System/currentTimeMillis) 1000)
                    )
           )
         (recur)
  )


(defn -main [& [input-file params-file :as args]]
  ; check exists (.exists (io/file "filename.txt"))
  (let [file (io/file input-file)]
  (start-watch [{:path (.getParent file)
                 :event-types [:create :modify :delete]
                 :bootstrap (fn [path] (println "Starting to watch " path))
                 :callback (fn [event filename]
                             (when (= (.getName file) (.getName (io/file filename)))
                               (put! c [event filename])
                             )
                             )
                 :options {:recursive true}}])
    )
  )
