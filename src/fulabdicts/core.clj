(ns fulabdicts.core
  (:use
   [clojure.test :refer [deftest run-tests is]]
   )
  )

(defn main
  "I don't do a whole lot."
  []
  (println "Hello, World!")
  (run-tests 'fulabdicts.core)
  )

(deftest test1
  (is (= (+ 2 3) 5)))
