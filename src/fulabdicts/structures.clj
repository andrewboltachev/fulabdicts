(ns fulabdicts.structures)

(use '[regexpforobj.core])
          
          
(def structures
  {"foo" [
      :line-first-level-process-fn (comp list #(do {:tag "text" :value %}))
      :transform-tags-fn (fn [x]
                           (let [v (:value x)
                                 fv (first v)]
                            (if (and
                                  (= (:tag x) "trn")
                                  (not (empty? v))
                                  (string? fv)
                                  )
                              (reduce #(do
                                        (println "one and two" %1 %2)
                                        (or %1 %2))
                                      (identity (reverse (cons [x] (map (fn [[tag-name e]]
                                             (if-let [n (re-find e fv)]
                                               [{:tag tag-name
                                                 :value n
                                                 }
                                                {:tag "trn"
                                                 :value (subs fv (count n))
                                                 }]
                                               ))
                                           {"N" #"^\d+\. " "P" #"^\d+\u0029 "}))))
                                      )
                               [x]
                             )
                             )
                           )
      :grammar (Or [
                    (Star (Seq [(Star (Char "N"))
                                (Star (Char "P"))
                                  (Char "trn") (Star (Seq [(Char "lang1"
                                                                 
                                                                 ) (Char "lang2")]
                                                          
                                                          {:fn1 (fn [x]
                                                                 (str
                                                                 (str "<span>" (-> x :value first :value) "</span>")
                                                                 (str "<span>" (-> x :value second :value) "</span>")
                                                                   )
                                                                 )}
                                                          ) {:fn1 (comp (partial reduce str) :value)})] {:fn1 :value}))
                    (Char "text")] {:fn1 :value})
      ]}
  )
