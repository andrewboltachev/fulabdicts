(ns fulabdicts.tokenizers)


(def mhr-rus-01-tokenizer
[
:transform-tags-fn (fn [{:keys [tag value] :as arg}]
                     (do #_(println arg)
                         (
                          ;ifipp
                          identity
                     [
                       (if (= tag "trn")
                     (let [
                           fv (first value)
                           fvs (string? fv)
                           m1 (if fvs (re-matches #"^(\d+)\.(?: )?(.*)$" fv))
                           m2 (if fvs (re-matches #"^(\d+)\u0029(?: )?(.*)$" fv))
                           ]
                     (cond
                       (some? m1)
                       (do #_(println "m1" (-> m1 second Integer.))
                       {:tag "trn1" :value {:number (-> m1 second Integer.) :body (vec (concat
                                                                                    (if-not (empty? (last m1)) [(last m1)])
                                                                                    (rest value)))}}
                           )
                       (some? m2)
                       (do #_(println "m2" (-> m2 second Integer.))
                       {:tag "trn2" :value {:number (-> m2 second Integer.) :body (vec (concat
                                                                                    (if-not (empty? (last m2)) [(last m2)])
                                                                                    (rest value)))}}
                           )
                       :else
                        {:tag tag :value value}
                        )
                     )
                        {:tag tag :value value}
                         )
                         ])))
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
                                            {:tag "L" :value x}

                                            :else
                                            x
                                            )
                                          ))
    ])

(def
  data
  {
   "mhr-rus-01"
   mhr-rus-01-tokenizer
   
   "mhr-rus-02"
   mhr-rus-01-tokenizer
   
   "mhr-rus-03"
   mhr-rus-01-tokenizer
   
   "rus-01" [
        :tag-compare-fn
      #(if-not
       (or (= %1 %2) (and (= %1 "m1") (= %2 "m")))
       {:error :tags-mismatch :context [%1 %2]}
       )
:transform-tags-fn (fn [{:keys [tag value] :as arg}]
                     (do #_(println arg)
                         (
                          ;ifipp
                          identity
                     [
                       (if (= tag "trn")
                     (let [
                           fv (first value)
                           fvs (string? fv)
                           m1 (if fvs (re-matches #"^(\d+)\.(?: )?(.*)$" fv))
                           m2 (if fvs (re-matches #"^(\d+)\u0029(?: )?(.*)$" fv))
                           ]
                     (cond
                       (some? m1)
                       (do #_(println "m1" (-> m1 second Integer.))
                       {:tag "trn1" :value {:number (-> m1 second Integer.) :body (vec (concat
                                                                                    (if-not (empty? (last m1)) [(last m1)])
                                                                                    (rest value)))}}
                           )
                       (some? m2)
                       (do #_(println "m2" (-> m2 second Integer.))
                       {:tag "trn2" :value {:number (-> m2 second Integer.) :body (vec (concat
                                                                                    (if-not (empty? (last m2)) [(last m2)])
                                                                                    (rest value)))}}
                           )
                       :else
                        {:tag tag :value value}
                        )
                     )
                        {:tag tag :value value}
                         )
                         ])))

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
                                            {:tag "L" :value x}

                                            :else
                                            x
                                            )
                                          ))


             ]
   "udm-rus-01"
   [
    :transform-tags-fn (fn [{:keys [tag value] :as arg}]
                     (do #_(println arg)
                         (
                          ;ifipp
                          identity
                     [
                       (cond (= tag "trn")
                     (let [
                           fv (first value)
                           fvs (string? fv)
                           m1 (if fvs (re-matches #"^(\d+)\.(?: )?(.*)$" fv))
                           m2 (if fvs (re-matches #"^(\d+)\u0029(?: )?(.*)$" fv))
                           ]
                     (cond
                       (some? m1)
                       (do #_(println "m1" (-> m1 second Integer.))
                       {:tag "trn1" :value {:number (-> m1 second Integer.) :body (vec (concat
                                                                                    (if-not (empty? (last m1)) [(last m1)])
                                                                                    (rest value)))}}
                           )
                       (some? m2)
                       (do #_(println "m2" (-> m2 second Integer.))
                       {:tag "trn2" :value {:number (-> m2 second Integer.) :body (vec (concat
                                                                                    (if-not (empty? (last m2)) [(last m2)])
                                                                                    (rest value)))}}
                           )
                       :else
                        {:tag tag :value value}
                        )
                     )
                             #_(and
                               (= tag "i")
                               (= value ["употребляется лишь в составе выражений:"]))
                             #_{:tag "ii"
                              :value value}
                             :else
                        {:tag tag :value value}
                         )
                         ])))
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
                                            {:tag "L" :value x}

                                            :else
                                            x
                                            )
                                          ))
    ]
   })
