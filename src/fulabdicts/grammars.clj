(ns fulabdicts.grammars)
(use 'regexpforobj.core)
(use 'com.rpl.specter)

(def
  data
  {
   "mhr-rus-01"
    (let [;MayBe Star
          #_Plus #_(fn [x & [p]]
                (Seq [x (Star x)]
                  (comp
                    (or p identity)
                    (fn [x]
                      {:type :SeqNode
                       :value (cons
                        (get-in x [:value 0])
                        (vec (get-in x [:value 1 :value]))
                        )}
                      )
                    )
                     )
                )
          ; ...
          Seq-of-MayBe (fn [items]
                         (Seq
                  (mapv (fn [item]
                         (MayBe
                           item
                           (fn [{:keys [[v :as value]] :as x}]
                             {(-> item :value keyword) v
                              })
                           )
                         )
                       items)
                         (comp
                           #(apply merge %)
                           :value)
                           )
                           ; ...
                           )

          ; ...
          ref_ (Seq [
                  (MayBe (Char "u1") (fn [x] {:u1 x}))
                  (Seq [(Char "ref")] (fn [x] {:ref (first x)}) )
                  (MayBe (Char "u") (fn [x] {:u x}))
                  ]
                    (comp
                           #(apply merge %)
                           :value)
                    )

        examples
          (Seq [
                (Star (Seq [
                            (Char "mhr")
                            (MayBe (Char "aut"))
                            (Char "rus")
                            ]
                           (fn [x]
                             (let [length3 (= (count (:value x)) 3)]
                             {:mhr (get-in x [:value 0 :payload])
                              :aut (when length3 (get-in x [:value 1 :value 0 :payload]))
                              :rus (get-in x [:value (if length3 2 1) :payload])
                              })
                             )
                           )
                      (comp
                        #(do {:examples %})
                        :value
                        )
                      )
          (MayBe (Seq [
                (Char "ex")
                ref_
                      (Star (Seq [
                              (Char "COMMA")
                              ref_
                              ]
                                 #(-> % :value second)
                                 ))
                ])
                 (fn [x] {:refs
                          (:value x)
                          }))
              ]
               (comp #(apply merge %) :value))

          trn-examples (Star
                         (Seq [(Char "trn") examples]
                              (fn [x]
                                (merge {:trn (-> x :value first)}
                                       (-> x :value second)
                                 )
                                )
                              )
                         (fn [x]
                           {:type :trn
                            :items (:value x)
                            }
                           )
                         )

        f1 (fn [header middle body]
            (Or [
                  (Seq (filter some? [
                    middle
                    body
                    ]
                               )
                               (fn [x]
                     (merge {
                      :items (-> x :value (get 1))
                      } (-> x :value (get 0)))
                     )
                       )
              (Star (Seq (filter some? [
                          header
                          middle
                          body
                    ])
                   (fn [x]
                     (merge {:header (-> x :value (get 0) :payload)
                      :items (-> x :value (get 2))
                      } (-> x :value (get 1)))
                     )
                                 
                         )
                    (fn [x]
                    {:value (:value x)
                     :type (keyword (:value header))
                     }
                      )
                    )
                  ]
                (comp :value)
                )
            )
          #_f2 #_(fn [lst tail] (Or (reduce (fn [a b] (conj a (Or [(Seq [b tail])  (Star (Or a))]) )) [] (reverse lst))))
        ]
        (reduce
          #(apply f1 (conj %2 %1))
          (reverse
            [
          ;; L
            [
              (Char "L")
              (Seq-of-MayBe
                [
                (Char "pre")
                ])
              ]


          ;; R 
          [
            (Char "R")
            (Seq-of-MayBe
              [
              (Char "pre")
              (Char "end")
              (Char "m1")
              ])
            ]

            (Or
                [
                (Seq [
                      (MayBe (Char "trn") ;:tsar
                             )
                (Plus (Seq [(Char "trn1")
                             (Or [examples
                              trn-examples
                              (Star (Seq [(Char "trn2")
                                  (Or [examples
                                              trn-examples
                                              ] :value)
                                  ])
                                    :value)
                                  ] :value)
                            
                            ]
                           (fn [x]
                             {:trn1 (-> x :value first :payload)
                              :items (-> x :value second)
                              }
                             )
                           ) :value)
                      ]
                     (fn [x]
                       {:trn1-with-tsar (-> x :value first :value first :payload)
                        :translations (-> x :value second)
                        }
                       )
                     )

                (Seq [

                (MayBe (Char "trn") ;:tsar
                       )

                (Plus (Seq [(Char "trn2")
                            
                            (Or [examples
                                        trn-examples
                                        ] :value)

                            ]) :value)
                      ]
                     (fn [x]
                       {:trn2-with-tsar (-> x :value first :value first :payload)
                        :translations (-> x :value second)
                        }
                       )
                     )

                trn-examples

                examples
                ]

                
              ;:trns
              )

          ])
          )
        
      )
     "mhr-rus-02"
      (let [first-char (fn [{:keys [value]}] (-> value first :payload))
            refseq1 (Seq [
                          (MayBe (Char "u1"))
                          (Char "ref")
                          (MayBe (Char "u"))
                          ])]
      (Star (Or [
                 (Seq [(Char "pre")] {:type :pre
                                      :fn first-char})
                 (Seq [(Char "end")] {:type :end
                                      :fn first-char})
                 (Seq [
                       (Char "L")
                       (Char "pre")
                       ] {:type :L
                          :fn (fn [{:keys [value]}]
                                {:L (get-in value [0 :payload])
                                 :pre (get-in value [1 :payload])})})
                 (Seq [
                       (Char "R")
                       (MayBe (Char "end"))
                       (MayBe (Char "m1"))
                       ] {:type :R
                          :fn (fn [{:keys [value]}]
                                {:R (get-in value [0 :payload])
                                 :end (get-in value [1 :value 0 :payload])
                                 :m1 (get-in value [2 :value 0 :payload])})})
                 (Seq [(Char "m1")] {:type :m1 :fn first-char})
                 (Seq [(Char "trn")] {:type :trn :fn first-char})
                 (Seq [(Char "trn1")] {:type :trn1 :fn first-char})
                 (Seq [(Char "trn2")] {:type :trn2 :fn first-char})
                 (Seq [(Char "mhr") (MayBe (Char "aut")) (Char "rus")]
                      {:type :example
                       :fn (fn [{:keys [value]}]
                             {:lang1 (:payload (first (filter #(= (:value %) "mhr") value)))
                              :aut (:payload (first (:value (first (filter #(= (:value %) "aut") value)))))
                              :lang2 (:payload (first (filter #(= (:value %) "rus") value)))
                              :phraseologism false
                              }
                             )})
                 (Seq [(Char "ex")
                       (Plus
                         (Seq [
                               refseq1
                               (Star (Seq [
                                           (Char "COMMA")
                                           refseq1
                                      ]))
                               ])
                         )
                       ] {:type :ex})
                 ])
            {:fn
             (fn [{:keys [value]}] (mapv :value value))
             })) 


      "mhr-rus-03"
      (let [first-char (fn [{:keys [value]}] (-> value first :payload))
            refseq1 (Seq [
                          (MayBe (Char "u1"))
                          (Char "ref")
                          (MayBe (Char "u"))
                          ])]





        (Seq [(Char "pre")
              (Char "trn1")])
      
        
        
        
        
        
        
        
        
        
        
        
        
        
        )






      "rus-01"
      (Star
        (Or
          [
           (Char "pre")
           (Char "end")
           (Char "m1")
           (Char "R")
           (Char "trn")
           (Char "trn2")

           (Char "ex")
           (Char "ref")
           (Char "COMMA")
           (Char "u")
           (Char "braces1")

           (Char "i")

           (Char "rus")
           ])
        )
      "udm-rus-01"
      #_(Star
        (Or
          [
           (Char "i")
           (Char "pre")
           (Char "m1")
           (Char "R")
           (Char "trn2")
           (Char "udm")
           (Char "rus")
           (Char "trn")
           ])
        )
        (let [examples (Star (Seq [
                                              (Char "udm")
                                              (Char "rus")
                                              ]))
              
              s1 (fn [h x]
                   (let [h (if (sequential? h) h [h])]
                   (Or [
                        x
                        (Star (Seq (conj h x)))
                        ])
                   ))
              
              MayBe2 (fn [x r]
                       (Or
                         [r
                          (Seq [x r])]))              
              ]
          
          (Seq [
            (MayBe (Char "i"))
            (MayBe (Char "pre"))
             (s1
               [(Char "R")]
                (Star (Seq [
                            (MayBe (Char "pre"))
                            (MayBe (Char "m1"))
                            (Or [
                                (Star (Seq [
                                  (Char "trn2")
                                  examples
                                  ]))
                                  (Seq
                                    [(Or [(Char "trn") (Char "i")])
                                     examples
                                     ])
                                    examples
                                ])
                            ]))
              )]))
   })
