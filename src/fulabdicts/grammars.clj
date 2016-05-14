(ns fulabdicts.grammars)
(use 'regexpforobj.core)
(use 'com.rpl.specter)

(def
  data
  {
   "mhr-rus-01"
    (let [MayBe Star
          Plus (fn [x & [p]]
                (Seq [x (Star x)]
                  (comp
                    (or p identity)
                    (fn [x]
                      (conj
                        (get-in [:value 0] x)
                        (vec (get-in [:value 1 :value] x))
                        )
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
                  (MayBe (Char "u1"))
                  (Char "ref")
                  (MayBe (Char "u"))
                  ])

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
                      :value)
          (MayBe (Seq [
                (Char "ex")
                ref_
                      (Star (Seq [
                              (Char "COMMA")
                              ref_
                              ]))
                ])
                 :refs
                 )
              ])

        f1 (fn [header middle body]
            (Or [
                  (Seq (filter some? [
                    middle
                    body
                    ]))
              (Star (Seq (filter some? [
                          (dissoc header :payload)
                          middle
                          body
                    ])))
                  ]
                (comp :value)
                )
            )
          #_f2 #_(fn [lst tail] (Or (reduce (fn [a b] (conj a (Or [(Seq [b tail])  (Star (Or a))]) )) [] (reverse lst))))
        ]
  (Seq [
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
                              (Star (Seq [(Char "trn") examples]))
                              (Star (Seq [(Char "trn2")
                                  (Or [examples
                                              (Star (Seq [(Char "trn") examples]))
                                              ])
                                  ])
                                    )
                                  ])
                            
                            ]))
                      ])

                (Seq [

                (MayBe (Char "trn") ;:tsar
                       )

                (Plus (Seq [(Char "trn2")
                            
                            (Or [examples
                                        (Star (Seq [(Char "trn") examples]))
                                        ])

                            ]))
                      ])

                (Star (Seq [(Char "trn") examples]))

                (Seq [examples])
                ]

                
              ;:trns
              )

          ])
          )
        ]
      ))
   })
