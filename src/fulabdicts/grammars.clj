(ns fulabdicts.grammars)
(use 'regexpforobj.core)

(def
  data
  {
   "mhr-rus-01"
    (let [MayBe Star
          Plus (fn [x & [p]]
                (Seq [x (Star x)]
                  (assoc
                    (or p {})
                    :plus
                    true)
                      )
                )
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
                            ]))
          (MayBe (Seq [
                (Char "ex")
                ref_
                      (Star (Seq [
                              (Char "COMMA")
                              ref_
                              ]))
                ]))
              ])

        f1 (fn [header middle body]
            (Or [
                  (Seq (filter some? [
                    middle
                    body
                    ]))
              (Star (Seq (filter some? [
                          header
                          middle
                          body
                    ])))
                  ])
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
              (Seq
                [
                (MayBe (Char "pre"))
                ])
              ]


          ;; R 
          [
            (Char "R")
            (Seq
              [
              (MayBe (Char "pre"))
              (MayBe (Char "end"))
              (MayBe (Char "m1"))
              ])
            ]

            (Or
                [
                (Seq [
                      (MayBe (Char "trn") :tsar)
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

                (MayBe (Char "trn") :tsar)

                (Plus (Seq [(Char "trn2")
                            
                            (Or [examples
                                        (Star (Seq [(Char "trn") examples]))
                                        ])

                            ]))
                      ])

                (Star (Seq [(Char "trn") examples]))

                (Seq [examples])
                ]

                
              :trns)

          ])
          )
        ]
      ))
   })
