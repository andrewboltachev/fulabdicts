(ns fulabdicts.structures.one)

(use 'regexpforobj.core)

(println "Hello from grammar!")

(def the-grammar
  (let [ref_ (Seq [
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
      #_translation
      #_(Seq [
              (Char "trn")
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
        f2 (fn [lst tail] (Or (reduce (fn [a b] (conj a (Or [(Seq [b tail])  (Star (Or a))]) )) [] (reverse lst))))
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
            (MayBe (Char "end"))
            (MayBe (Char "m1"))
            ])
          ]

           (f2
             [
              [
               (Char "trn1")
               (Char "trn2")
               (Char "trn")
               ]
              ]

              examples
             )

           ;; (Seq [trn examples])
           ;; (Seq [trn2 examples])
           ;; (Seq [trn2 (Or [examples (Star (Seq [trn examples])) ])])

           ;; f2 (fn [x y e] (Seq [x (Or (cons e (Star (Seq (cons y e)))))]))
           ;; (f2 trn1 trn2 (f2 trn2 trn [examples]))

           ;; trn2 examples
           ;; trn2 trn examples

         ;; trn1



         ])
        )
      ]
     ))
  )
