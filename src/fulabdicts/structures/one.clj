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

         ])
        )
      ]
     ))
  )
