(ns fulabdicts.structures.one)

(use 'regexpforobj.core)

(println "Hello from grammar!")

(def the-grammar
  (let [ref_ (Seq [
                 (Char "ref")
                 (MayBe (Char "u"))
                 ])

      translation
      (Seq [
              (Char "trn")
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
                (Seq [
                  middle
                  body
                  ])
             (Star (Seq [
                         header
                         middle
                         body
                   ]))
                ])
           )
      ]
(Seq [
      (f1
        (Char "L")
          (Seq
            [
            (MayBe (Char "pre"))
            ])
        (f1
          (Char "R")
          (Seq
            [
            (MayBe (Char "end"))
            (MayBe (Char "m1"))
            ])
          (Star
            translation
            )
          )
        )
      ]
     ))
  )
