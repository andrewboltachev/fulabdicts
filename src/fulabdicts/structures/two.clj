(ns fulabdicts.structures.two)

(use 'regexpforobj.core)
(require '[fipp.edn :refer (pprint) :rename {pprint fipp}])


(println "Hello Peter!")

(def the-grammar
  (let [ref_ (Seq [
                 (Char "ref")
                 (MayBe (Char "u"))
                 ])
        ex (Seq [
              (Char "ex")
              ref_
                     (Star (Seq [
                            (Char "COMMA")
                            ref_
                            ]))
              ])

      examples
        (Seq [
              (Star (Seq [
                          (Char "mhr")
                          (MayBe (Char "aut"))
                          (Char "rus")
                          ]))
        (MayBe ex)
             ])
      ]
(Star (Or [
         ;; L
          (Char "L")
          (Char "pre")


         ;; R 
          (Char "R")
          (Char "end")
           (Char "m1")


            ;; trnN
            (Char "trn1")
            (Char "trn2")

            (Star (Seq [(Char "trn") examples]))


      ]
     ))
    (Seq [])
  ))
