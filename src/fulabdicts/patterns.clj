(ns fulabdicts.patterns)


(def x1
  '(Seq
     (MayBe (Char "pre"))
     (Star
       (Seq [
         (Char "trn")
        ]
       )
    ))
  )


'(
  
  x
  
  (Or [x
       (Plus (Seq
               [tag
                x]
               ))
       ])
  )
