(ns fulabdicts.structures)

(use '[regexpforobj.core])

(def g1_strange (let
 [s2-0
  (Seq
   [(MayBe (Char "pre"))
    (MayBe (Char "m1"))
    (Star (Char "end"))
    (MayBe (Char "m1"))])
  s3-0
  (Seq
   [s2-0
    (Char "trn")
    (Star
     (Or
      [(Seq
        [(Char "mhr") (Char "aut") (Char "rus")]
        "пример без автора")
       (Seq [(Char "mhr") (Char "rus")] "пример без автора")])
     "примеры")
    (Star
     (Seq
      [(Char "ex")
       (Seq
        [(Seq [(Char "ref") (Star (Char "ref"))]) (MayBe (Char "u"))])
       (Star
        (Seq
         [(Char "COMMA")
          (Seq
           [(Seq [(Char "ref") (Star (Char "ref"))])
            (MayBe (Char "u"))])]))]))])
  s3-1
  (Seq
   [s2-0
    (Star
     (Or
      [(Seq
        [(Char "mhr") (Char "aut") (Char "rus")]
        "пример без автора")
       (Seq [(Char "mhr") (Char "rus")] "пример без автора")]))
    (MayBe
     (Seq
      [(Char "ex")
       (Seq
        [(Seq [(Char "ref") (Star (Char "ref"))]) (MayBe (Char "u"))])
       (Star
        (Seq
         [(Char "COMMA")
          (Seq
           [(Seq [(Char "ref") (Star (Char "ref"))])
            (MayBe (Char "u"))])]))]))])
  s4-0
  (Seq [s3-0 (Star s3-0)])
  s5-0
  (Seq [s4-0])
  s6-0
  (Or [s5-0 s3-1])
  s7-0
  (Seq [(Char "R") s2-0 s6-0])
  s8-0
  (Seq [s7-0 (Star s7-0)])
  s9-0
  (Seq [s2-0 s8-0])
  s15-0
  (Seq
   [(Or
     [(Seq
       [s2-0
        (Seq
         [(Seq [(Char "L") s2-0 (Or [s6-0 s9-0])])
          (Star (Seq [(Char "L") s2-0 (Or [s6-0 s9-0])]))])])
      s9-0
      s6-0])])]
 s15-0))
          

(def g1
  (Char "pre")
  )
          
(def structures
  {"foo" [
        :tag-compare-fn
      #(if-not
       (or (= %1 %2) (and (= %1 "m1") (= %2 "m")))
       {:error :tags-mismatch :context [%1 %2]}
       )
:line-first-level-process-fn (comp list (fn [x]
                                          ;(println "got x" x)
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
                                            {:tag "R" :value x}

                                            :else
                                            x
                                            )
                                          ))
        :grammar
        g1 #_(Seq [
          (MayBe (Char "pre"))
          (MayBe (Char "end"))
          (MayBe (Char "m1"))
          (Star
            (Seq [
              (Char "trn")
              (Star (Seq [
                          (Char "mhr")
                          (MayBe (Char "aut"))
                          (Char "rus")
                          ]))
              (Star
                (Seq [
                      (Char "ex")
                      (Char "ref")
                      ])
                )
                  ]
                 )
            )
              ]
          )
        ]}
  )
