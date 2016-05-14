(ns fulabdicts.structures.two)

(use 'regexpforobj.core)
(require '[fipp.edn :refer (pprint) :rename {pprint fipp}])


(println "Hello from grammar!")


(-> the-grammar my_fold make_let grammar_pretty fipp)
