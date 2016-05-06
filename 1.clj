boot.user=> (def x [(InputChar "x") (InputChar "x") (InputChar "L") (InputChar "x")])
boot.user=> (-> (let [x  (InputChar "x")] (Or [(Star x) (Star (Seq [(InputChar "L") (Star x)]))])) my_fold)
boot.user=> (def gl (-> (let [x  (InputChar "x")] (Or [(Star x) (Star (Seq [(InputChar "L") (Star x)]))])) my_fold))



boot.user=> (clojure.walk/postwalk (fn [x] (if (and (sequential? x) (= (first x) :value)) (let [v (second x)][:value (cond (symbol? v) (InputChar v) (vector? v) (mapv (fn [y] y) v) :else v)]) x)) gl)

boot.user=> (defn apply-one-level [level-name g x] (-> (loop [[h & t :as l] x r [] b []] (if (some? h) (let [[rph rpr :as rp] (process g l) success (-> rph empty? not)] (if success (concat b [(InputChar level-name rph)] rpr) (recur t (conj r rp) (conj b h)))) x)) vec))

boot.user=> (defn symbol-to-c [gl] (clojure.walk/postwalk (fn [x] (if (symbol? x) (Char x) x)) gl))


