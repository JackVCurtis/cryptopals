(ns cryptopals.analysis)
(require '[clojure.math.numeric-tower :as math])
(require '[clojure.string :as str])
(require '[cryptopals.utils :as utils])

(defn square-diff
  [x y]
  (math/expt (- x y) 2))

(defn chi-sq
  [map1 map2]
  (reduce +
    (vals 
     (merge-with #(with-precision 5 (/ (square-diff %1 %2) %2)) map1 map2))))

(def letter-probs [0.08167M 0.01492M 0.02782M 0.04253M 0.12702M 0.02228M 0.02015M 0.06094M 0.06966M 0.00153M 0.00772M 0.04025M 0.02406M 0.06749M 0.07507M 0.01929 0.00095M 0.05987M 0.06327M 0.09056M 0.02758M 0.00978M 0.0236M 0.0015M 0.01974M 0.00074M])
(def letters (mapv (comp str char) (range 97 123)))
(def letter-map (zipmap letters (repeat 0)))

(defn letter-counts
  [s] 
  (merge-with + 
              letter-map 
              (frequencies (filter #(utils/in? letters %) 
                                   (utils/split-str-by 1 (str/lower-case s))))))
(defn expected-letters
  [s]
  (zipmap letters (mapv #(* % (count s)) letter-probs)))

(defn score
  [s]
  (chi-sq (letter-counts s)
          (expected-letters s)))
