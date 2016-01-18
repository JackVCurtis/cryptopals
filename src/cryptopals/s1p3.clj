(ns cryptopals.s1p3)
(require '[clojure.math.numeric-tower :as math])
(require '[clojure.string])

(require '[cryptopals.encoding :as enc])
(require '[cryptopals.utils :as utils])
(require '[cryptopals.operations :as op])
(require '[cryptopals.analysis :as an])

(def cypher-txt "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736")

(defn xor-with-all-chars
    [string]
    (let [characters (utils/char-list*)]
      (map op/xor-with-char (repeat string) characters)))

(defn get-string
  [s]
  (apply str (map char s)))

(defn strings
  [s]
  (map get-string s))

(defn score-list
  [l]
  (map an/score (strings (xor-with-all-chars l))))

(defn chi-sq-scores
  [s] 
  (sort-by last (zipmap (utils/char-list*) 
                        (score-list s))))
; plain text is 'Cooking MC's like a pound of bacon'
; haven't checked number, need to figure out a scoring system that isn't fakakta
