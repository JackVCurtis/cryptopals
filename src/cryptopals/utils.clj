(ns cryptopals.utils)
(require '[clojure.string :as str-lib])

(defn b64-map* 
  "returns a map of characters representing a single digit in base 64"
  []
  (let [
    zero-to-nine (mapv str (range 10))
    uc-alpha (mapv (comp str char) (range 65 91))
    lc-alpha (mapv (comp str char) (range 97 123))
    extra-sym ["+" "\\"] ]
  (zipmap
    (vec (range 64))
    (flatten (conj 
        uc-alpha lc-alpha zero-to-nine extra-sym)))))    

(defn split-str-by
  "returns vector of string split at every nth character"
  ([n string] 
    (split-str-by n string []))
  ([n string result]
    (if (<= (count string) n) 
       (conj result string)
      (let [next-str (subs string 0 n) remaining-string (subs string n)] 
          (recur n remaining-string (conj result next-str))))))

(defn make-padding [n]
  (apply str (repeat n "0")))

(defn pad-mod 
    "pads a string with zeros up to length n"
    [n string]
    (let [
      length (count string)
      remainder (mod (count string) n)] 
      (if (< length n) 
        (str (make-padding (- n length)) string) 
        (str (make-padding remainder) string))))

(defn in? 
  "true if seq contains elm"
  [seq elm]  
  (some #(= elm %) seq))

(defn char-list* 
  "returns alphabetical characters plus some extra bits"
  [] 
  (map char (take 94 (iterate inc 32))))

(defn char-counts
  "returns map of character counts in a string"
  [string]
  (frequencies (split-str-by 1 (str-lib/lower-case string))))

(defn letters 
  [freq-map]
  (let [ltrs (mapv (comp str char) (range 97 123))]
    (filter (fn [[k v]] (in? ltrs k)) freq-map)))

(defn over-map
  [fnc m]
  (into {} (for [[k v] m] [k (fnc v)])))
