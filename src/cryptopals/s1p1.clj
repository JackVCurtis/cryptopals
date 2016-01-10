(require '[clojure.string :as str])

(defn b64-map* []
  (let [
    zero-to-nine (mapv str (range 10))
    uc-alpha (mapv (comp str char) (range 65 91))
    lc-alpha (mapv (comp str char) (range 97 123))
    extra-sym ["+" "\\"] ]
  (zipmap
    (vec (range 64))
    (flatten (conj 
        uc-alpha lc-alpha zero-to-nine extra-sym)))))    

(def b64-map (b64-map*))
(def hex-num "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")
(def b64 "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")

(defn split-str-by
  ([n string result]
    (if (<= (count string) n) 
       (conj result string)
      (let [next-str (subs string 0 n) remaining-string (subs string n)] 
          (recur n remaining-string 
            (conj result next-str)))))
    ([n string] 
      (split-str-by n string [])))

(defn make-padding [n]
  (apply str (repeat n "0")))

(defn pad-mod 
    [n string]
    (let [
      length (count string)
      remainder (mod (count string) n)] 
      (if (< length n) 
        (str (make-padding (- n length)) string) 
        (str (make-padding remainder) string))))

(defn hex-to-byte 
  [hex-str]
  (mapv #(Integer/parseInt % 16) (split-str-by 2 hex-str)))

(defn byte-to-bin 
  [n]
  (pad-mod 8 (Integer/toString n 2)))

(defn bytes-to-b64 
  [bytes]
  (let [bin-string (apply str (mapv byte-to-bin bytes))]
    (map  #(Integer/parseInt % 2) 
      (split-str-by 6 bin-string))))

(defn hex-to-b64 
  [s]
  (apply str (mapv #(get b64-map %) (bytes-to-b64 (hex-to-byte s)))))

(hex-to-b64 hex-num)