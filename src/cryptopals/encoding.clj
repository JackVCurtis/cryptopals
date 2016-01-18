(ns cryptopals.encoding)
(require '[clojure.string :as str])
(require '[cryptopals.utils :as utils])

(def b64-map (utils/b64-map*))

(defn byte-to-bin 
  [n]
  (utils/pad-mod 8 (Integer/toString n 2)))

(defn byte-to-hex
  [n]
  (utils/pad-mod 2 (Integer/toString n 16)))

(defn bytes-to-b64 
  [bytes]
  (let [bin-string (apply str (mapv byte-to-bin bytes))]
    (map  #(Integer/parseInt % 2) 
      (utils/split-str-by 6 bin-string))))

(defn hex-to-byte
  [hex-str]
  (mapv #(Integer/parseInt % 16) (utils/split-str-by 2 hex-str)))

(defn byte-to-string
  [arr]
  (apply str (map char arr)))

(defn hex-to-b64 
  [s]
  (apply str (mapv #(get b64-map %) (bytes-to-b64 (hex-to-byte s)))))

(defn hex-to-char 
  [string]
  (apply str (map char (hex-to-byte string))))

(defn char-to-hex
  [c]
  (Integer/toString (int c) 16))
