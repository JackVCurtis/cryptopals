(ns cryptopals.operations)
(require '[cryptopals.encoding :as enc])

(defn hex-str-xor 
  [str1 str2]
  (apply str (map (comp enc/byte-to-hex bit-xor)
    (enc/hex-to-byte str1) 
    (enc/hex-to-byte str2))))

(defn xor-with-char 
  [hex-str c]
  (mapv #(bit-xor % (int c)) (enc/hex-to-byte hex-str)))