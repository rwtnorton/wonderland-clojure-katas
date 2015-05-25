(ns alphabet-cipher.coder)

(defn lazy-seq->str [s]
  (apply str (vec s)))

(def alphabet (->> (range (int \a) (inc (int \z)))
                   (map char)
                   lazy-seq->str))

(defn distance-from-a [c]
  (- (int c) (int \a)))

(defn stretch-keyword [keyword n]
  (->> (seq keyword)
       cycle
       (take n)
       lazy-seq->str))

(defn encode-char [kw-char msg-char]
  (let [n (+ (distance-from-a kw-char)
             (distance-from-a msg-char))
        i (mod n (count alphabet))]
    (get alphabet i)))

(defn decode-char [kw-char enc-char]
  (let [n (- (distance-from-a enc-char)
             (distance-from-a kw-char))
        i (mod n (count alphabet))]
    (get alphabet i)))

(defn gen-coding-fn [coding-char-fn]
  (fn [keyword message]
    (let [stretched (stretch-keyword keyword (count message))]
      (->> (map vector stretched message)
           (map (fn [[k m]] (coding-char-fn k m)))
           lazy-seq->str))))

(def encode (gen-coding-fn encode-char))

(def decode (gen-coding-fn decode-char))

