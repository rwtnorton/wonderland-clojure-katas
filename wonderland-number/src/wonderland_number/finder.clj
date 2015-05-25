(ns wonderland-number.finder)

(defn six-digits? [n]
  (<= 100000 n 999999))

(defn digits [n]
  (loop [acc '()
         m n]
    (let [q (quot m 10)
          r (rem m 10)]
      (if (and (zero? q) (<= r 0))
        acc
        (recur (conj acc r) q)))))

(defn same-digits? [m n]
  (apply = (map (comp frequencies digits) [m n])))

(defn winner? [n]
  (and (six-digits? n)
       (same-digits? (* n 2) n)
       (same-digits? (* n 3) n)
       (same-digits? (* n 4) n)
       (same-digits? (* n 5) n)
       (same-digits? (* n 6) n)))

(defn wonderland-number []
  (->> (range) ;; (range 100000 1000000)
       (filter winner?)
       first))
