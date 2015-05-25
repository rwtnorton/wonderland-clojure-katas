(ns fox-goose-bag-of-corn.puzzle)

(def start-pos [
                [[:fox :goose :corn :you]
                 [:boat]
                 []]])

(defn goal? [[left-bank river right-bank]]
  (and (empty? left-bank)
       (= #{:boat} (set river))
       (= #{:fox :goose :corn :you} (set right-bank))))

(defn unattended-on-bank? [malcontents bank]
  (let [b (set bank)]
    (and (not (:you b))
         (every? b malcontents))))

(def fox-alone-with-goose-on-bank?
  (partial unattended-on-bank? [:fox :goose]))

(def goose-alone-with-corn-on-bank?
  (partial unattended-on-bank? [:goose :corn]))

(defn banks? [predicate [left-bank _ right-bank]]
  (or (predicate left-bank)
      (predicate right-bank)))

(defn fox-alone-with-goose? [state]
  (banks? fox-alone-with-goose-on-bank? state))

(defn goose-alone-with-corn? [state]
  (banks? goose-alone-with-corn? state))

(defn river-crossing-plan []
  start-pos)
