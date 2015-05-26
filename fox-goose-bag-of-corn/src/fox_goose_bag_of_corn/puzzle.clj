(ns fox-goose-bag-of-corn.puzzle)

(def start-pos [
                [
                 [:fox :goose :corn :you]
                 [:boat]
                 []]])

(defn goal? [[left-bank river right-bank]]
  (and (empty? left-bank)
       (= #{:boat} (set river))
       (= #{:fox :goose :corn :you} (set right-bank))))

(defn unattended-at? [malcontents place]
  (let [b (set place)]
    (and (not (:you b))
         (every? b malcontents))))

(def fox-alone-with-goose-at?
  (partial unattended-at? [:fox :goose]))

(def goose-alone-with-corn-at?
  (partial unattended-at? [:goose :corn]))

(defn fox-alone-with-goose? [state]
  (some fox-alone-with-goose-at? state))

(defn goose-alone-with-corn? [state]
  (some goose-alone-with-corn-at? state))

(defn on-left-bank? [[left-bank _ _]]
  (:you (set left-bank)))

(defn on-boat? [[_ river _]]
  (:you (set river)))

(defn on-right-bank? [[_ _ right-bank]]
  (:you (set right-bank)))

(defn remove-at [goners place]
  (vec (remove (set goners) place)))

(defn move->left-bank->boat [[left-bank river right-bank]]
  [(remove-at [:you] left-bank)
   (conj river :you)
   right-bank])

(defn move->boat->right-bank [[left-bank river right-bank]]
  [left-bank
   (remove-at [:you] river)
   (conj right-bank :you)])

(defn move->boat->left-bank [[left-bank river right-bank]]
  [(conj left-bank :you)
   (remove-at [:you] river)
   right-bank])

(defn move->right-bank->boat [[left-bank river right-bank]]
  [left-bank
   (conj river :you)
   (remove-at [:you] right-bank)])

(defn haul->left-bank->boat [cargo [left-bank river right-bank]]
  [(remove-at [:you cargo] left-bank)
   (conj river :you cargo)
   right-bank])

(defn haul->boat->right-bank [cargo [left-bank river right-bank]]
  [left-bank
   (remove-at [:you cargo] river)
   (conj right-bank :you cargo)])

(defn haul->right-bank->boat [cargo [left-bank river right-bank]]
  [left-bank
   (conj river :you cargo)
   (remove-at [:you cargo] right-bank)])

(defn haul->boat->left-bank [cargo [left-bank river right-bank]]
  [(conj left-bank :you cargo)
   (remove-at [:you cargo] river)
   right-bank])

(defn haulables-at [place]
  (vec (remove #{:you :boat} place)))

(defn haulables? [place]
  (not-empty (haulables-at place)))

(defn next-state [[left-bank river right-bank :as state]]
  ;; Iteratively explore all valid actions
  ;; with all valid arguments until goal is reached.
  state)

(defn river-crossing-plan []
  start-pos)
