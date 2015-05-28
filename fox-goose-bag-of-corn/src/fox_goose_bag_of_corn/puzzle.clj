(ns fox-goose-bag-of-corn.puzzle)

(def start-pos [
                [
                 [:fox :goose :corn :you]
                 [:boat]
                 []]])

(def start-state (first start-pos))

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

(defn fail? [state]
  (or (fox-alone-with-goose? state)
      (goose-alone-with-corn-at? state)))

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

(defn haul-actions-from [where [left-bank river right-bank :as state]]
  (case where
    :left-bank (let [hs (haulables-at left-bank)]
                 (mapv (fn [h] (list 'haul->left-bank->boat h state)) hs))
    :right-bank (let [hs (haulables-at right-bank)]
                  (mapv (fn [h] (list 'haul->right-bank->boat h state)) hs))
    :river (let [hs (haulables-at river)]
             (vec
              (concat (mapv (fn [h] (list 'haul->boat->left-bank h state)) hs)
                      (mapv (fn [h] (list 'haul->boat->right-bank h state)) hs))))
    (throw (IllegalArgumentException. (str "Bad where: " where)))))

(defn actions-from [[left-bank river right-bank :as state]]
  (cond
   (on-left-bank? state) (conj (haul-actions-from :left-bank state)
                               (list 'move->left-bank->boat state))
   (on-right-bank? state) (conj (haul-actions-from :right-bank state)
                                (list 'move->right-bank->boat state))
   (on-boat? state) (conj (haul-actions-from :river state)
                          (list 'move->boat->left-bank state)
                          (list 'move->boat->right-bank state))
   :else (throw (IllegalArgumentException. "Missing :you"))))

(defn act [action]
  ;; Not sold on this approach.
  (eval action))

(defn step
  [queue]
  ;; Iteratively explore all valid actions
  ;; with all valid arguments until goal is reached.
  (let [{:keys [state path] :or {path []}} (first queue)
        ;; _ (clojure.pprint/pprint state)
        remaining (vec (rest queue))
        actions (actions-from state)
        new-states (map act actions)
        new-path (conj path state)]
    (if (fail? state)
      remaining
      (into remaining
            (->> new-states
                 (remove fail?)
                 (map (fn [s] {:state s, :path new-path}))
                 vec)))))

(defn river-crossing-plan []
  (->> [{:state start-state}]
       (iterate step)
       ;; (remove (fn [{state :state}] (fail? state)))
       (take 100000)
       (filter (fn [{state :state}] (goal? state)))
       first
       :path))
