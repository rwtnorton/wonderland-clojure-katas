(ns fox-goose-bag-of-corn.actions
  (:require [fox-goose-bag-of-corn.position :as pos]))

(defn remove-at [goners place]
  (vec (remove (set goners) place)))

(defn ^:action move->left-bank->boat [[left-bank river right-bank]]
  [(remove-at [:you] left-bank)
   (conj river :you)
   right-bank])

(defn ^:action move->boat->right-bank [[left-bank river right-bank]]
  [left-bank
   (remove-at [:you] river)
   (conj right-bank :you)])

(defn ^:action move->boat->left-bank [[left-bank river right-bank]]
  [(conj left-bank :you)
   (remove-at [:you] river)
   right-bank])

(defn ^:action move->right-bank->boat [[left-bank river right-bank]]
  [left-bank
   (conj river :you)
   (remove-at [:you] right-bank)])

(defn ^:action haul->left-bank->boat [cargo [left-bank river right-bank]]
  [(remove-at [:you cargo] left-bank)
   (conj river :you cargo)
   right-bank])

(defn ^:action haul->boat->right-bank [cargo [left-bank river right-bank]]
  [left-bank
   (remove-at [:you cargo] river)
   (conj right-bank :you cargo)])

(defn ^:action haul->right-bank->boat [cargo [left-bank river right-bank]]
  [left-bank
   (conj river :you cargo)
   (remove-at [:you cargo] right-bank)])

(defn ^:action haul->boat->left-bank [cargo [left-bank river right-bank]]
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
                 (mapv
                  (fn [h] #(haul->left-bank->boat h state)) hs))
    :right-bank (let [hs (haulables-at right-bank)]
                  (mapv
                   (fn [h] #(haul->right-bank->boat h state)) hs))
    :river (let [hs (haulables-at river)]
             (vec
              (concat (mapv (fn [h] #(haul->boat->left-bank h state)) hs)
                      (mapv (fn [h] #(haul->boat->right-bank h state)) hs))))
    (throw (IllegalArgumentException. (str "Bad where: " where)))))

(defn actions-from [[left-bank river right-bank :as state]]
  (cond
   (pos/on-left-bank? state) (conj (haul-actions-from :left-bank state)
                                   #(move->left-bank->boat state))
   (pos/on-right-bank? state) (conj (haul-actions-from :right-bank state)
                                    #(move->right-bank->boat state))
   (pos/on-boat? state) (conj (haul-actions-from :river state)
                              #(move->boat->left-bank state)
                              #(move->boat->right-bank state))
   :else (throw (IllegalArgumentException. "Missing :you"))))

(defn act [action]
  ;; An action is just a no-arg closure.
  (action))
