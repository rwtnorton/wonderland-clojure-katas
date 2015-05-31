(ns fox-goose-bag-of-corn.puzzle
  (:require [fox-goose-bag-of-corn.actions :as acts]
            [fox-goose-bag-of-corn.position :as pos]
            [fox-goose-bag-of-corn.states :as states]
            [clojure.set]
            [clojure.pprint]))

;;
;; step could use a utility function.
;;
;; Some heuristics for consideration:
;; 1.) If all haulables are at the same place, haul the goose.
;; 2.) In general, favor hauling actions over moving (but do not forbid).
;; 3.) Keep corn and fox together.  BFFs.
;; 4.) Avoid winding up back at state we just visited.
;; 5.) Might nudge goose to gravitate towards banks.
;; 6.) All things being equal, try to move all haulables toward right bank.
;;

(defn haulables-all-together-at? [place]
  (clojure.set/subset? #{:fox :goose :corn} (set place)))

(defn haulables-all-together? [state]
  (some haulables-all-together-at? state))

(defn state-as-sets [state]
  (mapv set state))

(defn state-as-vecs [state]
  (mapv vec state))

(defn step
  [queue]
  ;; Iteratively explore all valid actions
  ;; with all valid arguments until goal is reached.
  (let [{:keys [state path] :or {path []} :as s} (first queue)
        _ (clojure.pprint/pprint s)
        remaining (vec (rest queue))
        actions (acts/actions-from state)
        new-states (map acts/act actions)
        new-states (remove states/fail? new-states)
        new-states (vec
                    (map state-as-vecs
                         (clojure.set/difference
                          (set (map state-as-sets new-states))
                          (set (map state-as-sets path)))))
        new-path (conj path state)]
    (if (states/fail? state)
      remaining
      (into
       (->> new-states
            ;; (remove states/fail?)
            (map (fn [s] {:state s, :path new-path}))
            vec)
       remaining))))

(defn river-crossing-plan []
  (->> [{:state states/start-state}]
       (iterate step)
       ;; (remove (fn [{state :state}] (fail? state)))
       ;; (take 100000)
       (filter (fn [{state :state}] (states/goal? state)))
       first
       :path))
