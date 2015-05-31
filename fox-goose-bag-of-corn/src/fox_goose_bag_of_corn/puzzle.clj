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

;;
;; Not quite what was desired for this exercise, I suspect, but
;; maybe divide the effort into sub-goals:
;;  * Haul goose to the right bank
;;  * Haul fox and corn onto the boat
;;  * Haul goose to the left bank
;;  * Haul fox and corn onto the right bank
;;  * Haul goose to the right bank
;;

(defn subgoal-goose-right-bank? [[left-bank river right-bank]]
  (= (set right-bank) #{:you :goose}))

(defn subgoal-fox-corn-on-boat? [[left-bank river right-bank]]
  (and
   (= (set river) #{:boat :you :corn :fox})
   (= (set right-bank) #{:goose})))

(defn subgoal-goose-left-bank? [[left-bank river right-bank]]
  (and
   (= (set left-bank) #{:goose :you})
   (= (set river) #{:boat :corn :fox})))

(defn subgoal-fox-corn-on-right-bank? [[left-bank river right-bank]]
  (and
   (= (set left-bank) #{:goose})
   (= (set right-bank) #{:boat :you :corn :fox})))

(defn river-crossing-plan []
  (->> [{:state states/start-state}]
       (iterate step)
       ;; (remove (fn [{state :state}] (fail? state)))
       ;; (take 100000)
       (filter (fn [{state :state}] (states/goal? state)))
       first
       :path))

(defn river-crossing-plan-via-subgoals []
  (let [goose-right
        (->> [{:state states/start-state}]
             (iterate step)
             (filter (fn [{state :state}] (subgoal-goose-right-bank? state)))
             first)]
    goose-right))
       ;; (iterate step)
       ;; (filter (fn [{state :state}] (subgoal-fox-corn-on-boat? state)))
       ;; first
       ;; (iterate step)
       ;; (filter (fn [{state :state}] (subgoal-goose-left-bank? state)))
       ;; first
       ;; (iterate step)
       ;; (filter (fn [{state :state}] (subgoal-fox-corn-on-right-bank? state)))
       ;; first
       ;; (iterate step)
       ;; (filter (fn [{state :state}] (states/goal? state)))
       ;; first
       ;; :path))
