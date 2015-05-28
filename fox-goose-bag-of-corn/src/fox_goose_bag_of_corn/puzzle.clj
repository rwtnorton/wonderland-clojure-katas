(ns fox-goose-bag-of-corn.puzzle
  (:require [fox-goose-bag-of-corn.actions :as acts]
            [fox-goose-bag-of-corn.position :as pos]
            [fox-goose-bag-of-corn.states :as states]))

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
        actions (acts/actions-from state)
        new-states (map act actions)
        new-path (conj path state)]
    (if (states/fail? state)
      remaining
      (into remaining
            (->> new-states
                 (remove states/fail?)
                 (map (fn [s] {:state s, :path new-path}))
                 vec)))))

(defn river-crossing-plan []
  (->> [{:state states/start-state}]
       (iterate step)
       ;; (remove (fn [{state :state}] (fail? state)))
       (take 100000)
       (filter (fn [{state :state}] (states/goal? state)))
       first
       :path))
