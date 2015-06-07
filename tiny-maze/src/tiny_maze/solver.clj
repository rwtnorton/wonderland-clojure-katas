(ns tiny-maze.solver)

(def sample-maze
  [[:S 0 1]
   [1  0 1]
   [1  0 :E]])

(defn maze->positions [maze]
  (->> maze
       (map-indexed
        (fn [row-index row]
          (->> row count range
               (map (fn [col-index] [row-index col-index])))))
       (apply concat)
       (into [])))

(defn maze->position-map [maze]
  (let [ps (->> maze maze->positions)
        vs (->> ps (map #(get-in maze %)))]
    (into {}
          (map vector ps vs))))

(defn maze->values-map [maze]
  (let [ps (->> maze maze->positions)
        vs (->> ps (map #(get-in maze %)))]
    (reduce (fn [acc [p v]]
              (if (find acc v)
                (update-in acc [v] conj p)
                (assoc acc v [p])))
            {}
            (map vector ps vs))))

(defn locate-start [maze]
  (->> maze
       maze->values-map
       :S
       first))

(defn locate-end [maze]
  (->> maze
       maze->values-map
       :E
       first))

(defn solve-maze [maze])
