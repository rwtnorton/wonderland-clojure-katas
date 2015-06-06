(ns magic-square.puzzle
  (:require [clojure.math.combinatorics :as combo]))

(def values [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0])

(defn sum-by-rows [vs]
  (->> vs
       (partition 3)
       (map #(reduce + %))))

(defn nth-all [vs indices]
  (map (fn [i] (nth vs i)) indices))

(defn sum-by-cols [vs]
  [(reduce + (nth-all vs [0 3 6]))
   (reduce + (nth-all vs [1 4 7]))
   (reduce + (nth-all vs [2 5 8]))])

(defn sum-by-diagonals [vs]
  [(reduce + (nth-all vs [0 4 8]))
   (reduce + (nth-all vs [2 4 6]))])

(defn magic? [vs]
  (and
   (apply = (sum-by-rows vs))
   (apply = (sum-by-cols vs))
   (apply = (sum-by-diagonals vs))))

(defn values->square [vs]
  (->> vs
       (partition 3)
       (map vec)
       (into [])))

(defn magic-square [values]
  (->> values
       combo/permutations
       (filter magic?)
       first
       values->square))
