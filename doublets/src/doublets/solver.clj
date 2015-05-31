(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clj-levy.core :as lev]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(defn words-of-length [n]
  (filter (fn [w] (= (count w) n)) words))

(defn doublets [word1 word2]
  (cond
   (not= (count word1) (count word2)) (throw
                                       (IllegalArgumentException.
                                        (str "Must have same lengths: "
                                             word1 word2)))
   (= word1 word2) []
   :else (let [words (words-of-length (count word1))
               ]
           [word2])))
