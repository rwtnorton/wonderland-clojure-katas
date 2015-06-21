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

(defn same-word-lengths? [word1 word2]
  (= (count word1) (count word2)))

(def not-same-word-lengths? (complement same-word-lengths?))

(defn close-words? [word1 word2]
  (if (not-same-word-lengths? word1 word2)
    false
    (<= (lev/levenshtein word1 word2) 1)))

(defn next-words [{:keys [start-word path words] :or {path []}}]
  (let [new-path (conj path start-word)
        seen (set new-path)
        close-words (->> words
                         (filter (fn [w] (close-words? start-word w)))
                         (remove seen))]
    {:words close-words
     :path new-path}))

(defn help-loop [queue target-word]
  ;; (clojure.pprint/pprint queue)
  (if (empty? queue)
    []
    (let [{:keys [start-word path words] :as arg} (peek queue)
          next-queue (pop queue)
          {new-words :words, new-path :path} (next-words arg)]
      (if-let [winner (some #(= % target-word) new-words)]
        (conj new-path target-word)
        (recur
         (into next-queue
               (map (fn [w] {:start-word w
                             :path new-path
                             :words words}) new-words))
         target-word)))))

(defn doublets [word1 word2]
  (cond
   (not-same-word-lengths? word1 word2) []
   (= word1 word2) []
   :else (let [words (words-of-length (count word1))
               queue (-> clojure.lang.PersistentQueue/EMPTY
                         (conj {:start-word word1, :words words}))]
           (help-loop queue word2))))
