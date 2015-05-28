(ns fox-goose-bag-of-corn.position)

(defn on-left-bank? [[left-bank _ _]]
  (:you (set left-bank)))

(defn on-boat? [[_ river _]]
  (:you (set river)))

(defn on-right-bank? [[_ _ right-bank]]
  (:you (set right-bank)))
