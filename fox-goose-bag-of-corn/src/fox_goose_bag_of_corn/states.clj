(ns fox-goose-bag-of-corn.states)

(def start-pos [
                [
                 [:fox :goose :corn :you]  ; left-bank
                 [:boat]                   ; river
                 []                        ; right-bank
                 ]])

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
