(ns bird-watcher)

(def last-week 
  [0 2 5 3 7 8 4])

(defn today [birds]
  (get birds 6))

(defn inc-bird [birds]
  (assoc birds 6 (+ 1 (today birds))))

(defn day-without-birds? [birds]
  (true? (some #(= 0 %) birds)))

(defn n-days-count [birds n]
  (apply + (subvec birds 0 n)))

(defn busy-days [birds]
  (count (filter (fn [day] (> day 4)) birds)))

(defn odd-week? [birds]
  (= [1 0 1 0 1 0 1] birds))
