(ns wonderland-number.finder)


(def wl-range
  (range 100000 1000000))

(defn have-same-nums
  [coll]
  (let [s1 (set (str (first coll)))
        c (map #(set (str %)) (rest coll))]
    (every? #(= s1 %) c)))

(defn make-nums
  [n]
  (map #(* n %) (range 1 7)))

(defn wonderland-number []
  ;; calculate me
  (first (filter #(have-same-nums (make-nums %)) wl-range)))

