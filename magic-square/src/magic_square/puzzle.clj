(ns magic-square.puzzle
  (:require [clojure.math.combinatorics :as combo]))

(def values [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0])

(defn magic-square [values]
  (first (reduce
           (fn [results grid]
             (conj results (vector (subvec grid 0 3) (subvec grid 3 6) (subvec grid 6 9))))
           []
           (filter #(= (+ (% 0) (% 1) (% 2))
                       (+ (% 0) (% 3) (% 6))
                       (+ (% 0) (% 4) (% 8))
                       (+ (% 3) (% 4) (% 5))
                       (+ (% 6) (% 7) (% 8))
                       (+ (% 1) (% 4) (% 7))
                       (+ (% 2) (% 5) (% 8))
                       (+ (% 2) (% 4) (% 6)))
                   (combo/permutations values)))))

