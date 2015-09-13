(ns doublets.solver
  (:require [clojure.java.io :refer [resource]]
            [clojure.edn :as edn]
            [flatland.ordered.set :refer [ordered-set]]))

(require '[clojure.java.io :refer [resource]] '[clojure.edn :as edn] '[flatland.ordered.set :refer [ordered-set]])
(def words (-> "words.edn"
               (resource)
               (slurp)
               (edn/read-string)))

(defn doublets [word1 word2]
  (let [words (filter #(= (count word1) (count %)) words)
        wordtree-root [(ordered-set word1)]]
    (vec (first
           (loop [wordtree-leaves wordtree-root]
             (let [new-leaves
                   (for [leaf wordtree-leaves word words
                         :when (= 1 (count (filter #(not (= (% 0) (% 1))) (map vector (last leaf) word))))]
                     (conj leaf word))]
               (let [winner (filter #(= word2 (last %)) wordtree-leaves)]
                 (if (seq winner)
                   winner
                   (if (seq new-leaves)
                     (recur new-leaves)
                     [])))))))))