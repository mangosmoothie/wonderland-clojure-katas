(ns card-game-war.game
  (:import (clojure.lang PersistentQueue)))

;; feel free to use these cards or use your own data structure
(def suits [4 3 2 1])
(def ranks [2 3 4 5 6 7 8 9 10 11 12 13 14])
(def cards
  (for [suit suits
        rank ranks]
    [suit rank]))

(defn play-round [[p1-suit p1-rank] [p2-suit p2-rank]]
  (if (= p1-rank p2-rank)
    (> p1-suit p2-suit)
    (> p1-rank p2-rank)))

(defn play-game [player1-cards player2-cards]
  (when (and (seq player1-cards) (seq player2-cards))
    (if (play-round (first player1-cards) (first player2-cards))
      (recur (conj (rest player1-cards) (first player1-cards) (first player2-cards)) (rest player1-cards))
      (recur (rest player1-cards) (conj (rest player2-cards) (first player2-cards) (first player1-cards))))))


(let [[p1cards p2cards] (split-at 26 (shuffle cards))]
  (play-game (apply conj PersistentQueue/EMPTY p1cards) (apply conj PersistentQueue/EMPTY p2cards)))
