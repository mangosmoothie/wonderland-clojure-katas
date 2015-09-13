(ns fox-goose-bag-of-corn.puzzle)

(def start-pos [#{:fox :goose :corn :you} #{} #{}])

(defn gameover? [step]
  (some true? (map #(or (and (% :fox) (% :goose) (not (% :you)))
                        (and (% :goose) (% :corn) (not (% :you))))
                   step)))

(defn new? [candidate used]
  (not-any? #(= candidate %) used))

(defn move-right [used-steps]
  (let [current (peek used-steps)
        candidates (for [passenger (disj (current 0) :you)] [(disj (current 0) passenger :you) #{:you passenger} (current 2)])
        legal-candidates (filter #(not (gameover? %)) candidates)
        new-candidates (filter #(new? % used-steps) legal-candidates)]
    new-candidates))

(defn move-left [used-steps]
  (let [current (peek used-steps)
        candidates (for [passenger (conj (disj (current 2) :you) :empty)] [(current 0) #{:you passenger} (disj (current 2) passenger :you)])
        legal-candidates (filter #(not (gameover? %)) candidates)
        new-candidates (filter #(new? % used-steps) legal-candidates)]
    (map #(vector (% 0) (disj (% 1) :empty) (% 2)) new-candidates)))

(defn boat-move [used-steps]
  (let [current (peek (vec used-steps))
        left-candidate (filter #(new? % used-steps)
                               (filter #(not (gameover? %))
                                       (vector (vector (apply conj (current 0) (current 1)) #{} (current 2)))))
        right-candidate (filter #(new? % used-steps)
                                (filter #(not (gameover? %))
                                        (vector (vector (current 0) #{} (apply conj (current 2) (current 1))))))]
    (if (empty? left-candidate) right-candidate left-candidate)))

(defn generate-step [used-steps]
  (let [your-location (.indexOf (map :you (peek (vec used-steps))) :you)]
    (cond
      (= 0 your-location) (move-right used-steps)
      (= 1 your-location) (boat-move used-steps)
      (= 2 your-location) (move-left used-steps))))

(defn river-crossing-plan []
  (let [used (conj [] start-pos)]
    (loop [used used]
      (println used)
      (let [lastone (peek (vec used))]
        (if (and (empty? (lastone 0)) (empty? (lastone 1)))
          used
          (recur (conj used (vec (first (generate-step used))))))))))

(defn choose [used-steps candidates]
  (println (str "choosing for used->" used-steps))
  (println (str "choices->" (vec candidates)))
  (reduce
    (fn [_ next-candidate]
      (let [next-steps (generate-step (conj used-steps next-candidate))]
        (cond (empty? next-steps) nil
              :else (if (= (peek (vec next-steps)) [#{}#{}#{:you :fox :corn :goose}])
                      (reduced (vector next-candidate))
                      (if (empty? (choose (conj used-steps next-candidate) next-steps))
                        nil
                        (reduced (vector next-candidate)))))))
    nil
    candidates))

(defn river-crossing-plan []
  (let [used (conj [] start-pos)]
    (loop [used used]
      (let [lastone (peek (vec used))]
        (if (and (empty? (lastone 0)) (empty? (lastone 1)))
          used
          (let [next-steps (generate-step used)
                next-step (choose used next-steps)]
            (if (= (peek (vec next-step)) [#{}#{}#{:you :fox :corn :goose}])
              (conj used (vec (first next-step)))
              (if (= (peek (vec next-steps)) [#{}#{}#{:you :fox :corn :goose}])
                (conj used (vec (first next-steps)))
                (if (= (peek (vec next-step)) [#{}#{}#{:you :fox :corn :goose}])
                  next-step
                  (recur (conj used (vec (first next-step)))))))))))))


(river-crossing-plan)
