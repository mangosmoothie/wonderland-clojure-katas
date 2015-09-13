(ns alphabet-cipher.coder)

(defn encode [keyword message]
  "encodeme"
  (reduce
    (fn [encoded-string [keychar msgchar]]
      (str encoded-string
           (char
             (let [charsum (- (+ msgchar keychar) 97)]
               (if (< 122 charsum)
                 (- charsum 26)
                 charsum)))))
    ""
    (map #(vector (int %1) (int %2)) (cycle keyword) message)))

(defn decode [keyword message]
  "decodeme"
  (reduce
    (fn [encoded-string [keychar msgchar]]
      (str encoded-string
           (char
             (let [charsum (+ (- msgchar keychar) 97)]
               (if (< charsum 97)
                 (+ charsum 26)
                 charsum)))))
    ""
    (map #(vector (int %1) (int %2)) (cycle keyword) message)))

(defn decypher [cypher message]
  "decypherme"
  (let [cyphered
        (reduce
          (fn [encoded-string [cyphchar msgchar]]
            (str encoded-string
                 (char
                   (let [charsum (+ (- cyphchar msgchar) 97)]
                     (if (< charsum 97)
                       (+ charsum 26)
                       charsum)))))
          ""
          (map #(vector (int %1) (int %2)) cypher message))]
    (reduce
      (fn [instring index]
        (cond
          (< (count instring) (* 2 (+ 1 index))) (reduced instring)
          (= (subs instring 0 index) (subs instring index (* 2 index))) (reduced (subs instring 0 index))
          :else instring))
      cyphered
      (range 2 (inc (count cyphered))))))
