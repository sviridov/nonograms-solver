
(ns nonograms-solver.optimization)

(defn- opposite-direction [direction]
  (direction {:vertical :horizontal
              :horizontal :vertical}))

(defn- rotate-hints-in-direction [hints direction]
  (hash-map
    direction (map reverse (direction hints))
    (opposite-direction direction) (reverse ((opposite-direction direction) hints))))

(defn rotate-hints [hints directions]
  (reduce rotate-hints-in-direction hints directions))

(defn- rotate-nonogram-in-direction [nonogram direction]
  (if (= direction :vertical)
      (reverse nonogram)
      (map reverse nonogram)))

(defn rotate-nonogram [nonogram directions]
  (reduce rotate-nonogram-in-direction nonogram directions))

(defn- score [direction-hints]
  (+ (count direction-hints)
     (reduce + 0
       (map #(reduce + 0 %) direction-hints))))

(defn- need-to-rotate? [direction-hints]
  (let [size (count direction-hints)
        first-half (take (quot size 2) direction-hints)
        second-half (drop (+ (quot size 2) (rem size 2)) direction-hints)
        first-half-score (score first-half)
        second-half-score (score second-half)]
    (< first-half-score second-half-score)))

(defn rotate-directions [hints]
  (filter #(need-to-rotate? ((opposite-direction %) hints)) [:horizontal :vertical]))
