
(ns nonograms-solver.core
  (:refer-clojure :exclude [== < +])
  (:use clojure.core.logic
        [clojure.core.logic.arithmetic :only (<)]
        [clojure.core.logic.fd :only (+ domain in)]))

(defne match-row-pattern [pattern row chain?]
  ([[ ] [] _])
  ([[0] [] _])

  ([[0 . pattern-tail] [0 . row-tail] true]
     (match-row-pattern pattern-tail row-tail false))

  ([[counter . pattern-tail] [1 . row-tail] _]
     (< 0 counter)
     (fresh [new-counter new-pattern]
       (+ new-counter 1 counter)
       (conso new-counter pattern-tail new-pattern)
       (match-row-pattern new-pattern row-tail true)))

  ([_ [0 . row-tail] false]
     (match-row-pattern pattern row-tail false)))

(defn constrain-rows [rows hints]
  (if (and (empty? rows) (empty? hints))
    succeed
    (all
      (match-row-pattern (first hints) (first rows) false)
      (constrain-rows (rest rows) (rest hints)))))

(defn solve [{vertical-hints :vertical horizontal-hints :horizontal}]
  (let [grid-width  (count vertical-hints)
        grid-height (count horizontal-hints)
        grid-size   (* grid-width grid-height)

        cells   (repeatedly grid-size lvar)
        rows    (->> cells (partition grid-width) (map vec) (into []))
        columns (apply map vector rows)]

    (partition grid-width
      (first
        (run 1 [result]
          (== result cells)
          (everyg #(in % (domain 0 1)) cells)
          (constrain-rows rows horizontal-hints)
          (constrain-rows columns vertical-hints))))))
