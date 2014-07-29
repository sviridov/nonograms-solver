
(ns nonograms-solver.core
  (:refer-clojure :exclude [== < +])
  (:use  clojure.core.logic
        [clojure.core.logic.arithmetic :only (<)]
        [clojure.core.logic.fd :only (+ domain in)]
         nonograms-solver.optimization))

(defne match-line-pattern [pattern line chain?]
  ([[ ] [] _])
  ([[0] [] _])

  ([[0 . pattern-tail] [0 . line-tail] true]
     (match-line-pattern pattern-tail line-tail false))

  ([[counter . pattern-tail] [1 . line-tail] _]
     (< 0 counter)
     (fresh [new-counter new-pattern]
       (+ new-counter 1 counter)
       (conso new-counter pattern-tail new-pattern)
       (match-line-pattern new-pattern line-tail true)))

  ([_ [0 . line-tail] false]
     (match-line-pattern pattern line-tail false)))

(defn- constrain-lines [lines hints]
  (if (and (empty? lines) (empty? hints))
    succeed
    (all
      (match-line-pattern (first hints) (first lines) false)
      (constrain-lines (rest lines) (rest hints)))))

(defn solve [hints]
  (let [rotate-directions (rotate-directions hints)

        {vertical-hints :vertical horizontal-hints :horizontal}
        (rotate-hints hints rotate-directions)

        grid-width  (count vertical-hints)
        grid-height (count horizontal-hints)
        grid-size   (* grid-width grid-height)

        cells   (repeatedly grid-size lvar)
        rows    (->> cells (partition grid-width) (map vec) (into []))
        columns (apply map vector rows)

        result (->> (run 1 [result]
                      (== result cells)
                      (everyg #(in % (domain 0 1)) cells)
                      (apply constrain-lines (sorted-lines rows columns horizontal-hints vertical-hints)))
                    (first)
                    (partition grid-width))]
    (rotate-nonogram result rotate-directions)))
