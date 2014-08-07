
(ns nonograms-solver.core
  (:refer-clojure :exclude [== < +])
  (:use  clojure.core.logic
        [clojure.core.logic.arithmetic :only (<)]
        [clojure.core.logic.fd :only (+ domain in)]
         nonograms-solver.optimization))

(defne match-line-pattern [line pattern chain?]
  ([[] [ ] _])
  ([[] [0] _])

  ([[0 . line-tail] [0 . pattern-tail] true]
     (match-line-pattern line-tail pattern-tail false))

  ([[1 . line-tail] [counter . pattern-tail] _]
     (< 0 counter)
     (fresh [new-counter new-pattern]
       (+ new-counter 1 counter)
       (conso new-counter pattern-tail new-pattern)
       (match-line-pattern line-tail new-pattern true)))

  ([[0 . line-tail] _ false]
     (match-line-pattern line-tail pattern false)))

(defn- constrain-lines [lines hints]
  (cond (and (seq lines) (seq hints))
        (all
          (match-line-pattern (first lines) (first hints) false)
          (constrain-lines (rest lines) (rest hints)))

        (and (empty? lines) (empty? hints)) succeed

        :else fail))

(defn solve [hints]
  (let [rotate-directions (rotate-directions hints)

        {vertical-hints :vertical horizontal-hints :horizontal}
        (rotate-hints hints rotate-directions)

        grid-width  (count vertical-hints)
        grid-height (count horizontal-hints)
        grid-size   (* grid-width grid-height)

        cells   (repeatedly grid-size lvar)
        rows    (partition grid-width cells)
        columns (apply map vector rows)

        lines (sorted-lines rows columns horizontal-hints vertical-hints)

        result (->> (run 1 [result]
                      (== result cells)
                      (everyg #(in % (domain 0 1)) cells)
                      (apply constrain-lines lines))
                    (first)
                    (partition grid-width))]
    (rotate-nonogram result rotate-directions)))
