
(ns nonograms-solver.io)

(defn- each [xs function]
  (when (seq xs)
    (function (first xs))
    (each (rest xs) function)))

(defn- print-nonogram-cell [cell]
  (if (zero? cell)
    (print \space)
    (print \X)))

(defn- print-nonogram-row [row]
  (each row print-nonogram-cell)
  (newline))

(defn print-nonogram [rows]
  (each rows print-nonogram-row)
  (newline))
