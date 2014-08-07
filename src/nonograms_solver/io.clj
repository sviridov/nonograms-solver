
(ns nonograms-solver.io
  (:use [clojure.java.io :only (reader)]))

(defn read-hints [filename]
  (with-open [input (java.io.PushbackReader. (reader filename))]
    (read input)))

(defn- each [xs function]
  (when (seq xs)
    (function (first xs))
    (each (rest xs) function)))

(defn- print-nonogram-cell [cell]
  (if (zero? cell)
    (print " ")
    (print "X")))

(defn- print-nonogram-row [row]
  (each row print-nonogram-cell)
  (newline))

(defn print-nonogram [rows]
  (each rows print-nonogram-row)
  (flush))
