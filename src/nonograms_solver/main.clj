
(ns nonograms-solver.main
  (:use nonograms-solver.core
        nonograms-solver.io)
  (:gen-class))

(defn -main [& args]
  (if (== (count args) 1)
    (-> (first args) read-hints solve print-nonogram)
    (println "Usage: NonogramsSolver <filename>")))
