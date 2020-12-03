(ns advent-of-code.day2
  (:require [clojure.java.io :as java.io]
            [clojure.string :as str]))

; How many passwords are valid according to their policies?
(defn valid-password? [min-occurrences max-occurrences required-letter password]
  (let [num-occurrences (count (filter #(= required-letter %) password))]
    (<= min-occurrences num-occurrences max-occurrences)))

(defn entry-has-valid-password? [entry]
  (let [split-spaces (str/split entry #" ")
        [min-occurrences max-occurrences] (map #(Integer/parseInt %) (str/split (first split-spaces) #"-"))
        required-letter (first (second split-spaces))
        password (nth split-spaces 2)]
    (valid-password? min-occurrences max-occurrences required-letter password)))

(count (filter entry-has-valid-password? input))


(defmacro with-lines
  [path & body]
  `(with-open [rdr# (java.io/reader ~path)]
     (let [~'lines (line-seq rdr#)]
       ~@body)))

(defn solution
  []
  (let [path "src/advent_of_code/inputs/day2.txt"]
    (with-lines path
                (->> lines
                     (filter entry-has-valid-password?)
                     count))))

(solution)