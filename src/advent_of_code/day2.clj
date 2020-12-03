(ns advent-of-code.day2
  (:require [clojure.java.io :as java.io]
            [clojure.string :as str]))

; Reads the txt and is accessible via "lines"
(defmacro with-lines
  [path & body]
  `(with-open [rdr# (java.io/reader ~path)]
     (let [~'lines (line-seq rdr#)]
       ~@body)))

; How many passwords are valid according to their policies?
(defn valid-password? [min-occurrences max-occurrences required-letter password]
  (let [num-occurrences (count (filter #(= required-letter %) password))]
    (<= min-occurrences num-occurrences max-occurrences)))

(defn entry-has-valid-password? [validation entry]
  (let [split-spaces (str/split entry #" ")
        [min-occurrences max-occurrences] (map #(Integer/parseInt %) (str/split (first split-spaces) #"-"))
        required-letter (first (second split-spaces))
        password (nth split-spaces 2)]
    (validation min-occurrences max-occurrences required-letter password)))

(defn read-and-validate [validation]
  (let [path "src/advent_of_code/inputs/day2.txt"]
    (with-lines path
                (->> lines
                     (filter (partial entry-has-valid-password? validation))
                     count))))
(read-and-validate valid-password?)

; part 2
; Exactly one of these positions must contain the given letter (1-indexed)
(defn valid-password-pt2? [first-position second-position required-letter password]
  (let [letter-at-first-position (nth password (dec first-position))
        letter-at-second-position (nth password (dec second-position))]
    (cond
      (= letter-at-first-position letter-at-second-position) false
      (= letter-at-first-position required-letter) true
      :else (= letter-at-second-position required-letter))))

(read-and-validate valid-password-pt2?)
