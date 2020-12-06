(ns advent-of-code.day5
  (:require [advent-of-code.inputs.input-reader :as ir]))

; part 1
; decoding FBFBBFFRLR reveals that it is the seat at row 44, column 5.
; Every seat also has a unique seat ID: multiply the row by 8, then add the column.
; In this example, the seat has ID 44 * 8 + 5 = 357.
;
; As a sanity check, look through your list of boarding passes.
; What is the highest seat ID on a boarding pass?

(defn seat-range [[lower-bound upper-bound] direction]
  (if (or (= direction \F) (= direction \L))
    [lower-bound (Math/floor (/ (+ upper-bound lower-bound) 2))]
    [(Math/ceil (/ (+ upper-bound lower-bound) 2)) upper-bound]))

(defn seat->id [seat]
  (let [row (first (reduce seat-range [0 127] (take 7 seat)))
        col (first (reduce seat-range [0 7] (drop 7 seat)))]
    (+ (* row 8) col)))

(defn part-1 []
  (ir/with-lines "src/advent_of_code/inputs/day5.txt"
                 (->> lines
                      (map seat->id)
                      (apply max))))

(defn empty-seat [seats]
  (for [seat-1 seats
        seat-2 seats
        :when (and
                (= (- seat-1 seat-2) 2)
                (not (contains? seats (inc seat-2))))]
    (inc seat-2)))

(defn part-2 []
  (ir/with-lines "src/advent_of_code/inputs/day5.txt"
                 (->> lines
                      (map (int (seat->id %1)))
                      set
                      empty-seat
                      )))