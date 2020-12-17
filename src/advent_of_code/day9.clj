(ns advent-of-code.day9
  (:require [advent-of-code.inputs.input-reader :as ir]))

(def input [
            35
            20
            15
            25
            47
            40
            62
            55
            65
            95
            102
            117
            150
            182
            127
            219
            299
            277
            309
            576])

; Part 1
; find the first number in the list (after the preamble) which is not the sum of two of the 25 numbers before it.
; What is the first number that does not have this property?

(defn diff-if-not-number [number goal]
  (let [diff (- goal number)]
    (if (= diff number)
      -1
      diff)))

(defn preamble-has-pair-sum? [preamble goal]
  ;(println preamble goal)
  (let [diff-set (set (map #(diff-if-not-number % goal) preamble))]
    (some #(contains? diff-set %) preamble)))

(defn find-wrong-number [preamble-size input]
  (loop [preamble (take preamble-size input)
         remaining-numbers (drop preamble-size input)]

    (cond
      ; No wrong number found
      (empty? remaining-numbers)
      -1

      (not (preamble-has-pair-sum? preamble (first remaining-numbers)))
      (first remaining-numbers)

      :else
      (recur
        (concat (rest preamble) [(first remaining-numbers)])
        (rest remaining-numbers)))))

(defn part-1 []
  (ir/with-lines "src/advent_of_code/inputs/day9.txt"
                 (->> lines
                      (map #(Long/parseLong %))
                      (find-wrong-number 25))))

(part-1)


; Part 2
; find a contiguous set of at least two numbers in your list which sum to the invalid number from step 1.
; To find the encryption weakness, add together the smallest and largest number in this contiguous range; in this example, these are 15 and 47, producing 62.
; What is the encryption weakness in your XMAS-encrypted list of numbers?
(defn find-weakness [input]
  (let [goal (find-wrong-number 25 input)]
   (loop [start-pivot-index 0
         end-pivot-index 1]

    (let [candidate-sequence (subvec input start-pivot-index end-pivot-index)
          candidate-sum (apply + candidate-sequence)]
      (cond

      (= candidate-sum goal)
      (+ (apply max candidate-sequence) (apply min candidate-sequence))

      :else
      (if (< candidate-sum goal)
        (recur start-pivot-index (inc end-pivot-index))
        (recur (inc start-pivot-index) (+ 2 start-pivot-index))))))))

(defn part-2 []
  (ir/with-lines "src/advent_of_code/inputs/day9.txt"
                 (->> lines
                      (map #(Long/parseLong %))
                      vec
                      find-weakness)))

(part-2)
