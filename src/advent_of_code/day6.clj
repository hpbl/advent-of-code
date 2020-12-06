(ns advent-of-code.day6
  (:require [advent-of-code.inputs.input-reader :as ir]))

(defn sum-yes-questions [lines]
  (loop [index 0
         current-group-answer ""
         current-sum 0]

    (cond
      (= index (count lines))
      (+ current-sum (count (distinct current-group-answer)))

      :else
      (let [current-line (nth lines index)]
        (if (empty? current-line)
          (recur (inc index) "" (+ current-sum (count (distinct current-group-answer))))
          (recur (inc index) (str current-group-answer current-line) current-sum))))))

(defn part-1 []
  (ir/with-lines "src/advent_of_code/inputs/day6.txt"
                 (->> lines
                      sum-yes-questions)))

(part-1)