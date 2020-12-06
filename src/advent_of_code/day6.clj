(ns advent-of-code.day6
  (:require [advent-of-code.inputs.input-reader :as ir]))

(defn sum-anyone-yes-questions [lines]
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
                      sum-anyone-yes-questions)))

(part-1)

(defn count-occurrences [char string]
  (reduce #(if (= char %2) (inc %1) %1) 0 string))

(defn count-questions-everyone-answered-yes [group-answer num-people]
  (let [questions-answered (distinct group-answer)]
    (reduce
      #(if (= (count-occurrences %2 group-answer) num-people)
         (inc %1)
         %1)
      0
      questions-answered)))

(defn sum-everyone-yes-questions [lines]
  (loop [index 0
         current-group-answer ""
         num-people 0
         current-sum 0]

    (cond
      (= index (count lines))
      (+ current-sum (count-questions-everyone-answered-yes current-group-answer num-people))

      :else
      (let [current-line (nth lines index)]
        (if (empty? current-line)
          (recur (inc index) "" 0 (+ current-sum (count-questions-everyone-answered-yes current-group-answer num-people)))
          (recur (inc index) (str current-group-answer current-line) (inc num-people) current-sum))))))

(defn part-2 []
  (ir/with-lines "src/advent_of_code/inputs/day6.txt"
                 (->> lines
                      sum-everyone-yes-questions)))

(part-2)