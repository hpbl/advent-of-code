(ns advent-of-code.day12
  (:require [advent-of-code.inputs.input-reader :as ir]))

; PART 1
; What is the Manhattan distance between that location and the ship's starting position?
(defn parse-line [line]
  {:action (first line) :amount (Integer/parseInt (apply str (rest line)))})

(defn parse-input [input]
  (map parse-line input))

(def directions [\E \S \W \N])

(defn direction? [action]
  (contains? (set directions) action))

(defn new-direction-index [action amount current-direction]
  (let [current-direction-index (.indexOf directions current-direction)]
    (if (= action \L)
      (mod (- current-direction-index (/ amount 90)) (count directions))
      (mod (+ current-direction-index (/ amount 90)) (count directions)))))

(defn change-direction [movements action amount]
  (->> movements
       :current-direction
       (new-direction-index action amount)
       (nth directions)
       (assoc movements :current-direction)))

(defn move [movements {:keys [action amount]}]
  (cond
    (direction? action)
    (update movements action #(+ % amount))

    (= action \F)
    (update movements (:current-direction movements) #(+ % amount))

    :else
    (change-direction movements action amount)))

(defn move-ship [parsed-input]
  (let [initial-state {\N 0 \W 0 \E 0 \S 0 :current-direction \E}]
    (reduce move initial-state parsed-input)))

(defn manhattan-distance [movements]
  (+
    (Math/abs (- (get movements \N) (get movements \S)))
    (Math/abs (- (get movements \E) (get movements \W)))))

(defn part-1 []
  (ir/with-lines "src/advent_of_code/inputs/day12.txt"
                 (->> lines
                      parse-input
                      move-ship
                      manhattan-distance)))

(part-1)