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

(defn new-direction [action amount current-direction]
  (->> current-direction
       (new-direction-index action amount)
       (nth directions)))

(defn change-direction [movements action amount]
  (->> movements
       :current-direction
       (new-direction action amount)
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


; PT 2
(defn move-forward-correctly [movements amount]
  (let [waypoint (seq (:waypoint movements))
        amounts-to-move (map #(assoc % 1 (* amount (last %))) waypoint)]
    (reduce #(update %1 (first %2) + (last %2)) movements amounts-to-move)))

(defn change-waypoint-direction [movements action amount]
  (let [waypoint-seq (seq (:waypoint movements))
        new-wp-seq (map (fn [curr-wp]
                          [(new-direction action amount (first curr-wp))
                           (last curr-wp)]) waypoint-seq)
        new-wp (into (sorted-map) new-wp-seq)]
    (assoc movements :waypoint new-wp)))

(defn move-correctly [movements {:keys [action amount]}]
  (cond
    (direction? action)
    (update-in movements [:waypoint action] #(+ % amount))

    (= action \F)
    (move-forward-correctly movements amount)

    :else
    (change-waypoint-direction movements action amount)))


(defn move-ship-correctly [parsed-input]
    (let [initial-state {\N 0 \W 0 \E 0 \S 0 :waypoint {\E 10 \N 1 \S 0 \W 0}}]
      (reduce move-correctly initial-state parsed-input)))

(defn part-2 []
  (ir/with-lines "src/advent_of_code/inputs/day12.txt"
                 (->> lines
                      parse-input
                      move-ship-correctly
                      manhattan-distance)))

(part-2)