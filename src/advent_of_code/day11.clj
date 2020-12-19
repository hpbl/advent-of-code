(ns advent-of-code.day11
  (:require [advent-of-code.inputs.input-reader :as ir]))

(def input [
            "L.LL.LL.LL"
            "LLLLLLL.LL"
            "L.L.L..L.."
            "LLLL.LL.LL"
            "L.LL.LL.LL"
            "L.LLLLL.LL"
            "..L.L....."
            "LLLLLLLLLL"
            "L.LLLLLL.L"
            "L.LLLLL.LL"
            ])

(defn position-in-plane? [{:keys [row col]} row-count col-count]
  (and (<= 0 row (dec row-count)) (<= 0 col (dec col-count))))

(defn adjacent-positions [{:keys [row col]}]
  (let [left {:row row :col (dec col)}
        top-left {:row (dec row) :col (dec col)}
        right {:row row :col (inc col)}
        top-right {:row (dec row) :col (inc col)}
        top {:row (dec row) :col col}
        down {:row (inc row) :col col}
        down-left {:row (inc row) :col (dec col)}
        down-right {:row (inc row) :col (inc col)}]
    [left top-left right top-right top down down-left down-right]))

(defn empty-seat? [{:keys [row col]} plane]
  (= \L (nth (nth plane row) col)))

(defn occupied-seat? [{:keys [row col]} plane]
  (= \# (nth (nth plane row) col)))

(defn seat? [position plane]
    (or (empty-seat? position plane)  (occupied-seat? position plane)))

(defn existing-adjacent-seats [seat plane]
  (let [row-count (count plane)
        col-count (count (first plane))
        adjacent-positions-in-plane (filter #(position-in-plane? % row-count col-count) (adjacent-positions seat))]
    (filter #(seat? % plane) adjacent-positions-in-plane)))

; If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
(defn empty-seat-rule? [seat plane]
  (let [adjacent-seats (existing-adjacent-seats seat plane)
        empty-adjacent-seats (filter #(empty-seat? % plane) adjacent-seats)]
    (and
      (empty-seat? seat plane)
      (= (count empty-adjacent-seats) (count adjacent-seats)))))

; If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
(defn occupied-seat-rule? [seat plane]
  (let [adjacent-seats (existing-adjacent-seats seat plane)
        occupied-adjacent-seats (filter #(occupied-seat? % plane) adjacent-seats)]
    (and
      (occupied-seat? seat plane)
      (>= (count occupied-adjacent-seats) 4))))

(defn line-next-state [row line plane]
  (map-indexed (fn [col seat]
         (cond
           (empty-seat-rule? {:col col :row row} plane) \#
           (occupied-seat-rule? {:col col :row row} plane) \L
           :else seat))
       line))

(defn next-state [plane]
  (map-indexed (fn [row line]
                 (line-next-state row line plane))
               plane))

(defn simulate-until-balance [plane]
  (loop [prev-plane plane]
    (let [cur-plane (next-state prev-plane)]
      (if (= prev-plane cur-plane)
        cur-plane
        (recur cur-plane)))))


(defn count-occupied-seats [plane]
  (->> plane
       simulate-until-balance
       (map #(count (filter #{\#} %)))
       (apply +)))

(defn part-1 []
  (ir/with-lines "src/advent_of_code/inputs/day11.txt"
                 (->> lines
                      count-occupied-seats)))

(time (part-1))