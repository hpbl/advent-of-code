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

(defn existing-immediately-adjacent-seats [seat plane]
  (let [row-count (count plane)
        col-count (count (first plane))
        adjacent-positions-in-plane (filter #(position-in-plane? % row-count col-count) (adjacent-positions seat))]
    (filter #(seat? % plane) adjacent-positions-in-plane)))

; If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
(defn empty-seat-rule? [seat plane adj-func]
  (let [adjacent-seats (adj-func seat plane)
        occupied-adjacent-seats (filter #(occupied-seat? % plane) adjacent-seats)]
    (and
      (empty-seat? seat plane)
      (= (count occupied-adjacent-seats) 0))))

; If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
(defn occupied-seat-rule? [seat plane threshold adj-func]
  (let [adjacent-seats (adj-func seat plane)
        occupied-adjacent-seats (filter #(occupied-seat? % plane) adjacent-seats)]
    (and
      (occupied-seat? seat plane)
      (>= (count occupied-adjacent-seats) threshold))))

(defn line-next-state [row line plane occ-threshold adj-func]
  (map-indexed (fn [col seat]
         (cond
           (empty-seat-rule? {:col col :row row} plane adj-func) \#
           (occupied-seat-rule? {:col col :row row} plane occ-threshold adj-func) \L
           :else seat))
       line))

(defn next-state [plane occ-threshold adj-func]
  (map-indexed (fn [row line]
                 (line-next-state row line plane occ-threshold adj-func))
               plane))

(defn simulate-until-balance [plane occ-threshold adj-func]
  (loop [prev-plane plane]
    (let [cur-plane (next-state prev-plane occ-threshold adj-func)]
      (if (= prev-plane cur-plane)
        cur-plane
        (recur cur-plane)))))


(defn count-occupied-seats [plane occ-threshold adj-func]
  (->> plane
       (#(simulate-until-balance % occ-threshold adj-func))
       (map #(count (filter #{\#} %)))
       (apply +)))

(defn part-1 []
  (ir/with-lines "src/advent_of_code/inputs/day11.txt"
                 (->> lines
                      (#(count-occupied-seats % 4 existing-immediately-adjacent-seats)))))

; Part 2
; Instead of considering just the eight immediately adjacent seats, consider the first seat in each of those eight directions.
;(def input [
;            ".......#."
;            "...#....."
;            ".#......."
;            "........."
;            "..#L....#"
;            "....#...."
;            "........."
;            "#........"
;            "...#....."
;            ])

(defn left-adjacent-seat [{:keys [row col]} plane]
    (loop [index (dec col)]
      (cond
        ; No left adjacent
        (= index -1)
        nil

        (seat? {:row row :col index} plane)
        {:row row :col index}

        :else
        (recur (dec index)))))

(defn right-adjacent-seat [{:keys [row col]} plane]
    (loop [index (inc col)]
      (cond
        ; No right adjacent
        (= index (count (first plane)))
        nil

        (seat? {:row row :col index} plane)
        {:row row :col index}

        :else
        (recur (inc index)))))

(defn top-adjacent-seat [{:keys [row col]} plane]
  (loop [index (dec row)]
    (cond
      ; No top adjacent
      (= index -1)
      nil

      (seat? {:row index :col col} plane)
      {:row index :col col}

      :else
      (recur (dec index)))))

(defn bottom-adjacent-seat [{:keys [row col]} plane]
  (loop [index (inc row)]
    (cond
      ; No bottom adjacent
      (= index (count plane))
      nil

      (seat? {:row index :col col} plane)
      {:row index :col col}

      :else
      (recur (inc index)))))

(defn top-left-adjacent-seat [{:keys [row col]} plane]
  (loop [current-col (dec col)
         current-row (dec row)]
    (cond
      ; No top-left adjacent
      (or (= current-col -1) (= current-row -1))
      nil

      (seat? {:row current-row :col current-col} plane)
      {:row current-row :col current-col}

      :else
      (recur (dec current-col) (dec current-row)))))

(defn top-right-adjacent-seat [{:keys [row col]} plane]
  (loop [current-col (inc col)
         current-row (dec row)]
    (cond
      ; No top-left adjacent
      (or (= current-col (count (first plane))) (= current-row -1))
      nil

      (seat? {:row current-row :col current-col} plane)
      {:row current-row :col current-col}

      :else
      (recur (inc current-col) (dec current-row)))))

(defn bottom-right-adjacent-seat [{:keys [row col]} plane]
  (loop [current-col (inc col)
         current-row (inc row)]
    (cond
      ; No top-left adjacent
      (or (= current-col (count (first plane))) (= current-row (count plane)))
      nil

      (seat? {:row current-row :col current-col} plane)
      {:row current-row :col current-col}

      :else
      (recur (inc current-col) (inc current-row)))))

(defn bottom-left-adjacent-seat [{:keys [row col]} plane]
  (loop [current-col (dec col)
         current-row (inc row)]
    (cond
      ; No top-left adjacent
      (or (= current-col -1) (= current-row (count plane)))
      nil

      (seat? {:row current-row :col current-col} plane)
      {:row current-row :col current-col}

      :else
      (recur (dec current-col) (inc current-row)))))

(defn existing-adjacent-seats [seat plane]
  (let [left (left-adjacent-seat seat plane)
        top-left (top-left-adjacent-seat seat plane)
        right (right-adjacent-seat seat plane)
        top-right (top-right-adjacent-seat seat plane)
        top (top-adjacent-seat seat plane)
        down (bottom-adjacent-seat seat plane)
        down-left (bottom-left-adjacent-seat seat plane)
        down-right (bottom-right-adjacent-seat seat plane)]
    (remove nil? [left top-left right top-right top down down-left down-right])))

(defn part-2 []
  (ir/with-lines "src/advent_of_code/inputs/day11.txt"
                 (->> lines
                      (#(count-occupied-seats % 5 existing-adjacent-seats)))))

(part-2)
;(print *e)