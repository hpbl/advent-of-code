(ns advent-of-code.day3
  (:require [advent-of-code.inputs.input-reader :as ir]))

; Starting at the top-left corner of your map and following a slope of right 3 and down 1, how many trees would you encounter?
(defn has-tree? [line position]
  (= \# (nth line position)))

; Position landed after moving 3 to the right
(defn right-movement [line previous-position movement-amount]
  (let [line-width (count line)
        desired-movement (+ previous-position movement-amount)]
    (if (>= desired-movement line-width)
      (mod desired-movement line-width)
      desired-movement)))

; Moving 1 down does not change the position number
(defn down-movement [_ previous-position]
  previous-position)

; Move according to the desired movement and update tree count and position
(defn move-to-new-line [movement-amount current-stats new-line ]
  (let [{previous-position :position previous-tree-count :tree-count} current-stats
        new-position (down-movement new-line (right-movement new-line previous-position movement-amount))]
    {:tree-count (if (has-tree? new-line new-position)
                       (inc previous-tree-count)
                       previous-tree-count)
     :position new-position}))

(defn move-to-exit [initial-stats horizontal-movement-amount vertical-movement-amount lines]
  (loop [stats initial-stats
         index vertical-movement-amount]

    (if (>= index (count lines))
      stats
      (let [new-line  (nth lines index)
            new-stats (move-to-new-line horizontal-movement-amount stats new-line)]
        (recur new-stats (+ index vertical-movement-amount))))))


(defn read-and-move-to-exit [horizontal-movement-amount vertical-movement-amount]
  (let [path "src/advent_of_code/inputs/day3.txt"
        initial-stats {:tree-count 0 :position 0}]
    (ir/with-lines path
                   (->> lines
                        (move-to-exit initial-stats horizontal-movement-amount vertical-movement-amount)
                        :tree-count))))

(read-and-move-to-exit 3 1)

; PT 2
; What do you get if you multiply together the number of trees encountered on each of the listed slopes?
;Right 1, down 1.
;Right 3, down 1. (This is the slope you already checked.)
;Right 5, down 1.
;Right 7, down 1.
;Right 1, down 2.
(*
  (read-and-move-to-exit 1 1)
  (read-and-move-to-exit 3 1)
  (read-and-move-to-exit 5 1)
  (read-and-move-to-exit 7 1)
  (read-and-move-to-exit 1 2))