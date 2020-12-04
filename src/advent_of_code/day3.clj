(ns advent-of-code.day3
  (:require [advent-of-code.inputs.input-reader :as ir]))

; Starting at the top-left corner of your map and following a slope of right 3 and down 1, how many trees would you encounter?
(defn has-tree? [line position]
  (= \# (nth line position)))

; Position landed after moving 3 to the right
(defn right-movement [line previous-position]
  (let [line-width (count line)
        desired-movement (+ previous-position 3)]
    (if (>= desired-movement line-width)
      (mod desired-movement line-width)
      desired-movement)))

; Moving 1 down does not change the position number
(defn down-movement [_ previous-position]
  previous-position)

; Move according to the desired movement and update tree count and position
(defn move-to-new-line [new-line current-stats]
  (let [{previous-position :position previous-tree-count :tree-count} current-stats
        new-position (down-movement new-line (right-movement new-line previous-position))]
    {:tree-count (if (has-tree? new-line new-position)
                       (inc previous-tree-count)
                       previous-tree-count)
     :position new-position}))

(defn move-to-exit [lines]
 (loop [index 1
        stats {:tree-count 0 :position 0}]

  (if (= index (count lines))
    stats

    (let [new-line (nth lines index)
          new-stats (move-to-new-line new-line stats)]
      (recur (inc index) new-stats)))))


(defn read-and-move-to-exit []
  (let [path "src/advent_of_code/inputs/day3.txt"]
    (ir/with-lines path
                   (->> lines
                        move-to-exit
                        :tree-count))))

(read-and-move-to-exit)