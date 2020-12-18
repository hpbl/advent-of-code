(ns advent-of-code.day10
  (:require [advent-of-code.inputs.input-reader :as ir]))

; What is the number of 1-jolt differences multiplied by the number of 3-jolt differences?

(def input [
            28
            33
            18
            42
            31
            14
            46
            20
            48
            47
            24
            23
            49
            45
            19
            38
            39
            11
            1
            32
            25
            35
            8
            17
            7
            9
            4
            2
            34
            10
            3
            ])

(defn jolt-differences [input]
  (let [sorted-plugs (sort (conj input 0))]
    (loop [index 0
           diffs {:1 0
                  :2 0
                  :3 0}]
      (cond
        (= index (dec (count sorted-plugs)))
        (update diffs :3 inc)

        :else
        (let [current-plug (nth sorted-plugs index)
              next-plug (nth sorted-plugs (inc index))
              diff (- next-plug current-plug)]
          (recur (inc index) (update diffs (keyword (str diff)) inc)))
        ))))

(defn part-1 []
  (ir/with-lines "src/advent_of_code/inputs/day10.txt"
                 (->> lines
                      (map #(Integer/parseInt %))
                      jolt-differences
                      (#(* (:1 %) (:3 %))))))

(part-1)

; Part 2
; path[]
; for value in [0, max(input)]
;   for diff in [1, 3]
;     if (value - diff) in input
;        path[value] += path[value - diff]

(defn in-vector? [value vector]
  (not (nil? (some #(= value %) vector))))

(defn possible-combinations [input]
  (let [sorted-plugs (sort (conj input 0))]
   (loop [index 1
         paths {0 1}]
    (cond
      (= index (count sorted-plugs))
      (get paths (last sorted-plugs))

      :else
      (let [jolt (nth sorted-plugs index)
            diffs [(- jolt 1) (- jolt 2)(- jolt 3)]
            diffs-in-input (filter #(in-vector? % sorted-plugs) diffs)
            jolt-paths (reduce #(+ %1 (get paths %2)) 0 diffs-in-input)]
        (recur (inc index) (assoc paths jolt jolt-paths)))))))

(defn part-2 []
  (ir/with-lines "src/advent_of_code/inputs/day10.txt"
                 (->> lines
                      (map #(Integer/parseInt %))
                      possible-combinations)))

(part-2)