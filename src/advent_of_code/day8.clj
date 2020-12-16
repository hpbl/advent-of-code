(ns advent-of-code.day8
  (:require [clojure.string :as str]
            [advent-of-code.inputs.input-reader :as ir]))


; Part 1
; Run your copy of the boot code. Immediately before any instruction is executed a second time, what value is in the accumulator?
(defn parse-instruction [line]
  (let [instruction (str/split line #" ")]
    {:operator (first instruction)
     :argument (Long/valueOf (second instruction))}))

(defn evaluate-instruction [parsed-instruction index accum]
  (let [{operator :operator argument :argument} parsed-instruction]
    (case operator
      "nop" {:new-index (inc index) :new-accum accum}

      "jmp" {:new-index (+ index argument) :new-accum accum}

      "acc" {:new-index (inc index) :new-accum (+ accum argument)})))

(defn execute [parsed-program]
    (loop [index 0
           accum 0
           visited-indexes []]

      (cond
        ; loop
        (some #(= index %) visited-indexes)
        {:accum accum :looped true :visited-indexes visited-indexes}

        ; no loop
        (>= index (count parsed-program))
        {:accum accum :looped false}

        :else
        (let [current-instruction (nth parsed-program index)
              {:keys [new-index new-accum]} (evaluate-instruction current-instruction index accum)
              new-visited-indexes (conj visited-indexes index)]
          (recur new-index new-accum new-visited-indexes)))))

(defn part-1 []
  (ir/with-lines "src/advent_of_code/inputs/day8.txt"
                 (->> lines
                      (map parse-instruction)
                      execute
                      :accum)))
(part-1)

; Part 2
(defn replace-possible-looping-instruction [parsed-looping-program possible-looping-instruction-index]
  (let [possible-looping-instruction (nth parsed-looping-program possible-looping-instruction-index)]
    (case (:operator possible-looping-instruction)
      "nop" (replace {possible-looping-instruction (assoc possible-looping-instruction :operator "jmp")} parsed-looping-program)
      "jmp" (replace {possible-looping-instruction (assoc possible-looping-instruction :operator "nop")} parsed-looping-program)
      "acc" parsed-looping-program)))

(defn fix-looping-program [parsed-program]
  (let [execution (execute parsed-program)
        execution-order (:visited-indexes execution)]
    (loop [[loop-candidate & rest] execution-order
           last-execution execution]
      (cond
        ; couldn't fix loop
        (nil? loop-candidate)
        last-execution

        ; loop was fixed
        (false? (:looped last-execution))
        last-execution

        ; still looped
        :else
        (let [new-program (replace-possible-looping-instruction parsed-program loop-candidate)
              new-execution (execute new-program)]
          (recur rest new-execution))))))

(defn part-2 []
  (ir/with-lines "src/advent_of_code/inputs/day8.txt"
                 (->> lines
                      (map parse-instruction)
                      fix-looping-program)))

(part-2)