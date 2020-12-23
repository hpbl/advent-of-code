(ns advent-of-code.day14
  (:require [clojure.string :as str]
            [advent-of-code.inputs.input-reader :as ir]))

; Part 1
; Execute the initialization program.
; What is the sum of all values left in memory after it completes? (Do not truncate the sum to 36 bits.)
(defn last-from-space-split [line]
  (last (str/split line #" ")))

(defn first-from-space-split [line]
  (first (str/split line #" ")))

(def instruction-regex #"^mem\[(\d+)\]\s=\s(\d+)")

(defn integer->paddedBinary [number]
  (let [binary (Integer/toBinaryString number)
        padding-needed (- 36 (count binary))]
    (str (apply str (repeat padding-needed "0")) binary)))

(defn parse-write-instruction [line]
  (let [matches (re-matches instruction-regex line)]
    {:address (Integer/parseInt (second matches))
     :value (integer->paddedBinary (Integer/parseInt (last matches)))}))

(defn parse-line [line]
  (let [mask? (= (first-from-space-split line) "mask")]
    (if mask?
      {:mask (last-from-space-split line)}
      (parse-write-instruction line))))

(defn parse-input [input]
  (map parse-line input))

(defn apply-mask [mask value]
  (apply str (map (fn [operator val]
                    (cond (= operator \X) val :else operator))
                  mask
                  value)))

(defn add-new-masked-instruction [masked-instructions instruction mask]
  (assoc masked-instructions (:address instruction) (apply-mask mask (:value instruction))))

(defn mask-instructions [masking-f program]
  (loop [index 0
         mask ""
         masked-instructions {}]

    (cond
      (= index (count program))
      masked-instructions

      :else
      (let [instruction (nth program index)
            new-mask (:mask instruction)]
        (if (nil? new-mask)
          (recur (inc index) mask (masking-f masked-instructions instruction mask))
          (recur (inc index) new-mask masked-instructions))))))

(defn execute-program [masked-instructions]
    (reduce-kv #(+ %1 (Long/parseLong %3 2)) 0 masked-instructions))

(defn part-1 []
  (ir/with-lines "src/advent_of_code/inputs/day14.txt"
                 (->> lines
                      parse-input
                      (mask-instructions add-new-masked-instruction)
                      execute-program)))


; Part 2
(defn apply-memory-mask [mask memory-address]
  (apply str (map (fn [operator val]
                    (cond
                      (= operator \0) val
                      :else operator))
                  mask
                  (integer->paddedBinary memory-address))))

(defn resolve-address [masked-address]
  (if (not (str/includes? masked-address "X"))
    [masked-address]
    (flatten (conj
      (resolve-address (str/replace-first masked-address "X" 1))
      (resolve-address (str/replace-first masked-address "X" 0))))))

(defn add-new-masked-memory-instructions [masked-instructions instruction mask]
  (let [masked-address (apply-memory-mask mask (:address instruction))
        actual-addresses (resolve-address masked-address)
        value (:value instruction)]
    (reduce #(assoc %1 %2 value) masked-instructions actual-addresses)))

(defn part-2 []
  (ir/with-lines "src/advent_of_code/inputs/day14.txt"
                 (->> lines
                      parse-input
                      (mask-instructions add-new-masked-memory-instructions)
                      execute-program)))