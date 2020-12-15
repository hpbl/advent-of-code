(ns advent-of-code.day7
  (:require [advent-of-code.inputs.input-reader :as ir]))

; How many bag colors can eventually contain at least one shiny gold bag?
(def rule-regex #"^(\w+\s\w+)\sbags\scontain((\s\d\s\w+\s\w+\sbags*,*)+).$")
(def inner-regex #"(\d)\s(\w+\s\w+)")

(defn parse-inner-bags [inner-bags regex]
  (let [matches (re-seq regex inner-bags)]
    (map
      #(assoc
         {}
         :amount (Integer/parseInt (second %))
         :color (nth % 2))
      matches)))

(defn parse-rule [regex rule]
  (let [matches (re-matches regex rule)
        outer-bag (second matches)
        inner-bags (nth matches 2)]
    (if (nil? matches)
      {}
      {:outer outer-bag
       :inner-bags (parse-inner-bags inner-bags inner-regex)}
      )))

(defn can-contain-directly? [chosen-color parsed-rule]
  (let [inner-bags (:inner-bags parsed-rule)
        filtered-inner-bags (filter #(= (:color %) chosen-color) inner-bags)]
    (not (empty? filtered-inner-bags))))


(defn color-can-contain? [chosen-color candidate-color parsed-rules]
  (loop [candidate-colors #{candidate-color}
         visited-colors #{}]

    (let [candidate (first candidate-colors)
          other-colors (disj candidate-colors candidate)]

    (cond
      (nil? candidate)
      false

      :else
      (let [candidate-rule (first (filter #(= (:outer %) candidate) parsed-rules))
            can-contain-directly? (can-contain-directly? chosen-color candidate-rule)]
        (if can-contain-directly?
          true
          (let [new-visited-colors (conj visited-colors candidate)
                inner-colors (map :color (:inner-bags candidate-rule))
                inner-colors-not-visited (filter #(not (contains? new-visited-colors %)) inner-colors)
                new-candidate-colors (reduce #(conj %1 %2) other-colors inner-colors-not-visited)]
            (recur new-candidate-colors new-visited-colors))))))))

(defn count-colors-can-contain [desired-color rules]
  (let [parsed-rules (map #(parse-rule rule-regex %) rules)]
    (loop [index 0
           color-count 0]

      (cond
        (= index (count parsed-rules))
        color-count

        :else
        (let [current-rule (nth parsed-rules index)
              candidate-color (:outer current-rule)
              color-can-contain? (color-can-contain? desired-color candidate-color parsed-rules)]
          (if color-can-contain?
            (recur (inc index) (inc color-count))
            (recur (inc index) color-count)))))))

(defn part-1 []
  (ir/with-lines "src/advent_of_code/inputs/day7.txt"
                 (->> lines
                      (count-colors-can-contain "shiny gold"))))
(part-1)


; PART 2
; How many individual bags are required inside your single shiny gold bag?
(defn get-rule-for-bag-color [bag-color parsed-rules]
  (first (filter #(= (:outer %) bag-color) parsed-rules)))

(defn count-inner-bags [bag-color parsed-rules]
  (let [bag-rule (get-rule-for-bag-color bag-color parsed-rules)
        inner-bags (:inner-bags bag-rule)]

    (cond
      (nil? bag-rule)
      0

      :else
      (reduce
        (fn [current-sum inner-bag]
          (+
            current-sum
            (:amount inner-bag)
            (* (:amount inner-bag) (count-inner-bags (:color inner-bag) parsed-rules))))
        0
        inner-bags))))

(defn part-2 []
  (ir/with-lines "src/advent_of_code/inputs/day7.txt"
                 (->> lines
                      (map (partial parse-rule rule-regex))
                      (count-inner-bags "shiny gold"))))
(part-2)







