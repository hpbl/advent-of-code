(ns advent-of-code.day13
  (:require [clojure.string :as str]
            [advent-of-code.inputs.input-reader :as ir]))

(defn parse-input [input]
  {:my-arrival (Integer/parseInt (first input))
   :bus-ids (map #(Integer/parseInt %) (filter #(not= "x" %) (str/split (last input) #",")))})

(defn close-arrivals [{:keys [my-arrival bus-ids]}]
  (->> bus-ids
       (map (fn [id] {:id id
                      :close-arrival (int (* (Math/ceil (/ my-arrival id)) id))}))
       (sort-by :close-arrival)))

(defn part-1 []
  (ir/with-lines "src/advent_of_code/inputs/day13.txt"
                 (let [parsed-input (parse-input lines)
                       my-arrival (:my-arrival parsed-input)
                       close-arrivals (close-arrivals parsed-input)
                       {:keys [id close-arrival]} (first close-arrivals)]
                   (* id (- close-arrival my-arrival)))))

(part-1)

; PART 2:
; What is the earliest timestamp such that all of the listed bus IDs depart at offsets matching their positions in the list?
(defn parse-input-2 [input]
  (let [split-input (str/split (last input) #",")]
    (remove nil? (map-indexed
                   (fn [index bus] (cond (not= bus "x") {:bus (Integer/parseInt bus) :offset index}))
                   split-input))))

(defn find-time [parsed-input]
  (loop [step (:bus (first parsed-input))
         time 0
         index 1]

    (cond
      (= index (count parsed-input))
      time

      :else
      (let [{:keys [bus offset]} (nth parsed-input index)
            bus-will-pass? (= (mod (+ time offset) bus) 0)]
        (if bus-will-pass?
          (recur (* step bus) time (inc index))
          (recur step (+ time step) index))))))

(defn part-2 []
  (ir/with-lines "src/advent_of_code/inputs/day13.txt"
                 (->> lines
                      parse-input-2
                      find-time)))

(part-2)