(ns advent-of-code.day13
  (:require [clojure.string :as str]
            [advent-of-code.inputs.input-reader :as ir]))

(def input [
            "939"
            "7,13,x,x,59,x,31,19"])

(defn parse-input [input]
  {:my-arrival (Integer/parseInt (first input))
   :bus-ids (map #(Integer/parseInt %) (filter #(not= "x" %) (str/split (last input) #",")))})

(defn close-arrivals [{:keys [my-arrival bus-ids]}]
  (->> bus-ids
       (map (fn [id] {:id id
                      :close-arrival (int (* (Math/ceil (/ my-arrival id)) id))}))
       (sort-by :close-arrival)))

(map #('(% %)) [1 2 3])

(defn part-2 []
  (ir/with-lines "src/advent_of_code/inputs/day13.txt"
                 (let [parsed-input (parse-input lines)
                       my-arrival (:my-arrival parsed-input)
                       close-arrivals (close-arrivals parsed-input)
                       {:keys [id close-arrival]} (first close-arrivals)]
                   (* id (- close-arrival my-arrival)))))

(part-2)