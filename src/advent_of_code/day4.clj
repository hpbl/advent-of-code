(ns advent-of-code.day4
  (:require [advent-of-code.inputs.input-reader :as ir]
            [clojure.string :as str]))

; Count the number of valid passports - those that have all required fields.
;byr (Birth Year)
;iyr (Issue Year)
;eyr (Expiration Year)
;hgt (Height)
;hcl (Hair Color)
;ecl (Eye Color)
;pid (Passport ID)
;cid (Country ID)
; Treat cid as optional. In your batch file, how many passports are valid?

(defn validate-passport [passport]
  (let [required-fields ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"]]
    (reduce #(and %1 (str/includes? passport %2)) true required-fields)))

(defn validate-passports [passports]
  (loop [index 0
         current-passport ""
         valid-passports-count 0]

    (if (>= index (count passports))
      (if (validate-passport current-passport)
        (inc valid-passports-count)
        valid-passports-count)

      (let [line (nth passports index)]
        (if (= line "")
          (if (validate-passport current-passport)
            (recur (inc index) "" (inc valid-passports-count))
            (recur (inc index) "" valid-passports-count))
          (recur (inc index) (str current-passport line) valid-passports-count))))))


(defn read-input []
  (let [path "src/advent_of_code/inputs/day4.txt"]
    (ir/with-lines path
                   (->> lines
                        validate-passports))))

(read-input)