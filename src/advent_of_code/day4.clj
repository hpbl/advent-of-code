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

(defn has-required-fields [passport]
  (let [required-fields ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"]]
    (reduce #(and %1 (str/includes? passport %2)) true required-fields)))

(defn validate-passports [validate passports]
  (loop [index 0
         current-passport ""
         valid-passports-count 0]

    (if (>= index (count passports))
      (if (validate current-passport)
        (inc valid-passports-count)
        valid-passports-count)

      (let [line (nth passports index)]
        (if (= line "")
          (if (validate current-passport)
            (recur (inc index) "" (inc valid-passports-count))
            (recur (inc index) "" valid-passports-count))
          (recur (inc index) (str current-passport " " line) valid-passports-count))))))


(defn read-and-validate [validate]
  (let [path "src/advent_of_code/inputs/day4.txt"]
    (ir/with-lines path
                   (->> lines
                        (validate-passports validate)))))

(read-and-validate has-required-fields)

; pt 2
; Count the number of valid passports - those that have all required fields and valid values.
; Continue to treat cid as optional. In your batch file, how many passports are valid?

;passport-fields (str/split passport #" ")

(defn year-validation [year minimum-value maximum-value]
  (let [match (re-matches #"\d{4}" year)]
    (if (some? match)
      (<=
        minimum-value
        (Integer/parseInt match)
        maximum-value)
      false)))

;byr (Birth Year) - four digits; at least 1920 and at most 2002.
(defn byr-validation [byr] (year-validation byr 1920 2002))

;iyr (Issue Year) - four digits; at least 2010 and at most 2020.
(defn iyr-validation [iyr] (year-validation iyr 2010 2020))

;eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
(defn eyr-validation [eyr] (year-validation eyr 2020 2030))

;hgt (Height) - a number followed by either cm or in:
(defn hgt-validation [hgt]
  (let [cm-matcher #"^(\d+)cm$"
        in-matcher #"^(\d+)in$"]
    (cond
      ;If cm, the number must be at least 150 and at most 193.
      (re-matches cm-matcher hgt)
      (<=
        150
        (Integer/parseInt (last (re-matches cm-matcher hgt)))
        193)

      ;If in, the number must be at least 59 and at most 76.
      (re-matches in-matcher hgt)
      (<=
        59
        (Integer/parseInt (last (re-matches in-matcher hgt)))
        76)

      :else
      false)))

;hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
(defn hcl-validation [hcl]
  (not (empty? (re-matches #"^#(\d|[a-f]){6}$" hcl))))

;ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
(defn ecl-validation [ecl]
  (let [allowed-values #{"amb" "blu" "brn" "gry"  "grn" "hzl" "oth"}]
    (contains? allowed-values ecl)))

;pid (Passport ID) - a nine-digit number, including leading zeroes.
(defn pid-validation [pid]
  (= 9 (count pid)))

;cid (Country ID) - ignored, missing or not.
(defn cid-validation [_]
  true)

(def validations
  {"byr" byr-validation
   "iyr" iyr-validation
   "eyr" eyr-validation
   "hgt" hgt-validation
   "hcl" hcl-validation
   "ecl" ecl-validation
   "pid" pid-validation
   "cid" cid-validation})

(defn valid-field? [validations field]
  (let [[field-name value] (str/split field #":")
        validate (validations field-name)]
    (validate value)))

(defn has-required-fields-and-valid-values [validations passport]
  (if (has-required-fields passport)
    (let [passport-fields (drop 1 (str/split passport #" "))]
      (reduce #(and (valid-field? validations %2) %1) true passport-fields))
    false))

(read-and-validate (partial has-required-fields-and-valid-values validations))