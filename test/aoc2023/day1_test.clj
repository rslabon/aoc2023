(ns aoc2023.day1-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all])
  )


(def puzzle-input (slurp "resources/day1.txt"))

(defn parse-digits-first
  [line]
  (str/replace-first line #"one|two|three|four|five|six|seven|eight|nine|[0-9]"
                     #(condp = %
                        "one" "1"
                        "two" "2"
                        "three" "3"
                        "four" "4"
                        "five" "5"
                        "six" "6"
                        "seven" "7"
                        "eight" "8"
                        "nine" "9"
                        % %
                        ))
  )

(defn parse-digits-last
  [line]
  (str/reverse
    (str/replace-first (str/reverse line)
                       (re-pattern (str (str/reverse "one|two|three|four|five|six|seven|eight|nine") "|[0-9]"))
                       #(condp = %
                          (str/reverse "one") "1"
                          (str/reverse "two") "2"
                          (str/reverse "three") "3"
                          (str/reverse "four") "4"
                          (str/reverse "five") "5"
                          (str/reverse "six") "6"
                          (str/reverse "seven") "7"
                          (str/reverse "eight") "8"
                          (str/reverse "nine") "9"
                          % %
                          )))
  )

(defn parse-digits
  [line]
  (str/replace (-> line (parse-digits-first) (parse-digits-last)) #"[a-z]" "")
  )

(defn find-celebration-value
  [line number-parser]
  (let [numbers (number-parser line)
        first-number (first numbers)
        last-number (last numbers)
        value (if (nil? last-number)
                (read-string (apply str (repeat 2 first-number)))
                (read-string (str first-number last-number))
                )]
    value
    ))

(defn find-celebration-value-1
  [line]
  (find-celebration-value line (fn [line] (str/replace line #"[a-z]" "")))
  )

(defn find-celebration-value-2
  [line]
  (find-celebration-value line (fn [line] (str/replace (-> line (parse-digits-first) (parse-digits-last)) #"[a-z]" "")))
  )

(defn solve
  [input finder]
  (let [lines (filter not-empty (str/split-lines input))
        values (map finder lines)]
    (reduce + values)
    )
  )

(defn part-1
  [input]
  (solve input find-celebration-value-1)
  )

(defn part-2
  [input]
  (solve input find-celebration-value-2)
  )

(deftest part1-test
  (testing "part1"
    (is (= (find-celebration-value-1 "1abc2") 12))
    (is (= (find-celebration-value-1 "pqr3stu8vwx") 38))
    (is (= (find-celebration-value-1 "a1b2c3d4e5f") 15))
    (is (= (find-celebration-value-1 "treb7uchet") 77))
    (is (= (part-1 "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet") 142))
    (is (= (part-1 puzzle-input) 54632))
    )
  (testing "part2"
    (is (= (parse-digits-last "oneoneone") "oneone1"))
    (is (= (parse-digits "one") "1"))
    (is (= (parse-digits "7twoneklt") "71"))
    (is (= (parse-digits "x7twoneklt") "71"))
    (is (= (parse-digits "oneone") "11"))
    (is (= (parse-digits "oneonetwo") "12"))
    (is (= (parse-digits "oneonetwonine") "19"))
    (is (= (parse-digits "1one") "11"))
    (is (= (parse-digits "xone") "1"))
    (is (= (parse-digits "xonex") "1"))
    (is (= (parse-digits "eightwothree") "83"))
    (is (= (find-celebration-value-2 "two1nine") 29))
    (is (= (find-celebration-value-2 "eightwothree") 83))
    (is (= (find-celebration-value-2 "7pqrstsixteen") 76))
    (is (= (find-celebration-value-2 "zoneight234") 14))
    (is (= (find-celebration-value-2 "xtwone3four") 24))
    (is (= (find-celebration-value-2 "1sevenseven7ld") 17))
    (is (= (find-celebration-value-2 "hfmqlrvknklhvnxrqztffive6bndcljvgzlnc8") 58))
    (is (= (find-celebration-value-2 "6four3") 63))
    (is (= (part-2 "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen") 281))
    (is (= (part-2 puzzle-input) 54019))
    ))
