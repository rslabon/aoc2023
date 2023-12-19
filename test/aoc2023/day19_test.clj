(ns aoc2023.day19-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]))

(def example-input "px{a<2006:qkq,m>2090:A,rfg}\npv{a>1716:R,A}\nlnx{m>1548:A,A}\nrfg{s<537:gd,x>2440:R,A}\nqs{s>3448:A,lnx}\nqkq{x<1416:A,crn}\ncrn{x>2662:A,R}\nin{s<1351:px,qqz}\nqqz{s>2770:qs,m<1801:hdj,R}\ngd{a>3333:R,R}\nhdj{m>838:A,pv}\n\n{x=787,m=2655,a=1222,s=2876}\n{x=1679,m=44,a=2067,s=496}\n{x=2036,m=264,a=79,s=2244}\n{x=2461,m=1339,a=466,s=291}\n{x=2127,m=1623,a=2188,s=1013}")
(def puzzle-input (slurp "resources/day19.txt"))

(defn parse-workflow
  [line]
  (let [line (-> line
                 (str/replace #"\{" " ")
                 (str/replace #"\}" ""))
        [name line] (str/split line #" ")
        rules (str/split line #",")]
    {:name name :rules rules}
    )
  )

(defn parse-rating
  [line]
  (let [line (-> line
                 (str/replace #"\{" "")
                 (str/replace #"\}" ""))
        rating-parts (str/split line #",")
        ratings (reduce
                  (fn [m part]
                    (let [[category value] (str/split part #"=")]
                      (assoc m category (read-string value))
                      )
                    ) {} rating-parts)]
    ratings
    )
  )

(defn parse-workflows
  [input]
  (map parse-workflow (str/split-lines input))
  )

(defn parse-ratings
  [input]
  (map parse-rating (str/split-lines input))
  )

(defn accept
  [rating rule]
  (if (str/includes? rule ":")
    (let [[_ rule-category rule-condition rule-value rule-goto] (re-matches #"(\w)([><])(\d+):(\w+)" rule)
          rule-value (read-string rule-value)
          rating-value (get rating rule-category)
          passed? (condp = rule-condition
                    "<" (< rating-value rule-value)
                    ">" (> rating-value rule-value)
                    )
          ]
      (if passed?
        rule-goto
        nil
        )
      )
    rule
    )
  )

(defn next-workflow-name
  [rating workflow]
  (let [rules (:rules workflow)]
    (loop [rules rules]
      (if (empty? rules)
        (throw "INVALID - EMPTY RULES!!!")
        (let [rule (first rules)
              rule-result (accept rating rule)]
          (cond
            (= rule-result "R") :reject
            (= rule-result "A") :accept
            (= rule-result nil) (recur (rest rules))
            :else rule-result
            )
          )
        )
      )
    )
  )

(defn follow-workflows
  [rating workflows-by-name current-workflow-name]
  (loop [current-workflow-name current-workflow-name]
    (if (or (= current-workflow-name :reject) (= current-workflow-name :accept))
      current-workflow-name
      (let [current-workflow (get workflows-by-name current-workflow-name)]
        (recur (next-workflow-name rating current-workflow))
        )
      )
    )
  )

(defn part1
  [input]
  (let [[workflows-input ratings-input] (str/split input #"\n\n")
        workflows (parse-workflows workflows-input)
        ratings (parse-ratings ratings-input)
        workflows-by-name (group-by :name workflows)
        workflows-by-name (reduce-kv (fn [m k v] (assoc m k (first v))) {} workflows-by-name)
        follow-results (map (fn [rating] [(follow-workflows rating workflows-by-name "in") rating]) ratings)
        follow-results (group-by first follow-results)
        follow-results (reduce-kv (fn [m k v] (assoc m k (map second v))) {} follow-results)
        accepted-ratings (:accept follow-results)
        accepted-ratings (map #(apply + (vals %)) accepted-ratings)]
    (apply + accepted-ratings)
    )
  )

(deftest day19-test
  (testing "day19"
    (is (= (parse-workflow "px{a<2006:qkq,m>2090:A,rfg}") {:name "px" :rules ["a<2006:qkq" "m>2090:A" "rfg"]}))
    (is (= (parse-rating "{x=787,m=2655,a=1222,s=2876}") {"x" 787 "m" 2655 "a" 1222 "s" 2876}))
    (is (= (accept {"x" 787 "m" 2655 "a" 1222 "s" 2876} "s<1351:px") nil))
    (is (= (accept {"x" 1679 "m" 44 "a" 2067 "s" 496} "s<1351:px") "px"))
    (is (= (accept {"x" 1679 "m" 44 "a" 2067 "s" 496} "px") "px"))
    (is (= (next-workflow-name {"x" 787 "m" 2655 "a" 1222 "s" 2876} (parse-workflow "in{s<1351:px,qqz}")) "qqz"))
    (is (= (part1 example-input) 19114))
    (is (= (part1 puzzle-input) 406934))
    ))