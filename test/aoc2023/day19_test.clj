(ns aoc2023.day19-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]))

(def example-workflows-input "px{a<2006:qkq,m>2090:A,rfg}\npv{a>1716:R,A}\nlnx{m>1548:A,A}\nrfg{s<537:gd,x>2440:R,A}\nqs{s>3448:A,lnx}\nqkq{x<1416:A,crn}\ncrn{x>2662:A,R}\nin{s<1351:px,qqz}\nqqz{s>2770:qs,m<1801:hdj,R}\ngd{a>3333:R,R}\nhdj{m>838:A,pv}")
(def example-input (str example-workflows-input "\n\n{x=787,m=2655,a=1222,s=2876}\n{x=1679,m=44,a=2067,s=496}\n{x=2036,m=264,a=79,s=2244}\n{x=2461,m=1339,a=466,s=291}\n{x=2127,m=1623,a=2188,s=1013}"))
(def puzzle-input (slurp "resources/day19.txt"))

(defn parse-workflow
  [line]
  (let [line (-> line
                 (str/replace #"\{" " ")
                 (str/replace #"\}" ""))
        [name line] (str/split line #" ")
        rules (str/split line #",")
        adj (mapv (fn [rule]
                    (if (str/includes? rule ":")
                      (let [[_ rule-category rule-condition rule-value rule-goto] (re-matches #"(\w)([><])(\d+):(\w+)" rule)
                            rule-value (read-string rule-value)]
                        [rule-goto (str "(" rule-condition " " rule-category " " rule-value ")")]
                        )
                      [rule :pass])) rules)
        ]
    {:name name :rules rules :adj adj}
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

(defn negate-condition
  [condition]
  (if (str/includes? condition ">")
    (str/replace condition ">" "<=")
    (str/replace condition "<" ">=")
    )
  )

(defn make-accepted-paths
  [workflows-by-name current-workflow-name conditions]
  (cond
    (= current-workflow-name "A") {:conditions (reverse conditions)}
    (= current-workflow-name "R") nil
    :else (let [current-workflow (get workflows-by-name current-workflow-name)]
            (map-indexed
              (fn [idx [name condition]]
                (let [prev-conditions (if (> idx 0)
                                        (take idx (:adj current-workflow))
                                        [])
                      new-conditions (map (fn [[_ v]] (negate-condition v)) prev-conditions)
                      new-conditions (if (empty? new-conditions)
                                       [condition]
                                       (conj new-conditions condition))
                      new-conditions (remove #(= % :pass) new-conditions)
                      ;_ (println "parent " current-workflow-name " current " name  idx " xxxx " prev-conditions  " next " new-conditions)
                      ]
                  (make-accepted-paths workflows-by-name name (into conditions new-conditions))
                  )
                )
              (:adj current-workflow))
            )
    )
  )

(defn find-accepted-conditions
  [workflows-input]
  (let [workflows (parse-workflows workflows-input)
        workflows-by-name (group-by :name workflows)
        workflows-by-name (reduce-kv (fn [m k v] (assoc m k (first v))) {} workflows-by-name)
        conditions-to-be-accepted (flatten (make-accepted-paths workflows-by-name "in" (list)))
        conditions-to-be-accepted (filter some? (map :conditions conditions-to-be-accepted))
        ;_ (println (first conditions-to-be-accepted))
        ]
    conditions-to-be-accepted
    ))


;takes forever to complete - refactoring needed!!!
(defn count-possible-numbers-that-passes
  [conditions]
  (let [s-conditions (filter #(str/includes? % "s") conditions)
        x-conditions (filter #(str/includes? % "x") conditions)
        m-conditions (filter #(str/includes? % "m") conditions)
        a-conditions (filter #(str/includes? % "a") conditions)
        s-count (if (empty? s-conditions)
                  4000
                  (count (filter (fn [s] (if (eval (read-string (str "(let [s " s "] (and " (str/join " " s-conditions) "))"))) true false)) (range 1 4001))))
        a-count (if (empty? a-conditions)
                  4000
                  (count (filter (fn [a] (if (eval (read-string (str "(let [a " a "] (and " (str/join " " a-conditions) "))"))) true false)) (range 1 4001))))
        m-count (if (empty? m-conditions)
                  4000
                  (count (filter (fn [m] (if (eval (read-string (str "(let [m " m "] (and " (str/join " " m-conditions) "))"))) true false)) (range 1 4001))))
        x-count (if (empty? x-conditions)
                  4000
                  (count (filter (fn [x] (if (eval (read-string (str "(let [x " x "] (and " (str/join " " x-conditions) "))"))) true false)) (range 1 4001))))
        ]
    (* s-count a-count m-count x-count)
    )
  )

(defn part2
  [input]
  (let [[workflows-input ratings-input] (str/split input #"\n\n")
        conditions (find-accepted-conditions workflows-input)
        total-numbers (map-indexed
                        (fn [idx condition]
                          (do (println "Processing " (inc idx) "/" (count conditions))
                              (count-possible-numbers-that-passes condition)))
                        conditions)
        ]
    (apply + total-numbers)
    )
  )

(defn number-range
  [conditions]
  (let [less (map (fn [condition] (let [[_ value] (re-matches #"(\d+)" condition)] (read-string value))) (filter #(str/includes? % "<") conditions))
        less (if less (map dec less) [4000])
        less (apply min less)
        greater (map (fn [condition] (let [[_ value] (re-matches #"(\d+)" condition)] (read-string value))) (filter #(str/includes? % ">") conditions))
        greater (if greater (map inc greater) [1])
        greater (apply max greater)
        ]
    (if (< less  greater)
      nil
      [greater less]
      )
    )
  )

(deftest day19-test
  (testing "day19"
    (is (= (parse-workflow "px{a<2006:qkq,m>2090:A,rfg}") {:adj   [["qkq" "(< a 2006)"]
                                                                   ["A" "(> m 2090)"]
                                                                   ["rfg" :pass]]
                                                           :name  "px"
                                                           :rules ["a<2006:qkq" "m>2090:A" "rfg"]}))
    (is (= (parse-rating "{x=787,m=2655,a=1222,s=2876}") {"x" 787 "m" 2655 "a" 1222 "s" 2876}))
    (is (= (accept {"x" 787 "m" 2655 "a" 1222 "s" 2876} "s<1351:px") nil))
    (is (= (accept {"x" 1679 "m" 44 "a" 2067 "s" 496} "s<1351:px") "px"))
    (is (= (accept {"x" 1679 "m" 44 "a" 2067 "s" 496} "px") "px"))
    (is (= (next-workflow-name {"x" 787 "m" 2655 "a" 1222 "s" 2876} (parse-workflow "in{s<1351:px,qqz}")) "qqz"))
    (is (= (part1 example-input) 19114))
    (is (= (part1 puzzle-input) 406934))
    (is (= (count-possible-numbers-that-passes ["(< s 10)" "(>= s 10)"]) 0))
    (is (= (negate-condition "(< s 1351)") "(>= s 1351)"))
    (is (= (negate-condition "(> s 1351)") "(<= s 1351)"))

    ;(is (= (number-range ["(> s 2)" "(< s 10)"]) [3 9]))

    ;(is (= (part2 example-input) 167409079868000))
    ;(is (= (part2 puzzle-input) 131192538505367))
    ;optymalization needed!!! part 2 takes 1h !!!
    ))