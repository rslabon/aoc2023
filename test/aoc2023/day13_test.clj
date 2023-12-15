(ns aoc2023.day13_test
  (:require [aoc2023.strings :as ss]
            [clojure.string :as str]
            [clojure.test :refer :all]))

(def example-input1 "#.##..##.\n..#.##.#.\n##......#\n##......#\n..#.##.#.\n..##..##.\n#.#.##.#.")
(def example-input2 "#...##..#\n#....#..#\n..##..###\n#####.##.\n#####.##.\n..##..###\n#....#..#")
(def example-input3 "#.##..##.\n..#.##.#.\n##......#\n##......#\n..#.##.#.\n..##..##.\n#.#.##.#.\n\n#...##..#\n#....#..#\n..##..###\n#####.##.\n#####.##.\n..##..###\n#....#..#")
(def puzzle-input (slurp "resources/day13.txt"))

(defn count-diffs
  [l1 l2]
  (apply + (map (fn [s1 s2]
                  (count (filter #(not= % 0) (map - (map int s1) (map int s2))))
                  ) l1 l2))
  )

(defn match?
  [s1 s2 smudge]
  (let [size (min (count s1) (count s2))]
    (= smudge (count-diffs (take size s1) (take size s2))
       )))


(defn h
  ([lines] (h lines 0))
  ([lines smudge]
   (loop [before (list (first lines))
          after (rest lines)]
     (if (empty? after)
       0
       (if (match? before after smudge)
         (count before)
         (recur (conj before (first after)) (rest after))
         )
       )
     )
   ))

(defn parse-puzzle
  [text] (str/split-lines text))

(defn v
  ([lines] (v lines 0))
  ([lines smudge]
   (h (parse-puzzle (ss/rotate-multi-line-text (str/join "\n" lines))) smudge))
  )

(defn part1
  [input]
  (let [puzzles (str/split input #"\n\n")
        result (map (fn [puzzle]
                      (let [lines (parse-puzzle puzzle)
                            vc (v lines)
                            hc (h lines)
                            ]
                        (+ vc (* 100 hc))
                        )
                      ) puzzles)]
    (apply + result)
    ))

(defn part2
  [input]
  (let [puzzles (str/split input #"\n\n")
        result (map (fn [puzzle]
                      (let [lines (parse-puzzle puzzle)
                            hc (h lines 1)
                            vc (v lines 1)
                            ]
                        (+ vc (* 100 hc))
                        )
                      ) puzzles)]
    (apply + result)
    ))

(def p "#####.#..\n#.#......\n#.#......\n#####.#.#\n#####.#.#\n#.#......\n#.#......\n#####.#..\n.#####...\n#..#...##\n#.#..##..\n####.####\n#...##...")

(deftest day13-test
  (testing "day13"
    (is (= (h ["111", "222"]) 0))
    (is (= (h ["111", "111"]) 1))
    (is (= (h ["111", "222", "222", "333"]) 0))
    (is (= (h ["111", "222", "222", "111"]) 2))
    (is (= (h ["111", "222", "222", "111", "333"]) 2))
    (is (= (h ["333", "111", "222", "222", "111"]) 3))
    (is (= (h ["333", "111", "222", "222", "111", "444"]) 0))
    (is (= (h ["111", "111", "333", "444", "555", "666"]) 1))
    (is (= (h ["555", "444", "333", "222", "111", "111"]) 5))
    (is (= (h ["555", "444", "333", "222", "111", "111"]) 5))
    (is (= (h (parse-puzzle "..###..##..\n.#.#.....##\n#.##..##.#.\n#.#..###..#\n.#..##.##.#\n.#....##..#\n#.####.##.#\n#.####.##.#\n.#....###.#\n.#..##.##.#\n#.#..###..#\n#.##..##.#.\n.#.#.....##\n..###..##..\n..###..##..")) 14))
    (is (= (h (parse-puzzle example-input2)) 4))
    (is (= (v (parse-puzzle example-input2)) 0))
    (is (= (v (parse-puzzle example-input1)) 5))
    (is (= (h (parse-puzzle example-input1)) 0))
    (is (= (part1 example-input3) 405))
    (is (= (part1 puzzle-input) 34993))
    (is (= (part2 example-input3) 400))
    (is (= (part2 puzzle-input) 29341))
    ))

