(ns aoc2023.day8-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]))

(def example-input "RL\n\nAAA = (BBB, CCC)\nBBB = (DDD, EEE)\nCCC = (ZZZ, GGG)\nDDD = (DDD, DDD)\nEEE = (EEE, EEE)\nGGG = (GGG, GGG)\nZZZ = (ZZZ, ZZZ)")
(def example-input2 "LLR\n\nAAA = (BBB, BBB)\nBBB = (AAA, ZZZ)\nZZZ = (ZZZ, ZZZ)")
(def example-input3 "LR\n\n11A = (11B, XXX)\n11B = (XXX, 11Z)\n11Z = (11B, XXX)\n22A = (22B, XXX)\n22B = (22C, 22C)\n22C = (22Z, 22Z)\n22Z = (22B, 22B)\nXXX = (XXX, XXX)")

(def puzzle-input (slurp "resources/day8.txt"))

(defn parse-instructions
  [input]
  (let [[first-line] (str/split input #"\n\n")
        instructions (map #(condp = (str %) "R" :right "L" :left) (str/split first-line #""))]
    (cycle instructions)
    )
  )

(defn parse-node
  [line]
  (let [[_ node left-node right-node] (re-matches #"(\w+)\s*=\s*\((\w+),\s*(\w+)\)" line)]
    (assoc {} node [node left-node right-node])
    )
  )

(defn parse-nodes
  [input]
  (let [[node-lines] (rest (str/split input #"\n\n"))
        node-lines (str/split-lines node-lines)
        ]
    (into {} (map parse-node node-lines))
    )
  )

(defn part1
  [input]
  (let [instructions (parse-instructions input)
        nodes (parse-nodes input)]
    (loop [instructions instructions
           steps 0
           [current-node left-node right-node] (get nodes "AAA")]
      (if (= current-node "ZZZ")
        steps
        (let [instruction (first instructions)
              next-node (condp = instruction
                          :left (get nodes left-node)
                          :right (get nodes right-node)
                          )]
          (recur (rest instructions) (inc steps) next-node)
          )
        )
      )
    ))

(defn find-steps-to-z-nodes
  [instructions nodes node]
  (loop [instructions instructions
         steps 0
         [_ left-node right-node] node]
    (let [current-instruction (first instructions)
          next-node (condp = current-instruction
                      :left (get nodes left-node)
                      :right (get nodes right-node)
                      )]
      (if (str/ends-with? (first next-node) "Z")
        (inc steps)
        (recur (rest instructions) (inc steps) next-node)
        )
      )))

(defn gcd
  [a b] (.gcd (BigInteger/valueOf a) (BigInteger/valueOf b)))

(defn lcm
  ([a b] (/ (* a b) (gcd a b)))
  ([a b c & varargs] (reduce lcm (concat [a b c] varargs)))
  )


(defn part2
  [input]
  (let [instructions (parse-instructions input)
        nodes (parse-nodes input)
        starting-nodes (filter #(str/ends-with? % "A") (keys nodes))
        starting-nodes (map #(get nodes %) starting-nodes)
        z-nodes-steps (map #(find-steps-to-z-nodes instructions nodes %) starting-nodes)]
    (apply lcm z-nodes-steps)
    ))

(deftest day8-test
  (testing "parse"
    (is (= (take 4 (parse-instructions example-input)) [:right :left :right :left]))
    (is (= (parse-nodes "RL\n\nAAA = (BBB, CCC)") {"AAA" ["AAA" "BBB" "CCC"]}))
    (is (= (parse-nodes example-input) {"AAA" ["AAA" "BBB" "CCC"]
                                        "BBB" ["BBB" "DDD" "EEE"]
                                        "CCC" ["CCC" "ZZZ" "GGG"]
                                        "DDD" ["DDD" "DDD" "DDD"]
                                        "EEE" ["EEE" "EEE" "EEE"]
                                        "GGG" ["GGG" "GGG" "GGG"]
                                        "ZZZ" ["ZZZ" "ZZZ" "ZZZ"]}))
    )
  (testing "part1"
    (is (= (part1 example-input) 2))
    (is (= (part1 example-input2) 6))
    (is (= (part1 puzzle-input) 20659))
    )
  (testing "part2"
    (is (= (lcm 21 6) 42))
    (is (= (find-steps-to-z-nodes (parse-instructions example-input3) (parse-nodes example-input3) ["1AA" "11B" "XXX"]) 2))
    (is (= (find-steps-to-z-nodes (parse-instructions example-input3) (parse-nodes example-input3) ["2AA" "22B" "22B"]) 3))
    (is (= (part2 example-input3) 6))
    (is (= (part2 puzzle-input) 15690466351717))
    )
  )
