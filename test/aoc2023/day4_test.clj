(ns aoc2023.day4-test
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer :all]))

(def example-input "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\nCard 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\nCard 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\nCard 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\nCard 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\nCard 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(def puzzle-input (slurp "resources/day4.txt"))

(defn parse-line
  [line]
  (let [[part1 part2] (str/split line #":")
        card-id (read-string (re-find #"\d+" part1))
        [winning-numbers my-numbers] (str/split part2 #"\|")
        my-numbers (str/split my-numbers #"\s+")
        my-numbers (filter not-empty (mapv str/trim my-numbers))
        my-numbers (mapv read-string my-numbers)
        winning-numbers (str/split winning-numbers #"\s+")
        winning-numbers (filter not-empty (mapv str/trim winning-numbers))
        winning-numbers (mapv read-string winning-numbers)]
    {:card card-id, :my-numbers my-numbers, :winning-numbers winning-numbers}
    )
  )

(defn parse-cards
  [input]
  (map parse-line (str/split-lines input))
  )

(defn double-n-times
  [n]
  (if (<= n 0)
    0
    (loop [n n
           acc 1]
      (if (= n 1)
        acc
        (recur (dec n) (* acc 2))
        )
      )
    ))

(defn score-card-in-points
  [card]
  (let [{my-numbers :my-numbers, winning-numbers :winning-numbers} card
        my-numbers (set my-numbers)
        winning-numbers (set winning-numbers)
        found-numbers (set/intersection winning-numbers my-numbers)
        score (double-n-times (count found-numbers))]
    score
    )
  )

(def score-card-in-cards
  (memoize (fn [cards card]
             (let [{card-id :card, my-numbers :my-numbers, winning-numbers :winning-numbers} card
                   my-numbers (set my-numbers)
                   winning-numbers (set winning-numbers)
                   found-numbers (set/intersection winning-numbers my-numbers)
                   copy-ids (range (inc card-id) (+ 1 card-id (count found-numbers)))
                   cards-by-id (group-by :card cards)
                   copy-cards (map #(first (get cards-by-id %)) copy-ids)
                   score (assoc {} card-id 1)
                   copy-scores (map #(score-card-in-cards cards %) copy-cards)
                   copy-scores (apply merge-with + copy-scores)
                   score (if (nil? copy-scores) score (merge-with + score copy-scores))]
               score
               )
             )))


(defn part1
  [input]
  (let [cards (parse-cards input)
        scores (map score-card-in-points cards)]
    (reduce + scores)
    )
  )

(defn part2
  [input]
  (let [cards (parse-cards input)
        scores (map #(score-card-in-cards cards %) cards)
        scores (apply merge-with + scores)]
    (reduce + (vals scores))
    )
  )

(deftest day4-test
  (testing "parse-cards"
    (is (= (parse-cards "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53") [{:card 1, :my-numbers [83 86 6 31 17 9 48 53], :winning-numbers [41 48 83 86 17]}]))
    )
  (testing "score-card"
    (is (= (score-card-in-points {:card 1, :my-numbers [83 86 6 31 17 9 48 53], :winning-numbers [41 48 83 86 17]}) 8))
    (is (= (score-card-in-points {:card 1, :my-numbers [1 2 3], :winning-numbers [4]}) 0))
    )
  (testing "part1"
    (is (= (part1 example-input) 13))
    (is (= (part1 puzzle-input) 24733))
    )
  (testing "score-card-in-cards"
    (is (= (score-card-in-cards [{:card 2, :my-numbers [], :winning-numbers []},
                                 {:card 3, :my-numbers [], :winning-numbers []},
                                 {:card 4, :my-numbers [], :winning-numbers []},
                                 {:card 5, :my-numbers [], :winning-numbers []}]
                                {:card 1, :my-numbers [83 86 6 31 17 9 48 53], :winning-numbers [41 48 83 86 17]})
           {1 1, 2 1, 3 1, 4 1, 5 1}
           ))
    )
  (testing "part2"
    (is (= (part2 example-input) 30))
    (is (= (part2 puzzle-input) 5422730))
    )
  )