(ns aoc2023.day6-test
  (:require [clojure.test :refer :all]))

(def example-input [[7 9] [15 40] [30 200]])
(def puzzle-input [[50 242] [74 1017] [86 1691] [85 1252]])

(defn distance
  [hold-button time-max]
  (let [time-left (- time-max hold-button)
        d (* hold-button time-left)]
    (if (or (= hold-button 0) (> hold-button time-max))
      0
      d
      )
    )
  )

(defn ways-to-win
  [[time max]]
  (loop [i 1
         count 0]
    (let [d (distance i time)
          count (if (> d max) (inc count) count)]
      (if (= d 0)
        count
        (recur (inc i) count)
        )
      )
    )
  )

(defn part1
  [races]
  (apply * (map ways-to-win races))
  )

(deftest day6-test
  (testing "option"
    (is (= (distance 0 7) 0))
    (is (= (distance 1 7) 6))
    (is (= (distance 2 7) 10))
    (is (= (distance 3 7) 12))
    (is (= (distance 4 7) 12))
    (is (= (distance 5 7) 10))
    (is (= (distance 6 7) 6))
    (is (= (distance 7 7) 0))
    (is (= (distance 8 7) 0))
    )
  (testing "ways-to-win"
    (is (= (ways-to-win [7 9]) 4))
    (is (= (ways-to-win [15 40]) 8))
    (is (= (ways-to-win [30 200]) 9))
    )
  (testing "part1"
    (is (= (part1 example-input) 288))
    (is (= (part1 puzzle-input) 1731600))
    )
  (testing "part2"
    (is (= (part1 [[71530 940200]]) 71503))
    (is (= (part1 [[50748685 242101716911252]]) 40087680))
    )
  )
