(ns aoc2023.day9-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]))

(def example-input "0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45")

(def puzzle-input (slurp "resources/day9.txt"))

(defn diff
  [numbers]
  (rest (map - numbers (concat [0] numbers)))
  )

(defn all-zeros?
  [numbers]
  (every? #(= 0 %) numbers))

(defn generate-diffs
  [numbers]
  (loop [diffs [numbers]]
    (if (all-zeros? (last diffs))
      diffs
      (recur (conj diffs (diff (last diffs))))
      )
    )
  )

(defn extrapolate-diffs
  [diffs]
  (let [extrapolated-diffs (vec (reverse diffs))]
    ;(println extrapolated-diffs)
    (loop [result []
           idx 0]
      (if (>= idx (count diffs))
        (reverse result)
        (let [current-diff (nth extrapolated-diffs idx)]
          (recur (if (= idx 0)
                   (conj result (conj current-diff 0))
                   (let [bellow-diff (last result)
                         last-number-in-bellow (last bellow-diff)
                         last-number-in-current (last current-diff)
                         current-diff (concat current-diff [(+ last-number-in-current last-number-in-bellow)])
                         ;_ (println bellow-diff current-diff last-number-in-bellow last-number-in-current)
                         ]
                     (conj result current-diff))
                   )
                 (inc idx)))
        )
      )
    )

  )

(defn predict-next-number
  [numbers]
  (let [diffs (-> (generate-diffs numbers)
                  (extrapolate-diffs))]
    ;(println diffs)
    (last (first diffs)))
  )

(defn parse
  [input]
  (let [lines (str/split-lines input)]
    (map #(map read-string (str/split % #" ")) lines))
  )

(defn part1
  [input]
  (let [histories (parse input)
        next-numbers (map predict-next-number histories)]
    (reduce + next-numbers)
    )
  )

(deftest day10-test
  (testing "parse"
    (is (= (parse example-input) [[0 3 6 9 12 15]
                                  [1 3 6 10 15 21]
                                  [10 13 16 21 30 45]]))
    )
  (testing "diff"
    (is (= (diff [1 2 3 4 5 6]) [1 1 1 1 1]))
    (is (= (diff [3 3 3 3 3]) [0 0 0 0]))
    (is (= (diff [0 0 0]) [0 0]))
    (is (= (diff [1 1 1]) [0 0]))
    (is (= (diff [3 0 2]) [-3 2]))
    (is (= (diff [1 10]) [9]))
    (is (= (diff [10 1]) [-9]))
    )
  (testing "generate-diffs"
    (is (= (generate-diffs [0 3 6 9 12 15]) [[0 3 6 9 12 15]
                                             [3 3 3 3 3]
                                             [0 0 0 0]]))
    (is (= (generate-diffs [1 3 6 10 15 21]) [[1 3 6 10 15 21]
                                              [2 3 4 5 6]
                                              [1 1 1 1]
                                              [0 0 0]]))
    )
  (testing "extrapolate-diffs"
    (is (= (extrapolate-diffs [[0 3 6 9 12 15]
                               [3 3 3 3 3]
                               [0 0 0 0]]) [[0 3 6 9 12 15 18]
                                            [3 3 3 3 3 3]
                                            [0 0 0 0 0]]))
    (is (= (extrapolate-diffs [[1 3 6 10 15 21]
                               [2 3 4 5 6]
                               [1 1 1 1]
                               [0 0 0]]) [[1 3 6 10 15 21 28]
                                          [2 3 4 5 6 7]
                                          [1 1 1 1 1]
                                          [0 0 0 0]]))
    )
  (testing "predict-next-number"
    (is (= (predict-next-number [0 3 6 9 12 15]) 18))
    (is (= (predict-next-number [1 3 6 10 15 21]) 28))
    (is (= (predict-next-number [10 13 16 21 30 45]) 68))
    (is (= (predict-next-number [3 2]) 1))
    (is (= (predict-next-number [2 1]) 0))
    (is (= (predict-next-number [1 0]) -1))
    )
  (testing "part1"
    (is (= (part1 example-input) 114))
    (is (= (part1 puzzle-input) 2043677056))
    )
  )