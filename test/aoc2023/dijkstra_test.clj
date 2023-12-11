(ns aoc2023.dijkstra-test
  (:require [aoc2023.dijkstra :refer :all]
            [clojure.test :refer :all]))

;Graph
;0->1 cost(1)
;1->2 cost(3)
;1->3 cost(1)
;2->3 cost(4)

(deftest dijkstra-test
  (testing "dijkstra"
    (let [neighbours (fn [v] (condp = v
                               0 [1]
                               1 [2 3]
                               2 [3]
                               3 []))
          edge-cost (fn [v u] (condp = [v u]
                                [0 1] 1
                                [1 2] 3
                                [1 3] 1
                                [2 3] 4
                                ))]
      (is (= (dijkstra 0 [0 1 2 3] neighbours edge-cost) [{0 0, 1 1, 2 4, 3 2}
                                                          {1 0, 2 1, 3 1}]))
      ))
  (testing "path-to"
    (let [neighbours (fn [v] (condp = v
                               0 [1]
                               1 [2 3]
                               2 [3]
                               3 []))
          edge-cost (fn [v u] (condp = [v u]
                                [0 1] 1
                                [1 2] 3
                                [2 3] 4
                                [1 3] 1
                                ))]
      (is (= (path-to 0 3 [0 1 2 3] neighbours edge-cost) [0 1 3]))
      )
    ;1  2  3  4
    ;5  6  7  8
    ;9  10 11 12
    (let [neighbours (fn [v] (condp = v
                               1 [2 5]
                               2 [1 3 6]
                               3 [2 4 7]
                               4 [3 8]
                               5 [1 6 9]
                               6 [2 5 10 7]
                               7 [6 11 8 3]
                               8 [4 7 12]
                               9 [5 10]
                               10 [6 9 11]
                               11 [10 7 12]
                               12 [8 11]
                               ))
          edge-cost (fn [v u] 1)]
      (is (= (path-to 1 12 [1 2 3 4 5 6 7 8 9 10 11 12] neighbours edge-cost) [1 2 3 4 8 12]))
      (is (= (path-to 12 1 [1 2 3 4 5 6 7 8 9 10 11 12] neighbours edge-cost) [12 8 4 3 2 1]))
      (is (= (path-to 1 10 [1 2 3 4 5 6 7 8 9 10 11 12] neighbours edge-cost) [1 2 6 10]))
      (is (= (path-to 10 1 [1 2 3 4 5 6 7 8 9 10 11 12] neighbours edge-cost) [10 6 2 1]))
      )
    ))