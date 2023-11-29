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
      (is (= (path-to 3 [0 1 2 3] neighbours edge-cost) [0 1 3]))
      )))