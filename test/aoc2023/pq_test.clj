(ns aoc2023.pq-test
  (:require [aoc2023.pq :refer :all]
            [clojure.test :refer :all]))

(deftest pq-test
  (testing "pq"
    (let [pq (make-pq (fn [a b] (- a b)))]
      (is (= (empty? pq) true))
      (is (= (poll! (add! pq 23)) 23))
      )
    (let [pq (make-pq (fn [a b] (- b a)))]
      (is (= (poll! (-> pq (add! 1) (add! 2) (add! 3))) 3))
      )
    ))
