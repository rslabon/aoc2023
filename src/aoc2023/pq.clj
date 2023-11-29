(ns aoc2023.pq
  (:import (java.util PriorityQueue)))


(defn make-pq [cmp] (new PriorityQueue cmp))

(defn add! [pq item] (do (.add pq item) pq))

(defn add-all! [pq items] (do (.addAll pq items) pq))

(defn poll! [pq] (.poll pq))

(defn has? [pq item] (.contains pq item))

(defn empty? [pq] (.isEmpty pq))