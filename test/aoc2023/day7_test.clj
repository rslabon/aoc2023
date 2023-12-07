(ns aoc2023.day7-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]))

(def example-input "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483")
(def puzzle-input (slurp "resources/day7.txt"))

(def cards-to-strength {"A" 14, "K" 13, "Q" 12, "J" 11, "T" 10, "9" 9, "8" 8, "7" 7, "6" 6, "5" 5, "4" 4, "3" 3, "2" 2})
(def cards-to-strength-v2 {"A" 14, "K" 13, "Q" 12, "J" -1, "T" 10, "9" 9, "8" 8, "7" 7, "6" 6, "5" 5, "4" 4, "3" 3, "2" 2})
(def type-to-strength {:five-of-a-kind 6, :four-of-a-kind 5, :full-house 4, :three-of-a-kind 3, :two-pair 2, :one-pair 1, :high-card 0})

(defn get-type
  [hand]
  (let [cards (str/split hand #"")
        counts (sort (map count (vals (group-by identity cards))))]
    (condp = counts
      [1 1 1 1 1] :high-card
      [1 1 1 2] :one-pair
      [1 2 2] :two-pair
      [1 1 3] :three-of-a-kind
      [2 3] :full-house
      [1 4] :four-of-a-kind
      [5] :five-of-a-kind
      )
    )
  )

(defn get-type-v2
  [hand]
  (let [cards (str/split hand #"")
        cards-group (group-by identity cards)
        number-of-jokers (count (get cards-group "J" []))]
    (if (= number-of-jokers (count cards))
      :five-of-a-kind
      (let [counts (sort (map count (vals (group-by identity (filter #(not= (str %) "J") cards)))))
            counts (reverse counts)
            max (first counts)
            others (rest counts)
            counts (reverse (concat [(+ max number-of-jokers)] others))]
        (condp = counts
          [1 1 1 1 1] :high-card
          [1 1 1 2] :one-pair
          [1 2 2] :two-pair
          [1 1 3] :three-of-a-kind
          [2 3] :full-house
          [1 4] :four-of-a-kind
          [5] :five-of-a-kind
          )
        )
      )
    )
  )

(defn is-stronger?
  [strength get-type-fn]
  (fn [hand1 hand2]
    (let [type-strength1 (get type-to-strength (get-type-fn hand1))
          type-strength2 (get type-to-strength (get-type-fn hand2))]
      (if (= type-strength1 type-strength2)
        (loop [h1 hand1
               h2 hand2]
          (if (= (first h1) (first h2))
            (recur (rest h1) (rest h2))
            (> (get strength (str (first h1))) (get strength (str (first h2))))
            ))
        (> type-strength1 type-strength2)
        )
      )))

(defn parse-camel-cards
  [input]
  (let [camel-cards (map (fn [line] (str/split line #" ")) (str/split-lines input))
        camel-cards (map (fn [[hand bid]] [hand (read-string bid)]) camel-cards)]
    camel-cards
    )
  )

(defn rank
  [sort-fn hands]
  (let [ranked-hands (vec (reverse (sort-by identity sort-fn hands)))
        ranks (into {} (map-indexed (fn [idx i] [i (inc idx)]) ranked-hands))]
    ranks
    )
  )


(defn part1
  [input]
  (let [camel-cards (parse-camel-cards input)
        hands (map first camel-cards)
        ranked-hands (rank (is-stronger? cards-to-strength get-type) hands)
        bids-by-hand (into {} camel-cards)]
    (apply + (map #(* (bigint (get ranked-hands %)) (bigint (get bids-by-hand %))) hands))
    )
  )

(defn part2
  [input]
  (let [camel-cards (parse-camel-cards input)
        hands (map first camel-cards)
        ranked-hands (rank (is-stronger? cards-to-strength-v2 get-type-v2) hands)
        bids-by-hand (into {} camel-cards)]
    (apply + (map #(* (bigint (get ranked-hands %)) (bigint (get bids-by-hand %))) hands))
    )
  )

(deftest day7-test
  (testing "parse"
    (is (= (parse-camel-cards example-input) [["32T3K" 765]
                                              ["T55J5" 684]
                                              ["KK677" 28]
                                              ["KTJJT" 220]
                                              ["QQQJA" 483]]))
    )
  (testing "get-type"
    (is (= (get-type "23456") :high-card))
    (is (= (get-type "A23A4") :one-pair))
    (is (= (get-type "23432") :two-pair))
    (is (= (get-type "TTT98") :three-of-a-kind))
    (is (= (get-type "23332") :full-house))
    (is (= (get-type "2AAAA") :four-of-a-kind))
    (is (= (get-type "AAAAA") :five-of-a-kind))
    )
  (testing "is-stronger?"
    (is (= ((is-stronger? cards-to-strength get-type) "33332" "2AAAA") true))
    (is (= ((is-stronger? cards-to-strength get-type) "33233" "2AAAA") true))
    (is (= ((is-stronger? cards-to-strength get-type) "33233" "A2AAA") false))
    (is (= ((is-stronger? cards-to-strength get-type) "A2AAA" "33233") true))
    (is (= ((is-stronger? cards-to-strength get-type) "2AAAA" "33332") false))
    (is (= ((is-stronger? cards-to-strength get-type) "23456" "A23A4") false))
    )
  (testing "rank"
    (is (= (rank (is-stronger? cards-to-strength get-type) ["32T3K", "KTJJT", "KK677", "T55J5", "QQQJA"]) {"32T3K" 1, "KTJJT" 2, "KK677" 3, "T55J5" 4, "QQQJA" 5}))
    )
  (testing "part1"
    (is (= (part1 example-input) 6440))
    (is (= (part1 puzzle-input) 250898830))
    )
  (testing "get-type-v2"
    (is (= (get-type-v2 "QJJQ2") :four-of-a-kind))
    (is (= (get-type-v2 "JKKK2") :four-of-a-kind))
    (is (= (get-type-v2 "JKKKK") :five-of-a-kind))
    (is (= (get-type-v2 "J2233") :full-house))
    (is (= (get-type-v2 "J2345") :one-pair))
    (is (= (get-type-v2 "JJ345") :three-of-a-kind))
    (is (= (get-type-v2 "KTJJT") :four-of-a-kind))
    (is (= (get-type-v2 "T55J5") :four-of-a-kind))
    (is (= (get-type-v2 "JJJJJ") :five-of-a-kind))
    )
  (testing "is-stronger-v2?"
    (is (= ((is-stronger? cards-to-strength-v2 get-type-v2) "KTJJT" "T55J5") true))
    (is (= ((is-stronger? cards-to-strength-v2 get-type-v2) "KTJJT" "QQQJA") true))
    (is (= ((is-stronger? cards-to-strength-v2 get-type-v2) "QQQJA" "T55J5") true))
    (is (= ((is-stronger? cards-to-strength-v2 get-type-v2) "T55J5" "QQQJA") false))
    (is (= ((is-stronger? cards-to-strength-v2 get-type-v2) "JJJJJ" "QQQQQ") false))
    (is (= ((is-stronger? cards-to-strength-v2 get-type-v2) "QQQQQ" "JJJJJ") true))
    )
  (testing "part2"
    (is (= (part2 example-input) 5905))
    (is (= (part2 puzzle-input) 252127335))
    )
  )
