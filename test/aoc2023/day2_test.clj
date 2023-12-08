(ns aoc2023.day2-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]))

(def puzzle-input (slurp "resources/day2.txt"))
(def example-input "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")


(defn parse-cube-line
  "parsing line like '3 blue, 4 red' into {\"blue\" 3, \"red\" 4}"
  [line]
  (let [cubes (->> (str/split line #",")
                   (map #(str/split (str/trim %) #" "))
                   (map (fn [[value color]] [color (read-string value)])))]
    (into {} cubes)
    )
  )

(defn parse-cubes
  [line]
  (let [cubes (str/split line #";")]
    (map parse-cube-line cubes)
    )
  )

(defn parse-game
  [line]
  (let [[part1 part2] (str/split line #":")
        id (read-string (re-find #"\d+" part1))
        cubes (parse-cubes part2)]
    {:id id, :cubes cubes}
    )
  )

(defn sum-cubes
  [cubes]
  (apply merge-with + cubes)
  )

(defn max-cubes
  [cubes]
  (apply merge-with max cubes)
  )

(defn has-valid-cubes?
  [game]
  (let [{cubes :cubes} game
        max-of-cubes (max-cubes cubes)
        red (get max-of-cubes "red")
        green (get max-of-cubes "green")
        blue (get max-of-cubes "blue")]
    (and (<= red 12) (<= green 13) (<= blue 14))
    )
  )

(defn part1
  [input]
  (let [lines (str/split-lines input)
        games (->> (map parse-game lines)
                   (filter has-valid-cubes?))
        ids (map :id games)]
    (reduce + ids)
    )
  )

(defn power-of-cubes
  [game]
  (let [{cubes :cubes} game
        max-of-cubes (max-cubes cubes)
        red (get max-of-cubes "red")
        green (get max-of-cubes "green")
        blue (get max-of-cubes "blue")]
    (* red green blue)
    )
  )

(defn part2
  [input]
  (let [lines (str/split-lines input)
        games (map parse-game lines)
        powers (map power-of-cubes games)]
    (reduce + powers)
    )
  )

(deftest day2-test
  (testing "parse-game"
    (is (= (parse-game "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
           {:id 1, :cubes [{"blue" 3, "red" 4}, {"red" 1, "green" 2, "blue" 6}, {"green" 2}]}))
    )
  (testing "sum-cubes"
    (is (= (sum-cubes [{"blue" 3, "red" 4}, {"red" 1, "green" 2, "blue" 6}, {"green" 2}])
           {"blue" 9, "red" 5, "green" 4}))
    )
  (testing "has-valid-cubes?"
    (is (= (has-valid-cubes? {:id 1, :cubes [{"blue" 3, "red" 4}, {"red" 1, "green" 2, "blue" 6}, {"green" 2}]}) true))
    (is (= (has-valid-cubes? {:id 1, :cubes [{"blue" 3, "red" 13}, {"red" 1, "green" 2, "blue" 6}, {"green" 2}]}) false))
    (is (= (has-valid-cubes? {:id 1, :cubes [{"blue" 15, "red" 1}, {"red" 1, "green" 2, "blue" 6}, {"green" 2}]}) false))
    (is (= (has-valid-cubes? {:id 1, :cubes [{"blue" 1, "red" 1}, {"red" 1, "green" 14, "blue" 6}, {"green" 2}]}) false))
    )
  (testing "part1"
    (is (= (part1 example-input) 8))
    (is (= (part1 puzzle-input) 2512))
    )
  (testing "power-of-cubes"
    (is (= (power-of-cubes {:id 1, :cubes [{"blue" 3, "red" 4}, {"red" 1, "green" 2, "blue" 6}, {"green" 2}]}) 48))
    )
  (testing "part2"
    (is (= (part2 example-input) 2286))
    (is (= (part2 puzzle-input) 67335))
    )
  )
