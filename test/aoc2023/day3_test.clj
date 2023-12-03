(ns aoc2023.day3-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]))

(def example-input "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598..")

(def puzzle-input (slurp "resources/day3.txt"))

(defn is-number?
  [string]
  (re-matches #"\d" string)
  )

(defn get-number-and-id
  [grid r-idx c-idx]
  (if (is-number? (nth (nth grid r-idx) c-idx))
    (let [row (nth grid r-idx)
          column-length (count row)
          [pre pre-idx] (loop [idx c-idx
                               s ""]
                          (if (or (< idx 0) (not (is-number? (nth row idx))))
                            [s (inc idx)]
                            (recur (dec idx) (str (nth row idx) s))))
          [post post-idx] (loop [idx (inc c-idx)
                                 s ""]
                            (if (or (>= idx column-length) (not (is-number? (nth row idx))))
                              [s (dec idx)]
                              (recur (inc idx) (str s (nth row idx)))))
          value (str pre post)]
      (if (empty? value)
        nil
        [(read-string value) (str r-idx pre-idx post-idx)]
        ))
    nil
    ))

(defn parse-cells
  [input]
  (let [lines (vec (str/split-lines input))
        grid (mapv #(vec (str/split % #"")) lines)
        schematic-cells (flatten (map-indexed
                                   (fn [r-idx row]
                                     (map-indexed
                                       (fn [c-index cell]
                                         (let [[number-value number-id] (get-number-and-id grid r-idx c-index)]
                                           {:coords [r-idx c-index],
                                            :cell   cell,
                                            :id     number-id
                                            :number number-value
                                            }
                                           )) row)) grid))
        schematic-cells (filter #(not= "." (:cell %)) schematic-cells)]
    schematic-cells
    ))

(defn get-adj-numeric-cells
  [numeric-by-cells symbolic-cells]
  (let [coords (:coords symbolic-cells)
        dx [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]
        adj-indexes (map #(map + coords %) dx)
        adj-cells (filter some? (map #(get numeric-by-cells %) adj-indexes))]
    (flatten adj-cells)))

(defn get-numbers
  [numeric-cells-by-id]
  (map #(:number (first (get numeric-cells-by-id %))) (keys numeric-cells-by-id))
  )

(defn part1
  [input]
  (let [cells (parse-cells input)
        numeric-cells (filter #(some? (:number %)) cells)
        numeric-cells-by-coords (group-by :coords numeric-cells)
        symbolic-cells (filter #(nil? (:number %)) cells)
        adj-cells (flatten (map #(get-adj-numeric-cells numeric-cells-by-coords %) symbolic-cells))
        numeric-cells-by-id (group-by :id adj-cells)
        numbers (get-numbers numeric-cells-by-id)]
    (reduce + numbers))
  )

(defn gear-ratios
  [numberic-by-coords gear-coords]
  (map #(let [adj-coords (get-adj-numeric-cells numberic-by-coords %)
              adj-by-id (group-by :id adj-coords)
              adj-numbers (get-numbers adj-by-id)]
          (if (> (count adj-numbers) 1)
            (apply * adj-numbers)
            0
            ))
       gear-coords)
  )

(defn part2
  [input]
  (let [cells (parse-cells input)
        numeric-cells (filter #(some? (:number %)) cells)
        numeric-cells-by-coords (group-by :coords numeric-cells)
        gear-cells (filter #(= "*" (str (:cell %))) cells)
        gears-ratios (gear-ratios numeric-cells-by-coords gear-cells)]
    (reduce + gears-ratios)
    )
  )

(deftest day3-test
  (testing "parse-cells"
    (is (= (parse-cells "1234") [{:cell   "1"
                                  :coords [0 0]
                                  :id     "003"
                                  :number 1234}
                                 {:cell   "2"
                                  :coords [0 1]
                                  :id     "003"
                                  :number 1234}
                                 {:cell   "3"
                                  :coords [0 2]
                                  :id     "003"
                                  :number 1234}
                                 {:cell   "4"
                                  :coords [0 3]
                                  :id     "003"
                                  :number 1234}]))
    (is (= (parse-cells "12.") [{:cell   "1"
                                 :coords [0 0]
                                 :id     "001"
                                 :number 12}
                                {:cell   "2"
                                 :coords [0 1]
                                 :id     "001"
                                 :number 12}]))
    (is (= (parse-cells "12.35") [{:cell   "1"
                                   :coords [0 0]
                                   :id     "001"
                                   :number 12}
                                  {:cell   "2"
                                   :coords [0 1]
                                   :id     "001"
                                   :number 12}
                                  {:cell   "3"
                                   :coords [0 3]
                                   :id     "034"
                                   :number 35}
                                  {:cell   "5"
                                   :coords [0 4]
                                   :id     "034"
                                   :number 35}]))
    (is (= (parse-cells "12.*") [{:cell   "1"
                                  :coords [0 0]
                                  :id     "001"
                                  :number 12}
                                 {:cell   "2"
                                  :coords [0 1]
                                  :id     "001"
                                  :number 12}
                                 {:cell   "*"
                                  :coords [0 3]
                                  :id     nil
                                  :number nil}]))
    )
  (testing "part1"
    (is (= (part1 ".+8.\n.12.\n.*6.") 26))
    (is (= (part1 example-input) 4361))
    (is (= (part1 puzzle-input) 535351))
    )
  (testing "part2"
    (is (= (part2 example-input) 467835))
    (is (= (part2 puzzle-input) 87287096))
    )
  )