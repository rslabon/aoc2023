(ns aoc2023.day11_test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]))

(def example-input "...#......\n.......#..\n#.........\n..........\n......#...\n.#........\n.........#\n..........\n.......#..\n#...#.....")
(def expanded-example "....#........\n.........#...\n#............\n.............\n.............\n........#....\n.#...........\n............#\n.............\n.............\n.........#...\n#....#.......")
(def puzzle-input (slurp "resources/day11.txt"))

(defn rotate
  [s]
  (loop [lines (map reverse (str/split-lines s))
         result []]
    (if (empty? (first lines))
      (str/join "\n" (map #(str/join %) result))
      (recur (map rest lines) (conj result (map first lines)))
      )
    )
  )

(defn expand-horizontally
  [input]
  (let [lines (str/split-lines input)
        width (count (first lines))
        empty-galaxy (str/join (repeat width "."))
        lines (mapcat #(if (= % empty-galaxy) [% empty-galaxy] [%]) lines)]
    (str/join "\n" lines)
    )
  )

(defn expand-galaxies
  [input]
  (-> input
      (expand-horizontally)
      (rotate)
      (expand-horizontally)
      (rotate)
      (rotate)
      (rotate))
  )

(defn parse-graph
  [input]
  (let [lines (str/split-lines input)
        grid (map #(str/split % #"") lines)
        nodes (map-indexed
                (fn [i row]
                  (map-indexed (fn [j cell] {:coord [i j] :cell (str cell)}) row)
                  ) grid)
        ]
    (flatten nodes)
    )
  )

(defn manhattan-distance
  [[sx sy] [tx ty]]
  (+ (abs (- sx tx)) (abs (- sy ty))))

(defn part1
  [input]
  (let [nodes (parse-graph (expand-galaxies input))
        galaxies (remove #(= (:cell %) ".") nodes)
        pairs (for [g1 galaxies g2 galaxies :when (not= g1 g2)] [g1 g2])
        distances (map (fn [[g1 g2]] (manhattan-distance (:coord g1) (:coord g2))) pairs)]
    (/ (apply + distances) 2)
    )
  )

(def count-empty-galaxies-vertically
  (memoize
    (fn
      [input from-inclusive to-inclusive]
      (let [lines (str/split-lines input)
            empty-indexes (filter some? (map-indexed (fn [idx line] (if (every? #(= (str %) ".") line) idx nil)) lines))
            empty-indexes (filter #(and (>= % from-inclusive) (<= % to-inclusive)) empty-indexes)]
        (count empty-indexes)
        )
      )))

(def count-empty-galaxies-horizontally
  (memoize
    (fn
      [input from to]
      (count-empty-galaxies-vertically (-> input (rotate) (rotate) (rotate)) from to)
      )))

(defn galaxy-distance
  [input empty-galaxy-multiplier g1 g2]
  (let [[g1x g1y] (:coord g1)
        [g2x g2y] (:coord g2)
        g1dx (count-empty-galaxies-vertically input 0 (dec g1x))
        g1dx (- (* empty-galaxy-multiplier g1dx) g1dx)
        g1dy (count-empty-galaxies-horizontally input 0 (dec g1y))
        g1dy (- (* empty-galaxy-multiplier g1dy) g1dy)
        g2dx (count-empty-galaxies-vertically input 0 (dec g2x))
        g2dx (- (* empty-galaxy-multiplier g2dx) g2dx)
        g2dy (count-empty-galaxies-horizontally input 0 (dec g2y))
        g2dy (- (* empty-galaxy-multiplier g2dy) g2dy)
        d (manhattan-distance [(+ g1dx g1x) (+ g1dy g1y)] [(+ g2dx g2x) (+ g2dy g2y)])]
    d
    )
  )

(defn part2
  [input empty-galaxy-multiplier]
  (let [nodes (parse-graph input)
        galaxies (remove #(= (:cell %) ".") nodes)
        pairs (for [g1 galaxies g2 galaxies :when (not= g1 g2)] [g1 g2])
        distances (map (fn [[g1 g2]] (galaxy-distance input empty-galaxy-multiplier g1 g2)) pairs)]
    distances (map bigint distances)
    (/ (apply + distances) 2)
    )
  )


(deftest day11-test
  (testing "rotate"
    (is (= (rotate "123\n456\n789") "369\n258\n147"))
    (is (= (rotate "1234\n5678") "48\n37\n26\n15"))
    )
  (testing "expand-galaxies"
    (is (= (expand-galaxies example-input) expanded-example))
    )
  (testing "expand-galaxies"
    (is (= (parse-graph "1") [{:coord [0 0], :cell "1"}]))
    )
  (testing "part1"
    (is (= (part1 example-input) 374))
    (is (= (part1 puzzle-input) 9556896))
    )
  (testing "count-empty-galaxies-vertically"
    (is (= (count-empty-galaxies-vertically example-input 0 9) 2))
    (is (= (count-empty-galaxies-vertically example-input 0 3) 1))
    (is (= (count-empty-galaxies-vertically example-input 0 2) 0))
    (is (= (count-empty-galaxies-vertically example-input 4 9) 1))
    (is (= (count-empty-galaxies-vertically example-input 7 7) 1))
    (is (= (count-empty-galaxies-vertically puzzle-input 0 139) 9))
    (is (= (count-empty-galaxies-vertically puzzle-input 0 52) 0))
    (is (= (count-empty-galaxies-vertically puzzle-input 0 53) 1))
    (is (= (count-empty-galaxies-vertically puzzle-input 0 54) 2))
    )
  (testing "count-empty-galaxies-horizontally"
    (is (= (count-empty-galaxies-horizontally example-input 0 9) 3))
    (is (= (count-empty-galaxies-horizontally example-input 0 3) 1))
    (is (= (count-empty-galaxies-horizontally example-input 0 6) 2))
    (is (= (count-empty-galaxies-horizontally puzzle-input 0 139) 9))
    (is (= (count-empty-galaxies-horizontally puzzle-input 0 20) 1))
    )
  (testing "part2"
    (is (= (part2 example-input 2) 374))
    (is (= (part2 puzzle-input 2) 9556896))
    (is (= (part2 example-input 10) 1030))
    (is (= (part2 example-input 100) 8410))
    (is (= (part2 puzzle-input 1000000) 685038186836))
    )
  )
