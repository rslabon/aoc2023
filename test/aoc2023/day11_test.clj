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
  )
