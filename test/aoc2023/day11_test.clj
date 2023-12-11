(ns aoc2023.day11_test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]))

(def example-input "...#......\n.......#..\n#.........\n..........\n......#...\n.#........\n.........#\n..........\n.......#..\n#...#.....")
(def expanded-example "....#........\n.........#...\n#............\n.............\n.............\n........#....\n.#...........\n............#\n.............\n.............\n.........#...\n#....#.......")


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
        lines (mapcat #(if (= % empty-galaxy) [% empty-galaxy] [%]) lines)
        ]
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

(defn possible-adj
  [i j max-i max-j]
  (filter (fn [[a b]] (and (>= a 0) (>= b 0) (< a max-i) (< b max-j))) [[(dec i) j] [(inc i) j] [i (dec j)] [i (inc j)]])
  )

(defn parse-graph
  [input]
  (let [lines (str/split-lines input)
        grid (map #(str/split % #"") lines)
        width (count (first grid))
        height (count grid)
        nodes (map-indexed (fn [i row]
                                (map-indexed (fn [j cell] {:coord [i j] :cell cell :adj (possible-adj i j width height)}) row)
                                ) grid)
        _ (println nodes)
        ]
    nodes
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
    (is (= (parse-graph "123\n567") {}))
    )

  )
