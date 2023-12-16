(ns aoc2023.day14-test
  (:require [aoc2023.strings :as ss]
            [clojure.string :as str]
            [clojure.test :refer :all]))

(def example-input "O....#....\nO.OO#....#\n.....##...\nOO.#O....O\n.O.....O#.\nO.#..O.#.#\n..O..#O..O\n.......O..\n#....###..\n#OO..#....")
(def puzzle-input (slurp "resources/day14.txt"))

(def count-load
  (memoize
    (fn
      [lines]
      (flatten
        (map
          #(map-indexed
             (fn [idx val]
               (if (= val \O)
                 (inc idx)
                 0
                 )) %) lines)))))

(defn move
  [line]
  (let [rounded-rock-count (count (filter #(= % \O) line))
        empty-space-count (count (filter #(= % \.) line))]
    (str/join (concat (repeat empty-space-count ".")
                      (repeat rounded-rock-count "O")
                      )
              )
    )
  )

(def tilt
  (memoize
    (fn [line]
      (loop [left-line line
             new-line ""]
        (cond
          (empty? left-line) new-line
          (every? #(= % \#) left-line) (str new-line left-line)
          :else (let [rock-index (str/index-of left-line "#")
                      end-index (count left-line)
                      n (if rock-index rock-index end-index)
                      pre (take n left-line)
                      pre (move pre)
                      pre (if rock-index (str pre "#") pre)
                      post (str/join (drop (count pre) left-line))
                      ]
                  (recur post (str new-line pre))
                  )
          )
        )
      )))

(defn part1
  [input]
  (let [input (ss/rotate-multi-line-text input)
        lines (str/split-lines input)
        titled-lines (map tilt lines)
        load-values (count-load titled-lines)
        ]
    (apply + load-values)
    )
  )

(defn title-text
  [input]
  (let [lines (str/split-lines input)]
    (str/join "\n" (map tilt lines))))

(def spin-cycle
  (memoize
    (fn [input]
      (let [input (ss/rotate-multi-line-text input)
            input (title-text input)
            input (ss/rotate-multi-line-text input)
            input (title-text input)
            input (ss/rotate-multi-line-text input)
            input (title-text input)
            input (ss/rotate-multi-line-text input)
            input (title-text input)
            ]
        input
        )
      )))

(defn load-for-n-cycle
  [input n]
  (loop [i n
         cycle-input input
         results (list)]
    (cond
      (= i 0) (first results)
      :else (let [input (spin-cycle cycle-input)
                  lines (str/split-lines (ss/rotate-multi-line-text input))
                  load-values (count-load lines)]
              (recur (dec i) input (conj results (apply + load-values)))
              )
      )))

(defn part2
  [input]
  (let [growth (load-for-n-cycle input 1000)]
    growth
    )
  )

(deftest day14-test
  (testing "day14"
    (is (= (tilt "OOO...#") "...OOO#"))
    (is (= (tilt "OOO...") "...OOO"))
    (is (= (tilt "OOO...##") "...OOO##"))
    (is (= (tilt "OOO..#.#") "..OOO#.#"))
    (is (= (tilt "...OOO#.#") "...OOO#.#"))
    (is (= (part1 example-input) 136))
    (is (= (part1 puzzle-input) 113525))
    (is (= (part2 puzzle-input) 101292))
    ))
