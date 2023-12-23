(ns aoc2023.day21-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]))

(def example-input "...........\n.....###.#.\n.###.##..#.\n..#.#...#..\n....#.#....\n.##..S####.\n.##..#...#.\n.......##..\n.##.#.####.\n.##..##.##.\n...........")
(def puzzle-input (slurp "resources/day21.txt"))

(defn parse
  [input]
  (let [lines (str/split-lines input)
        cells (set
                (flatten
                  (map-indexed
                    (fn [i row]
                      (map-indexed
                        (fn [j cell] {:pos [i j] :cell (str cell)}) row)) lines)))

        ]
    cells
    )
  )

(defn print-cells
  [row-max col-max cells-by-pos visited]
  (println
    (str/join "\n"
              (map-indexed
                (fn [i row]
                  (str/join
                    (map-indexed
                      (fn [j _]
                        (let [visited (set (map :pos visited))]
                          (if (contains? visited [i j])
                            "O"
                            (:cell (get cells-by-pos [i j]))
                            )
                          )) row))
                  ) (repeat (inc row-max) (range 0 (inc col-max)))
                ))))

(defn adj
  [row-max col-max cells-by-pos point adjust-pos]
  (let [next-points [[0 1] [0 -1] [-1 0] [1 0]]
        next-points (mapv (fn [d] (map + d point)) next-points)
        next-points (if adjust-pos
                      next-points
                      (filterv (fn [[x y]] (and (>= x 0) (>= y 0) (<= row-max) (<= col-max))) next-points))
        next-points (filterv (fn [[x y]] (let [[x y] (if adjust-pos (adjust-pos row-max col-max [x y])
                                                                    [x y])
                                               next-cell (get cells-by-pos [x y])
                                               cell-value (:cell next-cell)]
                                           (not= cell-value "#")
                                           )) next-points)
        ]
    next-points
    ))


(defn travel
  [row-max col-max cells-by-pos point max-steps adjust-pos]
  (loop [q (set [point])
         new-q (set [])
         step 0
         visited (set [])
         cycle []]
    (let []
      (cond
        (= step max-steps) (count visited)
        (= (count cycle) 3) (let [p0 (bigint (nth cycle 0))
                                  p1 (bigint (- (nth cycle 1) (nth cycle 0)))
                                  p2 (bigint (- (nth cycle 2) (nth cycle 1)))
                                  number-of-cycles (bigint (/ max-steps (inc row-max)))
                                  x1 (* p1 number-of-cycles)
                                  x2 (bigint (* number-of-cycles (/ (- number-of-cycles 1) 2)))
                                  x3 (- p2 p1)
                                  part2 (+ p0 x1 (* x2 x3))]
                              part2
                              )
        (empty? q) (let [step (inc step)
                         cycle (if (= (mod step (inc row-max)) (mod max-steps (inc row-max)))
                                 (conj cycle (count visited))
                                 cycle)
                         visited (if (= step max-steps)
                                   visited
                                   (set []))]
                     (recur new-q (set []) step visited cycle))
        :else (let [next-points (adj row-max col-max cells-by-pos (first q) adjust-pos)
                    q (rest q)]
                (recur q (into new-q next-points) step (into visited next-points) cycle)
                )
        ))))

(defn adjust-pos
  [row-max col-max [ox oy]]
  (let [[x y] (if (< ox 0) [(- (inc row-max) (mod (abs ox) (inc row-max))) oy] [ox oy])
        [x y] (if (> x row-max) [(mod x (inc row-max)) y] [x y])
        [x y] (if (< y 0) [x (- (inc col-max) (mod (abs y) (inc col-max)))] [x y])
        [x y] (if (> y col-max) [x (mod y (inc col-max))] [x y])]
    [x y]
    ))

(defn boundry
  [input]
  (let [lines (str/split-lines input)
        height (count lines)
        width (count (first lines))
        ]
    [height width]
    )
  )

(defn part1
  [input n]
  (let [cells (parse input)
        cells-by-pos (group-by :pos cells)
        cells-by-pos (reduce-kv (fn [m k v] (assoc m k (first v))) {} cells-by-pos)
        [bx by] (boundry input)
        start (first (filter #(= (:cell %) "S") cells))
        visited (travel (dec bx) (dec by) cells-by-pos (:pos start) n nil)
        ]
    visited
    )
  )

(defn part2
  [input n]
  (let [cells (parse input)
        cells-by-pos (group-by :pos cells)
        cells-by-pos (reduce-kv (fn [m k v] (assoc m k (first v))) {} cells-by-pos)
        [bx by] (boundry input)
        start (first (filter #(= (:cell %) "S") cells))
        visited (travel (dec bx) (dec by) cells-by-pos (:pos start) n adjust-pos)
        ]
    visited
    )
  )

(deftest day21-test
  (testing "day21"
    ;(is (= (part1 example-input 6) 16))
    ;(is (= (part1 puzzle-input 64) 3782))
    (is (= (adjust-pos 4 4 [0 5]) [0 0]))
    (is (= (adjust-pos 4 4 [0 -1]) [0 4]))
    (is (= (adjust-pos 4 4 [-1 0]) [4 0]))
    (is (= (part2 puzzle-input 26501365) 630661863455116))
    ))
