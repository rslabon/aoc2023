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

(def adj
  (memoize
    (fn
      [row-max col-max cells-by-pos cell]
      (let [next-cells [[0 1] [0 -1] [-1 0] [1 0]]
            next-cells (mapv (fn [d] (map + d (:pos cell))) next-cells)
            next-cells (filterv (fn [[x y]] (and (>= x 0) (>= y 0) (<= row-max) (<= col-max))) next-cells)
            next-cells (filterv (fn [[x y]] (let [next-cell (get cells-by-pos [x y])
                                                  cell-value (:cell next-cell)]
                                              (or (= cell-value ".") (= cell-value "S"))
                                              )) next-cells)
            next-cells (set (map #(get cells-by-pos %) next-cells))
            ]
        next-cells
        ))))

(def travel
  (memoize
    (fn
      [row-max col-max cells-by-pos [cell steps]]
      (if (= steps 0)
        [cell]
        (let [next-cells (adj row-max col-max cells-by-pos cell)
              next-cells (map (fn [c] [c (dec steps)]) next-cells)]
          (if
            (empty? next-cells)
            []
            (set (mapcat #(travel row-max col-max cells-by-pos %) next-cells))
            )
          )
        )
      )))

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
        visited (travel (dec bx) (dec by) cells-by-pos [start n])
        visited (set visited)
        _ (print-cells bx by cells-by-pos visited)
        ]
    (count (set visited))
    )
  )

(deftest day21-test
  (testing "day21"
    (is (= (part1 example-input 6) 16))
    ;(is (= (part1 puzzle-input 64) 3782))
    ;takes 28 sec
    ))
