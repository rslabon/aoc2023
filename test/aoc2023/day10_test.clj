(ns aoc2023.day10_test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]))

(def example-input ".....\n.S-7.\n.|.|.\n.L-J.\n.....")
(def example-input2 "-L|F7\n7S-7|\nL|7||\n-L-J|\nL|-JF")
(def example-input3 "7-F7-\n.FJ|7\nSJLL7\n|F--J\nLJ.LJ")
(def puzzle-input (slurp "resources/day10.txt"))

(def pipe-connections {
                       "|" {:up ["7" "F" "|" "S"] :down ["J" "L" "|" "S"] :left [] :right []}
                       "-" {:up [] :down [] :left ["L" "F" "-" "S"] :right ["J" "7" "-" "S"]}
                       "L" {:up ["|" "7" "F" "S"] :down [] :left [] :right ["J" "7" "-" "S"]}
                       "J" {:up ["|" "7" "F" "S"] :down [] :left ["-" "L" "F" "S"] :right []}
                       "7" {:up [] :down ["|" "J" "L" "S"] :left ["-" "L" "F" "S"] :right []}
                       "F" {:up [] :down ["|" "J" "L" "S"] :left [] :right ["-" "J" "7" "S"]}
                       "S" {:up ["7" "F" "|"] :down ["J" "L" "|"] :left ["-" "L" "F"] :right ["7" "-" "J"]}
                       })

(defn parse-cells
  [input]
  (let [lines (str/split-lines input)
        grid (map #(str/split % #"") lines)
        cells (->> (map-indexed (fn [row-idx row]
                                  (map-indexed (fn [col-idx cell]
                                                 {:coord [row-idx col-idx], :pipe-type (str cell)}
                                                 ) row)) grid)
                   (flatten)
                   (remove #(= (:pipe-type %) ".")))
        ]
    cells
    )
  )

(defn pipe-connects?
  [pipe1 direction pipe2]
  ;(println pipe1 direction pipe2 (set (get (get pipe-connections pipe1) direction)))
  (contains? (set (get (get pipe-connections pipe1) direction)) pipe2)
  )

(defn ensure-connected
  [cell1 direction cell2]
  (if (or (nil? cell2) (not (pipe-connects? (:pipe-type cell1) direction (:pipe-type cell2))))
    nil
    cell2
    )
  )

(defn next-cells
  [cells-by-coord cell]
  (let [[x y] (:coord cell)
        up-cell (first (get cells-by-coord [(dec x) y]))
        up-cell (ensure-connected cell :up up-cell)
        down-cell (first (get cells-by-coord [(inc x) y]))
        down-cell (ensure-connected cell :down down-cell)
        left-cell (first (get cells-by-coord [x (dec y)]))
        left-cell (ensure-connected cell :left left-cell)
        right-cell (first (get cells-by-coord [x (inc y)]))
        right-cell (ensure-connected cell :right right-cell)
        possible-cells (filterv some? [up-cell down-cell left-cell right-cell])
        ;_ (println possible-cells)
        ]
    possible-cells
    )
  )

(defn find-loop
  ([cells-by-coord prev-cell current-cell visited]
   (let [next-cells (next-cells cells-by-coord current-cell)
         next-cells (remove #(= % prev-cell) next-cells)
         has-loop (and (> (count visited) 1) (contains? (set visited) current-cell))
         ;_ (println has-loop current-cell next-cells visited)
         ]
     (cond
       has-loop visited
       (empty? next-cells) nil
       :else (first (filterv some? (mapv #(find-loop cells-by-coord current-cell % (conj visited current-cell)) next-cells)))
       )
     )))

(defn find-farthest-point-in-steps
  ([cells]
   (let [starting-cell (first (filter #(= (:pipe-type %) "S") cells))
         ;_ (println starting-cell)
         cells-by-coord (group-by :coord cells)
         loop (find-loop cells-by-coord nil starting-cell [])
         steps (count loop)
         ;_ (println loop)
         ]
     (/ steps 2)
     ))
  )

(defn part1
  [input]
  (find-farthest-point-in-steps (parse-cells input))
  )

(deftest day10-test
  (testing "parse-cells"
    (is (= (parse-cells example-input) [{:pipe-type "S"
                                         :coord     [1 1]}
                                        {:pipe-type "-"
                                         :coord     [1 2]}
                                        {:pipe-type "7"
                                         :coord     [1 3]}
                                        {:pipe-type "|"
                                         :coord     [2 1]}
                                        {:pipe-type "|"
                                         :coord     [2 3]}
                                        {:pipe-type "L"
                                         :coord     [3 1]}
                                        {:pipe-type "-"
                                         :coord     [3 2]}
                                        {:pipe-type "J"
                                         :coord     [3 3]}]))
    )
  (testing "connects?"
    (is (= (pipe-connects? "L" :down "|") false))
    (is (= (pipe-connects? "L" :right "-") true))
    (is (= (pipe-connects? "L" :up "|") true))
    (is (= (pipe-connects? "-" :up "|") false))
    (is (= (pipe-connects? "-" :left "-") true))
    (is (= (pipe-connects? "-" :right "-") true))
    (is (= (pipe-connects? "S" :right "-") true))
    )
  (testing "find-farthest-point-in-steps"
    (is (= (find-farthest-point-in-steps (parse-cells example-input)) 4))
    (is (= (find-farthest-point-in-steps (parse-cells example-input2)) 4))
    (is (= (find-farthest-point-in-steps (parse-cells example-input3)) 8))
    )
  (testing "part1"
    (is (= (part1 example-input2) 4))
    (is (= (part1 example-input3) 8))
    (is (= (part1 puzzle-input) 6870))
    ;-Xss100m !!!
    )
  )