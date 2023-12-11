(ns aoc2023.day10_test
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer :all]))

(def example-input ".....\n.S-7.\n.|.|.\n.L-J.\n.....")
(def example-input2 "-L|F7\n7S-7|\nL|7||\n-L-J|\nL|-JF")
(def example-input3 "7-F7-\n.FJ|7\nSJLL7\n|F--J\nLJ.LJ")
(def example-input4 "...........\n.S-------7.\n.|F-----7|.\n.||.....||.\n.||.....||.\n.|L-7.F-J|.\n.|..|.|..|.\n.L--J.L--J.\n...........")
(def example-input4-1 "..........\n.S------7.\n.|F----7|.\n.||....||.\n.||....||.\n.|L-7F-J|.\n.|..||..|.\n.L--JL--J.\n..........")
(def example-input5 ".F----7F7F7F7F-7....\n.|F--7||||||||FJ....\n.||.FJ||||||||L7....\nFJL7L7LJLJ||LJ.L-7..\nL--J.L7...LJS7F-7L7.\n....F-J..F7FJ|L7L7L7\n....L7.F7||L7|.L7L7|\n.....|FJLJ|FJ|F7|.LJ\n....FJL-7.||.||||...\n....L---J.LJ.LJLJ...")
(def example-input6 "FF7FSF7F7F7F7F7F---7\nL|LJ||||||||||||F--J\nFL-7LJLJ||||||LJL-77\nF--JF--7||LJLJ7F7FJ-\nL---JF-JLJ.||-FJLJJ7\n|F|F-JF---7F7-L7L|7|\n|FFJF7L7F-JF7|JL---7\n7-L-JL7||F7|L7F-7F7|\nL.L7LFJ|||||FJL7||LJ\nL7JLJL-JLJLJL--JLJ.L")
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

(def squeeze-pipe-connections {
                               "|" {:up ["7" "F" "|" "S" "."] :down ["J" "L" "|" "S" "."] :left [] :right []}
                               "-" {:up [] :down [] :left ["L" "F" "-" "S" "."] :right ["J" "7" "-" "S" "."]}
                               "L" {:up ["|" "7" "F" "S" "."] :down ["."] :left ["."] :right ["J" "7" "-" "S" "."]}
                               "J" {:up ["|" "7" "F" "S" "."] :down ["."] :left ["-" "L" "F" "S" "."] :right ["."]}
                               "7" {:up ["."] :down ["|" "J" "L" "S" "."] :left ["-" "L" "F" "S" "."] :right ["."]}
                               "F" {:up ["."] :down ["|" "J" "L" "S" "."] :left ["."] :right ["-" "J" "7" "S" "."]}
                               "S" {:up ["7" "F" "|"] :down ["J" "L" "|"] :left ["-" "L" "F"] :right ["7" "-" "J"]}
                               "." {:up ["." "|" "L" "J" "7" "O"] :down ["." "|" "L" "J" "7" "O"] :left ["." "-" "F" "7" "O"] :right ["." "-" "J" "7" "O"]}
                               "O" {:up [] :down [] :left [] :right []}
                               })

(defn increase-input
  [input]
  (let [lines (str/split-lines input)
        column-size (count (first lines))
        lines (map #(str "O" % "O") lines)
        outside-row (str/join (repeat (+ 2 column-size) "O"))
        lines (concat [outside-row] lines [outside-row])
        ]
    (str/join "\n" lines)
    ))

(defn parse-cells
  [input]
  (let [lines (str/split-lines input)
        grid (map #(str/split % #"") lines)
        cells (->> (map-indexed (fn [row-idx row]
                                  (map-indexed (fn [col-idx cell]
                                                 {:coord [row-idx col-idx], :pipe-type (str cell)}
                                                 ) row)) grid)
                   (flatten)
                   ;(remove #(= (:pipe-type %) "."))
                   )
        ;_ (println cells)
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

(defn find-loop-cells
  ([cells-by-coord starting-cell prev-cell current-cell visited]
   (let [next-cells (next-cells cells-by-coord current-cell)
         next-cells (remove #(= % prev-cell) next-cells)
         has-loop (and (> (count visited) 1) (= current-cell starting-cell))
         ;_ (println (:coord prev-cell) (:coord current-cell) (map :coord next-cells) (map :coord visited))
         ]
     (cond
       has-loop visited
       (empty? next-cells) nil
       :else (first (filterv some? (mapv #(find-loop-cells cells-by-coord starting-cell current-cell % (conj visited current-cell)) next-cells)))
       )
     )))

(defn find-farthest-point-in-steps
  ([cells]
   (let [starting-cell (first (filter #(= (:pipe-type %) "S") cells))
         ;_ (println starting-cell)
         cells-by-coord (group-by :coord cells)
         loop (find-loop-cells cells-by-coord starting-cell nil starting-cell [])
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

(def char-mappings
  {
   "|" "│"
   "F" "┌"
   "L" "└"
   "J" "┘"
   "-" "─"
   "7" "┐"
   "S" "S"
   "O" "O"
   "." "."
   })

(defn pretty-string
  [cells loop-cells rows cols]
  (let [cells-by-coord (group-by :coord cells)
        loop-cells-by-coord (group-by :coord loop-cells)]
    (str/join "\n"
              (map (fn [i]
                     (str/join (map (fn [j]
                                      (get char-mappings (:pipe-type (first (get loop-cells-by-coord [i j] (get cells-by-coord [i j])))))
                                      ) (range 0 cols))
                               ))
                   (range 0 rows)
                   ))
    ))

(defn can-squeeze-outside?
  ([cells-by-coord cell] (can-squeeze-outside? cells-by-coord cell []))
  ([cells-by-coord cell visited]
   (let [[x y] (:coord cell)
         up-cell (first (get cells-by-coord [(dec x) y]))
         down-cell (first (get cells-by-coord [(inc x) y]))
         left-cell (first (get cells-by-coord [x (dec y)]))
         right-cell (first (get cells-by-coord [x (inc y)]))
         near-outside (some #(= (:pipe-type %) "O") [up-cell down-cell left-cell right-cell])
         possible-pipes (get squeeze-pipe-connections (:pipe-type cell))
         ;_ (println "cell=" cell )
         ;_ (println "possible-pipes=" possible-pipes )
         ;_ (println "possible=" [up-cell down-cell left-cell right-cell])
         ;_ (println "visited=" visited)
         ;_ (println "up=" (set (:up possible-pipes)) (:pipe-type up-cell))
         ;_ (println "down=" (set (:down possible-pipes)) (:pipe-type down-cell))
         ;_ (println "left=" (set (:left possible-pipes)) (:pipe-type left-cell))
         ;_ (println "right=" (set (:right possible-pipes)) (:pipe-type right-cell))
         ;_ (println "\n\n")
         ]
     (cond
       near-outside (do (println "TRUE!!! " visited) true)
       (contains? (set visited) cell) (do (println "NOT!!!! " visited) false)
       :else (or
               (and (contains? (set (:up possible-pipes)) (:pipe-type up-cell)) (can-squeeze-outside? cells-by-coord up-cell (conj visited cell)))
               (and (contains? (set (:down possible-pipes)) (:pipe-type down-cell)) (can-squeeze-outside? cells-by-coord down-cell (conj visited cell)))
               (and (contains? (set (:left possible-pipes)) (:pipe-type left-cell)) (can-squeeze-outside? cells-by-coord left-cell (conj visited cell)))
               (and (contains? (set (:right possible-pipes)) (:pipe-type right-cell)) (can-squeeze-outside? cells-by-coord right-cell (conj visited cell)))
               )
       ))))

(defn fix
  [s]
  (-> s
      (str/replace "┌*┐" "┌─┐")
      (str/replace "┌*─" "┌──")
      (str/replace "─*─" "───")
      (str/replace "└*┘" "└─┘")
      (str/replace "─*┐" "──┐")
      (str/replace "└*─" "└──")
      (str/replace "─*┘" "──┘")
      (str/replace "─*─" "───")
      (str/replace "└*┐" "└─┐")
      (str/replace "┌*┘" "┌─┘")
      (str/replace "┌*S" "┌─S")
      (str/replace "S*┌" "S─┌")
      (str/replace "S*─" "S──")
      (str/replace "S*┐" "S─┐")
      (str/replace "┘*S" "S─┐")
      ))

(defn transpose
  [s]
  (let [lines (str/split-lines s)
        grid (map #(str/split % #"") lines)

        ]
    s
    )
  )

(defn part2
  [input]
  (let [input (increase-input input)
        lines (str/split-lines input)
        rows (count lines)
        cols (count (first lines))
        cells (parse-cells input)
        starting-cell (first (filter #(= (:pipe-type %) "S") cells))
        cells-by-coord (group-by :coord cells)
        loop-cells (set (find-loop-cells cells-by-coord starting-cell nil starting-cell []))
        ground-cells (remove #(= (:pipe-type %) "O") (set/difference (set cells) loop-cells))
        ground-cells (map (fn [cell] {:coord (:coord cell), :pipe-type "."}) ground-cells)
        ground-cells-by-coord (group-by :coord ground-cells)
        cells-by-coord (reduce-kv (fn [m k v] (assoc m k (get ground-cells-by-coord k v))) {} cells-by-coord)
        cells (flatten (vals cells-by-coord))
        ]
    (println (pretty-string cells loop-cells rows cols))
    ))


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
    ;(is (= (part1 puzzle-input) 6870))
    ;-Xss100m !!!
    )
  (testing "part2"
    ;(is (= (part2 example-input4) 4))
    (is (= (part2 example-input4-1) 4))
    ;(is (= (part2 example-input5) 10))
    ;(is (= (part2 example-input6) 10))
    ;(is (= (part2 puzzle-input) 0))
    ;-Xss100m !!!
    )
  )

