(ns aoc2023.day16-test
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer :all]))

(def example-input ".|...\\....\n|.-.\\.....\n.....|-...\n........|.\n..........\n.........\\\n..../.\\\\..\n.-.-/..|..\n.|....-|.\\\n..//.|....")
(def puzzle-input (slurp "resources/day16.txt"))

(defn make-beam
  [direction x y]
  {:type :beam :dir direction :position [x y]}
  )

(defn change-beam-direction
  [beam direction]
  (assoc beam :dir direction)
  )

(defn make-empty-space
  [x y]
  {:type :empty-space :position [x y]}
  )

(defn make-mirror
  [direction x y]
  {:type :mirror :dir direction :position [x y]}
  )

(defn make-splitter
  [direction x y]
  {:type :splitter :dir direction :position [x y]}
  )

(defn can-make-split?
  [splitter beam]
  (let [splitter-dir (:dir splitter)
        beam-dir (:dir beam)]
    (or (and (= splitter-dir :vertical) (contains? (set [:left :right]) beam-dir))
        (and (= splitter-dir :horizontal) (contains? (set [:up :down]) beam-dir)))
    ))

(defn make-split
  [splitter]
  (let [sdir (:dir splitter)
        [sx sy] (:position splitter)]
    (condp = sdir
      :horizontal [(make-beam :left sx sy) (make-beam :right sx sy)]
      :vertical [(make-beam :up sx sy) (make-beam :down sx sy)]
      )
    )
  )

(defn parse-cells
  [input]
  (let [lines (str/split-lines input)
        cells (flatten (map-indexed
                         (fn [i row]
                           (map-indexed
                             (fn [j cell]
                               (condp = (str cell)
                                 "-" (make-splitter :horizontal i j)
                                 "|" (make-splitter :vertical i j)
                                 "/" (make-mirror :right i j)
                                 "\\" (make-mirror :left i j)
                                 "." (make-empty-space i j)
                                 )
                               ) row)
                           ) lines))
        ]
    cells
    )
  )

(defn next-position
  [beam]
  (let [dir (:dir beam)
        [x y] (:position beam)
        new-position (condp = dir
                       :right [x (inc y)]
                       :left [x (dec y)]
                       :up [(dec x) y]
                       :down [(inc x) y]
                       )]
    (assoc beam :position new-position)
    ))

(defn move-beam
  [beam cells-by-position]
  (let [beam-at-next-position (next-position beam)
        object-at-next-position (get cells-by-position (:position beam-at-next-position))]
    (if object-at-next-position
      (let [type (:type object-at-next-position)]
        (condp = type
          :empty-space [beam-at-next-position]
          :splitter (if (can-make-split? object-at-next-position beam) (make-split object-at-next-position) [beam-at-next-position])
          :mirror (let [beam-dir-to-mirror-tilt [(:dir beam) (:dir object-at-next-position)]]
                    (condp = beam-dir-to-mirror-tilt
                      [:right :right] [(change-beam-direction beam-at-next-position :up)]
                      [:right :left] [(change-beam-direction beam-at-next-position :down)]
                      [:left :right] [(change-beam-direction beam-at-next-position :down)]
                      [:left :left] [(change-beam-direction beam-at-next-position :up)]
                      [:up :right] [(change-beam-direction beam-at-next-position :right)]
                      [:up :left] [(change-beam-direction beam-at-next-position :left)]
                      [:down :right] [(change-beam-direction beam-at-next-position :left)]
                      [:down :left] [(change-beam-direction beam-at-next-position :right)]
                      ))
          )
        )
      []
      )))

(defn print-positions
  [positions width height]
  (println
    (str/join "\n"
              (map-indexed
                (fn [i row]
                  (str/join (map-indexed (fn [j _]
                                           (if (contains? positions [i j]) "#" ".")
                                           ) row))
                  ) (repeat height (range width)))) "\n\n")
  )

(defn count-energize
  [beam cells-by-position width height]
  (loop [beams (set [beam])
         energized-positions (set [])
         n 30]
    (let [new-beams (mapcat #(move-beam % cells-by-position) beams)
          new-beams (set/difference (set new-beams) beams)
          new-beams-position (set (map :position new-beams))]
      (if (= 0 n)
        (do
          ;(print-positions energized-positions width height)
          (count energized-positions))
        (recur new-beams (into energized-positions new-beams-position) (if (set/subset? new-beams-position energized-positions) (dec n) n))
        )
      )
    ))
(defn part1
  [input]
  (let [cells (parse-cells input)
        lines (str/split-lines input)
        height (count lines)
        width (count (first lines))
        cells-by-position (group-by :position cells)
        cells-by-position (reduce-kv (fn [m k v] (assoc m k (first v))) {} cells-by-position)
        beam (make-beam :right 0 -1)
        ]
    (count-energize beam cells-by-position width height)
    )
  )

(defn part2
  [input]
  (let [cells (parse-cells input)
        lines (str/split-lines input)
        height (count lines)
        width (count (first lines))
        cells-by-position (group-by :position cells)
        cells-by-position (reduce-kv (fn [m k v] (assoc m k (first v))) {} cells-by-position)
        left-side-beams (map #(make-beam :right % -1) (range height))
        up-side-beams (map #(make-beam :down -1 %) (range width))
        down-side-beams (map #(make-beam :up height %) (range width))
        right-side-beams (map #(make-beam :left % width) (range height))
        all-beams (concat left-side-beams right-side-beams up-side-beams down-side-beams)
        all-beams-count (count all-beams)
        energized (map-indexed
                    (fn [idx beam] (let [result (count-energize beam cells-by-position width height)
                                         _ (println "DONE " (inc idx) "/" all-beams-count)]
                                     result)) all-beams)
        ;_ (println energized)
        ]
    (apply max energized)
    ))

(deftest day16-test
  (testing "day16"
    (is (= (count (parse-cells example-input)) 100))
    (is (= (part1 example-input) 46))
    (is (= (part1 puzzle-input) 8551))
    (is (= (part2 example-input) 51))
    ;(is (= (part2 puzzle-input) 8754))
    ; takes 8 minutes ! :(
    ))