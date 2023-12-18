(ns aoc2023.day18-test
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer :all]))

(def example-input "R 6 (#70c710)\nD 5 (#0dc571)\nL 2 (#5713f0)\nD 2 (#d2c081)\nR 2 (#59c680)\nD 2 (#411b91)\nL 5 (#8ceee2)\nU 2 (#caa173)\nL 1 (#1b58a2)\nU 2 (#caa171)\nR 2 (#7807d2)\nU 3 (#a77fa3)\nL 2 (#015232)\nU 2 (#7a21e3)")
(def puzzle-input (slurp "resources/day18.txt"))

(defn dig
  ([dir moves]
   (concat [[0 0]] (drop-last 1 (dig [0 0] dir moves))))
  ([start dir moves]
   (let [d (condp = dir
             :right [0 1]
             :left [0 -1]
             :up [-1 0]
             :down [1 0]
             )]
     (loop [n moves
            points (list start)]
       (if (= n 0)
         (vec (rest (reverse points)))
         (recur (dec n) (conj points (map + (first points) d)))
         )
       )
     ))
  )

(defn parse-line
  [line]
  (let [[direction moves color] (str/split line #" ")]
    (condp = (str direction)
      "R" [:right (read-string moves) color]
      "L" [:left (read-string moves) color]
      "D" [:down (read-string moves) color]
      "U" [:up (read-string moves) color]
      )
    )
  )

(defn print-points
  [points outside-points]
  (let [dx (abs (apply min (map first points)))
        dy (abs (apply min (map second points)))
        adjusted-points (map (fn [[x y]] [(+ x dx) (+ y dy)]) points)
        x-max (apply max (map first adjusted-points))
        y-max (apply max (map second adjusted-points))
        adjusted-points (set adjusted-points)
        outside-points (map (fn [[x y]] [(+ x dx) (+ y dy)]) outside-points)
        outside-points (set outside-points)]
    (println
      (str/join "\n" (map-indexed
                       (fn [i row]
                         (str/join
                           (map-indexed
                             (fn [j _] (cond
                                         (contains? adjusted-points [i j]) "#"
                                         (contains? outside-points [i j]) "O"
                                         :else ".")) row))
                         ) (repeat (inc x-max) (range (inc y-max))))))
    ))

(defn move
  [[x y]]
  [[(- x 1) y] [(+ x 1) y] [x (- y 1)] [x (+ y 1)]]
  )

(defn travel
  [point visited-points x-min x-max y-min y-max]
  (let [next-moves (move point)
        next-moves (set (filter (fn [[x y]] (and (>= x x-min) (>= y y-min) (<= x x-max) (<= y y-max))) next-moves))
        new-moves (set/difference next-moves visited-points)]
    (if (or (empty? new-moves) (contains? visited-points point))
      (conj visited-points point)
      (reduce (fn [acc v] (travel v acc x-min x-max y-min y-max)) (conj visited-points point) new-moves)
      )
    )
  )

(defn count-points-inside
  [points]
  (let [x-min (apply min (map first points))
        y-min (apply min (map second points))
        x-max (apply max (map first points))
        y-max (apply max (map second points))
        points (set points)
        total-points-count (* (inc (- x-max x-min)) (inc (- y-max y-min)))
        visited-points points
        left-side-points (map (fn [i] [i y-min]) (range x-min (inc x-max)))
        visited-points (reduce (fn [acc point] (travel point acc x-min x-max y-min y-max)) visited-points left-side-points)
        right-side-points (map (fn [i] [i y-max]) (range x-min (inc x-max)))
        visited-points (reduce (fn [acc point] (travel point acc x-min x-max y-min y-max)) visited-points right-side-points)
        up-side-points (map (fn [i] [x-min i]) (range y-min (inc y-max)))
        visited-points (reduce (fn [acc point] (travel point acc x-min x-max y-min y-max)) visited-points up-side-points)
        down-side-points (map (fn [i] [x-max i]) (range y-min (inc y-max)))
        visited-points (reduce (fn [acc point] (travel point acc x-min x-max y-min y-max)) visited-points down-side-points)
        empty-space-outside (set/difference visited-points points)
        inside-count (- total-points-count (count empty-space-outside))]
    inside-count
    ))

(defn part1
  [input]
  (let [lines (str/split-lines input)
        commands (map parse-line lines)]
    (loop [commands commands
           points (list)]
      (let [[direction moves] (first commands)]
        (if (empty? commands)
          (do (count-points-inside points))
          (recur (rest commands)
                 (concat (reverse (if (empty? points)
                                    (dig direction moves)
                                    (dig (first points) direction moves)
                                    ))
                         points
                         ))
          )
        )
      )
    ))


(defn dig2
  [point direction moves]
  (let [[x y] (if point point [0 0])
        d (if point 1 0)
        ;moves (if point moves (dec moves))
        moves (inc moves)
        value (condp = direction
                :left [[x y] [x (- y moves)]]
                :right [[x y] [x (+ y moves)]]
                :up [[x y] [(- x moves) y]]
                :down [[x y] [(+ x moves) y]]
                )
        _ (println "xxxxxxxxxx = " value)
        ]
    value
    ))

(defn parse-color-as-command
  [line]
  (let [[_ _ color] (str/split line #" ")
        color (str/replace color #"[\(|\)]" "")
        dir (read-string (str (last color)))
        moves (subs (str color) 1 (dec (count color)))
        moves (Integer/parseInt moves 16)
        ]
    (condp = dir
      0 [:right moves]
      1 [:down moves]
      2 [:left moves]
      3 [:up moves]
      )
    )
  )

; private static double shoelaceArea(List<Point> v) {
;        int n = v.size();
;        double a = 0.0;
;        for (int i = 0; i < n - 1; i++) {
;            a += v.get(i).x * v.get(i + 1).y - v.get(i + 1).x * v.get(i).y;
;        }
;        return Math.abs(a + v.get(n - 1).x * v.get(0).y - v.get(0).x * v.get(n - 1).y) / 2.0;
;    }


;// Get the center (mean value) using reduce
;const center = points.reduce((acc, { x, y }) => {
;  acc.x += x / points.length;
;  acc.y += y / points.length;
;  return acc;
;}, { x: 0, y: 0 });
;
;// Add an angle property to each point using tan(angle) = y/x
;const angles = points.map(({ x, y }) => {
;  return { x, y, angle: Math.atan2(y - center.y, x - center.x) * 180 / Math.PI };
;});
;
;// Sort your points by angle
;const pointsSorted = angles.sort((a, b) => a.angle - b.angle);
(defn sort-points-clockwise
  [points]
  (let [center-point [(bigint (/ (apply + (map #(bigint (first %)) points)) (count points))) (bigint (/ (apply + (map #(bigint (second %)) points)) (count points)))]
        [cx cy] center-point
        ;_ (println center-point)
        angles (map (fn [[x y]] [x y
                                 (/ (* (bigdec (Math/atan2 (- y cy) (- x cx))) 180) Math/PI)
                                 ]) points)
        sorted-points (sort-by #(nth % 2) - angles)
        sorted-points (reverse sorted-points)
        ;_ (println "xxxxxxxx " sorted-points)
        ]
    (mapv #(take 2 %) sorted-points)
    )
  )

(defn shoelace-area
  [points]
  (let [points (sort-points-clockwise points)
        ;_ (println points)
        points (vec points)
        points (map (fn [[x y]] [(bigint x) (bigint y)]) points)
        n (count points)]
    (loop [i 0
           area (bigint 0)]
      (if (>= i (dec n))
        (let [[lx ly] (nth points (dec n))
              [fx fy] (nth points 0)]
          (bigint (/ (abs (+ area (- (* lx fy) (* fx ly)))) 2)))
        (let [[ix iy] (nth points i)
              [iix iiy] (nth points (inc i))]
          (recur (inc i) (+ area (- (* ix iiy) (* iix iy))))))
      )))

(defn part1
  [input]
  (let [lines (str/split-lines input)
        commands (map parse-line lines)]
    (loop [commands commands
           points (list)]
      (let [[direction moves] (first commands)]
        (if (empty? commands)
          (do
            (println (reverse points))
            (shoelace-area points))
          (recur (rest commands)
                 (if (empty? points)
                   (concat (reverse (dig2 nil direction moves)) points)
                   (concat (reverse (dig2 (first points) direction moves)) points)
                   )
                 )
          )
        )
      )))

(defn part2
  [input]
  (let [lines (str/split-lines input)
        commands (map parse-color-as-command lines)
        _ (println commands)]
    (loop [commands commands
           points (list)]
      (let [[direction moves] (first commands)
            _ (println (reverse points))]
        (if (empty? commands)
          (do
            (println (reverse points))
            (shoelace-area points))
          (recur (rest commands)
                 (if (empty? points)
                   (concat (reverse (dig2 nil direction moves)) points)
                   (concat (reverse (dig2 (first points) direction moves)) points)
                   )
                 )
          )
        )
      )))


(deftest day18-test
  (testing "day18"
    ;(is (= (dig :right 4) [[0 0] [0 1] [0 2] [0 3]]))
    ;(is (= (dig :down 4) [[0 0] [1 0] [2 0] [3 0]]))
    ;(is (= (dig [0 0] :right 4) [[0 1] [0 2] [0 3] [0 4]]))
    ;(is (= (dig [0 0] :down 2) [[1 0] [2 0]]))
    ;(is (= (dig [1 0] :up 1) [[0 0]]))
    ;(is (= (dig [0 0] :up 1) [[-1 0]]))
    ;(is (= (dig [0 1] :left 1) [[0 0]]))
    ;(is (= (part1 example-input) 62))
    ;(is (= (count-points-inside [[1 1] [1 2] [1 3]
    ;                             [2 1] [2 3]
    ;                             [3 1] [3 2] [3 3]
    ;                             ]) 9))
    ;(is (= (count-points-inside [[1 1] [1 3]
    ;                             [2 1] [2 2] [2 3]
    ;                             [3 1] [3 2] [3 3]
    ;                             ]) 8))
    ;(is (= (part1 puzzle-input) 26857))
    ;(is (= (dig2 [0 0] :right 4) [[0 0] [0 5]]))
    (is (= (dig2 nil :right 6) [[0 0] [0 7]]))
    (is (= (dig2 [0 7] :down 5) [[0 7] [6 7]]))
    ;(is (= (dig2 [0 0] :right 4) [[0 0] [0 4]]))
    ;;(is (= (dig2 [0 0] :left 4) [[0 -1] [0 -4]]))
    ;(is (= (parse-color-as-command "R 6 (#70c710)") [:right 461937]))
    ;(is (= (shoelace-area [[3, 4] [5, 11] [12, 8] [9, 5] [5, 6]]) 30))
    ;(is (= (shoelace-area (shuffle [[9, 5] [3, 4] [5, 11] [12, 8] [5, 6]])) 30))
    (is (= (part1 example-input) 62))
    ;(is (= (part2 example-input) 952408144115))
    ; -Xss100m because of clojure recursion :(
    ))
