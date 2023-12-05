(ns aoc2023.day5_test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]))

(def example-input "seeds: 79 14 55 13\n\nseed-to-soil map:\n50 98 2\n52 50 48\n\nsoil-to-fertilizer map:\n0 15 37\n37 52 2\n39 0 15\n\nfertilizer-to-water map:\n49 53 8\n0 11 42\n42 0 7\n57 7 4\n\nwater-to-light map:\n88 18 7\n18 25 70\n\nlight-to-temperature map:\n45 77 23\n81 45 19\n68 64 13\n\ntemperature-to-humidity map:\n0 69 1\n1 0 69\n\nhumidity-to-location map:\n60 56 37\n56 93 4")

(def puzzle-input (slurp "resources/day5.txt"))

(defn parse-category
  [categories index]
  (let [category (nth categories index)
        category (rest (str/split-lines category))
        category (map #(map bigint (str/split % #" ")) category)]
    category
    )
  )

(defn parse-almanac
  [input]
  (let [parts (str/split input #"\n\n")
        [_ seeds] (str/split (first parts) #":")
        seeds (str/trim seeds)
        seeds (str/split seeds #" ")
        seeds (map bigint seeds)
        seeds (map (fn [seed] [seed 1]) seeds)
        categories (vec (rest parts))]
    {
     :seeds                   seeds,
     :seed-to-soil            (parse-category categories 0)
     :soil-to-fertilizer      (parse-category categories 1)
     :fertilizer-to-water     (parse-category categories 2)
     :water-to-light          (parse-category categories 3)
     :light-to-temperature    (parse-category categories 4)
     :temperature-to-humidity (parse-category categories 5)
     :humidity-to-location    (parse-category categories 6)
     }
    )
  )

(defn parse-almanac-ver2
  [input]
  (let [parts (str/split input #"\n\n")
        [_ seeds] (str/split (first parts) #":")
        seeds (str/trim seeds)
        seeds (str/split seeds #" ")
        seeds (map bigint seeds)
        seeds (partition 2 seeds)
        categories (vec (rest parts))]
    {
     :seeds                   seeds,
     :seed-to-soil            (parse-category categories 0)
     :soil-to-fertilizer      (parse-category categories 1)
     :fertilizer-to-water     (parse-category categories 2)
     :water-to-light          (parse-category categories 3)
     :light-to-temperature    (parse-category categories 4)
     :temperature-to-humidity (parse-category categories 5)
     :humidity-to-location    (parse-category categories 6)
     }
    )
  )


(defn convert-mapping
  [[m-start n-start n-length] [number length]]
  (let [n-max (+ n-start (dec n-length))
        number-max (+ number (dec length))
        dx (max 0 (- number n-start))
        dx-max (max 0 (- number-max n-start))
        m-max (+ m-start (dec n-length))
        result []
        result (if (< number n-start) (conj result [number (min (dec n-start) number-max)]) result)
        result (if (and (<= number n-max) true) (conj result [(+ m-start dx) (min (+ m-start dx-max) m-max)]) result)
        mapped-length (reduce + (map second (map (fn [[min max]] [min (inc (- max min))]) result)))
        result (if (> number-max n-max) (conj result [(+ number mapped-length) number-max]) result)]
    (if (or (and (< number n-start) (< number-max n-start)) (> number n-max))
      nil
      (map (fn [[min max]] [min (inc (- max min))]) result)
      )
    )
  )

(defn map-to-category
  [mappings seed]
  (let [converted (filter some? (map #(convert-mapping % seed) mappings))]
    (if (empty? converted)
      [seed]
      (partition 2 (flatten converted))
      )
    )
  )

(defn convert-into
  [category seeds]
  (let [mapping (map (fn [seed] (map-to-category category seed)) seeds)]
    (partition 2 (flatten mapping))
    ))

(defn part1
  [input]
  (let [almanac (parse-almanac input)
        seeds (:seeds almanac)
        seed-to-soil (convert-into (:seed-to-soil almanac) seeds)
        soil-to-fertilizer (convert-into (:soil-to-fertilizer almanac) seed-to-soil)
        fertilizer-to-water (convert-into (:fertilizer-to-water almanac) soil-to-fertilizer)
        water-to-light (convert-into (:water-to-light almanac) fertilizer-to-water)
        light-to-temperature (convert-into (:light-to-temperature almanac) water-to-light)
        temperature-to-humidity (convert-into (:temperature-to-humidity almanac) light-to-temperature)
        humidity-to-location (convert-into (:humidity-to-location almanac) temperature-to-humidity)
        humidity (map first humidity-to-location)]
    (apply min humidity)
    )
  )

(defn part2
  [input]
  (let [almanac (parse-almanac-ver2 input)
        seeds (:seeds almanac)
        seed-to-soil (convert-into (:seed-to-soil almanac) seeds)
        soil-to-fertilizer (convert-into (:soil-to-fertilizer almanac) seed-to-soil)
        fertilizer-to-water (convert-into (:fertilizer-to-water almanac) soil-to-fertilizer)
        water-to-light (convert-into (:water-to-light almanac) fertilizer-to-water)
        light-to-temperature (convert-into (:light-to-temperature almanac) water-to-light)
        temperature-to-humidity (convert-into (:temperature-to-humidity almanac) light-to-temperature)
        humidity-to-location (convert-into (:humidity-to-location almanac) temperature-to-humidity)
        humidity (map first humidity-to-location)
        humidity (filter #(> % 0) humidity)]
    (apply min humidity)
    )
  )

(def example-almanac {:fertilizer-to-water     [[49 53 8]
                                                [0 11 42]
                                                [42 0 7]
                                                [57 7 4]]
                      :humidity-to-location    [[60 56 37]
                                                [56 93 4]]
                      :light-to-temperature    [[45 77 23]
                                                [81 45 19]
                                                [68 64 13]]
                      :seed-to-soil            [[50 98 2]
                                                [52 50 48]]
                      :seeds                   [[79 1] [14 1] [55 1] [13 1]]
                      :soil-to-fertilizer      [[0 15 37]
                                                [37 52 2]
                                                [39 0 15]]
                      :temperature-to-humidity [[0 69 1]
                                                [1 0 69]]
                      :water-to-light          [[88 18 7]
                                                [18 25 70]]})

(deftest day5-test
  (testing "parse"
    (is (= (:seeds (parse-almanac example-input)) [[79 1] [14 1] [55 1] [13 1]]))
    (is (= (:seed-to-soil (parse-almanac example-input)) [[50 98 2] [52 50 48]]))
    (is (= (:seed-to-soil (parse-almanac example-input)) [[50 98 2] [52 50 48]]))
    (is (= (parse-almanac example-input) example-almanac))
    )
  (testing "map-to-category"
    (is (= (convert-mapping [52 50 48] [79 1]) [[81 1]]))
    (is (= (convert-mapping [0 50 48] [50 1]) [[0 1]]))
    (is (= (convert-mapping [0 2 2] [2 2]) [[0 2]]))
    (is (= (convert-mapping [0 2 2] [2 4]) [[0 2] [4 2]]))
    (is (= (convert-mapping [0 2 2] [3 4]) [[1 1] [4 3]]))
    (is (= (convert-mapping [0 2 2] [0 4]) [[0 2] [0 2]]))
    (is (= (convert-mapping [0 2 2] [5 10]) nil))
    (is (= (convert-mapping [0 2 2] [4 1]) nil))
    (is (= (convert-mapping [0 2 2] [1 1]) nil))
    (is (= (convert-mapping [0 10 2] [5 3]) nil))
    (is (= (convert-mapping [0 1 1] [5 3]) nil))
    (is (= (convert-mapping [0 1 1] [1 1]) [[0 1]]))
    (is (= (convert-mapping [0 5 2] [0 10]) [[0 5] [0 2] [7 3]]))
    (is (= (convert-mapping [0 5 1] [0 10]) [[0 5] [0 1] [6 4]]))
    (is (= (convert-mapping [0 5 1] [0 5]) nil))
    (is (= (convert-mapping [0 5 1] [6 1]) nil))
    (is (= (convert-mapping [0 5 1] [5 1]) [[0 1]]))
    (is (= (convert-mapping [0 2 2] [0 1]) nil))
    (is (= (convert-mapping [0 2 2] [3 1]) [[1 1]]))
    (is (= (convert-mapping [0 1 1] [0 1]) nil))
    (is (= (convert-mapping [0 1 1] [1 1]) [[0 1]]))
    (is (= (map-to-category (:seed-to-soil example-almanac) [79 1]) [[81 1]]))
    (is (= (map-to-category (:seed-to-soil example-almanac) [14 1]) [[14 1]]))
    (is (= (map-to-category (:seed-to-soil example-almanac) [55 1]) [[57 1]]))
    (is (= (map-to-category (:seed-to-soil example-almanac) [13 1]) [[13 1]]))
    (is (= (map-to-category (:soil-to-fertilizer example-almanac) [81 1]) [[81 1]]))
    )
  (testing "part1"
    (is (= (part1 example-input) 35))
    (is (= (part1 puzzle-input) 88151870))
    )
  (testing "part2"
    (is (= (part2 example-input) 46))
    (is (= (part2 puzzle-input) 2008785))
    )
  )