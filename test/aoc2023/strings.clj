(ns aoc2023.strings
  (:require [clojure.string :as str]
            [clojure.test :refer :all]))

(defn rotate-multi-line-text
  [text]
  (let [lines (vec (str/split-lines text))
        width (count (first lines))
        new-lines (map (fn [idx] (str/join (map #(nth % idx) (reverse lines)))) (range 0 width))
        ]
    (str/join "\n" new-lines)
    )
  )

(deftest rotate-multi-line-text-test
  (testing "rotate-multi-line-text"
    (is (= (rotate-multi-line-text "123\n456") "41\n52\n63"))
    )
  )
