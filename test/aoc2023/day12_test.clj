(ns aoc2023.day12-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]))

(def example-input "???.### 1,1,3\n.??..??...?##. 1,1,3\n?#?#?#?#?#?#?#? 1,3,1,6\n????.#...#... 4,1,1\n????.######..#####. 1,6,5\n?###???????? 3,2,1")
(def puzzle-input (slurp "resources/day12.txt"))

(def matches?
  (memoize (fn [text group]
             (let [found (re-find (re-pattern (str "(?<!#)#{" group "}(?!#)")) text)]
               (not= nil found)))))

(def mem-replace-missing
  (memoize (fn [line group offset end]
             (println offset end)
             (if (>= offset end)
               [line]
               (filterv #(matches? % group)
                        (sort (set
                                (let [text (subs line offset end)
                                      pre (.substring line 0 offset)
                                      p (re-pattern (str "(?<!#)[\\?|#]{" group "}(?!#)"))
                                      replaced (if (= 1 group)
                                                 (str pre (str/replace-first text #"(?<!#)\?(?!#)" "#"))
                                                 (str pre (str/replace-first text p (str/join (repeat group "#")))))
                                      ;_ (println line group replaced)
                                      replacement-idx (loop [s1 (subs line offset)
                                                             s2 (subs replaced offset)
                                                             idx offset]
                                                        (if (or (= line replaced) (empty? s1) (not= (first s1) (first s2)))
                                                          (inc idx)
                                                          (recur (rest s1) (rest s2) (inc idx))
                                                          ))
                                      ]
                                  (cons replaced (mem-replace-missing line group replacement-idx end))
                                  ))
                              ))))))

(defn replace-missing
  ([line group] (mem-replace-missing line group 0 (count line)))
  ([line group end] (mem-replace-missing line group 0 end))
  )

(defn find-arrangements
  [line groups]
  (let [p (map (fn [group] (str "[\\.\\?]*(?<!#)#{" group "}(?!#)[\\.\\?]*")) groups)
        p (str/join p)
        p (str "^" p "$")
        n (- (count line) (apply + groups) (inc (count groups)))
        ]
    (filterv
      #(re-find (re-pattern p) %)
      (sort
        (set
          (loop [groups groups
                 arrangements [line]]
            ;(println arrangements)
            (if (empty? groups)
              arrangements
              (let [group (first groups)]
                (recur (rest groups)
                       (mapcat #(replace-missing % group n) arrangements)
                       )
                ))
            )
          )))))

(defn parse-line
  [line]
  (let [[text groups] (str/split line #" ")
        groups (-> groups
                   (str/trim)
                   (str/split #","))
        groups (mapv read-string groups)
        ]
    [text groups]
    )
  )

(defn parse
  [input]
  (let [lines (str/split-lines input)]
    (map parse-line lines)
    )
  )

(defn part1
  [input]
  (let [records (parse input)
        arrangements-count (mapv (fn [[text groups]] (let [w (count (find-arrangements text groups))
                                                           ;_ (println "text= " text "groups= " groups w)

                                                           ] w)) records)
        ;_ (println arrangements-count)
        ]
    (apply + arrangements-count)
    )
  )

(defn part2
  [input]
  (let [records (parse input)
        records (mapv (fn [[text groups]] [(str/join "?" (repeat 5 text)) (flatten (repeat 5 groups))]) records)
        ;_ (println records)
        arrangements-count (mapv (fn [[text groups]] (let [w (count (find-arrangements text groups))
                                                           ;_ (println "text= " text "groups= " groups w)

                                                           ] w)) records)
        ;_ (println arrangements-count)
        ]
    (apply + arrangements-count)
    )
  )

;@cache
;def recurse(lava, springs, result=0):
;    if not springs:
;        return '#' not in lava
;    current, springs = springs[0], springs[1:]
;    for i in range(len(lava) - sum(springs) - len(springs) - current + 1):
;        if "#" in lava[:i]:
;            break
;        if (nxt := i + current) <= len(lava) and '.' not in lava[i : nxt] and lava[nxt : nxt + 1] != "#":
;            result += recurse(lava[nxt + 1:], springs)
;    return result
;with open("day12.txt", "r") as file:
;    data = [x.split() for x in file.read().splitlines()]
;    p1, p2 = 0, 0
;    for e, (lava, springs) in enumerate(data):
;        p1 += recurse(lava, (springs := tuple(map(int, springs.split(",")))))
;        p2 += recurse("?".join([lava] * 5), springs * 5)
;    print(p1, p2)

(defn count-arrangements
  ([line groups result]
   (println groups)
   (if (empty? groups)
     (if (str/includes? line "#")
       (do (println "0") 0)
       (do (println "1") 1)
       )
     (let [current (first groups)
           groups (rest groups)
           positions (range (- (count line) (apply + groups) (count groups) (inc current)))
           _ (println positions)]
       (loop [positions positions
              result result]
         (let [i (first positions)]
           (if (empty? positions)
             result
             (if (str/includes? (subs line 0 i) "#")
               result
               (let [next (+ i current)
                     _ (println "next=" next)]
                 (if (and (<= next (count line)) (not (str/includes? (subs line i next) ".")) (not (str/includes? (subs line next (inc next)) "#")))
                   (recur (rest positions) (+ result (count-arrangements (subs line (inc next)) groups result)))
                   (recur (rest positions) result)
                   )
                 ))
             )
           )
         )
       ))))

(deftest day12-test
  (testing "matches?"
    (is (= (matches? "???.###" 3) true))
    (is (= (matches? "###.###" 3) true))
    (is (= (matches? "#?.###" 3) true))
    (is (= (matches? "#?.###" 4) false))
    (is (= (matches? "#?.###" 2) false))
    (is (= (matches? "#?.###" 1) true))
    )
  (testing "replace-missing"
    (is (= (replace-missing "???.###" 3) ["###.###" "???.###"]))
    (is (= (replace-missing "###.###" 3) ["###.###"]))
    (is (= (replace-missing "#?#?.??#???????.??" 1) ["#?#?.#?#???????.??"
                                                     "#?#?.??#?#?????.??"
                                                     "#?#?.??#??#????.??"
                                                     "#?#?.??#???#???.??"
                                                     "#?#?.??#????#??.??"
                                                     "#?#?.??#?????#?.??"
                                                     "#?#?.??#??????#.??"
                                                     "#?#?.??#???????.#?"
                                                     "#?#?.??#???????.?#"
                                                     "#?#?.??#???????.??"]))
    (is (= (replace-missing "#?#?.??#???????.??" 2) ["#?##.??#???????.??"
                                                     "#?#?.?##???????.??"
                                                     "#?#?.??##??????.??"
                                                     "#?#?.??#?##????.??"
                                                     "#?#?.??#??##???.??"
                                                     "#?#?.??#???##??.??"
                                                     "#?#?.??#????##?.??"
                                                     "#?#?.??#?????##.??"
                                                     "#?#?.??#???????.##"]))
    (is (= (replace-missing "#?#?.??#???????.??" 4) ["####.??#???????.??"
                                                     "#?#?.####??????.??"
                                                     "#?#?.?####?????.??"
                                                     "#?#?.??####????.??"
                                                     "#?#?.??#?####??.??"
                                                     "#?#?.??#??####?.??"
                                                     "#?#?.??#???####.??"]))
    (is (= (replace-missing "???.###" 1) ["#??.###" "?#?.###" "??#.###"]))
    (is (= (replace-missing "???.###" 2) ["##?.###" "?##.###"]))
    (is (= (replace-missing "???###" 4) ["??####"]))
    (is (= (replace-missing "???###" 2) ["##?###"]))
    (is (= (replace-missing "??????" 4) ["####??" "?####?" "??####"]))
    (is (= (replace-missing "?#?#?#?#?#?#?#?" 1) ["?#?#?#?#?#?#?##"
                                                  "?#?#?#?#?#?#?#?"]))
    (is (= (replace-missing "?#?#?#?#?#?#?#?" 3) ["?###?#?#?#?#?#?"
                                                  "?#?###?#?#?#?#?"
                                                  "?#?#?###?#?#?#?"
                                                  "?#?#?#?###?#?#?"
                                                  "?#?#?#?#?###?#?"
                                                  "?#?#?#?#?#?###?"]))
    (is (= (replace-missing "?#?#?#?#?#?#?#?" 6) ["######?#?#?#?#?"
                                                  "?#?#?#?#?######"]))
    (is (= (replace-missing "###.###?##.?????..#" 3) ["###.###?##.###??..#"
                                                      "###.###?##.?###?..#"
                                                      "###.###?##.??###..#"
                                                      "###.###?##.?????..#"]))
    (is (= (replace-missing "##?.?#????.?????..#" 3) ["###.?#????.?????..#"
                                                      "##?.###???.?????..#"
                                                      "##?.?###??.?????..#"
                                                      "##?.?#?###.?????..#"
                                                      "##?.?#????.###??..#"
                                                      "##?.?#????.?###?..#"
                                                      "##?.?#????.??###..#"]))
    )
  (testing "find-arrangements"
    (is (= (find-arrangements "???.###" [1 3]) ["#??.###" "?#?.###" "??#.###"]))
    (is (= (find-arrangements "???.###" [2 3]) ["##?.###" "?##.###"]))
    (is (= (find-arrangements "???.###" [3 3]) ["###.###"]))
    (is (= (find-arrangements "???.???" [3 3]) ["###.###"]))
    (is (= (find-arrangements "##?.?#?" [3 3]) ["###.###"]))
    (is (= (find-arrangements "?#?#?#?#?#?#?#?" [1 3 1 6]) ["?#?###?#?######"]))
    (is (= (find-arrangements "??..??.?.." [1 2]) ["#?..##.?.."
                                                   "?#..##.?.."]))
    (is (= (find-arrangements "??.???..???##." [2 3 1 3]) ["##.###..#?###."]))
    (is (= (find-arrangements "??#?.?????#?#?#???." [2 12]) ["?##?.############?."
                                                             "?##?.?############."
                                                             "??##.############?."
                                                             "??##.?############."]))
    (is (= (find-arrangements "#.???????#" [1 1 1]) ["#.#??????#"
                                                     "#.?#?????#"
                                                     "#.??#????#"
                                                     "#.???#???#"
                                                     "#.????#??#"
                                                     "#.?????#?#"]))
    (is (= (find-arrangements "##?.?#????.?????..#" [3 3 2 1 3 1]) ["###.###?##.#?###..#"]))
    (is (= (find-arrangements "#?#?.??#???????.??" [1, 2, 1, 1, 4, 2]) ["#?##.#?#?####??.##"
                                                                        "#?##.#?#??####?.##"
                                                                        "#?##.#?#???####.##"
                                                                        "#?##.??#?#?####.##"]))
    )
  (testing "parse"
    (is (= (parse example-input) [["???.###" [1 1 3]]
                                  [".??..??...?##." [1 1 3]]
                                  ["?#?#?#?#?#?#?#?" [1 3 1 6]]
                                  ["????.#...#..." [4 1 1]]
                                  ["????.######..#####." [1 6 5]]
                                  ["?###????????" [3 2 1]]]))
    )
  (testing "part1"
    (is (= (part1 example-input) 21))
    ;(is (= (part1 puzzle-input) 7633))
    )
  ;(testing "count-arrangements"
  ;  (is (= (count-arrangements "????.######..#####." [1 6 5] 0) 4))
  ;  )
  (testing "part2"
    ;(is (= (part2 example-input) 0))
    )
  )
