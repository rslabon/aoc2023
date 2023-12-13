(ns aoc2023.day12-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]))

(def example-input "???.### 1,1,3\n.??..??...?##. 1,1,3\n?#?#?#?#?#?#?#? 1,3,1,6\n????.#...#... 4,1,1\n????.######..#####. 1,6,5\n?###???????? 3,2,1")
(def puzzle-input (slurp "resources/day12.txt"))

(defn matches?
  ([text group]
   (let [found (re-find (re-pattern (str "(?<!#)#{" group "}(?!#)")) text)]
     (not= nil found))))

(defn replace-missing
  ([line group] (replace-missing line group 0))
  ([line group offset]
   (filterv #(matches? % group)
            (sort (set
                    (let [text (subs line offset)
                          pre (.substring line 0 offset)
                          p (re-pattern (str "(?<!#)[\\?|#]{" group "}(?!#)"))
                          replaced (if (= 1 group)
                                     (str pre (str/replace-first text #"(?<!#)\?(?!#)" "#"))
                                     (str pre (str/replace-first text p (str/join (repeat group "#")))))
                          ;_ (println line group replaced)
                          replacement-idx (loop [s1 line
                                                 s2 replaced
                                                 idx 0]
                                            (if (or (empty? s1) (not= (first s1) (first s2)))
                                              (inc idx)
                                              (recur (rest s1) (rest s2) (inc idx))
                                              ))
                          ]
                      (if (= line replaced)
                        (if (= 0 offset) [line] nil)
                        (concat
                          [replaced]
                          [line]
                          (replace-missing line group replacement-idx)
                          )
                        )
                      ))
                  ))))

(defn find-arrangements
  [line groups]
  (let [p (map (fn [group] (str "[\\.\\?]*(?<!#)#{" group "}(?!#)[\\.\\?]*")) groups)
        p (str/join p)
        p (str "^" p "$")]
    (filterv
      #(re-find (re-pattern p) %)
      (sort
        (set
          (loop [groups groups
                 arrangements [line]]
            (println arrangements)
            (if (empty? groups)
              arrangements
              (let [group (first groups)]
                (recur (rest groups)
                       (concat arrangements (mapcat #(replace-missing % group) arrangements))
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
                                                           _ (println "text= " text "groups= " groups w)

                                                           ] w)) records)
        _ (println arrangements-count)
        ]
    (apply + arrangements-count)
    )
  )
;##?.?#????.?????..# groups=  [3 3 2 1 3 1]
;###.###?##.#?###..# groups=  [3 3 2 1 3 1]
(deftest day12-test
  ;(testing "matches?"
  ;  (is (= (matches? "???.###" 3) true))
  ;  (is (= (matches? "###.###" 3) true))
  ;  (is (= (matches? "#?.###" 3) true))
  ;  (is (= (matches? "#?.###" 4) false))
  ;  (is (= (matches? "#?.###" 2) false))
  ;  (is (= (matches? "#?.###" 1) true))
  ;  )
  ;(testing "replace-missing"
  ;  (is (= (replace-missing "???.###" 3) ["###.###" "???.###"]))
  ;  (is (= (replace-missing "###.###" 3) ["###.###"]))
  ;  (is (= (replace-missing "#?#?.??#???????.??" 1) ["#?#?.#?#???????.??"
  ;                                                   "#?#?.??#?#?????.??"
  ;                                                   "#?#?.??#??#????.??"
  ;                                                   "#?#?.??#???#???.??"
  ;                                                   "#?#?.??#????#??.??"
  ;                                                   "#?#?.??#?????#?.??"
  ;                                                   "#?#?.??#??????#.??"
  ;                                                   "#?#?.??#???????.#?"
  ;                                                   "#?#?.??#???????.?#"
  ;                                                   "#?#?.??#???????.??"]))
  ;  (is (= (replace-missing "#?#?.??#???????.??" 2) ["#?##.??#???????.??"
  ;                                                   "#?#?.?##???????.??"
  ;                                                   "#?#?.??##??????.??"
  ;                                                   "#?#?.??#?##????.??"
  ;                                                   "#?#?.??#??##???.??"
  ;                                                   "#?#?.??#???##??.??"
  ;                                                   "#?#?.??#????##?.??"
  ;                                                   "#?#?.??#?????##.??"
  ;                                                   "#?#?.??#???????.##"]))
  ;  (is (= (replace-missing "#?#?.??#???????.??" 4) ["####.??#???????.??"
  ;                                                   "#?#?.####??????.??"
  ;                                                   "#?#?.?####?????.??"
  ;                                                   "#?#?.??####????.??"
  ;                                                   "#?#?.??#?####??.??"
  ;                                                   "#?#?.??#??####?.??"
  ;                                                   "#?#?.??#???####.??"]))
  ;  (is (= (replace-missing "???.###" 1) ["#??.###" "?#?.###" "??#.###"]))
  ;  (is (= (replace-missing "???.###" 2) ["##?.###" "?##.###"]))
  ;  (is (= (replace-missing "???###" 4) ["??####"]))
  ;  (is (= (replace-missing "???###" 2) ["##?###"]))
  ;  (is (= (replace-missing "??????" 4) ["####??" "?####?" "??####"]))
  ;  (is (= (replace-missing "?#?#?#?#?#?#?#?" 1) ["?#?#?#?#?#?#?#?"]))
  ;  (is (= (replace-missing "?#?#?#?#?#?#?#?" 3) ["?###?#?#?#?#?#?"
  ;                                                "?#?###?#?#?#?#?"
  ;                                                "?#?#?###?#?#?#?"
  ;                                                "?#?#?#?###?#?#?"
  ;                                                "?#?#?#?#?###?#?"
  ;                                                "?#?#?#?#?#?###?"]))
  ;  (is (= (replace-missing "?#?#?#?#?#?#?#?" 6) ["######?#?#?#?#?"
  ;                                                "?#?#?#?#?######"]))
  ;  )
  ;(testing "find-arrangements"
  ;  (is (= (find-arrangements "???.###" [1 3]) ["#??.###" "?#?.###" "??#.###"]))
  ;  (is (= (find-arrangements "???.###" [2 3]) ["##?.###" "?##.###"]))
  ;  (is (= (find-arrangements "???.###" [3 3]) ["###.###"]))
  ;  (is (= (find-arrangements "?#?#?#?#?#?#?#?" [1 3 1 6]) ["?#?###?#?######"]))
  ;  (is (= (find-arrangements "??..??.?.." [1 2]) ["#?..##.?.."
  ;                                                 "?#..##.?.."]))
  ;  (is (= (find-arrangements "??.???..???##." [2 3 1 3]) ["##.###..#?###."]))
  ;  (is (= (find-arrangements "??#?.?????#?#?#???." [2 12]) ["?##?.############?."
  ;                                                           "?##?.?############."
  ;                                                           "??##.############?."
  ;                                                           "??##.?############."]))
  ;  (is (= (find-arrangements "#.???????#" [1 1 1]) ["#.#??????#"
  ;                                                   "#.?#?????#"
  ;                                                   "#.??#????#"
  ;                                                   "#.???#???#"
  ;                                                   "#.????#??#"
  ;                                                   "#.?????#?#"]))
  (is (= (find-arrangements "##?.?#????.?????..#" [3 3 2 1 3 1]) [nil]))
  ;  (is (= (find-arrangements "#?#?.??#???????.??" [1, 2, 1, 1, 4, 2]) ["#?##.#?#?####??.##"
  ;                                                                      "#?##.#?#??####?.##"
  ;                                                                      "#?##.#?#???####.##"
  ;                                                                      "#?##.??#?#?####.##"]))
  ;  ; #?##.#?#???####.##
  ;  ; #?##.??#?#?####.##
  ;  )
  ;(testing "parse"
  ;  (is (= (parse example-input) [["???.###" [1 1 3]]
  ;                                [".??..??...?##." [1 1 3]]
  ;                                ["?#?#?#?#?#?#?#?" [1 3 1 6]]
  ;                                ["????.#...#..." [4 1 1]]
  ;                                ["????.######..#####." [1 6 5]]
  ;                                ["?###????????" [3 2 1]]]))
  ;  )
  ;(testing "part1"
  ;  (is (= (part1 example-input) 21))
  ;  (is (= (part1 puzzle-input) 0))
  ;  ;9113 zle
  ;  ;7536 zle
  ;  )
  )
