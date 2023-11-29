(ns aoc2023.test_spec
  (:require [speclj.core :refer :all]))

(describe "Truth"

          (it "is true"
              (should true))

          (it "is true"
              (should= 1 2))

          (it "is not false"
              (should-not false)))