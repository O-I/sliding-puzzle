(ns sliding-puzzle.core-test
  (:require [clojure.test :refer :all]
            [sliding-puzzle.core :refer :all]))

(deftest random-grid-test
  (testing "random-grid"
    (let [puz (random-grid 3)]
      (is (= 3 size))
      (is (= 9 (count puz)))
      (is (= (set (range 0 9)) (set puz))))))

(deftest blank-at-test
  (testing "blank-at"
    (is (= 6 (blank-at [2 3 1 4 7 5 0 6 8])))))

(deftest tile-count-test
  (testing "tile-count"
    (is (= 9 (tile-count (random-grid 3))))))

(deftest solved?-test
  (testing "solved?"
      (is (true?  (solved? [1 2 3 4 5 6 7 8 0])))
      (is (false? (solved? [1 5 2 3 4 0 7 8 6])))))