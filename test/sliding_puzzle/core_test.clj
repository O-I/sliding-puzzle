(ns sliding-puzzle.core-test
  (:require [clojure.test :refer :all]
            [sliding-puzzle.core :refer :all]))

(deftest random-grid-test
  (testing "random-grid"
    (let [puz (random-grid 3)]
      (is (= 3 (:size puz)))
      (is (= 9 (count (:tiles puz))))
      (is (= (set (range 9)) (set (:tiles puz)))))))

(deftest tile-at-test
  (testing "tile-at"
    (is (= 6 (tile-at {:tiles [2 3 1 4 7 5 0 6 8]} 0)))))

(deftest blank-at-test
  (testing "blank-at"
    (is (= 6 (blank-at {:tiles [2 3 1 4 7 5 0 6 8]})))))

(deftest tile-count-test
  (testing "tile-count"
    (is (= 9 (tile-count (random-grid 3))))))

(deftest goal-test
  (testing "goal"
    (is (=       {:size 3 :tiles [1 2 3 4 5 6 7 8 0]}
           (goal {:size 3 :tiles [2 3 1 4 7 5 0 6 8]})))))

(deftest solved?-test
  (testing "solved?"
    (is (true?  (solved? {:size 3 :tiles [1 2 3 4 5 6 7 8 0]})))
    (is (false? (solved? {:size 3 :tiles [1 5 2 3 4 0 7 8 6]})))))

(deftest tile-at-row-test
  (testing "tile-at-row"
    (is (= 2 (tile-at-row {:size 3 :tiles [2 3 1 4 7 5 0 6 8]} 0)))))

(deftest tile-at-column-test
  (testing "tile-at-column"
    (is (= 0 (tile-at-column {:size 3 :tiles [2 3 1 4 7 5 0 6 8]} 0)))))

(deftest blank-at-row-test
  (testing "blank-at-row"
    (is (= 2 (blank-at-row {:size 3 :tiles [2 3 1 4 7 5 0 6 8]})))))

(deftest blank-at-column-test
  (testing "blank-at-column"
    (is (= 0 (blank-at-column {:size 3 :tiles [2 3 1 4 7 5 0 6 8]})))))

(deftest slices-test
  (testing "slices"
    (is (= '((1 2 3 4) (2 3 4) (3 4) (4))
           (slices {:tiles [1 2 3 4]})))))

(deftest inversions-test
  (testing "inversions"
    (is (= 8 (inversions {:tiles [2 6 3 1 0 4 7 8 5]})))
    (is (= 0 (inversions {:tiles [1 2 3 4 5 6 7 8 0]})))))

(deftest solvable?-test
  (testing "solvable?"
    (is (true?  (solvable? {:size 3 :tiles [8 6 7 2 5 4 3 0 1]})))
    (is (false? (solvable? {:size 3 :tiles [1 4 6 5 2 0 8 3 7]})))
    (is (false? (solvable? {:size 3 :tiles [1 2 3 4 5 6 8 7 0]})))))

(deftest swap-test
  (testing "swap"
    (is (= [1 2 4 3] (swap [1 2 3 4] 2 3)))))

(deftest blank-at-top?-test
  (testing "blank-at-top?"
    (is (true?  (blank-at-top? {:size 3 :tiles [1 2 0 3 4 5 6 7 8]})))
    (is (false? (blank-at-top? {:size 3 :tiles [1 2 3 4 5 0 6 7 8]})))))

(deftest blank-at-bottom?-test
  (testing "blank-at-bottom?"
    (is (true?  (blank-at-bottom? {:size 3 :tiles [1 2 3 4 5 6 0 7 8]})))
    (is (false? (blank-at-bottom? {:size 3 :tiles [1 2 3 4 5 0 6 7 8]})))))

(deftest blank-at-far-left?-test
  (testing "blank-at-far-left?"
    (is (true?  (blank-at-far-left? {:size 3 :tiles [1 2 3 0 4 5 6 7 8]})))
    (is (false? (blank-at-far-left? {:size 3 :tiles [1 2 3 4 5 0 6 7 8]})))))

(deftest blank-at-far-right?-test
  (testing "blank-at-far-right?"
    (is (true?  (blank-at-far-right? {:size 3 :tiles [1 2 3 4 5 0 6 7 8]})))
    (is (false? (blank-at-far-right? {:size 3 :tiles [1 2 3 4 0 5 6 7 8]})))))

(deftest slide-up-test
  (testing "slide-up"
    (is (=           {:size 3 :tiles [1 2 3 4 5 6 7 0 8]}
           (slide-up {:size 3 :tiles [1 2 3 4 0 6 7 5 8]})))
    (is (=           {:size 3 :tiles [1 2 3 4 5 6 7 0 8]}
           (slide-up {:size 3 :tiles [1 2 3 4 5 6 7 0 8]})))))

(deftest slide-down-test
  (testing "slide-down"
    (is (=             {:size 3 :tiles [1 0 3 4 2 6 7 5 8]}
           (slide-down {:size 3 :tiles [1 2 3 4 0 6 7 5 8]})))
    (is (=             {:size 3 :tiles [1 0 3 4 2 6 7 5 8]}
           (slide-down {:size 3 :tiles [1 0 3 4 2 6 7 5 8]})))))

(deftest slide-left-test
  (testing "slide-left"
    (is (=             {:size 3 :tiles [1 2 3 4 6 0 7 5 8]}
           (slide-left {:size 3 :tiles [1 2 3 4 0 6 7 5 8]})))
    (is (=             {:size 3 :tiles [1 2 3 4 5 0 6 7 8]}
           (slide-left {:size 3 :tiles [1 2 3 4 5 0 6 7 8]})))))

(deftest slide-right-test
  (testing "slide-right"
    (is (=              {:size 3 :tiles [1 2 3 0 4 6 7 5 8]}
           (slide-right {:size 3 :tiles [1 2 3 4 0 6 7 5 8]})))
    (is (=              {:size 3 :tiles [1 2 3 4 5 6 0 7 8]}
           (slide-right {:size 3 :tiles [1 2 3 4 5 6 0 7 8]})))))

(deftest slides-test
  (testing "slides"
    (is (=        [{:size 3 :tiles [1 2 3 4 5 6 7 0 8]}
                   {:size 3 :tiles [1 0 3 4 2 6 7 5 8]}
                   {:size 3 :tiles [1 2 3 4 6 0 7 5 8]}
                   {:size 3 :tiles [1 2 3 0 4 6 7 5 8]}]
           (slides {:size 3 :tiles [1 2 3 4 0 6 7 5 8]})))
    (is (=        [{:size 3 :tiles [1 2 3 4 5 0 7 8 6]}
                   {:size 3 :tiles [1 2 3 4 5 6 7 0 8]}]
           (slides {:size 3 :tiles [1 2 3 4 5 6 7 8 0]})))))

(deftest manhattan-distance-test
  (testing "manhattan-distance"
    (is (= 0 (manhattan-distance [0 0] [0 0])))
    (is (= 4 (manhattan-distance [1 2] [3 0])))
    (is (= 6 (manhattan-distance [0 0] [3 3])))))

(deftest targets-test
  (testing "targets"
    (let [grid {:size 3 :tiles [8 6 7 2 5 4 3 0 1]}
          goal {:size 3 :tiles [1 2 3 4 5 6 7 8 0]}]
      (is (= '([0 0] [0 1] [0 2] [1 0] [1 1] [1 2] [2 0] [2 2])
              (targets grid grid))
      (is (= '([2 1] [1 2] [2 0] [0 1] [1 1] [1 0] [0 2] [0 0])
              (targets goal grid)))))))

(deftest cost-test
  (testing "cost"
    (is (= 21 (cost {:size 3 :tiles [8 6 7 2 5 4 3 0 1]})))
    (is (= 0  (cost {:size 3 :tiles [1 2 3 4 5 6 7 8 0]})))))

(deftest solve-test
  (testing "solve, ida-star, search"
    (let [unsolvable {:size 3 :tiles [6 4 1 8 2 7 5 0 3]}
            solvable {:size 3 :tiles [2 6 3 1 0 4 7 8 5]}]
    (is (= [] (solve unsolvable)))
    (is (= '([2 6 3 1 0 4 7 8 5] [2 0 3 1 6 4 7 8 5] [0 2 3 1 6 4 7 8 5]
             [1 2 3 0 6 4 7 8 5] [1 2 3 7 6 4 0 8 5] [1 2 3 7 6 4 8 0 5]
             [1 2 3 7 0 4 8 6 5] [1 2 3 7 4 0 8 6 5] [1 2 3 7 4 5 8 6 0]
             [1 2 3 7 4 5 8 0 6] [1 2 3 7 4 5 0 8 6] [1 2 3 0 4 5 7 8 6]
             [1 2 3 4 0 5 7 8 6] [1 2 3 4 5 0 7 8 6] [1 2 3 4 5 6 7 8 0])
           (map :tiles (solve solvable)))))))
