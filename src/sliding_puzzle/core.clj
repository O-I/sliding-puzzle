(ns sliding-puzzle.core)

(defn random-grid
  "Builds a random n x n vector with unique values from
   0 to n^2 - 1 that represents a sliding puzzle grid"
   [n]
   (def size n)
   (shuffle (range 0 (* n n))))

(defn blank-at
  "Finds the blank in the grid represented by 0"
  [grid]
  (.indexOf grid 0))

(defn tile-count
  "Returns the number of tiles in grid plus the blank"
  [grid]
  (count grid))

(defn solved?
  "Returns true if grid is in a solved state"
  [grid]
  (= (conj (vec (range 1 (tile-count grid))) 0) grid))

(defn -main [] )