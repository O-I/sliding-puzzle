(ns sliding-puzzle.core)

(defn random-grid
  "Builds a map that represents a sliding puzzle grid.
   :size is the height/width n of the grid and :tiles
   is an n^2 - 1 random vector representing the grid state."
   [n]
   (hash-map :size n :tiles (shuffle (range (* n n)))))

(defn blank-at
  "Finds the blank in the grid represented by 0"
  [grid]
  (.indexOf (:tiles grid) 0))

(defn tile-count
  "Returns the number of tiles in grid plus the blank"
  [grid]
  (count (:tiles grid)))

(defn solved?
  "Returns true if grid is in a solved state"
  [grid]
  (= (conj (vec (range 1 (tile-count grid))) 0) (:tiles grid)))

(defn blank-at-row
  "Returns the row of the blank tile"
  [grid]
  (quot (blank-at grid) (:size grid)))

(defn blank-at-column
  "Returns the column of the blank tile"
  [grid]
  (rem (blank-at grid) (:size grid)))

(defn slices
  "Returns a list of tile-count many lists where each
  is the previous one with the head dropped"
  [grid]
  (map #(drop % (:tiles grid)) (range (tile-count grid))))

(defn inversions
  "Returns the number of inversions in a grid"
  [grid]
  (reduce +
    (map count
         (map (fn [n] (filter #(< 0 % (nth (:tiles grid) n)) (drop n (:tiles grid))))
              (range (tile-count grid))))))

(defn -main [] )