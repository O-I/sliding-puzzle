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

(defn solvable?
  "Returns true if the grid is solvable"
  [grid]
  (or
    (and (odd?  (:size grid))  (even? (inversions grid)))
    (and (even? (:size grid)) ((even? (inversions grid)) (odd? (blank-at-row grid))))))

(defn swap
  "Swaps two elements at positions i and j in vector v"
  [v i j]
  (assoc v j (v i) i (v j)))

(defn blank-at-top?
  "Returns true if the blank tile is in the first row of a grid"
  [grid]
  (zero? (blank-at-row grid)))

(defn blank-at-bottom?
  "Returns true if the blank tile is in the last row of a grid"
  [grid]
  (= (dec (:size grid)) (blank-at-row grid)))

(defn blank-at-far-left?
  "Returns true if the blank tile is in the first column of a grid"
  [grid]
  (zero? (blank-at-column grid)))

(defn blank-at-far-right?
  "Returns true if the blank tile is in the last column of a grid"
  [grid]
  (= (dec (:size grid)) (blank-at-column grid)))

(defn slide-up
  "Slides a tile up"
  [grid]
  (if (blank-at-bottom? grid)
    grid
    (let [size (:size grid) blank (blank-at grid) tile (+ blank size)]
      (hash-map :size size :tiles (swap (:tiles grid) blank tile)))))

(defn slide-down
  "Slides a tile down"
  [grid]
  (if (blank-at-top? grid)
    grid
    (let [size (:size grid) blank (blank-at grid) tile (- blank size)]
      (hash-map :size size :tiles (swap (:tiles grid) blank tile)))))

(defn slide-left
  "Slides a tile left"
  [grid]
  (if (blank-at-far-right? grid)
    grid
    (let [size (:size grid) blank (blank-at grid) tile (inc blank)]
      (hash-map :size size :tiles (swap (:tiles grid) blank tile)))))

(defn slide-right
  "Slides a tile right"
  [grid]
  (if (blank-at-far-left? grid)
    grid
    (let [size (:size grid) blank (blank-at grid) tile (dec blank)]
      (hash-map :size size :tiles (swap (:tiles grid) blank tile)))))

(defn slides
  "Returns a vector of all possible grid states from the current one.
   This excludes any moves that result in no change to the current state."
   [grid]
   (filter #(not= grid %)
           ((juxt slide-up slide-down slide-left slide-right) grid)))

(defn -main [] )