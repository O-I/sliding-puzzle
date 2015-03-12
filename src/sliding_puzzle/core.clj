(ns sliding-puzzle.core
  (:require [clojure.data.priority-map :refer :all]))

(defn random-grid
  "Builds a map that represents a sliding puzzle grid.
   :size is the height/width n of the grid and :tiles
   is an n^2 - 1 random vector representing the grid state."
   [n]
   (hash-map :size n :tiles (shuffle (range (* n n)))))

(defn tile-at
  "Finds the position of a tile in a grid"
  [{:keys [tiles]} tile]
  (.indexOf tiles tile))

(defn blank-at
  "Finds the blank in the grid represented by 0"
  [grid]
  (tile-at grid 0))

(defn tile-count
  "Returns the number of tiles in grid plus the blank"
  [{:keys [tiles]}]
  (count tiles))

(defn goal
  "Returns the expected goal state of a grid"
  [{:keys [size] :as grid}]
  (hash-map :size size :tiles (conj (vec (range 1 (tile-count grid))) 0)))

(defn solved?
  "Returns true if grid is in a solved state"
  [grid]
  (= (goal grid) grid))

(defn tile-at-row
  "Returns the row of the given tile"
  [{:keys [size] :as grid} tile]
  (quot (tile-at grid tile) size))

(defn tile-at-column
  "Returns the column of the given tile"
  [{:keys [size] :as grid} tile]
  (rem (tile-at grid tile) size))

(defn blank-at-row
  "Returns the row of the blank tile"
  [grid]
  (tile-at-row grid 0))

(defn blank-at-column
  "Returns the column of the blank tile"
  [grid]
  (tile-at-column grid 0))

(defn slices
  "Returns a list of tile-count many lists where each
  is the previous one with the head dropped"
  [{:keys [tiles] :as grid}]
  (map #(drop % tiles) (range (tile-count grid))))

(defn inversions
  "Returns the number of inversions in a grid"
  [{:keys [tiles] :as grid}]
  (reduce +
    (map count
         (map (fn [n] (filter #(< 0 % (nth tiles n)) (drop n tiles)))
              (range (tile-count grid))))))

(defn solvable?
  "Returns true if the grid is solvable"
  [{:keys [size] :as grid}]
  (or
    (and (odd?  size)  (even? (inversions grid)))
    (and (even? size) ((even? (inversions grid)) (odd? (blank-at-row grid))))))

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
  [{:keys [size] :as grid}]
  (= (dec size) (blank-at-row grid)))

(defn blank-at-far-left?
  "Returns true if the blank tile is in the first column of a grid"
  [grid]
  (zero? (blank-at-column grid)))

(defn blank-at-far-right?
  "Returns true if the blank tile is in the last column of a grid"
  [{:keys [size] :as grid}]
  (= (dec size) (blank-at-column grid)))

(defn slide-up
  "Slides a tile up"
  [{:keys [size tiles] :as grid}]
  (if (blank-at-bottom? grid)
    grid
    (let [blank (blank-at grid) tile (+ blank size)]
      (hash-map :size size :tiles (swap tiles blank tile)))))

(defn slide-down
  "Slides a tile down"
  [{:keys [size tiles] :as grid}]
  (if (blank-at-top? grid)
    grid
    (let [blank (blank-at grid) tile (- blank size)]
      (hash-map :size size :tiles (swap tiles blank tile)))))

(defn slide-left
  "Slides a tile left"
  [{:keys [size tiles] :as grid}]
  (if (blank-at-far-right? grid)
    grid
    (let [blank (blank-at grid) tile (inc blank)]
      (hash-map :size size :tiles (swap tiles blank tile)))))

(defn slide-right
  "Slides a tile right"
  [{:keys [size tiles] :as grid}]
  (if (blank-at-far-left? grid)
    grid
    (let [blank (blank-at grid) tile (dec blank)]
      (hash-map :size size :tiles (swap tiles blank tile)))))

(defn slides
  "Returns a vector of all possible grid states from the current one.
   This excludes any moves that result in no change to the current state."
   [grid]
   (filter #(not= grid %)
           ((juxt slide-up slide-down slide-left slide-right) grid)))

(defn manhattan-distance
  "Calculates the Manhattan distance between two points"
  [u v]
  (reduce + (map #(Math/abs (- %1 %2)) u v)))

(defn targets
  "Returns a list of [x y] coordinates for each tile's position in grid
   g1 in relation to grid g2"
  [g1 g2]
  (map (juxt #(tile-at-row g1 %) #(tile-at-column g1 %))
       (filter #(not= 0 %) (:tiles g2))))

(defn cost
  "Calculates the cost of a grid state"
  [grid]
    (let [current-state (targets grid grid)
          goal-state    (targets (goal grid) grid)]
    (reduce + (map #(manhattan-distance %1 %2) current-state goal-state))))

(defn search
  "IDA* algorithm — takes a priority queue of grid states and a bound.
   If the current grid state is solved, returns a vector of the steps
   taken to solve the initial state. If the priority exceeds the given
   bound, return it to ida-star. Otherwise, pop the current state, push
   all next states onto the priority queue with their respective costs,
   and recur with updated queue and bound."
  [state bound]
  (loop [state state bound bound]
    (let [[[steps current toll] priority] (peek state)
          journey (conj steps current) fee (inc toll)]
      (if (solved? current)
          journey
          (if (> priority bound)
              priority
              (recur (into (pop state)
                      (for [g (filter #(not= % (last steps)) (slides current))]
                           [[journey g fee] (+ fee (cost g))]))
                     bound))))))

(defn ida-star
  "IDA* wrapper — takes a grid and bound, initially set to the grid's cost and
   calls search with the initial state and bound. If a collection is returned,
   we're done. Otherwise, recur with the returned bound."
  [grid bound]
    (loop [threshold bound]
      (let [state (priority-map [[] grid 0] (cost grid))
            result (search state threshold)]
        (if (coll? result)
            result
            (recur result)))))

(defn solve
  "Returns the minimum move solution for a grid
   or an empty vector if the grid is unsolvable"
   [grid]
   (if (solvable? grid)
       (ida-star grid (cost grid))
       []))

(defn -main [] )