(ns sliding-puzzle.core
  (:require [clojure.data.priority-map :refer :all]
            [taoensso.timbre :as timbre]
            [sliding-puzzle.cheaplist :refer :all]))

(timbre/refer-timbre)

(defrecord Grid [size tiles])

(defmulti locations class)

(defmethod locations Grid [grid]
  (let [size (:size grid) tiles (:tiles grid)]
    (zipmap tiles (for [i (range size) j (range size)] [i j]))))

(defn make-grid
  "Builds a square Grid left to right, top to bottom
   given an integer height and a vector of tiles"
   [size tiles]
   (Grid. size tiles))

(defn random-grid
  "Builds a map that represents a sliding puzzle grid.
   :size is the height/width n of the grid and :tiles
   is an n^2 - 1 random vector representing the grid state."
   [n]
   {:size n :tiles (shuffle (range (* n n)))})

(defn tile-at-aux
  "Finds the position of a tile in a grid"
  [{:keys [tiles]} tile]
  (first (keep-indexed #(when (= tile %2) %1) tiles)))

(def tile-at (memoize tile-at-aux))

(defn blank-at-aux
  "Finds the blank in the grid represented by 0"
  [grid]
  (tile-at grid 0))

(def blank-at (memoize blank-at-aux))

(defn tile-count
  "Returns the number of tiles in grid plus the blank"
  [{:keys [tiles]}]
  (count tiles))

(defn goal-aux
  "Returns the expected goal state given a grid size"
  [size]
  {:size size :tiles (conj (vec (range 1 (* size size))) 0)})

(def goal (memoize goal-aux))

(defn solved?
  "Returns true if grid is in a solved state"
  [grid]
  (= (-> grid :size goal) grid))

(defn tile-at-row-aux
  "Returns the row of the given tile"
  [{:keys [size] :as grid} tile]
  (quot (tile-at grid tile) size))

(def tile-at-row (memoize tile-at-row-aux))

(defn tile-at-column-aux
  "Returns the column of the given tile"
  [{:keys [size] :as grid} tile]
  (rem (tile-at grid tile) size))

(def tile-at-column (memoize tile-at-column-aux))

(defn tile-location-aux
  "Returns the [row column] of the given tile"
  [grid tile]
  [(tile-at-row grid tile) (tile-at-column grid tile)])

(def tile-location (memoize tile-location-aux))

(defn blank-at-row-aux
  "Returns the row of the blank tile"
  [grid]
  (tile-at-row grid 0))

(def blank-at-row (memoize blank-at-row-aux))

(defn blank-at-column-aux
  "Returns the column of the blank tile"
  [grid]
  (tile-at-column grid 0))

(def blank-at-column (memoize blank-at-column-aux))

(defn blank-location-aux
  "Returns the [row column] of the blank tile"
  [grid]
  [(blank-at-row grid) (blank-at-column grid)])

(def blank-location (memoize blank-location-aux))

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
    (and (odd?  size)    (even? (inversions grid)))
    (and (even? size) (= (even? (inversions grid)) (odd? (blank-at-row grid))))))

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

(defn manhattan-distance
  "Calculates the Manhattan distance between two points"
  [[x1 y1] [x2 y2]]
  (+ (Math/abs ^Integer (- x2 x1)) (Math/abs ^Integer (- y2 y1))))

(defn directions
  "Returns the set of possible directions for a grid"
  [grid]
  (let [ways  [:up :down :left :right]
        preds ((juxt blank-at-bottom? blank-at-top?
                blank-at-far-right? blank-at-far-left?) grid)]
    (remove nil? (map #(when-not %1 %2) preds ways))))

(defn opposite
  "Returns the opposite direction"
  [direction]
  (condp = direction
    :up    :down
    :down  :up
    :left  :right
    :right :left
    nil))

(defn slide-aux
  "Slides a tile given a grid and a direction"
  [{:keys [size tiles] :as grid} direction]
  (let [blank (blank-at grid)
        [pred index]
        (condp = direction
          :up    [(blank-at-bottom?    grid) (+   blank size)]
          :down  [(blank-at-top?       grid) (-   blank size)]
          :left  [(blank-at-far-right? grid) (inc blank)]
          :right [(blank-at-far-left?  grid) (dec blank)])
        tile index]
    (if pred
        grid
        (let [slid-grid {:size size :tiles (swap tiles blank tile)}
              slid-tile ((:tiles slid-grid) blank)
              slid-from (blank-location slid-grid)
              slid-to   (blank-location grid)
              aim       (tile-location (-> grid :size goal) slid-tile)
              prev      (manhattan-distance slid-from aim)
              curr      (manhattan-distance slid-to aim)
              fee       (if (< prev curr) 2 0)]
          (with-meta slid-grid {:tile slid-tile :fee fee :dir direction})))))

(def slide (memoize slide-aux))

(defn slide-up
  "Slides a tile up"
  [grid]
  (slide grid :up))

(defn slide-down
  "Slides a tile down"
  [grid]
  (slide grid :down))

(defn slide-left
  "Slides a tile left"
  [grid]
  (slide grid :left))

(defn slide-right
  "Slides a tile right"
  [grid]
  (slide grid :right))

(defn slides
  "Returns a vector of all possible grid states from the current one.
   This excludes any moves that result in no change to the current state."
   [grid]
   (remove #(= grid %)
           ((juxt slide-up slide-down slide-left slide-right) grid)))

(defn targets
  "Returns a list of [x y] coordinates for each tile's position in grid
   g1 in relation to grid g2"
  [g1 g2]
  (map (juxt #(tile-at-row g1 %) #(tile-at-column g1 %))
       (remove zero? (:tiles g2))))

(defn cost
  "Calculates the cost of a grid state"
  [grid]
  (let [current-state (targets grid grid)
        goal-state    (targets (-> grid :size goal) grid)]
  (reduce + (map #(manhattan-distance %1 %2) current-state goal-state))))

(defn search
  "IDA* algorithm — takes a priority queue of grid states and a bound.
   If the current grid state is solved, returns a vector of the steps
   taken to solve the initial state. If the priority exceeds the given
   bound, return it to ida-star. Otherwise, pop the current state, push
   all next states onto the priority queue with their respective costs,
   and recur with updated queue and bound."
  [states bound]
  (let [[[steps current] priority] (peek states)
        journey (conj steps current)]
    (if (solved? current)
        journey
        (if (> priority bound)
            priority
            (let [ways (for [g (map #(slide current %)
                               (remove #(= % (-> current meta :dir opposite))
                                       (directions current)))]
                            [[journey g] (+ (-> g meta :fee) priority)])]
              (recur (into (pop states) ways) bound))))))

(defn ida-star
  "IDA* wrapper — takes a solvable grid, and calls search with the
   initial state and its cost as an initial bound. If a collection
   is returned, we're done. Otherwise, recur with the returned bound."
  [grid]
  (let [cost (cost grid)
        states (priority-map [[] grid] cost)]
    (loop [threshold cost]
      (let [result (search states threshold)]
        (if (coll? result)
            result
            (recur result))))))

(defn solve
  "Returns the minimum move solution for a grid
   or an empty vector if the grid is unsolvable"
   [grid]
   (if (solvable? grid)
       (ida-star grid)
       []))

(defn -main [] )