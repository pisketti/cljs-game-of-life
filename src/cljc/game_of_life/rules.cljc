(ns game-of-life.rules
  (:require #? (:clj [clojure.core.matrix :as m]
                :cljs [clojure.core.matrix :as m])))

(defn will-be-alive?
  "Checks if the cell will live or be born in the next generation"
  [alive-now? no-of-live-neighbours]
  ;;(println "alive-now: " alive-now?)
  (if alive-now?
    (contains? #{2 3} no-of-live-neighbours)
    (= no-of-live-neighbours 3)))

(defn point-val
  "Gets the value of point in a grid when coordinates given as [x,y]"
  [[x y] grid]
  (get-in grid [y, x]))

(defn number-of-living-neighbours?
  "Gets the living neighbours for a cell in a grid"
  [[x y] grid]
  (let [neighbours [;; The row above
                    (point-val [(dec x), (dec y)] grid)
                    (point-val [x,       (dec y)] grid)
                    (point-val [(inc x), (dec y)] grid)

                    ;; The same row
                    (point-val [(dec x),  y] grid)
                    (point-val [(inc x),  y] grid)

                    ;; The row below
                    (point-val [(dec x), (inc y)] grid)
                    (point-val [x,       (inc y)] grid)
                    (point-val [(inc x), (inc y)] grid)]]
    ;;(println "x:"x " y:"y " grid:"grid " neighbours: " neighbours)
    (->> neighbours
         (remove nil?)
         (apply +))))

(defn will-live-on-the-grid?
  "Checks if the cell will live or be born in the next generation"
  [[x y] grid]
  (let [current-value (point-val [x y] grid)
        alive-now? (= 1 current-value)
        living-neighbours (number-of-living-neighbours? [x y] grid)]
    ;;(println "[x y]: " [x y] " current-value:" current-value)
    (will-be-alive? alive-now? living-neighbours)))

(defn next-generation
  "Creates the next generation for the current cells"
  [grid]
  (let [new-point (fn [[y x] _]
                    (if (will-live-on-the-grid? [x y] grid)
                      1
                      0))]
    (m/emap-indexed new-point grid)))
