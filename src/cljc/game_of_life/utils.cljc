(ns game-of-life.utils
  (:require #? (:clj [clojure.core.matrix :as m]
                :cljs [clojure.core.matrix :as m])))

(defn opposite-state [cell-state]
  (if (= 1 cell-state)
    0
    1))

(defn toggle-cell
  "Switches a cell to the opposite state in the matrix.
   Dead cell will become alive and a living cell will become dead"
  [grid [x y]]
  (if (get-in grid [x y])
    (update-in grid [y x] opposite-state)
    grid))

(defn set-living-cell
  "Makes cell a living cell on the grid"
  [grid [x y]]
  (if (get-in grid [x y])
    (update-in grid [y x] (fn [_] 1))
    grid))

(defn create-empty-grid
  "Creates a new cell grid"
  [rows columns]
  (m/emap (constantly 0) (m/new-matrix rows columns)))

(defn prepend-rows
  "Prepends new rows to the matrix"
  [matrix rows]
  (let [new-rows-matrix (create-empty-grid rows (m/column-count matrix))]
    (vec (concat new-rows-matrix matrix))))

(defn append-rows
  "Appends new rows to the matrix"
  [matrix rows]
  (let [new-rows-matrix (create-empty-grid rows (m/column-count matrix))]
    (vec (concat matrix new-rows-matrix))))

(defn prepend-columns
  "Prepends new columns to the matrix"
  [matrix columns]
  (if (zero? columns)
    matrix
    (let [extra-row-items (take columns (repeat 0))]
      (vec (map #(vec (concat extra-row-items %)) matrix)))))

(defn append-columns
  "Appends new columns to the matrix"
  [matrix columns]
  (if (zero? columns)
    matrix
    (let [extra-row-items (take columns (repeat 0))]
      (vec (map #(vec (concat % extra-row-items)) matrix)))))

(defn expand-to
  "Expands a matrix to have more rows and columns"
  [matrix rows columns]
  (let [extra-rows-needed (- rows (m/row-count matrix))
        extra-rows-top (if (even? extra-rows-needed)
                            (/ extra-rows-needed 2)
                            (/ (dec extra-rows-needed) 2))
        extra-rows-bottom (- extra-rows-needed extra-rows-top)
        extra-columns-needed (- columns (m/column-count matrix))
        extra-columns-left (if (even? extra-columns-needed)
                              (/ extra-columns-needed 2)
                              (/ (dec extra-columns-needed) 2))
        extra-columns-right (- extra-columns-needed extra-columns-left)]
    ;; (println "rows: " rows  " extra-rows-needed: " extra-rows-needed
    ;;          "extra-columns-needed " extra-columns-needed)
    (-> matrix
         (prepend-rows extra-rows-top)
         (append-rows extra-rows-bottom)
         (prepend-columns extra-columns-left)
         (append-columns extra-columns-right))))
