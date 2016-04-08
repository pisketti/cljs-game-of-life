(ns game-of-life.utils-test
  (:require #?(:clj  [clojure.test :refer [is testing deftest]]
               ;;:cljs [cljs.test :refer-macros [is testing deftest]]
                     )
            [game-of-life.utils :as util]))


(deftest toggle-cell
  (testing "A living cell will be switched to dead cell"
    (is (= [[0,0]
            [0,0]] (util/toggle-cell  [[1,0]
                                       [0,0]] [0,0])))
    (is (= [[0,0,0,0]
            [0,0,0,0]
            [0,0,0,0]
            [0,0,0,0]] (util/toggle-cell  [[0,0,0,0]
                                           [0,0,0,0]
                                           [0,0,0,0]
                                           [0,0,1,0]] [2,3]))))
  (testing "A dead cell will be switched to a living cell"
    (is (= [[1,0]
            [0,0]] (util/toggle-cell [[0,0]
                                      [0,0]] [0,0]))))
  (testing "Returns matrix as is if if cell not on matrix"
    (is (= [[1,1]
            [1,1]] (util/toggle-cell [[1,1]
                                      [1,1]] [3,0]))))
  (testing "Toggles the last cell on the matrix"
    (is (= [[0,0,0,0]
            [0,0,0,1]] (util/toggle-cell [[0,0,0,0]
                                          [0,0,0,0]] [3,1]))))

  (testing "Toggles the last cell of a long row matrix"
    (let [long-rowmatrix (util/create-empty-grid 1 50)
          last-cell-set-matrix (util/toggle-cell long-rowmatrix [49 0])
          last-cell-value (-> last-cell-set-matrix first last)]
      (is (= 1 last-cell-value)))))

(deftest create-empty-grid
  (testing "Creates a grid with given dimensions"
    (is (= [[0,0]
            [0,0]] (util/create-empty-grid 2 2)))

    (is (= [[0,0,0,0]
            [0,0,0,0]
            [0,0,0,0]
            [0,0,0,0]] (util/create-empty-grid 4 4)))

    (is (= [[0,0,0,0]
            [0,0,0,0]] (util/create-empty-grid 2 4)))))

(deftest adds-rows-to-a-matrix
  (testing "Adding now new rows should keep the matrix as is"
    (is (= [[1,1]
            [1,1]] (util/expand-to [[1,1]
                                    [1,1]] 2 2))))

  (testing "Adds 1 row to a matrix"
    (is (= [[1,1]
            [1,1]
            [0,0]] (util/expand-to [[1,1]
                                    [1,1]] 3 2))))

  (testing "Adds 2 rows to a matrix"
    (is (= [[0,0]
            [1,1]
            [1,1]
            [0,0]] (util/expand-to [[1,1]
                                    [1,1]] 4 2))))

  (testing "Adds 3 rows to a matrix"
    (is (= [[0,0]
            [1,1]
            [1,1]
            [0,0]
            [0,0]] (util/expand-to [[1,1]
                                    [1,1]] 5 2))))

  (testing "Adds 4 rows to a matrix"
    (is (= [[0,0]
            [0,0]
            [1,1]
            [1,1]
            [0,0]
            [0,0]] (util/expand-to [[1,1]
                                    [1,1]] 6 2))))

  (testing "Adds 5 rows to a matrix"
    (is (= [[0,0]
            [0,0]
            [1,1]
            [1,1]
            [0,0]
            [0,0]
            [0,0]] (util/expand-to [[1,1]
                                    [1,1]] 7 2))))


  (testing "Attempting to decrease rows (less than original) should not affect the matrix"
    (is (= [[1,1]
            [1,1]] (util/expand-to [[1,1]
                                    [1,1]] 1 2)))))


(deftest adds-columns-to-a-matrix

  (testing "Adds one column to a 2x2 matrix"
    (is (= [[1,1,0]
            [1,1,0]] (util/expand-to [[1,1]
                                      [1,1]] 2 3))))
  (testing "Adds two columns to a 2x2 matrix"
    (is (= [[0,1,1,0]
            [0,1,1,0]] (util/expand-to [[1,1]
                                        [1,1]] 2 4))))
  (testing "Adds three columns to a 2x2 matrix"
    (is (= [[0,1,1,0,0]
            [0,1,1,0,0]] (util/expand-to [[1,1]
                                          [1,1]] 2 5))))
  (testing "Adds four columns to a 2x2 matrix"
    (is (= [[0,0,1,1,0,0]
            [0,0,1,1,0,0]] (util/expand-to [[1,1]
                                            [1,1]] 2 6))))
  (testing "Adds five columns to a 2x2 matrix"
    (is (= [[0,0,1,1,0,0,0]
            [0,0,1,1,0,0,0]] (util/expand-to [[1,1]
                                              [1,1]] 2 7))))

  (testing "Attempting to decrease columns (less than original) should not affect the matrix"
    (is (= [[1,1]
            [1,1]] (util/expand-to [[1,1]
                                    [1,1]] 2 1)))))

(deftest adds-both-rows-and-columns-to-a-matrix

  (testing "Adds one row and one column to a 2x2 matrix"
    (is (= [[1,1,0]
            [1,1,0]
            [0,0,0]] (util/expand-to [[1,1]
                                      [1,1]] 3 3))))
  (testing "Adds two rows and two columns to a 2x2 matrix"
    (is (= [[0,0,0,0]
            [0,1,1,0]
            [0,1,1,0]
            [0,0,0,0]] (util/expand-to [[1,1]
                                        [1,1]] 4 4))))
  (testing "Adds three rows and three columns to a 2x2 matrix"
    (is (= [[0,0,0,0,0]
            [0,1,1,0,0]
            [0,1,1,0,0]
            [0,0,0,0,0]
            [0,0,0,0,0]] (util/expand-to [[1,1]
                                          [1,1]] 5 5))))
  (testing "Adds four rows and four columns to a 2x2 matrix"
    (is (= [[0,0,0,0,0,0]
            [0,0,0,0,0,0]
            [0,0,1,1,0,0]
            [0,0,1,1,0,0]
            [0,0,0,0,0,0]
            [0,0,0,0,0,0]] (util/expand-to [[1,1]
                                            [1,1]] 6 6))))

  (testing "Adds four rows and four columns to a 2x2 matrix"
    (is (= [[0,0,0,0,0,0]
            [0,0,0,0,0,0]
            [0,0,1,1,0,0]
            [0,0,1,1,0,0]
            [0,0,0,0,0,0]
            [0,0,0,0,0,0]] (util/expand-to [[1,1]
                                            [1,1]] 6 6))))

(testing "Adds 5 rows and 3 columns to a 1x3 matrix"
    (is (= [[0,0,0,0,0,0]
            [0,0,0,0,0,0]
            [0,1,1,1,0,0]
            [0,0,0,0,0,0]
            [0,0,0,0,0,0]
            [0,0,0,0,0,0]] (util/expand-to [[1,1,1]] 6 6)))))
