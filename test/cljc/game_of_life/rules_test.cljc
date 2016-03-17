(ns game-of-life.rules-test
  (:require #?(:clj  [clojure.test :refer [is testing deftest]]
               ;;:cljs [cljs.test :refer-macros [is testing deftest]]
                     )
            [game-of-life.rules :as util]))


(deftest cell-will-be-dead
  (testing "Living cell will die"
    (is (= false (util/will-be-alive? true 1)))
    (is (= false (util/will-be-alive? true 4)))
    (is (= false (util/will-be-alive? true 5)))
    (is (= false (util/will-be-alive? true 6)))
    (is (= false (util/will-be-alive? true 7)))
    (is (= false (util/will-be-alive? true 8))))

  (testing "Dead cell will not be born"
    (is (= false (util/will-be-alive? false 1)))
    (is (= false (util/will-be-alive? false 2)))
    (is (= false (util/will-be-alive? false 4)))
    (is (= false (util/will-be-alive? false 5)))
    (is (= false (util/will-be-alive? false 6)))
    (is (= false (util/will-be-alive? false 7)))
    (is (= false (util/will-be-alive? false 8)))))


(deftest cell-will-be-alive
  (testing "Living cell will survive"
    (is (= true (util/will-be-alive? true 2)))
    (is (= true (util/will-be-alive? true 3))))

  (testing "Dead cell will be born"
    (is (= true (util/will-be-alive? false 3)))))

(deftest check-no-of-living-neighbours-at-the-edge-of-grid
  (testing "Top left corner"
    (is (= 0 (util/number-of-living-neighbours? [0,0] [[0,0]
                                                       [0,0]])))
    (is (= 1 (util/number-of-living-neighbours? [0,0] [[0,1]
                                                       [0,0]])))
    (is (= 2 (util/number-of-living-neighbours? [0,0] [[0,1]
                                                       [1,0]])))
    (is (= 3 (util/number-of-living-neighbours? [0,0] [[0,1]
                                                       [1,1]]))))

  (testing "Top right corner"
    (is (= 0 (util/number-of-living-neighbours? [1,0] [[0,0]
                                                       [0,0]])))
    (is (= 1 (util/number-of-living-neighbours? [1,0] [[1,0]
                                                       [0,0]])))
    (is (= 2 (util/number-of-living-neighbours? [1,0] [[1,0]
                                                       [1,0]])))
    (is (= 3 (util/number-of-living-neighbours? [1,0] [[1,0]
                                                       [1,1]]))))

  (testing "Bottom left corner"
    (is (= 0 (util/number-of-living-neighbours? [0,1] [[0,0]
                                                       [0,0]])))
    (is (= 1 (util/number-of-living-neighbours? [0,1] [[1,0]
                                                       [0,0]])))
    (is (= 2 (util/number-of-living-neighbours? [0,1] [[1,1]
                                                       [0,0]])))
    (is (= 3 (util/number-of-living-neighbours? [0,1] [[1,1]
                                                       [0,1]]))))

    (testing "Bottom right corner"
    (is (= 0 (util/number-of-living-neighbours? [1,1] [[0,0]
                                                       [0,0]])))
    (is (= 1 (util/number-of-living-neighbours? [1,1] [[1,0]
                                                       [0,0]])))
    (is (= 2 (util/number-of-living-neighbours? [1,1] [[1,1]
                                                       [0,0]])))
    (is (= 3 (util/number-of-living-neighbours? [1,1] [[1,1]
                                                       [1,0]])))))

(deftest check-no-of-living-neighbours-in-the-middle-of-the-grid
  (testing "Point that has all 8 neighbours"
    (is (= 0 (util/number-of-living-neighbours? [1,1] [[0,0,0]
                                                       [0,0,0]
                                                       [0,0,0]])))
    (is (= 1 (util/number-of-living-neighbours? [1,1] [[1,0,0]
                                                       [0,0,0]
                                                       [0,0,0]])))
    (is (= 2 (util/number-of-living-neighbours? [1,1] [[1,1,0]
                                                       [0,0,0]
                                                       [0,0,0]])))
    (is (= 3 (util/number-of-living-neighbours? [1,1] [[1,1,1]
                                                       [0,0,0]
                                                       [0,0,0]])))
    (is (= 4 (util/number-of-living-neighbours? [1,1] [[1,1,1]
                                                       [1,0,0]
                                                       [0,0,0]])))
    (is (= 5 (util/number-of-living-neighbours? [1,1] [[1,1,1]
                                                       [1,0,1]
                                                       [0,0,0]])))
    (is (= 6 (util/number-of-living-neighbours? [1,1] [[1,1,1]
                                                       [1,0,1]
                                                       [1,0,0]])))
    (is (= 7 (util/number-of-living-neighbours? [1,1] [[1,1,1]
                                                       [1,0,1]
                                                       [1,1,0]])))
    (is (= 8 (util/number-of-living-neighbours? [1,1] [[1,1,1]
                                                       [1,0,1]
                                                       [1,1,1]])))))
