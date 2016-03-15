(ns game-of-life.rules-test
  (:require #?(:clj  [clojure.test :refer [is testing deftest]]
               ;;:cljs [cljs.test :refer-macros [is testing deftest]]
                     )
            [game-of-life.rules :as util]
            ))

;; ;; (deftest foo-test
;; ;;   (is false))

(deftest a-test
  (testing "FIXME, I fail UTIL 2."
    (is (= (util/foo-cljc 99) 5))))

(deftest b-test
  (testing "meh"
    (is (= :a :d))))
