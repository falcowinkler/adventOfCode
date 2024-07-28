(ns day1.core-test
  (:require [clojure.test :refer :all]
            [day1.core :refer :all]))

(deftest test-update-state
  (testing "state is updated correctly"
    (is (= (update-state initial-state "L3")
           {:direction [(- 1) 0] :location [(- 3) 0]}))
    (is (= (update-state {:direction [1 0] :location [2 0]} "L3")
           {:direction [0 (- 1)] :location [2 (- 3)]}))
    (is (= (update-state {:direction [1 0] :location [5 4]} "R3")
           {:direction [0 1] :location [5 7]}))))

(deftest test-solve
  (testing "if solve function solves example inputs correctly"
    (is (= (solve1 "R2, L3") 5))
    (is (= (solve1 "R2, R2, R2") 2))
    (is (= (solve1 "R5, L5, R5, R3") 12))))
