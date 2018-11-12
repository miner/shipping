(ns miner.test-shipping
  (:require [clojure.test :refer :all]
            [miner.shipping :refer :all]))

(deftest attempt-1-test
  (testing "attempt-1"
    (is (= (count (attempt-1 legs)) 2414))))


(deftest attempt-2-test
  (testing "attempt-2"
    (is (= (count (attempt-2 legs)) 2414))))


(deftest shipping-test
  (testing "shipping"
    (is (= (count (shipping legs)) 2414))))

(deftest shipping1-test
  (testing "shipping1"
    (is (= (count (shipping1 legs)) 2414))))





