(defpackage #:paip-test/01
  (:use #:cl #:paip-01 #:rove))

(in-package #:paip-test/01)

(deftest numbers-and-negations
  (ok (equal '(1 -1 2 -2 3 -3) (numbers-and-negations '(1 2 3)))))

(deftest mappend
  (testing "number-and-negation"
    (ok (equal '(1 -1 2 -2 3 -3) (mappend #'number-and-negation '(1 2 3))))))

(run-suite *package*)
