(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "sicp-drills")
  (quicklisp-client:quickload :fiveam))

(defpackage :sicp-drills-test
  (:use :cl :fiveam)
  (:export :run-tests))

(in-package :sicp-drills-test)

(def-suite* sicp-drills-suite)

(test sum-of-squares-of-largest-two-numbers-1-2-3
  (is (equal 13 (sicp-drills:sum-of-squares-of-largest-two-numbers 1 2 3))))

(test sum-of-squares-of-largest-two-numbers-1-3-2
  (is (equal 13 (sicp-drills:sum-of-squares-of-largest-two-numbers 2 1 3))))

(test sum-of-squares-of-largest-two-numbers-2-1-3
  (is (equal 13 (sicp-drills:sum-of-squares-of-largest-two-numbers 2 1 3))))

(test sum-of-squares-of-largest-two-numbers-2-3-1
  (is (equal 13 (sicp-drills:sum-of-squares-of-largest-two-numbers 2 3 1))))

(test sum-of-squares-of-largest-two-numbers-3-1-2
  (is (equal 13 (sicp-drills:sum-of-squares-of-largest-two-numbers 3 1 2))))

(test sum-of-squares-of-largest-two-numbers-3-2-1
  (is (equal 13 (sicp-drills:sum-of-squares-of-largest-two-numbers 3 2 1))))

(test sum-of-squares-of-largest-two-numbers-7-7-7
  (is (equal 98 (sicp-drills:sum-of-squares-of-largest-two-numbers 7 7 7))))

(test sum-of-squares-of-largest-two-numbers-7-7-8
  (is (equal 113 (sicp-drills:sum-of-squares-of-largest-two-numbers 7 7 8))))

(test sum-of-squares-of-largest-two-numbers-7-8-7
  (is (equal 113 (sicp-drills:sum-of-squares-of-largest-two-numbers 7 8 7))))

(test sum-of-squares-of-largest-two-numbers-0-0-0
  (is (equal 0 (sicp-drills:sum-of-squares-of-largest-two-numbers 0 0 0))))

(test my-better-sqrt-large-number
  (is (equal (sqrt
              99999999999999999999999999999999999999)
             (sicp-drills:my-better-sqrt
              99999999999999999999999999999999999999))))

(test my-better-sqrt-small-number
  (is (equal 3.1706231e-6 (sicp-drills:my-better-sqrt 0.00000000001))))

(defun run-tests (&optional (test-or-suite 'sicp-drills-suite))
  "Provides human readable results of test run. Default to entire suite."
  (run! test-or-suite))

(run-tests)
