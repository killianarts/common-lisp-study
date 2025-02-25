(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "paip-drills")
  (quicklisp-client:quickload :fiveam))

(defpackage :paip-drills-test
  (:use :cl :fiveam)
  (:export :run-tests))

(in-package :paip-drills-test)

(def-suite* paip-drills-suite)
