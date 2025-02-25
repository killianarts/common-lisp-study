(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "touretzky-drills")
  (quicklisp-client:quickload :fiveam))

(defpackage :touretzky-drills-test
  (:use :cl :fiveam)
  (:export :run-tests))

(in-package :touretzky-drills-test)

(def-suite* touretzky-drills-suite)

(test miles-per-gallon
  (is (equal 10 (touretzky-drills:miles-per-gallon 500 600 10))))

(test longer-than-2-5
  (is (equal 'nil (touretzky-drills:longer-than '(a b) '(c d e f g)))))

(test longer-than-5-2
  (is (equal 't (touretzky-drills:longer-than '(a b c d e) '(f g)))))

(test addlength
  (is (equal '(3 a b c) (touretzky-drills:addlength '(a b c)))))

(test addlength-double
  (is (equal '(4 3 a b c) (touretzky-drills:addlength (touretzky-drills:addlength '(a b c))))))

(test firstp-t
  (is (equal 't (touretzky-drills:firstp 'mikasa '(mikasa ena)))))

(test firstp-nil
  (is (equal 'nil (touretzky-drills:firstp 'ena '(mikasa ena)))))

(test my-abs-negative-number
  (is (equal 5 (touretzky-drills:my-abs -5))))

(test my-abs-float
  (is (equal 5.4321 (touretzky-drills:my-abs -5.4321))))

(test mid-add1
  (is (equal '(TAKE 4 COOKIES) (touretzky-drills:mid-add1 '(TAKE 3 COOKIES)))))
(test f-to-c
  (is (equal 0 (touretzky-drills:f-to-c 32))))
(test make-even
  (is (equal 6 (touretzky-drills:make-even 5))))

(test further-positive
  (is (equal 5 (touretzky-drills:further 4))))

(test further-negative
  (is (equal -5 (touretzky-drills:further -4))))

(test further-zero
  (is (equal 0 (touretzky-drills:further 0))))

(test my-not-t
  (is (equal 't (touretzky-drills:my-not '(hello is anybody there)))))

(test my-not-nil
  (is (equal 'nil (touretzky-drills:my-not '()))))

(test ordered-4-3
  (is (equal '(3 4) (touretzky-drills:ordered 4 3))))

(test ordered-50-100
  (is (equal '(50 100) (touretzky-drills:ordered 50 100))))

(test ordered-7-7
  (is (equal '(7 7) (touretzky-drills:ordered 7 7))))

;; TODO (test emphasize3)
;; TODO (test constrain-cond)
;; TODO (test constrain-if)

(defun run-tests (&optional (test-or-suite 'touretzky-drills-suite))
  "Provides human readable results of test run. Default to entire suite."
  (run! test-or-suite))
(run-tests)
