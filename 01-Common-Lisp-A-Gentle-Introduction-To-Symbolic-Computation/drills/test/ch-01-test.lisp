(in-package :touretzky-drills/test)

(def-suite* touretzky-drills-suite)

(test miles-per-gallon
  (is (equal 10 (drills:miles-per-gallon 500 600 10))))

(test longer-than-2-5
  (is (equal 'nil (drills:longer-than '(a b) '(c d e f g)))))

(test longer-than-5-2
  (is (equal 't (drills:longer-than '(a b c d e) '(f g)))))

(test addlength
  (is (equal '(3 a b c) (drills:addlength '(a b c)))))

(test addlength-double
  (is (equal '(4 3 a b c) (drills:addlength (drills:addlength '(a b c))))))

(test firstp-t
  (is (equal 't (drills:firstp 'mikasa '(mikasa ena)))))

(test firstp-nil
  (is (equal 'nil (drills:firstp 'ena '(mikasa ena)))))

(test my-abs-negative-number
  (is (equal 5 (drills:my-abs -5))))

(test my-abs-float
  (is (equal 5.4321 (drills:my-abs -5.4321))))

(test mid-add1
  (is (equal '(TAKE 4 COOKIES) (drills:mid-add1 '(TAKE 3 COOKIES)))))
(test f-to-c
  (is (equal 0 (drills:f-to-c 32))))

(defun run-tests (&optional (test-or-suite 'touretzky-drills-suite))
  "Provides human readable results of test run. Default to entire suite."
  (run! test-or-suite))
(run-tests)
