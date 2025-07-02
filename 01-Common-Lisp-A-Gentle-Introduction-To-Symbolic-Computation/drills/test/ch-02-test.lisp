(in-package :touretzky-drills/test)
(def-suite* touretzky-drills-suite)
(test make-even
  (is (equal 6 (drills:make-even 5))))

(test further-positive
  (is (equal 5 (drills:further 4))))

(test further-negative
  (is (equal -5 (drills:further -4))))

(test further-zero
  (is (equal 0 (drills:further 0))))

(test my-not-t
  (is (equal 't (drills:my-not '(hello is anybody there)))))

(test my-not-nil
  (is (equal 'nil (drills:my-not '()))))

(test ordered-4-3
  (is (equal '(3 4) (drills:ordered 4 3))))

(test ordered-50-100
  (is (equal '(50 100) (drills:ordered 50 100))))

(test ordered-7-7
  (is (equal '(7 7) (drills:ordered 7 7))))
(test ordered--5-5
  (is (equal '(-5 5) (drills:ordered 5 -5))))
(test emphasize-good-day
  (is (equal '(drills::great drills::day) (drills:emphasize '(drills::good drills::day)))))
(test emphasize-bad-day
  (is (equal '(drills::awful drills::day) (drills:emphasize '(drills::bad drills::day)))))
(test emphasize2-weird-day
  (is (equal '(drills::very drills::weird drills::day) (drills:emphasize '(drills::weird drills::day)))))
(test constrain-cond-300--50-50
  (is (equal 50 (drills:constrain-cond 300 -50 50))))
(test constrain-cond-3--50-50
  (is (equal 3 (drills:constrain-cond 3 -50 50))))
(test constrain-if-300--50-50
  (is (equal 50 (drills:constrain-if 300 -50 50))))
(test constrain-if-3--50-50
  (is (equal 3 (drills:constrain-if 3 -50 50))))
(defun run-tests (&optional (test-or-suite 'touretzky-drills-suite))
  "Provides human readable results of test run. Default to entire suite."
  (run! test-or-suite))
(run-tests)
