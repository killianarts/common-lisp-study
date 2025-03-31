(in-package :touretzky-drills/test)
(def-suite* touretzky-drills-suite)
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
(test ordered--5-5
  (is (equal '(-5 5) (touretzky-drills:ordered 5 -5))))
;; TODO (test emphasize3)
(test constrain-cond
  (is (equal 50 (touretzky-drills: 5 -5))))
;; TODO (test constrain-if)

(defun run-tests (&optional (test-or-suite 'touretzky-drills-suite))
  "Provides human readable results of test run. Default to entire suite."
  (run! test-or-suite))
(run-tests)
