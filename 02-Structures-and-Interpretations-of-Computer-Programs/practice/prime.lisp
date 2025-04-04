(defun square (x) (* x x))
(defun smallest-divisor (n)
  (find-divisor n 2))
(defun divides-p (a b)
  (= (rem b a) 0))
(defun find-divisor (n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides-p test-divisor n) test-divisor)
        (t (find-divisor n (1+ test-divisor)))))
(defun prime-p (n)
  (= n (smallest-divisor n)))
(get-internal-run-time)
(prime-p (get-internal-run-time))
