;; The math for this is way over my head
;; From: https://www.wolframalpha.com/input/?i=Pascal%27s+triangle
;; The sum of the nth row is 2^n
;; The sum of rows 0 through n is 2^n+1 - 1
;; The generating function of the nth row is (x + 1)^n
;;
;; I guess that x == the sum
;; The first row is 1 because the 2^0 = 1? Or simply because we start at one?
;;
;; Then we take the sum of the row, add 1, then take it to the nth power?

(defun r-factorial (n)
  "Compute factorial using linear recursion."
  (if (= n 1)
      1
      (* n (r-factorial (- n 1)))))

(defun factorial (n)
  "Compute factorial using a linear iterative process that includes a counter."
  (factorial-iter 1 1 n))
(defun factorial-iter (product counter max-count)
  (if (> counter max-count)
      product
      (factorial-iter (* counter product)
                      (+ counter 1)
                      max-count)))

(defun sum-of-row (row)
  (expt 2 row))
(defun sum-of-rows (row)
  "The sum of all of the rows from 0 to row"
  (1- (expt 2 (1+ row))))

(defun coefficient (n k)
  (/ (factorial n)
     (* (factorial k)
        (factorial (- n k)))))

(defun pascal-triangle (height row rows)
  (if (= height row)
      '()
      (pascal-triangle height (1+ row) (append rows (coefficient height row)))))

(defparameter *matrix* (make-list 5 :initial-element '(1 2 3 4 5)))

(defun increment-nums-in-matrix (matrix)
  (let ((new-matrix (make-list (length matrix)
                               :initial-element (make-list (length (nth 0 matrix))
                                                           :initial-element 0))))
    (dotimes (row (length matrix))
      (dotimes (col (length (nth row matrix)))
        (setf (nth col (nth row new-matrix)) (1+ (nth col (nth row *list*))))))
    new-matrix))

(increment-nums-in-matrix *matrix*)
