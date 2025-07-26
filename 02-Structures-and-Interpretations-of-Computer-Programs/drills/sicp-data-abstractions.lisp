(defpackage #:sicp-data-abstractions
  (:use #:cl))
(in-package #:sicp-data-abstractions)

;; Exercise 2.1
;; Define a better version of make-rat that handles both positive and negative arguments.
;; Make-rat should normalize the sign so that if the rational number is positive,
;; both the numerator and denominator are positive, and if the rational number is negative,
;; only the numerator is negative.

(defun make-rat (n d)
  (let ((g (gcd n d))
        (d (abs d)))
    (cons (/ n g)
          (/ d g))))
(defun numer (rat)
  (car rat))
(defun denom (rat)
  (cdr rat))
(defun add-rat (x y)
  (make-rat
   (+ (* (numer x) (denom y))
      (* (numer y) (denom x)))
   (* (denom x) (denom y))))
(defun sub-rat (x y)
  (make-rat
   (- (* (numer x) (denom y))
      (* (numer y) (denom x)))
   (* (denom x) (denom y))))
(defun mul-rat (x y)
  (make-rat
   (* (numer x) (numer y))
   (* (denom x) (denom y))))
(defun div-rat (x y)
  (make-rat
   (* (numer x) (denom y))
   (* (numer y) (denom x))))
(defun rat-equal (x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))
(defun format-rat (rat)
  (format nil "~&~a / ~a" (numer rat) (denom rat)))

(defparameter one-half (make-rat -1 -2))
(defparameter one-third (make-rat 1 3))
(format-rat one-half)
(format-rat (add-rat one-half one-half))
(format-rat (add-rat one-third one-third))

(defun positive-rat-p (rat)
  (> (numer rat) 0))

(defun negative-rat-p (rat)
  (< (numer rat) 0))

(positive-rat-p one-half)
(negative-rat-p one-half)

;; Exercise 2.2
;; Consider the problem of representing line segments in a plane. Each segment is represented as a pair of points:
;; a starting point and an ending point. Define a constructor make-segment and selectors start-segment and end-segment
;; that define the representation of segments in terms of points. Furthermore, a point can be represented as a pair of numbers:
;; the x coordinate and the y coordinate. Accordingly, specify a constructor make-point and selectors x-point and y-point that
;; define this representation. Finally, using your selectors and constructors, define a procedure midpoint-segment that takes a
;; line segment as argument and returns its midpoint (the point whose coordinates are the average of the coordinates of the endpoints).
;; To try your procedures, you’ll need a way to print points:

(defun average (x y)
  (/ (+ x y) 2.0))
(defun make-point (x y)
  (cons x y))
(defun x-point (point)
  (car point))
(defun y-point (point)
  (cdr point))
(defun make-segment (start-point end-point)
  (cons start-point end-point))
(defun segment-start (seg)
  (car seg))
(defun segment-end (seg)
  (cdr seg))
(defun midpoint-segment (seg)
  (make-point
   (average (x-point (segment-start seg)) (x-point (segment-end seg)))
   (average (y-point (segment-start seg)) (y-point (segment-end seg)))))
(defun format-point (point)
  (format nil "~&(~a, ~a)" (x-point point) (y-point point)))

;; Exercise 2.3
;; Implement a representation for rectangles in a plane. (Hint: You may want to make use of Exercise 2.2.)
;; In terms of your constructors and selectors, create procedures that compute the perimeter and the area
;; of a given rectangle. Now implement a different representation for rectangles. Can you design your system
;; with suitable abstraction barriers, so that the same perimeter and area procedures will work using either representation?

(defun make-rectangle (seg)
  (list (segment-start seg)                         (make-point (x-point (segment-end seg))
                                                                (y-point (segment-start seg)))
        (make-point (x-point (segment-start seg))
                    (y-point (segment-end seg)))   (segment-end seg)))

(defun get-point (rectangle p)
  (nth p rectangle))
(defun get-side (rectangle)
  (make-segment (x-point (get-point rectangle 1))
                (x-point (get-point rectangle 3))))
(defun get-length (rectangle)
  (let ((start-x (x-point (get-point rectangle 0)))
        (end-x (x-point (get-point rectangle 1))))
    (abs (- end-x start-x))))

(defun get-width (rectangle)
  (let ((start-y (y-point (get-point rectangle 1)))
        (end-y (y-point (get-point rectangle 2))))
    (abs (- end-y start-y))))

(defun perimeter (rectangle)
  (let ((width (get-width rectangle))
        (length (get-length rectangle)))
    (* 2 (+ width length))))

(defun area (rectangle)
  (let* ((width (get-width rectangle))
         (length (get-length rectangle)))
    (* length width)))

(make-rectangle (make-segment (make-point )))

(let* ((p1 (make-point 3 0))
       (p2 (make-point 12 10))
       (seg (make-segment p1 p2))
       (rectangle (make-rectangle seg)))
  (midpoint-segment seg))


;; Exercise 2.7
;; Alyssa’s program is incomplete because she has not specified the implementation
;; of the interval abstraction. Here is a definition of the interval constructor:
;; (define (make-interval a b) (cons a b))
;; Define selectors upper-bound and lower-bound to complete the implementation.

(defun make-interval (a b)
  (list a b))
(defun upper-bound (interval)
  (cadr interval))
(defun lower-bound (interval)
  (car interval))
(defun add-interval (x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))
(defun multiply-interval (x y)
  (let ((p1 (* (lower-bound x)
               (lower-bound y)))
        (p2 (* (lower-bound x)
               (upper-bound y)))
        (p3 (* (upper-bound x)
               (lower-bound y)))
        (p4 (* (upper-bound x)
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(defun divide-interval (x y)
  (multiply-interval x
                     (make-interval (/ 1.0 (upper-bound y))
                                    (/ 1.0 (lower-bound y)))))

;; Exercise 2.8
;; SKIP
;; Using reasoning analogous to Alyssa’s, describe how the difference of two intervals may be computed.
;; Define a corresponding subtraction procedure, called sub-interval.
;;
;; Exercise 2.9
;; The width of an interval is half of the difference between its upper and lower bounds.
;; The width is a measure of the uncertainty of the number specified by the interval.
;; For some arithmetic operations the width of the result of combining two intervals is a function
;; only of the widths of the argument intervals, whereas for others the width of the combination is not
;; a function of the widths of the argument intervals. Show that the width of the sum (or difference) of
;; two intervals is a function only of the widths of the intervals being added (or subtracted).
;; Give examples to show that this is not true for multiplication or division.

;; Exercise 2.19

(defun except-first-denomination (coin-values)
  (rest coin-values))
(defun first-denomination (coin-values)
  (first coin-values))
(defun no-more-p (coin-values)
  (null coin-values))

(defun cc (amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0)
             (no-more-p coin-values))
         0)
        (t
         (+ (cc amount (except-first-denomination coin-values))
            (cc (- amount (first-denomination coin-values)) coin-values)))))

(defparameter *us-coins* (list 50 25 10 5 1))

(defparameter *ja-coins* (list 500 100 50 10 5 1))

(defun count-change (amount)
  (cc amount *us-coins*))
(count-change 300)


;; Exercise 2.30

(defun square-tree (tree factor)
  (flet ((square (x)
           (if (atom x)
               (* x factor)
               (square-tree x factor))))
    (mapcar #'square tree)))



;; Exercise 2.32
;; not working
(defun subsets (s)
  (if (null s)
      (list nil)
      (let ((rst (subsets (cdr s))))
        (append rst
                (mapcar #'(lambda (x) (list x (subsets s))) s)))))


;; 2.2.3

(defun square (x) (* x x))

(defun sum-odd-squares (tree)
  (cond ((null tree) 0)
        ((atom tree)
         (if (oddp tree) (square tree) 0))
        (t (+ (sum-odd-squares
               (car tree))
              (sum-odd-squares
               (cdr tree))))))


(defun fib-iter (a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
(defun fib (a)
  (fib-iter 0 1 a))
(fib 10)

(defun even-fibs (n)
  (labels ((next (k)
             (if (> k n)
                 nil
                 (let ((f (fib k)))
                   (if (evenp f)
                       (cons f (next (+ k 1)))
                       (next (+ k 1)))))))
    (next 0)))

(defun enumerate-interval (low high)
  (if (> low high)
      nil
      (cons low
            (enumerate-interval
             (+ low 1)
             high))))


(defun enumerate-tree (tree)
  (cond ((null tree) nil)
        ((atom tree) (list tree))
        (t (append
            (enumerate-tree (car tree))
            (enumerate-tree (cdr tree))))))


;; New
;; (defun sum-odd-squares (tree)
;;   (reduce #'+ (mapcar #'square (remove-if-not #'oddp (enumerate-tree tree)))))


;; (defun even-fibs (n)
;;   (remove-if-not #'evenp (mapcar #'fib (enumerate-interval 1 n))))



(defun fib-squares (n)
  (mapcar #'square (mapcar #'fib (enumerate-interval 1 n))))

(defun flatten (x)
  (cond ((null x) nil)
        ((atom x) (list x))
        (t (append (flatten (car x))
                   (flatten (cdr x))))))

(defun dividesp (a b)
  (= (mod b a) 0))

(defun find-divisor (n test-divisor)
  (cond ((> (square test-divisor) n)
         n)
        ((dividesp test-divisor n)
         test-divisor)
        (t (find-divisor
            n
            (+ test-divisor 1)))))

(defun smallest-divisor (n)
  (find-divisor n 2))

(defun primep (n)
  (= n (smallest-divisor n)))

(defun prime-sum (pair)
  (primep (+ (car pair) (cadr pair))))

(defun make-pair-sum (pair)
  (list (car pair)
        (cadr pair)
        (+ (car pair) (cadr pair))))

(defun flatmap (proc seq)
  (reduce #'append (mapcar proc seq)))

;; Exercise 2.40
(defun unique-pairs (n)
  (flatmap
   #'(lambda (i)
       (mapcar #'(lambda (j)
                   (list i j))
               (enumerate-interval
                1
                (- i 1))))
   (enumerate-interval 1 n)))

(defun prime-sum-pairs (n)
  (mapcar #'make-pair-sum
          (remove-if-not
           #'prime-sum
           (unique-pairs n))))

(defun triple-sum (triple)
  (+ (first triple) (second triple) (third triple)))

(defun make-triple-sum (triple)
  (let ((n1 (first triple))
        (n2 (second triple))
        (n3 (third triple)))
    (list n1 n2 n3 (+ n1 n2 n3))))

;; Exercise 2.41?
(defun unique-triples (n)
  (flatmap
   #'(lambda (i)
       (flatmap #'(lambda (j)
                    (mapcar #'(lambda (k) (list i j k))
                            (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))
#+nil
(unique-triples 5)

(defun triples-with-sum (n s)
  (remove-if-not #'(lambda (x) (= s (triple-sum x)))
                 (mapcar #'make-triple-sum (unique-triples n))))

(triples-with-sum 500 50)

(defun permutations (s)
  (if (null s)
      (list nil)
      (flatmap #'(lambda (x)
                   (mapcar #'(lambda (p)
                               (cons x p))
                           (permutations
                            (remove-if #'(lambda (m) (= x m)) s))))
               s)))

;; Exercise 2.36
;; The procedure accumulate-n is similar to accumulate except that it takes as
;; its third argument a sequence of sequences, which are all assumed to have
;; the same number of elements. It applies the designated accumulation
;; procedure to combine all the first elements of the sequences, all the
;; second elements of the sequences, and so on, and returns a sequence of the
;; results. For instance, if s is a sequence containing four sequences,
;; ((1 2 3) (4 5 6) (7 8 9) (10 11 12)), then the value of (accumulate-n + 0 s)
;; should be the sequence (22 26 30). Fill in the missing expressions in the
;; following definition of accumulate-n:
;;
;; (define (accumulate-n op init seqs)
;;   (if (null? (car seqs))
;;       nil
;;       (cons (accumulate op init ⟨??⟩)
;;             (accumulate-n op init ⟨??⟩))))

;; We'll call it reduce-n to follow Common Lisp naming conventions
(defun reduce-n (op seqs)
  "Like reduce, except that it takes as its third argument a sequence of sequences which are all assumed to have the same number of elements.
It applies the designated reduction procedure to combine all of the first elements of the sequences, all the second elements of the sequences, and so.

(reduce-n #'+ 0 '((1 2 3) (4 5 6))) -> (5 7 9)"
  (if (null (first seqs))
      nil
      (cons (reduce op (mapcar #'(lambda (x) (first x)) seqs))
            (reduce-n op (mapcar #'(lambda (x) (rest x)) seqs)))))
(reduce-n #'+ '((1 2 3) (4 5 6)))
;; Exercise 2.37

(defparameter *matrix* '((1 2 3 4)
                         (4 5 6 6)
                         (6 7 8 9)))

(defparameter *vec* '(4 7 8 2))

(defun dot-product (v w)
  (reduce #'+ (mapcar #'* v w)))

(defun matrix-*-vector (m v)
  (mapcar #'(lambda (x) (dot-product x v)) m))

;; This is cheating
;; (defun transpose (m)
;;   (apply #'mapcar #'list m))

;; (defun nth-column (n matrix)
;;   (mapcar #'(lambda (x) (nth n x)) matrix))
;; (defun transpose (m)
;;   (let ((col-num (length (first m))))
;;     (mapcar #'(lambda (x) (nth-column x m))
;;             (enumerate-interval 0 (1- col-num)))))

(defun transpose (m)
  (if (null (first m))
      nil
      (cons (mapcar #'(lambda (x) (first x)) m)
            (transpose (mapcar #'(lambda (x) (rest x)) m)))))

(transpose *matrix*)

(defun matrix-*-matrix (m n)
  (mapcar #'(lambda (v) (matrix-*-vector (transpose n) v)) m))

(matrix-*-matrix '((1 2) (3 4)) '((5 6) (7 8)))


(transpose *matrix*)

;; Exercise 2.38
;; The accumulate procedure is also known as fold-right, because it combines the first
;; element of the sequence with the result of combining all the elements to the right.
;; There is also a fold-left, which is similar to fold-right, except that it combines
;; elements working in the opposite direction:
(defun fold-left (op initial sequence)
  (labels ((iter (result rest)
             (if (null rest)
                 result
                 (iter (funcall op result (first rest))
                   (rest rest)))))
    (iter initial sequence)))
;; What are the values of
(fold-right #'* 1 (list 1 2 3))
(fold-left #'* 1 (list 1 2 3))

(fold-right #'list '() (list 1 2 3))
(fold-left #'list 'nil (list 1 2 3))
;; Give a property that op should satisfy to guarantee that fold-right and fold-left will produce the same values for any sequence.
;;;; I guess that the op can't create a data structure, just a single value.
;;;; Maybe it needs to result in a flattening of the data structure?

;; Exercise 2.39
;; Complete the following definitions of reverse (Exercise 2.18) in terms of fold-right and fold-left from Exercise 2.38:

;; Also known as ~accumulate~
(defun fold-right (op initial sequence)
  (if (null sequence)
      initial
      (funcall op (first sequence)
               (fold-right op
                           initial
                           (rest sequence)))))

(defun reverse-right (sequence)
  (fold-right #'(lambda (x y) (append y (list x))) nil sequence))

(defun reverse-left (sequence)
  (fold-left #'(lambda (x y) (cons y x)) nil sequence))

(reverse-right '(1 2 3 4 5))
(reverse-left '(1 2 3 4 5))

;; Exercise 2.42
;; TODO I'm stumped. Return to this one another time.

(defun make-matrix (size)
  (labels
      ((_make-matrix (count)
         (if (= count 0)
             nil
             (cons (enumerate-interval 1 size)
                   (_make-matrix (1- count))))))
    (_make-matrix size)))

(defun format-matrix (matrix)
  (mapcar #'(lambda (row)
              (format nil "~&~a"
                      row)) matrix))

;; (defun safep (k positions))

;; (defun adjoin-position (row position rest-of-queens)
;;   "A procedure that adjoins a new row-column position to a set of positions."
;;   (let ((candidates (permutations (nth (1- row) rest-of-queens))))
;;     ()))
;; (defparameter *empty-board* (make-matrix 8))
;; (defun queens (board-size)
;;   "A procedure that produces every permutation of chess board with eight queens on it where
;; all of the queens on it are safe."
;;   (labels ((queen-cols (k)
;;              (if (= k 0)
;;                  (list *empty-board*)
;;                  (remove-if-not
;;                   #'(lambda (positions) (safep k positions))
;;                   (flatmap
;;                    #'(lambda (rest-of-queens)
;;                        (mapcar
;;                         #'(lambda (new-row)
;;                             (adjoin-position new-row k rest-of-queens))
;;                         (enumerate-interval 1 board-size)))
;;                    (queen-cols (- k 1)))))))
;;     (queen-cols board-size)))

;; picking a row new-row
;; (defparameter *candidates* (permutations (enumerate-interval 1 8)))
;; (labels ((get-row (x) (nth (1- x) *candidates*))
;;          (get-row-column (x) (nth (1- x) (get-row 40320))))
;;   (get-row-column 5))

;; (length *candidates*)

;; picking a column k
;; (defparameter *k* 1)
;; (member 1 (nth (1- *k*) (transpose (list
;;                                     (enumerate-interval 1 8)
;;                                     (enumerate-interval 1 8)
;;                                     (enumerate-interval 1 8)
;;                                     (enumerate-interval 1 8)
;;                                     (enumerate-interval 1 8)
;;                                     (enumerate-interval 1 8)
;;                                     (enumerate-interval 1 8)
;;                                     (enumerate-interval 1 8)))))

;; (transpose (list
;;             (enumerate-interval 1 8)
;;             (enumerate-interval 1 8)
;;             (enumerate-interval 1 8)
;;             (enumerate-interval 1 8)
;;             (enumerate-interval 1 8)
;;             (enumerate-interval 1 8)
;;             (enumerate-interval 1 8)
;;             (enumerate-interval 1 8)))

;; Exercise 2.43
;; TODO finish after 2.42

;; Exercise 2.44
;; Define the procedure up-split used by corner-split.
;; It is similar to right-split, except that it switches the roles of below and beside.

(defun right-split (painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (1- n))))
        (beside painter
                (below smaller smaller)))))

(defun corner-split (painter n)
  (if (= n 0)
      painter
      (let* ((up (up-split painter (1- n)))
             (right (right-split painter (1- n)))
             (top-left (beside up up))
             (bottom-right (below right right))
             (corner (corner-split painter (1- n))))
        (beside (below painter top-left)
                (below bottom-right
                       corner)))))

(defun up-split (painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (1- n))))
        (above painter (beside smaller smaller)))))
