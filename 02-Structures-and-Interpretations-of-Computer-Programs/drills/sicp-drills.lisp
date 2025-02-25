(defpackage #:sicp-drills
  (:use #:cl)
  (:export #:sum-of-squares-of-largest-two-numbers
           #:my-sqrt
           #:my-better-sqrt))
(in-package :sicp-drills)

;; 01.01 (SICP Exercise 1.3)
;; Define a procedure that takes three numbers as arguments and
;; returns the sum of the squares of the two larger numbers.

(defun sum-of-squares (x y)
  (+ (square x)(square y)))

(defun square (x)
  (* x x))

(defun sum-of-squares-of-largest-two-numbers (n1 n2 n3)
  (cond ((>= n1 n2 n3) (sum-of-squares n1 n2))
        ((>= n1 n3 n2) (sum-of-squares n1 n3))
        ((>= n2 n1 n3) (sum-of-squares n1 n2))
        ((>= n2 n3 n1) (sum-of-squares n2 n3))
        ((>= n3 n1 n2) (sum-of-squares n3 n1))
        ((>= n3 n2 n1) (sum-of-squares n3 n2))))

;; Factorial

(defun factorial (n)
  "Compute factorial using linear recursion."
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))
(factorial 531)

(defun neo-factorial (n)
  "Compute factorial using a linear iterative process that includes a counter."
  (factorial-iter 1 1 n))
(defun factorial-iter (product counter max-count)
  (if (> counter max-count)
      product
      (factorial-iter (* product counter)
                      (+ counter 1)
                      max-count)))
(neo-factorial 1590)

;; Tree recursion vs. iterative recursion

(defun tree-fib (n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (t (+ (tree-fib (- n 1))
              (tree-fib (- n 2))))))

(defun fib (n)
  (fib-iter 1 0 n))

(defun fib-iter (a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
(tree-fib 35)
(fib 795)

(defun count-change (amount)
  (cc amount 4))

(defun cc (amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (t (+ (cc amount (- kinds-of-coins 1))
              (cc (- amount (first-denomination kinds-of-coins))
                  kinds-of-coins)))))
(defun first-denomination (kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)))

(count-change 200)

;; 01.02 (SICP Exercise 1.1.7)
;; The good-enough-p test used in computing square roots will not be very effective for finding
;; the square roots of very small numbers.
;;
;; An alternative strategy for implementing good-enough-p is to watch how
;; guess changes from one iteration to the next and to stop when the change is a very small
;; fraction of the guess. Design a square-root procedure that uses this kind of end test.
;; Does this work better for small and large numbers?

(defun my-sqrt (n)
  (sqrt-iter 1.0 n))

(defun sqrt-iter (guess x)
  (if (good-enough-p guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(defun improve (guess x)
  (average guess (/ x guess)))

(defun average (n1 n2)
  (/ (+ n1 n2) 2))

(defun good-enough-p (guess x)
  (< (abs (- (square guess) x)) 0.000001))

;; EXERCISE
(defun better-good-enough-p (previous-guess current-guess)
  (< (abs (- current-guess previous-guess)) 0.000001))

(defun sqrt-iter-better (previous-guess current-guess n)
  (if (better-good-enough-p previous-guess current-guess)
      current-guess
      (sqrt-iter-better current-guess (improve current-guess n) n)))

(defun my-better-sqrt (n)
  (sqrt-iter-better 1.0 (improve 1.0 n) n))

(defun cube (n) (* n n n))
(defun increment (n) (1+ n))
(defun sum (term n1 next n2)
  (labels ((_sum (n1 result)
             (if (> n1 n2)
                 result
                 (_sum (funcall next n1) (+ (funcall term n1) result)))))
    (_sum n1 0)))
(defun higher-sum-cubes-iter (n1 n2)
  (sum #'cube n1 #'increment n2))
(higher-sum-cubes-iter 1 10)

(defun product (term n1 next n2)
  (labels ((_product (n1 result)
             (if (> n1 n2)
                 result
                 (_product (funcall next n1)
                           (* (funcall term n1) result)))))
    (_product n1 1)))

(defun ident (x) x)
(defun higher-factorial (n)
  (product #'ident 1 #'increment n))

(defun accumulate (combiner null-value term n1 next n2)
  (labels ((_accumulate (n1 result)
             (if (> n1 n2)
                 result
                 (_accumulate (funcall next n1)
                              (funcall combiner (funcall term n1) result)))))
    (_accumulate n1 null-value)))

(defun filtered-accumulate (combiner null-value term n1 next n2 filter)
  (labels ((_accumulate (n1 result)
             (if (> n1 n2)
                 (remove-if-not filter result)
                 (_accumulate (funcall next n1)
                              (funcall combiner (funcall term n1) result)))))
    (_accumulate n1 null-value)))

;; TODO recursive version of accumulate

(defun sky-high-sum (term n1 next n2)
  (accumulate #'+ 0 term n1 next n2))

(sky-high-sum #'ident 1 #'increment 10)

(defun close-enough-p (x y)
  (< (abs (- x y)) 0.001))

(defun positive-p (n)
  (> n 0))
(defun negative-p (n)
  (< n 0))
(defun fixed-point-search (fun neg-point pos-point)
  (let ((mid-point (average neg-point pos-point)))
    (if (close-enough-p neg-point pos-point)
        mid-point
        (let ((test-value (funcall fun mid-point)))
          (cond
            ((positive-p test-value)
             (fixed-point-search fun neg-point mid-point))
            ((negative-p test-value)
             (fixed-point-search fun mid-point pos-point))
            (t mid-point))))))

(defun half-interval-method (fun n1 n2)
  (let ((n1-value (funcall fun n1))
        (n2-value (funcall fun n2)))
    (cond ((and (negative-p n1-value)
                (positive-p n2-value))
           (fixed-point-search fun n1 n2))
          ((and (negative-p n2-value)
                (positive-p n1-value))
           (fixed-point-search fun n2 n1))
          (t (error "Values ~a and ~a are not of opposite signs." n1 n2)))))

(half-interval-method #'sin 2.0 4.0)
(half-interval-method
 #'(lambda (n) (- (cube n) (* 2 n) 3)) 1.0 2.0)

(defparameter *tolerance* 0.00001)

(defun fixed-point (fun first-guess)
  (labels ((close-enough-p (v1 v2)
             (< (abs (- v1 v2)) *tolerance*))
           (try (guess)
             (let ((next-guess (funcall fun guess)))
               (if (close-enough-p guess next-guess)
                   next-guess
                   (try (print next-guess))))))
    (try first-guess)))

(fixed-point #'cos 1.0)
(fixed-point #'(lambda (y) (+ (sin y) (cos y)))
             1.0)
(defun fixed-sqrt (x)
  (fixed-point #'(lambda (y) (average y (/ x y))) 1.0))
(fixed-sqrt 36)

(defun average-damp (fun)
  #'(lambda (x)
      (average x (funcall fun x))))

(defun double (f)
  #'(lambda (x) (funcall f (funcall f x))))

(let ((dub (double (double (double #'increment)))))
  (funcall dub 5))

(defun compose (f1 f2)
  #'(lambda (x) (funcall f1 (funcall f2 x))))

(let ((inc-then-square (compose #'square #'increment)))
  (funcall inc-then-square 5))

(funcall (compose #'square #'increment) 5)

(defun repeated (f max-repeats)
  (let ((count 1))
    (labels
        ((_repeated (result-f count)
           (if (= count max-repeats)
               result-f
               (compose (_repeated f (1+ count)) f))))
      (_repeated f count))))

(let ((increment-five-times (repeated #'increment 5)))
  (funcall increment-five-times 5))

;;; Project 1, 6.001, Spring 2005

;;; idea is to simulate a baseball robot

;; imagine hitting a ball with an initial velocity of v
;; at an angle alpha from the horizontal, at a height h
;; we would like to know how far the ball travels.

;; as a first step, we can just model this with simple physics
;; so the equations of motion for the ball have a vertical and a
;; horizontal component

;; the vertical component is governed by
;; y(t) = v sin alpha t + h - 1/2 g t^2
;; where g is the gravitational constant of 9.8 m/s^2

;; the horizontal component is governed by
;; x(t) = v cos alpha t
;; assuming it starts at the origin

;; First, we want to know when the ball hits the ground
;; this is governed by the quadratic equation, so we just need to know when
;; y(t)=0 (i.e. for what t_impact is y(t_impact)= 0).
;; note that there are two solutions, only one makes sense physically

(defconstant +gravity+ 9.8)
(defconstant +pi+ 3.14159)

(defun position-at-time (acceleration velocity position time)
  (+ (/ (* acceleration (square time)) 2)
     (* velocity time)
     position))

(position-at-time 0 0 0 0)
(position-at-time 0 0 20 0)
(position-at-time 0 5 10 10)
(position-at-time 2 2 2 2)
(position-at-time 5 5 5 5)

(defun root1 (a b c)
  (let ((discriminant (- (square b) (* 4 a c))))
    (if (> 0 discriminant)
        (values (/ (- b) (* 2 a)) (/ (sqrt discriminant) (* 2 a)))
        (/ (+ (- b) (sqrt discriminant))
           (* 2 a)))))

(defun root2 (a b c)
  (let ((discriminant (- (square b) (* 4 a c))))
    (if (< 0 discriminant)
        (values (/ (- b) (* 2 a)) (/ (- (sqrt discriminant)) (* 2 a)))
        (/ (+ (- b) (sqrt discriminant))
           (* 2 a)))))


(root1 5 3 6)
(root2 5 3 6)
