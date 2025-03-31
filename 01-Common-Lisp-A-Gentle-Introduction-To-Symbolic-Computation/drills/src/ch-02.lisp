(in-package :touretzky-drills)

;; 02.01
;; Write a function MAKE-EVEN that makes an odd number even by adding one to it.
;; If the input to MAKE-EVEN is already even, it should be returned unchanged.
(defun make-even (n)
  (if (evenp n)
      n
      (1+ n)))


;; 02.02
;; Write a function FURTHER that makes a positive number larger by adding one to it,
;; and a negative number smaller by subtracting one from it. What does your function do if given the number 0?
(defun further (n)
  (cond ((> n 0) (1+ n))
        ((< n 0) (1- n))
        (t n)))


;; 02.03
;; Recall the primitive function NOT: It returns NIL for a true input and T for a false one.
;; Suppose Lisp didn’t have a NOT primitive. Show how to write NOT using just IF and constants (no other functions).
(defun my-not (input)
  (if input
      't
      'nil))


;; 02.04
;; Write a function ORDERED that takes two numbers as input and makes a list of them in ascending order.
;; (ORDERED 3 4) should return the list (3 4). (ORDERED 4 3) should also return (3 4), in other words,
;; the first and second inputs should appear in reverse order when the first is greater than the second.
(defun ordered (n1 n2)
  (if (>= n2 n1)
      (list n1 n2)
      (list n2 n1)))

;; 02.05
;; Write a version of the absolute value function MY-ABS using COND instead of IF.
(defun my-abs (n)
  (cond ((> n 0) n)
        ((= n 0) n)
        (t (- n))))

;; TODO 02.06
;; Write EMPHASIZE3, which is like EMPHASIZE2 but adds the symbol VERY onto the list if it doesn't know how to emphasize it.
;; For example, EMPHASIZE3 of (LONG DAY) should produce (VERY LONG DAY). What does EMPHASIZE3 of (VERY LONG DAY) produce?
(defun emphasize (x)
  (cond ((equal (first x) 'good) (cons 'great (rest x)))
        ((equal (first x) 'bad) (cons 'awful (rest x)))))


(defun emphasize2 (x)
  (cond ((equal (first x) 'good) (cons 'great (rest x)))
        ((equal (first x) 'bad) (cons 'awful (rest x)))
        (t x)))

(defun emphasize3 (x)
  (cond ((equal (first x) 'good) (cons 'great (rest x)))
        ((equal (first x) 'bad) (cons 'awful (rest x)))
        (t (cons 'very x))))


;; TODO 02.07
;; Write a function CONSTRAIN that takes three inputs called X, MAX, and MIN. If X is less than MIN, it should return MIN;
;; if X is greater than MAX, it should return MAX. Otherwise, since X is between MIN and MAX, it should return X.
;; (CONSTRAIN 3 -50 50) should return 3. (CONSTRAIN 92 -50 50) should return 50. Write one version using COND and another
;; using nested IFs.
(defun constrain-cond (n min max)
  (cond ((< n min) min)
        ((< max n) max)
        (t n)))
(defun constrain-if (n min max))
