(defpackage #:touretzky-drills
  (:use #:cl)
  (:export #:miles-per-gallon
           #:longer-than
           #:addlength
           #:firstp
           #:my-abs
           #:mid-add1
           #:f-to-c
           #:make-even
           #:further
           #:my-not
           #:ordered
           #:constrain-cond
           #:constrain-if
           ))
(in-package :touretzky-drills)

;; 01.01
;; Define a function MILES-PER-GALLON that takes three inputs, called INITIAL-ODOMETER-READING,
;; FINAL-ODOMETER-READING, and GALLONS-CONSUMED, and computes the number of miles traveled per gallon of gas.
(defun miles-per-gallon (initial-odometer-reading final-odometer-reading gallons-consumed)
  (/ (- final-odometer-reading initial-odometer-reading)
     gallons-consumed))

;; 01.02
;; Define a predicate called LONGER-THAN that takes two lists as input and returns T if the first list is longer than the second.
(defun longer-than (list1 list2)
  (> (length list1) (length list2)))

;; 01.03
;; Write a function ADDLENGTH that takes a list as input and returns a new list with the length of the input added onto the front of it.
;; If the input is (MOO GOO GAI PAN), the output should be (4 MOO GOO GAI PAN). What is the result of (ADDLENGTH (ADDLENGTH ’(A B C)))?
(defun addlength (list)
  (cons (length list) list))

;; 01.04
;; Write a predicate FIRSTP that returns T if its first argument (a symbol) is equal to the first element of
;; its second argument (a list). That is, (FIRSTP ’FOO '(FOO BAR BAZ)) should return T.
;; (FIRSTP ’BOING ’(FOO BAR BAZ)) should return NIL.
(defun firstp (symbol list)
  (equal symbol (first list)))

;; 01.05
;; Write a function MID-ADD1 that adds 1 to the middle element of a three-element list.
;; For example, (MID-ADD1 ’(TAKE 2 COOKIES)) should return the list (TAKE 3 COOKIES).
;; Note: You are not allowed to make MID-ADD1 a function of three inputs. It has to take a single input that is a list of three elements.
(defun mid-add1 (list)
  (list (first list) (1+ (second list)) (third list)))

;; 01.05
;; Write a function F-TO-C that converts a temperature from Fahrenheit to Celsius.
;; The formula for doing the conversion is: Celsius temperature = [5 × (Fahrenheit temperature - 32)]/9.
;; To go in the opposite direction, the formula is: Fahrenheit temperature = (9/5 × Celsius temperature) + 32.
(defun f-to-c (temperature)
  (/ (* 5 (- temperature 32)) 9))

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
        ((equal (first x) ’bad) (cons ’awful (rest x)))))

(defun emphasize2 (x)
  (cond ((equal (first x) ’good) (cons ’great (rest x)))
        ((equal (first x) ’bad) (cons ’awful (rest x)))
        (t x)))

(defun emphasize3 (x))


;; TODO 02.07
;; Write a function CONSTRAIN that takes three inputs called X, MAX, and MIN. If X is less than MIN, it should return MIN;
;; if X is greater than MAX, it should return MAX. Otherwise, since X is between MIN and MAX, it should return X.
;; (CONSTRAIN 3 -50 50) should return 3. (CONSTRAIN 92 -50 50) should return 50. Write one version using COND and another
;; using nested IFs.
(defun constrain-cond (n min max))
(defun constrain-if (n min max))
