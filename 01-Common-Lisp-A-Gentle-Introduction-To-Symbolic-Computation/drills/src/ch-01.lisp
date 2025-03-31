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
