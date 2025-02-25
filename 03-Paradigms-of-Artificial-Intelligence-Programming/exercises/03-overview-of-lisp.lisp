;; EXERCISES
;; * Exercise 3.1 [m] Show a lambda expression that is equivalent to the above let* expression. You may need more than one lambda.

(let* ((x 6)
       (y (* x x)))
  (+ x y))

(funcall (lambda (a b) (+ a b))
         6
         (funcall (lambda (y) (* y y)) 6))

((lambda (a) (+ a (* a a))) 6)

;; * Exercise 3.2 [s] The function cons can be seen as a special case of one of the other functions listed previously. Which one?

;; push?
(setq 3-2-var '(b c))
(cons 'a 3-2-var)
(push 'a 3-2-var)

;; * TODO Exercise 3.3 [m] Write a function that will print an expression in dotted pair notation.
;; * Use the built-in function princ to print each component of the expression.
;; AI GENERATED ANSWER
(defun print-dotted (exp)
  (cond
    ((null exp) (princ "NIL"))          ; Base case: empty list prints as NIL
    ((atom exp) (princ exp))            ; Atomic case: print numbers, symbols, etc.
    (t (princ "(")                      ; Start of a cons cell
       (print-dotted (car exp))         ; Print the car
       (princ " . ")                    ; Print the dot separator
       (print-dotted (cdr exp))         ; Print the cdr
       (princ ")"))))                   ; Close the cons cell
(print-dotted '(defun sum (&rest args) (+ args)))


(defun as-dotted-pair (list)
  (cond ((null list) (princ list))
        ((atom list) (princ list))
        (t (princ "(") (as-dotted-pair (first list)) (as-dotted-pair (rest list)))))

(as-dotted-pair '(a b c))

;; I don't know what this question is asking.
;; (as-dotted-pair '(hello my name is micah)) => (HELLO (MY (NAME (IS (MICAH)))))?
;; (as-dotted-pair '(hello my name is micah)) => (HELLO . (MY . (NAME . (IS . (MICAH . NIL)))))?

(as-dotted-pair '(defun sum (&rest args) (+ args)))

;; * TODO Exercise 3.4 [m] Write a function that, like the regular print function, will print an
;; * expression in dotted pair notation when necessary but will use normal list notation when possible.


;; * Exercise 3.5 [h] (Exercise in altering structure.) Write a program that will play the
;; * role of the guesser in the game Twenty Questions. The user of the program will have in
;; * mind any type of thing. The program will ask questions of the user, which must be
;; * answered yes or no, or "it" when the program has guessed it. If the program runs out of
;; * guesses, it gives up and asks the user what "it" was. At first the program will not play
;; * well, but each time it plays, it will remember the user's replies and use them for subsequent guesses.


;; * Exercise 3.6 [s] Given the following initialization for the lexical variable a and the special variable *b*, what will be the value of the let form?
(setf a 'global-a)
(defvar *b* 'global-b)

(defun fn () *b*)

(let ((a 'local-a)
      (*b* 'local-b))
  (list a *b* (fn) (symbol-value 'a) (symbol-value '*b*)))

;; * Exercise 3.7 [s] Why do you think the leftmost of two keys is the one that counts, rather than the rightmost?
They're read first.
;; * Exercise 3.8 [m] Some versions of Kyoto Common Lisp (KCL) have a bug wherein they use the rightmost value when more than one keyword/value pair is specified for the same keyword. Change the definition of find-all so that it works in KCL.
