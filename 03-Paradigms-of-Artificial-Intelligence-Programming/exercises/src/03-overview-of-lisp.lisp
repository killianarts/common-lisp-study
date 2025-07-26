(defpackage #:paip-03
  (:use #:cl)
  (:local-nicknames (#:c #:paip-common)))
(in-package #:paip-03)
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
;; (setq 3-2-var '(b c))
;; (cons 'a 3-2-var)
;; (push 'a 3-2-var)

;; Answer: list*

;; * Exercise 3.3 [m]
;; Write a function that will print an expression in dotted pair notation.
;; Use the built-in function princ to print each component of the expression.
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

;; ** ANSWER
(defun dprint (x)
  "Print an expression in dotted pair notation."
  (cond ((atom x) (princ x))
        (t (princ "(")
           (dprint (first x))
           (pr-rest (rest x))
           (princ ")")
           x)))

(defun pr-rest (x)
  (princ " . ")
  (dprint x))

;; * Exercise 3.4 (m)
;; Write a function that, like the regular print function, will print an
;; expression in dotted pair notation when necessary but will use normal list notation when possible.

(defun print-dotted2 (exp)
  (cond
    ((null exp) (princ " . NIL"))
    ((atom exp) (princ exp))
    (t (princ "(")
       (print-dotted2 (car exp))
       (print-dotted2 (cdr exp))
       (princ ")"))))

;; ** ANSWER
;; (defun pr-rest (x)
;;   (cond ((null x))
;;         ((atom x) (princ " . ") (princ x))
;;         (t (princ " ") (dprint (first x)) (pr-rest (rest x)))))

;; (dprint '(defun sum (&rest args) (+ args)))

;; I don't get it, but whatever.

;; * Exercise 3.5 (h)
;; (Exercise in altering structure.) Write a program that will play the
;; role of the guesser in the game Twenty Questions. The user of the program will have in
;; mind any type of thing. The program will ask questions of the user, which must be
;; answered yes or no, or "it" when the program has guessed it. If the program runs out of
;; guesses, it gives up and asks the user what "it" was. At first the program will not play
;; well, but each time it plays, it will remember the user's replies and use them for subsequent guesses.

;; We need 1. Questions, 2. Memory of questions and their answers; 3. Memory of the thing in mind; 4. A list of things it could be, with a default of one item that grows over time.

(defparameter *attributes* '((is savory) (has carbs) (has meat) (is ethnic) (is brown)
                             (has vegetables) (is white) (has cheese) (has sauce) (is healthy)
                             (is red) (has chicken) (is crunchy) (is chewy) (is hard)
                             (is spicy) (is hot) (is cold) (has fruit) (has garlic)
                             (has fish) (is sweet) (is sour) (has cream) (is dessert)
                             (is soft) (is blue) (is yellow) (is green) (has eggs)))
(defparameter *meals* '(("katsu curry" (savory carbs meat ethnic popular brown white pork rice curry))
                        ("ice cream" (sweet cold fruit cream dessert popular carbs red white brown crunchy chewy blue yellow green))
                        ("pizza" (hot savory carbs meat cheese popular yellow red garlic sweet chewy soft sauce))
                        ("salad" (healthy green cheese chicken vegetables red crunchy sauce white))
                        ("teishoku" (healthy fish carbs vegetables white brown crunchy soft sour green yellow red savory))
                        ("kimchi" (healthy spicy red vegetables crunchy savory sour cold ethnic))
                        ("a donut" (popular sweet carbs brown red yellow dessert cream fruit chewy crunchy white))))

(defun number-of-questions ()
  (length *attributes*))
(defun number-of-meals ()
  (length *meals*))

(defun pick-meal ()
  (let ((meal-number (random (number-of-meals))))
    (nth meal-number *meals*)))

(defun meal-attributes (meal)
  (first (last meal)))

(defun pick-question ()
  (let ((question-number (random (number-of-questions))))
    (nth question-number *attributes*)))

(defun question-type (q)
  (first q))

(defun question-detail (q)
  (first (last q)))

(defun ask-question ()
  (let* ((q (pick-question))
         (type (question-type q))
         (detail (question-detail q)))
    (cond ((eql 'has type) (format t "~&Does it have ~a? (y or n)~&" detail))
          ((eql 'is type) (format t "~&Is it ~a? (y or n)~&" detail)))
    detail))

(defun ask-questions ()
  (let ((clues '()))
    (dotimes (n 20 n)
      (let ((question-detail (ask-question))
            (answer (read)))
        (if (or (eql 'y answer)
                (eql 'yes answer))
            (push question-detail clues))))
    clues))

(defun meal-score (meal)
  (first (last meal)))
(defun meal-name (meal)
  (first meal))

(defun assign-score (meal clues)
  (length (intersection (meal-attributes meal) clues)))

(defun pick-answer (clues)
  (let ((candidate nil)
        (candidate-score 0))
    (loop for meal in *meals*
          if (or (null candidate)
                 (> (assign-score meal clues)
                    candidate-score))
            do (setf candidate meal)
          do (setf candidate-score (assign-score meal clues)))
    candidate))

(defun add-to-meals (clues)
  (format t "~&What is it?~&")
  (let ((answer (read-line)))
    (push (list answer clues) *meals*)))

(defun play ()
  (let* ((clues (ask-questions))
         (answer (pick-answer clues)))
    (format t "~&Is it ~a? (y or n)~&" (meal-name answer))
    (let ((player-answer (read)))
      (cond ((or (eql 'y player-answer)
                 (eql 'yes player-answer))
             (format t "I win!"))
            (t (add-to-meals (print clues))
               (format t "I'll get you next time..."))))))

;; * Exercise 3.6 (s)
;; Given the following initialization for the lexical variable a and the special variable *b*, what will be the value of the let form?

;; (setf a 'global-a)
;; (defvar *b* 'global-b)
;; (defun fn () *b*)
;; (let ((a 'local-a)
;;       (*b* 'local-b))
;;   (list a *b* (fn) (symbol-value 'a) (symbol-value '*b*)))

;; * Exercise 3.7 (s)
;; Why do you think the leftmost of two keys is the one that counts, rather than the rightmost?
;; They're read first.
;; * Exercise 3.8 (m)
;; Some versions of Kyoto Common Lisp (KCL) have a bug wherein they use the rightmost value when more than one keyword/value pair is specified for the same keyword. Change the definition of find-all so that it works in KCL.
;; Nah
;; * Exercise 3.9 (m)
;; Write a version of length using the function reduce.
(defun length-with-reduce (list)
  (reduce #'+ (mapcar #'(lambda (x) 1) list)))

;; * Exercise 3.10 (m)
;; Use a reference manual or DESCRIBE to figure out what the functions LCM and NRECONC do

;; LCM
;; Return the least common multiple of one or more integers. LCM of no arguments is defined to be 1.

;; NRECONC
;; Return (NCONC (NREVERSE X) Y)

;; * Exercise 3.11 (m)
;; There is a built-in Common Lisp function that, given a key, a value, and an
;; association list, returns a new association list that is extended to include
;; the key/value pair. What is the name of this function?

;; subst

;; * TODO Exercise 3.12 (m)
;; Write a single expression using format that will take a list of words and print
;; them as a sentence, with the first word capitalized and a period after the last word.
;; You will have to consult a reference to learn new format directives.

;; ~@( ... ~) for capitalizing the first word
;; (format nil"~{~@(~a~)~}" '(hi my name is micah))
;; Who the hell knows, man?
