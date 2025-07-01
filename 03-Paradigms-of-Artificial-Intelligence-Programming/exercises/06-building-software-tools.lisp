(load "00-utils")
(defconstant fail nil "Indicates pat-match failure")

(defconstant no-bindings '((t . t)) "Indicates pat-match success, with no variables")

;; (defun pat-match (pattern input)
;;   "Does pattern match input? Any variable can match anything."
;;   (if (variable-p pattern)
;;       (list (cons pattern input))
;;       (if (or (atom pattern) (atom input))
;;           (eql pattern input)
;;           (append (pat-match (first pattern) (first input))
;;                   (pat-match (rest pattern) (rest input))))))

#+:#
(defun pat-match (pattern input &optional (bindings no-bindings))
  "Match pattern against input in the context of the bindings."
  (cond
    ;; This causes the recursive expansion to stop
    ;; on the next call after failure is detected.
    ((eq bindings fail) fail)
    ;; The first time a variable is spotted, variable-match will bind a ? variable.
    ;; On subsequent calls, it will reuse that variable
    ((variable-p pattern) (variable-match pattern input bindings))
    ;; If we haven't failed, and the current atom in pattern
    ;; isn't a variable, then we check to see if the current "state"
    ;; of pattern and input are the same.
    ;; If they are, we're done and should return the bindings
    ;; If they aren't, that means that either somewhere further down the chain
    ;; there's a variable, or the pattern is broken.
    ((eql pattern input) bindings)
    ((segment-pattern-p pattern)
     (segment-match pattern input bindings))
    ;; If we're looking at conses, but they're not the same,
    ;; we need to cdr down, adding bindings if necessary with
    ;; variable-match above.
    ((and (consp pattern) (consp input))
     (pat-match (rest pattern) (rest input)
                (pat-match (first pattern) (first input) bindings)))
    (t fail)))

(defun pat-match (pattern input &optional (bindings no-bindings))
  "Match pattern against input in the context of the bindings"
  (cond ((eq bindings fail) fail)
        ((variable-p pattern)
         (variable-match pattern input bindings))
        ((eql pattern input) bindings)
        ((segment-pattern-p pattern)
         (segment-matcher pattern input bindings))
        ((single-pattern-p pattern) ; ***
         (single-matcher pattern input bindings)) ; ***
        ((and (consp pattern) (consp input))
         (pat-match (rest pattern) (rest input)
                    (pat-match (first pattern) (first input)
                               bindings)))
        (t fail)))

(defun variable-match (var input bindings)
  "Does VAR match input? Uses (or updates) and returns bindings."
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
          ((equal input (binding-val binding)) bindings)
          (t fail))))

(defun variable-p (x)
  "Is ? a variable (a symbol beginning with '?')?"
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (cons var val)
        (if (eq bindings no-bindings)
            nil
            bindings)))

(defun segment-match (pattern input bindings &optional (start 0))
  "Match the segment pattern ((?* var) . pat) against input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if (null pat)
        (variable-match var input bindings)
        (let ((pos (first-match-pos (first pat) input start)))
          (if (null pos)
              fail
              (let ((b2 (pat-match pat (subseq input pos)
                                   (variable-match var (subseq input 0 pos)
                                                   bindings))))
                ;;  If this match failed, try another longer one
                (if (eq b2 fail)
                    (segment-match pattern input bindings (+ pos 1))
                    b2)))))))
(defun first-match-pos (pat1 input start)
  "Find the first position that pat1 could possibly match input,
starting at position start. If pat1 is non-constant, then just return start."
  (cond ((and (atom pat1) (not (variable-p pat1)))
         (position pat1 input :start start :test #'equal))
        ((<= start (length input)) start)
        (t nil)))

(defun rule-pattern (rule) (first rule))
(defun rule-responses (rule) (rest rule))

(defparameter *eliza-rules*
  '((((?* ?x) hello (?* ?y))
     (How do you do. Please state your problem.))
    (((?* ?x) I want (?* ?y))
     (What would it mean if you got ?y)
     (Why do you want ?y) (Suppose you got ?y soon))
    (((?* ?x) if (?* ?y))
     (Do you really think its likely that ?y) (Do you wish that ?y)
     (What do you think about ?y) (Really-- if ?y))
    (((?* ?x) no (?* ?y))
     (Why not?) (You are being a bit negative)
     (Are you saying "NO" just to be negative?))
    (((?* ?x) I was (?* ?y))
     (Were you really?) (Perhaps I already knew you were ?y)
     (Why do you tell me you were ?y now?))
    (((?* ?x) I feel (?* ?y))
     (Do you often feel ?y ?))
    (((?* ?x) I felt (?* ?y))
     (What other feelings do you have?))))

(defun eliza ()
  "Response to user input using pattern matching rules."
  (interactive-interpreter 'eliza>
                           (compose #'flatten #'use-eliza-rules)))

(defun use-eliza-rules (input)
  "Find some rule with which to transform the input."
  (rule-based-translator
   input *eliza-rules*
   :action #'(lambda (bindings responses)
               (sublis (switch-viewpoint bindings)
                       (random-elt responses)))))

(defun switch-viewpoint (words)
  "Change I to you and vice versa, and so on."
  (sublis '((I . you) (you . I) (me . you) (am . are))
          words))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapter 6
;;;;;;;;;;;;;;;;;;;;;;;;

(defun interactive-interpreter (prompt transformer)
  "Read an expression, transform it, and print the result."
  (loop
    (handler-case
        (progn
          (if (stringp prompt)
              (print prompt)
              (funcall prompt))
          (print (funcall transformer (read))))
      ;; In case of error, do this:
      (error (condition)
        (format t "~&;; Error ~a ignored, back to top level."
                condition)))))

(defun prompt-generator (&optional (num 0) (ctl-string "[~d] "))
  "Return a function that prints prompts like [l] [2] etc."
  #'(lambda () (format t ctl-string (incf num))))

;; Table entries for segment match and single match functions
(setf (get '?is 'single-match) 'match-is)
(setf (get '?or 'single-match) 'match-or)
(setf (get '?and 'single-match) 'match-and)
(setf (get '?not 'single-match) 'match-not)
(setf (get '?* 'segment-match) 'segment-match)
(setf (get '?+ 'segment-match) 'segment-match+)
(setf (get '?? 'segment-match) 'segment-match?)
(setf (get '?if 'segment-match) 'match-if)

;; data-driven programming:
;; First, we define the "glue" abstractions that work with the particular
;; data structure we've chosen.
;; 1. Predicates to determine if the data is shaped the right way.
;; 2.

(defun segment-pattern-p (pattern)
  "Is this a segment-matching pattern like ((?* var) . pat)?"
  ;; What is a segment-pattern?
  ;; It's a cons with a cons in car and a symbol in the car of the car
  ;; that matches a defined segment match function.
  (and (consp pattern) (consp (first pattern))
       (symbolp (first (first pattern)))
       ;; Semipredicate; returns a function when t, otherwise returns nil
       (segment-match-fn (first (first pattern)))))

(defun single-pattern-p (pattern)
  "Is this a single-matching pattern?
E.g. (?is x predicate) (?and . patterns) ( ?or . patterns)."
  ;; What is a single pattern?
  ;; Any cons that has a single match function in the first slot.
  (and (consp pattern)
       (single-match-fn (first pattern))))

(defun segment-matcher (pattern input bindings)
  "Call the right function for this kind of segment pattern."
  ;; segment-match-fn returns a function, which we then call.
  ;; These "matcher" functions are called dispatch functions.
  (funcall (segment-match-fn (first (first pattern)))
           pattern input bindings))

(defun single-matcher (pattern input bindings)
  "Call the right function for this kind of single pattern."
  ;; single-match-fn returns a function, which we then call.
  (funcall (single-match-fun (first pattern))
           (rest pattern) input bindings))

(defun segment-match-fn (x)
  "Get the segment-match function for x,
if it is a symbol that has one."
  ;; Above, we defined a table of symbols that map to functions.
  ;; This will return the appropriate function for the pattern.
  ;;
  ;; This modular approach--mapping the syntax of a pattern to a
  ;; function to call on its contents--allows developers to add
  ;; new syntax without touching existing code.
  ;;
  ;; Just add a pattern type table entry and function to act on
  ;; the new pattern type.
  ;;
  ;; This function is the function getter.
  (when (symbolp x) (get x 'segment-match)))

(defun single-match-fn (x)
  "Get the single-match function for x,
if it is a symbol that has one."
  (when (symbolp x) (get x 'single-match)))

;; * * * * * * * * * *
;; These are the functions that map to symbols in the above table.
;; * * * * * * * * * *

(defun match-is (var-and-pred input bindings)
  "Succeed and bind var if the unput satisfies pred,
where var-and-pred is the list (var pred)."
  (let* ((var (first var-and-pred))
         (pred (second var-and-pred))
         (new-bindings (pat-match var input bindings)))
    ;; The use of "fail" is important.
    ;; We want to be explicit that nil means failure.
    ;; This is the prevent problems discussed about semipredicates
    ;; in chapter 4.
    (if (or (eq new-bindings fail)
            (not (funcall pred input)))
        fail
        new-bindings)))

(defun match-and (patterns input bindings)
  "Succeed if all the patterns match the input."
  (cond ((eq bindings fail) fail)
        ((null patterns) bindings)
        (t (match-and (rest patterns) input
                      (pat-match (first patterns) input
                                 bindings)))))

(defun match-or (patterns input bindings)
  "Succeed if any one of the patterns match the input."
  (if (null patterns)
      fail
      (let ((new-bindings (pat-match (first patterns) input bindings)))
        (if (eq new-bindings fail)
            (match-or (rest patterns) input bindings)
            new-bindings))))

(defun match-not (patterns input bindings)
  "Succeed if none of the patterns match the input
This will never bind any variables."
  (if (match-or patterns input bindings)
      fail
      bindings))

(pat-match '(a (?* ?x) d) '(a b c d))
                                        ; => ((?X B C))

(pat-match '(my name is ?x) '(my name is micah))
(pat-match '(a (?* ?x) (?* ?y) d) '(a b c d))
                                        ; => ((?Y B C) (?X))
(pat-match  '(a (?* ?x) (?* ?y) ?x ?y)  '(a b c d (b c) (d)))
                                        ; => ((?Y D) (?X B C))

(defun segment-match+ (pattern input bindings)
  "Match one or more elements of input."
  (segment-match pattern input bindings 1))

(defun segment-match? (pattern input bindings)
  "Match zero or more element of input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (or (pat-match (cons var pat) input bindings)
        (pat-match pat input bindings))))

(defun match-if (pattern input bindings)
  "Test an arbitrary expression involving variables.
The pattern looks like ((?if code) . rest)."
  (let ((vars (mapcar #'car bindings))
        (vals (mapcar #'cdr bindings))
        (form (second (first pattern))))
    (and (progv vars vals `(funcall ,@form))
         (pat-match (rest pattern) input bindings))))

(pat-match  '(?x ?op ?y is ?z (?if (eql (?op ?x ?y) ?z))) '(3 + 4 is 7))

(defun pat-match-abbrev (symbol expansion)
  "Define symbol as a macro standing for a pat-match pattern."
  (setf (get symbol 'expand-pat-match-abbrev)
        (expand-pat-match-abbrev expansion)))

(defun expand-pat-match-abbrev (pat)
  "Expand out all pattern matching abbreviations in pat."
  (cond ((and (symbolp pat) (get pat 'expand-pat-match-abbrev)))
        ((atom pat) pat)
        (t (cons (expand-pat-match-abbrev (first pat))
                 (expand-pat-match-abbrev (rest pat))))))

(defun rule-based-translator (input rules &key (matcher #'pat-match) (rule-if #'first) (rule-then #'rest) (action #'sublis))
  "Find the first rule in rules that matches input, and apply the action to that rule."
  (some #'(lambda (rule)
            (let ((result (funcall matcher (funcall rule-if rule) input)))
              (if (not (eq result fail))
                  (funcall action result (funcall rule-then rule)))))
        rules))

;; Searching trees

(defun tree-search (states goal-p successors combiner)
  "Find a state that satisfies goal-p.  Start with states,
  and search according to successors and combiner."
  (cond ((null states) fail)
        ((funcall goal-p (first states)) (first states))
        (t (tree-search
            (funcall combiner
                     (funcall successors (first states))
                     (rest states))
            goal-p successors combiner))))

(defun depth-first-search (start goal-p successors)
  "Search new states first until goal is reached."
  (tree-search (list start) goal-p successors #'append))

(defun binary-tree (x) (list (* 2 x) (+  1 (* 2 x))))

#+nil
(depth-first-search 1 (is 12) #'binary-tree) ; infinite loop

(defun prepend (x y) "Prepend y to start of x" (append y x))

(defun breadth-first-search (start goal-p successors)
  "Search old states first until goal is reached."
  (tree-search (list start) goal-p successors #'prepend))

#+nil
(breadth-first-search 1 (is 12) #'binary-tree)


(defun finite-binary-tree (n)
  "Return a successor function that generates a binary tree
 with n nodes."
  #'(lambda (x)
      (remove-if #'(lambda (child) (> child n))
                 (binary-tree x))))

(defun diff (num)
  "Return the function that finds the difference from num."
  #'(lambda (x) (abs (- x num))))

#+nil
(funcall (diff 10) 15)

(defun sorter (cost-fn)
  "Return a combiner function that sorts according to cost-fn."
  #'(lambda (new old)
      (sort (append new old) #'< :key cost-fn)))

(defun best-first-search (start goal-p successors cost-fn)
  "Search lowest cost states first until goal is reached."
  (tree-search (list start) goal-p successors (sorter cost-fn)))

(defun price-is-right (price)
  "Return a function that measures the difference from price,
  but gives a big penalty for going over price."
  #'(lambda (x) (if (> x price)
                    most-positive-fixnum
                    (- price x))))

(best-first-search 1 (is 12) #'binary-tree (price-is-right 12))

(defun beam-search (start goal-p successors cost-fn beam-width)
  "Search highest scoring states first until goal is reached,
  but never consider more than beam-width states at a time."
  (tree-search (list start) goal-p successors
               #'(lambda (old new)
                   (let ((sorted (funcall (sorter cost-fn) old new)))
                     (if (> beam-width (length sorted))
                         sorted
                         (subseq sorted 0 beam-width))))))

(beam-search 1 (is 12) #'binary-tree (price-is-right 12) 2)

(defstruct (city (:type list)) name long lat)

(defparameter *cities*
  '((Atlanta        84.23 33.45)      (Los-Angeles       118.15 34.03)
    (Boston           71.05 42.21)      (Memphis           90.03 35.09)
    (Chicago          87.37 41.50)      (New-York          73.58 40.47)
    (Denver           105.00 39.45)     (Oklahoma-City     97.28 35.26)
    (Eugene           123.05 44.03)     (Pittsburgh        79.57 40.27)
    (Flagstaff        111.41 35.13)     (Quebec            71.11 46.49)
    (Grand-Jct        108.37 39.05)     (Reno              119.49 39.30)
    (Houston          105.00 34.00)     (San-Francisco     122.26 37.47)
    (Indianapolis     86.10 39.46)      (Tampa             82.27 27.57)
    (Jacksonville     81.40 30.22)      (Victoria          123.21 48.25)
    (Kansas-City      94.35 39.06)      (Wilmington        77.57 34.14)))

(defun neighbors (city)
  "Find all cities within 1000 kilometers."
  (find-all-if #'(lambda (c)
                   (and (not (eq c city))
                        (< (air-distance c city) 1000.0)))
               *cities*))

(defun city (name)
  "Find the city with this name."
  (assoc name *cities*))

(defstruct (path (:print-function print-path))
  state (previous nil) (cost-so-far 0) (total-cost 0))


(defun trip (start dest &optional (beam-width 1))
  "Search for the best path from the start to dest."
  (beam-search
   (make-path :state start)
   (is dest :key #'path-state)
   (path-saver #'neighbors #'air-distance
               #'(lambda (c) (air-distance c dest)))
   #'path-total-cost
   beam-width))

(defconstant earth-diameter 12765.0
  "Diameter of planet earth in kilometers.")
(defun air-distance (city1 city2)
  "The great circle distance between two cities."
  (let ((d (distance (xyz-coords city1) (xyz-coords city2))))
    ;; d is the straight-line chord between the two cities,
    ;; The length of the subtending arc is given by:
    (* earth-diameter (asin (/ d 2)))))

(defun xyz-coords (city)
  "Returns the x,y,z coordinates of a point on a sphere.
  The center is (0 0 0) and the north pole is (0 0 1)."
  (let ((psi (deg->radians (city-lat city)))
        (phi (deg->radians (city-long city))))
    (list (* (cos psi) (cos phi))
          (* (cos psi) (sin phi))
          (sin psi))))

(defun distance (point1 point2)
  "The Euclidean distance between two points.
  The points are coordinates in n-dimensional space."
  (sqrt (reduce #'+ (mapcar #'(lambda (a b) (expt (- a b) 2))
                            point1 point2))))

(defun deg->radians (deg)
  "Convert degrees and minutes to radians."
  (* (+ (truncate deg) (* (rem  deg 1) 100/60)) pi 1/180))

(defun is (value &key (key #'identity) (test #'eql))
  "Returns a predicate that tests for a given value."
  #'(lambda (path) (funcall test value (funcall key path))))

(defun path-saver (successors cost-fn cost-left-fn)
  #'(lambda (old-path)
      (let ((old-state (path-state old-path)))
        (mapcar
         #'(lambda (new-state)
             (let ((old-cost
                     (+ (path-cost-so-far old-path)
                        (funcall cost-fn old-state new-state))))
               (make-path
                :state new-state
                :previous old-path
                :cost-so-far old-cost
                :total-cost (+ old-cost (funcall cost-left-fn
                                                 new-state)))))
         (funcall successors old-state)))))
