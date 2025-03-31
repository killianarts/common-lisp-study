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

(defun segment-pattern-p (pattern)
  "Is this a segment matching patern: ((?* var) . pat)."
  (and (consp pattern)
       (starts-with (first pattern) '?*)))

(defun segment-match (pattern input bindings &optional (start 0))
  "Match the segment pattern ((?* var) . pat) against input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if (null pat)
        (variable-match var input bindings)
        ;; We assume that pat starts with a constant
        ;; In other words, a pattern can't have 2 consecutive vars
        (let ((pos (position (first pat) input :start start :test #'equal)))
          (if (null pos)
              fail
              (let ((b2 (pat-match pat (subseq input pos)
                                   (variable-match var (subseq input 0 pos)
                                                   bindings))))
                ;;  If this match failed, try another longer one
                ;;  If it worked, check that the variables match
                (if (eq b2 fail)
                    (segment-match pattern input bindings (+ pos 1))
                    b2)))))))

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
  (some #'(lambda (rule)
            (let ((result (pat-match (rule-pattern rule) input)))
              (if (not (eq result fail))
                  (sublis (switch-viewpoint result)
                          (random-elt (rule-responses rule))))))
        *eliza-rules*))

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
