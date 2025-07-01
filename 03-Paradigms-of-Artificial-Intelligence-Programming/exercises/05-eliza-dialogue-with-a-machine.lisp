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
         (segment-match pattern input bindings))
        ;; ((single-pattern-p pattern) ; ***
        ;;  (single-matcher pattern input bindings))
                                        ; ***
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
  "Respond to user input using pattern matching rules."
  (loop
    (print 'eliza>)
    (write (flatten (use-eliza-rules (read))) :pretty t)))

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
