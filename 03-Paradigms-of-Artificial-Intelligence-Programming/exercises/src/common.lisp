(defpackage #:paip-common
  (:use #:cl)
  (:export #:mappend
           #:find-all-if
           #:*dbg-ids*
           #:dbg
           #:paip-debug
           #:undebug
           #:dbg-indent
           #:member-equal
           #:find-all
           #:starts-with
           #:flatten
           #:mklist
           #:random-elt
           #:compose))
(in-package #:paip-common)

;; * 01
(defun mappend (fn the-list)
  (apply #'append (mapcar fn the-list)))

;; * 04
(setf (symbol-function 'find-all-if) #'remove-if-not)

(defvar *dbg-ids* nil "Identifiers used by dbg")

(defun dbg (id format-string &rest args)
  "Print debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *standard-output*)
    (apply #'format t format-string args)))

(defun paip-debug (&rest ids)
  "Start dbg output on the given ids."
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun undebug (&rest ids)
  "Stop dbg on the ids.  With no ids, stop dbg altogether."
  (setf *dbg-ids* (if (null ids) nil
                      (set-difference *dbg-ids* ids))))

(defun dbg-indent (id indent format-string &rest args)
  "Print indented debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *standard-output*)
    (dotimes (i indent) (princ "  " *standard-output*))
    (apply #'format *standard-output* format-string args)))

(defun member-equal (item list)
  (member item list :test #'equal))

(defun find-all (item sequence &rest keyword-args &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item, according to the keywords. Doesn't alter sequence."
  (if test-not
      (apply #'remove item sequence :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence :test (complement test) keyword-args)))

(defun starts-with (list x)
  "Is this a list whose first element is x?"
  (and (consp list) (eql (first list) x)))

(defun orderings (l)
  (if (> (length l) 1)
      (list l (reverse l))
      (list l)))

(defun permutations (s)
  (if (null s)
      (list nil)
      (mappend #'(lambda (x)
                   (mapcar #'(lambda (p)
                               (cons x p))
                           (permutations
                            (remove-if #'(lambda (m) (equal x m)) s))))
               s)))

;; * 05
(defun flatten (the-list)
  "Append together elements (or lists) in the list."
  (mappend #'mklist the-list))

(defun mklist (x)
  "Return x if it is a list, otherwise (x)."
  (if (listp x)
      x
      (list x)))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

(defun compose (f g)
  "Return the function that computes (f (g x))."
  #'(lambda (x) (funcall f (funcall g x))))
