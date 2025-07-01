(defpackage pcl.ch3
  (:use :cl)
  (:export #:prompt-for-review))

(in-package #:pcl.ch3)
(defparameter *db* nil)

(defun add-record (review)
  (push review *db*))

;; What is a book review?
#+nil
(list :title "Common Lisp: A Gentle Introduction to Symbolic Computation"
      :author "David Touretzky"
      :length 587
      :difficulty "easy"
      :review "Great book. Lots of exercises. No CLOS, limited information on data structures beyond lists."
      :recommend? t)

(defun make-book-review (title author difficulty review recommend?)
  (list :title title :author author :difficulty difficulty :review review :recommend? recommend?))

(defun print-db ()
  (dolist (row *db*)
    (format t "~{~a:~15t~a~%~}~%" row)))

#+nil
(add-record (make-book-review "Common Lisp: A Gentle Introduction to Symbolic Computation"
                              "David Touretzky"
                              1
                              "Great book. Lots of exercises. No CLOS, limited information on data structures beyond lists."
                              t))
#+nil
(print-db)

(defun prompt-for (key)
  (format *query-io* "~a: " key)
  (read-line *query-io*))

(defun prompt-for-review ()
  (make-book-review
   (prompt-for "Title")
   (prompt-for "Author")
   (or (parse-integer (prompt-for "Difficulty") :junk-allowed t) 1)
   (prompt-for "Review")
   (y-or-n-p "Do you recommend others read this book? [y/n]: ")))

(defun add-reviews ()
  (loop (add-record (prompt-for-review))
        (if (not (y-or-n-p "Add another review? ")) (return))))

(defun save-db (filename)
  (with-open-file (out (merge-pathnames *default-pathname-defaults* filename)
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))
#+nil
(save-db "~/book-reviews.sexp")

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))
