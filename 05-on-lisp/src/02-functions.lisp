(defpackage on-lisp-02
  (:use :cl))
(in-package #:on-lisp-02)
;; Functions are data objects
(defun sqrt (x)
  (* x x))

(sqrt 5)
