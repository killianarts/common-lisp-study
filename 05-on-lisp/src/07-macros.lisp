(defun map0-n (fn n)
  (mapa-b fn 0 n))

(map0-n #'1+ 5)
                                        ; => (1 2 3 4 5 6)

(defun map1-n (fn n)
  (mapa-b fn 1 n))

(map1-n #'identity 5)
                                        ; => (1 2 3 4 5)

(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

(defmacro nil! (var)
  ;; Macros are functions that return lists
  `(setq ,var nil))

(defparameter *nums* (map1-n #'identity 10))

(nil! *nums*)

(defmacro nif (expr pos zero neg)
  `(case (truncate (signum ,expr))
     (1 ,pos)
     (0 ,zero)
     (-1 ,neg)))

(defmacro memq (object list)
  `(member ,object ,list :test #'eq))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))
(let ((counter 0))
  (while (< counter 10)
    (print "help I'm stuck in a loop")
    (incf counter)))

(defun make-initforms (bindforms)
  (mapcar #'(lambda (b)
              (if (consp b)
                  (list (car b) (cadr b))
                  (list b nil)))
          bindforms))

(make-initforms '((a 1) (b 2)))
(defun make-stepforms (bindforms)
  (mapcan #'(lambda (b)
              (if (and (consp b) (third b))
                  (list (car b) (third b))
                  nil))
          bindforms))

