
(let ((nums '(1 2 3 4 5 6 7 8 9 10)))
  (values (remove-if-not (complement #'evenp) nums)))
                                        ; => (2 4 6 8 10), (2 4 6 8 10), (2 4 6 8 10)


(defun memoize (fn)
  "This is afunction for caching return values and
searching the hash-table for previous values when called."
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win
              val
              (setf (gethash args cache)
                    (apply fn args)))))))

(setq slow-id (memoize #'(lambda (x) (sleep 5) x)))

(time (funcall slow-id 1))
