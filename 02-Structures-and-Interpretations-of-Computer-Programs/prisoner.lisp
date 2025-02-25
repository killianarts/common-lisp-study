;;
;;  The play-loop procedure takes as its  arguments two prisoner's
;;  dilemma strategies, and plays an iterated game of approximately
;;  one hundred rounds.  A strategy is a procedure that takes
;;  two arguments: a history of the player's previous plays and
;;  a history of the other player's previous plays.  The procedure
;;  returns either a "c" for cooperate or a "d" for defect.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun play-loop (strat0 strat1)
  (labels ((play-loop-iter (strat0 strat1 count history0 history1 limit)
             (cond ((= count limit) (print-out-results history0 history1 limit))
                   (t (let ((result0 (funcall strat0 history0 history1))
                            (result1 (funcall strat1 history1 history0)))
                        (play-loop-iter strat0 strat1 (+ count 1)
                                        (extend-history result0 history0)
                                        (extend-history result1 history1)
                                        limit))))))
    (play-loop-iter strat0 strat1 0
                    the-empty-historyp
                    the-empty-historyp
                    (+ 90 (random 21)))))


(play-loop #'eye-for-eye #'eye-for-two-eyes)
(play-loop #' #'eye-for-two-eyes)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The following procedures are used to compute and print
;;  out the players' scores at the end of an iterated game
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-play (&rest args) args)

(defparameter the-empty-historyp '())

(defun extend-history (a b) (cons a b))
(defun empty-historyp (arg) (null arg))

(defun most-recent-play (some-cons) (car some-cons))
(defun rest-of-plays (some-cons) (cdr some-cons))


(defun print-out-results (history0 history1 number-of-games)
  (let ((scores (get-scores history0 history1)))
    (format t "~&Player 1 Score: ~a~&" (* 1.0 (/ (car scores) number-of-games)))
    (format t "~&Player 2 Score: ~a~&" (* 1.0 (/ (cadr scores) number-of-games)))))

(defun get-scores (history0 history1)
  (labels ((get-scores-helper (history0 history1 score0 score1)
             (cond ((empty-historyp history0)
                    (list score0 score1))
                   (t (let ((game (make-play (most-recent-play history0)
                                             (most-recent-play history1))))
                        (get-scores-helper (rest-of-plays history0)
                                           (rest-of-plays history1)
                                           (+ (get-player-points 0 game) score0)
                                           (+ (get-player-points 1 game) score1)))))))
    (get-scores-helper history0 history1 0 0)))

(defun get-player-points (num game)
  (nth num (get-point-list game)))

(defparameter *game-association-list*
  ;; format is that first sublist identifies the players' choices
  ;; with "c" for cooperate and "d" for defect; and that second sublist
  ;; specifies payout for each player
  '((("c" "c") (3 3))
    (("c" "d") (0 5))
    (("d" "c") (5 0))
    (("d" "d") (1 1))))

(defparameter *play* (make-play "c" "d"))
(defun extract-entry (play scoring-patterns)
  (remove-if #'null (mapcan #'(lambda (pattern) (if (equal (first pattern)
                                                           play)
                                                    pattern))
                            scoring-patterns)))

(cadr (extract-entry '("c" "c") *game-association-list*))
(defun get-point-list (game)
  (cadr (extract-entry game *game-association-list*)))

;; note that you will need to write extract-entry


;; A sampler of strategies

(defun nasty (my-history other-history)
  "d")

(defun patsy (my-history other-history)
  "c")

(defun spastic (my-history other-history)
  (if (= (random 2) 0)
      "c"
      "d"))

(defun egalitarian  (my-history other-history)
  (labels ((count-instances-of (test hist)
             (cond ((empty-historyp hist) 0)
                   ((string= (most-recent-play hist) test)
                    (+ (count-instances-of test (rest-of-plays hist)) 1))
                   (t (count-instances-of test (rest-of-plays hist))))))
    (let ((ds (count-instances-of "d" other-history))
          (cs (count-instances-of "c" other-history)))
      (if (> ds cs) "d" "c"))))

(defun eye-for-eye (my-history other-history)
  (if (empty-historyp my-history)
      "c"
      (most-recent-play other-history)))
(defun eye-for-two-eyes (my-history other-history)
  (if (and (empty-historyp my-history)
           (string= "d" (most-recent-play other-history))
           (string= "d" (most-recent-play (rest-of-plays other-history))))
      "d"
      "c"))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; code to use in 3 player game
;;

                                        ;(defun *game-association-list*
                                        ;  (list (list (list "c" "c" "c") (list 4 4 4))
                                        ;        (list (list "c" "c" "d") (list 2 2 5))
                                        ;        (list (list "c" "d" "c") (list 2 5 2))
                                        ;        (list (list "d" "c" "c") (list 5 2 2))
                                        ;        (list (list "c" "d" "d") (list 0 3 3))
                                        ;        (list (list "d" "c" "d") (list 3 0 3))
                                        ;        (list (list "d" "d" "c") (list 3 3 0))
                                        ;        (list (list "d" "d" "d") (list 1 1 1))))


;; in expected-values: f = don't care
;;                      X = actual-value needs to be #f or X
;; (defun test-entry (expected-values actual-values)
;;   (cond ((null expected-values) (null actual-values))
;;         ((null actual-values) f)
;;         ((or (not (car expected-values))
;;              (not (car actual-values))
;;              (= (car expected-values) (car actual-values)))
;;          (test-entry (cdr expected-values) (cdr actual-values)))
;;         (t f)))

;; (defun is-he-a-foolp (hist0 hist1 hist2)
;;   (test-entry (list 1 1 1)
;;               (get-probability-of-c
;;                (make-history-summary hist0 hist1 hist2))))

;; (defun could-he-be-a-foolp (hist0 hist1 hist2)
;;   (test-entry (list 1 1 1)
;;               (mapcar #'(lambda (elt)
;;                           (cond ((null elt) 1)
;;                                 ((= elt 1) 1)
;;                                 (t 0)))
;;                       (get-probability-of-c (make-history-summary hist0 hist1 hist2)))))
