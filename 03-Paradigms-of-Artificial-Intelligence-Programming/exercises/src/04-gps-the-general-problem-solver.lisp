(defpackage #:paip-04
  (:use #:cl)
  (:local-nicknames (#:c #:paip-common)))
(in-package #:paip-04)

(defvar *state* nil "The current state: a list of conditions.")
(defvar *ops* nil "A list of available operators.")

(defstruct op
  "An operation"
  (action nil)
  (preconds nil)
  (add-list nil)
  (del-list nil))

(defun GPS (state goals &optional (*ops* *ops*))
  "General Problem Solver: from state, achieve goals using *ops*."
  (c:find-all-if #'action-p
                 (achieve-all (cons '(start) state) goals nil)))
(defun action-p (x)
  "Is x something that is (start) or (executing ...)?"
  (or (equal x '(start)) (executing-p x)))

(defun achieve-all (state goals goal-stack)
  "Achieve each goal, trying several orderings"
  (some #'(lambda (goals) (achieve-each state goals goal-stack))
        (orderings goals)))

(defun achieve-each (state goals goal-stack)
  "Achieve each goal, and make sure they still hold at the end."
  (let ((current-state state))
    (if (and (every #'(lambda (g)
                        (setf current-state
                              (achieve current-state g goal-stack)))
                    goals)
             (subsetp goals current-state :test #'equal))
        current-state)))

(defun orderings (l)
  (if (> (length l) 1)
      (list l (reverse l))
      (list l)))

(defun permutations (s)
  (if (null s)
      (list nil)
      (c:mappend #'(lambda (x)
                     (mapcar #'(lambda (p)
                                 (cons x p))
                             (permutations
                              (remove-if #'(lambda (m) (equal x m)) s))))
                 s)))

(defun achieve (state goal goal-stack)
  "A goal is achieved if it already holds,
or if there is an appropriate op for it that is applicable."
  (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
  (cond ((c:member-equal goal state) state)
        ((c:member-equal goal goal-stack) nil)
        (t (some #'(lambda (op) (apply-op state goal op goal-stack))
                 (appropriate-ops goal state)))))

(defun appropriate-ops (goal state)
  "Return a list of appropriate operators,
sorted by the number of unfulfilled preconditions."
  (sort (copy-list (c:find-all goal *ops* :test #'appropriate-p)) #'<
        :key #'(lambda (op)
                 (count-if #'(lambda (precond)
                               (not (c:member-equal precond state)))
                           (op-preconds op)))))

(defun appropriate-p (goal op)
  "An op is approprate to a goal if it is in its add list."
  (c:member-equal goal (op-add-list op)))

(defun apply-op (state goal op goal-stack)
  "Print a message and update *state* if op is applicable."
  ;; (dbg-indent :gps (length goal-stack) "Consider: ~a" (op-action op))
  (format t "~&Consider: ~a" (op-action op))
  (let ((state2 (achieve-all state (op-preconds op)
                             (cons goal goal-stack))))
    (unless (null state2)
      (dbg-indent :gps (length goal-stack) "Action: ~a" (op-action op))
      (append (remove-if #'(lambda (x)
                             (c:member-equal x (op-del-list op)))
                         state2)
              (op-add-list op)))))

(defparameter *school-ops*
  (list
   (make-op :action 'drive-son-to-school
            :preconds '(son-at-home car-works)
            :add-list '(son-at-school)
            :del-list '(son-at-home))
   (make-op :action 'shop-installs-battery
            :preconds '(car-needs-battery shop-knows-problem shop-has-money)
            :add-list '(car-works))
   (make-op :action 'tell-shop-problem
            :preconds '(in-communication-with-shop)
            :add-list '(shop-knows-problem))
   (make-op :action 'telephone-shop
            :preconds '(know-phone-number)
            :add-list '(in-communication-with-shop))
   (make-op :action 'look-up-number
            :preconds '(have-phone-book)
            :add-list '(know-phone-number))
   (make-op :action 'give-shop-money
            :preconds '(have-money)
            :add-list '(shop-has-money)
            :del-list '(have-money))
   (make-op :action 'ask-phone-number
            :preconds '(in-communication-with-shop)
            :add-list '(know-phone-number))))
(defvar *dbg-ids* nil "Identifiers used by dbg")

(defun dbg (id format-string &rest args)
  "Print debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))

(defun paip-debug (&rest ids)
  "Start dbg output on the given ids."
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun undebug (&rest ids)
  "Stop dbg on the ids. With no ids, stop dbg altogether."
  (setf *dbg-ids* (if (null ids) nil
                      (set-difference *dbg-ids* ids))))
(defun dbg-indent (id indent format-string &rest args)
  "Print indented debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (dotimes (i indent) (princ " " *debug-io*))
    (apply #'format *debug-io* format-string args)))

(defun executing-p (x)
  "Is x of the form: (executing ...) ?"
  (starts-with x 'executing))

(defun starts-with (list x)
  "Is this a list whose first element is x?"
  (and (consp list) (eql (first list) x)))

(defun convert-op (op)
  "Make op conform to the (EXECUTING op) convention."
  (unless (some #'executing-p (op-add-list op))
    (push (list 'executing (op-action op)) (op-add-list op)))
  op)

(defun op (action &key preconds add-list del-list)
  "Make a new operator that obeys the (EXECUTING op) convention."
  (convert-op
   (make-op :action action :preconds preconds
            :add-list add-list :del-list del-list)))

(defun use (oplist)
  "Use oplist as the default list of operators."
  ;; Return something useful, but not too verbose:
  ;; the number of operators.
  (length (setf *ops* oplist)))

(defparameter *banana-ops*
  (list
   (op
    'climb-on-chair
    :preconds '(chair-at-middle-room at-middle-room on-floor)
    :add-list '(at-bananas on-chair)
    :del-list '(at-middle-room on-floor))
   (op
    'push-chair-from-door-to-middle-room
    :preconds '(chair-at-door at-door)
    :add-list '(chair-at-middle-room at-middle-room)
    :del-list '(chair-at-door at-door))
   (op
    'walk-from-door-to-middle-room
    :preconds '(at-door on-floor)
    :add-list '(at-middle-room)
    :del-list '(at-door))
   (op
    'grasp-bananas
    :preconds '(at-bananas empty-handed)
    :add-list '(has-bananas)
    :del-list '(empty-handed))
   (op
    'drop-ball
    :preconds '(has-ball)
    :add-list '(empty-handed)
    :del-list '(has-ball))
   (op
    'eat-bananas
    :preconds '(has-bananas)
    :add-list '(empty-handed not-hungry)
    :del-list '(has-bananas hungry))))

(defun make-maze-ops (pair)
  "Make maze ops in both directions"
  (list (make-maze-op (first pair) (second pair))
        (make-maze-op (second pair) (first pair))))
(defun make-maze-op (here there)
  "Make an operator to move between two places"
  (op
   `(move from ,here to ,there)
   :preconds `((at ,here))
   :add-list `((at ,there))
   :del-list `((at ,here))))
(defparameter *maze-ops*
  (c:mappend #'make-maze-ops
             '((1 2) (2 3) (3 4) (4 9) (9 14) (9 8) (8 7) (7 12) (12 13)
               (12 11) (11 6) (11 16) (16 17) (17 22) (21 22) (22 23)
               (23 18) (23 24) (24 19) (19 20) (20 15) (15 10) (10 5) (20 25))))
(defun find-path (start end)
  "Search a maze for a path from start to end"
  (let ((results (gps `((at ,start)) `((at ,end)))))
    (unless (null results)
      (cons start
            (mapcar #'destination (remove '(start) results :test #'equal))))))
(defun destination (action)
  (fifth (second action)))

(defun make-block-ops (blocks)
  (let ((ops nil))
    (dolist (a blocks)
      (dolist (b blocks)
        (unless (equal a b)
          (dolist (c blocks)
            (unless (or (equal c a) (equal c b))
              (push (move-op a b c) ops)))
          (push (move-op a 'table b) ops)
          (push (move-op a b 'table) ops))))
    ops))
(defun move-op (a b c)
  "Make an operator to move A from B to C."
  (op
   `(move ,a from ,b to ,c)
   :preconds `((space on ,a) (space on ,c) (,a on ,b))
   :add-list (move-ons a b c)
   :del-list (move-ons a c b)))
(defun move-ons (a b c)
  (if (eq b 'table)
      `((,a on c))
      `((,a on ,c) (space on ,b))))

;; * Exercise  4.3 (h)
;; GPS does not recognize the situation where a goal is accidentally solved as part of achieving another goal.
;; Consider the goal of eating dessert.
;; Assume that there are two operators available: eating ice cream (which requires having the ice cream) and eating cake (which requires having the cake).
;; Assume that we can buy a cake, and that the bakery has a deal where it gives out free ice cream to each customer who purchases and eats a cake.
;; (1) Design a list of operators to represent this situation.
;; (2) Give gps the goal of eating dessert.
;; Show that, with the right list of operators, `gps` will decide to eat ice cream, then decide to buy and eat the cake in order to get the free ice cream, and then go ahead and eat the ice cream, even though the goal of eating dessert has already been achieved by eating the cake.
;; (3) Fix gps so that it does not manifest this problem.
;; * * * * * * * * * *

;; * Exercise  4.4 (h) The Not Looking after You Don't Leap Problem
;; Write a program that keeps track of the remaining goals so that it does not get stuck considering only one possible operation when others will eventually lead to the goal.
;; Hint: have achieve take an extra argument indicating the goals that remain to be achieved after the current goal is achieved.
;; `achieve` should succeed only if it can achieve the current goal and also `achieve-all` the remaining goals.

;; * Exercise  4.5 (d)
;; Write a planning program that, like Warren's Warplan, keeps track of the list of goals that remain to be done as well as the list of goals that have been achieved and should not be undone.
;; The program should never undo a goal that has been achieved, but it should allow for the possibility of reordering steps that have already been taken.
;; In this way, the program will solve the Sussman anomaly and similar problems.

;; * Exercise  4.6 (d) The Lack of Descriptive Power Problem
;; Read [chapters 5](chapter5.md) and [6](chapter6.md) to learn about pattern matching.
;; Write a version of GPS that uses the pattern matching tools, and thus allows variables in the operators.
;; Apply it to the maze and blocks world domains.
;; Your program will be more efficient if, like Chapman's Tweak program, you allow for the possibility of variables that remain unbound as long as possible.

;; * Exercise  4.7 (d) Speculate on the design of a planner that can address the *Perfect Information* and *Interacting Goals* problems.
