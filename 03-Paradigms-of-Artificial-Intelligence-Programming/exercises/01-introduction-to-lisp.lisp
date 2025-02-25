;; * CHAPTER 1 CODE

;; 1.1 - 1.5 NOTHING
;; 1.6 USING FUNCTIONS
(defparameter *titles*
  '(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General)
  "A list of titles that can appear at the start of a name.")

(defun first-name (name)
  "Select the first name from a name represented as a list."
  (if (member (first name) *titles*)
      (first-name (rest name))
      (first name)))

(defun last-name (name)
  "Select the last name from a name represented as a list."
  (first (last name)))

(setf names '((John Q Public) (Malcolm X)
              (Admiral Grace Murray Hopper) (Spot)
              (Aristotle) (A A Milne) (Z Z Top)
              (Sir Larry Olivier) (Miss Scarlet)))

;; 1.7 HIGHER-ORDER FUNCTIONS
(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn the-list)))

(defun numbers-and-negations (input)
  "Given a list, return only the numbers and their negations."
  (mappend #'number-and-negation input))

(defun number-and-negation (x)
  "If x is a number, return a list of x and -x."
  (if (numberp x)
      (list x (- x))
      nil))

;; 1.8 - 1.10 NOTHING

;; EXERCISES
;; * 1.1 (m) Define a version of last-name that handles "Rex Morgan MD," "Morton Downey, Jr.," and whatever other cases you can think of.
;; PAIP answer
(defun last-name (name)
  "Select the last name from a name represented in a list using recursion"
  (_last-name (reverse name)))

(defun _last-name (name)
  (cond ((null name) nil)
        ((member (first name) *titles*) (_last-name (rest name)))
        (t (first name))))

;; * 1.2 (m) Write a function to exponentiate, or raise a number to an integral power.
;; * For example: (power 3 2) => 9

(defun power (x n)
  "Power raises x to the nth power.  N must be an integer >= 0.
   This executes in log n time, because of the check for even n."
  (cond ((= n 0) 1)
        ((evenp n) (expt (power x (/ n 2)) 2))
        (t (* x (power x (- n 1))))))

;; my answer
(defun power (number exponent)
  (if (= exponent 0)
      1
      (* number (power number (- exponent 1)))))

;; * 1.3 (m) Write a function that counts the number of atoms in an expression. For example: (count-atoms '(a (b) c)) = 3.
;; * Notice that there is something of an ambiguity in this: should (a nil c) count as three atoms, or as two, because it
;; * is equivalent to (a () c)?

(defun count-atoms (x)
  "Count all non-nil atoms in a nested list."
  (cond
    ((null x) 0)
    ((atom x) 1)
    (t (+ (count-atoms (first x))
          (count-atoms (rest x))))))

(defun count-all-atoms (x)
  "Count all atoms in a nested list--including nil."
  (cond
    ((atom x) 1)
    (t (+ (count-all-atoms (first x))
          (count-all-atoms (rest x))))))

;; * 1.4 (m) Write a function that counts the number of times an expression occurs anywhere within another expression.
;; * Example: (count-anywhere 'a '(a ((a) b) a)) => 3.

(defun count-anywhere (symbol x)
  (cond ((null x) 0)
        ((and (atom x) (not (equal symbol x))) 0)
        ((and (atom x) (equal symbol x)) 1)
        (t (+ (count-anywhere symbol (first x))
              (count-anywhere symbol (rest x))))))

;; * 1.5 (m) Write a function to compute the dot product of two sequences of numbers, represented as lists.
;; * The dot product is computed by multiplying corresponding elements and then adding up the resulting products. Example:
;; * (dot-product '(10 20) '(3 4)) = 10 x 3 + 20 x 4 = 110

(defun dot-product (left right)
  (reduce #'+ (mapcar #'(lambda (x y) (* x y)) left right)))

(dot-product '(10 20) '(3 4))
