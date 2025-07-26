(defpackage #:paip-02
  (:use #:cl)
  (:local-nicknames (#:c #:paip-common)))
(in-package #:paip-02)
;; Chapter 2 CODE

;; * * * * * * * * * * * *
;; 2.2 The Straightforward Solution
;; Rules:
;; Sentence => Noun-Phrase + Verb-Phrase
;; Noun-Phrase => Article + Noun
;; Verb-Phrase => Verb + Noun-Phrase
;; Article => the, a, ...
;; Noun => man, ball, woman, table...
;; Verb => hit, took, saw, liked...

;; The straightforward solution is complex.
;; Why is it complex?
;; 1. The code is more complex than the rules as defined in natural language.
;; 2. Adding new rules requires an understanding of Common Lisp semantics.
;; You need conditionals, you need to understand DEFUN, parameter lists, APPEND, etc.
;; * * * * * * * * * * * *

;; (defun sentence ()    (append (noun-phrase) (verb-phrase)))
;; (defun noun-phrase () (append (Article) (Noun)))
;; (defun verb-phrase () (append (Verb) (noun-phrase)))
;; (defun Article ()     (one-of '(the a)))
;; (defun Noun ()        (one-of '(man ball woman table)))
;; (defun Verb ()        (one-of '(hit took saw liked)))


(defun one-of (set)
  "Pick one element of set, and make a list of it."
  (list (random-elt set)))

;; (defun random-elt (choices)
;;   "choose an element from a list at random."
;;   (elt choices (random (length choices))))

(defun Adj* ()
  (if (= (random 2) 0)
      nil
      (append (Adj) (Adj*))))

(defun PP* ()
  (if (random-elt '(t nil))
      (append (PP) (PP*))
      nil))

;; * * * * * * * * * * * *
;; What if you don't want to use conditionals, but rely on the data-type itself?
;; With the complex code, can't.
;; (defun Adj* ()
;;   "Warning - incorrect definition of Adjectives."
;;   (one-of '(nil (append (Adj) (Adj*))))
;;   )
                                        ; returns '(nil (append (Adj) (Adj*)))
;; (defun Adj* ()
;;   "Warning - incorrect definition of Adjectives."
;;   (one-of (list nil (append (Adj) (Adj*))))
;;   )
                                        ; infinite loop

;; Code complexity will continue to increase as more grammar rules are added. Lisp knowledge necessary for creating rules. Original code will also need to be modified to add more rules.
;; * * * * * * * * * * * *

;; NOUN-PHRASE modified to account for optional adj and pp
(defun noun-phrase () (append (Article) (Adj*) (Noun) (PP*)))
(defun PP () (append (Prep) (noun-phrase)))
(defun Adj () (one-of '(big little blue green adiabatic)))
(defun Prep () (one-of '(to in by with on)))


;; * * * * * * * * * * * *
;; The simple, rules-based approach. Data-driven programming?
;; Notice the similarity of the code and rules above.
;; * * * * * * * * * * * *

(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  "A grammar for a trivial subset of English.")

(defvar *grammar* *simple-grammar*
  "The grammar used by generate. Initially, this is ,*simple-grammar*, but we can switch to other grammars.")

(defun random-elt (choices)
  "choose an element from a list at random."
  (elt choices (random (length choices))))

(defun rule-lhs (rule)
  "The left-hand side of a rule."
  (first rule))

(defun rule-rhs (rule)
  "The right-hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))

(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn the-list)))

;; (defun generate (phrase)
;;   "Generate a random sentence or phrase"
;;   (cond ((listp phrase)
;;          (mappend #'generate phrase))
;;         ((rewrites phrase)
;;          (generate (random-elt (rewrites phrase))))
;;         (t (list phrase))))

;; (defun generate (phrase)
;;   "Generate a random sentence or phrase. Alternate version that avoids calling (rewrites phrase) twice."
;;   (if (listp phrase)
;;       (mappend #'generate phrase)
;;       (let ((choices (rewrites phrase)))
;;         (if (null choices)
;;             (list phrase)
;;             (generate (random-elt choices))))))

;; (defun generate (phrase)
;;   "Generate a random sentence or phrase. ANSWER TO;; 2.1"
;;   (let ((choices (rewrites phrase)))
;;     (cond ((listp phrase)
;;            (mappend #'generate phrase))
;;           ((null choices) (list phrase))
;;           (t (generate (random-elt choices))))))

(defun generate (phrase)
  "Generate a random sentence or phrase. ANSWER TO;; 2.2"
  (let* ((choices (rewrites phrase))
         (terminal (null choices)))
    (cond ((listp phrase)
           (mappend #'generate phrase))
          (terminal (list phrase))
          (t (generate (random-elt choices))))))

(defparameter *bigger-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP* -> () (PP PP*))
    (Adj* -> () (Adj Adj*))
    (PP -> (Prep noun-phrase))
    (Prep -> to in by with on)
    (Adj -> big little blue green adiabatic)
    (Article -> the a)
    (Name -> Pat Kim Lee Terry Robin)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)
    (Pronoun -> he she it these those that)))

(setf *grammar* *bigger-grammar*)
;; (setf *grammar* *simple-grammar*)
;; (setf *grammar* *japanese-grammar*)

(defun generate-tree (phrase)
  "Generate a random sentence or phrase,
  with a complete parse tree."
  (cond ((listp phrase)
         (mapcar #'generate-tree phrase))
        ((rewrites phrase)
         (cons phrase
               (generate-tree (random-elt (rewrites phrase)))))
        (t (list phrase))))

;; !WARNING!WARNING!WARNING!WARNING!WARNING!WARNING!
;; When used with recursive grammar like PP*, Adj*, etc.
;; generate-all will produce an infinite loop.
;; Only use on simple grammar.
;; !WARNING!WARNING!WARNING!WARNING!WARNING!WARNING!
(defun generate-all (phrase)
  "Generate a list of all possible expansions of this phrase."
  (cond ((null phrase) (list nil))
        ((listp phrase)
         (combine-all (generate-all (first phrase))
                      (generate-all (rest phrase))))
        ((rewrites phrase)
         (mappend #'generate-all (rewrites phrase)))
        (t (list (list phrase)))))

(defun combine-all (xlist ylist)
  "Return a list of lists formed by appending a y to an x.
  E.g., (combine-all '((a) (b)) '((1) (2)))
  -> ((A 1) (B 1) (A 2) (B 2))."
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (append x y)) xlist))
           ylist))



;; EXERCISES
;; * 2.1 [m] Write a version of generate that uses cond but avoids calling rewrites twice.
;; (defun generate (phrase)
;;   "Generate a random sentence or phrase. ANSWER TO EXERCISE 2.1"
;;   (let ((choices (rewrites phrase)))
;;     (cond ((listp phrase)
;;            (mappend #'generate phrase))
;;           ((null choices) (list phrase))
;;           (t (generate (random-elt choices))))))

;; * 2.2 [m] Write a version of generate that explicitly differentiates between terminal symbols (those with no rewrite rules) and nonterminal symbols.
;; (defun generate (phrase)
;;   "Generate a random sentence or phrase. ANSWER TO EXERCISE 2.2"
;;   (let* ((choices (rewrites phrase))
;;          (terminal (null choices)))
;;     (cond ((listp phrase)
;;            (mappend #'generate phrase))
;;           (terminal (list phrase))
;;           (t (generate (random-elt choices))))))

;; * TODO 2.3 [h] Write a trivial grammar for some other language. This can be a natural language other than English, or perhaps a subset of a computer language.
(defparameter *japanese-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Noun PP* Article Adj* Noun) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP* -> () (PP PP*))
    (Adj* -> () (Adj Adj*))
    (PP -> (Prep noun-phrase))
    (Prep -> に と の上に の中に で)
    (Adj -> 大きい 小さな 青い 緑の)
    (Article -> が を)
    (Name -> 恵梛 美風 マイカ 隆恵)
    (Noun -> 男性 球 女性 テーブル)
    (Verb -> 見た 好きだった 蹴った 取った)
    (Pronoun -> 彼 彼女 それ これら それら あれ))
  "Unfinished grammar for Japanese. ANSWER TO 2.3")
;; * TODO 2.4 [m] One way of describing combine-all is that it calculates the cross-product of the function append on the argument lists. Write the higher-order function cross-product, and define combine-all in terms of it.
