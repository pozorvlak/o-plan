;;;; File: random-choices.lisp
;;; Contains: Redefinitions that make certain choices random
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: September 1995
;;; Updated: Wed Dec 18 21:08:58 1996 by Jeff Dalton
;;; Copyright: (c) 1995, AIAI, University of Edinburgh

(in-package :oplan)


;;; Which schema?

(defun sort-schema-alternatives (from)
  (let ((preferred-order (shuffle-list from)))
    (if (eq *schema-selection-mode* :ask)
	(ask-user-to-pick-best-schema preferred-order)
      preferred-order)))


;;; Which value for a PSV?

(advice+ 'auto-bind 'random-choice
  #'(lambda (previous)
      #'(lambda (ag var-name bindings)
	  (funcall previous ag var-name (shuffle-list bindings)))))


;;; Which branch of an or-tree?

#+:undef
(defun pick-from-next-level (res-list)
  (setq res-list (shuffle-list res-list))
  (values (car res-list) (cdr res-list)))


;;; Some Useful functions

;;; (nshuffle-vector v) destructively permutes the vector v in
;;; such a way that all permutations are equally probable.

(defun nshuffle-vector (v)
  (check-type v simple-vector)
  (do ((i (1- (length v)) (1- i)))
      ((< i 0) v)
    (rotatef (svref v i)
             (svref v (random (1+ i))))))

(defun shuffle-list (lis)
  (coerce (nshuffle-vector (coerce lis 'simple-vector))
	  'list))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
