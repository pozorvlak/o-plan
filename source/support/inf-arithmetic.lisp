;;;; File: inf-arithmetic.lisp
;;; Contains: functions for arithmetic with infinity
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: June 1994
;;; Updated: Wed Jan  6 04:24:19 1999 by Jeff Dalton
;;; Copyright: (c) 1994, AIAI, University of Edinburgh

(in-package :oplan-util)

(export '(*infinity*))
(export '(%infp infp infinitep &numberp))
(export '(&+ &- &incf &decf))
(export '(&= &< &<= &> &>=))
(export '(&min &max))

;;; Arithmetic with :inf

(defconstant *infinity* :inf)		;for old code /\/

(defmacro %infp (x)			;inlining doesn't always happen /\/
  `(eq ,x :inf))

(defun-inline infp (x)
  (eq x :inf))

(defun infinitep (x)			;for old code /\/
  (and (symbolp x)
       (or (%infp x) (error "Bogus infinite: ~S" x))))

(defun-inline &numberp (x)
  (or (numberp x)
      (%infp x)))

(defun-inline &+ (i j)
  (if (or (%infp i) (%infp j))
      :inf
    (+ i j)))

(defun-inline &- (i j)
  (cond ((%infp i) :inf)
	((%infp j) (error "Can't subtract infinity from ~S." i))
	(t (- i j))))

(define-modify-macro &incf (&optional (delta 1)) &+)
(define-modify-macro &decf (&optional (delta 1)) &-)

(defun-inline &= (i j)
  (cond ((%infp i) (%infp j))
	((%infp j) nil)
	(t (= i j))))

(defun-inline &< (i j)
  (cond ((%infp i) nil)
	((%infp j) (not (%infp i)))
	(t (< i j))))

(defun-inline &<= (i j)
  (cond ((%infp i) (%infp j))
	((%infp j) t)
	(t (<= i j))))

(defun-inline &> (i j)
  (&< j i))

(defun-inline &>= (i j)
  (&<= j i))

(defun-inline &min (i j)
  (if (&< i j) i j))

(defun-inline &max (i j)
  (if (&> i j) i j))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
