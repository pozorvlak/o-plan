;;;; File: kcl-extensions.lsp
;;; Contains: An attempt to make KCL more up to date
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: Sun Nov 14 1993
;;; Updated: Wed May 19 04:39:13 1999 by Jeff Dalton
;;; Copyright: (c) 1993, AIAI, University of Edinburgh

(in-package :lisp)

(export '(destructuring-bind with-standard-io-syntax))

;;; A poor man's destructuring-bind.  There's destructuring only at
;;; the top-level, i.e., like a defun rather than a defmacro.

(defmacro destructuring-bind (lambda-list expr &body body)
  `(apply #'(lambda ,lambda-list . ,body)
          ,expr))

;;; With-standard-io-syntax

(defvar *initial-readtable* (copy-readtable nil))
(defvar *initial-print-pprint-dispatch* (xp:copy-pprint-dispatch nil))

; (defvar *initial-readtable* *readtable*)
; (defvar *initial-print-pprint-dispatch* *print-pprint-dispatch*)

(eval-when (eval compile load)
  (defparameter *standard-io-syntax-forms*
    `((*package*                    (find-package "USER"))
      (*print-array*                t)
      (*print-base*                 10)
      (*print-case*                 :upcase)
      (*print-circle*               nil)
      (*print-escape*               t)
      (*print-gensym*               t)
      (*print-length*               nil)
      (*print-level*                nil)
      (*print-lines*                nil)
      (*print-miser-width*          nil)
      (*print-pprint-dispatch*      *initial-print-pprint-dispatch*)
      (*print-pretty*               nil)
      (*print-radix*                nil)
      (*print-readably*             t)
      (*print-right-margin*         nil)
      (*read-base*                  10)
      (*read-default-float-format*  'single-float)
      (*read-eval*                  t)
      (*read-suppress*              nil)
      (*readtable*                  *initial-readtable*))))

(eval-when (eval compile load)
  (proclaim `(special ,@(mapcar #'car *standard-io-syntax-forms*))))

(defmacro with-standard-io-syntax (&body forms)
  `(let ,*standard-io-syntax-forms*
     ,@forms))

(defmacro assign-standard-io-syntax ()
  `(setq ,@(apply #'append *standard-io-syntax-forms*)))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
