;;;; File: condition-util.lsp
;;; Contains: Some useful condition types, etc.
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: December 1996
;;; Updated: Mon Dec 16 20:23:51 1996 by Jeff Dalton
;;; Copyright: (c) 1996, AIAI, University of Edinburgh

(in-package :oplan-util)

(export '(harmless-error simple-harmless-error *errors-are-harmless*
	  invalid-command))


;;;; Harmless errors

;;; Harmless errors have sufficiently localized consequences that it
;;; shouldn't be necessary to reinitialize the planner if one occurs.
;;; Perhaps "harmless" isn't quite the right term... /\/

(defvar *errors-are-harmless* nil)

(define-condition harmless-error (simple-error) ())
(define-condition simple-harmless-error (harmless-error) ())

(defun harmless-error (datum &rest args)
  (etypecase datum
    (symbol (apply #'signal-error datum args))
    (string (signal-error 'simple-harmless-error
			  :format-string datum
			  :format-arguments args))))


;;;; Invalid commands

;;; This is for syntax-error-like problems with commands from the TA,
;;; for instance if we get (:domain "house-5") and there's no such
;;; domain.

(define-condition invalid-command (harmless-error) ())

(defun invalid-command (message &rest format-args)
  (signal-error 'invalid-command
		:format-string message
		:format-arguments format-args))


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
