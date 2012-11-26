;;;; File: world-state-util.lisp
;;; Contains: 
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: June 1999, from world-services.lisp
;;; Updated: Wed Sep  8 01:11:18 1999 by Jeff Dalton
;;; Copyright: (c) 1993, 1994, 1999, AIAI, University of Edinburgh

(in-package :oplan-world-state-util)


;;;; World state

;;; We can maintain the current pattern = value settings from action
;;; effects, but only if we have a model that tells us what effects
;;; an action has.  Normally we don't have such a model.  /\/

;;; Note that the world state does not include "always" facts.
;;; To look up a pattern value in a way that takes "always" into
;;; account, use get-world-value

(defvar *world-state* (make-hash-table :test #'equal))

(defun clear-world-state ()
  (clrhash *world-state*))

(defun set-world-pattern-value (pattern value)
  (setf (gethash pattern *world-state*) value))

(defun get-world-pattern-value (pattern) ; -> value, there-p
  ;; The pattern may contain ??.
  (multiple-value-bind (value there-p) (gethash pattern *world-state*)
    (if there-p
	(values value t)
      (try-world-pattern-wildcard-match pattern))))

(defun try-world-pattern-wildcard-match (pattern) ; -> value, there-p
  (let ((possibles '()))
    (maphash #'(lambda (p v)
		 (declare (ignore v))
		 (when (match p pattern)
		   (push p possibles)))
	     *world-state*)
    (when possibles
      (when (length>1 possibles)
        (break "Multiple matches for ~S: ~S." pattern possibles))
      (gethash (first possibles) *world-state*))))

(defun world-pattern-has-value-p (pattern)
  (multiple-value-bind (value there-p) (get-world-pattern-value pattern)
    (values there-p value)))

(defun world-fact-p (pattern desired-value)
  ;; The desired-value may contain ??.
  (multiple-value-bind (value there-p) (get-world-pattern-value pattern)
    (if there-p
	(match desired-value value)	;was equal /\/
      nil)))

(defun get-world-state ()
  (canonical-description-order
    (hash-table-alist *world-state*)))


;;; Get-world-value looks first in the "always" table, then in the
;;; world state.

(defun get-world-value (pattern &optional (default nil))
  (multiple-value-bind (value there-p) (get-world-always-value pattern)
    (if there-p
	value
      (multiple-value-bind (value there-p) (get-world-pattern-value pattern)
	(if there-p
	    value
	  default)))))


;;;; Always facts

(defvar *world-always-table* (make-hash-table :test #'equal))

(defun clear-world-always ()
  (clrhash *world-always-table*))

(defun set-world-always-value (pattern value)
  (setf (gethash pattern *world-always-table*) value))

(defun get-world-always-value (pattern)
  (gethash pattern *world-always-table*))

(defun has-world-always-value-p (pattern)
  (multiple-value-bind (value there-p) (get-world-always-value pattern)
    (values there-p value)))

(defun world-always-p (pattern desired-value)
  ;; The desired-value may contain ??.
  (multiple-value-bind (value there-p) (get-world-always-value pattern)
    (if there-p
	(match desired-value value)	;was equal /\/
      nil)))

(defun get-world-always ()
  (canonical-description-order
    (hash-table-alist *world-always-table*)))


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
