;;;; File: KS-PLANNER-FINISHED.lisp
;;; Contains: A KS to stuff a finished message out the TA.
;;; Author: Richard Kirby (rbk)
;;; Created: Thu Dec 19 17:47:20 1991
;;; Updated: Sun Sep 28 19:34:40 1997 by Jeff Dalton
;;; Copyright: (c) 1992, AIAI, University of Edinburgh

(in-package :oplan-knowledge-source)

(defun KS-PLANNER-FINISHED (ag)
  (declare (ignore ag))
  (when (db-request :CONTEXT-WAS-POISONED)
    (cerror "Continue anyway"
	    "Finished in poisoned context.~%Poisoned because ~A"
	    (db-request :CONTEXT-WAS-POISONED)))
  (db-request :PRINT-DATABASE)
  (ipc-send :AM :FINISHED-PLANNING :FINISHED)
  (ipc-send-out :FINISHED))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
