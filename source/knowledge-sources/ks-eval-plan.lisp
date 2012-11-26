;;;; File: KS-EVAL-PLAN.lisp
;;; Contains: The KS for evaluating a plan
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: September 1995
;;; Updated: Sun Nov 24 16:24:19 1996 by Jeff Dalton
;;; Copyright: (c) 1995, AIAI, University of Edinburgh

(in-package :oplan-knowledge-source)

;;; This KS handles:
;;;
;;; (:EVAL-PLAN)
;;;
;;;    Ask the DM to run the plan evaluator.  The resut is sent to
;;;    the TA as :EVALUATION <result>.
;;;

(defun KS-EVAL-PLAN (event)
  (let* ((message (ag-body event))
	 (subop (if (rest message) (second message) nil)))
    (case subop

      ((NIL)
       (ipc-send-out :EVALUATION (db-request :EVAL-PLAN)))

      (t
       (error "Can't handle ~S." message)))))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
