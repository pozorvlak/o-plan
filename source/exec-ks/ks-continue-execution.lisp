;;;; File: KS-CONTINUE-EXECUTION.lisp
;;; Contains: KS that continues execution after plan fixes.
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: Wed 22 February 1995
;;; Updated: Tue Mar 23 01:50:07 1999 by Jeff Dalton
;;; Copyright: (c) 1995, AIAI, University of Edinburgh

;;; :CONTINUE-EXECUTION is put on the agenda after some :FIX entries
;;; have been added to fix problems caused by an execution failure
;;; at the node-end specified by failure-etag.  :CONTINUE-EXECUTION
;;; has a low priority, so it shouldn't execute until the Planner's
;;; fixed the plan.  At that point, we can continue execution more
;;; or less where we left off.

;;; /\/: It might be better to rely on triggers instead of priorities.

;;; In principle, we might continue execution while fixing the plan,
;;; because there may be actions that are not affected and that could
;;; at least be sent to the Exec (even if it's not time to execute
;;; them), but it would be tricky.  Maybe it's not even possible,
;;; because we don't know what links might be added.

(in-package :oplan-knowledge-source)

(defun KS-CONTINUE-EXECUTION (ag)
  (destructuring-bind (failure-etag) (cdr (ag-body ag))
    (db-call 'rebuild-departure-lists)
    (case failure-etag
      (:AFTER-ADDED-ACTION
       ;; This time we don't have any particular node-end to work with.
       (mapc #'tell-exec-to-execute
	     (db-call 'next-departing-group)))
      (t
       ;; This failure-etag is an etag, not a special case.
       (tell-exec-to-execute-next-fringe failure-etag)))))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
