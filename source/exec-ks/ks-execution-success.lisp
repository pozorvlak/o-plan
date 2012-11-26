;;;; File: KS-EXECUTION-SUCCESS.lisp
;;; Contains: KS that deal with successful execution of an end of an action
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: Sat 18 February 1995
;;; Updated: Sat Sep  4 21:39:35 1999 by Jeff Dalton
;;; Copyright: (c) 1995, AIAI, University of Edinburgh

(in-package :oplan-knowledge-source)

(defun KS-EXECUTION-SUCCESS (event)
  (destructuring-bind (when etag pattern) (cdr (ag-body event))
    (declare (ignore when pattern))

    ;(dev-debug :trace "Successfully executed ~A ~W" etag pattern)

    ;; Mark the node-end as having been executed.
    (db-call 'set-etag-exec-status etag :finished)

    ;; Continue execution.
    (tell-exec-to-execute-next-fringe etag)))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
