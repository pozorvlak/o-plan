;;;; File: ks-get.lisp
;;; Contains: KS the handles external requests for plan information
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: October 1996
;;; Updated: Fri Aug 27 01:18:08 1999 by Jeff Dalton
;;; Copyright: (c) 1996, AIAI, University of Edinburgh

(in-package :oplan-knowledge-source)

;;; KS-GET can get as (ag-body event):
;;;
;;; :GET :PLAN-VIEW viewer-arg...
;;;
;;;    The task assigner wants to see a plan view.
;;;
;;; :GET :WORLD-VIEW node-id viewer-arg...
;;;
;;;    The task assigner wants to see a world view.  The node-id is
;;;    actually the node-number(s), separated by "-"s, as a string.
;;;
;;; :GET :ACTION-LEVELS
;;;
;;;    Send the TA an a-list mapping action-names to level numbers.
;;;

(defun KS-GET (event)
  (let ((*errors-are-harmless* t))
    (destructuring-bind (ks request &rest args) (ag-body event)
      (assert (eq ks :GET))
      (apply (case request
	       (:PLAN-VIEW     #'ks-get-plan-view)
	       (:WORLD-VIEW    #'ks-get-world-view)
	       (:ACTION-LEVELS #'ks-get-action-levels)
	       (t (or (get request :KS-GET-PLUGIN)
		      (error "Unknown ks-get request ~S." request))))
	     args))))


;;;; Plan view

(defun ks-get-plan-view (&rest viewer-args)
  (dev-debug :detail "Asking DM for current plan.")
  (let ((network (pw-get-plan-for-viewing)))
    (if (null viewer-args)
	(ipc-send-out :PLAN-VIEW network)
      (apply #'pw-handle-view :plan network viewer-args))))


;;;; World view

(defun ks-get-world-view (node-id &rest viewer-args)
  (let ((pv-pairs (pw-get-world-for-viewing node-id)))
    (if (null viewer-args)
	(ipc-send-out :WORLD-VIEW node-id pv-pairs)
      (apply #'pw-handle-view :world node-id pv-pairs viewer-args))))


;;;; Action levels

(defun ks-get-action-levels ()
  (ipc-send-out :ACTION-LEVELS (db-request :GET-ACTION-LEVELS)))


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
