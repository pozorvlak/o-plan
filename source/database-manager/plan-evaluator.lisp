;;;; File: plan-evaluator.lisp
;;; Contains: Simple plan evaluator
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: September 1995
;;; Updated: Thu Apr  8 01:39:25 1999 by Jeff Dalton
;;; Copyright: (c) 1995, AIAI, University of Edinburgh

(in-package :oplan)

;;; The plan-evaluator is called to gather information about a complete
;;; plan.  (So it shouldn't be called until the Planner has finished.)
;;;
;;; To request a plan evaluation from "outside the Planner", send
;;; (:EVAL-PLAN) to O-Plan.  The result will be sent back as
;;; (:EVALUATION result).
;;;
;;; The information gathered by the plan evaluator is meant to reflect
;;; plan quality, in some sense, and to make it easier for the user to
;;; tell whether there are interesting differences between plans.  The
;;; generic version supplied here may not do this well for all domains.
;;; However, domain-specific versions can be defined.  E.e. use
;;; defun-for-domain to redefine eval-plan.
;;;

(defvar *plan-evaluators* nil)		;list of function names"

(defvar *evaluation-results* nil)	;an a-list

(defun eval-plan ()
  (let ((*evaluation-results* '()))
    (mapc #'(lambda (name)
	      (funcall (get name 'plan-evaluator)))
	  *plan-evaluators*)
    (reverse *evaluation-results*)))

(defun record-eval (name value)
  (push (cons name value) *evaluation-results*))

(defun add-plan-evaluator (name)
  (unless (member name *plan-evaluators*)
    (setq *plan-evaluators* (nconc *plan-evaluators* (list name)))))

(defmacro define-plan-evaluator (name parameters &body body)
  (check-type name symbol)
  (check-type parameters null)
  (let ((fname (concat-name name "-PLAN-EVALUATOR")))
    `(progn
       (add-plan-evaluator ',name)
       (setf (get ',name 'plan-evaluator)
	 ',fname)
       (defun ,fname ,parameters
         ,@body))))

;;; Plan evaluators

;;; Note that it's up to each evaluator to call record-eval.

(define-plan-evaluator number-of-nodes ()
  (record-eval :number-of-nodes (/ (node-end-count) 2)))

(define-plan-evaluator plan-length ()
  (let ((ne-distance-table
	 (find-longest-path-lengths
	   (get-node-end (etag 'node-1 :begin))
	   #'ne-post-ends)))
    (record-eval
      :plan-length
      (gethash (get-node-end (etag 'node-2 :end))
	       ne-distance-table))))

(define-plan-evaluator time-required ()
  (let ((est (tpoint-min (ne-time-point (get-node-end (etag 'node-1 :begin)))))
	(eft (tpoint-min (ne-time-point (get-node-end (etag 'node-2 :end))))))
    (let ((duration (- eft est)))
      (record-eval
        :duration
	`(,duration = ,(seconds->description duration))))))

(define-plan-evaluator psv-diversity ()
  (let ((object-types '())
	(values '()))
    (map-over-psvs
      #'(lambda (tag body)
	  (declare (ignore tag))
	  (pushnew (oplan-psv::psv-body-type body) object-types)
	  (pushnew (oplan-psv::psv-body-value body) values)))
    (record-eval :psv-object-types (canonical-description-order object-types))
    (record-eval :psv-values (canonical-description-order values))
    (record-eval :n-psv-object-types (length object-types))
    (record-eval :n-psv-values (length values))))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
