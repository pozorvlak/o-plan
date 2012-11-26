;;;; File: other-constraint-cm.lisp
;;; Contains: The constraint manager for constraints w/o managers
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: November 1996
;;; Updated: Mon Dec  2 20:15:48 1996 by Jeff Dalton
;;; Copyright: (c) 1996, AIAI, University of Edinburgh


(in-package :oplan)


;;; The CM

(define-constraint-manager other-cm (simple-cm)
  ((%constraints :accessor %other-constraints
		 :initform '())
   (%filters :accessor %other-filters
	     :initform '())))

(define-context-accessor other-constraints %other-constraints)
(define-context-accessor other-filters %other-filters)

(register-constraint-manager other-cm
  :constraint-types '(other))


;;; Methods

(defmethod cm-init-constraints ((self other-cm))
  ;; N.B. clear everything, not only for the current context.
  (setf (%other-constraints self) '()
	(%other-filters self) '()))

(defmethod cm-add-constraint ((self other-cm) constraint)
  (push constraint (other-constraints self))
  t)

(defmethod cm-eval-filter ((self other-cm) constraint bindings)
  (push (list constraint bindings) (other-filters self))
  t)

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
