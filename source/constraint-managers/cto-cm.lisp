;;;; File: cto-cm.lisp
;;; Contains: The "consume_the_only" constraint-manager
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: January 1997
;;; Updated: Sun Jan 26 04:13:19 1997 by Jeff Dalton
;;; Copyright: (c) 1997, AIAI, University of Edinburgh

(in-package :oplan)

;;;; The CM

(define-constraint-manager cto-cm (variable-free-cm simple-cm))

(register-constraint-manager cto-cm
  :constraint-types '(cto))


;;;; Constraint information

(defvar *cto-table* (make-hash-table :test #'equal))


;;;; Initialization

(defmethod cm-init-constraints ((self cto-cm))
  (clrhash *cto-table*))


;;;; Parser

(define-constraint-parser (consume_the_only cto) ()
  (let ((item (<pattern-component>)))
    (make-constraint
      :type 'cto
      :pattern item)))


;;;; Filtering

#+:undef
(defmethod cm-eval-filter ((self cto-cm) cond bindings)
  t)

;;; For compute conditions:

(defun could_still_consume (item)
  (not (ctxt-gethash item *cto-table* nil)))


;;;; Adding a constraint

(defmethod cm-add-constraint ((self cto-cm) constraint)
  (let ((item (constraint-pattern constraint)))
    (cond ((ctxt-gethash item *cto-table* nil)
	   ;; Somebody already ate it.  You lose.
	   nil)
	  (t
	   ;; Looks like you can have it.
	   (setf (ctxt-gethash item *cto-table*) t)
	   t))))


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;

