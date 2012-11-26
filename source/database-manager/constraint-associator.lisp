;;;; File: constraint-associator.lisp
;;; Contains: Constraint associator
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: March 1996
;;; Updated: Fri May 14 00:54:15 1999 by Jeff Dalton
;;; Copyright: (c) 1996, AIAI, University of Edinburgh


;;; /\/: For now, the constraint type is specified by the caller rather
;;; than being extracted from the constraint(s).  This is because some
;;; constraints might not, at present, contain their type.  At some
;;; point, this may change.  In particular, we will want to provide a
;;; *constrant-env* across the addition of constraints of more than one
;;; type.


(in-package :oplan)


;;;; Initialization

(define-initializer :dm ca-init-constraint-managers ()
  (dolist (cm *all-constraint-managers*)
    (cm-init-constraints cm)))


;;;; Constraint managers

(defun walk-constraint-managers (fn)
  (mapc fn *all-constraint-managers*))

(defun walk-registered-constraint-managers (fn)
  (mapc fn *all-registered-constraint-managers*))


;;;; Adding constraints

;;; Constraints are added by finding the constraint manager (CM) for
;;; the constraint-type and calling the CM's add-constraint operation.
;;; The result will be one of:
;;;    T            The constraint has been added.
;;;    NIL          The constraint cannot be added.
;;;    An or-tree   The constraint has been added but additional steps
;;;                   are required to ensure it's correct.

(defvar *constraint-env*)
(defvar *cm-poison*)
(defvar *cm-results*)

(defmessage (:dm :add-constraints) (constraint-type constraints)
  (ca-add-constraints constraint-type
		      constraints))

(defmessage (:dm :add-constraint) (constraint-type constraint)
  (ca-add-constraints constraint-type
		      (list constraint)))

(defun ca-add-constraints (constraint-type constraints)
  (let ((*constraint-env* '()))
    (cm-add-constraints (or (exists-cm constraint-type)
			    (find-cm 'other))
			constraints)))

(defun ca-add-constraints-for (cm constraints)
  ;; Adds constraints one at a time.
  (let ((or-trees '()))
    (dolist (c constraints
	       (or (nreverse or-trees)
		   t))       
      (let ((result (cm-add-constraint cm c)))
	(cond ((null result)		;exit on 1st failure
	       (return nil))
	      ((eq result t)		;ignore straight successes
	       )
	      ((or-tree-p result)	;record any or-trees
	       (push result *constraint-env*)
	       (push result or-trees))
	      (t
	       (error "Invalid add-constraints result for constraint ~S:~%~S."
		      c result)))))))

;;; Some CMs need to be able to ask for agenda entries to be posted.

(defun ca-agenda-wrapper (handler constraints)
  ;; The handler should return true if all the constraints can be added
  ;; and false if it's posted a poison.  Moreover, when all the constraints
  ;; can be added, *cm-poison* must be null; otherwise *cm-results* must
  ;; be null -- this allows a consistency check.
  (let ((*cm-poison* nil)
	(*cm-results* '()))
    (cond ((funcall handler constraints)
	   (check-type *cm-poison* null)
	   (if *cm-results*
	       (reverse *cm-results*)
	     t))
	  (t
	   (check-type *cm-results* null)
	   (if *cm-poison*
	       (list *cm-poison*)
	     nil)))))

(defun ca-post-agenda (body &rest more-args)
  (assert (null *cm-poison*))
  (let ((result (list* :AGENDA body more-args)))
    (cond ((list-beginning :POISON-STATE body)
	   (setq *cm-poison* result
		 *cm-results* nil))
	  (t
	   (push result *cm-results*)))))

(defun ca-post-failure () ; -> nil
  (assert (null *cm-poison*))
  (setq *cm-results* nil))


;;;; Add-constraints methods

(defmethod cm-add-constraints ((cm one-at-a-time-cm) constraints)
  (ca-add-constraints-for cm constraints))

(defmethod cm-add-constraints :before ((cm variable-free-cm) constraints)
  ;; /\/: For now, signal an error if the constraints contain
  ;; any variables.  Later, use an :around method and return agenda
  ;; entries for constraints that need to wait for variables to be
  ;; bound.
  (dolist (c constraints)
    (unless (fully-instantiated-p c)
      (error "~A can't handle variables in constraint ~S." cm c))))


;;;; Processing filter constraints while selecting a schema

;;; From each constraint, we can get t, nil, or an or-tree.  If
;;; there's more than one constraint, the results are combined,
;;; and the result is again t, nil, or an or-tree.

;;; The bindings passed in are not modified, and we check to make sure.

;;; A constraint manager that handles blocks of constraints on its
;;; one (ie, it does not call ca-eval-filters-for-cm) can return
;;; whatever it wants, since nothing in the CA checks.  /\/

(defmessage (:dm :eval-filters) (constraint-type constraints bindings)
  (ca-eval-filters constraint-type
		   constraints
		   bindings))

(defmessage (:dm :eval-filter) (constraint-type constraint bindings)
  (ca-eval-filters constraint-type
		   (list constraint)
		   bindings))

(defun ca-eval-filters (constraint-type constraints bindings)
  (let ((original-bindings (copy-tree bindings))
	(*constraint-env* '()))
    (prog1
      (cm-eval-filters (or (exists-cm constraint-type)
			   (find-cm 'other))
		       constraints
		       bindings)
      ;; Check that the CM hasn't messed with the original bindings.
      (assert (equal bindings original-bindings)))))

(defun ca-eval-filters-for (cm constraints bindings)
  ;; Evaluate the filters one at a time.
  ;; /\/: The accumulated-bindings can be used if we discover some values
  ;; along the way.
  (let ((accumulated-bindings nil)
	(or-tree nil))
    (dolist (c constraints
	       (or or-tree
		   t))
      (let ((result (cm-eval-filter cm c (or accumulated-bindings bindings))))
	(cond ((null result)		;exit on 1st failure
	       (return nil))
	      ((eq result t)		;ignore straight successes
	       )
	      ((or-tree-p result)
	       (setq or-tree
		     (if or-tree
			 (merge-or-trees (list or-tree result))
		       result))
	       ;; /\/: Maybe change accumulated-bindings here
	       )
	      (t
	       (error "Invalid eval-filters result for constraint ~S:~%~S."
		      c result)))))))


;;;; Eval-filters methods

(defmethod cm-eval-filters ((cm one-at-a-time-cm) constraints bindings)
  (ca-eval-filters-for cm constraints bindings))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
