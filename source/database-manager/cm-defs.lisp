;;;; File: cm-defs.lsp
;;; Contains: Classes and macros for defining constraint managers
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: March 1996
;;; Updated: Tue Feb 18 06:00:09 1997 by Jeff Dalton
;;; Copyright: (c) 1996, AIAI, University of Edinburgh

;;; /\/: Need to allow an CM to handle > 1 type.
;;; /\/: May want to allow > 1 CM per type.
;;; /\/: CMs should be able to see > 1 constraint at once.


;;;; Package

;;; This package exists so that there is a package CMs can use if
;;; they're not in the OPLAN package.

(in-package :oplan-cm-defs)

#+:lucid
(use-package :lucid-common-lisp)

(use-package :oplan-util)
(use-package :oplan-schema-defs)

(import '(oplan::check-error))		;from the sanity-checker

(export '(*all-constraint-managers*
	  *all-registered-constraint-managers*))

(export '(check-error))

;; From the constraint-associator
(export '(*constraint-env*))
(export '(*cm-poison* *cm-results*))	;should they be exported? /\/


;;;; Constraint managers

(defvar *all-constraint-managers* '())

(defvar *all-registered-constraint-managers* '())


;;; CM lookup

(defmacro %constraint-type->cm (type)	;internal primitive
  `(get ,type 'constraint-manager))

(defun-export exists-cm (type) ; -> cm or nil
  (%constraint-type->cm type))

(defun-export find-cm (type) ; -> cm or error
  (or (%constraint-type->cm type)
      (error "No constraint manager for type ~S." type)))

(defun-export constraint->cm (constraint)
  (or (%constraint-type->cm (constraint-type constraint))
      (error "No constraint manager for constraint ~S." constraint)))


;;; The constrait-manager class

(defclass-export constraint-manager ()
  ((name :accessor cm-name
	 :initarg :name
	 :initform (error "No name for CM."))
   (constraint-types			;set when registered
         :accessor cm-constraint-types)
   (own-contexts-p			;/\/ s.b. a method?
         :accessor cm-own-contexts-p
	 :initform nil)))

(defun-export make-constraint-manager (class &rest initargs)
  ;; Always call this, not make-instance.
  (let ((cm (apply #'make-instance class initargs)))
    (nconcf-new *all-constraint-managers* cm)
    cm))


;;; CM definition

;;; /\/: Must use defparameter, not defvar, for Micro-CLOS, because
;;; it doesn't do class redefinition.  Instead, there'll be a new
;;; class of the same name.

(defmacro-export define-constraint-manager
      (name &optional (supers '(constraint-manager))
	              slots)
  `(progn
     (defclass ,name ,supers ,slots)
     (defparameter ,name
       (make-constraint-manager ',name :name ',name))))


;;; CM registration

(defun-export register-constraint-manager
      (cm &key (constraint-types
		(error "No constraint types for ~S." cm)))
  (cm-register-self cm constraint-types)
  cm)

(defgeneric-export cm-register-self (cm constraint-types))

(defmethod cm-register-self ((cm constraint-manager) constraint-types)
  (nconcf-new *all-registered-constraint-managers* cm)
  (setf (cm-constraint-types cm) constraint-types)
  (dolist (type constraint-types)
    (setf (%constraint-type->cm type) cm))
  cm)


;;;; Generic functions

(defgeneric-export constraint-manager-p (obj)
  (:method (obj)
     (declare (ignore obj))
     nil)
  (:method ((cm constraint-manager))
     t))

(defgeneric-export cm-init-constraints (cm)) ;cm-init ? /\/

(defgeneric-export cm-new-domain (cm domain)
  (:method ((cm constraint-manager) domain)
     (declare (ignore domain))
     nil))

(defgeneric-export cm-eval-filters (cm constraints bindings))

(defgeneric-export cm-eval-filter (cm constraint bindings))

(defgeneric-export cm-add-constraints (cm constraints))

(defgeneric-export cm-add-constraint (cm constraint))

(defgeneric-export cm-check-constraints (cm)	;sanity-check
  (:method ((cm constraint-manager))
     nil))

(defgeneric-export cm-constraint-descriptions (cm))

(defgeneric-export cm-push-context (cm))

(defgeneric-export cm-pop-context (cm))

(defgeneric-export cm-get-context (cm))

(defgeneric-export cm-set-context (cm context-number))

(defgeneric-export cm-new-context (cm parent-humber))

(defgeneric-export cm-poison-context (cm agenda-id reason))


;;;; Other CM classes


;;; One-at-a-time-cm

(defclass-export one-at-a-time-cm (constraint-manager) ; mixin
  ())


;;; Variable-free-cm

(defclass-export variable-free-cm (constraint-manager) ; mixin
  ())


;;; Automatic-or-merging-cm

(defclass-export automatic-or-merging-cm (constraint-manager) ; mixin
  ())


;;; Own-context CM

;;; We provide a do- macro, rather than a walk- function, to avoid a
;;; function call when there are no own-context CMs.

(defvar *all-registered-own-context-constraint-managers* '())

(defmacro-export do-own-context-cms ((var &optional result-form) &rest body)
  ;; N.B. Only the registered own-context CMs are considered.
  `(dolist (,var *all-registered-own-context-constraint-managers* ,result-form)
     ,@body))

(defclass-export own-context-cm (constraint-manager) ;mixin
  ((own-contexts-p :initform t)))

(defmethod cm-register-self :after ((cm own-context-cm) constraint-types)
  (declare (ignore constraint-types))
  (nconcf-new *all-registered-own-context-constraint-managers* cm))

(defmethod cm-new-context ((cm own-context-cm) parent-number)
  ;; /\/: Not clear that this will work. Instead, the steps below might
  ;; have to be done at the DM level, to make sure that the mapping
  ;; from DM contexts to CM contexts is right.
  (let ((save (cm-get-context cm)))
    (cm-set-context cm parent-number)
    (prog2
      (cm-push-context cm)
      (cm-get-context cm)
      (cm-set-context cm save))))


;;; Standard CM

(defclass-export standard-cm (constraint-manager) ())


;;; Simple CM

(defclass-export simple-cm (one-at-a-time-cm constraint-manager) ())


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
