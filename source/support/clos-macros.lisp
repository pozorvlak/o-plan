;;;; File: clos-macros.lsp
;;; Contains: Macros related to CLOS
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: October 1996
;;; Updated: Fri Apr 16 21:45:24 1999 by Jeff Dalton
;;; Copyright: (c) 1996, AIAI, University of Edinburgh

(in-package :oplan-util)

(export '(defgeneric-export
	  defclass-export
	  import-class-slot-names))


;;; defgeneric-export 

(defmacro defgeneric-export (name lambda-list &rest etc)
  `(progn
     (export ',name)
     (defgeneric ,name ,lambda-list ,@etc)))


;;; Defclass-export

;;; Defclass-export exports the class and accessor names, but not the
;;; slot names.  However, the slot names are recorded and can be imported
;;; by using import-class-slot-names.

;;; Defclass-export does not take inheritance into account.  /\/

(defmacro defclass-export (name superclass-names slot-specs &rest options)
  (let ((exports '())
	(slot-names '()))
    (push name exports)
    (dolist (slot slot-specs)
      (let ((slot-name (car slot))
	    (slot-opts (cdr slot)))
	(push slot-name slot-names)
	(walk-plist #'(lambda (prop val)
			(when (eq prop :accessor)
			  (push val exports)))
		    slot-opts)))
    ;; Produce the macro expansion
    `(progn
       (eval-when (eval compile load)
	 (setf (get ',name 'class-slot-names) ',slot-names
	       (get ',name 'class-exports) ',exports))
       (export ',exports)
       (defclass ,name ,superclass-names
	 ,slot-specs
	 ,@options))))

(defmacro import-class-slot-names (class-name &optional (package nil))
  `(import (get ,class-name 'class-slot-names)
           ,@(if package (list package) nil)))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
