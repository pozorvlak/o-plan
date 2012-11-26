;;;; File: hidden-clos.lsp
;;; Contains: Support for CLOSification
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: May 1999
;;; Updated: Thu Feb 24 05:06:40 2000 by Jeff Dalton
;;; Copyright: (c) 1999 AIAI, University of Edinburgh

;;; Suppose you have a bunch of function definitions, and you decide
;;; that, really, you should to things in a more object-oriented way,
;;; with different versions of the functions for different classes.
;;; It may be that the relevant object -- the one that determines
;;; which version should be used -- is not already passed to the
;;; functions (instead, it's the value of a special variable) and
;;; that it would be awkward to add it as a new parameter everywhere.
;;; Or maybe there isn't already such an object, but there could be one.

;;; If so, you may want to use the macros defined here.

;;; (in-local-defun-class class-name var-name), where var-name is the
;;; name of a special variable, defines a compile-time context for
;;; {defun,defmethod}-{local,for} definitions that lasts until the next
;;; in-local-defun-class.  A typical way to use it would be to put
;;; an in-local-defun-class near the start of any file that contains
;;; -local or -for definitions (before any of those definitions).

;;; If the in-local-defun-class class and variable are C and V,
;;; respectively, defun-local and defun-for define a "version"
;;; of a function that has V as an implicit extra parameter.
;;; defun-local defines a version for the case when V is an
;;; instance of C; defun-for lets you specify the class.

;;; When the function is called, the most specific applicable
;;; version is called, where "most specific" is determined by
;;; order in the class-precedence-list of (class-of V).

;;; Think of it like this.  Suppose F is once of these "functions
;;; with versions".  Then (F arg*) = (funcall (F-version V) arg*).

;;; This is somewhat confusing, and may not be the best way to do
;;; this sort of thing.  But it is nonetheless useful.

;;; Defmethod-local and defmethod-for are similar in intent but
;;; work in a more obvious way.  For a function F that takes an
;;; implicit parameter, they define a method on a generic function
;;; called F-methods and ensure that F is a function that calls
;;; F-methods passing the value of the implicit var as the first
;;; argument.  So (F arg*) = (F-methods V arg*).


(in-package :oplan-util)

(export '(in-local-defun-class
	  defun-local defun-for
	  defmethod-local defmethod-for
	  implicit-self))

(defvar *local-defun-class* 'not-a-local-defun-class)
(defvar *local-defun-var* 'not-a-local-defun-var)

(defmacro in-local-defun-class (class-name var-name)
  `(eval-when (eval compile)
     (setq *local-defun-class* ',class-name
	   *local-defun-var* ',var-name)))


;;; Defun-local and defun-for

(defmacro defun-local (fname lambda-list &body body)
  `(defun-for ,*local-defun-class* ,fname ,lambda-list . ,body))

(defmacro defun-for (implicit-class fname lambda-list &body body)
  (let ((full-name (concat-name fname "-FOR-" implicit-class))
	(lookup-name (concat-name fname "-VERSION")))
    (multiple-value-bind (forms dcls docs) (parse-body body)
      `(progn
         (defun ,full-name ,lambda-list
	   ,@docs
	   ,@dcls
	   (block ,fname
	     ,@forms))
	 (defmethod ,lookup-name ((implicit-arg ,implicit-class))
	   ',full-name)
	 (ensure-fn-for ',fname ',lookup-name ',*local-defun-var*)
	 ',full-name))))

(defun ensure-fn-for (fname lookup-fn var-name)
  (unless (fboundp fname)
    (setf (get fname :implicit-parameter) var-name)
    (setf (symbol-function fname)
	  (make-defun-for-dispatching-function lookup-fn var-name))))

(defun make-defun-for-dispatching-function (lookup-fn var-name)
  (#+akcl sys:turbo-closure
   #-akcl identity
   #'(lambda (&rest args)
       (apply (funcall lookup-fn (symbol-value var-name))
	      args))))


;;; Defmethod-local and defmethod-for

(defmacro defmethod-local (fname qualifier lambda-list &rest body)
  `(defmethod-for ,*local-defun-class* ,fname ,qualifier ,lambda-list . ,body))

(defmacro defmethod-for (implicit-class fname qualifier lambda-list &rest body)
  (when (listp qualifier)		;it's really the lambda-list
    (setq body (cons lambda-list body)
	  lambda-list qualifier
	  qualifier nil))
  (let ((full-name (concat-name fname "-METHODS")))
    (multiple-value-bind (forms dcls docs) (parse-body body)
      `(progn
	 (ensure-method-for ',fname ',full-name ',*local-defun-var*)
         (defmethod ,full-name
	            ,@(when qualifier (list qualifier))
	            ((implicit-self ,implicit-class) ,@lambda-list)
	   ,@docs
	   ,@dcls
	   (block ,fname
	     ,@forms))))))

(defun ensure-method-for (fname full-name var-name)
  (unless (fboundp fname)
    (setf (get fname :implicit-parameter) var-name)
    (setf (symbol-function fname)
	  (make-defmethod-for-dispatching-function full-name var-name))))

(defun make-defmethod-for-dispatching-function (full-name var-name)
  (#+akcl sys:turbo-closure
   #-akcl identity
   #'(lambda (&rest args)
       (apply full-name (symbol-value var-name) args))))


;;; Util

(defun parse-body (body)
  "Splits doc string and declarations from a body of code."
  (let ((docs '())
	(dcls '()))
    (do () ((endp body))
      (let ((exp (car body)))
	(cond ((and (stringp exp) (not (endp (cdr body))))
	       (if (null docs)
		   (setq docs (list exp))
		 (error "Multiple doc strings:~%~S~%and ~S." (car docs) exp)))
	      ((and (consp exp) (eq (car exp) 'declare))
	       (push exp dcls))
	      (t (return)))
	(pop body)))
    (values body (nreverse dcls) docs)))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;

