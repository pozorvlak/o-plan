;;;; File: com-defs.lisp
;;; Contains: Definitions for parser and compiler
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: March 1992
;;; Updated: Sun Aug 22 23:41:56 1999 by Jeff Dalton
;;; Copyright: (c) 1992, 1993, 1994 AIAI, University of Edinburgh

(in-package :oplan-tf-compiler)

(defparameter *tf-language-package* (find-package :tf-language))

;;; Compilation parameters

;;; If the variable *check-syntax-only* is true, then syntax
;;; checking should be performed without other processing such
;;; as code generation.

;;; /\/: Actually, this is somewhat losing, because a number of useful
;;; checks are effectively part of code generation.  Maybe make "check-only"
;;; do all but modify the domain given to the compiler.

(defvar *syntax-check-only* nil
  "True to turn off code generation.")

(defvar *running-as-tfc* nil
  "True if running as the standalone syntax checker (tfc).")

(defvar *tf-stream* nil
  "The stream containing TF forms, when compiling from a stream.")

(defvar *include-stack* nil
  "Information saved while processing include forms.")

(defvar *input-domain* nil
  "Domain information constructed before the current call to the compiler
   and being used as a base for further compilation.")

(defvar *output-domain* nil
  "Domain information that will be the result of the current call to the
   compiler.")

(defvar *domain* nil
  "The domain information structure being processed by the compiler.")


;;; Reporting parameters

(defparameter *suppress-messages* nil)

(defparameter *progress-reports* t)

(defparameter *level-report* nil)

(defparameter *level-report-include-action-effects* nil)


;;; Other globals

(defvar *new-schemas* nil
  "New schema structures as they are produced by the compiler.")

(defvar *schema-plist* nil
  ;; Stores random information that is not part of the parse tree.
  )

(defmacro getf-schema (prop)
  `(getf *schema-plist* ,prop))

(defparameter *schema-types*		; /\/: should be keywords?
  '(schema meta_schema repair_schema process_schema meta_process_schema))

(defconstant *undef* ':undef)


;;; Schema names

(defun ->schema-name (sym)
  sym)

(defun ->task-name (sym)
  (concat-name '#:task_ sym))

(defun same-name-p (name1 name2)
  (eq name1 name2))

(defun task-name-p (name)
  (and (symbolp name)
       (eql (mismatch "task_" (symbol-name name))
	    #.(length "task_"))))

;;; End
