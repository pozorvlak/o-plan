;;;; File: template.lisp
;;; Contains: The general Knowledge Source template.
;;; Author: Richard Kirby (rbk)
;;; Created: Wed Sep 19 13:07:45 1990
;;; Updated: Sun Nov 10 01:45:43 1996 by Jeff Dalton
;;; Copyright: (c) 1992, AIAI, University of Edinburgh

;;; The file is named the same as the knowledge source (in uppercase), with
;;; the prefix "KS-", and either ".o" or ".lisp" as the suffix. In the file
;;; there must be atleast one function definition, which must have the same
;;; name as the filename (without the ".o" or ".lisp" suffix).

;;; Below is the general template for a null KS.
;;; The one argument is set to the agenda record that caused this KS to be
;;; called.

;;; stage-manager is a macro for putting in the forms of each stage. The
;;; special forms (stage <why>) are used for possibly causing staging.
;;; <why> is a form that is evaluated. If <why> returns t then the KS will
;;; stage. <state-vars> is a list of variable names whose values are saved if
;;; the KS stages. <local-vars> is a list of local variables that do not need
;;; to be saved. On resumption of a stage, the <state-vars> are restored to
;;; the saved values, as if nothing had happened. NOTE: <local-vars> will be
;;; set to nil. Either or both lists can be nil.

(defun KS-NULL (arg)
  (stage-manager (<state-vars>) (<local-vars>)
		 arg
		 <stage 0 forms>
		 (stage <why>)
		 <stage 1 forms>
		 (stage <why>)
		 <stage 2 forms>
		 (stage <why>)
		 ...
		 (stage <why>)
		 <stage n forms>))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;

