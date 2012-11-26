;;;; File: o-face.lisp
;;; Contains: Interface to TF compiler.
;;; Author: Jeff Dalton and Richard Kirby (rbk)
;;; Created: Thu Jun 18 12:06:13 1992
;;; Updated: Tue Mar 16 04:16:31 1999 by Jeff Dalton
;;; Copyright: (c) 1992, AIAI, University of Edinburgh

(in-package :oplan-tf-compiler)

;;; Reporting procedures

;;; Syntax errors are reported by calling SYNTAX-ERROR.  COMPILE-TF
;;; arranges for SYNTAX-ERROR to call TF-SYNTAX-ERROR-REPORTER.

;;; Note that we don't increment *ERROR-COUNT* here.  That's done
;;; by SYNTAX-ERROR.

(defun tf-syntax-error-reporter (message &rest format-args)
  "The procedure that reports syntax errors detected by the TF compiler."
  (unless *suppress-messages*
    (if *include-stack*
	(tf-message :error "Error in ~S, line ~D: ~(~?~)"
		    (car *include-stack*) *line-number* message format-args)
        (tf-message :error "Error, line ~D: ~(~?~)"
		    *line-number* message format-args))))

;;; Other errors detected by the TF compiler are reported by calling
;;; TF-ERROR.  Similar procedures for warnings, progress resports, and
;;; general messages are TF-WARNING, TF-PROGRESS and TF-NOTE or TF-MESSAGE
;;; respectively.  TF-MESSAGE is the basic primitive used by the others.

(defun tf-error (message &rest format-args)
  (incf *error-count*)
  (unless *suppress-messages*
    (tf-message :error "Error: ~?" message format-args)))

(defun tf-warning (message &rest format-args)
  (incf *warning-count*)
  (unless *suppress-messages*
    (tf-message :warning "Warning: ~?" message format-args)))

(defun tf-note (message &rest format-args)
  (unless *suppress-messages*
    (apply #'tf-message :note message format-args)))

(defun tf-message (message-type message &rest format-args)
  (unless *suppress-messages*
    (if *running-as-tfc*
	(xp-format t "~&~?~%" message format-args)
      ;; Running as part of the planner.  The level type must be given
      ;; explicitly, because that's what dev-note requires.  We also
      ;; have to call fresh-line, because dev-note doesn't.  Hence the
      ;; somewhat elaborate code.
      (case message-type
	(:error
	 (dev-level-case
	   (:error   (fresh-line *trace-output*)
		     (dev-note :tf "~?" message format-args))))
	(:warning
	 (dev-level-case
	   (:warning (fresh-line *trace-output*)
		     (dev-note :tf "~?" message format-args))))
	(otherwise
	 (dev-level-case
	   (:trace   (fresh-line *trace-output*)
		     (dev-note :tf "~?" message format-args))))))))

;;; TF-PROGRESS is used to begin a new line of progress description.
;;; TF-SUB-PROGRESS can then be used to print more items on the same line
;;; and to print the final newline.

(defun tf-progress (message &rest format-args)
  (unless *suppress-messages*
    (when *progress-reports*
      (if *running-as-tfc*
	  (progn
	    (xp-format t "~&~?" message format-args)
	    (force-output t))
	(dev-format :trace "~&~A: ~?" :tf message format-args)))))

(defun tf-sub-progress (message &rest format-args)
  (unless *suppress-messages*
    (when *progress-reports*
      (if *running-as-tfc*
	  (progn
	    (format t "~?" message format-args)
	    (force-output t))
	(dev-format :trace "~?" message format-args)))))

;;; End

