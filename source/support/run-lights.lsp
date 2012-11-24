;;;; File: run-lights.lsp
;;; Contains: Code for maintaining a window that shows the running processes
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: February 1993
;;; Updated: Thu Dec 12 01:21:43 1996 by Jeff Dalton
;;; Copyright: (c) 1992, 1993, AIAI, University of Edinburgh

;;; Run lights

(in-package :pprocess)

(use-package :oplan-x)

(export '(run-lights-on run-lights-off *run-lights-geometry*))

(defvar *run-lights-geometry* "51x1")

(defvar *lights-window* nil)

(defun ensure-lights-window (&optional (window-args nil))
  (unless *lights-window*
    (x-open-and-register-io-win :run-lights
      ;; N.B. later args seem to take precedence
      (append
        (list "-sl" "0")		;save no scrolled-off lines
	(or window-args
	    (list "-title" "O-Plan Running Processes"
		  "-name"  "O-Plan Processes"
		  "-geometry" *run-lights-geometry*
		  "-fn" "fixed"))))
    (setq *lights-window* (x-get-stream :run-lights))))

#+:undef
(defun display-run-lights (p)
  ;; p is the pprocess that's just about to run or else NIL
  (format *lights-window* "~%~{~S ~}~@[~S~]"
	  (mapcar #'pprocess-name
		  (ldiff *pushed-pprocesses* *push-pprocess*))
	  (if p (pprocess-name p) nil)))

(defun display-run-lights (p)
  ;; p is the pprocess that's just about to run or else NIL
  ;; Assumes pprocess names are keywords.
  (let ((out *lights-window*))
    (terpri out)
    (do ((pprocs *pushed-pprocesses* (cdr pprocs))
	 (end *push-pprocess*))
	((eq pprocs end))
      (write-char #\: out)
      (write-string (symbol-name (pprocess-name (car pprocs))) out)
      (write-string " " out))
    (when p
      (write-char #\: out)
      (write-string (symbol-name (pprocess-name p)) out))
    (force-output out)))		;/\/ do we need this?

(defun run-lights-on (&optional (window-args nil))
  (ensure-lights-window window-args)
  (display-run-lights *pprocess*)
  (advice+ 'run-pprocess 'run-lights
    #'(lambda (next)
	#'(lambda (p)
	    (display-run-lights p)
	    (unwind-protect (funcall next p)
	      (display-run-lights nil)))))
  (advice+ 'call-in-pprocess-env 'run-lights
    #'(lambda (next)
	#'(lambda (p fn &rest args)
	    (display-run-lights p)
	    (unwind-protect (apply next p fn args)
	      (display-run-lights nil)))))
  t)

(defun run-lights-off ()
  (mapc #'(lambda (f)
	    (advice- f 'run-lights))
	'(run-pprocess call-in-pprocess-env))
  (format *lights-window* "~%")
  t)

;;; End
