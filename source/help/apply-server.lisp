;;; File: help/apply-server.lisp
;;; Author: Jeff Dalton
;;; Updated: Thu Dec 12 01:51:14 1996 by Jeff Dalton

;;; To make O-Plan act as an "apply server", run it in "subroutine" mode,
;;; load this file, and have it call the apply-server function.  You'll
;;; normally want to do this by using the script help/oplan-apply-server.

(in-package :oplan)

(defun apply-server ()
  (let ((*print-length* nil)
	(*print-level* nil)
	(*print-pretty* nil)
	(*print-escape* t))
    (ipc-write-agent-ready *standard-output*)
    (handler-case
        (loop
          (let ((request (read-safely *standard-input* nil :quit)))
	    (when (eq request :quit)
	      (return))
	    (send-server-reply
	      (apply #'funcall request))))
      (error (c)
	(send-server-reply
	  `(:error ,(type-of c) ,(princ-to-string c)))))))

(defun send-server-reply (reply)
  (print-readably reply *standard-output*)
  (terpri *standard-output*)
  (force-output *standard-output*))

;;; End
