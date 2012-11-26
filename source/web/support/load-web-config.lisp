;;;; File: load-web-config.lisp
;;; Contains: Support for Web / CGI demos
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: February 2000
;;; Updated: Sun Feb 13 23:04:19 2000 by Jeff Dalton
;;; Copyright: (c) 2000, AIAI, University of Edinburgh

(in-package :oplan)

(defparameter *web-config-name* "../lib/web-config.lisp")

(eval-when (load eval)
  (cond ((probe-file *web-config-name*)
	 (format t ";;; Loading web config file ~S." *web-config-name*)
	 (load *web-config-name*))
	(t
	 (warn "No Web config file ~S." *web-config-name*))))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
