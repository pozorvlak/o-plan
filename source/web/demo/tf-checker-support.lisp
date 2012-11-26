;;;; File: any-tf-matrix-support.lsp
;;; Contains: Support code for a TF syntax-checker demo
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: April 1999
;;; Updated: Tue Apr 20 05:48:08 1999 by Jeff Dalton
;;; Copyright: (c) 1997, 1998, 1999, AIAI, University of Edinburgh

(in-package :oplan)

(defparameter *tf-checker-parameters*
  '((:host     (:text)      "host name")	;e.g. localhost
    (:port     (:int 0)     "port number")	;usually 80
    (:path     (:text)      "path")		;e.g. /~jeff/
    (:levels   (:checkbox)  "show action and effect levels")
    (:effects  (:checkbox)  "show effects with actions")
    (:schemas  (:checkbox)  "include schema table")))

(defun tf-checker-cgi ()
  (write-cgi-response-headers "text/plain")
  (with-web-environment "tf-checker"
    (parse-query-args)
    (convert-query-args *tf-checker-parameters*)
    (let ((host (query-arg :host))
	  (port (query-arg :port))
	  (path (query-arg :path)))
      (when (sequence-begins "/" path)
        (setq path (sequence-after "/" path)))
      (with-open-stream (stream (get-http-url-stream host port path))
	(output "Checking http:" "//" host ":" port "/" path // //)
	(let ((*standard-input* stream))
	  (apply #'tfc
	    `(,@(when (query-arg :levels)  '("-l"))
	      ,@(when (query-arg :effects) '("-e"))
	      "-")))))))

(advice+ 'oplan-tf-compiler::convert-language-text 'tf-checker
  #'(lambda (previous)
       #'(lambda (language-name lines)
           (let ((forms (funcall previous language-name lines)))
	     (dolist (form forms)
	       (when (list-beginning 'oplan-tf-compiler::eval-in-tf-compiler
				     form)
		 (oplan-tf-compiler::syntax-error
		   "Illegal \"language lisp\" form")))
	     '()))))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
