;;;; File: pretend-web.lsp
;;; Contains: Code for running Web demos without the Web
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: October 1994
;;; Updated: Sun Nov 10 01:48:59 1996 by Jeff Dalton
;;; Copyright: (c) 1994, AIAI, University of Edinburgh

(in-package :oplan)

;;; Like this:
;;;   Make sure you're in the source directory.
;;;   Run O-Plan in subr mode.
;;;   Load this file and the Web demo "-support" file.
;;;   Call (fake-query-args <string>).
;;;   Call the main fn of the Web demo.
;;;
;;; Fake-query-args expects the args to be separated by newline-space-space
;;; instead of "&", because that's what appears in the "-info" files.
;;;
;;; N.B. If use use with-web-environment, call fake-query-args
;;; even if you don't use query-args.  (In that case, use "".)
;;;
;;; This file exists because when a Web demo goes wrong it can be
;;; a major pain to figure out why.  For instance, you might get an
;;; error message but not a backtrace.  With this file, you can run
;;; the demo without Mosaic, CGI, or anything else and get a Lisp
;;; error in the usual (interactive) way.
;;;

(defparameter *web-demo-dir-url* "http:://localhost/fake-demo")

(defun call-with-web-environment (thunk) ; redefine
  ;; N.B. no condition handler
  (establish-web-setup)
  (funcall thunk))

(defun fake-query-args (string)
  (setq *query-arg-string*
	(convert-fake-query-args string)))

(defun convert-fake-query-args (string)
  (big-string-concat
    (separate-by "&"
      (remove ""
	      (mapcar (partial1 #'string-trim '(#\Space))
		      (break-string-at #\Newline string))
	      :test #'string=))))

;;; (separate-by 'or '(a b c)) -> (a or b or c)

(defun separate-by (separator items)
  (cdr (mapcan (partial1 #'list separator) items)))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
