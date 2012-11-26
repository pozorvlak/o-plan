;;;; File: http-mode.lsp
;;; Contains: HTTP / Netscape mode
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: May 1999
;;; Updated: Mon Feb 14 01:44:46 2000 by Jeff Dalton
;;; Copyright: (c) 1999, AIAI, University of Edinburgh

(in-package :oplan)

;;; HTTP[-server] / Netscape mode

;;; The relevant command-line arguments are:
;;;
;;;    -browser name
;;;
;;;    -http
;;;
;;;    -netscape
;;;       Equivalent to "-http -browser netscape"
;;;
;;;    -port number
;;;
;;;    -server-host hostname
;;;
;;;    -domain tf-file-name
;;;
;;;    -interface-class class-name
;;;
;;; Note that it may also be necessary to use -load and -load-system,
;;; e.g. to define the interface class.  However, a system that has the
;;; same name as the class will be loaded automatically, if such a
;;; system has been defined.
;;;

(defun set-up-http-mode ()
  (set-parameter :http t)
  (set-parameter :windows nil)
  (set-parameter :interactive nil)
  (load-system 'matrix-server-support :recursive :if-not-loaded)
  (set-up-subr-mode)
  (set-parameter :do "(enter-http-mode)"))

(defun set-up-netscape-mode ()
  (unless (parameter-set-p :browser)
    (set-parameter :browser "netscape"))
  (set-up-http-mode))

(defun enter-http-mode ()
  (run-as-http-server))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
