;;;; File: any-tf-http-server-support.lisp
;;; Contains: Support code for generic matrix demo
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: November 1999
;;; Updated: Thu Feb 24 05:49:29 2000 by Jeff Dalton
;;; Copyright: (c) 1999, 2000, AIAI, University of Edinburgh

(in-package :oplan)


;;;; The demo class

(defclass any-tf-http-server-demo (http-server-interface-mixin
				   any-tf-matrix-demo)
  ())

(in-local-defun-class any-tf-http-server-demo *demo*)


;;;; Startup

;;; /\/: See any-tf-matrix-support.lisp for something about why we
;;; construct the TF file name as we do.

(defmethod-local init-http-server-demo :after ()
  (let* ((domain (or (get-parameter :domain)
		     (error "No domain specified")))
	 (relative-name (concat-string domain ".tf")))
    (setf (demo-tf-file *demo*)
	  (concat-string (get-parameter :oplan-tf-dir) "/" relative-name))
    (setf (demo-tf-url *demo*)
	  (http-server-tf-url relative-name))))

(defun-local install-demo-parameters ()
  ;; Already done by init-http-server-demo.
  nil)

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;

