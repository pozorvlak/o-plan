;;;; File: mout1-http-server-setup.lsp
;;; Contains: Support code for Mout-1 matrix http interface
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: May 2000
;;; Updated: Tue May 30 20:01:46 2000 by Jeff Dalton
;;; Copyright: (c) 2000, AIAI, University of Edinburgh

(in-package :oplan)


;;;; System definition

(defsystem (mout1-http-server-demo
	      :required-systems (matrix-interface-support
				 mout1-demo))
  ;; The only file is this one.
  )


;;; Must load before we can define the class.

(load-system 'mout1-http-server-demo :recursive :if-not-loaded)


;;;; The demo class

(ensuref (get-parameter :interface-class) "mout1-http-server")

(defclass mout1-http-server-demo (http-server-interface-mixin
				  mout1-demo)
  ())

(in-local-defun-class mout1-http-server-demo *demo*)

(advice-replace 'server-restart-url 'mout1-http-server-setup
  #'demo-restart-url)


;;; Methods very like those for any-tf-http-server-demo

(defmethod-local init-http-server-demo :after ()
  (setf (get-parameter :oplan-tf-dir)
	(concat-string (get-parameter :oplan-dir) "/source/test-tf"))
  (let* ((domain (or (get-parameter :domain)
		     "mout1"))
	 (relative-name (concat-string domain ".tf")))
    (setf (demo-tf-file *demo*)
	  (concat-string (get-parameter :oplan-tf-dir) "/" relative-name))
    (setf (demo-tf-url *demo*)
	  (http-server-tf-url relative-name))))

(defun-local install-demo-parameters ()
  ;; Already done by init-http-server-demo.
  nil)


;;;; Explicitly specify index.html for scenario

(advice-replace 'mout1-scenario-url 'mout1-http-server-setup
  #'(lambda ()
      (web-demo-url "mout1-scenario/index.html")))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
