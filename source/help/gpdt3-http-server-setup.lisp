;;;; File: gpdt3-http-server-setup.lisp
;;; Contains: Support code for GPDT matrix http interface
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: May 2000
;;; Updated: Tue May 30 20:04:00 2000 by Jeff Dalton
;;; Copyright: (c) 2000, AIAI, University of Edinburgh

(in-package :oplan)


;;;; System definition

(defsystem (gpdt3-http-server-demo
	      :required-systems (matrix-interface-support
				 gpdt3-demo))
  ;; The only file is this one.
  )

;;; Must load before we can define the class.

(load-system 'gpdt3-http-server-demo :recursive :if-not-loaded)


(setf (get-parameter :oplan-tf-dir) ; for the TF file URL
      (concat-string (get-parameter :oplan-dir) "/source/web/demo"))


;;;; The demo class

(ensuref (get-parameter :interface-class) "gpdt3-http-server")

(defclass gpdt3-http-server-demo (http-server-interface-mixin
				  gpdt3-demo)
  ())

(in-local-defun-class gpdt3-http-server-demo *demo*)

(advice-replace 'server-restart-url 'gpdt3-http-server-setup
  #'demo-restart-url)

(defmethod-local init-http-server-demo :after ()
  (setf (get-parameter :oplan-tf-dir)
	(concat-string (get-parameter :oplan-dir) "/source/web/demo"))
  (let* ((domain "gpdt3")
	 (relative-name (concat-string domain ".tf")))
    (setf (demo-tf-file *demo*)
	  (concat-string (get-parameter :oplan-tf-dir) "/" relative-name))
    (setf (demo-tf-url *demo*)
	  (http-server-tf-url relative-name))))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;

