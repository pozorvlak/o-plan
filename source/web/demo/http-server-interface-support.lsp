;;;; File: http-server-interface-support.lsp
;;; Contains: Support code for http-mode matrix interface
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: November 1999
;;; Updated: Thu Feb 24 05:31:18 2000 by Jeff Dalton
;;; Copyright: (c) 1999, 2000, AIAI, University of Edinburgh

(in-package :oplan)


;;;; A mixin class

(defclass http-server-interface-mixin ()
  ())

(in-local-defun-class http-server-interface-mixin *demo*)


;;;; Install a non-standard web-config

;;; N.B. much of this happens at load time /\/

(progn
  (setq *web-demo-dir-url* "invalid-web-demo-dir-url")
  (setq *web-special-url* "invalid-web-special-url")
  (setq *web-cgi-url* "invalid-web-cgi-url"))

(add-http-uri-interpretation "/demo-file/" 'http-show-file)

(add-http-uri-interpretation "/tf-file/" 'http-server-interface-show-tf-file)

(defun-local http-server-interface-show-tf-file (path r stream)
  (let ((*http-file-root*
	 (concat-string (get-parameter :oplan-tf-dir) "/")))
    (web-note "~&TF file ~S in ~S~%~%" path *http-file-root*)
    (http-show-file path r stream)))

(defun http-server-tf-url (name)
  (http-server-url *http-port* (concat-string "tf-file/" name)))


;;; Early init

(defmethod-local init-http-server-demo ()
  (setq *web-demo-dir-url*
	(http-server-url *http-port* "demo-file")))


;;;; Button bars and related stuff

(defun-local demo-restart-url ()
  (path-action-url :not-yet))

(defun-local write-coa-definition-page-buttons ()
  (html-button-bar
    `(("TF file"
       ,(demo-tf-url *demo*)))))

(defun-local write-farewell-page-buttons ()
  (html-button-bar
    `(("O-Plan"              "http://www.aiai.ed.ac.uk/~oplan/"))))


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;

