;;;; File: gpdt3-for-dera-matrix-support.lisp
;;; Contains: Support code for a "go places and do things" Web demo
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: May 1999
;;; Updated: Mon Feb  7 01:26:16 2000 by Jeff Dalton
;;; Copyright: (c) 1999, AIAI, University of Edinburgh

(in-package :oplan)


;;;; The demo class

(defclass gpdt3-for-dera-demo (gpdt3-demo)
  ((tf-file :initform "gpdt3-for-dera.tf")
   (tf-url  :initform (web-demo-url "gpdt3-for-dera.tf"))))


;;; Add a "Run server" button to the schema- and object-selection forms.

(defun-for gpdt3-for-dera-demo write-schema-selection-form-buttons (coa)
  (html-matrix-form-buttons "Select schema"
    :action-buttons
       `(("Run simulation" ,(coa-path-action-url :run-simulation coa)))
    :return-button nil))

(defun-for gpdt3-for-dera-demo write-object-selection-top-form-bottons (coa)
  (html-matrix-form-buttons "Select objects"
    :action-buttons
       `(("Run simulation" ,(coa-path-action-url :run-simulation coa)))
    :return-button nil))

(defun-for gpdt3-for-dera-demo write-object-selection-bottom-form-buttons (coa)
  (html-matrix-form-buttons "Select objects"
    :action-buttons
       `(("Run simulation" ,(coa-path-action-url :run-simulation coa)))
    :return-button nil))


;;;; :run-simulation

;;; For now, just sends plain text containing the input that would
;;; be given to the simulator.

(setf (get :run-simulation :path-action) 'write-dera-simulator-input)

(define-output-expander :xp-format (format-string &rest format-args)
  `(xp-format *output* ,format-string ,@format-args))

(defun write-dera-simulator-input ()
  (which-coa)
  (send-http-response-headers *http-request* *http-io*
    :content-type "text/plain")
  (output (:stream *http-io*)
       "[Domain File]"
    // (pathname-name (demo-tf-file *demo*))
    //
    // "[Issue]"
    // (:xp-format "~:W" (coa-question-agenda *coa*))
    //
    // "[Choices of approach]"
    // (:include (describe-choice-for-dera-simulator))
    //
    // "[World state]"
    // (:xp-format "~:{~&~:W = ~W~}"
	  (append (db-call 'get-always-facts)
		  (coa-initial-effects *coa*)))
    //))

(defun describe-choice-for-dera-simulator ()
  (ecase (coa-question-kwd *coa*)
    (:schema-order
     (describe-schemas-for-dera-simulator))
    (:psv-binding
     (describe-bindings-for-dera-simulator))))

(defun describe-schemas-for-dera-simulator ()
  ;; /\/: Need a less messy way to do this.
  ;; /\/: S.b. able to use ks-user code.
  (let ((*standard-output* *http-io*))
    (unwind-protect
	(progn
	  (advice-replace 'html-schema-selection-name 'gpdt3-for-dera
	    #'schema-name)
	  (write-schema-selection-descriptions
	    (coa-question-data *coa*)))
      (advice- 'html-schema-selection-name 'gpdt3-for-dera))))

(defun describe-bindings-for-dera-simulator ()
  (dolist (var (coa-question-data *coa*))
    (ks-user-describe-var var *http-io*)))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
