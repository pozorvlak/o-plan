;;;; File: xml-viewer.lsp
;;; Contains: A way to view a plan as XML
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: April 1999
;;; Updated: Thu Apr 22 23:37:06 1999 by Jeff Dalton
;;; Copyright: (c) 1999, AIAI, University of Edinburgh

;;; Requires a xml-print-plan-description procedure.

(in-package :oplan-plan-world-viewer)

;;; Handles viewer-args:
;;;
;;;  :MODE :XML
;;;  :LEVELS <:ALL or number>
;;;  :OUTPUT-FILE <file name or :SCREEN>

(defun output-xml-plan (all-nodes)
  (let* ((nodes (slice-to-level all-nodes))
	 (where (get-xml-destination)))
    (case where
      (:quit)
      (:screen
       (write-xml nodes *window*))
      (t
       (with-open-file (stream where :direction :output)
	 (write-xml nodes stream))))))

(defun get-xml-destination ()
  (let ((dest (ask-user :output-file "File for XML plan description")))
    (cond ((string= dest "")
	   :quit)
	  ((string= dest ":screen")
	   :screen)
	  (t
	   dest))))

;;; Interface to the imported XML printer

(defvar *xml-printer* nil)

(defun write-xml (nodes stream)
  ;; Call the function if it exists
  (let ((printer (ensure-xml-printer)))
    (if printer
	(funcall printer nodes stream
		 :domain oplan-task-assigner::*domain*
		 :task   oplan-task-assigner::*task*)
      (error "No XML plan-formatter has been defined"))))

(defun ensure-xml-printer ()
  ;; Grab the symbol if we haven't already done so.
  (ensuref *xml-printer*
	   (and (find-package '#:xml)
		(find-symbol (symbol-name '#:xml-print-plan-description)
			     '#:xml)))
  ;; See if the function is defined
  (and *xml-printer*
       (fboundp *xml-printer*)
       *xml-printer*))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
