;;;; File: KS-DOMAIN.lsp
;;; Contains: Handles the loading of a new TF file.
;;; Author: Richard Kirby (rbk)
;;; Created: Thu Jul 12 11:03:09 1990
;;; Updated: Sun Nov 10 01:42:46 1996 by Jeff Dalton
;;; Copyright: (c) 1992, AIAI, University of Edinburgh

(in-package :oplan-knowledge-source)

(defun KS-DOMAIN (what)
  (let* ((body (ag-body what))
	 (dom (cadr body))
	 (result (db-request :DOMAIN dom)))
    (ipc-send-out :DOMAIN result)
    (if result
	(dev-debug :information "Set up for domain ~A~%." what)
	(dev-debug :non-fatal-error
		   "Error in setting up for domain ~A~%." what))))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
