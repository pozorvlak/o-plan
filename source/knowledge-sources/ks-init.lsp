;;;; File: KS-INIT.lsp
;;; Contains: Initialises the planner.
;;; Author: Richard Kirby (rbk)
;;; Created: Tue Jul 10 17:13:35 1990
;;; Updated: Sun Nov 10 01:55:34 1996 by Jeff Dalton
;;; Copyright: (c) 1992, 1996, ks-kAIAI, University of Edinburgh

(in-package :oplan-knowledge-source)

(defun KS-INIT (one)
  (declare (ignore one))
  ;; Ask the IM to ask all clients except this KP to initialise.
  (ipc-ask-control :init)
  ;; Initialise this KP.
  (kp-init)
  ;; Report.
  (ipc-send-out :init-ok))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
