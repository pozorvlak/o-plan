;;;; File: KS-NO-MORE-ALTERNATIVES.lsp
;;; Contains: A special agenda entry to indicate that the planner has run
;;;   out of options.
;;; Author: Richard Kirby (rbk)
;;; Created: Sat Jul 18 11:32:32 1992
;;; Updated: Sun Sep 28 19:35:30 1997 by Jeff Dalton
;;; Copyright: (c) 1992, AIAI, University of Edinburgh

(in-package :oplan-knowledge-source)

(defun KS-NO-MORE-ALTERNATIVES (ag)
  (declare (ignore ag))
  (ipc-send :AM :FINISHED-PLANNING :NO-MORE-ALTERNATIVES)
  (ipc-send-out :NO-MORE-ALTERNATIVES))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;

