;;;; File: KS-KILL.lisp
;;; Contains: Kills off the planner.
;;; Author: Richard Kirby (rbk) and Jeff Dalton
;;; Created: Fri Aug 10 15:58:46 1990
;;; Updated: Sun Nov 10 01:40:14 1996 by Jeff Dalton
;;; Copyright: (c) 1992, 1996, AIAI, University of Edinburgh

;;; /\/: Kills off the planner?  Actually, it isn't normally called at all.

(in-package :oplan-knowledge-source)

(defun KS-KILL (one)
  (declare (ignore one))
  (ipc-ask-control :dying)
  (terminate-pprocess))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;

