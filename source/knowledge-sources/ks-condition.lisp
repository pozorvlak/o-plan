;;;; File: KS-CONDITION.lsp
;;; Contains: Check on conditions that need tying up.
;;; Author: Richard Kirby (rbk)
;;; Created: Thu Jul 11 11:56:35 1991
;;; Updated: Mon Dec 16 16:26:27 1996 by Jeff Dalton
;;; Copyright: (c) 1992, AIAI, University of Edinburgh

(in-package :oplan-knowledge-source)

(defun KS-CONDITION (ag)
  (STAGE-MANAGER
   (contributors-to-ignore) (cond) ag
   ;; The first stage.
   ;; See if condition is satisfyable with the existing effects.
   (setq cond (cadr (ag-body ag)))
   (whats-going-on "Trying to satisfy~%~S" cond)
   (if (handle-adding-condition cond contributors-to-ignore)
       (whats-going-on "Satisfied")
     (progn
       (dev-debug :non-fatal-error "Couldn't add cond ~S~%" cond)
       (whats-going-on "Couldn't satisfy")
       (post-agenda `(:POISON-STATE :COULD-NOT-SATISFY-CONDITION ,cond))))))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
