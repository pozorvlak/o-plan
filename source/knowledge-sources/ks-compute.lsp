;;;; File: KS-COMPUTE.lsp
;;; Contains: The COMPUTE KS
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: July 1994
;;; Updated: Sun Dec  1 18:01:54 1996 by Jeff Dalton
;;; Copyright: (c) 1994, AIAI, University of Edinburgh

(in-package :oplan-knowledge-source)

;;; The trigger should have assured that all PSVs in the pattern are bound,
;;; but the value may contain some unbound ones.

(defun KS-COMPUTE (ag)
  (destructuring-bind (cond) (cdr (ag-body ag))
    (kp-add-constraint 'compute cond)))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
