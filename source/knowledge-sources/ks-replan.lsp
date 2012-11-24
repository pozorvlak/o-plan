;;;; File: ks-replan.lsp
;;; Contains: A KS that replans
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: December 1996
;;; Updated: Wed Dec 18 21:01:20 1996 by Jeff Dalton
;;; Copyright: (c) 1996, AIAI, University of Edinburgh

(in-package :oplan-knowledge-source)

(defun KS-REPLAN (ae)
  (declare (ignore ae))
  (handle-poisoning
    `(:POISON-STATE :REPLANNING)))  

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
