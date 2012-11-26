;;;; File: always.lsp
;;; Contains: Simple "always" store
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: August 1996
;;; Updated: Wed Dec  4 13:48:46 1996 by Jeff Dalton
;;; Copyright: (c) 1996, AIAI, University of Edinburgh

(in-package :oplan)

(defvar *always-index* (make-hash-table :test #'eq))

(define-initializer :tgm clear-always-store ()
  (clrhash *always-index*))

(defun index-always-fact (fact)
  (push fact (gethash (car (always-fact-pattern fact)) *always-index*)))

(defun add-always-facts (facts)
  (mapc #'index-always-fact facts))

(defun get-always-shortlist (pattern &optional (value '??))
  (declare (ignore value))
  (gethash (car pattern) *always-index*))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
