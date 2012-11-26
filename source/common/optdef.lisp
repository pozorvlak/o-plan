;;;; File: optdef.lsp
;;; Contains: Definition of the option structure
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: November 1996
;;; Updated: Sat Dec 14 23:34:45 1996 by Jeff Dalton
;;; Copyright: (c) 1996, AIAI, University of Edinburgh

(in-package :oplan)

;;; Options are similar to externally accessible contexts.

(defstruct (option (:print-function print-option))
  (name (error "Option lacks name."))
  (parent nil)
  (children '())
  base-context				;context number
  twinning-context			;context number
  current-context			;context number
  (alternatives '())
  (added-constraints '())
  )

(defun print-option (opt stream depth)
  (declare (ignore depth))
  (format stream "#<option ~S>" (option-name opt)))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
