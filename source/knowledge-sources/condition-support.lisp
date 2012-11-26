;;;; File: condition-support.lisp
;;; Contains: Support for ks-condition and for expansion
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: November 1996
;;; Updated: Mon Dec 16 16:26:53 1996 by Jeff Dalton
;;; Copyright: (c) 1996, AIAI, University of Edinburgh

(in-package :oplan-knowledge-source)

(defun try-adding-condition (cond &optional bindings contributors-to-ignore)
  (let ((cond-temp cond)
        result)
    (if contributors-to-ignore
        (setq cond-temp (append cond-temp
                                (list (cons :NOT contributors-to-ignore)))))
    (setq result (db-request :CHECK-GOST cond-temp bindings))
    result))

(defun really-add-condition (cond)
  (db-request :ADD-GOST cond))

;; Goes through all the interactions in interact-list working out all the
;; KS-ORs possible. Returns t if added the cond, or nil if couldn't.
;;
(defun handle-adding-condition-interaction (cond interact)
  (handle-cm-result
    (db-request :handle-adding-condition-interaction cond interact)))

(defun find-contributors (or-rec)
  ;; Searches actions field of or-branch for an UPDATE entry, and
  ;; returns the arguments.
  (check-type or-rec or-branch)
  (second (find :UPDATE (or-branch-actions or-rec)
		:key #'car)))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
