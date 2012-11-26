;;;; File: ks-question.lsp
;;; Contains: A KS for receiving answers to questions
;;; Author: Jeff Dalton
;;; Created: July 1998
;;; Updated: Sun May 30 22:38:19 1999 by Jeff Dalton
;;; Copyright: (c) 1998, AIAI, University of Edinburgh

(in-package :oplan-knowledge-source)

;;; KS-QUESTION can get as (ag-body event):
;;;
;;; :QUESTION question-kwd
;;;
;;;    At present, the question-kwd must be :SCHEMA-ORDER or :PSV-BINDING.
;;;
;;; A :question agenda-entry has an :answer trigger that makes it
;;; wait until an answer is available.  It then has to grab the answer
;;; and get planning started again.  Most of the interesting stuff is,
;;; as usual, done in the DM.  But it's here that we actually install
;;; the answer.

;;; /\/: At present, the mapping from question-kwds to accept procedures
;;; is wired-in.  That should change.

;;; /\/: The question-kwd, agenda-entry, and data are given in different
;;; orders at different points.  For instance, the "answer handler"
;;; gets the agenda-entry 1st, but at other points it's 2nd, or last.

(defun ks-question (event)
  (let* ((body (ag-body event))
	 (args (cdr body))
	 (question-kwd (first args)))
    (assert (length=1 args))
    (destructuring-bind (data agenda-entry)
			(db-request :get-question-answer question-kwd)
      (funcall (ks-question-answer-handler question-kwd agenda-entry)
	       agenda-entry
	       question-kwd
	       data))))

(defun ks-question-answer-handler (question-kwd agenda-entry)
  (ecase question-kwd
    (:schema-order
     (ecase (first (ag-body agenda-entry))
       (:expand  #'ks-expand-accept-answer)
       (:achieve #'ks-achieve-accept-answer)
       (:fix     #'ks-fix-accept-answer)))
    (:psv-binding
     #'ks-bind-accept-answer)))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
