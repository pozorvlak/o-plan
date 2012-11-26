;;;; File: KS-SET-TASK.lsp
;;; Contains: Handles the initialisation of the plan state to a new task.
;;; Author: Richard Kirby (rbk)
;;; Created: Thu Jul 12 11:02:41 1990
;;; Updated: Sun May  9 02:17:05 1999 by Jeff Dalton
;;; Copyright: (c) 1992, 1996, AIAI, University of Edinburgh

(in-package :oplan-knowledge-source)

(defun KS-SET-TASK (ag)
  (let* ((task-name (first (ag-args ag)))
	 (task-schema-name (if (eq task-name :EMPTY)
			       task-name
			     (intern (string-upcase task-name)))))
    ;; Make sure there's a schema for this task.
    (unless (or (eq task-schema-name :EMPTY)
		(db-request :EXISTS-NAMED-SCHEMA task-schema-name))
      (invalid-command "There is no task named \"~A\"." task-name))

    ;; Set up an alternative so that if we run out of alternatives, the
    ;; planner gracefully gives up.
    (am-request :POST-NO-MORE-ALTERNATIVES)
    #+:undef
    (post-alternatives (make-agenda-entry
			 :body '(:NO-MORE-ALTERNATIVES)
			 :trigger t))

    ;; Post an :expand-task
    (post-agenda `(:EXPAND-TASK ,task-schema-name)
      :trigger t
      :level 0)
      
    ;; Post an agenda entry to tell the TA when the planner has finished,
    ;; so the trigger is :EMPTY.
    (post-agenda '(:PLANNER-FINISHED)
      :trigger '(:EMPTY))))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
