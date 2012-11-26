;;;; File: ks-expand-task.lsp
;;; Contains: Expands a task schema.
;;; Author: Richard Kirby and Jeff Dalton
;;; Created: Thu Jul 12 11:02:41 1990
;;; Updated: Sun May  9 02:17:22 1999 by Jeff Dalton
;;; Copyright: (c) 1992, 1996, AIAI, University of Edinburgh

(in-package :oplan-knowledge-source)

;;; The issue body must be (:EXPAND-TASK task-name [:TASK-ADDITION])

;;; The task name comes in as a lower-case string.

;;; :TASK-ADDITION indicates that this issue was posted by an :ADD-TO-TASK.

(defun KS-EXPAND-TASK (ag)
  (let ((task-name (first (ag-args ag)))
	(addition-p (eq (second (ag-args ag)) :task-addition)))
    (when (eq task-name :EMPTY)
      (return-from KS-EXPAND-TASK
	(expand-empty-task)))
    (let* ((schema (ipc-with-own-copy
		     ;; Because process-schema calls the destructive
		     ;; process-node-end.  /\/
		     (s (db-request :GET-NAMED-SCHEMA task-name)
		        (deep-copy-schema s))))
	   (schema-nodes (schema-nodes schema))
	   (node-list '())
	   (added-node 'not-a-node))

      (when (null schema)
	(dev-debug :fatal-error "Couldn't get schema for ~S~%" task-name)
	(return-from KS-EXPAND-TASK nil))

      ;; If we're adding a task, we must have already expanded a
      ;; task to add it to.
      (when addition-p
	;; Check a few things.
	(assert (am-request :get-option 'option-1))
	(assert (valid-etag-p (etag 'node-1 :begin)))
	(assert (valid-etag-p (etag 'node-2 :end)))
	;; Drop the start and finish nodes
	(setq schema-nodes
	      (remove-if #'(lambda (n) (member (node-type n) '(start finish)))
			 schema-nodes))
	;; But make sure we can refer to the start and finish nodes
	;; The TF compiler ensures that they are numbered 1 and 2,
	;; respectively, in task schemas, so we assume that here.
	(push (cons 1 'node-1) node-list)
	(push (cons 2 'node-2) node-list))

      ;; Now process the task schema.
      (dolist (node schema-nodes)
	(unless (setq added-node (db-request :ADD-INITIAL-NODE node))
	  (dev-debug :fatal-error "Couldn't add ~S~%" node)
	  (return-from KS-EXPAND-TASK nil))
	(push (cons (node-number node) added-node) node-list)
	(when (eq (node-type node) 'action)
	  (post-expand `(:EXPAND ,added-node ,(node-pattern node)))))

      (dolist (link (schema-orderings schema))
	(unless (db-request :ADD-LINK
	           (etag (lookup (ordering-node-number1 link) node-list)
			 (ordering-node-end1 link))
		   (etag (lookup (ordering-node-number2 link) node-list)
			 (ordering-node-end2 link)))
	  (dev-debug :fatal-error "Couldn't add link ~S~%" link)
	  (return-from KS-EXPAND-TASK nil)))

      (unless (process-schema schema 'NODE t)
	(return-from KS-EXPAND-TASK nil))

      (unless addition-p
        (am-request :make-option-1)))))

(defun expand-empty-task ()
  ;; Cannot be an addition
  (assert (not (am-request :get-option 'option-1)))
  (assert (not (valid-etag-p (etag 'node-1 :begin))))
  (assert (not (valid-etag-p (etag 'node-2 :end))))
  (let ((node-1
	 (db-request :add-initial-node
	   (node 1 'start)))
	(node-2
	 (db-request :add-initial-node
	   (node 2 'finish))))
    (if (and node-1
	     node-2
	     (db-request :add-link
			 (etag 'node-1 :end)
			 (etag 'node-2 :begin)))
	(am-request :make-option-1)
      (dev-debug :fatal-error "Couldn't expand empty task~%"))))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
