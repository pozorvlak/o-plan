;;;; File: KS-EXPAND.lisp
;;; Contains: The EXPAND KS.
;;; Author: Richard Kirby (rbk)
;;; Created: Fri Jun 22 07:58:42 1990
;;; Updated: Sun May 30 22:45:58 1999 by Jeff Dalton
;;; Copyright: (c) 1992, 1995, 1996, AIAI, University of Edinburgh


(in-package :oplan-knowledge-source)

(defun get-expand-schemas (pattern)
  (ipc-with-own-copy
    (result (db-request :GET-EXPAND-SCHEMAS pattern)
	    (deep-copy-envs-and-schemas result))))

(defun expand-filter (schema bindings)
  (let ((expanding-node (cadr (ag-body *ag*))))
    (filter-on-only-use-ifs
       schema
       bindings
       (make-etag :node expanding-node
		  :end :BEGIN))))

(defun KS-EXPAND (ag &aux (*ag* ag))
  (STAGE-MANAGER
   (schema-list schema not-again-p)
   (node expansion-pattern waiting-schema-list)
   ag
   
   (setq node (cadr (ag-body ag)))
   (setq expansion-pattern (caddr (ag-body ag)))
   
   (when schema-list
     (whats-going-on "Already have a schema list."))
   
   ;; The first stage.
   (unless schema-list
     (setq schema-list (get-expand-schemas expansion-pattern))
     (assert (listp schema-list)))
   (when (null schema-list)
     (dev-debug :information "No schemas found matching ~A~%" node)
     (post-agenda '(:POISON-STATE :NO-SCHEMA :INITIALLY))
     (return-from KS-EXPAND nil))
   
   (STAGE (if-want-real-time-response-p))
   ;; The second stage.
   ;; Apply filtering rules to each schema.
   ;;   (setq schema-list (mapcan #'expand-filters schema-list))
   (multiple-value-setq (schema-list waiting-schema-list)
     (filter-schemas schema-list #'expand-filter))
   ;; If we have some schemas that must wait for other agenda entries to be
   ;; processed before proceeding, then post alternatives for each such schema
   ;; and a trigger to wait.
   (unless (and (null schema-list) not-again-p)
     ;; Only try this once as long as there is not a change to the
     ;; waiting-schema-list.
     (dolist (schema waiting-schema-list)
       (post-alternatives
	 (make-agenda-entry 
	    :id (ag-id ag)
	    :trigger (cdr schema)
	    :body (ag-body ag)
	    ;; Make sure that we don't ask again
	    ;; for a list of schemas.
	    :info (list (list (car schema)) nil :NOT-AGAIN)))))
   (when (null schema-list)
     (dev-debug :information "Filtered out all schemas for ~A~%"
		(cadr (ag-body ag)))
     (post-agenda '(:POISON-STATE :NO-SCHEMA :AFTER-FILTERING))
     (return-from KS-EXPAND nil))
   
   (STAGE (if-want-real-time-response-p))
   ;; The third stage.
   ;; If more than one schema, pick one, and send the rest to the AM as
   ;; alternatives.

   ;; /\/: Here's where we do the magic that allows us to ask the user
   ;; which schema to use when we're not running interactively.  

   (cond ((list-beginning :answer-to-question schema-list)
	  ;; We've already sorted the schemas, and the user has picked
	  ;; which one should be tried first.  This will be handled
	  ;; after this cond.
	  (pop schema-list))

	 ((list-beginning :question schema-list)
	  ;; Shouldn't happen
	  (error "KS-EXPAND question-answering went wrong."))

	 ((and (length>1 schema-list)
	       (eq *schema-selection-mode* :ask)
	       (or (not (get-parameter :interactive))
		   (get-parameter :test-ta-questions)))
	  ;; We want to suspend this ks-expand, ask the user which schema,
	  ;; and pick up at the stage boundary above.

	  ;; Sort the schema list.
	  (setq schema-list (standard-schema-sort schema-list))

	  ;; Record that we're asking a question
	  (push :question schema-list)

	  ;; Pretend we staged at the stage-boundary above, but without
	  ;; telling the AM to put us back on the agenda (cf stage-agenda).
	  (setf (ag-stage ag) 2)
	  (setf (ag-info ag) (list schema-list schema not-again-p))

	  ;; Say that we want to ask the TA a question
	  (ipc-send :am :question :schema-order
		    ag
		    (encode-schema-order-question schema-list))
	  
	  ;; And we're done for now.
	  (return-from ks-expand))

	 ((length>1 schema-list)
	  ;; This is the ordinary case.
	  ;; Do some debugging stuff, then sort the schemas.
	  (dev-debug :information "More than one applicable schema for ~A~%"
		     (cadr (ag-body ag)))
	  (whats-going-on "More than one applicable schema for ~A~%~
                           so post alternatives."
			  (cadr (ag-body ag)))
	  (setq schema-list (sort-schema-alternatives schema-list))))

   ;; We now have schema-list with entries in the right order.
   (when (length>1 schema-list)
     ;; Just use the first one, and save the rest.
     (with-ag-copy (ag ag)
       (setf (ag-info ag) (list (cdr schema-list)))
       ; (setf (ag-branch-1 ag) (length (cdr schema-list)))
       ;; Hack to force alternatives to be recognised.
       ;; /\/: The idea is for this agenda entry to take the KS back
       ;; to the earlier stage where it will consider the other schemas.
       ;; [jd 30 May 99]
       (setf (ag-stage ag) 0)
       (post-alternatives ag)))

   ;; Grab the (schema . bindings) we're going to go with.
   (setq schema (car schema-list))
   ;; To save saving it if we should stage.
   (setq schema-list nil)
   
   (STAGE (if-want-real-time-response-p))
   ;; The fourth stage. This is the final stage where the Plan State is
   ;; changed.
   ;; We now have a schema to try. First add in all the nodes, and then see
   ;; if any work is involved in adding the schema's conditions and effects.
   ;; If possible and easily satisfied then continue, otherwise need to do
   ;; some more work but how is the question!

   (setq node (cadr (ag-body ag)))

   (handler-case (instantiate-schema schema node)
     (posted-poison ()
       (return-from KS-EXPAND nil))
     (:no-error (inst)
       (setq schema inst)))
       
   ;; Expand the schema.
   (whats-going-on "Using ~S as schema for EXPANDING ~S" 
		   (schema-name (car schema)) node)
   (if (expand-from-schema node (car schema))
       (progn
	 (dev-debug :information "Expanded ~A OK.~%" node))
       (progn
	 (dev-debug :information "Error in expanding ~A~%" node)
	 (post-agenda '(:POISON-STATE :NO-EXPAND))
	 (return-from KS-EXPAND nil)))
   
   (unless (process-schema (car schema) node t)
     (return-from KS-EXPAND nil))
   
   ;; Now post BINDs for any PSV mentioned.
   (if (listp (cdr schema))
       (dolist (var (cdr schema))
	 (when (psvar-p (var-value var))
	   (post-bind `(:BIND ,(var-value var))))))))


;;; Support for the :schema-order question

(defun ks-expand-accept-answer (ae question-kwd data)
  (ecase question-kwd
    (:schema-order
      (let ((schema-list (car (ag-info ae))))
	(assert (list-beginning :question schema-list))
	(setf (car (ag-info ae))
	      (cons :answer-to-question
		    (decode-schema-order-answer
		      (cdr schema-list)		;ie, w/o car = :question
		      data)))))))

(defun encode-schema-order-question (data)
  ;; We have (schema . bindings) pairs and we want (schema-name . bindings).
  (assert (list-beginning :question data))
  (cons :question
	(mapcar #'(lambda (s+b)
		    (cons (schema-name (car s+b))
			  (cdr s+b)))
		(cdr data))))

(defun decode-schema-order-answer (schema-list data)
  ;; We get back a reordering of what we sent out, but we want the
  ;; schemas from our list, not the names we got back from the user.
  ;; Note that we can't just call find-schema-with-name, because
  ;; we want the copies that have been modified by earlier processing,
  ;; not a fresh instance.
  (mapcar #'(lambda (s+b)
	      (let ((schema-name (car s+b)))
		(assoc schema-name schema-list
		       :test #'equal
		       :key #'schema-name)))
	  data))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
