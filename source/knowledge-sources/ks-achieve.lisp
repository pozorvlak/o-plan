;;;; File: KS-ACHIEVE.lisp
;;; Contains: Achieve a condition KS.
;;; Author: Richard Kirby (rbk)
;;; Created: Wed Sep 26 11:02:42 1990
;;; Updated: Fri Jun  4 01:19:27 1999 by Jeff Dalton
;;; Copyright: (c) 1992, 1995, 1996, AIAI, University of Edinburgh


(in-package :oplan-knowledge-source)

(defun achieve-filter (schema bindings)
  (let ((cond (cadr (ag-body *ag*))))
    (filter-on-only-use-ifs schema bindings (con-at-node-end cond))))

;; KS-ACHIEVE handles conditions of type achievable. The agenda table entry
;; that causes this KS to run is posted by KS-SET-TASK if the initial task
;; schema specifies achievable conditions, or KS-ACHIEVE itself when bringing
;; in a new node, or KS-EXPAND.
(defun KS-ACHIEVE (ag &aux (*ag* ag))
  (STAGE-MANAGER
   (schema-list schema use-expansion-option-p contributors-to-ignore
		not-again-p)
   (interact cond waiting-schema-list added-node bindings
	     cond-node-end after-point)
   ag
   
   ;;; The first stage.
   ;; See if condition is satisfyable with the existing nodes.
   (setq cond (cadr (ag-body ag)))
   (unless use-expansion-option-p
     (whats-going-on "Checking to see if~%~S~%is already satisfied." cond)
     (setq interact (try-adding-condition cond nil contributors-to-ignore))
     (when (some-work-to-do-p (cadr interact))
       ;; Because we are going to do some work to satisfy this achievable,
       ;; post an alternative to achieve this condition by expansion.
       (setf (ag-info ag) (list schema-list schema :EXPAND))
       (post-alternatives ag))
     (when (handle-adding-condition-interaction cond interact)
       (whats-going-on "It is")
       (return-from KS-ACHIEVE nil))
     (whats-going-on "No way to satisfy cond, so bring in a node to do it."))
   
   ;; The second stage.
   ;; NEED TO ADD A NODE TO SATISFY THE CONDITION.
   (STAGE (if-want-real-time-response-p))
   ;; Find all the schemas that have only_use_for_effects entries that satisfy
   ;; the required condition.
   (setq cond (second (cadr (ag-body ag))))
   (cond (schema-list
	  (whats-going-on "Already have a schema list."))
	 (t
	  (dev-debug :information "Asking DM for schemas for ~S~%" cond)
	  (setq schema-list (get-effect-schemas
			     (list cond (third (cadr (ag-body ag))))))
	  (assert (listp schema-list))
	  (dev-debug :information "Got ~A back.~%"
		     (mapcar #'(lambda (x) (schema-name (car x)))
			     schema-list))))
   (when (null schema-list)
     (dev-debug :information "No schemas found for effect ~A~%" cond)
     (post-agenda '(:POISON-STATE :NO-SCHEMA :INITIALLY))
     (return-from KS-ACHIEVE nil))
   
   ;; Apply filtering rules to each schema.
   ;;   (setq schema-list (mapcan #'achieve-filters schema-list))
   (multiple-value-setq (schema-list waiting-schema-list)
     (filter-schemas schema-list #'achieve-filter))
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
	    ;; Force reconsideration of oui
	    ;; conds.
	    :stage 1
	    :body (ag-body ag)
	    ;; Make sure we don't ask again for
	    ;; a list of schemas.
	    :info (list (list (car schema))
			nil nil nil t)))))
   (if (null schema-list)
       (progn
	 (dev-debug :information "Filtered out all schemas for ~A~%"
		    (cadr (ag-body ag)))
	 (post-agenda '(:POISON-STATE :NO-SCHEMA :AFTER-FILTERING))
	 (return-from KS-ACHIEVE nil)))
   
   (STAGE (if-want-real-time-response-p))
   ;; The third stage.
   ;; If more than one schema, pick one, and send the rest to the AM as
   ;; alternatives.

   ;; /\/: Here's where we do the magic that allows us to ask the user
   ;; which schema to use when we're not running interactively.  This
   ;; pretty much follows ks-expand (which was done first).

   (cond ((list-beginning :answer-to-question schema-list)
	  ;; We've already sorted the schemas, and the user has picked
	  ;; which one should be tried first.  This will be handled
	  ;; after this cond.
	  (pop schema-list))

	 ((list-beginning :question schema-list)
	  ;; Shouldn't happen
	  (error "KS-ACHIEVE question-answering went wrong."))

	 ((and (length>1 schema-list)
	       (eq *schema-selection-mode* :ask)
	       (or (not (get-parameter :interactive))
		   (get-parameter :test-ta-questions)))
	  ;; We want to suspend this ks-achieve, ask the user which schema,
	  ;; and pick up at the stage boundary above.

	  ;; Sort the schema list.
	  (setq schema-list (standard-schema-sort schema-list))

	  ;; Record that we're asking a question
	  (push :question schema-list)

	  ;; Pretend we staged at the stage-boundary above, but without
	  ;; telling the AM to put us back on the agenda (cf stage-agenda).
	  (setf (ag-stage ag) 2)
	  (setf (ag-info ag) (list schema-list schema use-expansion-option-p
				   contributors-to-ignore not-again-p))
	  ;; Say that we want to ask the TA a question
	  (ipc-send :am :question :schema-order
		    ag
		    (encode-schema-order-question schema-list))
	  
	  ;; And we're done for now.
	  (return-from ks-achieve))

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
       (setf (ag-stage ag) 1)
       (post-alternatives ag)))

   ;; Grab the (schema . bindings) we're going to go with.
   (setq schema (car schema-list))
   ;; To save saving it if we should stage.
   (setq schema-list nil)
   
   (STAGE (if-want-real-time-response-p))
   ;; The fourth and final stage. Basically adds in a node with the schema
   ;; pattern, linking it in before the condition node-end that requires the
   ;; effect. Then posts an EXPAND for the node. If the schema has multiple
   ;; nodes then add a node, and then use the nodes as an expansion of the
   ;; parent node.
   (handler-case (setq schema (instantiate-schema schema))
     (posted-poison ()
       (return-from KS-ACHIEVE nil)))
   (setq bindings (cdr schema))
   (setq schema (car schema))
   (setq cond (cadr (ag-body ag)))
   (setq cond-node-end (con-at-node-end cond))

   ;; /\/: The after-point is a etag, but we're going to make the
   ;; after-point variable be nil or a list containing the etag,
   ;; because :add-node automatically adds a link after node-1.
   (setq after-point (db-call 'get-achieve-after-point cond))
   (if (and (eq (etag-node after-point) :start)
	    (eq (etag-end after-point) :end))
       ;; Don't bother to link after end_of node-1
       (setq after-point nil)
     ;; otherwise, make a :pre-list for :add-node
     (setq after-point (list after-point)))
   (assert (listp after-point))

   ;; Add in a node before the condition. If there is a node list in this
   ;; schema, then add the node as a dummy, then expand. If there isn't a node
   ;; list, then if there is an expands, add the node as an action with the
   ;; expands pattern, otherwise add the node as a dummy.
   (if (schema-nodes schema)
       (if ; (= (length (schema-nodes schema)) 1)
	   (not (schema-may-add-more-than-one-node-p schema))
	   (setq added-node
		 (db-request :ADD-NODE
		   :type (node-type (car (schema-nodes schema)))
		   :pattern (node-pattern (car (schema-nodes schema)))
		   :reason `(:achieve . ,(cdr cond))
		   :parent (db-call 'get-achieve-parent cond)
		   :pre-list after-point
		   :post-list (list cond-node-end)))
	   (setq added-node
		 (db-request :ADD-NODE
		   :type 'dummy
		   :pattern nil
		   :reason `(:achieve . ,(cdr cond))
		   :parent (db-call 'get-achieve-parent cond)
		   :pre-list after-point
		   :post-list (list cond-node-end))))
       (if (schema-expands schema)
	   (setq added-node
		 (db-request :ADD-NODE
		   :type 'action
		   :pattern (schema-expands schema)
		   :reason `(:achieve . ,(cdr cond))
		   :parent (db-call 'get-achieve-parent cond)
		   :pre-list after-point
		   :post-list (list cond-node-end)))
	   (setq added-node
		 (db-request :ADD-NODE
		   :type 'dummy
		   :pattern nil
		   :reason `(:achieve . ,(cdr cond))
		   :parent (db-call 'get-achieve-parent cond)
		   :pre-list after-point
		   :post-list (list cond-node-end)))))
   (unless added-node
     (dev-debug :information "Could not add satisfying node.")
     (post-agenda '(:POISON-STATE :COULD-NOT-ADD-NODE))
     (return-from KS-ACHIEVE nil))
   (whats-going-on "Adding a new Node ~S to the Plan." added-node)
   (db-call 'psv-set-current-source-node added-node) ;inform PSVs
   (if (or (schema-may-add-more-than-one-node-p schema)
	   (null (schema-nodes schema))) ;/\/ 30 May 99
       (progn
	 ;; Expand the just added node.
	 (whats-going-on "Using ~S as schema for EXPANDING ~S"
			 (schema-name schema) added-node)
	 (unless (expand-from-schema added-node schema)
	   (dev-debug :information "Error in expanding ~A~%" added-node)
	   (post-agenda '(:POISON-STATE :NO-EXPAND))
	   (return-from KS-ACHIEVE nil))
	 ;; Handle all the effects etc.
	 (unless (process-schema schema added-node t)
	   (return-from KS-ACHIEVE nil)))
       
       ;; Add the effects of the schema, handle the conditions, and then post
       ;; an expand for the node.
       (progn
	 (unless (process-schema schema added-node nil)
	   (return-from KS-ACHIEVE nil))
	 ;; Post an EXPAND for the node, with its conditions as triggers.
	 (unless (null (schema-nodes schema))
	   (post-expand `(:EXPAND ,added-node ,(node-pattern
						(car (schema-nodes schema))))
	       :trigger (cons :and (get-conditions-for-triggering
				    (schema-conditions schema)))))))
   
   ;; Post a BIND for any PSV mentioned.
   (if (listp bindings)
       (dolist (var bindings)
	 (when (psvar-p (var-value var))
	   (post-bind `(:BIND ,(var-value var))))))
   
   ;; Well since we have added a node to satisfy the condition, lets add the
   ;; condition to the GOST.
   ;;   (setf (con-type cond) 'ACHIEVABLE)
   (whats-going-on "Should be able to satisfy~%~S~%now." cond)
   (setq interact (try-adding-condition cond nil contributors-to-ignore))
   ;; interact will always be (:SATISFY ((<or record>))) where the <or record>
   ;; gives the added node as the contributor, and may or may not include
   ;; information on removing any possible deletors.
   (set-update-contributor-satisfaction-method
     (tgm-satisfy-qa-result interact)
     :EXPAND)
   (unless (handle-adding-condition-interaction cond interact)
     (whats-going-on "We brought in a Node to satisfy the cond, but still ~
                      we can't - so POISON PLAN.")
     (post-agenda `(:POISON-STATE :COULD-NOT-SATISFY-THE-CONDITION ,cond))
     (return-from KS-ACHIEVE nil))
   (whats-going-on "Indeed we can.")))


;; Traverses or-tree replacing all the satisfaction methods of :UPDATE actions
;; with method. DESTRUCTIVE.
;;
(defun set-update-contributor-satisfaction-method (or-tree method)
  (check-type or-tree or-tree)
  (dolist (b (or-tree-branches or-tree))
    (let ((update-action (assoc :UPDATE (or-branch-actions b))))
      (if update-action
	  (mapc #'(lambda (x)
		    (setf (cdr x) method))
		(cadr update-action))))
    (let ((subtree (or-branch-subtree b)))
      (when subtree
	(set-update-contributor-satisfaction-method subtree method)))))


;;; Support for the :schema-order question

(defun ks-achieve-accept-answer (ae question-kwd data)
  (ks-expand-accept-answer ae question-kwd data))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
