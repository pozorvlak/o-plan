;;;; File: KS-FIX.lisp
;;; Contains: KS that tries to fix an execution failure
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: Wed 22 February 1995
;;; Updated: Sun Sep  5 00:28:10 1999 by Jeff Dalton
;;; Copyright: (c) 1995, AIAI, University of Edinburgh

(in-package :oplan-knowledge-source)

;;; /\/: It should be possible to tell KS-USER about new modes.

;;; /\/: :Wait-on-effect triggers may have to consider :FIX agenda entries.

;;; KS-FIX is scheduled when an execution failure has eliminated
;;; the last contributor in a GOST entry.  So there's a condition
;;; to be satisfied.  The GOST entry is removed and a :FIX entry
;;; added to the agenda.  It's up to KS-FIX to try to satisfy the
;;; condition in one way or another.
;;;
;;; KS-FIX is basically a rational reconstruction of KS-ACHIEVE that
;;; is also able to handle an after-point.  Eventually, ACHIEVE should
;;; handle after-points and both KSes should be rewritten.  /\/
;;;
;;; The after-point given to KS-FIX is a node-end tag.  E.g (node-3 :end).
;;;
;;; Note that since the GOST entry is gone, we can "fix" the problem
;;; by doing nothing.  The result is probably not a plan that could
;;; have been produced from the TF for the current domain, and it
;;; won't (for instance) pass the sanity-checker, because the failed
;;; effects will no longer be in the TOME.
;;;
;;; This "do nothing" option is available to users, because they may
;;; be investigating some other aspect of the Planner/Exec/World
;;; system and not care about making proper fixes to the plan.  Note
;;; too that in some domain descriptions (TF) there aren't any schemas
;;; that could fix the plan, yet we may still want to execute plans in
;;; that domain and cause some actions to fail, perhaps just to see
;;; what KS-EXECUTION-FAILURE reports as being affected by the
;;; failure.
;;;
;;; KS-FIX can get as (ag-body ag):
;;;
;;; :FIX :FIX gost-entry after-point
;;;
;;;    Initial entry.  Tries first to satisfy the condition without
;;;    adding a node (action) to the plan.
;;;
;;; :FIX :EXPAND gost-entry after-point
;;;
;;;    The entry used by alternatives.  Tries to satisfy the condition
;;;    by adding a node.  Uses the ag-info slot as a p-list.
;;;
;;; Possible ag-info properties are:
;;;
;;; SCHEMA-LIST list-of-schemas
;;;
;;; NOT-AGAIN-P true-or-false
;;;

(definit :kp *fix-mode* :ask)		;one of :ask, :auto

(definit :kp *fix-default-action* :fix)	;one of nil, :fix

(defun KS-FIX (ag &aux (*ag* ag))
  (let* ((body (ag-body ag))
	 (args (cddr body)))
    (assert (eq (car body) :FIX))
    ;; Deal with user options.
    (ecase *fix-mode*
      (:ask
       (loop
	 (ecase (apply #'ks-fix-menu-choice args)
	   (:achieve (return))
	   (:nothing (return-from KS-FIX))
	   (:flip-mode
	    (flip-mode '*fix-mode*)
	    (when (eq *fix-mode* :auto)
	      (setq *fix-default-action*
		    (ks-fix-auto-menu-choice))))
	   (:break
	    (break "KS-FIX")))))
      (:auto
       (when (null *fix-default-action*)
	 (return-from KS-FIX))))
    ;; Try to satisfy the condition as if we were KS-ACHIEVE.
    (apply (ecase (cadr body)
	     (:FIX       'fix-any-way-possible)
	     (:EXPAND    'fix-by-expansion))
	   ag
	   args)))

(defun ks-fix-menu-choice (gost-entry after-point)
  (let ((*print-pretty* nil)
	(*print-length* nil)
	(*print-level* nil))
    (menu-request
      `("-heading" 
	,(format nil "Fix ~(~A after ~A~)" gost-entry after-point)
	"Fix by achieve=:achieve"
	"Fix by doing nothing=:nothing"
	,(format nil "Set fix mode to: ~(~A~)=:flip-mode"
	             (opposite-mode *fix-mode*))
	"-line"
	"Break in=:break"))))

(defun ks-fix-auto-menu-choice ()
  (menu-request
    '("-heading" "What should be done automatically?"
      "Fix by achieve=:fix"
      "Fix by doing nothing=nil")))


;;; Initial entry

(defun fix-any-way-possible (ag gost-entry after-point)
  ;; [Similar to the 1st stage of an achieve.]
  ;; See if condition is satisfyable with the existing nodes.
  (let* ((cond (make-cond-from-gost gost-entry))
	 (interact (try-adding-condition cond nil)))
     (when (some-work-to-do-p (cadr interact))
       ;; Because we are going to do some work to satisfy this achievable,
       ;; post an alternative to achieve this condition by expansion.
       (with-ag-copy (alt-ag ag)
	 (setf (ag-body alt-ag) `(:fix :expand ,gost-entry ,after-point))
	 (post-alternatives alt-ag)))
     (if (handle-adding-condition-interaction cond interact)
	 (dev-debug :detail "Already satisfied")
       (fix-by-expansion ag gost-entry after-point))))


;;; Step 2 and entry from alternatives

(defun fix-by-expansion (ag gost-entry after-point)
  ;; [Similar to the 2nd stage of an achieve.]

  ;; First check whether we've got an answer to a :question.
  (let ((schema-list (getf (ag-info ag) 'schema-list)))
    (when (list-beginning :answer-to-question schema-list)
      (return-from fix-by-expansion
	(fix-pick-schema ag gost-entry after-point schema-list))))

  ;; Need to add a node to satisfy the condition.
  (let ((cond-pattern (tgm-gost-pattern gost-entry)))

    ;; Find all the schemas that have only_use_for_effects entries that satisfy
    ;; the required condition.
    (let ((schema-list
	   (or (getf (ag-info ag) 'schema-list)
	       (get-effect-schemas
		 (list cond-pattern (tgm-gost-value gost-entry)))))
	  (waiting-schema-list '())
	  (not-again-p (getf (ag-info ag) 'not-again-p)))
      (assert (listp schema-list))
      (when (null schema-list)
	(post-agenda '(:POISON-STATE :NO-SCHEMA :INITIALLY))
	(return-from fix-by-expansion nil))
   
      ;; Apply filtering rules to each schema.
      (multiple-value-setq (schema-list waiting-schema-list)
	(filter-schemas schema-list #'fix-filter))

      ;; If we have some schemas that must wait for other agenda
      ;; entries to be processed before proceeding, then post
      ;; alternatives for each such schema and a trigger to wait.
      (unless (and (null schema-list) not-again-p)
	;; Only try this once as long as there is not a change to the
	;; waiting-schema-list.
	(dolist (schema waiting-schema-list)
	  (post-alternatives
	    (make-agenda-entry
	      :id (ag-id ag)
	      :trigger (cdr schema)
	      ;; Force reconsideration of oui conds.
	      :body `(:fix :expand ,gost-entry ,after-point)
	      ;; Make sure we don't ask again for
	      ;; a list of schemas.
	      :info (list 'schema-list (list (car schema))
			  'not-again-p t)))))
      (if (null schema-list)
	  (post-agenda '(:POISON-STATE :NO-SCHEMA :AFTER-FILTERING))
	(fix-pick-schema ag gost-entry after-point schema-list)))))
   
(defun fix-filter (schema bindings)
  (let ((gost (third (ag-body *ag*))))
    (filter-on-only-use-ifs
       schema
       bindings
       (tgm-gost-node-end gost))))

(defun fix-pick-schema (ag gost-entry after-point schema-list)
  ;; [Similar to the 3nd stage of an achieve.]

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
	 (error "KS-FIX question-answering went wrong."))

	((and ; for now we'll always ask ... (length>1 schema-list)
	      (eq *schema-selection-mode* :ask)
	      (or (not (get-parameter :interactive))
		  (get-parameter :test-ta-questions)))
	 ;; We want to suspend this ks-achieve, ask the user which schema,
	 ;; and pick up at the stage boundary above.

	 ;; Sort the schema list.
	 (setq schema-list (standard-schema-sort schema-list))

	 ;; Record that we're asking a question
	 (push :question schema-list)

	 ;; Set up the agenda entry to get us back to the :answer-to-question,
	 ;; already-sorted schemas case.
	 (setf (ag-body ag) `(:fix :expand ,gost-entry ,after-point))
	 (setf (ag-info ag) (list 'schema-list schema-list))

	 ;; Say that we want to ask the TA a question
	 (ipc-send :am :question :schema-order
		   ag
		   (encode-schema-order-question schema-list))
	  
	 ;; And we're done for now.
	 (return-from fix-pick-schema))

	((length>1 schema-list)
	 ;; This is the ordinary case: more than one applicable schema.
	 (setq schema-list (sort-schema-alternatives schema-list))))

  ;; We now have schema-list with entries in the right order.
  (when (> (length schema-list) 1)
    ; (setq schema-list (sort-schema-alternatives schema-list))
    (with-ag-copy (ag ag)
      (setf (ag-body ag) `(:fix :expand ,gost-entry ,after-point))
      (setf (ag-info ag) (list 'schema-list (cdr schema-list)))
      ; (setf (ag-branch-1 ag) (length (cdr schema-list)))
      (post-alternatives ag)))
  (fix-add-node ag gost-entry after-point (car schema-list)))

(defun fix-add-node (ag gost-entry after-point chosen-schema)
  (declare (ignore ag))
  ;; [Similar to the 4th and final stage of an achieve.]

  ;; The fourth and final stage. Basically adds in a node with the schema
  ;; pattern, linking it in before the condition node-end that requires the
  ;; effect. Then posts an EXPAND for the node. If the schema has multiple
  ;; nodes then add a node, and then use the nodes as an expansion of the
  ;; parent node.
  (let* ((inst (handler-case (instantiate-schema chosen-schema)
		 (posted-poison ()
		   (return-from fix-add-node nil))))
	 (schema (car inst))
	 (bindings (cdr inst))
	 (cond (make-cond-from-gost gost-entry))
	 (cond-node-end (con-at-node-end cond))
	 (added-node nil)
	 (interact nil))
   
    ;; Add in a node before the condition. If there is a node list in this
    ;; schema, then add the node as a dummy, then expand. If there isn't a node
    ;; list, then if there is an expands, add the node as an action with the
    ;; expands pattern, otherwise add the node as a dummy.
    ;; /\/: Should make this a separate procedure.
    (setq added-node
	  (if (schema-nodes schema)
	      (if (= (length (schema-nodes schema)) 1)
		  (db-request :ADD-NODE
		    :type (node-type (car (schema-nodes schema)))
		    :pattern (node-pattern (car (schema-nodes schema)))
		    :reason `(:achieve . ,(cdr cond))
		    :pre-list (list after-point)
		    :post-list (list cond-node-end))
		  (db-request :ADD-NODE
		    :type 'dummy
		    :pattern nil
		    :reason `(:achieve . ,(cdr cond))
		    :pre-list (list after-point)
		    :post-list (list cond-node-end)))
	      (if (schema-expands schema)
		  (db-request :ADD-NODE
		    :type 'action 
		    :pattern (schema-expands schema)
		    :reason `(:achieve . ,(cdr cond))
		    :pre-list (list after-point)
		    :post-list (list cond-node-end))
		  (db-request :ADD-NODE
		    :type 'dummy
		    :pattern nil
		    :reason `(:achieve . ,(cdr cond))
		    :pre-list (list after-point)
		    :post-list (list cond-node-end)))))
    (unless added-node
      (post-agenda '(:POISON-STATE :COULD-NOT-ADD-NODE))
      (return-from fix-add-node nil))
    
    (dev-debug :detail "Adding a new Node ~S to the Plan." added-node)

    (if (schema-may-add-more-than-one-node-p schema)
	(progn
	  ;; Expand the just added node.
	  (unless (expand-from-schema added-node schema)
	    (post-agenda '(:POISON-STATE :NO-EXPAND))
	    (return-from fix-add-node nil))
	  ;; Handle all the effects etc.
	  (unless (process-schema schema added-node t)
	    (return-from fix-add-node nil)))
       
	;; Add the effects of the schema, handle the conditions, and then post
	;; an expand for the node.
	(progn
	  (unless (process-schema schema added-node nil)
	    (return-from fix-add-node nil))
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

    (setq interact (try-adding-condition cond nil))
    ;; interact will always be (:SATISFY ((<or record>))) where the <or record>
    ;; gives the added node as the contributor, and may or may not include
    ;; information on removing any possible deletors.
    (set-update-contributor-satisfaction-method
     (tgm-satisfy-qa-result interact) :EXPAND)
    (unless (handle-adding-condition-interaction cond interact)
      (post-agenda `(:POISON-STATE :COULD-NOT-SATISFY-THE-CONDITION ,cond))
      (return-from fix-add-node nil))
    (whats-going-on "Fixed.")))


;;; Support for the :schema-order question

(defun ks-fix-accept-answer (ae question-kwd data)
  (ecase question-kwd
    (:schema-order
      (let ((schema-list (getf (ag-info ae) 'schema-list)))
	(assert (list-beginning :question schema-list))
	(setf (getf (ag-info ae) 'schema-list)
	      (cons :answer-to-question
		    (decode-schema-order-answer
		      (cdr schema-list)		;ie, w/o car = :question
		      data)))))))


;;;; Utilities

(defun make-cond-from-gost (gost-entry)
  (list 'achievable
	(tgm-gost-pattern gost-entry)
	(tgm-gost-value gost-entry)
	(tgm-gost-node-end gost-entry)))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
