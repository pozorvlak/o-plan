;;;; File: levels.lisp
;;; Contains: Action / effect level algorithms and analysis
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: Thu Sep 30 15:49:01 1993 by Jeff Dalton
;;; Updated: Sun May  9 02:55:36 1999 by Jeff Dalton
;;; Copyright: (c) 1993, 1994, AIAI, University of Edinburgh

(in-package :oplan-tf-compiler)

;;; /\/: Always-effects and task effects both get level 0.
;;; /\/: What if we add a new task after we've got a plan?  Presumably
;;;      it can introduce effects at levels we thought were finished.
;;; /\/: We need a better way to collect lists of unique values.
;;;      "Dynamic sets"?  How much time are we spending in duplicate
;;;      elimination anyway?

;;; /\/: Check for node actions that can't be expanded and for
;;;      achieves (and other conds?) that can't be satisfied.


;;;; Entry point from the main compiler.

;;; *domain* should be the domain to check.
;;; All new schemas, etc, should already be in place.

(define-mapping action-name level)
(define-mapping effect-name level)

(defun process-domain-levels (domain)
  (assert (eq *domain* domain))
  (tf-progress "Level analysis~%")
  (tf-progress "Building level tables~%")
  (multiple-value-bind (action-levels effect-levels components)
      (build-level-tables domain)
    (setf (mapping action-name level) action-levels
	  (mapping effect-name level) effect-levels)
    (tf-progress "Checking condition levels~%")
    (check-condition-levels domain action-levels effect-levels components)))


;;;; Procedures for looking up levels

(defun action-level (action-name)
  (or (gethash action-name (mapping action-name level))
      (error "No level for action ~S." action-name)))

(defun effect-level (effect-name)
  (or (gethash effect-name (mapping effect-name level))
      (error "no level for effect ~S." effect-name)))

(defmessage (:dm :get-domain-levels) ()
  (list (hash-table-alist (mapping action-name level))
	(hash-table-alist (mapping effect-name level))))


;;;; And finally: levels

(defun build-level-tables (domain)
  (let* ((*domain* domain)
	 (root-actions (domain-root-actions domain))
	 (action-successor-table (build-action-successor-table domain))
	 (all-actions (hash-table-keys action-successor-table))
	 (unreachable-actions
	  (find-unreachable-actions
	   root-actions all-actions action-successor-table)))
    ;; All-actions will include any actions that are not reachable from
    ;; the root actions (ie, from the tasks).  These "unreachable"
    ;; actions are made reachable when building the component graph
    ;; by making them the successors of an invented root action.
    (when unreachable-actions
      (tf-warning "Domain \"~A\" has unreachable actions:~%   ~S."
		  (domain-name domain)
		  unreachable-actions)
      (push '|task for unreachable actions| root-actions)
      (setf (gethash '|task for unreachable actions| action-successor-table)
	    unreachable-actions))
    (multiple-value-bind
	  (components
	   component-successor-table
	   action-to-component-table)
	(build-component-graph root-actions
			       (hash-table->function action-successor-table))
      (let* ((action-levels
	      (build-action-level-table
	        domain
		root-actions
		all-actions
		action-to-component-table
		component-successor-table))
	     (effect-levels
	      (build-effect-level-table
	        domain
		action-levels)))
	(values
	  action-levels
	  effect-levels
	  components)))))		;n.b. action names, not effect names

(defun domain-root-actions (domain)
  (mapcar #'schema-action-name (domain-task-schemas domain)))

(defun domain-task-schemas (domain)
  (remove-if-not #'task-schema-p (domain-schemas domain)))

(defun find-unreachable-actions (roots all successor-table)
  (let ((reachable
	 (dfs-finish-order roots (hash-table->function successor-table))))
    (set-difference all reachable :test #'eq)))  

#+:undef
(defun successor-table-minimals (table)
  (let ((candidates (hash-table-keys table)))
    (dolist (successor-set (hash-table-values table))
      (dolist (s successor-set)
	(setq candidates (delete-1-eq s candidates))))
    candidates))

;;; We want to define a mapping from action names to successor action
;;; names.  We already have:
;;;   action name -> schemas        a "basic schema table"
;;;   schema -> successor-schemas   the schema successor table
;;;   schema -> action name         using the schema-action-name slot accessor

(defun build-action-successor-table (domain)
  (declare (ignore domain))
  ;; N.B. all action names of all schemas must be in the table,
  ;; even when an action name has no successors.
  (compose-maps
    (hash-table-keys (mapping action-name schemas)) ;all action names
    (mapping action-name schemas)
    #'(lambda (s)
	(mapcar #'schema-action-name
		(gethash s (mapping schema successor-schemas))))))

;;; Action levels

(defun build-action-level-table (domain
				 root-actions
				 all-actions
				 action-to-component-table
				 component-successor-table)
  (let* ((component-level-table
	  (find-longest-path-lengths*
	   (mapcar (hash-table->function action-to-component-table)
		   root-actions)
	   (hash-table->function component-successor-table)))
	 (action-level-table
	  (make-hash-table :test #'eq)))
    ;; We now have levels for the components, but not for the actions
    ;; in the components.  So we have to get the action levels.
    (let ((unreachable-actions '()))
      (dolist (action all-actions)
	(let ((component (gethash action action-to-component-table)))
	  (when (null component)
	    (push action unreachable-actions))
	  ;; unreachable actions get level 0,
	  ;; others get their component's level.
	  (setf (gethash action action-level-table)
		(gethash component component-level-table 0))))
      (when unreachable-actions
	;; Because of our earlier cleverness, it is like totally fatal
	;; to still have actions that look unreachable.
	(error "Domain \"~A\" still has unreachable actions:%   ~S."
	       (domain-name domain)
	       unreachable-actions)))
    ;; Remove the invented action that has the unreachable actions
    ;; as its successors.
    (remhash '|task for unreachable actions| action-level-table)
    action-level-table))

;;; The level of an effect name E is max of the levels of all action
;;; names A of schemas that have an effect named E.  If there is no such
;;; A (ie, E is not an effect of any schema), the level of E is 0.

;;; /\/: We could construct a (mapping action-name direct-effect-names)
;;; and use it here instead of the nested loop below.  But we don't,
;;; partly because it's more efficient not to, and partly for historical
;;; reasons.  However, such a mapping is constructed by report-domain-levels
;;; when *level-report-include-action-effects* is true.

(defun build-effect-level-table (domain action-level-table)
  (let ((effects (get-domain-effect-names domain))
	(actions (hash-table-keys action-level-table))
	(table (make-hash-table :test #'eq)))
    (macrolet ((effect-level (e) `(gethash ,e table))
	       (action-level (a) `(gethash ,a action-level-table)))
      ;; Make sure all effect names are in the table.
      (dolist (e effects)
	(setf (effect-level e) 0))
      ;; For each action name A, go through all effect names from schemas
      ;; that have action name A.
      (dolist (a actions)
	(let ((level (action-level a)))
	  (dolist (s (gethash a (mapping action-name schemas)))
	    (dolist (e (gethash s (mapping schema effect-names)))
	      (when (< (effect-level e) level)
	        (setf (effect-level e) level))))))
      table)))

;;; get-domain-effect-names tries to find all the effect names.
;;; It looks at (domain-initially domain) because the TF file might
;;; be for something like a Web demo where some tasks are defined
;;; interactively rather than being in the file.  So "intially"
;;; may list some effects that do not appear as effects in the schemas.
;;; However, in these interactive cases, this may still not be all
;;; the effects, so in check-condition-levels we assign level 0
;;; to any effect-without-a-level mentioned in a condition, after
;;; issuing a warning.  That way, the effect will have the right
;;; level for an effect that appears only in tasks, but the user
;;; will still get a warning in case it's a spelling or other mistake.

(defun get-domain-effect-names (domain)
  (let ((effects '()))
    (dolist (p-v (domain-always domain))
      (nconcf-new effects (car (pv-pattern p-v)) :test #'eq))
    (dolist (e (domain-initially domain))
      (nconcf-new effects (car (effect-pattern e)) :test #'eq))
    (dolist (s (domain-schemas domain))
      (dolist (e (schema->effect-names s))
	(nconcf-new effects e :test #'eq)))
    effects))

;;; We'll also want to check condition levels.

(defun check-condition-levels (domain action-levels effect-levels components)
  (let ((level-errors 0))
    (dolist (schema (domain-schemas domain)) ;includes tasks
      (let ((schema-level
	     (gethash (schema-action-name schema) action-levels)))
	;; Schema-level is the level of an action.  Certain condition types
	;; restrict how the effects that could satisfy them can differ in
	;; level from the action.
	(assert (integerp schema-level))
	(dolist (c (schema-conditions schema))
	  (unless (eq (con-type c) 'compute)
	    ;; Look at the effect that corresponds to the condition
	    (let* ((effect-name (car (con-pattern c)))
		   (effect-level (gethash effect-name effect-levels)))
	      (when (null effect-level)
		(tf-warning "No level for effect \"~A\"." effect-name)
		;; Now pretend it's a task-only effect
		(setf (gethash effect-name effect-levels) 0)
		(setq effect-level 0))
	      (unless (effect-level-ok-for-condition-p
		        (con-type c) effect-level schema-level)
		;; Level mismatch, a.k.a. hierarchical promiscuity.
		(incf level-errors)
		(condition-level-error
		 schema c effect-level schema-level)))))))
    ;; If there are level errors, or if the user wants to know anyway,
    ;; produce a level description.
    (when (> level-errors 0)
      (tf-note "~D level error~:P.  Level assignments follow."
	       level-errors))
    (when (or (> level-errors 0)
	      *level-report*)
      (report-domain-levels domain action-levels effect-levels components))))

(defun condition-level-error (schema condition effect-level action-level)
  (tf-warning				;was tf-error
   "Level mismatch in schema \"~A\",~%~
      ~6T for condition \"~S {~S ...}\".~%~
      ~6T The level of effect \"~S\" is ~D,~%~
      ~6T while that of action \"~S\" is ~D,~%~
      ~6T but the effect should not be at ~A level."
     (schema-name schema)
     (con-type condition)
     (car (con-pattern condition))
     (car (con-pattern condition))
     effect-level
     (schema-action-name schema)
     action-level
     (ecase (signum (- effect-level action-level))
       (-1 "a higher (lower numbered)" )
       ( 0 "the same"                  )
       ( 1 "a lower (higher numbered)" ))))

(defun effect-level-ok-for-condition-p (cond-type effect-level action-level)
  (ecase cond-type
    ((only_use_if only_use_for_query)
     (<= effect-level action-level))
    ((supervised)
     (>= effect-level action-level))
    ((unsupervised)
     ;; /\/: AAAI 94 paper says: same or higher if cond is sat externally
     ;; to the sub-actions of the schema or same or lower if sat by the
     ;; sub-actions.  [I suppose we could see whether only the subactions
     ;; _could_ satisfy it.  If not, I don't know what check we could make.]
     ; (>= effect-level action-level)
     t)
    ((achieve
      achievable)			;/\/ for now
     ;; Achieve for action A will force the actions for schemas with
     ;; corresponding oufe conditions to be >= level(A).  Therefore, the
     ;; effects that go with the schemas will also have level >= level(A).
     ;; /\/: However, when there's no oufe, there may still be an ordinary
     ;; effect at a higher (lower-numbered) level.  Since we allow such
     ;; effects to satisfy achieves, we should probably treat them as
     ;; oufe effects when calculating levels, but we don't.  Instead,
     ;; we treat this (rare) case as a level error.
     (>= effect-level action-level))))

;; /\/: The mapping is constructed if needed, below; but see the "/\/"
;; before build-effect-level-table.
(define-mapping action-name direct-effect-names)

(defun report-domain-levels (domain action-levels effect-levels components)
  (tf-note "")
  (tf-note "Levels for domain \"~A\"" (domain-name domain))
  (tf-note "")
  (report-domain-components components)
  (tf-note "")
  (when *level-report-include-action-effects*
    (tf-note "The possible effects of an action will be shown, indented")
    (tf-note "under the action name, in the action-level table.")
    (tf-note "")
    (setf (mapping action-name direct-effect-names)
	  (compose-maps
	    (hash-table-keys (mapping action-name schemas)) ;all action names
	    (mapping action-name schemas)
	    (mapping schema effect-names))))
  (report-level-assignments :action action-levels)
  (tf-note "")
  (report-level-assignments :effect effect-levels)
  (tf-note "")
  (values))

(defun report-domain-components (components)
  (let ((interesting
	 (remove-if-not #'(lambda (c) (not (null (cdr c)))) ; len > 1
			components)))
    (tf-note "There are ~D strongly connected components in the~%~
              action graph that have length greater than one."
	     (length interesting))
    (when interesting
      (tf-note "They are:")
      (dolist (component interesting)
	(tf-note "~3T~S" component)))))

(defun report-level-assignments (type level-table)
  (tf-note "~@(~A~) levels:" type)
  (let* ((items
	  (hash-table-keys level-table))
	 (levels
	  (group-by-numeric-key
	    items
	    (hash-table->function level-table))))
    (loop for level-members in levels
	  for level from 0
	  when level-members
       do (tf-note "~4D ~S" level (car level-members))
          (loop for item in (cdr level-members)
	     do (report-level-item type item)))))

(defun report-level-item (type item)
  (tf-note "     ~S" item)
  (when (and (eq type :action) *level-report-include-action-effects*)
    (let ((action-effects (action-name->direct-effect-names item)))
      (when action-effects
	(loop for effect in action-effects
	   do (tf-note "          ~S" effect))))))
  
;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
