;;;; File: expand-support.lsp
;;; Contains: KS support for expansion
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: November 1996
;;; Updated: Sat Jun  5 17:45:44 1999 by Jeff Dalton
;;; Copyright: (c) 1996, AIAI, University of Edinburgh

(in-package :oplan-knowledge-source)

;; Returns a copy of a condition cond with the condition node-end replaced
;; with node-end.
(defun copy-cond-and-replace-cond-node-end (cond node-end)
  (let ((result (copy-list cond)))
    (setf (con-at-node-end result) node-end)
    result))

(defun check-only-use-ifs (cond bindings)
  ;; Makes a request on the DM for checking the applicability of a schema via
  ;; its only-use-if conditions, passing in the schema vars bindings. The
  ;; return result is either nil indicating a failure, or else an adjusted set
  ;; of bindings.
  (let ((result (db-request :EVAL-FILTER 'world-state cond bindings)))
    (when result
      (when (and (or-tree-p result)
		 (length=1 (or-tree-branches result))
		 (null (or-branch-subtree
			(car (or-tree-branches result)))))
	;; If there is only one result, then set any of the bindings
	;; (if this is an only_use_for_if).
	(when (eql (con-type cond) 'only_use_if)
	  (dolist (action (or-branch-actions
			   (car (or-tree-branches result))))
	    (when (and (eql (first action) :BIND)
		       (not (psvar-p (second action))))
	      (setf (second (assoc (second action) bindings))
		    (third action))))))
      t)))

(defun try-compute (cond bindings)
  ;; Returns true or false to indicate success or failure.
  ;; May also set the values of some variables in bindings.
  (let ((p (con-pattern cond))
	(v (con-value cond)))
    (if (eq (car p) :computed)
	;; Already checked it once, but now we've presumably backtracked
	;; and want to see if the schema still passes the filters.
	(progn
	  (assert (db-call 'fully-instantiable-p v bindings))
	  t)
      (let ((cm-result (db-request :eval-filter 'compute cond bindings)))
	(cond ((null cm-result)
	       nil)
	      ((eq cm-result 't)
	       t)
	      ((or-tree-p cm-result)
	       (setf (con-pattern cond) `(:computed ,(con-pattern cond)))
	       (maybe-set-values bindings cm-result)
	       t)
	      (t (error "Bogus try-compute result: ~S." cm-result)))))))

(defun maybe-set-values (old-binds or-tree)
  ;; /\/: This is not perfect, given that the binding lists may contain
  ;; unifications and PSV references, but it should work about as well
  ;; as our current handling of only_use_if and expands patterns.
  ;; The idea is that if an entry in new-binds has a value when the
  ;; corresponding entry in old-binds has value :undef, we can change
  ;; the entry in old-binds.
  (assert (length=1 (or-tree-branches or-tree)))
  (assert (every #'(lambda (b) (null (or-branch-subtree b)))
		 (or-tree-branches or-tree)))
  (macrolet ((v-name (tuple) `(first ,tuple)) 	;/\/ shouldn't need own
	     (v-val (tuple) `(second ,tuple))) 	;/\/ shouldn't need own
    (dolist (act (or-branch-actions (first (or-tree-branches or-tree))))
      (when (list-beginning :bind act)
	(let ((var (second act))
	      (val (third act)))
	  (assert (not (eq val :undef)))
	  (let ((old-tuple (assoc var old-binds)))
	    (when (and old-tuple
		       (eq (v-val old-tuple) :undef))
	      (setf (v-val old-tuple) val))))))))

(defun make-wait-for-effect-trigger (condition bindings)
  (declare (ignore bindings))
  ;; bindings may be used later on for instantiating the cond pattern which
  ;; would allow for a more specific trigger.
  (cons :WAIT-ON-EFFECT (con-pattern condition)))

(defun possibly-satisfying-plan-state-changes-p (condition bindings)
  ;; Maybe call it possibly-satisfying-effects-p /\/
  (let ((effect (con-pattern condition)))
    (db-request :CHECK-FOR-EFFECT effect bindings (ag-id *ag*))))

;;; (filter-on-only-use-ifs schema bindings psuedo-node-end) --
;;;
;;; Given the schema with the set of bindings, checks all the
;;; only_use_ifs and only_use_for_querys.  For each such condition, if
;;; it can be satisfied there and then, then carry on, otherwise if
;;; can't satisfy, check if there is an outstanding agenda entry that
;;; could possibly cause an effect to be added that could satisfy the
;;; condition.  If so then if this is an only_use_if set up a trigger
;;; to wait for the effect, or if it is an only_use_for_query carry
;;; on, otherwise if there is no such agenda-entry return nil.
;;;
;;; Returns one of:
;;;   nil - throw away the schema;
;;;   t   - keep schema;
;;;   <trigger> - keep schema as an alternative but with a trigger.

(defun filter-on-only-use-ifs (schema bindings psuedo-node-end)
  (let ((conditions (schema-conditions schema))
	trigger)
    (dolist (cond conditions)
      (case (con-type cond)
	((only_use_if only_use_for_query)
	 ;; Need to check if the condition is satisfiable, but we don't do
	 ;; any work to change the plan state since this schema may not be
	 ;; chosen anyway.
	 ;; Should keep any TGM result around to save having to do the work
	 ;; again if we eventually pick this schema.
	 (setq cond (copy-cond-and-replace-cond-node-end cond psuedo-node-end))
	 (let ((check (check-only-use-ifs cond bindings)))
	   (when (consp check)
	     (setq bindings check))
	   (unless check
	     ;; Now check if there is an outstanding Agenda Entry which could
	     ;; potentially provide the effect that we are looking for to 
	     ;; satisfy this condition. If there is such a thing then add a
	     ;; trigger for the cond to wait for the Agenda Entrys to be 
	     ;; processed.
	     (if (possibly-satisfying-plan-state-changes-p cond bindings)
		 (push (make-wait-for-effect-trigger cond bindings)
		       trigger)
	       (progn
		 (whats-going-on
		    "Throwing away ~S on a failed only_use_if check"
		    (schema-name schema))
		 (return-from filter-on-only-use-ifs nil))))))
	((compute)
	 ;; If the condition can be evaluated, then evaluate it.  This
	 ;; may bind some variables by matching against the value pattern.
	 ;; If the match fails, reject the schema.
	 (unless (try-compute cond bindings)
	   (return-from filter-on-only-use-ifs nil)))))
    ;; If reach here then we have checked all the conditions and either they
    ;; all passed or trigger has been set to a list of triggers to cause this
    ;; schema to wait for some agenda entries that may introduce the effects
    ;; required to possibly satisfy the conditions of the schema.
    (if trigger
	(if (> (length trigger) 1)
	    ;; Want to wait for all such agenda entries to finish.
	    (cons :AND trigger)
	    (car trigger))
	t)))

;;; (filter-schemas candidates fn) applies fn to the list of candidate
;;; schema/bindings pairs.  fn is expected to take a schema and a
;;; binding alist as its arguments and to return t if can use the
;;; schema now, a trigger if could possibly use the schema after some
;;; upcoming changes to the plan state have been made, or nil if the
;;; schema cannot be used.  This function then returns two results. A
;;; list of those schemas that could be used now, and a list of those
;;; schemas that require some changes to the plan state, with their
;;; triggers.

(defun filter-schemas (candidates fn)
  (let (now-schemas to-wait-schemas)
    (dolist (candidate candidates (values (nreverse now-schemas)
					  (nreverse to-wait-schemas)))
      (let ((fn-result (funcall fn (car candidate) (cdr candidate))))
	(if (eql fn-result t)
	    (push candidate now-schemas)
	    (unless (null fn-result)
	      (push (cons candidate fn-result) to-wait-schemas)))))))

(defun expand-from-schema (node schema)
  (check-type schema schema)
  (assert (schema-name schema))		;/\/ [bugs 30 May 99, JD]
  (handler-case (db-request :EXPAND node
			    (schema-name schema)
			    (schema-nodes schema)
			    (schema-orderings schema))
    (cannot-expand-node ()
      nil)
    (:no-error (subnodes)
      (loop for (tag type pattern) in subnodes
	    unless (eq type 'dummy)
	    do (post-expand `(:EXPAND ,tag ,pattern)))
      t)))

(defun schema-may-add-more-than-one-node-p (schema)
  (or (length>1 (schema-nodes schema))
      (and (length=1 (schema-nodes schema))
	   (node-iterators (first (schema-nodes schema))))))



;;; /\/: If we need fns for PSVs and binding tuples, they should be
;;; in a module that's common to the DM and the KPs.

;; Predicate for determining if a symbol is a PSV.
(defun psvar-p (var)
  (and (symbolp var)
       (> (length (symbol-name var)) 4)
       (string= var "PSV-" :end1 4)))

;; Predicate for determining if a binding tuple has a Plan State var.
(defun var-p (tuple)
  (psvar-p (cadr tuple)))

;; Predicate for determining if a binding tuple has no value.
(defun undef-p (tuple)
  (eql (cadr tuple) :undef))

;; Returns the name of the PSVAR.
(defun psvar-name (var)
  (second var))

(defun sort-schema-alternatives (from)
  ;; We have a list of schemas, all wanting to be chosen, so use the criteria
  ;; of least number of unbound variables, and then first come first served to
  ;; order the list.  The "from" list has the form ((schema . bindings) ...).
  ;; /\/: Does t ever actually appear as the bindings?
  (let ((preferred-order (standard-schema-sort from)))
    (if (eq *schema-selection-mode* :ask)
	(ask-user-to-pick-best-schema preferred-order)
      preferred-order)))

(defun standard-schema-sort (from)
  ;; Sorts a list of (schema . bindings) pairs (see sort-schema-alternatives).
  (stable-sort from
    #'(lambda (x y)
	(if (eql (cdr x) t)
	    t			;/\/ s.b. (not (eql (cdr y) t))
	  (if (eql (cdr y) t)
	      nil
	    (< (count-if #'undef-p (cdr x))
	       (count-if #'undef-p (cdr y))))))))

(defun ask-user-to-pick-best-schema (from)
  ;; /\/: Really we ought to go via a :USER agenda entry,
  ;;      but this will do for now.
  (ks-user-schema-choice from))


(defun abort-add-cond-or-effect nil
  (if *kp-debug-p* (format t "~&*** abort-add-cond-or-effect ***~%"))
  (db-request :ABORT-ADD-TOME-OR-GOST))


;; Looks through a form for any mention of PSVs, and returns those PSV's
;; mentioned.
(defun find-cond-psvs (cond &optional accumulator)
  (cond ((psvar-p cond)
	 (cons cond accumulator))
	((consp cond)
	 (find-cond-psvs (car cond) (find-cond-psvs (cdr cond) accumulator)))
	(t accumulator)))

;; If the condition mentions a Plan State var, then we return a trigger that
;; requires the PS var to be bound. Handles multiple PS vars.
(defun trigger-for-cond (cond)
  (let ((psvs-found (find-cond-psvs cond)))
    (or (if psvs-found
	    (if (length=1 psvs-found)
		(cons :BIND (car psvs-found))
		(cons :AND (mapcar #'(lambda (x) (cons :BIND x))
				   psvs-found))))
	t)))

;; Takes a list of conditions and returns only those that are not
;; filter conditions (ie, not only_use_if, only_use_for_query, or
;; compute), minus their condition type.
(defun get-conditions-for-triggering (conds)
  (loop for c in conds
	unless (or (eq (con-type c) 'only_use_for_query)
		   (eq (con-type c) 'only-use-if)
		   (eq (con-type c) 'compute))
	collect
	 (case (con-type c)
	   ((achievable) (cdr (con-minus-after-point c)))
	   (     t       (cdr c)))))

(defun instantiate-schema (schema+bindings &optional (node-tag :unknown))
  (let ((result (db-request :INSTANTIATE-SCHEMA
		  (car schema+bindings) (cdr schema+bindings) node-tag)))
    (cond ((list-beginning :poison result)
	   (post-agenda `(:poison-state ,@(cdr result)))
	   (signal-error 'posted-poison
	     :format-string "Posted poison ~S."
	     :format-arguments (list result)))
	  (t
	   result))))

(defun get-effect-schemas (patt)
  (ipc-with-own-copy
    (result (db-request :GET-EFFECT-SCHEMAS patt)
	    (deep-copy-envs-and-schemas result))))

(defun deep-copy-envs-and-schemas (e/s)
  (cond ((schema-p e/s)
	 (deep-copy-schema e/s))
	((consp e/s)
	 (cons (deep-copy-envs-and-schemas (car e/s))
	       (deep-copy-envs-and-schemas (cdr e/s))))
	(t
	 e/s)))  

(defun process-node-end (etag parent-node parent-p)
  ;; The atnode number can be either :self or a number.
  ;; If it is :self, then replace with parent-node
  ;; If it is a number and parent-p is nil then replace with parent-node
  ;; Otherwise append the number to parent-node and use that.
  ;; DESTRUCTIVE
  (if (or (eq (etag-node etag) :self)
	  (not parent-p))
      (setf (etag-node etag) parent-node)
      (setf (etag-node etag)
	    (intern (concatenate 'string
		       (symbol-name parent-node)
		       "-" (int->string (etag-node etag)))))))

;;; The following method may be simpler overall, in the end.

(defun process-constraint-etags! (constraint parent-node parent-p)
  ;; Assumes that constraints are lists and the etags appear only
  ;; as list elements, not e.g. buried in sublists.
  (dolist (part constraint constraint)
    (when (raw-etag-p part)
      (process-node-end part parent-node parent-p))))

(defun raw-etag-p (obj)
  (and (consp obj)
       (consp (cdr obj))		;length = 2
       (null (cddr obj))
       (or (numberp (etag-node obj))
	   (eq (etag-node obj) :self))
       (or (eq (etag-end obj) :begin)
	   (eq (etag-end obj) :end))))


;;; Process-schema
;;
;; Takes an instantiated schema and processes all the fields. IE adds the
;; effects, conditions etc.  Called from KS-ACHIEVE and KS-EXPAND.
;; If parent-p is t then this schema is from an expansion, and node is the
;; parent node being expanded. If parent-p is nil, then this schema is from
;; an achieve, and node is the added node.
;; Return t if okay, otherwise nil indicates a problem.

(defun process-schema (schema node parent-p)
  (and (add-time-windows schema node parent-p)
       (add-effects schema node parent-p)
       (add-conditions schema node parent-p)
       ; (add-time-windows schema node parent-p)
       (add-resources schema node parent-p)
       (add-constraints schema node parent-p)))
    
(defun add-effects (schema node parent-p)
  ;;; ADDING EFFECTS
  (let ((effects (append (schema-only-use-for-effects schema)
			 (schema-effects schema))))
    (when (null effects)
      (return-from add-effects t))
    ;; Change the atnode numbers
    (dolist (e effects)
      (process-node-end (effect-at-node-end e) node parent-p))
    ;; Try to add the effects
    (handle-cm-result
      (db-request :add-constraints 'effect effects))))

(defun handle-cm-result (cm-result) ; -> t or nil
  (cond ((eq cm-result t)
	 t)
	((null cm-result)
	 nil)
	((and (length=1 cm-result)
	      (list-beginning :AGENDA (first cm-result))
	      (list-beginning :POISON-STATE (cadr (first cm-result))))
	 ;; Poison
	 (apply #'post-agenda (cdr (first cm-result)))
	 nil)
	(t
	 ;; Add agenda entries
	 (dolist (r cm-result t)
	   (assert (list-beginning :AGENDA r))
	   (let ((body (cadr r))
		 (args (cddr r)))
	     (ecase (car body)
	       (:OR
		(assert (null args))
		(apply #'post-or (cdr body)))
	       (:ACHIEVE
		(apply #'post-achieve body args))
	       (:CONDITION
		(apply #'post-condition body args))))))))


(defvar *or-trees-to-merge*)

(defun add-conditions (schema node parent-p)
  ;;; ADDING CONDITIONS
  (let ((*or-trees-to-merge* '()))
    ;; For each condition, ask the DM to add it, and if okay continue, else if
    ;; there are some interactions with TOME entries then post the interactions
    ;; as KS-OR records, unless the condition type warrents some other action.
    ;; If we cannot add a condition then POISON PLAN.
    (dolist (cond (schema-conditions schema)
		  (handle-merging-or-trees node *or-trees-to-merge*))
      ;; Change the atnode number.
      (process-node-end (con-at-node-end cond) node parent-p)
      ;; If this is a supervised condition, then the fifth item of cond
      ;; is a list of contributing nodes, which need to be changed to the
      ;; actual nodes.
      (when (eq (con-type cond) 'supervised)
	(mapc #'(lambda (n)
		  (process-node-end n node parent-p))
	      (con-contributors cond)))
      (ecase (con-type cond)
	(supervised
	 ;; Add GOST entry regardless.
	 (assert (handle-adding-condition cond)))
	(only_use_if
	 ;; Fail if cannot add GOST entry.
	 (unless (handle-adding-condition cond)
	   ;; This should not happen since we check only_use_ifs before
	   ;; expanding the schema.
	   (post-agenda `(:POISON-STATE :COULD-NOT-ADD-COND ,cond
			  :WHEN :EXPANDING ,node))
	   (return nil)))
	(only_use_for_query
	 (unless (handle-adding-condition cond)
	   ;; Failed to add the only_use_for_query cond, so first check to
	   ;; see if there are any outstanding agenda-entries that could
	   ;; possibly provide an effect that could satisfy the cond, and if
	   ;; so post to add this cond with a trigger to wait for the effect,
	   ;; otherwise fail.
	   (if (possibly-satisfying-plan-state-changes-p cond nil)
	       (post-condition `(:CONDITION ,cond)
		  :trigger (make-wait-for-effect-trigger cond nil))
	     (progn
	       (post-agenda `(:POISON-STATE :COULD-NOT-ADD-COND ,cond
			      :WHEN :EXPANDING ,node))
	       (return nil)))))
	(achievable
	 (let ((c (con-minus-after-point cond))
	       (a (con-after-point cond)))
	   (unless (eq (etag-node a) :start)
	     (assert (raw-etag-p a))
	     (process-node-end a node parent-p))
	   (db-call 'record-achieve-parent c node)
	   (db-call 'record-achieve-after-point c a)
	   (post-achieve `(:ACHIEVE ,c)
	       :trigger (trigger-for-cond c))))
	(unsupervised
	 ;; This just checks that the condition holds by the time
	 ;; that most of the work in expanding and adding nodes is
	 ;; done.
	 (post-condition `(:CONDITION ,cond)
			 :trigger (make-wait-for-effect-trigger cond nil)))
	(compute
	 (unless (handle-adding-compute cond)
	   (return-from add-conditions nil)))))))

(defun handle-merging-or-trees (node or-trees)
  (if (null or-trees)
      t
    ;; If there are or-trees, they must all be satisfied.
    (let ((m (db-call 'merge-or-trees or-trees)))
      (cond ((null-or-tree m)
	     (post-agenda `(:POISON-STATE :COULD-NOT-ADD-MERGED-TREES
					  :WHEN :EXPANDING ,node))
	     nil)
	    (t
	     (post-or `(:merged) nil nil m)
	     t)))))

(defun handle-adding-condition (cond &optional contributors-to-ignore)
  (when contributors-to-ignore
    (setq cond (append cond (list (cons :NOT contributors-to-ignore)))))
  (handle-cm-result
    (db-request :add-constraint 'world-state cond)))

(defun handle-adding-compute (cond)
  ;; Evaluate p = v as soon as all the vars in p have values.
  ;; If they all already have values, we can do it now.
  ;; /\/: Note that trigger-for-cond doesn't have to be given a
  ;; condition per se.
  (let ((trigger (trigger-for-cond (con-pattern cond)))
	(p (con-pattern cond))
	(v (con-value cond)))
    (cond ((eq (car p) ':computed)
	   ;; Already computed during filtering.  That should have
	   ;; bound all variables in the value.
	   (assert (db-call 'fully-instantiated-p v)))
	  ((eq trigger t)
	   ;; Eval now
	   (unless (kp-add-constraint 'compute cond
		     :or-handler
		       #'(lambda (or-tree)
			   (nconcf *or-trees-to-merge* (list or-tree))))
	     (return-from handle-adding-compute nil)))
	  (t
	   ;; Eval later, when the PSVs in p have been bound.
	   (post-agenda `(:COMPUTE ,cond)
			:trigger trigger
			:level 0)))
    ;; Ends up here if no poison.
    t))

(defun add-time-windows (schema node parent-p)
  (dolist (time-window (schema-time-windows schema) t)
    (process-constraint-etags! time-window node parent-p)
    (unless (kp-add-constraint 'time-window time-window :or-handler :reject)
      (return nil))))

(defun add-resources (schema node parent-p)
  (or (null (schema-resources schema))
      (kp-add-constraint-block
         'resource 
	 (mapc #'(lambda (c)
		   (process-constraint-etags! c node parent-p))
	       (schema-resources schema))
	 :or-handler :reject)))

(defun add-constraints (schema node parent-p)
  ;; The schema-constraints is a list of constraint-blocks.
  (let ((blocks (schema-constraints schema)))
    ;; Process the "from" and "do" node-ends
    (dolist (b blocks)
      (dolist (c (constraint-block-constraints b))
	(process-constraint-etags! c node parent-p)))
    ;; Add the constraints
    (every #'(lambda (b)
	       (kp-add-constraint-block
		 (constraint-block-type b)
		 (constraint-block-constraints b)))
	   blocks)))


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
