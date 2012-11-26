;;;; File: complr.lisp
;;; Contains: Domain construction (after parsing) in the TF compiler
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: March 1992
;;; Updated: Sun Aug 22 23:49:11 1999 by Jeff Dalton
;;; Copyright: (c) 1992, 1993, 1994, 1995 AIAI, University of Edinburgh


(in-package :oplan-tf-compiler)


;;; Construct-domain is in effect pass 2 of the compiler.  It is
;;; responsible for installing elements of the parse tree in the
;;; domain information structure, making any necessary transformations
;;; along the way.

;;; Note that forms are processed in the order in which they occur
;;; in the source file.

(defun construct-domain (domain parse-tree)
  (let ((*domain* domain)
	(*new-schemas* nil))
    (dolist (form parse-tree)
      (when form
	(process-tf-form domain form)))
    (setq *new-schemas* (nreverse *new-schemas*))
    (install-new-schemas domain *new-schemas*)))

(defun process-tf-form (domain form)
  (let ((form-type (car form))
	(form-info (cdr form)))
    (case form-type
      ((language)           (install-language domain form-info))
      ((always)             (install-always domain form-info))
      ((types)              (install-types domain form-info))
      ((defaults)           nil)	;no action needed /\/
      ((constraint_syntax)  nil)	;ignore for now /\/
      ((schema)             (process-schema domain form-info))
      ((compute_condition)  (install-compute-functions domain form-info))
      ((resource_units)     (install-resource-units domain form-info))
      ((resource_types)     (install-resource-types domain form-info))
      ((initially)          (install-initially domain form-info))
      ((initial_resources)  (install-initial-resources domain form-info))
      ((initial_time)       (install-initial-time domain form-info))
      (t
       (tf-error "Unimplemented TF form: \"~A\"." form-type)))))

(defun install-language (domain info)
  (let ((language-name (car info))
	(forms (cadr info)))
    (if (eq language-name 'lisp)
	(setf (domain-included-code domain)
	      forms)
      (tf-error "Cannot handle language \"~A\", only Lisp." language-name))))

(defun install-always (domain facts)
  ;; /\/: Replace or append.  Right now, we append.
  (setf (domain-always domain)
	(append (domain-always domain) facts)))

(defun install-types (domain types)
  (setf (domain-object-types domain)
	(expand-type-expressions
	  (replacing-merge (domain-object-types domain)
			   types
			   :key #'type-def-name))))

(defun replacing-merge (old new &key (key #'identity) (test #'eql))
  ;; /\/: Could call this function stable-union?
  (append (stable-set-difference old new :key key :test test)
	  new))

(defun install-compute-functions (domain function-declarations)
  (setf (domain-compute-functions domain)
	(replacing-merge (domain-compute-functions domain)
			 function-declarations
			 :key #'(lambda (cf)
				  (car (compute-function-call-pattern cf))))))

(defun install-resource-units (domain units)
  (setf (domain-resource-units domain)
	(replacing-merge (domain-resource-units domain)
			 units
			 :key #'first
			 :test #'intersection))) ;remember synonyms

(defun install-resource-types (domain types)
  (setf (domain-resource-types domain)
	(replacing-merge (domain-resource-types domain)
			 types
			 :key #'second
			 :test #'equal)))

(defun install-initially (domain pattern-assignments)
  (setf (domain-initially domain)
	(mapcar #'(lambda (p+v)
		    (effect (pv-pattern p+v)
			    (pv-value p+v)
			    (node-end 1 :end)))
		pattern-assignments)))

(defun install-initial-resources (domain resource-usage-specs)
  (setf (domain-initial-resources domain)
	resource-usage-specs))

(defun install-initial-time (domain time-spec)
  (setf (domain-initial-time domain)
	time-spec))


;;; Type expressions

;;; /\/: The original definitions in terms of and, or, etc are lost.
;;; It might be a good idea to retain them somewhere.

(defun expand-type-expressions (defs)

  ;; The later code destructively modifies type-def-sets, so make
  ;; a copy if we're going to do that, to help in debugging etc.
  (when (loop for d in defs thereis (actorp (type-def-set d)))
    (setq defs (mapcar #'copy-type-def defs)))

  ;; Turn all definitions into enumerations.  Now that when the loop
  ;; gets to a definition, the def may already have been converted because
  ;; an earlier type referred to it.  So not all conversions happen right
  ;; here.
  (dolist (d defs)
    (when (actorp (type-def-set d))
      (setf (type-def-set d)
        (t-eval (type-def-set d) defs (list (type-def-name d))))))

  defs)

(defun t-eval (expr defs path)
  (cond ((not (actorp expr))		;already an enumeration?
	 expr)				;yes
	((actor-type-p 'type expr)
	 ;; Type -- ref to a named type.  Eval the definition of the
	 ;; referred-to type and record the resulting enumeration in
	 ;; the defs so that we don't have to compute it again.
	 (let* ((ty (first (actargs expr)))
		(d (find ty defs :key #'type-def-name)))
	   (if (member ty path)
	       (progn (tf-error "Cycle in type definions: ~S."
				(reverse path))
		      nil)
	     (setf (type-def-set d)
		   (t-eval (type-def-set d) defs (cons ty path))))))
	((actor-type-p 'and expr)
	 ;; And -- allows intersection and set-difference.
	 (t-eval-and (actargs expr) defs path))
	((actor-type-p 'or expr)
	 ;; Or -- union.
	 (let ((sets (mapcar #'(lambda (arg) (t-eval arg defs path))
			     (actargs expr))))
	   (reduce #'oplan-util::more-stable-union ;/\/
		   sets
		   :initial-value '())))
	(t
	 (error "Invlaid type expr ~S." expr))))

(defun t-eval-and (args defs path)
  ;; And is basically an intersection, but ?{not <type_expr>}s may also
  ;; appear and thus negated values must be taken out by set-difference.
  (multiple-value-bind (nots ands)
      (separate-list #'(lambda (e) (and (actorp e) (actor-type-p 'not e)))
		     args)
    (let* ((and-sets (mapcar #'(lambda (arg) (t-eval arg defs path))
			     ands))
	   (not-sets (mapcar #'(lambda (arg)
				 (t-eval (first (actargs arg)) defs path))
			     nots))
	   (in (reduce #'stable-intersection (cdr and-sets)
		       :initial-value (car and-sets))))
      (dolist (n not-sets)
	(setq in (stable-set-difference in n)))
      in)))

(defun separate-list (test list)	;/\/ s.b. in util-functions
  (let ((in '())
	(out '()))
    (dolist (e list)
      (if (funcall test e) (push e in) (push e out)))
    (values
      (nreverse in)
      (nreverse out))))


;;; Schemas

;;; Install-new-schemas modifies the schema list in a domain
;;; and returns a list of the replaced schemas.

;;; /\/: It's not clear what the order of the schema list in
;;; the modified domain structure should be.  Perhaps it doesn't
;;; matter?

(defun install-new-schemas (domain new-schemas)
  (let* ((old-schemas (domain-schemas domain))
	 (retained-schemas
	  (stable-set-difference old-schemas new-schemas
				 :key #'schema-name
				 :test #'same-name-p))
	 (replaced-schemas
	  (stable-set-difference old-schemas retained-schemas)))
    (setf (domain-schemas domain)
	  (append retained-schemas new-schemas))
    replaced-schemas))

(defun process-schema (domain schema-description)
  (destructuring-bind (type name clauses *schema-plist*) schema-description
    (case type
      ((meta_schema meta_process_schema)
       (process-meta-schema domain type name clauses *schema-plist*))
      (t
       (let ((inst-of (assoc 'instance_of clauses)))
	 (push (if inst-of
		   (construct-meta-inst
		     (second inst-of) type name clauses *schema-plist*)
		 (construct-schema
		   type name clauses *schema-plist*))
	       *new-schemas*))))))


;;; Meta-schemas

;;; Meta-schemas are stored as structs that record the results of
;;; parsing the meta-schema.  These results can be combined with the
;;; results of parsing a schema what has an "instance_of" clause and
;;; then fed into the later stages of schema processing to produce an
;;; actual schema (see construct-meta-inst).

;;; 

(defstruct (meta-schema (:conc-name ms-))
  type		;meta_schema or meta_process_schema
  name		;the meta-schema's name
  pattern	;specifys the meta-schema's parameters
  clauses	;as returned by the parser
  plist)	;the *schema-plist* from when the meta-schema was parsed

(defun ms-inst-type (ms)
  (ecase (ms-type ms)
    (meta_schema 'schema)
    (meta_process_schema 'process_schema)))

(defun process-meta-schema (domain type name clauses *schema-plist*)
  ;; Constructs a meta-schema and adds it to a domain.
  (let ((inst-clause (assoc 'instantiates clauses)))
    (if (null inst-clause)
	(tf-error "~@(~A~) \"~A\" has no \"instantiates\" clause."
		  type name)
      (push (make-meta-schema
	       :type type
	       :name name
	       :pattern (second inst-clause)
	       :clauses (remove-1-eq inst-clause clauses)
	       :plist *schema-plist*)
	    (domain-meta-schemas domain)))))

(defun construct-meta-inst (meta-pattern type name clauses *schema-plist*)
  ;; Expand the instance_of clause of a schema.  At present, there can
  ;; be only one istance_of clause and there cannot be any other clauses.
  ;; /\/
  (if (not (and (length=1 clauses) (eq (caar clauses) 'instance_of)))
      (tf-error "Schema \"~A\" has clauses in addition to instance_of."
		name)
    (multiple-value-bind (meta-schema
			  arg-env
			  meta-clauses)
	(instantiate-meta-schema meta-pattern)
      (declare (ignore arg-env))
      (assert (eq type (ms-inst-type meta-schema)))
      ;; /\/ insert check for plist compatibility
      (let ((inst
	     (construct-schema
	       type
	       name
	       meta-clauses
	       (ms-plist meta-schema))))
	inst))))

(defun instantiate-meta-schema (pattern)
  (multiple-value-bind (m env) (find-meta-schema pattern)
    (values
      m env (replace-meta-vars (ms-clauses m) env))))

(defun find-meta-schema (pattern) ; -> meta-schema, env
  (dolist (m (domain-meta-schemas *domain*))
    (let ((e (match-meta-pattern (ms-pattern m) pattern nil)))
      (when (not (eq e :fail))
	(return (values m e))))))

(defun replace-meta-vars (item env)
  (cond ((var-pattern-p item)
	 (let ((binding (assoc (actor->variable-name item) env)))
	   (if binding
	       (cdr binding)
	     item)))
	((atom item)
	 item)
	((actorp item)
	 item)
	(t
	 (recons item
		 (replace-meta-vars (car item) env)
		 (replace-meta-vars (cdr item) env)))))

(defun match-meta-pattern (mpat item env) ; -> env
  ;; The env maps names, rather than actors, to values.
  (cond ((eq mpat actorsym)
	 env)
	((eq item actorsym)
	 env)
	((var-pattern-p mpat)
	 (cons (cons (actor->variable-name mpat) item) env))
	((atom mpat)
	 (if (eql mpat item) env :fail))
	((actorp item)
	 :fail)
	((actorp mpat)
	 :fail)
	(t
	 (let ((e (match-meta-pattern (car mpat) (car item) env)))
	   (if (eq e :fail)
	       :fail
	     (match-meta-pattern (cdr mpat) (cdr item) e))))))


;;; (Construct-schema type name clauses plist) -> schema
;;;  * Name should be a symbol.
;;;  * Type should be a keyword in *schema-types*.
;;;  * Clauses should be a list of parse results.
;;;  * Plist should be a plist constructued during parsing.

;;; /\/: Right now, *schema-types* does not contain keywords,
;;;      just symbols such as schema and meta_schema.
;;; /\/: Should we downcase the schema name?  [We do now.]
;;; /\/: Why souldn't the schema names be symbols?
;;; /\/: What do we do about task ... end_task?
;;;      At present, all we do is make the name be task_<name>.
;;; /\/: What should the vars list really look like?
;;;      We have entires like (<var-name> <var-restriction>).
;;; /\/: For vars_relations, we use the var names rather than the
;;;      whole actors.  Is this right?  If not, why not?
;;; /\/: Do we need unique start and end nodes for all schemas
;;;      that have nodes?  [Yes, according to bat, but for the user
;;;      interface not the planner.  No, according to rbk, but yes
;;;      for task schemas.]
;;; /\/: Note that the time_windows clause can specify a delay
;;;      between nodes that has to be combined with the information
;;;      in the orderings clause.

(defun construct-schema (type name clauses *schema-plist*)
  (check-schema-type name type)
  (check-schema-completeness type name clauses)
  (check-clause-order name clauses)
  (let ((new-schema (construct-basic-schema type name clauses)))
    (when (eq type 'task)
      (pre-process-task new-schema))
    (check-schema-types new-schema)
    (check-schema-vars new-schema)
    (check-schema-resources new-schema)
    (process-schema-orderings-and-time-windows new-schema)
    (process-schema-nodes new-schema)
    (when (eq type 'task)
	  (post-process-task new-schema))
    new-schema))

;;; Construct-basic-schema constructs a schema structure and installs
;;; slot values directly from the parse tree.  This turns out to be
;;; sufficient for everything that doesn't involve nodes.  Slots that
;;; do involve nodes will have to be modified if the nodes must be
;;; renumbered.

(defun construct-basic-schema (type name clauses)
  (when (lookup 'only_use_for_resources clauses)
    (tf-warning "Unimplemented schema clause \"~A\" in schema \"~A\"."
		'only_use_for_resources name))
  (make-schema
    :name (if (eq type 'task) (->task-name name) (->schema-name name))
    :type (schema-type->keyword type name)
    :vars (append (lookup 'vars clauses)
		  (lookup 'local_vars clauses))
    :relationships
       (lookup 'vars_relations clauses)
    :expands
       (lookup 'expands clauses)
    :only-use-for-effects
       (lookup 'only_use_for_effects clauses)
    :nodes
       (lookup 'nodes clauses)
    :orderings
       (lookup 'orderings clauses)
    :conditions
       (lookup 'conditions clauses)
    :effects
       (lookup 'effects clauses)
    :time-windows
       (lookup 'time_windows clauses)
    :resources
       (lookup 'resources clauses)
    :constraints
       (loop for clause in clauses
	     when (list-beginning 'constraints clause)
	     collect (cadr clause))))

(defun schema-type->keyword (type schema-name)
  ;; This is not called for meta schemas.
  (case type
    ((schema)        :action)
    ((task)          :task)
    ((repair_schema) :repair)
    (t
     (tf-warning "Unimplemented schema type \"~A\" for schema \"~A\"."
		 type schema-name)
     type)))				;leave others as plain symbols.


;;; Check for use of undefined variables, types, and resources.

;;; Also check for variables that are defined but not used.

;;; Note that at this point we do not distinguish between patterns,
;;; pattern-components, and actor-restrictions -- all are regarded as
;;; patterns.

;;; Note that we can't (easily) move the type check to the parser,
;;; because when parsing a schema we don't know what types were
;;; defined earlier in the same file.

(defun check-schema-vars (schema)
  (let ((defined-vars (mapcar #'var-def-name (schema-vars schema)))
	(used-vars '()))
    (walk-schema-patterns
      #'(lambda (pat)
	  (dolist (v (pattern-variables pat))
	    (pushnew v used-vars)))
      schema)
    (mapc #'(lambda (r)
	      (pushnew (actor->variable-name (var-relation-var1 r)) used-vars)
	      (pushnew (actor->variable-name (var-relation-var2 r)) used-vars))
	  (schema-relationships schema))
    ;; /\/: here check for vars in time-windows and resources
    (setq used-vars (nreverse used-vars))
    (let ((undefined (stable-set-difference used-vars defined-vars))
	  (ignored (stable-set-difference defined-vars used-vars)))
      (when undefined
	(tf-error "Schema \"~A\" uses undefined variables: ~S."
		  (schema-name schema) undefined))
      (when ignored
	(tf-error "Schema \"~A\" fails to use defined variables: ~S."
		  (schema-name schema) ignored)))))

(defun check-schema-types (schema)
  (let ((defined-types (mapcar #'type-def-name
			       (domain-object-types *domain*)))
	(undefined '()))
    (walk-schema-patterns
      #'(lambda (pat)
	  (dolist (ty (pattern-types pat))
	    (unless (member ty defined-types)
	      (pushnew ty undefined))))
      schema)
    (when undefined
      (tf-error "Schema \"~A\" uses undefined types:~%~8t~S."
		(schema-name schema) (reverse undefined)))))

(defun check-schema-resources (schema)
  (let ((defined-resource-types (domain-resource-types *domain*))
	(undefined '()))
    (dolist (r (schema-resources schema))
      (let ((resource (resource-operation-resource r)))
	;; /\/: really ought to pattern-match, once vars are allowed
	(unless (member resource
			defined-resource-types
			:key #'resource-type-def-resource
			:test #'equal)
	  (pushnew resource undefined :test #'equal))))
    (when undefined
      (tf-error "Schema \"~A\" uses undefined resource types:~%~
                 ~{~8t~S~^~%~}"
		(schema-name schema)
		(reverse undefined)))))


;;; Pattern processing

(defun walk-schema-patterns (fn schema)
  (funcall fn (schema-expands schema))
  ;; /\/: The conditions are treated as a single pattern.
  (funcall fn (schema-conditions schema))
  ;; /\/: Resources?  Time_windows?
  (flet ((walk (field items)
	   (mapc #'(lambda (item)
		     (funcall fn (funcall field item)))
		 items)))
     (walk #'var-def-restriction (schema-vars schema))
     (walk #'effect-pattern      (schema-only-use-for-effects schema))
     (walk #'effect-value	 (schema-only-use-for-effects schema))
     (walk #'node-pattern        (schema-nodes schema))
     (walk #'effect-pattern	 (schema-effects schema))
     (walk #'effect-value	 (schema-effects schema))
     (dolist (block (schema-constraints schema))
       (walk #'constraint-pattern  (constraint-block-constraints block))
       (walk #'constraint-value    (constraint-block-constraints block)))))

(defun pattern-variables (pattern) ; -> list of names
  ;; /\/: See "parser" for var-pattern-p and actor->variable-name.
  (cond ((var-pattern-p pattern)
	 (list (actor->variable-name pattern)))
	((actorp pattern)
	 (pattern-variables (cddr pattern)))
	((atom pattern)
	 nil)
	(t
	 (mapcan #'pattern-variables pattern))))

(defun pattern-types (pattern) ; -> list of names
  (cond ((type-actor-p pattern)
	 (list (actor->type pattern)))
	((actorp pattern)
	 (pattern-types (cddr pattern)))
	((atom pattern)
	 nil)
	(t
	 (mapcan #'pattern-types pattern))))

(defun type-actor-p (obj)
  (and (actorp obj)
       (eq (actfn obj) (actor-rename 'type))))

(defun actor->type (actor)
  (car (actargs actor)))


;;; Process-schema-orderings-and-time-windows combines time window
;;; information from the "orderings" clause with that from the
;;; "time_windows" clause, storing the result in the time-windows
;;; slot of the schema.

;;; /\/: This relies on the fact that an ordering struct contains
;;; a duration when one has been specified in the "orderings" clause.
;;; If we change those structs, we'll need some other way to pass
;;; that information.

;;; It also adds any orderings specified in the "time_windows" clause
;;; or by sequential-blocks in the "nodes" clause to those given in
;;; the "orderings" clause.

;;; The goal is to put all orderings in the orderings slot and all
;;; time windows in the time-windows slot, even though, at present,
;;; orderings also contain "delays".  /\/

(defun process-schema-orderings-and-time-windows (schema)
  (let ((orderings (schema-orderings schema))
	(windows (schema-time-windows schema)))
    ;; Add delays from "orderings" to "time_windows".
    (let ((orderings-with-windows
	   (remove-if #'null orderings :key #'ordering-delay)))
      (when orderings-with-windows
	(setf (schema-time-windows schema)
	      (append (mapcar #'ordering->time-constraint
			      orderings-with-windows)
		      windows))))
    ;; Add orderings from "time_windows" to "orderings".
    ;; Redundant orderings are detected and hence not added
    ;; to the list
    (let* ((windows-with-orderings
	    (remove-if-not #'delay-between-window-p windows))
	   (window-orderings
	    (mapcar #'time-constraint->ordering windows-with-orderings))
	   (new-orderings-from-windows
	    (stable-set-difference window-orderings orderings
				   :test #'equal)))
      (when new-orderings-from-windows
	(setf (schema-orderings schema)
	      (append orderings new-orderings-from-windows))))
    ;; Handle any orderings from the "nodes" clause.
    (let ((order-blocks (getf-schema 'order-blocks)))
      (when order-blocks
	(let ((implicit-orderings
	       (get-schema-implicit-orderings schema order-blocks)))
	  (when implicit-orderings
	    (setf (schema-orderings schema)
		  (append (schema-orderings schema) implicit-orderings))))))
    ;; The situation is sufficiently complex that we had better
    ;; check that there are no cycles.  Note that the implicit orderings
    ;; cannot produce a cycle by themselves, so they're unlikely to be
    ;; the culprit.
    (when (ordering-cycle-p (schema-orderings schema))
      (tf-error "Schema \"~A\" has an ordering cycle.~%~
                 Check the orderings and time_windows clauses."
		(schema-name schema)))
    ;; And we're done.
    schema))

(defun delay-between-window-p (constraint)
  (not (or ;; at <point>?
	   (eq (tcon-end-1 constraint) :abst0)
	   ;; duration?
	   (eql (node-end-node (tcon-end-1 constraint))
		(node-end-node (tcon-end-2 constraint))))))

(defun ordering->time-constraint (ordering)
  (construct-time-constraint
    (ordering-from-ne ordering)
    (ordering-to-ne ordering)
    (ordering-delay ordering)))

(defun time-constraint->ordering (constraint)
  (construct-ordering
    (tcon-end-1 constraint)
    (tcon-end-2 constraint)
    ;; Assume we don't need (tcon-bounds constraint) and hence can use nil.
    nil))

;;; Processing of "sequential" and "parallel" node blocks

(defvar *implicit-orderings*)

(defun get-schema-implicit-orderings (schema order-blocks)
  (declare (ignore schema))
  (assert (not (null order-blocks)))
  (let ((*implicit-orderings* '()))
    (generate-orderings order-blocks)
    (nreverse *implicit-orderings*)))

(defun generate-orderings (blocks)
  (dolist (b blocks)
    (when (list-beginning 'sequential b)
      (for-adjacent-elements #'make-sequential (cdr b)))))

(defun make-sequential (r s)
  (dolist (rmax (block-maximals r))
    (dolist (smin (block-minimals s))
      (emit-ordering rmax smin))))

(defun emit-ordering (from to)
  (flet ((cvt-end (end) (ecase end (begin_of :begin) (end_of :end))))
    (push (ordering (node-number from)
		    (cvt-end (getf-schema 'link_from_node_end))
		    (node-number to)
		    (cvt-end (getf-schema 'link_to_node_end)))
	  *implicit-orderings*)))

(defun block-minimals (b)
  (cond ((list-beginning 'sequential b)
	 (block-minimals (cadr b)))
	((list-beginning 'parallel b)
	 (mapcan #'block-minimals (cdr b)))
	(t
	 (list b))))

(defun block-maximals (b)
  (cond ((list-beginning 'sequential b)
	 (block-maximals (last-element (cdr b))))
	((list-beginning 'parallel b)
	 (mapcan #'block-maximals (cdr b)))
	(t
	 (list b))))

;;; Ordering-cycle test.

(defun ordering-cycle-p (orderings)
  ;; The orderings aren't in the right form for our graph routines, because
  ;; they expect to find successors easily.  So this may be slower than we'd
  ;; like.  /\/
  (if (null orderings)
      nil
    (has-cycle-p
      ;; node-ends that have successors
      (mapcar #'ordering-from-ne orderings)
      ;; successor fn
      #'(lambda (ne)
	  (let* ((n (node-end-node ne))
		 (e (node-end-end ne))
		 (succ-ords
		  (remove-if-not
		    #'(lambda (ord)
			(and (eql n (ordering-node-number1 ord))
			     (eq e (ordering-node-end1 ord))))
		    orderings)))
	    ;; The orderings in succ-ords point to the successors of ne.
	    ;; So we convert succ-ords to a list of node-ends.
	    (let ((explicit-successors (mapcar #'ordering-to-ne succ-ords)))
	      ;; begin_of N has end_of N as an implicit successor
	      (if (eq e :begin)
		  (cons (node-end n :end) explicit-successors)
		explicit-successors))))
      :hash-test
        #'equal)))


;;; Process-schema-nodes

;;; At present, process-schema-nodes just checks that nodes are sequentially
;;; numbered.  At one point, the plan was to renumber nodes, if necessary,
;;; so that there is a unique start numbered 1 and a unique end numbered 2,
;;; but this would break the link between the node numbers created while
;;; planning and the numbers in the schema definitions.

;;; References to undefined nodes are detected while parsing.

(defun process-schema-nodes (schema)
  (check-node-sequence schema)
  schema)


;;; Check-node-sequence checks that nodes are sequentially numbered,
;;; starting with one.  Since this is the normal practice, a departure
;;; from it may indicate an error.

;;; /\/: Worse, since node-renumbering doesn't occur, and since
;;; the planner expects sequential nodes, this check is moderately
;;; necessary.

(defun check-node-sequence (schema)
  (let ((nodes (schema-nodes schema)))
    (unless (nodes-in-sequence-p nodes)
      ;; Could either be out of order or missing some numbers.
      ;; First try sorting them.
      (let ((sorted (sort (copy-list nodes) #'< :key #'node-number)))
	(cond ((nodes-in-sequence-p sorted)
	       ;; Sorting works, so nothing's missing.
	       (tf-warning "Nodes are not sequentially numbered in ~
                            schema \"~A\"." (schema-name schema))
	       (setf (schema-nodes schema)
		     sorted))
	      (t
	       ;; Must be some missing node numbers.
	       (tf-error "There's a gap in the node numbers of schema \"~A\"."
			 (schema-name schema))))))))

(defun nodes-in-sequence-p (nodes)
  (loop for node in nodes
	for n = (node-number node)
	for i from 1
	always (= n i)))
    

;;; Check-schema-nodes would look for references to undefined nodes.
;;; (This is now done while parsing.)

#+undef
(defun check-schema-nodes (schema)
  (let ((defined-nodes (mapcar #'node-number (schema-nodes schema)))
	(undefined '()))
    (walk-schema-node-refs
      #'(lambda (node)
	  (unless (member node defined-nodes)
	    (setq undefined
		  (nconc undefined (list node)))))
      schema)
    (when undefined
      (tf-error "Schema \"~A\" uses undefined nodes: ~S."
		(schema-name schema) undefined))))


;;; Pre- and Post-process-task make the changes in a schema structure
;;; that are needed for tasks.


;;; Pre-processing for tasks

(defun pre-process-task (task) ; -> task
  ;; The information that comes from parsing the task body should
  ;; already be in place.  We add things to it.
  ;; /\/: May want a replacing-merge rather than an append.
  ;; /\/: The replacing-merge :test must consider the node-end.
  ;; /\/: Maybe should fix up node-ends here rather than in post-process-task.

  ;; From "initially".
  (setf (schema-effects task)
	(append
	   (domain-initially *domain*)
	   (schema-effects task)))

  ;; From "initial_resources".
  (setf (schema-resources task)
	(append
	   (domain-initial-resources *domain*)
	   (schema-resources task)))

  ;; From "initial_time".
  (when (domain-initial-time *domain*)
    (setf (schema-time-windows task)
	  (cons (construct-time-constraint
	           :abst0
		   (node-end 1 :begin)	;/\/ ignore default end
		   (domain-initial-time *domain*))
		(schema-time-windows task))))

  task)


;;; Post-processing for tasks

(defun post-process-task (task) ; -> task

  ;; Check or establish various things about nodes 1 and 2
  (check-task-nodes-1-and-2 task)

  ;; Fix references to :self as a node number.
  (fix-task-self-refs task)
  
  ;; And we're done.
  task)


;;; Make sure nodes "1 start" and "2 finish" exist and have the right
;;; types.  Try to ensure that node 1 is the only minimal node and that
;;; node 2 is the only maximal, adding links if necessary.

;;; Here we mostly work with nodes rather than node ends.  That should
;;; be ok for nodes 1 and 2 in tasks at least.

(defun check-task-nodes-1-and-2 (task)
  (let* ((nodes (schema-nodes task))
	 (node-1 (find 1 nodes :key #'node-number))
	 (node-2 (find 2 nodes :key #'node-number)))
    (if (not (and node-1
		  node-2
		  (eq (node-type node-1) 'start)
		  (eq (node-type node-2) 'finish)))
	(tf-error "Task \"~A\" needs nodes \"1 start\" and \"2 finish\"."
		  (schema-name task))
      ;; Nodes 1 and 2 exist and have the right types.
      ;; Now check that they're ordered correctly.
      (unless (task-has-unique-start-and-finish-p task)
	(ensure-node-is-first task node-1)
	(ensure-node-is-last task node-2)))))

(defun task-has-unique-start-and-finish-p (task)
  (let* ((nodes (schema-nodes task))
	 (orderings (schema-orderings task))
	 (min-nodes (minimal-nodes nodes orderings))
	 (max-nodes (maximal-nodes nodes orderings)))
    (or (null nodes)
	(null (cdr nodes))
	(and (length=1 min-nodes)
	     (length=1 max-nodes)
	     (= (node-number (car min-nodes)) 1)
	     (= (node-number (car max-nodes)) 2)))))

(defun ensure-node-is-first (task min-node)
  (if (not (minimal-node-p task min-node))
      (tf-error "Some node is linked before ~D in task \"~A\"."
		(node-number min-node) (schema-name task))
    ;; Link any minimal nodes other than min-node after min-node.
    (let ((new-ords
	   (loop for m in (schema-minimal-nodes task)
		 unless (eq m min-node)
		 collect (make-node-ordering min-node m))))
      (when new-ords
	(setf (schema-orderings task)
	      (append (schema-orderings task) new-ords))))))

(defun ensure-node-is-last (task max-node)
  (if (not (maximal-node-p task max-node))
      (tf-error "Some node is linked after ~S in task \"~A\"."
		(node-number max-node) (schema-name task))
    ;; Link any maximal nodes other than max-node before max-node.
    (let ((new-ords
	   (loop for m in (schema-maximal-nodes task)
		 unless (eq m max-node)
		 collect (make-node-ordering m max-node))))
      (when new-ords
	(setf (schema-orderings task)
	      (append (schema-orderings task) new-ords))))))

(defun make-node-ordering (from to)
  (ordering (node-number from) :end (node-number to) :begin))

(defun schema-minimal-nodes (schema)
  (minimal-nodes (schema-nodes schema) (schema-orderings schema)))

(defun minimal-nodes (nodes orderings)
  ;; nodes w/o predecessors
  (remove-if #'(lambda (node)
		 (some #'(lambda (ordering)
			   (= (ordering-node-number2 ordering)
			      (node-number node)))
		       orderings))
	     nodes))

(defun schema-maximal-nodes (schema)
  (maximal-nodes (schema-nodes schema) (schema-orderings schema)))

(defun maximal-nodes (nodes orderings)
  ;; nodes w/o successors
  (remove-if #'(lambda (node)
		 (some #'(lambda (ordering)
			   (= (ordering-node-number1 ordering)
			      (node-number node)))
		       orderings))
	     nodes))

(defun minimal-node-p (schema node)	;true if no predecessor
  (let ((n (node-number node)))
    (loop for ord in (schema-orderings schema)
	  never (= n (ordering-node-number2 ord)))))

(defun maximal-node-p (schema node)	;true if no successor
  (let ((n (node-number node)))
    (loop for ord in (schema-orderings schema)
	  never (= n (ordering-node-number1 ord)))))


;;; Fix task refs to :self node

;;; (:self :begin) becomes (1 :begin) and (self :end) becomes (2 :end).
;;; /\/: While this sounds right, it tends to get conditions and effects
;;; at the wrong ends.  In tasks, effects are usually at begin_of self
;;; (hence begin_of 1) while conditions are are end_of self (hence
;;; end_of 2), but the default ends are the other way around!

(defun fix-task-self-refs (task)
  ;; Fix up :self in conditions
  (dolist (c (schema-conditions task))
    (fix-task-at-point (con-point c)))

  ;; Fix up :self in effects
  (dolist (e (schema-effects task))
    (fix-task-at-point (effect-point e)))

  ;; Fix up :self in resources
  ;; Overall applies to the task as a whole, so we leave :self as :self.
  (dolist (r (schema-resources task))
    (unless (eq (resource-operation-scope r) 'overall)
      (fix-task-at-point (resource-operation-at r))))

  ;; Fix up :self in time windows
  (dolist (w (schema-time-windows task))
    (unless (eq (tcon-end-1 w) :abst0)
      (fix-task-at-point (tcon-end-1 w)))
    (fix-task-at-point (tcon-end-2 w))))

(defun fix-task-at-point (at-point)
  (when (eq (node-end-node at-point) :self)
    (setf (node-end-node at-point)
	  (ecase (node-end-end at-point)
	    (:begin 1)
	    (:end   2))))
  at-point)
	      

;;; Semi-syntactic checks for schemas

;;; Check schema type

;;; This is a check for whether the type has been implemented.
;;; The parser has already checked whether what the user typed
;;; was a valid schema type.

(defun check-schema-type (name type)
  (case type
    ((schema task))
    (t (tf-error "Schema \"~A\" has unimplemented type \"~A\"."
		 name type))))


;;; Check schema completeness

;;; A schema must have at least one of expands or only_use_for_effects.
;;; However, task schemas get an only_use_for_effects implicitly.

;;; /\/: Could avoid calling ->schema-name if the check was made
;;; after the call to construct-basic-schema.  On the other hand,
;;; this check could be moved to the parser so that the error would
;;; appear in context.

(defun check-schema-completeness (type name clauses)
  (unless (or (eq type 'task)
	      (task-name-p (->schema-name name)))
    (unless (or (lookup 'expands clauses)
		(lookup 'only_use_for_effects clauses))
      (tf-error "Schema \"~A\" is incomplete.  It needs at least~%~
                 ~6T one of expands or only_use_for_effects."
		name))))


;;; Check schema clause order

;;; /\/: Could be done in the parser (when we have more than the name
;;; to refer to and where the context (of the schema definition) is
;;; clearer.

(defparameter *preferred-clause-order*
  ;; vars_relations sometimes comes after vars rather than local_vars.
  '(instance_of
    info
    vars
    vars_relations		;may move to after local_vars
    expands
    only_use_for_effects
    only_use_for_resources
    local_vars			;may become: local_vars vars_relations
    nodes
    orderings
    conditions
    effects
    resources
    time_windows))

(defparameter *allowed-after-constraints*
  (member 'effects *preferred-clause-order*))

(defun check-clause-order (name clauses)
  ;; Note: also (implicitly) detects multiple occurrences of a clause
  ;; and invalid clause names.
  (let* ((actual-order
	  (mapcar #'car clauses))
	 (preferred-order
	  *preferred-clause-order*))

    ;; Adjust for vars_relations possibilities.
    ;; It can be after vars or local_vars, but not both.
    (when (search '(local_vars vars_relations) actual-order)
      ;; Actual order contains: local_vars vars_relations.
      ;; So move vars_relations to after local_vars in the preferred-order.
      (setq preferred-order
	    (replace-sublist '(vars) '(vars vars_relations)
	        (replace-sublist '(local_vars vars_relations)
				 '(local_vars)
				 preferred-order))))

    ;; Remove unused clauses from the preferred order.
    (setq preferred-order
	  (remove-if-not #'(lambda (name) (member name actual-order))
			 preferred-order))
    
    ;; constraints can occur more than once and any time after conditions.
    (let ((tail (member 'constraints actual-order)))
      (when tail
	(setq tail (remove 'constraints tail))
	(let ((too-late
	       (stable-set-difference tail *allowed-after-constraints*)))
	  (when too-late
            (tf-warning
	       "In schema \"~A\", a \"constraints\" clause appears before ~
                ~{~S~^, ~} instead of after." name too-late)))
	(setq actual-order (remove 'constraints actual-order))))

    ;; compare orders.
    (unless (equal actual-order preferred-order)
      ;; /\/: We could try to give a better message.
      ;; /\/: We should explicitly check for multiple occurrences
      (tf-warning
         "In schema \"~A\", some clauses were not in the usual order.~%~
          ~3T Order is: ~S,~%~
          ~3T Instead of: ~S."
	 name actual-order preferred-order))))

;;; End
