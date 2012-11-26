;;;; File: psvs.lisp
;;; Contains: Code for handling Plan State Variables.
;;; Author: Richard Kirby (rbk) and Jeff Dalton
;;; Created: Thu Jul 11 16:09:29 1991
;;; Updated: Sun May 30 23:31:26 1999 by Jeff Dalton
;;; Copyright: (c) 1992, AIAI, University of Edinburgh

(in-package :oplan-psv)

(use-package :oplan-developerlib)
(use-package :oplan-obase)
(use-package :oplan-ctxt)
(use-package :oplan-util)
(use-package :oplan-nodes)		;for some psv descriptions

(import 'oplan::schema-name)

(export '(psv-actorise-pattern
	  psv-replace-psvs-in-pattern
	  psv-add-type
	  psv-get-possibles-for-type
	  psv-get-type-from-restrictions
	  type
	  act-type			;what type's renamed to
	  *psv-current-source-node-ref*
	  psv-set-current-source-node
	  psv-create-psv
	  psv-create-binding-tuple
	  psv-has-value-p
	  psv-get-value
	  psv-p
	  psv-begin-var-transaction
	  psv-commit-var-transaction
	  psv-abort-var-transaction
	  psv-set-value
	  psv-add-restriction
	  psv-get-possibles
	  psv-set-possibles
	  psv-make-a-list-of-the-psvs
	  psv-get-psv-descriptions
	  psv-get-psv-descriptions-for-user
	  psv-print-psv-table
	  psv-clear-psv-tables))

;;; /\/: When a variable has a value, its possibles-cache may still
;;; contain other values.  So far as I know, nothing goes wrong because
;;; of this, but it's untidy and perhaps confusing to the user.

;;; Some psv slots -- original-names and source-nodes -- are just
;;; to provide information to the user.

;;; /\/: The source-nodes slot is handled in a somewhat strange way.
;;; This relies on object identity and side effects in a way that
;;; should be explicitly noted.
;;;
;;; We want to provide some useful-to-the-user information about where
;;; the psv came from, e.g. that it was from schema S used to expand
;;; node N which is an action node with pattern P.  To do that, we need
;;; to know which node was being expanded when the psv was introduced.
;;; Unfortunately, psvs are created sometimes (by db-instantiate-schema)
;;; before the node is created (e.g. in ks-achieve, though not in ks-expand).
;;; We therefore use ref objects when creating the psvs and fill in
;;; the node tag once it's known.  We use the same ref object for all
;;; PSVs created during a given schema instantiation, so one setf is
;;; all that's needed to put in the node tag once the tag's known.

(defstruct psv-body
  ;; The info about a variable or several variables that have been unified.
  ctxt-tags			;all PSV- tags that refer to this body
  ctxt-type
  ctxt-restrictions
  ctxt-not-sames
  ctxt-value
  ctxt-possibles-cache
  ctxt-original-names		;(var-name schema-name) lists to show
				;  what the vars were called in TF
  ctxt-source-nodes)		;refs to node-tags of nodes where the
				; vars were introduced.

(define-context-accessor psv-body-tags psv-body-ctxt-tags)
(define-context-accessor psv-body-type psv-body-ctxt-type)
(define-context-accessor psv-body-restrictions psv-body-ctxt-restrictions)
(define-context-accessor psv-body-not-sames psv-body-ctxt-not-sames)
(define-context-accessor psv-body-value psv-body-ctxt-value)
(define-context-accessor psv-body-possibles-cache
                         psv-body-ctxt-possibles-cache)
(define-context-accessor psv-body-original-names psv-body-ctxt-original-names)
(define-context-accessor psv-body-source-nodes psv-body-ctxt-source-nodes)

(defparameter *psv-hash-table-initial-size* 100)

(defvar *psv-table* (make-hash-table :size *psv-hash-table-initial-size*
				     :test #'eq)
  "Hashtable to store the mapping of PSV tags to PSV bodies. This is
   context layered to allow tags to point to different bodies in different
   contexts.")

;; PSV hashtable accessor and updater in context.

(defun psv-body (tag)
  (deref-in-context (gethash tag *psv-table*)))

(defsetf psv-body (tag) (new-body)
  `(update-in-context
     #'(lambda (key val)
	 (setf (gethash key *psv-table*) val))
     #'(lambda (key) (gethash key *psv-table*))
     ,tag
     ,new-body))

;; PSV tags

(definit :dm *psv-tag-count* 0)

(defun new-psv-tag ()
  (let* ((n (incf (ctxt-symbol-value '*psv-tag-count*)))
	 (s (intern (concatenate 'string "PSV-" (prin1-to-string n)))))
    (setf (get s :tag-number) n)
    s))

(defun psv-tag-number (psv-tag)
  (get psv-tag :tag-number))

;; PSV types - These are stored in a hash table. Types are defined in the TF.

(defparameter *psv-types-table-initial-size* 100)

(defvar *psv-types-table* (make-hash-table :size *psv-types-table-initial-size*
					   :test #'eq))

(defun psv-add-type (type-name values)
  (setf (gethash type-name *psv-types-table*) values))

(defun psv-get-possibles-for-type (type-name)
  (gethash type-name *psv-types-table*))

(defun psv-get-type-from-restrictions (actor)
  ;; /\/ Won't work for e.g. ?{or ?{type t1} ?{type t2}}.
  (and (act:actorp actor)
       (or (and (eq (act:actfn actor) (actor-rename 'type))
		(car (act:actargs actor)))
	   (and (eq (act:actfn actor) 'act:actand)
		(some #'psv-get-type-from-restrictions
		      (act:actargs actor))))))

#+:undef
(defun psv-get-type-from-restrictions (actor)
  ;; /\/ Won't work for e.g. ?{or ?{type t1} ?{type t2}}.
  (if (not (act:actorp actor))
      nil
    (if (eq (act:actfn actor) (actor-rename 'type))
	(car (act:actargs actor))
      (if (or (eq (act:actfn actor) 'act:actand)
	      (eq (act:actfn actor) 'act:actor))
	  (some #'psv-get-type-from-restrictions
	        (act:actargs actor))
	nil))))

(defun psv-get-filter-from-restrictions (r type-name)
  ;; Since we get a variable's initial list of possible values from its
  ;; type, we can save time if we don't also use that type to restrict
  ;; the possible values.  (We'd be intersecting the type with itself.)
  ;; So our task here is to return a version of the var's restrictions
  ;; that doesn't include ?{type T} for vars of type T.  The most common
  ;; case is there the restriction is just ?{type T}, for which no filtering
  ;; is needed; and for now that's the only case we handle.  /\/
  (and (act:actorp r)
       (cond ((eq (act:actfn r) (actor-rename 'type))
	      (if (eq (car (act:actargs r)) type-name)
		  nil
		r))
	     (t
	      r))))

(defvar *psv-current-source-node-ref* nil)

(defun psv-set-current-source-node (node-tag)
  (assert (reference-p *psv-current-source-node-ref*))
  (assert (eq (ref-value *psv-current-source-node-ref*) :unknown))
  (setf (ref-value *psv-current-source-node-ref*) node-tag)
  (setq *psv-current-source-node-ref* nil))

(defun psv-create-psv (schema name bindings)
  (assert (reference-p *psv-current-source-node-ref*))
  (let* ((tag (new-psv-tag))		;was (pop-gensym "PSV-")
	 (psv-s (make-psv-body))
	 (tuple (get-tuple name bindings))
	 (restrictions (var-restrictions tuple))
	 (type (psv-get-type-from-restrictions restrictions))
	 (filter (psv-get-filter-from-restrictions restrictions type))
	 (possibles
	  (if filter
	      (remove-if-not
	        #'(lambda (x) (obmatch filter x)) ;/\/ *bindings* ???
		(psv-get-possibles-for-type type))
	    (psv-get-possibles-for-type type))))
    (setf (psv-body tag) psv-s)
    (setf (psv-body-tags psv-s) (list tag))
    (setf (psv-body-original-names psv-s) `((,name ,(schema-name schema))))
    (setf (psv-body-source-nodes psv-s) (list *psv-current-source-node-ref*))
    (setf (psv-body-type psv-s) type)
    (setf (psv-body-restrictions psv-s) restrictions)
    (setf (psv-body-value psv-s) (var-value tuple))
    (setf (psv-body-possibles-cache psv-s) possibles)
    (cond ((null possibles)
	   (dev-debug :error "~S, type ~S, from ~S in ~S cannot have a value."
		      tag type name (schema-name schema))
	   (assert (eq (psv-body-value psv-s) :undef))
	   ;; /\/: Leave possibles-cache null
	   )
	  ((null (cdr possibles))
	   ;; Only one possible value, so assign it now.
	   ;; /\/: Probably should avoid creating a PSV in this case.
	   (let ((value (car possibles)))
	     (dev-warn "~S is born bound, to ~S" tag value)
	     (assert (or (eq (psv-body-value psv-s) :undef)
			 (eql (psv-body-value psv-s) value)))
	     (setf (psv-body-value psv-s) value))))
    (dev-note :psv :detail "Created ~W:~%~W~%" tag psv-s)
    tag))

(defun psv-create-binding-tuple (psv)
  "Returns a binding tuple for use by the matcher for the given PSV."
  (let ((psv-s (psv-body psv)))
    (make-var
      :name psv
      :value (psv-body-value psv-s)
      :not-sames (psv-body-not-sames psv-s)
      :restrictions (psv-body-restrictions psv-s)
      :possible-values (psv-body-possibles-cache psv-s))))

#+:undef
(defun psv-create-binding-tuple (psv)
  "Returns a binding tuple for use by the matcher for the given PSV."
  (let ((psv-s (psv-body psv)))
    (make-tuple psv
		(psv-body-value psv-s)
		(psv-body-not-sames psv-s)
		(psv-body-restrictions psv-s))))

#+:undef
(defun psv-p (x)
  "Returns T if x is a psv tag (ie starts with PSV-)"
  (let (x-s)
    (and (symbolp x)
	 (> (length (setq x-s (symbol-name x))) 4)
	 (string= x-s "PSV-" :end1 4))))

(defun psv-p (x)
  "Returns T if x is a psv tag (ie starts with PSV-)"
  (and (symbolp x)
       (let ((x-s (symbol-name x)))
	 (declare (type string x-s))	;simple-string? /\/
	 (macrolet ((%char= (c1 c2)	;matters in AKCL/GCL /\/
		      `(char= (the character ,c1) (the character ,c2))))
	   (and (> (length x-s) 4)
		(%char= #\P (char x-s 0))
		(%char= #\S (char x-s 1))
		(%char= #\V (char x-s 2))
		(%char= #\- (char x-s 3)))))))

;; Takes a pattern or an OBASE item and returns a new pattern with any PSVs
;; replaced with given actors, or if the PSV has a value, then the value
;; instead.
;;
;; /\/: Does not handle the case where a PSV's value contains another PSV.
;;
(defun psv-actorise-pattern (pattern)
  (%psv-actorise-pattern 
    (if (itemp pattern)
	(oplan-obase::dbitem-identifier pattern)
      pattern)))

(defun %psv-actorise-pattern (pattern)
  (if (atom pattern)
      (if (psv-p pattern)
	  (let ((psv-s (psv-body pattern)))
	    (if (psv-body-p psv-s)
		(let ((psv-value (psv-body-value psv-s)))
		  (if (eq psv-value :undef)
		      (make-given-actor pattern)
		    psv-value))
	      ;; PSV does not exist in this context - just stops it breaking.
	      pattern))
	pattern)
    (recons pattern (%psv-actorise-pattern (car pattern))
	            (%psv-actorise-pattern (cdr pattern)))))

;; Takes a pattern, and returns a new pattern with any PSVs replaced with their
;; values, or their current restrictions if no value.
;;
;; /\/: Does not handle the case where a PSV's value contains another PSV.
;;
(defun psv-replace-psvs-in-pattern (pattern)
  (%psv-replace-psvs-in-pattern
    (if (itemp pattern)
	(oplan-obase::dbitem-identifier pattern)
      pattern)))

(defun %psv-replace-psvs-in-pattern (pattern)
  (if (atom pattern)
      (if (psv-p pattern)
	  (let ((psv-s (psv-body pattern)))
	    (if (psv-body-p psv-s)
		(let ((psv-value (psv-body-value psv-s)))
		  (if (eq psv-value :undef)
		      (psv-body-restrictions psv-s)
		    psv-value))
	      ;; PSV does not exist in this context - just stops it breaking.
	      pattern))
	pattern)
    (recons pattern (%psv-replace-psvs-in-pattern (car pattern))
	            (%psv-replace-psvs-in-pattern (cdr pattern)))))

(defun psv-update-psv-body-from-binding-tuple (tuple)
  (let ((psv-s (psv-body (var-name tuple))))
    (declare (ignore psv-s))		;/\/ so far
    (break "In psv-update-psv-body-from-binding-tuple")))

(defun psv-has-value-p (psv)
  (not (eq (psv-body-value (psv-body psv)) :undef)))

(defun psv-get-value (psv)
  (psv-body-value (psv-body psv)))

(defun psv-get-possibles (psv)
  (let ((psv-s (psv-body psv)))
    (if (eq (psv-body-value psv-s) :undef)
	(psv-body-possibles-cache psv-s)
      ':ALREADY-BOUND)))

;; This is an OBase actor for handling types. The variable is the first
;; arg (or rather the thing to match), and the second arg is the type specifier
;;
(setf (actor-rename 'type) 'act-type)

(defun act-type (thing type-specifier)
  (let ((objects (psv-get-possibles-for-type type-specifier)))
    (if (psv-p thing)
	(let ((psv-s (psv-body thing)))
	  (if (eq (psv-body-value psv-s) :undef)
	      ;; The PSV has no value currently, so see if there is an
	      ;; intersection between the PSV's type and this type. If so,
	      ;; we  are in business potentially.
	      ;; /\/: Should we check its possibles-cache instead of its
	      ;; type?  Yes, because we want to know whether the object
	      ;; represented by the PSV can be an instance of the type.
	      #+:undef
	      (intersection (psv-get-possibles-for-type (psv-body-type psv-s))
			    objects)
	      (intersection (psv-body-possibles-cache psv-s) objects)
	    ;; The PSV has a value, so see if it is of the same type.
	    (member (psv-body-value psv-s) objects)))
      ;; Otherwise just use obmatch to see if there is a possible match in
      ;; the type with thing.
      (some #'(lambda (x)
		(let ((act:*bindings* nil))
		  (obmatch thing x)))
	    objects))))

;; Adds value as a restriction on PSV if possible. If value is a PSV then the
;; not-sames lists are adjusted. Returns t or nil.
;;
(defun psv-add-restriction (var type value)
  (dev-note :psv :detail "Restriction: ~W ~W ~W~%" var type value)
  (assert (eq type :NOT))
  (if (psv-p value)
      ;; Value is a PSV tag.
      (restrict-psv-not-psv var value)
    ;; Value is not a PSV tag.
    (restrict-psv-not-value var value)))

(defun restrict-psv-not-psv (var other-var)
  ;; Value is a PSV so adjust the not-sames list of both if able to.
  (let ((psv-s (psv-body var))
	(other-psv-s (psv-body other-var)))
    ;; The other-var sometimes acquires a value after we decide to
    ;; restrict against it and before we get around to performing the
    ;; restriction by calling psv-add-restriction.  If it happens, we
    ;; restrict against the value instead.
    (unless (eq (psv-body-value other-psv-s) :undef)
      (return-from restrict-psv-not-psv
	(restrict-psv-not-value var (psv-body-value other-psv-s))))
    (if (eq other-psv-s psv-s)
	;; The two PSVs point to the same body, thus they are
	;; supposed to be equal!
	nil
      (if (and (eql (psv-body-value psv-s)
		    (psv-body-value other-psv-s))
	       (not (eq (psv-body-value psv-s) :undef)))
	  ;; The two PSVs have the same value, so again they cannot
	  ;; be made unequal.
	  nil
	(if (member other-var (psv-body-not-sames psv-s))
	    ;; Already not same
	    t
	  ;; Mutually constrain the vars to have different values.
	  (progn
	    (when (disjoint-sets-p (psv-body-possibles-cache psv-s)
				   (psv-body-possibles-cache other-psv-s))
	      (break "Unnecessary psv restriction ~S not ~S" var other-var))
	    (push other-var (psv-body-not-sames psv-s))
	    (push var (psv-body-not-sames other-psv-s))
	    ;; Note that other-var should not already have a value,
	    ;; because if it had one, we would have constrained against
	    ;; the value, not the var.  However, it may get a value 
	    ;; if a value is removed from its possibles-cache, below.
	    (and (or ;; If var has no value, nothing to do directly.
		     (eq (psv-body-value psv-s) :undef)
		     ;; Else, see what happens when other-var loses
		     ;; var's value from its possibles list.
		     (remove-a-value-from-possibles-cache
		       (psv-body-value psv-s)
		       other-var))
		 (restrict-via-not-sames psv-s)
		 (restrict-via-not-sames other-psv-s))))))))

(defun restrict-psv-not-value (var value)
  ;; Restricting binding of var to not be value.
  (let ((psv-s (psv-body var)))
    (if (eql (psv-body-value psv-s) value)
	;; Already set to this value.
	nil
      (if nil
	  ;; /\/ Changed psv-descriptions in test-results.
	  ;; Was: (not (eq (psv-body-value psv-s) :undef))
	  ;; Already set to a different value.
	  t
	(let ((possibles (psv-body-possibles-cache psv-s)))
	  (if (not (member value possibles))
	      ;; No work needed.
	      t
	    (progn
	      (setf (psv-body-possibles-cache psv-s)
		    (remove value possibles))
	      ;; Add ?{not <value>} to restriction list.
	      (setf (psv-body-restrictions psv-s)
		    (make-and-actor (make-not-value-actor value)
				    (psv-body-restrictions psv-s)))
	      (if (null (psv-body-possibles-cache psv-s))
		  nil
		(if (null (cdr (psv-body-possibles-cache psv-s)))
		    ;; Reduced to a choice of 1, so set value to
		    ;; this and then propogate through all the
		    ;; not-sames.
		    (progn
		      (setf (psv-body-value psv-s)
			    (car (psv-body-possibles-cache psv-s)))
		      (apply #'remove-a-value-from-possibles-cache
			     (psv-body-value psv-s)
			     (psv-body-not-sames psv-s)))
		  ;; Still some possible values left.
		  (restrict-via-not-sames psv-s)
		  #+:undef
		  t)))))))))

;; Go through a list of PSVs and remove value from the possibles-cache of each
;; one, if present. Called when we set a PSV value, and that PSV is not the
;; same as some other PSVs. Returns t for success or nil for a problem.
;;
(defun remove-a-value-from-possibles-cache (value &rest psvs)
  (dolist (psv psvs t)
    (let ((psv-s (psv-body psv)))
      (when (and (member value (psv-body-possibles-cache psv-s))
		 (not (psv-add-restriction psv :NOT value)))
	(return nil)))))


;;; Restricting via not-sames

(defstruct eset ;exclusion set
  vars		;vars (psv-bodies) whose values must be pairwise disjoint
  values	;union of their possible value sets
  not-sames)	;intersection of their not-sames sets

(defun restrict-via-not-sames (psv-s)
  (exclude-via-not-sames psv-s)
  (if (null (psv-body-possibles-cache psv-s))
      nil
    (if (null (cdr (psv-body-possibles-cache psv-s)))
	(progn
	  (setf (psv-body-value psv-s)
		(car (psv-body-possibles-cache psv-s)))
	  (apply #'remove-a-value-from-possibles-cache
		 (psv-body-value psv-s)
		 (psv-body-not-sames psv-s)))
      t)))

(defun exclude-via-not-sames (psv-s)
  ;; Either does nothing or sets (psv-body-possibles-cache psv-s)
  ;; to contain zero or one values.
  (let* ((possibles (psv-body-possibles-cache psv-s))
	 (number-possible (length possibles))
	 (esets '()))
    (dolist (ns-tag (psv-body-not-sames psv-s))
      (let* ((ns (psv-body ns-tag))
	     (ns-possibles (psv-body-possibles-cache ns)))
	;; Proceed only if ns must take a value also wanted by psv-s,
	;; i.e. if psv-s has no possible value that's not also a possible
	;; value of ns.
	(when (null (set-difference ns-possibles possibles))
	  ;; Add ns to any esets it belongs in.
	  (dolist (eset esets)
	    (when (not (try-extending-eset eset ns psv-s
					   possibles number-possible))
	      ;; At least one var can't be bound
	      (return-from exclude-via-not-sames nil)))
	  ;; Make a new eset containing only ns.
	  (push (make-eset
		  :vars (list ns)
		  :values ns-possibles
		  :not-sames (psv-body-not-sames ns))
		esets))))))

;;; Try-extending-eset tries to add variable NS (from the not-sames list
;;; of PSV-S) to ESET.  If it can, and an impossibility is discovered,
;;; try-extending-eset returns NIL; otherwise it returns T.  Note that
;;; the T result includes the case where NS cannot be added to ESET.

;;; POSSIBLES and NUMBER-POSSIBLE are passed along from exclude-via-
;;; not-sames just to avoid recomputing them.

(defun try-extending-eset (eset ns psv-s possibles number-possible)
			  ; -> T or NIL
  (let ((ns-possibles (psv-body-possibles-cache ns)))
    ;; See if ns must have a different value than all other
    ;; variables in eset.
    (if (not (or (disjoint-sets-p ns-possibles (eset-values eset))
		 (member ns (eset-not-sames eset) :key #'psv-body)))
	;; Can't add ns to eset.
	t
      (progn
	;; Ns must have a different value, so add it to eset.
	(push ns (eset-vars eset))
	(setf (eset-values eset)
	      (union ns-possibles
		     (eset-values eset)))
	(setf (eset-not-sames eset)
	      (intersection (psv-body-not-sames ns)
			    (eset-not-sames eset)))
	(let ((esize (length (eset-vars eset))))
	  ;; Esize is the number of different values that will be
	  ;; taken by the eset.
	  (if (not (< esize number-possible))
	      ;; There will be no values left for psv-s.
	      (progn
		(dev-warn "Not-sames ~S/~S take all values from ~S/~S"
		    (mapcar #'psv-body-tags (eset-vars eset))
		    (psv-body-not-sames psv-s)
		    (psv-body-tags psv-s)
		    (psv-body-original-names psv-s))
		(setf (psv-body-possibles-cache psv-s) nil)
		nil)
	      (let ((number-evalues (length (eset-values eset))))
		(if (= number-evalues esize)
		    ;; We know exactly which values the eset will take
		    (let ((remaining-possibles
			   (set-difference possibles (eset-values eset))))
		      (assert (not (null remaining-possibles)))
		      (dev-warn "Not-sames ~S reduce possibles of ~S/~S~%~
                                 from ~S to ~S"
			  (psv-body-not-sames psv-s)
			  (psv-body-tags psv-s)
			  (psv-body-original-names psv-s)
			  (psv-body-possibles-cache psv-s)
			  remaining-possibles)
		      (setf (psv-body-possibles-cache psv-s)
			    remaining-possibles)
		      t)
		  (if (not (<= esize (length (eset-values eset))))
		      ;; There aren't enough values for the variables in eset.
		      (progn
			(dev-warn "Eset ~S has too few values in ~S"
			    (mapcar #'(lambda (v)
					(list (psv-body-tags v)
					      (psv-body-original-names v)))
				    (eset-vars eset))
			      (eset-values eset))
			;; /\/: We have to do something to ensure failure
			(setf (psv-body-possibles-cache psv-s)
			      nil)
			nil)
		    ;; Nothing interesting happened.
		    t)))))))))


;; Merges two plan state bodies together into one new one which is returned.
;; Assumes that any requirements have already been checked (ie if have value
;; field set, that set equally). Could return nil if there is a problem
;; somewhere.
;;
(defun amalgamate-two-psvs (psv-1 psv-2)
  (let ((new-psv-s (make-psv-body))
	(psv-s-1 (psv-body psv-1))
	(psv-s-2 (psv-body psv-2)))
    (assert (eq (psv-body-type psv-s-1) (psv-body-type psv-s-2)))
    (setf (psv-body-tags new-psv-s)
	  (stable-union (psv-body-tags psv-s-1)
			(psv-body-tags psv-s-2)))
    ;; Should really set to the most specific type 
    ;; -- or the least specific type? /\/
    (setf (psv-body-type new-psv-s) (psv-body-type psv-s-1))
    (setf (psv-body-restrictions new-psv-s)
	  (conjoin-restrictions (psv-body-restrictions psv-s-1)
				(psv-body-restrictions psv-s-2)))
    (setf (psv-body-not-sames new-psv-s)
	  (stable-union (psv-body-not-sames psv-s-1)
			(psv-body-not-sames psv-s-2)))
    (dolist (not-same-psv (stable-set-difference (psv-body-not-sames psv-s-1)
						 (psv-body-not-sames psv-s-2)))
      (unless (psv-add-restriction not-same-psv :NOT psv-2)
	(return-from amalgamate-two-psvs nil)))
    (dolist (not-same-psv (stable-set-difference (psv-body-not-sames psv-s-2)
						 (psv-body-not-sames psv-s-1)))
      (unless (psv-add-restriction not-same-psv :NOT psv-1)
	(return-from amalgamate-two-psvs nil)))
    (setf (psv-body-value new-psv-s)
	  (or (and (not (eq (psv-body-value psv-s-1) :undef))
		   (psv-body-value psv-s-1))
	      (psv-body-value psv-s-2)))
    (setf (psv-body-possibles-cache new-psv-s)
	  (stable-intersection (psv-body-possibles-cache psv-s-1)
			       (psv-body-possibles-cache psv-s-2)))
    (assert (not (null (psv-body-possibles-cache new-psv-s))))
    (setf (psv-body-original-names new-psv-s)
	  (stable-union (psv-body-original-names psv-s-1)
			(psv-body-original-names psv-s-2)
			:test #'equal))
    (setf (psv-body-source-nodes new-psv-s)
	  (stable-union (psv-body-source-nodes psv-s-1)
			(psv-body-source-nodes psv-s-2)))
    new-psv-s))

;;; The fancy conjunction routine is needed because otherwise the
;;; restrictions can become very large when a number of PSVs are
;;; merged into one.  /\/: This isn't really the right place for
;;; it, though; and it should have a more general-sounding name.

(defun conjoin-restrictions (r1 r2)
  (cond ((eq (actfn r1) 'act:actand)
	 (if (eq (actfn r2) 'act:actand)
	     (make-function-actor
	       'act:actand
	       (union (actargs r1) (actargs r2) :test #'equal))
	   (if (member r2 (actargs r1) :test #'equal)
	       r1
	     (make-function-actor
	       'act:actand
	       (cons r2 (actargs r1))))))
	((eq (actfn r2) 'act:actand)
	 (if (member r1 (actargs r2) :test #'equal)
	     r2
	   (make-function-actor
	     'act:actand
	     (cons r1 (actargs r2)))))
	((equal r1 r2)
	 r1)
	(t
	 (make-function-actor 'act:actand (list r1 r2)))))

;; Set a PSV to value (which may be a PSV itself). If not able to, return nil;
;; else t.
;;
(defun psv-set-value (psv value)
  (let ((psv-s (psv-body psv)))
    (if (psv-p value)
	;; Trying to set two PSVs to be equal.
	(if (member value (psv-body-not-sames psv-s))
	    ;; Trying to make two not equal PSVs equal!
	    nil
	  (let ((other-psv-s (psv-body value))
		new-psv-s)
	    (if (eq psv-s other-psv-s)
		;; Already point to the same body, so return t.
		:ALREADY-EQUAL
	      (if (null (intersection
			 (psv-body-possibles-cache psv-s)
			 (psv-body-possibles-cache other-psv-s)))
		  ;; Since there is no intersection between the
		  ;; possibles-cache they cannot ever have the same value
		  nil
		(if (and (not (eq (psv-body-value psv-s) :undef))
			 (not (eq (psv-body-value other-psv-s) :undef))
			 (not (eql (psv-body-value psv-s)
				   (psv-body-value other-psv-s))))
		    ;; PSVs have values, but not the same, so
		    nil
		  (if (setq new-psv-s (amalgamate-two-psvs psv value))
		      (progn
			(assert (member psv (psv-body-tags new-psv-s)))
			(assert (member value (psv-body-tags new-psv-s)))
			(dolist (tag (psv-body-tags new-psv-s))
			  (setf (psv-body tag) new-psv-s))
			t)
		    nil))))))
      ;; Trying to set PSV to a value.
      (if (eql (psv-body-value psv-s) value)
	  :ALREADY-BOUND
	(if (eq (psv-body-value psv-s) :undef)
	    (if (member value (psv-body-possibles-cache psv-s))
		(progn
		  (setf (psv-body-value psv-s) value)
		  (apply #'remove-a-value-from-possibles-cache
			 value
			 (psv-body-not-sames psv-s)))
	      ;; Can't set a value if its not in the possibles-cache.
	      nil)
	  ;; If it has a value not equal to value, and not :undef, then
	  ;; it must be a different value, so fail.
	  nil)))))

;; Psv-set-possibles is called from KS-USER to restrict the possible
;; values of a variable or, rather, to remove some of the values that
;; are currently possible.  If psv-set-possibles succeeds in removing
;; them, it returns true; otherwise, it returns nil.
;;
;; Note that we may not actually remove any values.  We might just be
;; changing the order of the values in the possibles cache.
;;
(defun psv-set-possibles (psv values)
  (assert (not (psv-has-value-p psv)))
  (assert (not (null values)))
  (let* ((psv-s (psv-body psv))
	 (current-possibles (psv-body-possibles-cache psv-s))
	 (invalid-suggestions
	  (set-difference values current-possibles)))
    ;; /\/: We're assuming the the ks-bind that called us has more
    ;; up-to-date information than the psv-body, but perhaps that's
    ;; not always the case.  If it isn't always the case, this
    ;; assertion will fail.
    (assert (null invalid-suggestions))
    (cond ((length=1 values)
	   ;; Change the possibles cache anyway so it looks right to the
	   ;; user
	   (setf (psv-body-possibles-cache psv-s)
		 values)
	   ;; Only one possibility left, so we can set the value (or try to).
	   (psv-set-value psv (car values)))
	  (t
	   ;; Remove any excluded values.  This may suffice...
	   (setf (psv-body-possibles-cache psv-s)
		 values)
	   ;; This may help, though.  [jd 20 May 99]
	   (restrict-via-not-sames psv-s)))))


;;;; Fake transaction code /\/

(defun psv-begin-var-transaction ()
  )

(defun psv-commit-var-transaction ()
  )

(defun psv-abort-var-transaction ()
  )


;;;; PSV descriptions of various sorts.

;;; /\/: It's a pain to have several different description formats,
;;; and this should be cleaned up at some point.

(defun psv-make-a-list-of-the-psvs ()
  (let (result bodies-to-tags)
    (maphash #'(lambda (k v)
		 (setq v (deref-in-context v))
		 (if v
		     (let ((entry (assoc v bodies-to-tags)))
		       (if entry
			   (setf (cdr entry) (cons k (cdr entry)))
			   (setq bodies-to-tags (acons v (list k)
						       bodies-to-tags))))))
	     *psv-table*)
    (setq result
	  (mapcar
	     #'(lambda (x)
		 (let ((*print-pretty* t) 	;e.g., for actor formatting
		       (*print-level* nil)
		       (*print-length* nil))
		   (xp-format nil
		      "~{~W~^, ~}=~
                       ~@<#<PSV ~;~
                            names ~W, ~:_~
                            type ~W, ~:_~
                            value = ~W, ~:_~
                            not-sames = ~W, ~:_~
                            restrictions = ~W, ~:_~
                            possibles = ~W~;>~:>"
		      (cdr x)
		      (psv-body-original-names (car x))
		      (psv-body-type (car x))
		      (psv-body-value (car x))
		      (psv-body-not-sames (car x)) 
		      (psv-body-restrictions (car x))
		      (psv-body-possibles-cache (car x)))))
	     bodies-to-tags))
    result))

(defun psv-get-psv-descriptions ()
  ;; So far, used only by KS-CHECK-PLAN (for the auto-tester).
  (let ((psvs (get-psv-tags)))
    (list
       ;; A (partial) description of each psv.
       ;;
       ;; /\/: We don't just use the psv-body struct in part because
       ;; it doesn't pretty print the way we want.  Using a list is
       ;; easier than the alternatives, at this point.
       ;;
       (mapcar #'(lambda (psv)
		   (let ((body (psv-body psv)))
		     (list psv
			   (psv-body-type body)
			   (psv-body-value body)
			   (psv-body-not-sames body)
			   ;(psv-body-restrictions body)
			   (psv-body-possibles-cache body))))
	       psvs)
       ;; The equivalence classes resulting from unification.
       ;; We get a list ((body psv-tag...)...) where each sublist
       ;; contains psv tags that have the same body.
       (let ((equivs (equivalence-classes psvs :key #'psv-body)))
	 (assert (every #'(lambda (equiv)
			    (set-eql (cdr equiv)
				     (psv-body-tags (psv-body (cadr equiv)))))
			equivs))
	 (mapcar #'cdr equivs)))))

(defun get-psv-tags ()
  (let ((psvs '()))
    (maphash #'(lambda (k v)
		 (when (deref-in-context v) ;exists in this context?
		   (push k psvs)))
	     *psv-table*)
    ;; Use a standard order so it doesn't depend on the details of how
    ;; hash tables are implemented.
    (sort psvs #'<
	  :key #'psv-tag-number)))

;;; Descriptions for KS-USER.

;;; /\/: It should at least be possible to use the same description
;;; format for KS-USER and KS-CHECK-PLAN.  If this is ever done, it
;;; should probably follow this, the KS-USER version.

;;; /\/: Here, a description is a plist with slot names (keywords) as
;;; keys.  There's one description per psv-body.  And additional key
;;; is :TAGS which says what tags point to a body.  Using a plist
;;; lets us be independent of the order in which slots appear
;;; (at least some of the time).

;;; /\/: Note that get-psv-tags returns the tags in "canonical
;;; description order" and that equivalence-classes preserves
;;; that order in the subsets of psv-tags that it creates.

(defun psv-get-psv-descriptions-for-user ()
  (let* ((psvs (get-psv-tags))
	 (equivs (equivalence-classes psvs :key #'psv-body)))
    ;; The equivalence classes result from unification.
    ;; We get a list ((body psv-tag...)...) where each sublist
    ;; contains psv tags that have the same body.  Below, the
    ;; first tag in each class is used to look up the body.
    (flet ((c (items) (canonical-description-order items))
	   (v (psvs) (sort (copy-list psvs) #'< :key #'psv-tag-number)))
      (sort
        (mapcar
          #'(lambda (equiv)
	      (let ((body (car equiv))
		    (tags (cdr equiv)))
		(assert (set-eql tags (psv-body-tags body)))
		;; Now here's the description
		(list :tags            tags
		      :original-names  (c (psv-body-original-names body))
		      :type            (psv-body-type body)
		      :value           (psv-body-value body)
		      :sources         (c (psv-body-sources body))
		      :restrictions    (psv-body-restrictions body)
		      :not-sames       (v (psv-body-not-sames body))
		      :possibles-cache (psv-body-possibles-cache body))))
	  equivs)
	#'<
	:key #'(lambda (d) (psv-tag-number (first (getf d :tags))))))))

(defun psv-body-sources (body)
  (let ((original-names (psv-body-original-names body))
	(source-nodes (mapcar #'ref-value (psv-body-source-nodes body))))
    (mapcar #'(lambda (node-tag)
		(let* ((n (get-node node-tag))
		       (r (n-reason n))
		       (s (n-expansion-schema n)) ;schema-name
		       (tf-var-name
			(first (find s original-names :key #'second))))
		  (assert tf-var-name)
		  `(,tf-var-name ,s ,node-tag ,(n-type n) ,(n-pattern n) ,r)))
	    source-nodes)))

;;;; Printing functions.

(defun psv-print-psv-table (&optional (stream t))
  (let ((psvs (get-psv-tags))
	(*print-pretty* t)
	(*print-length* nil)
	(*print-level* nil))
    (while psvs
      (let* ((psv (car psvs))
	     (body (psv-body psv))
	     (tags (remove-if-not #'(lambda (tag) (eq (psv-body tag) body))
				  psvs)))
	(xp-format stream "~{~W~^, ~}:~%~W~%"
		    tags
		    (psv-body psv))
	(setq psvs (stable-set-difference psvs tags))))))

;;;; Pretty printing functions.

(defun psv-pretty-print-body (stream body)
  (xp-format stream
      "~@<PSV-BODY ~;~
          Names = ~W, ~:_~
          Tags = ~W, ~:_~
          Type = ~W, ~:_~
          Value = ~W, ~:_~
          Sources: ~W, ~:_~
          Restrictions = ~W, ~:_~
          Not sames = ~W, ~:_~
          Possibles cache = ~W;~
          ~:>"
      (remove-duplicates (mapcar #'first (psv-body-original-names body)))
      (psv-body-tags body)
      (psv-body-type body)
      (psv-body-value body)
      (psv-body-sources body)
      (psv-body-restrictions body)
      (psv-body-not-sames body)
      (psv-body-possibles-cache body)))

(eval-when (load eval)
  (set-pretty-printer 'psv-body #'psv-pretty-print-body))

;;;; Init routine

(define-initializer :dm psv-clear-psv-tables ()
  (clrhash *psv-table*)
  (clrhash *psv-types-table*))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;

