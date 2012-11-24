;;;; File: analysis.lsp
;;; Contains: TF compiler's domain analysis
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: July 1994
;;; Updated: Thu Mar 18 23:00:20 1999 by Jeff Dalton
;;; Copyright: (c) 1994, AIAI, University of Edinburgh

(in-package :oplan-tf-compiler)

;;; The analysis phase determines action and effect levels (see levels.lsp).
;;; It also constructs a number of tables as support for "domain services"
;;; (see domain-services-package.lsp).

;;; The following mappings are needed by other parts of O-Plan:
;;;
;;; schema name -> schema
;;;   Used only to look up task schemas and so probably isn't really
;;;   necessary.
;;;
;;; expand name -> schemas
;;;   Used to determine what schemas might be able to expand an action.
;;;   The expand name is the first word of the schema's expands pattern,
;;;   and only schemas that have expands patterns are in the table.
;;;
;;; oufe effect name -> (effect . schema)s
;;;   Used to determine what schemas might be able to satisfy an achieve.
;;;
;;; expand name -> possible effect names
;;;   Used when evaluating :wait-on-effect triggers to see what effects
;;;   might be introduced by an :expand agenda entry.
;;;
;;; oufe effect name -> possible effect names
;;;   Used when evaluating :wait-on-effect triggers to see what effects
;;;   might be introduced by an :achieve agenda entry.
;;;
;;; action name -> level
;;;   Used to set the level when posting an :expand agenda entry.
;;;
;;; effect name -> level
;;;   Used to set the level when posting an :achieve or :condition
;;;   agenda entry.
;;;
;;; Some other mappings are construted for internal use, e.g. as steps
;;; on the way to constructing some of the mappings above.
;;;

;;;; Mapping tables

;;; The mapping macro lets us use something other than global
;;; variables, if we want to.  For instance, the tables could be
;;; stored in slots in the domain structure (though we might then
;;; need a special variable bound to the domain).

(defvar *all-mapping-tables* '())

(defmacro define-mapping (from to)	;/\/ &key to ?
  (check-type from symbol)
  (check-type to symbol)
  (let ((var-name (concat-name "*" from "->" to '#:-table "*"))
	(fn-name (concat-name from "->" to)))
    `(progn
       (nconcf-new *all-mapping-tables* ',var-name)
       (defparameter ,var-name (make-hash-table :test #'eq))
       (defun ,fn-name (,from)
	 (gethash ,from ,var-name))
       (defsetf ,fn-name (,from) (,to)
	 `(setf (gethash ,,from ,',var-name) ,,to))
       `(:mapping ,',from ,',to))))

(defmacro mapping (from to)
  ;; Macro calls might not be expanded until run-time, when *package*
  ;; will be the oplan package.  /\/
  (intern (concat-string "*" from "->" to '#:-table "*") :oplan-tf-compiler))

(defmacro mapping-function (from to)
  (intern (concat-string from "->" to) :oplan-tf-compiler))

(defun init-mapping-tables ()
  (dolist (var *all-mapping-tables*)
    (clrhash (symbol-value var))))


;;; /\/: Perhaps we should use oufe-name instead of effect-name in
;;; the defintions below.

;;; Basic schema tables needed outside the compiler

(define-mapping schema-name schema)
(define-mapping expand-name schemas)
(define-mapping effect-name effect+schema_s)

;;; Some other basic schema tables

(define-mapping effect-name schemas)
(define-mapping action-name schemas)	;used for levels

;;; Possible-effects tables

(define-mapping expand-name possible-effect-names)
(define-mapping effect-name possible-effect-names)


;;;; Analyze-domain

(defun analyze-domain (domain)
  (init-mapping-tables)
  (set-schema-action-names domain)	;for levels /\/
  (construct-basic-schema-tables domain)
  (construct-possible-effects-tables domain)
  (process-domain-levels domain))


(defun construct-basic-schema-tables (domain)

  (dolist (s (domain-schemas domain))

    ;; schema name -> schema
    (let ((n (schema-name s)))
      ;; Can't have 2 schemas with the same name.
      (assert (null (schema-name->schema n)))
      (setf (schema-name->schema n) s))

    ;; expand name -> schemas
    (when (schema-expands s)
      (nconcf (expand-name->schemas (first (schema-expands s)))
	      (list s)))

    ;; oufe effect name -> (effect . schema)s
    ;; oufe effect name -> schemas
    (when (schema-only-use-for-effects s)
      (dolist (effect (schema-only-use-for-effects s))
	(let ((effect-name (first (effect-pattern effect))))
	  ;; The same effect name might come up > once, hence the nconcf-new.
	  (nconcf (effect-name->effect+schema_s effect-name)
		  (list (cons effect s)))
	  (nconcf-new (effect-name->schemas effect-name)
		      s
		      :test #'eq))))

    ;; action name -> schemas
    (nconcf (action-name->schemas (schema-action-name s))
	    (list s))

    ))

;;; Possible-effects tables
;;;
;;; We want:
;;;   expand name -> possible effect names
;;;   oufe effect name -> possible effect names
;;;
;;; We have, constructed above:
;;;   expand name -> schemas
;;;   oufe effect name -> schemas
;;;
;;; So, in both cases, we can get to schemas without much trouble.
;;; We then need to get from schemas to possible effects, where the
;;; possible effects of a schema, S, are the effects directly
;;; introduced by S, plus the effects directly introduced by all
;;; schemas reachable from S via expand and achieve.  So we need:
;;;   schema -> effect names
;;;   schema -> reachable schemas
;;;
;;; Schema -> effect names is easily constructed.  Schema -> reachable
;;; schemas is formed by taking the transitive closure of schema ->
;;; directly reachable schemas (ie, of the schema-successor table).

(define-mapping schema effect-names)		;also used for levels
(define-mapping schema successor-schemas) 	;also used for levels
(define-mapping schema reachable-schemas) 	;could be local /\/
(define-mapping schema possible-effect-names)	;could be local /\/

(defun construct-possible-effects-tables (domain)

  ;; schema -> effect names
  (dolist (s (domain-schemas domain))
    (setf (schema->effect-names s)
	  (get-schema-effect-names s)))	;see levels.lsp

  ;; schema -> [directly reachable] schemas
  (setf (mapping schema successor-schemas)
	(make-schema-successor-table domain))

  ;; schema -> reachable schemas
  (setf (mapping schema reachable-schemas)
	(tclosure (mapping schema successor-schemas)))

  ;; schema -> possible effect names
  (setf (mapping schema possible-effect-names)
	(compose-maps 
	  (domain-schemas domain)
	  #'(lambda (s) (cons s (schema->reachable-schemas s)))	;include self
	  (mapping schema effect-names)))

  ;; expand-name -> possible effect names
  (let ((expand-names (hash-table-keys (mapping expand-name schemas))))
    (setf (mapping expand-name possible-effect-names)
	  (compose-maps
	    expand-names
	    (mapping expand-name schemas)
	    (mapping schema possible-effect-names))))

  ;; oufe effect name -> possible effect names
  (let ((effect-names (hash-table-keys (mapping effect-name schemas))))
    (setf (mapping effect-name possible-effect-names)
	  (compose-maps
	    effect-names
	    (mapping effect-name schemas)
	    (mapping schema possible-effect-names))))

  )

(defun make-schema-successor-table (domain)
  ;; Needs some of the basic schema tables constructed above.
  (map-union
    (domain-schemas domain)
    ;; Successors from expanding nodes
    (compose-maps
       (domain-schemas domain)
       #'get-schema-node-action-names
       (mapping expand-name schemas))
    ;; Successors from achieve
    (compose-maps
       (domain-schemas domain)
       #'get-schema-achieve-names
       #'effect-name->schemas)))


;;;; Some other things we can get from schemas

;;; These routines should be called only when setting up various
;;; mappings.

(defun set-schema-action-names (domain)	;action-name is a slot
  (dolist (s (domain-schemas domain))
    (setf (schema-action-name s)
	  (if (schema-expands s)
	      (car (schema-expands s))
	    (schema-name s)))))		;/\/ questionable

(defun get-schema-achieve-names (s)
  (let ((names '()))
    (dolist (c (schema-conditions s) names)
      (when (or (eq (con-type c) 'achieve)
		(eq (con-type c) 'achievable)) ;for now /\/
	(nconcf-new names (first (con-pattern c)) :test #'eq)))))

(defun get-schema-effect-names (schema)
  (delete-duplicates
    (mapcar #'(lambda (effect) (car (effect-pattern effect)))
	    (append (schema-effects schema)
		    (schema-only-use-for-effects schema)))))

(defun get-schema-node-action-names (schema)
  ;; Collect action-names from the action nodes of the schema.
  (delete-duplicates
    (loop for n in (schema-nodes schema)
	  when (eq (node-type n) 'action)
	  collect (car (node-pattern n)))))


;;;; Combining mappings

;;; A mapping cen be represented by a function or by an eq hash-table.

(defun-inline apply-map (map item)
  (cond ((hash-table-p map)
	 (gethash item map))
	(t
	 (funcall map item))))

;;; Map-union

;;; All of the maps are expected to return sets (so nil counts as the
;;; empty set).

(defun map-union (domain &rest maps)
  (let ((result (make-hash-table :test #'eq)))
    (dolist (k domain result)
      (let ((v '()))
	(dolist (m maps (setf (gethash k result) v))
	  (let ((mk (apply-map m k)))
	    (assert (listp mk))
	    (dolist (e mk)
	      (nconcf-new v e :test #'eq))))))))

;;; Compose-maps

;;; All of the maps are expected to return sets (so nil counts as the
;;; empty set).

(defun compose-maps (domain &rest maps)
  (let ((result (make-hash-table :test #'eq)))
    (dolist (k domain result)
      ;; Apply all the maps to k.
      (do ((m maps (cdr m))
	   (v (list k)
	      ;; v is a set.  Apply the map to each elt of v
	      ;; and make a set of the results.
	      (let ((new-v '()))
		(dolist (u v new-v)
		  (let ((mu (apply-map (first m) u)))
		    (assert (listp mu))
		    (dolist (e mu)
		      (nconcf-new new-v e :test #'eq)))))))
	  ((null m)
	   (setf (gethash k result) v))))))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
