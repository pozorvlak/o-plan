;;;; File: sc-rum.lisp
;;; Contains: Resource Utilization Manager for strictly consumable resources
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: 10 June 1993
;;; Updated: Mon Dec  2 23:54:02 1996 by Jeff Dalton
;;; Copyright: (c) 1992, 1993, AIAI, University of Edinburgh

(in-package :oplan)

;;; This is a much simplified resource manager.  Among the simplifications
;;; are:
;;;
;;;  * Only strictly consumable resources are handled.
;;;  * There is no hierarchy of resource types; all references to
;;;    resources (for both specification and use) must be fully qualified.
;;;  * Resource units and unit synonyms are ignored.
;;;  * Only_use_for_resources is not supported.
;;;  * Resource setting is not allowed; use an overall specification
;;;    in the task schema instead.
;;;
;;; /\/: more...

;;; Some additional simplifications are allowed because the resources
;;; are strictly consumable.  In particular:
;;;
;;;  * The node-end at which consumption occurs is irrelevant.
;;;
;;; /\/: ...


;;;; The CM

(define-constraint-manager sc-rum (variable-free-cm standard-cm))

(register-constraint-manager sc-rum
  :constraint-types '(resource))

;;; Methods

(defmethod cm-init-constraints ((self sc-rum))
  (clear-resources))

(defmethod cm-add-constraints ((self sc-rum) constraints)
  (process-resource-events constraints))

(defmethod cm-constraint-descriptions ((self sc-rum))
  (rut-description))


;;;; Resource-events

;;; Resource events are the entries placed in the resources slot of a
;;; schema by the TF compiler.

(defstruct (resource-event (:type list)
			   (:conc-name re-))
  ;; /\/: This essentially duplicates resource-operation in schema-defs.
  operation	; one of: set, allocates, deallocates, produces, or consumes
  resource	; a resource type, e.g. (resource money), (resource fuel port1)
  range		; a range, from e.g. 0..100
  units		; a name, e.g. pounds, dollars, gallons
  scope		; at or overall
  at)		; a node-end (ie, an etag)


;;;; Resource Usage Table

;;; The resource usage table (RUT) is a hash table indexed by a cons
;;; of the form (node-name . fully-qualified-resource-reference), for
;;; example: (node-3-2 . (resource fuel port_1)).

;;; It was necessary decide whether to have different structs in
;;; different contexts or just different slot values for variable
;;; slots.  The latter option was chosen.  Consequently, anything
;;; that wants to treat new-to-context records specially should look
;;; for uninitialized slot values instead of non-existent structs.

(defvar *resource-usage-table* (make-hash-table :test #'equal))

(defun clear-resources ()
  (clrhash *resource-usage-table*))

;;; The values stored in the RUT are resource-records:

(defstruct (resource-record
	     (:conc-name rr-)
	     (:print-function print-resource-record))
  node		; a node name, e.g. node-3-2
  resource	; a resource type, e.g. (resource money), (resource fuel port1)
  parent	; the parent node's rr for the same resource
  %children	; a list of child node rrs for the same resource
  (%min :undef)	; a non-negative integer
  (%max	:undef)	; a non-negative integer or :inf
  )

(define-context-accessor rr-children rr-%children)
(define-context-accessor rr-min      rr-%min)
(define-context-accessor rr-max      rr-%max)

(defun rr-valid-p (rr)			;valid in current context?
  (not (eq (rr-min rr) :undef)))

(defun print-resource-record (rr stream depth)
  ;; Prints the name of the parent node rather than printing the
  ;; the parent rr struct.
  (if (and (numberp *print-level*) (> depth *print-level*))
      (format stream "#")
    (xp-format stream
       "~@<#<Resource-record ~;~
             ~W ~:_~
             ~W ~:_~
             [~W,~W]; ~:_~
             parent ~W, ~:_~
             children ~W~
           ~;>~:>"
       (rr-node rr)
       (rr-resource rr)
       (rr-min rr)
       (rr-max rr)
       (if (rr-parent rr)
	   (rr-node (rr-parent rr))
	 nil)
       (mapcar #'rr-node (rr-children rr)))))

;;; Descriptions, typically used for storing in test results.

(defun resource-record-description (rr)
  (list (rr-node rr)
	(rr-resource rr)
	(rr-min rr)
	(rr-max rr)
	(if (rr-parent rr) (rr-node (rr-parent rr)) nil)
	(mapcar #'rr-node (rr-children rr))))

(defun rut-description ()
  (filtered-hash-table-alist
    *resource-usage-table*
    #'(lambda (k v)
	(declare (ignore k))		;the information's repeated in v
	(if (rr-valid-p v)
	    (resource-record-description v)
	  nil))))


;;; Resource-record lookup
  
(defun get-resource-record (node resource)
  (gethash (cons node resource) *resource-usage-table*))

(defun set-resource-record (node resource resource-record)
  (setf (gethash (cons node resource) *resource-usage-table*)
	resource-record))

(defsetf get-resource-record set-resource-record)

(defun get-resource-record-else-error (node resource)
  (or (get-resource-record node resource)
      (error "Can't find resource record for ~S ~S."
	     node resource)))


;;;; Resource event processing

;;; /\/: If we have something like "consumes ... at m" in node-n,
;;; we're going to apply the consumption to the rr for node-n-m.
;;; But it's not clear that this is actually right.  We could
;;; regard it instead as comsumption in node-n but at a point
;;; (in time) relative to node-n-m.

(defun process-resource-events (events)
  (every #'process-resource-event
	 (mapcar #'preprocess-resource-event events)))

;;; Preprocess-resource-event checks the event.

;;; Events that are "overall" for a task will end up with node-name = "NODE".

(defun preprocess-resource-event (resource-event)
  (validate-resource-event resource-event)
  resource-event)

(defun validate-resource-event (re)
  (let ((node-name (etag-node (re-at re))))
    (cond ((or (string-equal node-name "NODE")
	       (length=1 (n-numbers (get-node node-name))))
	   ;; Looks like this is in a task
	   (case (re-operation re)
	     ((consumes)
	      (unless (eq (re-scope re) 'overall)
		(error "A task must consume ~S overall, not at ~S."
		       (re-resource re) (re-at re))))
	     ((sets produces)
	      (unless (and (eq (re-scope re) 'at)
			   (eq (get-node-end (re-at re))
			       (earliest-node-end)))
		(error "A task has ~S ~S at an illegal point: ~S."
		       (re-operation re)
		       (re-resource re)
		       (re-at re))))))
	  (t
	   ;; Assume it's an ordinary, everyday schema
	   (unless (eq (re-operation re) 'consumes)
	     (error "Illegal schema operation: ~S ~S."
		    (re-operation re)
		    (re-resource re)))))))


;;;; Process-resource-event

;;; /\/: So far, works only for overall.

;;; /\/: Since the node is part of the re struct, we don't really need
;;; to pass it around.  But it used to not be part of the re struct...

;;; False -> fail.

(defun process-resource-event (re) ; -> true or false
  (let ((node (etag-node (re-at re))))
    (case (re-operation re)
      ((consumes)
       (process-consumes-event re node))
      (otherwise
       (error "Can't handle ~S ~S" (re-operation re) (re-resource re))))))

(defun process-consumes-event (re node) ; -> true or false
  (if (eq (re-scope re) 'overall)
      (process-consumes-spec re node)
    (process-consumes-use re node)))

;;; Consumes overall

(defun process-consumes-spec (re node) ; -> true or false
  ;; We have comsumes <resource> <range> overall.
  (let ((rr (ensure-resource-record node (re-resource re))))
    (and (increase-rr-min rr (range-min (re-range re)))
	 (decrease-rr-max rr (range-max (re-range re))))))

;;; Consumes at point

(defun process-consumes-use (re node) ; -> true or false
  (error "Can't yet handle ~S ~S." node re))

;;; Increase rr min

(defun increase-rr-min (rr new-min) ; -> true or false
  ;; We allow the existing min to be greater, in which case there's
  ;; no work to do.
  (cond ((&>= (rr-min rr) new-min)
	 t)
	((&< (rr-max rr) new-min)
	 nil)
	(t
	 (setf (rr-min rr) new-min)
	 (let ((parent (rr-parent rr)))
	   (or (null parent)
	       (and (increase-rr-min parent
				     (sum-over-children parent #'rr-min))
		    ;; If this rr uses more, the max for siblings may
		    ;; decrease.  However, this rr's max should not change.
		    (let ((current-max (rr-max rr)))
		      (prog1
			  (decrease-rr-children-max parent)
			(assert (eql current-max (rr-max rr)))))))))))
		
		     
;;; Decrease rr max

(defun decrease-rr-max (rr new-max) ; -> true or flase
  ;; We allow the existing max to be less, in which case there's
  ;; no work to do.
  (cond ((&<= (rr-max rr) new-max)
	 t)
	((&> (rr-min rr) new-max)
	 nil)
	(t
	 (setf (rr-max rr) new-max)
	 (decrease-rr-children-max rr))))

(defun decrease-rr-children-max (rr)
  (loop for child in (rr-children rr)
	always
	  (let ((new-child-max
		 (&- (rr-max rr)
		     (&- (rr-min rr) (rr-min child)))))
	    ;; The max the child can use is what's left of the
	    ;; parent max after min use by everything but this
	    ;; child has been removed.
	    (decrease-rr-max child new-child-max))))

;;; Resource-record creation / initialization

;;; Ensure-resource-record creates a record if it does not already
;;; exist.  It also creates ancestor records if they don't exist.
;;; Why ancestors?, why not just the parent?, you might ask.  The
;;; answer is that no records are created until a resource event
;;; comes along.  When one does, it might be for a node several
;;; levels down, and all of those levels might have to be created.

;;; Ensure-resource-record also ensures that the rr has been initialized.
;;; Intialization fills in the initial min and max use levels for the rr
;;; in a way that is consustent with the levels of the parent and sibling
;;; records.  If consistent values are not possible, then something has
;;; gone wrong earlier (or else we're continuing in an invalid context)
;;; and an error is signalled.

;;; So, by using ensure-resource-record instead of get-resource-record,
;;; it is possible to pretend that all resource records always already exist.

(defun ensure-resource-record (node resource)
  (ensure-rr-initialized
    (or (get-resource-record node resource)
        (setf (get-resource-record node resource)
              (new-resource-record node resource)))))

(defun new-resource-record (node resource)
  (let* ((parent (get-parent-resource-record node resource))
         (new (make-resource-record
                :node node
                :resource resource
                :parent parent)))
    #+:undef
    (when parent
      (setf (rr-children parent)
            (append (rr-children parent) (list new))))
    new))

(defun get-parent-resource-record (node resource)
  (if (eq node 'node)
      ;; We're already as parent as we can get.
      nil
    ;; Else create parent record if it doesn't already exist.
    (ensure-resource-record
       (name->parent-name node)
       resource)))

;;; Resource-record initialization

(defun ensure-rr-initialized (rr) ; -> rr
  ;; Link to parent alreadu exists; we just have to fill in the link from
  ;; the parent and the min and max values (if they're not already there).
  (cond ((not (eq (rr-min rr) :undef))
	 ;; Already initialized -- check that the values are reasonable ones.
	 ;; (We have separate assertions so that the error message will be
	 ;; more precise.)
	 (assert (integerp (rr-min rr)))
	 (assert (>= (rr-min rr) 0))
	 (assert (not (eq (rr-max rr) :undef)))
	 (assert (or (and (integerp (rr-max rr)) (>= (rr-max rr) 0))
		     (infp (rr-max rr))))
	 (assert (or (null (rr-parent rr))
		     (member rr (rr-children (rr-parent rr)))))
	 rr)
	(t
	 ;; Initialize.
	 (assert (eq (rr-max rr) :undef))
	 (let ((parent (rr-parent rr)))
	   (when parent
	     (ensure-rr-initialized parent))
	   (setf (rr-min rr) 0)
	   (setf (rr-max rr)
		 (if parent
		     (&- (rr-max parent)
			 (sum-over-children parent #'rr-min))
		   :inf))
	   (assert (&>= (rr-max rr) 0))
	   (when parent
	     (assert (not (member rr (rr-children parent))))
	     (setf (rr-children parent)
		   (append (rr-children parent) (list rr))))
	   rr))))

;;; Utilities

(defun name->parent-name (name) ; -> node name or nil if no parent
  ;; E.g. for node-3-2, return node-3.
  (let* ((n (symbol-name name))
         (p (position '#\- n :from-end t)))
    (if (and p
	     ;; Must be one "-" left in the parent name
	     ;; /\/: not here.
	     #+:undef
             (find '#\- n :from-end t :end (1- p)))
        (values (intern (subseq n 0 p)))
      nil)))

(defun sum-over-children (rr f)
  (let ((sum 0))
    (dolist (child (rr-children rr) sum)
      (setq sum (&+ sum (funcall f child))))))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)

