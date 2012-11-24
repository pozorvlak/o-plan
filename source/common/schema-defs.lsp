;;;; File: schema-defs
;;; Contains: TF domain and schema definitions
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: June 1994
;;; Updated: Sun Aug 22 23:16:01 1999 by Jeff Dalton
;;; Copyright: (c) 1994, AIAI, University of Edinburgh

(in-package :oplan-schema-defs)

(use-package :oplan-util)
(use-package :oplan-tf)

;;; A naming convention we sometimes follow is to have constructors called
;;; MAKE-<struct-name> be the standard constructors with keyword arguments
;;; while those called simply <struct-name> take positional arguments.

;;; /\/: Note that we're not yet consistent.  Some things are defined
;;; as list-structs while others are still handled directly as lists.


(defmacro defexport (&rest whatever)	;an abbreviation
  `(defstruct-export . ,whatever))


;;;; Domain definition structure.

(defexport domain
  name                 ; The domain name.
  always               ; Pattern/value pairs that are always true.
  object-types         ; Alist of types against possible objects.
  meta-schemas         ; The domain's meta-schemas.
  schemas              ; The domain's schemas (includes tasks).
  included-code        ; Domain included code.
  compute-functions    ;
  resource-units       ; 
  resource-types       ; 
  defaults             ;
  initially            ;
  initial-resources    ;
  initial-time         ;
  )


;;;; Schema definition structure.

(defexport schema
  name                 ; Schema name.
  type		       ; :action, :task, or :repair
  vars                 ; Schema vars.
  relationships        ; Sames and not sames list.
  expands              ; Expansion pattern - may be nil.
  action-name          ; First word in expands pattern else constructed.
  only-use-for-effects ; Visible effects - may be nil.
  nodes                ; List of nodes making up this schemas expansion.
  orderings            ; Constraints on the orderings of the expansion nodes.
  conditions           ; Condition requirements for particular nodes.
  effects              ; Asserted by the schema's action itself.
  time-windows         ; Time constraints.
  resources            ; Resource use and constraints.
  constraints	       ; Constraints that may have plug-in managers.
  cost		       ; Estimated cost when fully expanded (for alt choice).
  )

(defun-export task-schema-p (schema)
  ;; /\/: A stupid way to test!
  (eql (search #.(string '#:task_) (string (schema-name schema)))
       0))

(defun-export deep-copy-schema (s)
  ;; Copies the schema and the slots that (probably /\/) need to be copied.
  (let ((new (copy-schema s)))
    (macrolet ((copy-slot (slot-name obj)
		 (assert (symbolp obj))		;var, not arb expr
		 `(setf (,slot-name ,obj)
			(copy-tree (,slot-name ,obj)))))
      (copy-slot schema-only-use-for-effects new)
      (copy-slot schema-nodes new)
      (copy-slot schema-orderings new)
      (copy-slot schema-conditions new)
      (copy-slot schema-effects new)
      (copy-slot schema-time-windows new)
      (copy-slot schema-resources new)
      (copy-slot schema-constraints new)
      new)))


;;;; Definitions for domain structures

(defexport (always-fact (:type list)
			(:constructor always-fact (pattern value)))
  pattern
  value
  (point :always))

(defexport (type-def (:type list)
		     (:constructor type-def (name set)))
  name
  set)

(defexport (compute-function (:type list)
			     (:constructor compute-function
				(call-pattern result-pattern
				 multiple-answer-p depends)))
  call-pattern
  result-pattern
  multiple-answer-p
  depends)

(defexport (resource-type-def (:type list)
			      (:constructor resource-type-def
				 (class resource unit)))
  class		;e.g. consumable_strictly
  resource	;e.g. {resource money}
  unit)		;e.g. pounds


;;;; Definitions for schema structures


;;; Var definitions

(defexport (var-def (:type list)
		    (:constructor var-def (name restriction)))
  name
  restriction)

(defexport (var-relation (:type list)
			 (:constructor
			    var-relation (var1 rel var2)))
  ;; /\/: Need better slot names.
  var1					;"given" actor
  rel					;relation: = or /=
  var2)					;"given" actor


;;; Nodes

;; Nodes in the schema record have the following structure:
;; (<node num> <node type> <pattern> nil <est> <lst> <eft> <lft>)

(defexport (node (:type list)
		 (:constructor make-node)
		 (:constructor node (number type &optional pattern)))
  number
  type
  pattern
  (mystery nil)				;/\/: what is this supposed to be?
  (est 0)				;earliest start time
  (lst *infinity*)			;latest start time
  (eft 0)				;earliest finish time
  (lft *infinity*)			;latest finish time
  (iterators '())
  (sequence-p nil))


;;; Orderings

;; Orderings in the schema record have the following structure:
;; (nil <node num> <node end> <node num> <node end> <delay>)

(defexport (ordering (:type list)
		     (:constructor make-ordering)
		     (:constructor ordering
		       (node-number1 node-end1 node-number2 node-end2
                        &optional delay)))
  ;; /\/: Need some better slot names.
  ;; /\/: What's the default delay?
  ;; /\/: The old system seems to want a list with NIL as the 1st element.
  (filler nil)
  node-number1
  node-end1
  node-number2
  node-end2
  delay)				;range

(defun-export construct-ordering (before after delay)
  (make-ordering
    :node-number1 (node-end-node before)
    :node-end1    (node-end-end before)
    :node-number2 (node-end-node after)
    :node-end2    (node-end-end after)
    :delay        delay))

(defun-export ordering-from-ne (ord)
  (node-end (ordering-node-number1 ord) (ordering-node-end1 ord)))

(defun-export ordering-to-ne (ord)
  (node-end (ordering-node-number2 ord) (ordering-node-end2 ord)))


;;; Conditions and effects

(defexport (pv-pair (:type list)	;pattern-value pair
		    (:conc-name pv-)
		    (:constructor pv-pair (pattern value)))
  pattern
  value)

;; Conditions in the schema record have the following structure:
;; (<type> <patt> <val> <at-node-end> <contributors>)

;; /\/: Can't use "condition" as the type name, because that's taken
;; by the CL condition system.  Can't use "cond", because we aren't
;; allowed to define names in the COMMON-LISP package.

(defexport (con (:type list))
  ;; /\/: At present, used only to read structs created by calling
  ;;      list directly.
  type
  pattern
  value
  point
  contributors)

(defmacro-export con-at-node-end (c) `(con-point ,c))

(defun-export con-after-point (c)
  (ecase (con-type c)
    ((achievable)
     (assert (fix= (length (the list c)) 5))
     (last-element c))))

(defun-export con-minus-after-point (c)
  (case (con-type c)
    ((achievable)
     (assert (fix= (length (the list c)) 5))
     (butlast c))
    (t c)))

(defun-export compute-memo (pattern value)
  (declare (ignore pattern))
  value)


;; Effects in the schema record have the following structure:
;; (<patt> <val> <at-node-end>)

(defexport (effect (:type list)
		   (:include pv-pair)
		   (:constructor make-effect)
		   (:constructor effect
		      (pattern &optional (value 'true)
                                         (point '(:self :end)))))
  ;; /\/ The defaults for the effect constructor are used in tests.
  (point
   (error "Node end not supplied for effect.")))

(defmacro-export effect-at-node-end (e) `(effect-point ,e))


;;; Node-ends

;;; Node = :start is for achieve after-points.

(defexport (node-end (:type list)
		     (:constructor node-end (node end)))
  node					;a number, :self, or :start
  end)					;:begin or :end


;;; Time windows

;;; Time constraints in the schema record have the following structure:
;;; (<node-end | :ABST0> <node-end> (<min> <max> <pref>))

(defexport (time-constraint
	     (:type list)
	     (:conc-name tcon-))
  ;; These are produced by the time_windows clause and by orderings that
  ;; include a delay.
  ;; /\/: Inherit from ordering?  Have orderings be missing the delay?
  ;; /\/: Perhaps they should be called time-windows?
  end-1					;node end
  end-2					;node end
  bounds)				;"time window"

(defun-export construct-time-constraint (before after bounds)
  (make-time-constraint :end-1 before :end-2 after :bounds bounds))

;;; Backwards-compatible:

(defun-export time-constraint-start (time-constraint)
  (first time-constraint))

(defun-export time-constraint-end (time-constraint)
  (second time-constraint))

(defun-export time-constraint-min (time-constraint)
  (first (third time-constraint)))

(defun-export time-constraint-max (time-constraint)
  (second (third time-constraint)))

(defun-export time-constraint-pref (time-constraint)
  (third (third time-constraint)))


;;; Time specs, effectively in seconds

(defun-export time-spec (days hours minutes seconds)
  (+ seconds (* 60 (+ minutes (* 60 (+ hours (* 24 days)))))))


;;; Numeric ranges

(defexport (range (:type list)
		  (:constructor range (min max)))
  min
  max)


;;; Time ranges

(defexport (time-window (:type list)
			(:constructor make-time-window)
			(:constructor time-window (min max)))
  ;; /\/: Should these be called time-windows or time-bounds?
  ;; They result from a <time-bounds-spec>, and the existence of
  ;; the time_windows schema clause may make time-window misleading.
  ;; This argues for time-bounds.  But the early parts of the TF
  ;; Manual are fairly explicit in calling them time-windows.
  ;; Maybe time-ranges?
  (min 0)
  (max *infinity*)
  (ideal :undef))


;;; Resources

(defexport (resource-operation (:type list)
			       (:constructor resource-operation
				  (keyword resource range units scope at)))
  keyword	;e.g. consumes
  resource	;e.g. {resource money}
  range		; a range, from e.g. 0..100
  units		; a name, e.g. pounds, dollars, gallons
  scope		; at or overall
  at)		; a node-end


;;; Other constraints

(defexport (constraint-block (:type list)
			     (:constructor constraint-block
				(type constraints)))
  type		; name, maps to the constraint manager
  constraints)	; list of constraints

(defexport (constraint (:type list))
  type		; name, maps to the constraint manager
  pattern	; contents, handled by the constraint manager
  value         ; more contents
  from          ; a node-end or nil
  to)		; a node-end


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
