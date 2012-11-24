;;;; File: nodes.lsp
;;; Contains: Definition of nodes and node-ends
;;; Author: Jeff Dalton
;;; Created: 02 March 1994
;;; Updated: Thu Jun  3 04:49:33 1999 by Jeff Dalton
;;; Copyright: (c) 1994, AIAI, University of Edinburgh

;;; Both nodes and node-ends are represented explicitly by structs.
;;; In addition, there is a time-point for every node-end (though not
;;; necessarily a node-end for every time-point).

;;; Some parts of O-Plan work only on node ends and don't need to
;;; know about nodes at all.

(in-package :oplan-nodes)

(use-package :oplan-util)
(use-package :oplan-developerlib)
(use-package :oplan-ctxt)
(use-package :oplan-schema-defs)
(use-package :oplan-tf)			;for 'dummy
(use-package :oplan-domain-services)	;for action-level

(import 'oplan::contains-psvs-p)	;/\/

;; Nodes
(export '(*estimated-max-nodes*))
(export '(plan-node n-tag n-parent-tag n-child-tags n-numbers
	  n-type n-pattern n-begin n-end n-reason n-expansion-schema
	  n-level))
(export '(get-node))
(export '(n-begin-tpoint n-end-tpoint))

;; Node-end tags
(export '(etag make-etag etag-node etag-end))

;; Node-ends
(export '(plan-node-end ne-tag ne-pre-ends ne-post-ends
	  ne-link-distance ne-time-point ne-index))
(export '(ne-node-tag ne-node ne-end))
(export '(get-node-end get-named-end))
(export '(node-end-count))
(export '(earliest-node-end))

;; Basic operations on nodes
(export '(walk-nodes list-nodes node-lessp))
(export '(walk-node-ends list-node-ends list-node-ends-depth-first))

;; Global after-point
(export '(global-after-point set-global-after-point))

;; Node construction and linking
(export '(ads-add-node make-node-and-ends))
(export '(link-etags link-node-ends))
(export '(link-etags-else-error link-node-ends-else-error))

;; Extracting information
(export '(walk-node-ends-depth-first node-ends-between))
(export '(make-node-end-mark-vector))

;; Descriptions
(export '(make-a-list-of-node-descriptions))

;; Support for KS requests
(export '(ads-expand-node))
(export '(cannot-expand-node uninstantiated-node-iteration-set))


;;; Globals

(defparameter *estimated-max-nodes* 500
  "A plausible upper bound on the number of nodes, used when
   constructing arrays and hash-tables.")

(defvar *node-table* (make-hash-table :size *estimated-max-nodes*
				      :test #'eq)
  "A mapping from node-tags to node structs [context-layered].")

(defvar *node-end-count* 0
  "The number of existing node-ends [context-layered].")

(defvar *top-level-node-count* 0
  "The number of top-level nodes [context-layered].")

(defvar *global-after-point* nil
  "A node-end tag that all new nodes must be liked after [context-layered].")

(define-initializer :dm init-nodes-db ()
  (clrhash *node-table*)
  (setq *node-end-count* 0)
  (setq *top-level-node-count* 0)
  (setq *global-after-point* nil)
  t)


;;;; Structures

;;; /\/: The "plan-" prefix is chiefly to distinguish names defined here
;;; from names defined elsewhere.  For instance, the TF compiler defines
;;; a "node" struct, and schemadef.lsp defines some "node-" accessors.
;;; Perhaps eventually this file should own "node", but at present that's
;;; not the case.  Similar problems arise for "node-end", which sometimes
;;; means a node-end tag such as (node-1 :end).

;;; Nodes

(defstruct (plan-node (:conc-name n-))
  tag				;symbol, e.g. node-3-2
  numbers			;numbers from tag, e.g. (3 2) 
  type				;node type
  pattern			;description, typically an action pattern
  reason			;(:expand), or (:achieve p v ...), or ...
  i-expansion-schema		;schema name, or nil if node not expanded
  parent-tag			;symbol, e.g. node-3
; i-child-tags			;list of symbols, e.g. (node-3-2-1 ...)
  (i-n-children 0)		;number of children [context-layered]
  begin				;node-end that represents the begin_of the node
  end)				;node-end that represents the end_of the node

(define-context-accessor n-expansion-schema n-i-expansion-schema)
(define-context-accessor n-n-children n-i-n-children)

(defun n-level (n)
  (if (n-pattern n)
      (action-level (first (n-pattern n)))
    (if (n-expansion-schema n)
	;; /\/: The TF compiler uses the schema name when the
	;; schema has no expands pattern.
	(action-level (n-expansion-schema n))
      0)))

(defun get-node (node-tag)
  (or (deref-in-context (gethash node-tag *node-table*))
      (error "There is no node with tag ~S." node-tag)))

(defsetf get-node (node-tag) (new-node-struct)
  `(update-in-context
     #'(lambda (k v) (setf (gethash k *node-table*) v))
     #'(lambda (k) (gethash k *node-table*))
     ,node-tag
     ,new-node-struct))


;;; Node-end tags

;;; /\/: Again we have to pick a name that won't conflict.
;;; /\/: Instead of e.g. (node-3 :begin) should we have (:begin_of node-3)?

;;; N.B. Must be comparable by EQUAL.  (/\/ really?)

;;; /\/: Must match the node-end struct in schema-defs.lsp.

(defstruct (end-tag (:conc-name etag-)
		    (:constructor make-etag)
		    (:constructor etag (node end))
		    (:type list))
  node				;node tag, e.g. node-1
  end)				;end, either :begin or :end


;;; Node-ends

(defstruct (plan-node-end (:conc-name ne-)
			  (:print-function print-plan-node-end))
  tag				;tag + end of the node it's an end of
  i-pre-ends			;node ends linked directly before this one
  i-post-ends			;node ends linked directly after this one
  (i-link-distance 0)		;length of longest path from begin_of start
  time-point			;time point that corresponds to this node-end
  (index (next-ne-index)	;index in any associated arrays
	 :type fixnum))

(define-context-accessor ne-pre-ends ne-i-pre-ends)
(define-context-accessor ne-post-ends ne-i-post-ends)
(define-context-accessor ne-link-distance ne-i-link-distance)

(defun print-plan-node-end (ne stream depth)
  ;; This :print-function exists so that we don't have to worry about
  ;; circularity when printing node-ends.
  (declare (ignore depth))
  (format stream "#<~A ~S ~D>" (type-of ne) (ne-tag ne) (ne-index ne)))

;;; Pseudo-accessors

(defun ne-node-tag (ne) (etag-node (ne-tag ne)))
(defun ne-node (ne)     (get-node (etag-node (ne-tag ne))))
(defun ne-end (ne)      (etag-end (ne-tag ne)))

;;; Node pseudo-accessors (here to be after the node-end defstruct
;;; and hence able to have ne- accessors compiled inline).

(defun n-begin-tpoint (n) (ne-time-point (n-begin n)))
(defun n-end-tpoint (n)   (ne-time-point (n-end n)))

;;; Node-end lookup

(defun get-node-end (etag)
  (let ((n (get-node (etag-node etag))))
    (ecase (etag-end etag)
      (:begin (n-begin n))
      (:end (n-end n)))))

(defun get-named-end (node end)
  (ecase end
    (:begin (n-begin node))
    (:end (n-end node))))

;;; Index / count operations

(defun next-ne-index ()
  ;; N.B. Call this whenever a node-end is constructed.
  (let ((i (ctxt-symbol-value '*node-end-count*)))
    (assert (<= i most-positive-fixnum))
    (incf (ctxt-symbol-value '*node-end-count*))
    i))

(defun node-end-count ()
  (ctxt-symbol-value '*node-end-count*))


;;;; Basic operations on nodes

(defun walk-nodes (fn)
  (maphash #'(lambda (k v)
	       (declare (ignore k))
	       (let ((node (deref-in-context v)))
		 (unless (null node)
		   (funcall fn node))))
	   *node-table*))

(defun list-nodes ()
  (let ((nodes '()))
    (walk-nodes #'(lambda (n) (push n nodes)))
    nodes))

(defun node-lessp (node-1 node-2)	;for sorting lists of nodes
  (list-lessp #'< (n-numbers node-1)
	          (n-numbers node-2)))

(defun walk-node-ends (fn)
  (walk-nodes
    #'(lambda (n)
	(funcall fn (n-begin n))
	(funcall fn (n-end n)))))

(defun list-node-ends ()
  (let ((node-ends '()))
    (walk-node-ends #'(lambda (ne) (push ne node-ends)))
    node-ends))

(defun list-node-ends-depth-first ()
  (let ((node-ends (make-tconc)))
    (walk-node-ends-depth-first #'(lambda (ne) (tconc node-ends ne)))
    (tconc-contents node-ends)))


;;;; Global after-point

(defun global-after-point ()
  (ctxt-symbol-value '*global-after-point*))

(defun set-global-after-point (etag)
  (setf (ctxt-symbol-value '*global-after-point*)
	etag))


;;;; Node construction

;;; Make-node-and-ends constructs a node, its two node-ends, and the
;;; two time-points associated with the node-ends.  The references back
;;; and forth between these structures are put in place; and the begin
;;; node-end is linked before the end node-end, both at the node-end
;;; (ADS) level and by a time-constraint in the TPN.

;;; /\/: The :PARENT argument is the parent's tag (or nil if no parent),
;;; rather than the parent node struct itself.  Maybe this should change.

;;; /\/: The :BASE-NAME argument is part of a temporary fix to the problem
;;; of names of nodes introduced by achieve conditions.  They are always 
;;; given "top-level" names such as node-3 rather than "subnode" names
;;; such as node-4.3.1.  So their names do not reflect their expansion
;;; level.  Unfortunately, they're also given null parent-tags in the
;;; node structs (because the base name was given by the :PARENT arg),
;;; thus making it hard to determine where they're "nested" in the plan.
;;; By having a separate :BASE-NAME argument, the :PARENT argument can give
;;; the correct parent name (so that we can recover the nesting) without
;;; changing the names of these achieve-introduced nodes (ie, they'll
;;; still be node-3, node-4, etc).  :BASE-NAME will be nil for the
;;; achieve nodes and unspecified for all other nodes.

;;; The :REASON indicates why the node was added to the plan:
;;;
;;; (:EXPAND) -- the node was derived from the nodes clause of the schema
;;; used to expand the parent node, if the node has a parent, or else from
;;; the nodes clause of the task schema (or from an :add-to-task).  This
;;; case also covers ks-fix during execution.
;;;
;;; (:ACHIEVE p v e) -- the node was introduced to achieve p = v at e.
;;;
;;; (:EXEC) -- nodes added for various reasons during plan execution.

#+:undef
(defun ads-add-node (&key type pattern reason)
  (n-tag (make-node-and-ends :type type :pattern pattern :reason reason)))

(defun ads-add-node (&rest keyword-args)
  (n-tag (apply #'make-node-and-ends keyword-args)))

(defun make-node-and-ends
    (&key type
	  pattern
	  parent			;parent node, not tag
	  reason			;why the node was added
	  (base-name nil base-name-p)	;/\/ for now
	  ;delay			;/\/ for now
	  ) ; -> node
  ;; /\/: base-name = parent unless :base-name nil was explicitly supplied.
  (assert (implies base-name-p (null base-name)))
  (unless base-name-p
    (setq base-name parent))
  (assert (implies parent (plan-node-p parent)))
  (assert (member (car reason) '(:expand :achieve :exec)))
  ;; First get a tag
  (multiple-value-bind (node-tag num)
      (if base-name (next-subnode-tag parent) (new-top-level-node-tag))
    ;; Now build structures, bottom-up
    (let* ((begin-tpoint-tag (tpn-add-time-point 0 *infinity*))
	   (end-tpoint-tag (tpn-add-time-point 0 *infinity*))
	   (begin-end
	    (make-plan-node-end
	      :tag        (etag node-tag :begin)
	      :time-point (tpn-get-tpoint begin-tpoint-tag)
	      :index      (next-ne-index)))
	   (end-end
	    (make-plan-node-end
	      :tag        (etag node-tag :end)
	      :time-point (tpn-get-tpoint end-tpoint-tag)
	      :index      (next-ne-index)))
	   (numbers
	    (append (and base-name parent (n-numbers parent))
		    (list num)))
	   (node
	    (make-plan-node
	      :tag        node-tag
	      :numbers    numbers
	      :type       type
	      :pattern    pattern
	      :reason     reason
	      :parent-tag (if parent (n-tag parent) nil)
	      :begin      begin-end
	      :end        end-end
	      )))
      ;; Record the node in the node-table.  Note that until we do this
      ;; we can't reach the node from its node-ends, because node-ends don't
      ;; contain a pointer to their node.  They each have a tag that contains
      ;; the node's tag instead.
      (setf (get-node node-tag) node)
      ;; The begin-end must be before the end-end.  Moreover, linking
      ;; them cannot fail.
      (link-node-ends-else-error begin-end end-end) ; should have a delay /\/
      ;; And we're done.
      node)))

;;; Tag construction

(defun make-node-tag (base-name child-number)
  (values
    (intern
      (concatenate 'string 
	(if (symbolp base-name) (symbol-name base-name) base-name)
	"-"
	(prin1-to-string child-number)))
    child-number))

(defun new-top-level-node-tag ()
  (make-node-tag "NODE" (incf (ctxt-symbol-value '*top-level-node-count*))))

(defun next-subnode-tag (node &optional expected-n)
  (let ((n (incf (n-n-children node))))
    (when (and expected-n (/= n expected-n))
      (error "Next subnode of ~S expected to be number ~S but was ~S."
	     (n-tag node) expected-n n))
    (make-node-tag (n-tag node) n)))


;;;; Adding links

;;; Link-node-ends constrains one node-end to be before another in time.
;;; If the TPN says this is impossible, it returns false without changing
;;; the plan state.  Otherwise, it adds a constraint to the TPN, links
;;; the node-end structs via their pre- and post-ends slots, and returns
;;; true.

(defun link-etags (from-tag to-tag &optional delay-between)
  (link-node-ends (get-node-end from-tag) (get-node-end to-tag) delay-between))

(defun link-node-ends (from to &optional delay-between) ; -> true / false
  (assert (null delay-between))			;/\/ for now

  ;; Constrain the associated time-points.
  (when (tpn-add-time-constraint
	  (ne-time-point from)
	  (ne-time-point to)
	  0
	  *infinity*)
    ;; N.B. Return nil for failure if the TPN can't add the constraint.
    ;; Otherwise ...

    ;; Link the node-ends.
    (push to (ne-post-ends from))		;from -> to
    (push from (ne-pre-ends to))		;from <- to

    ;; Maintain longest-path lengths
    (let ((new-to-dist (fix1+ (ne-link-distance from))))
      (declare (fixnum new-to-dist))
      (when (fix> new-to-dist (ne-link-distance to))
        (update-link-distance to new-to-dist)))

    ;; Success
    t))

(defun update-link-distance (ne dist)
  (setf (ne-link-distance ne) dist)
  (let ((new-dist (fix1+ dist))
	(nd-object nil))
    (declare (fixnum new-dist))
    (dolist (p (ne-post-ends ne))
      (when (fix> new-dist (ne-link-distance p))
	(ensuref nd-object new-dist)
	(update-link-distance p nd-object)))))

(defun link-etags-else-error (from-tag to-tag &optional delay-between)
  (or (link-etags from-tag to-tag delay-between)
      (cerror "Continue anyway."
	      "Can't link ~S --> ~S, delay ~S."
	      from-tag to-tag delay-between)))

(defun link-node-ends-else-error (from-ne to-ne &optional delay-between)
  (or (link-node-ends from-ne to-ne delay-between)
      (cerror "Continue anyway."
	      "Can't link ~S --> ~S, delay ~S."
	      from-ne to-ne delay-between)))


;;;; Following links

;;; /\/: Maybe this should be in the GOP.

;;; (walk-node-ends-depth-first fn &key from)
;;;
;;; Calls the function fn on node-end structs that are linked after
;;; (the ne struct) from, proceeding depth-first along ne-post-ends
;;; links.  The descent continues from a node-end only if fn returns
;;; true.  Node-ends are visited at most once, and an error is signalled
;;; if a cycle is found.
;;;
;;; Walk-node-ends-depth-first will visit node-ends in a less random
;;; order than (the present version of) walk-node-ends and may even
;;; be faster.

(defun walk-node-ends-depth-first (fn &key (from (earliest-node-end)))
  (let ((marks (make-node-end-mark-vector)))
    (declare (type simple-vector marks))
    (macrolet ((mark (ne) `(svref marks (ne-index ,ne))))
      (label walk ((at from))
	(ecase (mark at)
	  ((:start)
	   (error "Node end cycle at ~S." at))
	  ((:finish)
	   ;; Already been here
	   )
	  ((nil)
	   ;; A node-end we haven't yet visited.
	   (setf (mark at) :start)
	   (let ((descend-p (funcall fn at)))
	     (when descend-p
	       (dolist (successor (ne-post-ends at))
		 (walk successor))))
	   (setf (mark at) :finish)))))))

(defun make-node-end-mark-vector ()
  (make-array (node-end-count) :initial-element nil))

(defun earliest-node-end ()
  (get-node-end
    (etag (intern (string '#:node-1) :oplan)
	  :begin)))


;;; (node-ends-between before after)
;;;
;;; Returns a list of the node-end structs that are linked after "after"
;;; and before "before" (and are hence between them).
;;;
;;; "Before" and "after" are ne structs.  Neither will be part of the
;;; result.  "Before" will normally be linked (directly or indirectly)
;;; before "after", but that's not necessary (you'll just get a null
;;; result).  The strategy is to walk the successors of "before",
;;; collecting those that are on paths that lead to "after".

(defun node-ends-between (before after) ; -> list of ne-structs
  (let ((marks (make-hash-table :test #'eq))
	(result '()))
    (macrolet ((mark (node) `(gethash ,node marks)))
      (setf (mark before) :looking)
      (setf (mark after) :on-path)
      (label walk ((ends (ne-post-ends before)))
	(dolist (at ends)
	  (ecase (mark at)
	    ((:looking)
	     (error "Node end cycle at ~S." at))
	    ((:on-path :off-path)
	     ;; Already processed
	     )
	    ((nil)
	     (let ((post-ends (ne-post-ends at)))
	       (walk post-ends)
	       (cond ((loop for p in post-ends
			    thereis (eq (mark p) :on-path))
		      (setf (mark at) :on-path)
		      (push at result))
		     (t
		      (setf (mark at) :off-path)))))))))
    result))


;;;; Expansion

;;; Returns a list of (tag type pattern) descriptions of all nodes
;;; created.

(define-condition cannot-expand-node (simple-error) ())

(defvar *expand-subnodes*)		;list of plan-node structs
(defvar *waiting-iterations*)		;((dummy schema-subnode) ...)

(defun ads-expand-node (node-tag schema-name schema-subnodes schema-orderings)

  (let* ((*expand-subnodes* '())
	 (*waiting-iterations* '())
	 (number->subnode-alist '())
	 (node (get-node node-tag)))

    ;; Record which schema was used to expand the node
    (assert schema-name)
    (setf (n-expansion-schema node) schema-name)

    ;; Create all the subnodes
    (dev-note :nodes :detail "Creating subnodes")
    (dolist (sub schema-subnodes)
      (let ((subnode
	     (if (node-iterators sub)
		 (iterate-schema-subnode node sub)
	       (make-schema-subnode node sub))))
	;; /\/: We rely in process-node-end on pop-gensym generating a
	;; a name with a final number that matches the number from the
	;; schema.  This is also why we have to delay the construction
	;; of subnodes produced by iteration.
	(assert (= (node-number sub) (last-element (n-numbers subnode))))
	(push (cons (node-number sub) 
		    subnode)
	      number->subnode-alist)))

    ;; Perform waiting iterations
    (when *waiting-iterations*
      (dolist (w (reverse *waiting-iterations*))
        (apply #'finish-iterating-schema-subnode node w)))

    ;; Create links between the various subnodes, if given.
    (dev-note :nodes :detail "Explicit links")
    (flet ((number->subnode (number)
	     (or (lookup number number->subnode-alist)
		 (error "Node ~S has no subnode numbered ~S."
			node-tag number))))
      (dolist (ord schema-orderings)
	(link-node-ends-else-error
	  (get-named-end (number->subnode (ordering-node-number1 ord))
			 (ordering-node-end1 ord))
	  (get-named-end (number->subnode (ordering-node-number2 ord))
			 (ordering-node-end2 ord)))))

    ;; Make sure all the subnodes are linked in somewhere.
    (dev-note :nodes :detail "Implicit links to initial and final subnodes")
    (dolist (number+subnode number->subnode-alist)
      (let ((subnode (cdr number+subnode)))

	;; If pre field of a subnode is not set yet, then create a
	;; link from the start of -node- to the start of -subnode-.
	(unless (ne-pre-ends (n-begin subnode))
	  (link-node-ends-else-error
	    (n-begin node) (n-begin subnode)))

	;; If post field of a subnode is not set yet, then create a link
	;; from the end of -subnode- to the end of -node-.
	(unless (ne-post-ends (n-end subnode))
	  (link-node-ends-else-error
	    (n-end subnode) (n-end node)))))

    ;; Finished!
    (setq *expand-subnodes* (nreverse *expand-subnodes*)) ; /\/
    (mapcar #'(lambda (n)
		(list (n-tag n) (n-type n) (n-pattern n)))
	    *expand-subnodes*)))

(defun make-schema-subnode (parent-node sub)
  (let ((n (make-node-and-ends
	     :type (node-type sub)
	     :pattern (node-pattern sub)
	     :parent parent-node
	     :reason '(:expand))))
    (push n *expand-subnodes*)
    n))

(defun make-iteration-subnode (parent-node sub pattern)
  (let ((n (make-node-and-ends
	     :type (node-type sub)
	     :pattern pattern
	     :parent parent-node
	     :reason '(:expand))))
    (push n *expand-subnodes*)
    n))

;;; For iterated nodes, we construct a dummy node and put any generated
;;; nodes between its ends.  That way the dummy can have the node number
;;; from the schema definition, and all references in the schema to that
;;; node number will still work.  Begin_of and end_of that number should
;;; still be right too.

;;; /\/: What we're _supposed_ to do is renumber references instead.
;;; But then we'd have to do general renumbering in the schema at run-time,
;;; which would be a pain.  It's not even clear that we could leave the
;;; begin_ofs and end_ofs as-is.  (Suppose i is the number of an iterated
;;; node, and the generated nodes will be left unordered.  We're supposed
;;; to make a dummy node to be linked before all the generated nodes and
;;; another dummy to be linked after.  Should refs to "end_of i" be
;;; changed to refer to the end of the linked-after dummy or to the begin
;;; of that dummy?)

;;; /\/: Is there a delay-between in the syntax?

(defun iterate-schema-subnode (parent-node sub) ; -> dummy node
  ;; Make the dummy, do the rest later.
  (assert (node-iterators sub))
  (let ((dummy (make-node-and-ends :type 'dummy :parent parent-node
				   :reason '(:expand))))
    (push dummy *expand-subnodes*)
    (push (list sub dummy) *waiting-iterations*)
    dummy))

(defun finish-iterating-schema-subnode (parent-node sub dummy)
  ;; The args after parent-tag come from what we pushed onto 
  ;; *waiting-iterations* above.
  (dev-note :nodes :detail "~@<Iteration within ~W at ~W for ~:_~W~:>"
	    (n-tag parent-node) (n-tag dummy) (node-pattern sub))

  ;; Make the subnodes.
  (dev-note :nodes :detail " - Making the iteration nodes")
  (let* ((patterns
	  (expand-iteration (node-pattern sub) (node-iterators sub)))
	 (subnodes
	  (mapcar #'(lambda (pat)
		      (make-iteration-subnode parent-node sub pat))
		  patterns)))

    ;; Link as needed.
    (dev-note :nodes :detail " - Linking")
    (cond ((node-sequence-p sub)
	   ;; Link in sequence
	   (link-node-ends-else-error
	     (n-begin dummy)
	     (n-begin (first subnodes)))
	   (for-adjacent-elements
	     #'(lambda (from to)
		 (link-node-ends-else-error (n-end from) (n-begin to)))
	     subnodes)
	   (link-node-ends-else-error
	     (n-end (last-element subnodes))
	     (n-end dummy)))
	  (t
	   ;; Just link between the ends of the dummy
	   (dolist (s subnodes)
	     (link-node-ends-else-error (n-begin dummy) (n-begin s))
	     (link-node-ends-else-error (n-end s) (n-end dummy)))))))

(define-condition uninstantiated-node-iteration-set (cannot-expand-node) ())

(defun expand-iteration (pattern iterators) ; -> list of patterns
  ;; Iterators is ((var set) ...).
  ;; Vars in pattern look like (:iterate-var name).
  (let ((patterns '()))
    (label expand ((i iterators)
		   (p pattern))
      (if (null i)
	  (push p patterns)
	(let ((var (caar i))
	      (set (cadar i)))
	  (when (contains-psvs-p set)	;make sure it's instantiated
	    (complain-about-psv-in-iteration-set var set))
	  (assert (listp set))
	  (dolist (val set)
	    (expand (cdr i)
		    (replace-iterate-var p var val))))))
    (nreverse patterns)))

(defun complain-about-psv-in-iteration-set (var set)
  (dev-debug :error "Node iteration set for variable ~S contains a psv: ~S."
    var set)
  (signal-error 'uninstantiated-node-iteration-set
    :format-string "Node iteration set for variable ~S contains a psv: ~S."
    :format-arguments (list var set)))

(defun replace-iterate-var (pattern var-name value)
  (label repl ((p pattern))
    (if (atom p)
	p
      (if (list-beginning :iterate-var p)
	  (let ((var (cadr p)))
	    (if (eq var var-name)
		value
	      p))
	(recons p (repl (car p))
		  (repl (cdr p)))))))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
