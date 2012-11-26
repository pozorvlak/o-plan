;;;; File: viewer-services.lisp
;;; Contains: Plan / World viewer utilities
;;; Authors: Jeff Dalton and Brian Drabble
;;; Created: Wed Mar  4 17:00:03 1992
;;; Updated: Tue Oct  7 05:07:10 1997 by Jeff Dalton
;;; Copyright: (c) 1992, 1993, 1994 AIAI, University of Edinburgh

(in-package :oplan-plan-world-viewer)

;;;
;;; Common setup
;;;

(defvar *viewer-args* nil)

(defvar *null-stream* (make-null-io-stream))

(defun common-viewer-setup (viewer-args)
  (setq *viewer-args* viewer-args)
  (setq *window* (if viewer-args *null-stream* (ensure-display-window))))

;;; If there are no viewer-args, (viewer-arg kwd) always returns nil.
;;; Otherise, it requires that the requested arg kwd be present and
;;; returns the corresponding value, unless a :default-value is 
;;; specified.  The most common cases are for no viewer args to
;;; be supplied, so that all valaues are obtained interactively,
;;; or for all relevant args to be specified, so that interaction
;;; is unnecessary.  The :default-value argument is used when a
;;; reasonable value can be determined without asking the user.

;;; Note that the above implies that arg values should never be nil,
;;; though :default-values can be.

;;; Note that the :default-value applies only when _some_ viewer args
;;; have been supplied.

(defun viewer-arg (kwd &key (default-value nil default-p))
  (if (null *viewer-args*)
      nil
    (let ((value (getf *viewer-args* kwd *viewer-args*)))
      (if (eq value *viewer-args*)
	  (if default-p
	      default-value
	    (error "A viewer arg named ~S is required but was not supplied."
		   kwd))
	value))))

(defun interactivep ()
  (null *viewer-args*))

;;; Common finish

(defun common-viewer-finish ()
  (when (not (interactivep))
    (ipc-send-out :finished-view)))


;;;; Window operations

(defun ensure-display-window ()
  ;;
  ;; Open the display window if it's not already open.
  ;;
  (or (x-get-stream *pw-viewing-window*)
      (open-display-window))
)

(defun open-display-window ()
  ;;
  ;; Open a window in which to display the output from the planner
  ;;
  (x-open-and-register-io-win *pw-viewing-window*
    (or (is:is-get-window-args *pw-viewing-window*)
        '("-title" "PlanWorld Viewer"
          "-n" "PlanWorld")))
  (x-redirect-errors *pw-viewing-window*)
  (clear-screen (x-get-stream *pw-viewing-window*))
  (x-get-stream *pw-viewing-window*)
)

(defun display-banner (label &rest format-args)
  (format *window* "~&~?~2%" label format-args))

(defun display-divider ()
  (format *window*
	  "~&====================================================~2%")
  (force-output *window*))

(defun ring-bell ()
  (write-char (int-char 7) *window*)
  (force-output *window*))

(defun sound-alarm ()	;N.B. May cause fresh-line to output a newline /\/
  (ring-bell)
  ; (sleep 1)
  (ring-bell))

(defun report-action (message &rest format-args)
  (format *window* "~&~?~2%" message format-args))

(defun ask-user (viewer-arg-kwd question &rest format-args)
  (or (viewer-arg viewer-arg-kwd)
      (prog2
        (format *window* "~&~?: " question format-args)
	(read-line *window*)
	(format *window* "~%"))))


;;;; Getting plan and world views
;;;
;;; These routines are called only by routines that call the viewer,
;;; not by code in the viewer itself.

(defun pw-get-plan-for-viewing ()
  (db-request :GET-PLAN-NODES :DESCRIPTIONS))

(defun pw-get-world-for-viewing (node-id)
  (check-type node-id string)
  (canonical-description-order
    (db-request :QA-ALL node-id)))



;;;; Random utilities

;;; concat-pw-symbol concatenates strings and symbols and returns a symbol.

(defun concat-pw-symbol (s1 s2 &rest more)
  (values
    (intern (apply #'concatenate 'string
		   (string s1) (string s2) (mapcar #'string more))
	    *viewer-io-package*)))


;;; Plan description

;;; In the drawer, the plan is represented as a list of node-description
;;; structs.  The nodes are ordered by node-numbers, with lower numbers
;;; first.  When a node-number has several components, they are compared
;;; from left to right.  The result is not what would result from a textual
;;; comparison of the node-names.  For instance, node-3 is before node-10,
;;; not after it.  An important property of this order is that parent nodes
;;; are listed before their children.  This is exploited in the code that
;;; prepares a plan view for AutoCAD.

;;; /\/: The node descriptions are sent across without a link between the
;;;      two ends of each node; we have to add these links.

;;; /\/: References to node-ends come across as (node-name :BEGIN) or
;;;      (node-name :END); we have to convert them to our node-end-tags.

(defstruct (node-description (:conc-name nd-)
			     (:type list))
  node-name	;e.g. node-1
  begin-pre	;e.g. ((end_of node-1) (begin_of node-2))
  begin-suc	;
  end-pre	;
  end-suc	;
  time-bounds   ;earliest and latest start and finish times: (est lst eft lft)
  node-type	;e.g. action
  pattern)	;e.g. (puton a b) /\/ converted to a string

(defstruct (node-end-tag (:type list)
			 (:conc-name ne-tag-)
			 (:constructor make-node-end-tag)
			 (:constructor ne-tag (end node)))
  ;; N.B. Must be comparable by EQUAL.
  end		;i.e. begin_of or end_of
  node)		;e.g. node-1

;;; Node-description lookup

(defvar *node-lookup-list*)		;for looking up nodes

(defun find-node (name &optional (nodes *node-lookup-list*))
  (find name nodes :key #'nd-node-name))

;;; Time accessors

(defun nd-est (n)
  (first (nd-time-bounds n)))

(defun nd-lst (n)
  (second (nd-time-bounds n)))

(defun nd-eft (n)
  (third (nd-time-bounds n)))

(defun nd-lft (n)
  (fourth (nd-time-bounds n)))

(defun nd-min-duration (n)
  ;; /\/ Will usually be right, but perhaps not always.
  (multiple-value-bind (begin-min begin-max end-min end-max)
      (values-list
       (nd-time-bounds n))
    (if (and (numberp begin-max) (numberp end-max))
	(min ;; Push down as far as we can go
	     (- end-min begin-min)
	     ;; Push up as far as we can go
	     (- end-max begin-max))
      (if (or (= end-min begin-min)
	      (eql end-max begin-max))
	  0
	'unknown))))

(defun nd-max-duration (n)
  ;; /\/: Will often be right, but not always.
  (multiple-value-bind (begin-min begin-max end-min end-max)
      (values-list
       (nd-time-bounds n))
    (declare (ignore begin-max end-min))
    (if (numberp end-max)
	(- end-max begin-min)
       *infinity*)))

(defun nd-time-info (n)
  (list (nd-est n) (nd-lst n)
	(nd-eft n) (nd-lft n)
	(nd-min-duration n)
	(nd-max-duration n)))

;;; /\/: Convert node descriptions to our format.

(defun fix-up-nodes (nodes)
  (dolist (n nodes)
    ;; Convert predecessor and successor references.
    (setf (nd-begin-pre n) (fix-up-refs (nd-begin-pre n)))
    (setf (nd-begin-suc n) (fix-up-refs (nd-begin-suc n)))
    (setf (nd-end-pre n) (fix-up-refs (nd-end-pre n)))
    (setf (nd-end-suc n) (fix-up-refs (nd-end-suc n)))
    ;; Add a link between the two ends of each node.
    (push (ne-tag 'end_of (nd-node-name n))
	  (nd-begin-suc n))
    (push (ne-tag 'begin_of (nd-node-name n))
	  (nd-end-pre n))
    ;; /\/: Convert the pattern to a string, once, here, so we don't
    ;; have to change other parts of the drawer when we change how
    ;; patterns are formatted.  (The problem is formatting actors.)
    (setf (nd-pattern n)
	  (pattern->string (nd-pattern n))))
  nodes)

(defun fix-up-refs (node-end-refs)
  (mapcar #'(lambda (ref)
	      (let ((name (first ref))
		    (end (second ref)))
		(make-node-end-tag
		  :end (ecase end (:begin 'begin_of) (:end 'end_of))
		  :node name)))
	  node-end-refs))

(defun pattern->string (pat)
  (let ((*print-case* :downcase)
	(*package* *viewer-io-package*)
	#+:lcl4.1 (lcl:*print-right-margin* 1000)	;nil won't do it
	#+kcl      (xp:*print-right-margin* 1000)
	)
    (if (null pat)
	""
      (xp-format nil "~:@W" pat))))

;;; Make-pairs takes a list of the form (a1 a2 b1 b2 ...) and returns
;;; a list of the form ((a1 a2) (b1 b2) ...).

(defun make-pairs (lis)
  (if (null lis)
      '()
    (cons (list (car lis) (cadr lis))
          (make-pairs (cddr lis)))))

;;; Flatten-pairs is the inverse of make-pairs.

(defun flatten-pairs (list-of-pairs)
  (flatten-one-level list-of-pairs))


;;;; Level slicer

;;; The level of a node is the number of components in its node-number.
;;; For instance, node-3 is at level 1 and node-3-2-1 is at level 3.
;;; The "level slicer" asks the user for a maximum level and returns
;;; a copy of the nodes with all nodes at greater levels removed.
;;; Links to those nodes are also removed so that there is no need
;;; to exclude them later on.

;;; The level slicer always returns a new list contining copies of
;;; the node-description structs for the nodes that have been retained.
;;; These structs may, therefore, be freely modified.  However, the
;;; slicer does not copy any of the slot values in the structs.

;;; Extract-plan-to-level can be called by viewers that want to do
;;; some level-slicing of their own.

(defun slice-to-level (nodes) ; -> list of node copies
  (let ((limit (get-max-plan-level nodes)))
    (setq *max-plan-level* limit)
    (if (eq limit :all)
	(mapcar #'copy-node-description nodes)
      (extract-plan-to-level nodes limit))))

(defun extract-plan-to-level (nodes max-level)
  ;; We need to copy so that the original list of nodes is not
  ;; altered -- in case we want to slice to a different level later.
  (setq nodes (mapcar #'copy-node-description nodes))
  (loop for n in nodes
	when (<= (node-tag->level (nd-node-name n))
		 max-level)
	collect (slice-links-below-level n nodes max-level)))

(defun slice-links-below-level (node nodes limit) ; -> copy of node
  (flet ((keep-link-p (ne-tag)
	   (<= (node-tag->level (ne-tag-node ne-tag))
	       limit))
	 (link-ends (from to)
	   ;; From and to are ne-tags
	   ;; Only end_of -> begin_of links are allowed
	   (let ((from-node (find-node (ne-tag-node from) nodes))
		 (to-node (find-node (ne-tag-node to) nodes)))
	     (ecase (ne-tag-end from)
	       (end_of
		 (ecase (ne-tag-end to)
		   (begin_of
		     (push to (nd-end-suc from-node))
		     (push from (nd-begin-pre to-node)))))))))
    (setf (nd-begin-pre node)
	  (remove-if-not #'keep-link-p (nd-begin-pre node)))
    (setf (nd-begin-suc node)
	  (remove-if-not #'keep-link-p (nd-begin-suc node)))
    (setf (nd-end-pre node)
	  (remove-if-not #'keep-link-p (nd-end-pre node)))
    (setf (nd-end-suc node)
	  (remove-if-not #'keep-link-p (nd-end-suc node)))
    ;; Make sure the node is still linked into the plan.
    (unless (member (nd-node-name node) '(node-1 node-2))
      (when (null (nd-begin-pre node))
        ;; Link after end_of node-1.
        (link-ends (ne-tag 'end_of 'node-1)
		   (ne-tag 'begin_of (nd-node-name node))))
      (when (null (nd-end-suc node))
        ;; Link before begin_of 2.
	(link-ends (ne-tag 'end_of (nd-node-name node))
		   (ne-tag 'begin_of 'node-2))))
    node))

(defun get-max-plan-level (nodes)
  (let ((deepest-level (get-plan-depth nodes))
	(levels (viewer-arg :levels :default-value :all)))
    (if levels
	(if (or (eq levels :all)
		(and (numberp levels)
		     (<= 1 levels deepest-level)))
	    levels
	  (error "Illegal value for plan levels: ~S." levels))
      (ask-max-plan-level deepest-level))))

(defun ask-max-plan-level (deepest-level)      
  (if (= deepest-level 1)
      1				;no choice
    (let ((levels
	   (loop for i from 1 to deepest-level
		 collect (prin1-to-string i))))
      (menu-request
	`("-heading" "Plan levels to show"
	  "-line"
	  ,(format nil "All (~D)=:all" deepest-level)
	  "-line"
	  ,@levels)))))

(defun get-plan-depth (nodes)
  (loop for n in nodes
	maximize (node-tag->level (nd-node-name n))))

(defun node-tag->level (node-tag)
  ;; Sample tag: node-3-9-8; N.B. node-3 is level 1, not 0.
  (count #\- (symbol-name node-tag)))


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;

