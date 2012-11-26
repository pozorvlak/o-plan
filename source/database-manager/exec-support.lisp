;;;; File: exec-support.lisp
;;; Contains: Database Manager's execution support
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: June 1999
;;; Updated: Sun Sep  5 22:04:18 1999 by Jeff Dalton
;;; Copyright: (c) 1999, AIAI, University of Edinburgh

(in-package :oplan)

(use-package :oplan-world-state-util)

;;;; A simple execution simulator

(define-condition exec-simulation-error (simple-error) ())

(defun run-exec-simulation ()
  (prepare-for-exec-simulation)
  ;; Install the always facts
  (loop for (p v) in (get-always-facts)
	do (set-world-always-value p v))
  ;; Execute node-ends in stages
  (mapc #'sim-exec-1-stage
	(node-ends-in-execution-stages :actions-only nil)))

(defun prepare-for-exec-simulation ()
  (init-ne-exec-slots)
  (init-ne-effect-lists)
  (init-ne-condition-lists)
  (clear-world-state)
  (clear-world-always))

(defun sim-exec-1-stage (node-ends)
  (let ((unsat (remove-if #'sim-exec-cond-satisfied-p
			  (sim-exec-stage-conds node-ends))))
    (when unsat
      (signal-error 'exec-simulation-error
	:format-string "Unsatisfied conditions before stage exec:~% ~S."
	:format-arguments (list unsat))))
  (mapc #'sim-exec-node-end node-ends)
  (let ((unsat (remove-if #'sim-exec-cond-satisfied-p
			  (sim-exec-stage-post-conds node-ends))))
    (when unsat
      (signal-error 'exec-simulation-error
	:format-string "Unsatisfied conditions after stage exec:~% ~S."
	:format-arguments (list unsat)))))

(defun sim-exec-node-end (ne)
  (loop for e in (ne-effects ne)
	for f = (fully-instantiate-psvs e)
	for (p v) = f
	unless (has-world-always-value-p p)
	do (set-world-pattern-value p v)))

(defun sim-exec-stage-conds (node-ends)
  ;; Returns a list of fully instantiated GOST entries
  (mapcan #'(lambda (ne)
	      (mapcar #'fully-instantiate-psvs
		      (append (ne-conds ne)
			      (ne-iconds ne))))
	  node-ends))

(defun sim-exec-cond-satisfied-p (gost)
  ;; Checks a fully instantiated GOST entry against the world state
  (let ((p (tgm-gost-pattern gost))
	(v (tgm-gost-value gost)))
    (or (world-always-p p v)
	(world-fact-p p v))))

(defun sim-exec-stage-post-conds (node-ends)
  ;; Returns a list of fully instantiated GOST entries.
  (mapcan #'(lambda (ne)
	      (mapcar #'fully-instantiate-psvs
		      (append (ne-post-conds ne)
			      (ne-iconds ne))))
	  node-ends))

(defun ne-post-conds (ne)
  (remove-if #'(lambda (cond)
		 (let ((c-p (psv-actorise-pattern (tgm-gost-pattern cond))))
		   (loop for e in (ne-effects ne)
			 for e-p = (psv-actorise-pattern (pv-pattern e))
			 thereis (obmatch3 c-p e-p nil))))
	     (ne-conds ne)))


;;; Some extra node-end slots

;;; (ne-exec-status ne) keeps track of whether a node-end has been
;;; sent off for execution and whether we've received a message saying
;;; its execution has completed.

;;; (ne-effects ne) is a list of pv-pairs collected from all the TOME
;;; entries that specify effects at (ne-tag ne).

;;; (ne-conds ne) is a list of the GOST entries, g, such that (ne-tag ne)
;;; appears as (tgm-gost-node-end g).  These are the conditions that are
;;; explicitly at ne.

;;; (ne-iconds ne) is a list of the GOST entries for supervised conditions
;;; that span ne but are not at ne explicitly.  These are the conditions
;;; that are implicitly at ne.

(define-struct-extension (ne-exec-aux :accessor-prefix ne-
				      :extension-prefix nex-
				      :base-accessor ne-exec-slots)
  ;; E.g. (ne-effects ne) == (nex-effects (ne-exec-slots ne)).
  (exec-status nil)		;one of nil, :ready, :sent, :finished
  (effects '())			;list of pv-pairs
  (conds '())			;list of conditions as GOST entries
  (iconds '()))			;list of implicit conditions as GOST entries

(definit :dm *ne-exec-aux-table* nil)

(defun ensure-ne-exec-aux-table ()
  (ensuref *ne-exec-aux-table*
	   (make-hash-table :size (* 2 (node-end-count))
			    :test #'eq)))

(defun ne-exec-slots (ne)
  (ensuref (gethash ne *ne-exec-aux-table*)
	   ;; New ne -- have to fill in correct values rather than
	   ;; just use the init-defaults.
	   (make-ne-exec-aux
	     :effects (get-ne-effects ne)
	     :conds (get-ne-conds ne)
	     :iconds (get-ne-iconds ne)
	     )))

(defun init-ne-exec-slots ()
  ;; Sets up exec slots for all existing node ends.  New ones, created
  ;; later on, will be handled by the ensuref in ne-exec-slots.
  (let ((table (ensure-ne-exec-aux-table)))
    ;; Remove node-ends that are not in the current context.
    ;; We used to just (clrhash table), but that doesn't work for
    ;; the way execution is handled by the web demos.  In particular,
    ;; we need to preserve the exec-status.
    (dolist (ne (hash-table-keys table))
      (unless (node-end-still-valid-p ne)
	(remhash ne table)))
    ;; Install slots for any current node-ends that are not in the table.
    (walk-node-ends
      #'(lambda (ne)
	  (unless (gethash ne table)
	    (setf (gethash ne table) (make-ne-exec-aux)))))
    table))

(defun node-end-still-valid-p (ne)
  (handler-case (eq ne (get-node-end (ne-tag ne)))
    (error ()
      ;; This happens if no node with right tag exists.
      nil)))


;;; Pseudo-accessors

(defun ne-exec-due-time (ne)
  (tpoint-min (ne-time-point ne)))


;;; Node-end effect lists

;;; Before plan execution begins, we set up the ne-effects lists of
;;; all node-ends that are then in the plan.  If a new node-end is
;;; created later on, it won't have any exec slots.  The first time
;;; something asks for its exec-slots, they'll be created by
;;; ne-exec-slots, and ne-exec-slots will call get-ne-effects when
;;; initializing the ne-effects slot.

(defun init-ne-effect-lists ()
  ;; Find effects for all node-ends in one TOME pass (cf get-ne-effects).
  (tgm-walk-tome
    #'(lambda (entry value)
	(let ((pattern (tgm-tome-pattern entry))
	      (etag (tgm-tome-node-end entry)))
	  (unless (eq etag :always)
	    (push (pv-pair pattern value)
	      (ne-effects (get-node-end etag))))))))

(defun get-ne-effects (ne) ; -> list of pv-pairs
  ;; Unfortunately, we have to search the whole TOME.  /\/
  (let ((etag (ne-tag ne))
	(effects '()))
    (tgm-walk-tome
      #'(lambda (entry value)
	  (let ((pattern (tgm-tome-pattern entry)))
	    (when (equal etag (tgm-tome-node-end entry))
	      (push (pv-pair pattern value)
		    effects)))))
    effects))


;;; Node-end condition lists

;;; The condition and implicit condition lists are handled in the same
;;; way as the effect lists.

;;; Before plan execution begins, init-ne-condition-lists is called to
;;; set up the ne-conds and ne-iconds lists of all node-ends that are
;;; then in the plan.  If a new node-end is created later on, it won't
;;; have any exec slots.  But the first time something asks for its
;;; exec-slots, they'll be created by ne-exec-slots; and ne-exec-slots
;;; will call get-ne-conds and get-ne-iconds to collect the conditions
;;; and implicit conditions for the new node-end.

(defun init-ne-condition-lists ()
  (tgm-walk-gost
    #'(lambda (entry value)
	;; The GOST entry always describes an explicit condition at
	;; its tgm-gost-node-end.
	(push entry (ne-conds (get-node-end (tgm-gost-node-end entry))))
	;; Supervised consitions can also be implicit.
	(when (eq (tgm-gost-condition-type entry) 'supervised)
	  (dolist (ne (supervised-spanned-node-ends entry value))
	    (push entry (ne-iconds ne)))))))
	
#+:undef
(defun init-ne-condition-lists ()
  ;; /\/: For now.  Later do all in one GOST pass.
  (walk-node-ends
    #'(lambda (ne)
	(setf (ne-conds ne) (get-ne-conds ne))
	(setf (ne-iconds ne) (get-ne-iconds ne)))))


;;; Supervised-spanned-node-ends
;;;
;;; Returns a list of the structs that represent node-ends spanned by
;;; a supervised condition.  The condition is specified by a GOST entry
;;; and its value.
;;;
;;; Here's a definition / explanation.
;;;
;;; Suppose the condition is "supervised P = V at E from [C]", where
;;; E and C are node-ends; and suppose that the condition was introduced
;;; by a schema, S, used to expand node N.  E and C will normally be ends
;;; of sibling nodes that are children of N.  In that case, the node-ends
;;; spanned by the condition are the ends of descendants of N (not counting
;;; N itself) that are linked after C and before E.  In other words: the
;;; ends of siblings of C and E, or of the descendants of such siblings,
;;; that are linked between C and E.
;;;
;;; Unfortunately, there's a complication.  C and E might not be ends of
;;; children of N, because in S the condition might have been "from" or
;;; "at" an end of "self".  So C or E might be an end of N rather than
;;; and end of one of N's children.  So long as they're not both ends of
;;; "self", we can work it out by seeing which of C or E is "deeper".
;;; The parent node of the deeper node-end will be N.  This is the same
;;; as saying N will be the node of the shallower node-end.  If C is an
;;; end of node-3-1 and E is an end of node-3-1-4, then N = node-3-1.
;;; In this case, the condition was something like
;;;
;;;    supervised p = v at 4 from [begin_of self]
;;;
;;; The difficult case is
;;;
;;;    supervised p = v at end_of self from [begin_of self]
;;;
;;; Suppose N is actually node-3-1.  Remember that N is the node that S,
;;; the schema that introduced the condition, was used to expand.  If S
;;; contained "supervised p = v at end_of self from [begin_of self]",
;;; and we look at the corresponding GOST entry, we'll conclude,
;;; incorrectly, that N = node-3.  That's because the GOST will refer
;;; to node-3-1, and we'll suppose that node-3-1 is a child of N
;;; (since that's how we normally interpret GOST entries, and, for all
;;; we know, the schema used to expand node-3 had a "supervised p = v
;;; at end_of 1 from [begin_of 1]").
;;;
;;; Of course, we could say that when C and E are ends of the same node,
;;; then that node, rather than its parent, is considered N.  Like the
;;; other rule, this would be right in some cases and wrong in others.
;;; Our assumption is that it's right in fewer cases.  To get the right
;;; answer in every case, we'd have to record which schema (indeed, which
;;; use of which schema -- since there might be recursion) introduced
;;; each condition.
;;;
;;; /\/: Other complications:
;;;
;;; 1. What if there's more than one contributor?  Right now, we take
;;; the first contributor if there's more than one.  We can get away
;;; with this because multiple contributors don't occur for supervised
;;; conditions in any of our current domains.
;;;
;;; 2. What about nodes introduced by an "achieve"?  At present, they're
;;; added as new top-level nodes: they don't look like descendants of N,
;;; and we can't tell that they _are_ descendants of N.
;;;

(defun supervised-spanned-node-ends (sup-entry sup-value)
  (when (> (length sup-value) 1)
    (cerror "Take the 1st." "More than 1 contributor for ~S." sup-entry))
  (let* ((cond-at (tgm-gost-node-end sup-entry))
	 (contrib (car (first sup-value)))
	 (cond-at-ne (get-node-end cond-at))
	 (contrib-ne (get-node-end contrib))
	 (cond-at-node (get-node (etag-node cond-at)))
	 (contrib-node (get-node (etag-node contrib)))
	 (parentage
	  (butlast
	   (n-numbers
	    (deeper-node cond-at-node contrib-node)))))
    (loop for ne in (node-ends-between contrib-ne cond-at-ne)
	  when (node-has-ancestor-numbered parentage (ne-node ne))
	  collect ne)))

;;; When calling from a KP, we want etags rather than structs.
;;; While we're at it, we'll also look up the GOST item's value.

(defun supervised-spanned-etags (sup-entry)
  (mapcar #'ne-tag
	  (supervised-spanned-node-ends
	     sup-entry
	     (value sup-entry))))

;;; Get-ne-conds and get-ne-iconds are used to get the condition lists
;;; for new nodes that did not exist when init-ne-condition-lists was
;;; called.

(defun get-ne-conds (ne)
  (let ((etag (ne-tag ne))
	(conds '()))
    (tgm-walk-gost
      #'(lambda (entry value)
	  (declare (ignore value))
	  (when (equal etag (tgm-gost-node-end entry))
	    (push entry conds))))
    conds))

(defun get-ne-iconds (ne)
  (let ((iconds '()))
    (tgm-walk-gost
      #'(lambda (entry value)
	  (when (eq (tgm-gost-condition-type entry) 'supervised)
	    (when (supervised-spans-ne-p entry value ne)
	      (push entry iconds)))))
    iconds))

(defun supervised-spans-ne-p (sup-entry sup-value ne)
  (when (> (length sup-value) 1)
    (cerror "Take the 1st." "More than one contributor for ~S." sup-entry))
  (let* ((cond-at (tgm-gost-node-end sup-entry))
	 (contrib (car (first sup-value)))
	 (cond-at-node (get-node (etag-node cond-at)))
	 (contrib-node (get-node (etag-node contrib)))
	 (parentage
	  (butlast
	   (n-numbers
	    (deeper-node cond-at-node contrib-node)))))
    (and (node-has-ancestor-numbered parentage (ne-node ne))
	 (gop-is-linked-between-p
	     (get-node-end contrib)
	     ne
	     (get-node-end cond-at)))))


;;; Node and node-end utilities

(defun deeper-node (n1 n2)
  (if (< (length (n-numbers n1)) (length (n-numbers n2)))
      n2
    n1))

(defun node-has-ancestor-numbered (desired-numbers node)
  (list-proper-prefix-p desired-numbers (n-numbers node)))


;;; Node-ends-in-execution-stages

;;; Each "stage" is a list of node-ends that could execute at the
;;; same time (as each other).  The stages are returned as a list,
;;; in the order in which they would have to execute.

;;; It's assumed that a node-end can execute at its earliest
;;; start time, provided that all the node ends linked before
;;; it have executed.

;;; Note that the stages are not derermined solely by the times
;;; calculated by the TPN, because links are also taken into account.
;;; There can therefore be more than one stage with the same
;;; numeric start time.

(defun node-ends-in-execution-stages ; -> list of list of node-ends
       (&key (actions-only t))
  (let ((waiting (list-node-ends-depth-first)) ;sorted later
	(marks (make-node-end-mark-vector))
	(stages (make-tconc)))
    (declare (type simple-vector marks))
    (labels ((finished-p (ne) (svref marks (ne-index ne)))
	     (execute (ne) (setf (svref marks (ne-index ne)) t))
	     (ne-est (ne) (tpoint-min (ne-time-point ne)))
	     (ready-p (now ne)
	       (and (= (ne-est ne) now)
		    (every #'finished-p (ne-pre-ends ne))))
	     (action-p (ne)
	       (eq (n-type (ne-node ne)) 'action)))
      ;; Start by putting the node-ends in order by earliest start time.
      (setq waiting (stable-sort waiting #'< :key #'ne-est))
      ;; Construct the stages.
      (loop
        (when (null waiting)
	  (return (tconc-contents stages)))
	(let* ((now (ne-est (first waiting)))
	       (ready (remove-if-not (partial1 #'ready-p now) waiting)))
	  (when (null ready)
	    (report-invalid-start-times waiting now))
	  (mapc #'execute ready)
	  (when actions-only
	    (setq ready (remove-if-not #'action-p ready)))
	  (when ready
	    (tconc stages		;sort by node-number within a stage
		   (sort-node-ends-by-node-number ready)))
	  (setq waiting (remove-if #'finished-p waiting)))))))

(defun sort-node-ends-by-node-number (node-ends)
  (stable-sort node-ends
    #'(lambda (a b)
	(let ((na (n-numbers (ne-node a)))
	      (nb (n-numbers (ne-node b))))
	  (cond ((list-lessp #'< na nb)
		 t)
		((equal na nb)			;begin < end if numbers equal
		 (eq (etag-end a) :begin))
		(t
		 nil))))))

(defun report-invalid-start-times (node-ends now)
  ;; We go here if nothing became ready as a result of executing
  ;; the node-ends in the previous stage.  This must be because some
  ;; node end with est = now had a predecessor with est > now.
  (flet ((ne-est (ne) (tpoint-min (ne-time-point ne))))
    (let ((losers (make-tconc)))
      (dolist (ne node-ends)
        (when (= (ne-est ne) now)
          (dolist (pre (ne-pre-ends ne))
            (when (> (ne-est pre) now)
	      (tconc losers pre)))))
      (assert (tconc-contents losers))
      (signal-error 'exec-simulation-error
	:format-string "Invalid earliest start times in node ends ~S."
	:format-arguments (list (tconc-contents losers))))))


;;; Miscellaneous utilities

;;; /\/: Consider moving them to more appropriate places.

(defun get-always-facts () ; -> list of pv-pairs
  (let ((always '()))
    (tgm-walk-tome
      #'(lambda (entry value)
	  (when (eq (tgm-tome-node-end entry) :always)
	    (push (pv-pair (tgm-tome-pattern entry) value)
		  always))))
    (canonical-description-order
     always)))

(defun fully-instantiate-psvs (pattern)
  (if (atom pattern)
      (if (psv-p pattern)
	  (let ((psv-s (oplan-psv::psv-body pattern)))
	    (if (oplan-psv::psv-body-p psv-s)
		(let ((psv-value (oplan-psv::psv-body-value psv-s)))
		  (if (eq :psv-value :undef)
		      (error "~S has no value." pattern)
		    psv-value))
	      (error "~S does not exist in current context." pattern)))
	pattern)
    (recons pattern
	    (fully-instantiate-psvs (car pattern))
	    (fully-instantiate-psvs (cdr pattern)))))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When)                   (What)
;;;
