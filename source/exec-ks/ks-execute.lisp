;;;; File: KS-EXECUTE.lisp
;;; Contains: KS that starts the execution of a plan.
;;; Author: Jeff Dalton
;;; Created: Sat 18 Feb 1995
;;; Updated: Sat Sep  4 21:55:27 1999 by Jeff Dalton
;;; Copyright: (c) 1995, AIAI, University of Edinburgh

(in-package :oplan-knowledge-source)

;;; This is the 1st execution-related KS loaded when the user decides
;;; to execute a plan.

;;; /\/: At present, it defines a number of things that are properly
;;; part of the AM, the DM, or the support code.

;(load-most-recent "single-process/micro-exec")		;/\/ for now
;(load-most-recent "worlds/micro-exec/micro-exec-world") ;/\/ for now


;;;; KS-EXECUTE

;;; /\/: Should we block backtracking (aka push alternatives) when we
;;; start execution (and unblock when we're done?)?

(defvar *pre-exec-context* nil)

(defun KS-EXECUTE (event)
  (declare (ignore event))

  (set-parameter :psgraph-all-nodes t)	;/\/ for now

  ;; Prepare in the Planner.
  (setq *pre-exec-context* (db-request :get-context))
  (db-request :push-context)
  (db-call 'prepare-for-execution)
  ; (setf (global-activities) '(:exec))
  (setq *schema-selection-mode* :ask)

  ;; Set things going in the Exec.
  (send-to-exec :init)
  (send-to-exec :always (db-call 'get-always-facts))
  (send-to-exec :begin-execution
     (determine-time-scale-factor))
  (tell-exec-to-execute (etag 'node-1 :begin))
  )

(defun tell-exec-to-execute (etag)
  (apply #'send-to-exec
	 :execute
	 (db-call 'node-end-exec-info etag)))

(defun tell-exec-to-execute-next-fringe (etag)
  (mapc #'tell-exec-to-execute
	(db-call 'next-execution-fringe etag)))

(defun get-earliest-exec-finish-time ()
  (first (db-call 'node-end-time-bounds (etag 'node-2 :end))))

;;; Determining the scale factor for simulated time

(defun determine-time-scale-factor ()
  (let ((eft (get-earliest-exec-finish-time)))
    ;; Eft is in seconds of real time.
    ;; If <= 5 minutes, take as-is; else ask the user.
    (if (<= eft (* 60 5))
	;; Ok, we'll go with 1 real sec = 1 sim sec
	1
      ;; Ask the user how much real time the plan execution should take.
      (let ((real-seconds (* 60 (ask-user-for-real-exec-duration eft)))
	    (sim-seconds eft))
	;; Want sim secs per [real] sec such that
	;;   real-seconds * ssps = sim-seconds.
	(round sim-seconds real-seconds)))))

(defun ask-user-for-real-exec-duration (eft) ; -> minutes
  ;; Eft is in seconds.
  (with-query-window
    (tell-user "~&~%The plan will take at least ~A simulated time.~%"
	       (seconds->description eft))
    (let ((min-string
	   (query-user "How many minutes of real time would you ~
		        like it to take: ")))
      (loop
        (multiple-value-bind (mins error)
			     (ignore-errors (parse-integer min-string))
	  (when mins
	    (return mins))
	  (tell-user "~&~A~&" error)
	  (setq min-string (query-user "Minutes of real time: ")))))))


;;;; Utilities

(defun gost-has-multiple-contributors-p (gost-entry)
  (> (length (db-call 'value gost-entry))
     1))

(defun send-to-exec (message &rest args)
  (apply #'ipc-send :micro-exec message args))


;;; ----- AM Part -----

(setq *agenda-priorities*
      (append *agenda-priorities*
	      '((:FIX . 57))))	;just under :POISON-STATE


;;; ----- DM Part -----

;;; This code runs in the DM despite appearing here, something that's
;;; possible in a "single-process" O-Plan.  It should eventually be
;;; moved to a different file.  /\/

;;; See also database-manager/exec-support.lisp.

(import-struct 'oplan-schema-defs:pv-pair)

(defun prepare-for-execution ()
  (init-ne-exec-slots)
  (init-ne-effect-lists)
  (init-ne-condition-lists)
  #+:undef
  (init-due-times)
  (init-departure-lists))

(defun node-end-exec-info (etag)
  ;; Must match the arguments of the Exec's :execute message.
  ;; Called only when the info is just about to be sent
  (let ((ne (get-node-end etag)))
    (set-etag-exec-status etag :sent)
    (list etag
	  (n-type (ne-node ne))
	  (psv-replace-psvs-in-pattern (n-pattern (ne-node ne)))
	  (tpoint-min (ne-time-point ne))
	  (tpoint-max (ne-time-point ne))
	  (mapcar #'psv-replace-psvs-in-pattern
		  (ne-effects ne))
	  (mapcar #'psv-replace-psvs-in-pattern
		  (mapcar #'make-pv-pair-from-gost
			  (append (ne-conds ne)
				  (ne-iconds ne)))))))


;;; Miscellaneous

(defun node-end-time-bounds (etag)
  (let ((tp (ne-time-point (get-node-end etag))))
    (list (tpoint-min tp)
	  (tpoint-max tp))))

(defun make-pv-pair-from-gost (gost-entry)
  ;; /\/: S.b. common to DM and KP.
  (pv-pair (tgm-gost-pattern gost-entry) (tgm-gost-value gost-entry)))


;;; Alternatives

;;; /\/: A more sophisticated version (with reasons, descriptions, etc)
;;; should be created at some point and make available via KS-USER.

;;; /\/: Should atm-push-alternatives automatically add a :no-more-
;;; alternatives alt?  Indeed, does the protocol allow the DM to do
;;; that, or does it have to go from a KP and via the AM?

;;; /\/: Modularity may dictate that all agenda operations should
;;; go through the AM.

(defvar *alt-table-stack* '())

(defun atm-push-alternatives ()
  (push *alternatives* *alt-table-stack*)
  (setq *alternatives* nil))

(defun atm-pop-alternatives ()
  (if *alt-table-stack*
      (setq *alternatives* (pop *alt-table-stack*))
    (error "No alternatives to pop.")))


;;; Exec status operations

(defun get-etag-exec-status (etag)
  (ne-exec-status (get-node-end etag)))

(defun set-etag-exec-status (etag status)
  (setf (ne-exec-status (get-node-end etag)) status))

(defun set-node-exec-status (node-tag status)
  (set-etag-exec-status (etag node-tag :begin) status)
  (set-etag-exec-status (etag node-tag :end) status))

(defun remove-executed-etags (etags)
  (loop for e in etags
	unless (eq (ne-exec-status (get-node-end e)) :finished)
	collect e))


;;; Execution fringe

;;; The basic idea is that a node-end E is ready for execution when all
;;; node-ends that are linked before it have successfully executed.
;;; This ensures that all explicit ordering constraints (ie, links
;;; from the orderings clauses of schemas) are satisfied and that
;;; all node-ends that provide effects needed by conditions at E
;;; have provided those effects.

;;; The actual "ready to execute" check considers only whether all the
;;; node-ends linked before E have been executed, regardless of whether
;;; the execution was successful.  It assumes that any problems due to
;;; execution failures or world events have been fixed.  (It's up to
;;; other parts of the system to ensure that this is so.)

;;; The "ready to execute" check also ignores the TPN time windows.
;;; Time windows are handled by putting node-ends that are ready to
;;; execute in a "departure queue" and not letting them out until
;;; all node-ends with earlier due-times have been executed.  (A
;;; due-time is just the min time from the corresponding time-point.)

;;; We have to obey time-windows to this extent because we sometimes
;;; need to link a node-end after all the node-ends that have been executed
;;; and before all node-ends that haven't.  If we looked only at links
;;; when deciding when to execute a node-end, we might execute E1 before
;;; E2 when the time windows said they had to be the other way around.
;;; Now suppose E1 has executed, E2 hasn't, and we try to link something
;;; after E1 and before E2.  The TPNM won't allow it.

;;; (As an example, consider house-4.  The begin_of the {decorate}
;;; action is reached quickly by links, but the time windows require
;;; that it be later.)

;;; During execution, node-ends take on exec-status values in the following
;;; order:
;;;
;;;  nil
;;;     Not ready for execution, either because we haven't looked at it
;;;     yet or because some prerequisite has not been met.
;;;  :ready
;;;     Assigned when we determine that all prerequisites have been met
;;;     and enqueue the node-end for departure.
;;;  :sent
;;;     Assigned when we send an :execute message to the Exec.
;;;  :finished
;;;     Assigned when we receive a success or failure message from the Exec.
;;;
;;; A node-end with status :ready can have its status set back to nil
;;; when KS-CONTINUE-EXECUTION rebuilds the departure lists.  That happens
;;; when we've finished changing the plan after an execution failure or
;;; an unexpected world event.

;;; Note that one use of the :ready status is to keep redundant
;;; ne-->post links from collecting a post node-end more than once
;;; (in next-execution-fringe).

(defvar *exec-status-sequence* '(nil :ready :sent :finished))


;;; (next-execution-fringe etag)
;;;
;;; Etag has either just been executed successfully or else we're
;;; continuing from it (KS-CONTINUE-EXECUTION) after an execution
;;; failure or an unexpected world event.

(defun next-execution-fringe (etag) ; -> list of etags
  (let ((ne (get-node-end etag)))
    (assert (eq (ne-exec-status ne) :finished))
    ;; Phase 1 -- enqueue for departure any successors of etag that
    ;; now have all their prerequisites met.
    (dolist (post (ne-post-ends ne))
      (unless (ne-exec-status post)
	(when (exec-prereqs-met-p post)
	  (enqueue-for-departure post))))
    ;; Phase 2 -- dequeue any node-ends that can now be executed.
    (remove-from-departure-checklist etag)
    (next-departing-group)))

(defun exec-prereqs-met-p (ne)
  (loop for pre in (ne-pre-ends ne)
	always (eq (ne-exec-status pre) :finished)))


;;; Departure lists

;;; The *departure-checklist* contains all node-ends that haven't yet
;;; been executed.

;;; The *departure-queue* contains node-ends that are ready to execute.
;;; They're held in the queue until all node-ends that have earlier
;;; due-times have been removed from the *departure-checklist*.

(defvar *departure-checklist* nil)

(defvar *departure-queue* nil)

(defun init-departure-lists ()
  (setq *departure-checklist*
	(sort (list-node-ends) #'< :key #'ne-exec-due-time))
  (setq *departure-queue*
	(make-queue)))

(defun rebuild-departure-lists ()
  (setq *departure-checklist*
	(sort (loop for ne in (list-node-ends)
		    unless (eq (ne-exec-status ne) :finished)
		    collect ne)
	      #'<
	      :key #'ne-exec-due-time))
  (setq *departure-queue*
	(make-queue))
  ;; Figure out which node-ends are now ready for execution.
  ;; Some node-ends that used to be ready might no longer be,
  ;; which is why we start be resetting their status to nil.
  (walk-node-ends
    #'(lambda (ne)
	(when (eq (ne-exec-status ne) :ready)
	  (setf (ne-exec-status ne) nil))
	(when (and (not (or (eq (ne-exec-status ne) :finished)
			    (eq (ne-exec-status ne) :sent)))
		   (exec-prereqs-met-p ne))
	  (enqueue-for-departure ne)))))

(defun enqueue-for-departure (ne)
  (setf (ne-exec-status ne) :ready)
  (enqueue-increasing ne *departure-queue* :key #'ne-exec-due-time))

(defun remove-from-departure-checklist (etag)
  ;; etag is the tag of a node-end that has (just) been executed.
  (setq *departure-checklist*
	(delete-1-eq (get-node-end etag) *departure-checklist*)))

(defun next-departing-group () ; -> list of etags
  (if (empty-queue-p *departure-queue*)
      '()
    (let ((d-time (first-due-time *departure-checklist*)))
      (loop while (and (not (empty-queue-p *departure-queue*))
		       (= d-time
			  (first-due-time (queue-contents *departure-queue*))))
	    collect (ne-tag (dequeue *departure-queue*))))))

(defun first-due-time (ne-list)
  (if (null ne-list)
      (error "No first due-time.")
    (ne-exec-due-time (first ne-list))))


;;; Exec-status-based fringes and linking

(defun minimal-done-fringe () ; -> list of etags
  ;; Return a list of etags of all node-ends that have been executed
  ;; and have no directly linked successor that has also been executed.
  ;; /\/: Should the done-fringe include node-ends that have exec-status
  ;; :ready or :sent as well as those that are :finished?
  (let ((fringe '()))
    (walk-node-ends
      #'(lambda (ne)
	  (when (eq (ne-exec-status ne) :finished)
	    ;; This ne has been executed.  See if any successors have.
	    (when (loop for post in (ne-post-ends ne)
			never (eq (ne-exec-status post) :finished))
	      ;; No successors have been executed, so ne is on the
	      ;; minimal fringe.
	      (push (ne-tag ne) fringe)))))
    fringe))

(defun minimal-not-done-fringe () ; -> list of etags
  ;; Return a list of etags of all node-ends that have not been executed
  ;; and have no directly linked predecessor that is also unexecuted.
  ;; /\/: Combine with minimal-done-fringe so we can make one pass.
  ;; /\/: Maybe use the same technique as in node-ends-between
  ;;      (ie, walk node-ends depth-1st, breaking off when there's
  ;;      no need to go deeper (later)).
  (let ((fringe '()))
    (walk-node-ends
      #'(lambda (ne)
	  (unless (eq (ne-exec-status ne) :finished)
	    ;; This ne has been not executed.  check predecessors.
	    (when (loop for pre in (ne-pre-ends ne)
			never (not (eq (ne-exec-status pre) :finished)))
	      ;; No predecessors are unexecuted, so ne is on the
	      ;; minimal fringe.
	      (push (ne-tag ne) fringe)))))
    fringe))

;;; (pinch-plan-at node-tag status), where node-tag is the tag of node N,
;;; links all node-ends that have exec-status status (or later) before
;;; begin_of N and all node-ends that have an earlier status after end_of N.
;;; Those two sets, plus N itself, form an exhaustive partition of the
;;; node-ends.  By an "earlier status" we mean a status earlier in the
;;; list *exec-status-sequence*.

;;; N typically represents something that happened at a particular
;;; point in plan execution.

;;; /\/: Try to get a routine that can be used by both KS-EXECUTION-FAILURE
;;; and KS-UNEXPECTED-WORLD-EVENT.  At least this one works both for
;;; KS-UNEXPECTED-WORLD-EVENT and for adding an action in KS-ADD-TO-TASK.

(defun pinch-plan-at (node-tag status)
  (let ((n-begin (get-node-end (etag node-tag :begin)))
	(n-end (get-node-end (etag node-tag :end)))
	(status-or-later (member status *exec-status-sequence*)))
    (assert (consp status-or-later))
    (set-global-after-point (etag node-tag :end))
    (walk-node-ends-depth-first
      #'(lambda (ne)
	  (cond ((eq (ne-node-tag ne) node-tag)
		 ;; Skip ends of N, but continue descent.
		 t)
		((member (ne-exec-status ne) status-or-later)
		 ;; This ne has been sufficiently executed and hence is in
		 ;; the before-set.  Check the successors.
		 (when (loop for post in (ne-post-ends ne)
			     never (member (ne-exec-status post)
					   status-or-later))
		   ;; No successors are in the before-set, so ne is on the
		   ;; minimal fringe.  Link ne before begin_of N.
		   (link-node-ends-else-error ne n-begin))
		 ;; Continue the descent regardless.
		 t)
		(t
		 (assert (not (member (ne-exec-status ne) status-or-later)))
		 ;; This ne has not yet been sufficiently executed and hence
		 ;; is in the after-set.  Check its predecessors.
		 (when (loop for pre in (ne-pre-ends ne)
			     always (member (ne-exec-status pre)
					    status-or-later))
		   ;; All predecessors are in the before-set, so ne is
		   ;; on the minimal fringe of the after-set.  Link ne
		   ;; after end_of N.
		   (link-node-ends-else-error n-end ne))
		 ;; Don't need to descend through ne's successors
		 nil))))
    ;; Check that enough links were made.
    (unless (and (consp (ne-pre-ends n-begin))
		 (consp (ne-post-ends n-end)))
      (cerror "Continue anyway."
	      "~S hasn't been linked-in." node-tag)))
  ;; Return the name of the node.
  node-tag)


;;; Remove TOME entries for failed effects

;;; Failed-affects is a list of pv-pairs, but as processed by
;;; psv-replace-psvs-in-pattern.  So we can't use them directly
;;; for looking up TOME entries.  However, the ne-effects of a
;;; node-end can be used.

;;; /\/: It might be better if we sent effects over w/o replacing
;;; PSVs in them so that it wasn't so tricky to match failed effects
;;; against TOME and GOST entries.  We'd have to send PSV values
;;; across too, of course, so PSVs could be replaced in the Exec
;;; and World (if they cared).

(defun cancel-failed-tome-entries (failed-etag failed-effects)
  (let ((failed-ne (get-node-end failed-etag))
	(cancelled 0))
    (dolist (effect (ne-effects failed-ne))
      (let* ((effect-pattern (pv-pattern effect))
	     (effect-value (pv-value effect))
	     (filled-pattern (psv-replace-psvs-in-pattern effect-pattern))
	     (filled-value (psv-replace-psvs-in-pattern effect-value))
	     (tome-entry (tgm-make-tome-entry effect-pattern failed-etag))
	     (tome-value (value tome-entry))
	     (failed-effect
	      (find filled-pattern failed-effects
		    :key #'pv-pattern :test #'equal)))
	(assert (equal effect-value tome-value))
	(when failed-effect
	  (assert (equal filled-value (pv-value failed-effect)))
	  (setvalue tome-entry :undef)
	  (incf cancelled))))
    (unless (= cancelled (length failed-effects))
      (error "Failed to cancel some effects."))))


;;; Determining the consequences of an execution failure

(defun find-broken-gost-entries (failed-etag failed-effects)
  ;; Returns a list of GOST entries that are affected by the failure.
  ;; This includes GOST entries that might survive in the end, because
  ;; of multiple contributors. 
  (let ((losers '()))
    (tgm-walk-gost
      #'(lambda (entry value)
	  (let ((cond-pattern (tgm-gost-pattern entry))
		(cond-value (tgm-gost-value entry))
		(cond-contributors value))
	    ;; See if the failed-etag is a contributor
	    (when (member failed-etag cond-contributors
			  :key #'car :test #'equal)
	      ;; Now see if the condition matches one of the failed
	      ;; effects.
	      (dolist (effect failed-effects)
		(when (pv-match effect
				(psv-replace-psvs-in-pattern
				 (pv-pair cond-pattern cond-value)))
		  ;; Matches, so this condition needed a failed effect.
		  (push entry losers)
		  (return)))))))
    (nreverse losers)))

(defun pv-match (pair1 pair2)
  (let ((bindings (obmatch3 (pv-pattern pair1) (pv-pattern pair2) nil)))
    (and bindings
	 (obmatch3 (pv-value pair1) (pv-value pair2) bindings))))


;;; Determining the consequences of an unexpected world event

;;; A GOST entry is affected when:
;;;   * the condition is at a node-end that has not been executed,
;;;   * a contributor is a node-end that has been executed, and
;;;   * the unexpected world-event has a conflicting effect.

(defun event-broken-gost-entries (effects)
  ;; Returns a list of GOST entries that are affected by the failure.
  ;; This includes GOST entries that might survive in the end, because
  ;; of multiple contributors. 
  (let ((losers '()))
    (tgm-walk-gost
      #'(lambda (entry value)
	  (let ((cond-pattern (tgm-gost-pattern entry))
		(cond-value (tgm-gost-value entry))
		(cond-at-ne (get-node-end (tgm-gost-node-end entry)))
		(cond-contributors value))
	    ;; See if the condition is at a node-end that has not
	    ;; been executed.
	    (when (not (eq (ne-exec-status cond-at-ne) :finished))
	      ;; See if any contributor is a node-end that has been
	      ;; executed.
	      (when (loop for c in cond-contributors
			  for c-etag = (car c)
			  thereis
			    (and (not (eq c-etag :always))
				 (eq (ne-exec-status (get-node-end c-etag))
				     :finished)))
		;; Finally see if the condition conflicts with one of the
		;; event's effects.
		(dolist (effect effects)
		  (when (pv-conflict
			  effect
			  (psv-replace-psvs-in-pattern
			   (pv-pair cond-pattern cond-value)))
		    ;; Matches, so this condition needed a conflicting effect.
		    (push entry losers)
		    (return))))))))
    (nreverse losers)))

(defun pv-conflict (pair1 pair2)
  (let ((bindings (obmatch3 (pv-pattern pair1) (pv-pattern pair2) nil)))
    (and bindings
	 (not (obmatch3 (pv-value pair1) (pv-value pair2) bindings)))))


;;; Remove broken GOST ranges

(defun cancel-broken-gost-entries (broken-entries)
  (dolist (gost-entry broken-entries)
    (setvalue gost-entry :undef)))



;;;; ----- KP code -----


;;;; Fixing broken GOST entries

;;; For KS-EXECUTION-FAILURE and KS-UNEXPECTED-WORLD-EVENT.

(defun fix-broken-gost (g after-point)
  (case (tgm-gost-condition-type g)
    ((supervised)
     (fix-broken-supervised g after-point))
    (t
     (db-call 'cancel-broken-gost-entries (list g))
     (post-fix-for-gost g after-point))))

;;; Fixing a broken supervised condition

; (trace supervised-spanned-etags)
; (trace remove-executed-etags)

;;; /\/: Note that in many cases, we don't *have* to create a "delivery
;;; point", because there's an existing node-end we could used.  For
;;; instance, if there are no spanned node-ends, we can use the end
;;; of the GOST range.  Or if there's a single minimal spanned node-end,
;;; we can use it.  But then there'd be more cases to test.

;;; /\/: Should print a message to say what unexecuted node-ends were
;;; spanned (if any).  Probably best to determine that before creating
;;; the "delivery point".

(defun fix-broken-supervised (g after-point)
  ;; Create a node to act as the "delivery point".
  (let* ((d-tag (db-call 'ads-add-node :type 'dummy :reason '(:exec)))
	 (d-begin (etag d-tag :begin))
	 (d-end (etag d-tag :end)))
    (dev-debug :trace "Delivery point for ~W = ~W" g d-tag)
    ;; Link begin_of d after the after-point
    (db-call 'link-etags-else-error after-point d-begin)
    ;; Get a list of the node-ends spanned by the GOST entry
    ;; that have not yet been executed.
    (let* ((spanned (db-call 'supervised-spanned-etags g))
	   (waiting (db-call 'remove-executed-etags spanned)))
      ;; If there are such node-ends, link end_of d before them;
      ;; otherwise link end_of d before the node-end at the end
      ;; of the GOST range.
      (if waiting
	  (dolist (w waiting)
	    (db-call 'link-etags-else-error d-end w))
	(db-call 'link-etags-else-error d-end (tgm-gost-node-end g))))
    ;; Change g's value to have end_of d as the contributor.
    ;; /\/: Should use the condition_contributor_node_end.
    (db-call 'setvalue g
      (tgm-make-gost-contributors
        :none
	(list d-end)))
    ;; Give end_of d p=v as an effect in the TOME.
    ;; /\/: Should use the condition_contributor_node_end.
    (add-etag-effects d-end
      (list (make-pv-pair-from-gost g)))
    ;; Post a FIX to achieve p=v at end_of d after the after-point.
    ;; KS-FIX wants a GOST entry that represents the cond to achieve.
    ;; Normally, the broken GOST is what's wanted, but we've already
    ;; handled that by putting in end_of d as the contributor.  What
    ;; we need to represent now is the condition at end_of d.  An
    ;; unsupervised seems best.  KS-FIX will turn it into an achieve.
    (post-fix-for-gost
      (tgm-make-gost-entry
        'unsupervised
	(tgm-gost-pattern g)
	(tgm-gost-value g)
	d-end)
      after-point)))


;;;; Support code for KS-EXECUTION-FAILURE and KS-UNEXPECTED-WORLD-EVENT

;;; Adding effects

(defun add-etag-effects (etag effects)
  (loop for (p v) in effects do
    (let ((tome (tgm-make-tome-entry p etag)))
      (db-call 'setvalue tome v))))

;;; After-points

(defun get-execution-after-point () ; -> etag
  (let* ((done-fringe (db-call 'minimal-done-fringe))
	 (not-done-fringe (db-call 'minimal-not-done-fringe))
	 (after-point
	   (if (length=1 done-fringe)
	       (first done-fringe)
	     (make-execution-after-point done-fringe))))
    ;; Link the after-point before all node-ends that have
    ;; not yet been executed.
    (dolist (etag not-done-fringe)
      (db-call 'link-etags-else-error after-point etag))
    ;; Make it the global after-point.
    (db-call 'set-global-after-point after-point)
    ;; And return it.
    after-point))

(defun make-execution-after-point (done-fringe) ; -> etag
  (dev-debug :trace "Making an after-point.")
  ;; /\/: Should give the node 0 duration.
  (let* ((node-tag (db-call 'ads-add-node :type 'dummy :reason '(:exec)))
	 (begin-etag (etag node-tag :begin))
	 (end-etag (etag node-tag :end)))
    ;; Link the node-ends in the done-fringe before the begin_of the
    ;; new node.
    (dolist (done-etag done-fringe)
      (db-call 'link-etags-else-error done-etag begin-etag))
    ;; Mark the new node itself as having been executed.
    (db-call 'set-node-exec-status node-tag :finished)
    ;; Return the end_of the new node.
    end-etag))

;;; Block backtracking / push alternatives

(defun block-backtracking ()
  (db-call 'atm-push-alternatives)
  ;; Set up an alternative so that if we run out of alternatives, the
  ;; planner gracefully gives up.  (Just as in KS-SET-TASK.)
  (am-request :POST-NO-MORE-ALTERNATIVES))

;;; Agenda posting

(defun post-fix-for-gost (gost-entry after-point)
  (post-fix `(:FIX :FIX ,gost-entry ,after-point)))

#+:undef
(defun post-fix-for-gost (gost-entry after-point)
  (let ((cond (make-achieve-from-gost gost-entry)))
    (post-achieve `(:ACHIEVE ,cond) :trigger (trigger-for-cond cond))))

#+:undef
(defun make-achieve-from-gost (gost-entry)
  ;; /\/: Same as make-cond-from-gost in KS-FIX.lisp
  (list 'achievable
	(tgm-gost-pattern gost-entry)
	(tgm-gost-value gost-entry)
	(tgm-gost-node-end gost-entry)))

(defun post-fix (body &rest initargs)
  ;; Need a more abstract way to deal with fix bodies /\/
  ;; Body is (:FIX subop gost after-point ...)
  (assert (eq (first body) :fix))
  (let ((con-pattern (tgm-gost-pattern (third body))))
    (apply #'post-agenda
	   body
	   :level (effect-level (car con-pattern))
	   initargs)))


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
