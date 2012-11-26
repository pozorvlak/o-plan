;;;; File: KS-UNEXPECTED-WORLD-EVENT.lisp
;;; Contains: KS to handle unplanned changes in the world.
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: 16 March 1995
;;; Updated: Sun Sep  5 19:32:56 1999 by Jeff Dalton
;;; Copyright: (c) 1995, AIAI, University of Edinburgh

;;; KS-EXECUTION-FAILURE: effects in the plan fail to occur.
;;; KS-UNEXPECTED-WORLD-EVENT: effects not in the plan occur anyway.

(in-package :oplan-knowledge-source)

;;; Something's happened in the world that was not in the plan.
;;; This is reported as a time, an event pattern, and a list
;;; of effects as (p v) pairs.  For instance, we may get an
;;; event {landslide} with effects
;;;   {status road-a} = blocked
;;;   {status road-b} = blocked
;;;
;;; In Lisp, this is (landslide) and
;;;   (((status road-a) blocked) ((status road-b) blocked))
;;;
;;; [Events are similar to plan actions -- they're just not in the plan.]
;;;
;;; The effects may break GOST ranges in the plan.  If so, we have
;;; to try to satisfy those conditions some other way.  But even if
;;; nothing is broken, we have to add a node to represent the world
;;; event.  Even if the event's effects don't make any difference now,
;;; they may matter later on.
;;;
;;; Note that the new node's effects can't be added until we've
;;; removed any broken GOST entries.  Otherwise the TGM might try
;;; to preserve those entries when we put in the effects.
;;; /\/: Not that we actually use the TGM.
;;;
;;; The new node represents something that has definitely and already
;;; happened.  So it must be linked after all node-ends that have
;;; already been executed and before all node-ends that have not yet
;;; been executed.
;;;
;;; A GOST entry loses a contributor when:
;;;   * the condition is at a node-end that has not been executed,
;;;   * the contributor is a node-end that has been executed, and
;;;   * the unexpected world-event has a conflicting effect.
;;;
;;; A GOST that loses all its contributors is "truly broken", as in
;;; KS-EXECUTION-FAILURE.
;;;
;;; So here's what we do:
;;;
;;;   /\/: Should we push a context before changing the plan state?
;;;
;;;   Add an event node, E, to represent the world event.  Link it
;;;   after all node-ends that have been executed and before all node-ends
;;;   that haven't.  Mark E as having already been executed.
;;;
;;;   Edit the GOST to remove any contributors that can no longer
;;;   contribute, and get a list of the truly broken GOST entries.
;;;
;;;   For each truly broken GOST entry g:
;;;     Set up for and post a :FIX agenda entry as in KS-EXECUTION-FAILURE,
;;;     using end_of E as the after-point.
;;;
;;;   Add the world event's effects at end_of E.  This should be easy,
;;;   because we've removed or modified all conflicting GOST entries,
;;;   because of the way E's linked, and because all PSVs are bound.
;;;
;;;   If there were no truly broken GOST entries, we're done.
;;;
;;;   Otherwise, push the current list of alternatives, so we don't
;;;   backtrack into them while dealing with the :FIX agenda-entries,
;;;   and post a :CONTINUE-EXECUTION to continue execution after the
;;;   fixes have been made. 
;;;
;;; /\/: There are some complications that don't occur with an EXECUTION-
;;; FAILURE.  It's not completely clear what to do about them.  When there's
;;; a failure at some node-end, some effects may not occur.  But nothing
;;; that depended on those effects will be sent off for execution until
;;; the plan's been fixed, because nothing's sent until all "contributors"
;;; have completed execution.  (What we have here is a failure as a
;;; completion.  So anything that needed the effects will still be waiting.)
;;; Maybe _other_ things might execute, but that should be ok.
;;;
;;; But with an UNEXPECTED-WORLD-EVENT, some things may execute (ie, be
;;; sent to the World for execution) when they shouldn't.  (Maybe the
;;; Exec starts something happening in the World and an unexpected
;;; event happens before the World receives that message.  Even greater
;;; timing problems occur when the Planner's brought in.)  We might try
;;; to handle this by giving the World a more elaborate model, so that
;;; plan steps effected by the unexpected event would fail, but then we
;;; might get a bunch of failures.  Getting everything right in such
;;; cases may be tricky.
;;;
;;; We therefore assume that things don't happen so quickly that there
;;; are timing problems.  However, there's still the case where several
;;; things are supposed to happen at the same time.  What if an unexpected
;;; event happens in the middle of them?
;;;

(defun KS-UNEXPECTED-WORLD-EVENT (event)
  (destructuring-bind (when pattern effects) (cdr (ag-body event))
    (declare (ignore when))
    
    (dev-debug :trace "Unexpected world event: ~A" pattern)
    (when effects
      (dev-debug :trace "Effects: ~%~{~7T~W = ~W~^~%~}"
		 (flatten-one-level effects)))

    ;; Create a node to represent the event.
    (let* ((event-tag
	    (db-call 'ads-add-node
		     :type 'event
		     :pattern pattern)))

      (dev-debug :trace "Event-point = ~W" event-tag)

      ;; Mark the event node as having been executed.
      (db-call 'set-node-exec-status event-tag :finished)

      ;; Link it after everything that has already executed and before
      ;; everything that hasn't.
      (db-call 'pinch-plan-at event-tag :finished)

      (let ((affected-gost (db-call 'event-broken-gost-entries effects)))

	(when (null affected-gost)
	  ;; No GOST cares.  We're outa here.
	  (dev-debug :trace "The event does not affect the plan.")
	  ;; But first put in the event's effects.
	  ;; /\/: Should use the effect_node_end from the TF defaults.
	  (add-etag-effects (etag event-tag :end) effects)
	  (return-from KS-UNEXPECTED-WORLD-EVENT))

	;; Yes, some affected GOST.
	(dev-debug :trace "Affected GOST:~%~{~7T~W~^~%~}" affected-gost)

	;; Check for multiple-contributors, which we can't handle yet. /\/
	(when (some #'gost-has-multiple-contributors-p affected-gost)
	  (cerror "Continue plan execution."
		  "Some of the affected GOST entries have > 1 contributor.")
	  (return-from KS-UNEXPECTED-WORLD-EVENT))

	;; If we reach this point, we have some "truly broken" GOST ranges.
	;; We have to replace them by agenda entries that will arrange for
	;; the conditions to be satisfied in another way.

	;; Post a FIX agenda entry for each broken truly GOST range,
	;; altering the contributor or removing the range as required.
	(let ((after-point (etag event-tag :end)))
	  (dolist (g affected-gost)
	    (fix-broken-gost g after-point)))

	;; Put in the event's effects
	;; /\/: Should use the effect_node_end from the TF defaults.
	(add-etag-effects (etag event-tag :end) effects)

	;; Make sure we don't backtrack into alts created before
	;; plan execution began.
	(block-backtracking)

	;; Arrange to continue execution.
	(post-agenda `(:CONTINUE-EXECUTION ,(etag event-tag :end)))

	;; And we're done.
	nil))))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
