;;;; File: KS-EXECUTION-FAILURE.lsp
;;; Contains: KS that deals with an execution failure
;;; Author: Jeff Dalton
;;; Created: Sat 18 February 1995
;;; Updated: Fri May 14 01:55:16 1999 by Jeff Dalton
;;; Copyright: (c) 1995, AIAI, University of Edinburgh

;;; KS-EXECUTION-FAILURE (this KS): effects in the plan fail to occur.
;;; KS-UNEXPECTED-WORLD-EVENT: effects not in the plan occur anyway.

(in-package :oplan-knowledge-source)

;;; Execution fails at a node-end (etag).  Some of the effects due to
;;; happen at that point may not happen; they're returned as failed-effects.
;;; Our task is to fix the plan so that any condition that needed one of the
;;; failed effects is satisfied in some other way.  Maybe there's already
;;; another contributor in the GOST entry, or maybe there's a "white knight".
;;; Otherwise, we'll have to try to add a node to the plan.
;;;
;;; Of course, we also win if there are no failed effects.  That's an
;;; execution failure in name only, we might say.
;;;
;;; So here's what we do:
;;;
;;;   Mark the node-end as having been executed.
;;;
;;;   If there are no failed effects, we're done.
;;;
;;;   /\/: Should we push a context before changing the plan state?
;;;   (One was pushed at the start of execution.)
;;;
;;;   Remove the TOME entries that correspond to the failed effects
;;;   by setting their value to :undef.
;;;
;;;   Determine which GOST entries are affected by the failed effects.
;;;   If there are none, we're done.
;;;
;;;   If we get this far, we have a failure we have to do something
;;;   about.
;;;
;;;   Go through the affected GOST entries.  If a GOST entry has more
;;;   than one contributor, see if any have survived.  (Is it possible
;;;   that more than one is from the same node-end, so that a failure
;;;   might take out more than one?  Presumably so.  /\/)  If there
;;;   are still some valid contributors, just reduce the contributor
;;;   list; otherwise record the GOST entry as truly broken.  If no
;;;   GOST entries are truly broken, we're done.  (/\/: At present,
;;;   we don't actually handle the multiple-contributor case.)
;;;
;;;   If we get this far, some GOST entries are truly broken, and we're
;;;   going to have to post ACHIEVEs for them.  Actualy, we'll post
;;;   FIX agenda entries rather than ACHIEVEs per se.
;;;
;;;   The ACHIEVEs will be AFTER a point that is linked after all node-ends
;;;   that have been executed so far.  Find the set of node-ends that have
;;;   been executed and that have no successor that has been executed.
;;;   This is the smallest set such that linking a node after all members
;;;   of the set will link it (directly or indirectly) after all node-ends
;;;   that have been executed.  If there's only one node-end in the set,
;;;   use it as the after-point.  Otherwise, add a new node, link it after
;;;   all members of the set, and use it as the after-point.
;;;
;;;   /\/: Austin thinks we should link the after-point before everything
;;;   that has not yet executed.  That would give us one point from which
;;;   to continue execution, which should make it easier to hold back
;;;   further execution by a trigger rather than (as at present) by agenda
;;;   priorities.  ...  Ok, we'll try it.
;;;
;;;   /\/: Should we always add a new node as the after-point?  If it's
;;;   supposed to represent the point in time at which execution failed,
;;;   an existing, already executed, node won't do.
;;;
;;;   Call the end_of the after-point A.  This is a node-end, not a node.
;;;   It will be the end_of that node.
;;;
;;;   For each truly broken GOST entry g:
;;;
;;;     g is "cond-type p = v at e from c".
;;;     e and c are node-end(-tag)s.
;;;
;;;     If g is not a supervised condition:
;;;        Remove g by setting its value to :undef.
;;;        Post a :FIX to re-establish the condition at the required point,
;;;          i.e. to "achieve p = v at e after A".
;;;
;;;     If g is a supervised condition:
;;;        p = v must be established over a range, rather than just at
;;;          a point.
;;;        Create a new dummy node d to act as the "delivery point".
;;;        Link d after the after-point, before e, and before all node-ends
;;;          that are spanned by g and have not yet been executed.
;;;        Change g's value to have d as the contributor.
;;;        Give d p=v as an effect in the TOME.
;;;        Post a FIX to re-establish p=v at d,
;;;          i.e. to "achieve p = v at d after A".
;;;        (For "which end of d", in all cases, see below.)
;;;
;;;   Push the current list of alternatives so we don't backtrack into
;;;   them.
;;;
;;;   Post a :CONTINUE-EXECUTION to continue execution after the fixes
;;;   have been made.  (:CONTINUE-EXECUTION has lower priority than anything
;;;   that's significant in planning, but it may be better to use triggers,
;;;   rather than priorities, to keep one from happening too soon. /\/)
;;;
;;; "Which end of d":
;;;
;;;   d is linked into the plan like this:
;;;
;;;     end_of after-node --> begin_of d --> end_of d --> spanned node-ends
;;;
;;;   Now, which end of d contributes p=v?  We'll use the default value
;;;   for condition_contributor_node_end (obtained from the TF compiler).
;;;   Call this node-end "ccne_of d".  We have to be sure not to leave
;;;   gaps in the GOST range, so we have to re-establish p=v at the same
;;;   end of d, so the :FIX should "achieve p = v at ccne_of d after A".
;;;   

(defun KS-EXECUTION-FAILURE (event)
  (destructuring-bind (when reason etag pattern failed-effects)
                      (cdr (ag-body event))
    (declare (ignore when reason))
    (dev-debug :trace "Failed to execute ~A ~W" etag pattern)

    ;; Mark the node-end has having been executed.
    (db-call 'set-etag-exec-status etag :finished)
    
    ;; Any failed effects?
    (when (null failed-effects)
      ;; No.  We're done.
      (dev-debug :trace "No failed effects, so off we go.")
      (tell-exec-to-execute-next-fringe etag)
      (return-from KS-EXECUTION-FAILURE))

    ;; Yes, some failed effects.
    (dev-debug :trace
      ;; /\/ Flatten effects list because in at least some Lisps
      ;; ~^ doesn't do the right thing in a ~:{ ... ~}.
      "Failed effects: ~%~{~7T~W = ~W~^~%~}"
      (flatten-one-level failed-effects))

    ;; Cancel the TOME entries for the failed effects.
    (db-call 'cancel-failed-tome-entries etag failed-effects)

    ;; Any affected GOST?
    (let ((affected-gost (db-call 'find-broken-gost-entries
				  etag failed-effects)))

      (when (null affected-gost)
	;; No GOST cares.  We're outa here.
	(dev-debug :trace "Though there are failed effects, no on cares.")
	(tell-exec-to-execute-next-fringe etag)
	(return-from KS-EXECUTION-FAILURE))

      ;; Yes, some affected GOST.
      (dev-debug :trace "Affected GOST:~%~{~7T~W~^~%~}" affected-gost)

      ;; Check for multiple-contributors, which we can't handle yet. /\/
      (when (some #'gost-has-multiple-contributors-p affected-gost)
	(cerror "Continue plan execution."
		"Some of the affected GOST entries have > 1 contributor.")
	(tell-exec-to-execute-next-fringe etag)
	(return-from KS-EXECUTION-FAILURE))

      ;; If we reach this point, we have some "truly broken" GOST ranges.
      ;; We have to replace them by agenda entries that will arrange for
      ;; the conditions to be satisfied in another way.

      ;; Find or create a point that's linked after everything that's
      ;; been executed so far.
      (let ((after-point (get-execution-after-point)))

	(dev-debug :trace "After-point = ~W" after-point)

	;; Post a FIX agenda entry for each truly broken GOST range
	;; altering the contributor or removing the range as required.
	(dolist (g affected-gost)
	  (fix-broken-gost g after-point))

	;; Make sure we don't backtrack into alts created before
	;; plan execution began.
	(block-backtracking)

	;; Post agenda enties that will continue plan execution.
	; (post-agenda `(:CONTINUE-EXECUTION ,etag))
	(post-agenda `(:CONTINUE-EXECUTION ,after-point))))))


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
