;;;; File: KS-CHECK-PLAN.lisp
;;; Contains: The KS for requesting a plan check.
;;; Author: Jeff Dalton
;;; Created: Thu Jul 29 19:04:43 1993 by Jeff Dalton
;;; Updated: Sat May 22 16:28:20 1999 by Jeff Dalton
;;; Copyright: (c) 1992, AIAI, University of Edinburgh

(in-package :oplan-knowledge-source)

;;; This KS handles:
;;;
;;; (:CHECK-PLAN)
;;;
;;;    Ask the DM to run the sanity-checker.  Sends the error count
;;;    to the TA as :CHECKED <count>.
;;;
;;; (:CHECK-PLAN :GET-PLAN-STATISTICS)
;;;
;;;    /\/: Should be :GET-PLANNING-STATISTICS.  KS-EVAL-PLAN gets some
;;;    plan statistics, though.
;;;
;;;    Gets planning statistics in the form used by the auto-tester: an
;;;    a-list indexed by statistic names as keywords.  This is sent
;;;    to the TA as :CHECKED <alist>.
;;;
;;; (:CHECK-PLAN :GET-PLAN-DESCRIPTION &OPTIONAL destination expected)
;;;
;;;    Get a description of the plan in the form used by the auto-tester.
;;;    Destination and expected, if given and not nil, must be namestrings
;;;    or pathnames.
;;;
;;;    The normal behavior of :GET-PLAN-DESCRIPTION is determined by
;;;    by destination.  If destination is a name, a description of
;;;    the current plan state is written to the indicated file and
;;;    destination is sent to the TA as confirmation.  Otherwise,
;;;    the description itself is sent to the TA.
;;;
;;;    The expected parameter is used when the caller has an idea of
;;;    what the description of the current state should be and does
;;;    not want to see the description if it matches.  So, if expected is
;;;    not nil, it should name a file that contains a plan description
;;;    which the description of the current plan should match.  If it
;;;    does match, (:OK statistics) is sent to the TA.  Otherwise, the
;;;    "normal" behavior occurs as desribed above.  (The statistics
;;;    are always provided, either in (:OK stats) or in the plan
;;;    description, so that the test controller can keep a running
;;;    total.)
;;;
;;;    A typical use of expected is to avoid the overhead of writing a
;;;    description to a file if there already is a file that is likely
;;;    to be correct.
;;;
;;; A result is always sent to the TA as :CHECKED ... so that the
;;; Task Assigner can see :CHECKED and know what it's got.  The result
;;; of :GET-PLAN-DESCRIPTION is sent as :CHECKED :DESCRIPTION <result>.
;;;
;;; The form of a plan description must be compatible with the
;;; auto-tester's definition.
;;;
;;; When writing to a file, we write items in each part of the
;;; description (nodes, TOME, GOST, etc) in a canonical order so
;;; that the description is independent of how entries are stored
;;; internally.  Note that some things (e.g. nodes) are already
;;; sorted.  In other cases, we use canonical-description-order,
;;; an alphabetical sort that descends sublists.
;;;
;;; /\/: We don't pretty-print to the file in part because the
;;;      pprint dispatch table may not be suitable, and getting a
;;;      standard one is still different in different Lisps [Sep 93].
;;;
;;;      When printing all on one line would be too hard to read,
;;;      there's a parameter :BREAKUP T that will print sublists
;;;      on separate lines.
;;;

(defun KS-CHECK-PLAN (event)
  (let* ((message (ag-body event))
	 (subop (if (rest message) (second message) nil)))
    (ecase subop

      ((NIL)
       (ipc-send-out :CHECKED (db-request :CHECK-PLAN)))

      ((:GET-PLAN-STATISTICS)
       (ipc-send-out :CHECKED :STATISTICS (get-plan-statistics)))

      ((:GET-PLAN-DESCRIPTION)
       (let* ((destination (third message))
	      (expected (fourth message))
	      (result (write-plan-description destination expected)))
	 (ipc-send-out :CHECKED :DESCRIPTION result))))))

;;; Write-plan-description obtains the description by making several
;;; db-requests.  The node description is the same as the raw form
;;; used by the PW viewer, since that is the user-level view of the
;;; plan.  In this, PSVs are replaced by their values, if they have
;;; them.  The TOME and GOST descriptions also have PSVs filled in.

;;; /\/: It might be better not to fill in PSVs, at least for the
;;; TOME and GOST.

(defun write-plan-description (destination expected)
  (let ((stats   (get-plan-statistics))
	(levels  (db-request :GET-DOMAIN-LEVELS))
	(nodes   (db-request :GET-PLAN-NODES :DESCRIPTIONS))
	(tome    (db-request :GET-TOME))
	(gost    (db-request :GET-GOST))
	(psvs    (db-request :GET-PSVS))
	(rut     (db-request :GET-RUT))
	(world-1 (db-request :QA-ALL "1"))
	(world-2 (db-request :QA-ALL "2")))

    (let ((action-levels (canonical-description-order (first levels)))
	  (effect-levels (canonical-description-order (second levels)))

	  (psv-descrs (first psvs))	;descriptions
	  (psv-equivs (second psvs))	;equivalence classes

	  (tome    (canonical-description-order tome))
	  (gost    (canonical-description-order gost))
	  (rut     (canonical-description-order rut))
	  (world-1 (canonical-description-order world-1))
	  (world-2 (canonical-description-order world-2))
	  (all nil))
      (setq all (list stats action-levels effect-levels
		      nodes tome gost psv-descrs psv-equivs rut
		      world-1 world-2))

      (when (and expected (equal all (file->list expected)))
	(return-from write-plan-description `(:ok ,stats)))

      ;; Deal with destination.  This is the so-called normal behavior.
      (etypecase destination
	(null
	 all)
	((or string pathname)
	 (with-open-file (out destination :direction :output
			                  :if-exists :supersede)
	   (labels ((out (label contents &key (breakup nil))
		      (format out "~%;;; ~A:~%" label)
		      (format out "(~%")
		      (dolist (part contents)
			(cond ((and breakup (consp part))
			       (princ "(" out)
			       (writeout (car part))
			       (dolist (p (cdr part))
				 (princ "   " out)
				 (writeout p))
			       (princ ")" out)
			       (terpri out))
			      (t
			       (writeout part))))
		      (format out ")~%"))
		    (writeout (part)
		      (write part :stream out
			 :pretty nil	; /\/ t uses pprint-dispatch
			 :case :downcase
			 :level nil
			 :length nil
			 :circle nil)
		      (terpri out)))
	     (out "Statistics"              stats)
	     (out "Action levels"           action-levels)
	     (out "Effect levels"           effect-levels)
	     (out "Nodes"                   nodes :breakup t)
	     (out "TOME"                    tome)
	     (out "GOST"                    gost :breakup t)
	     (out "PSV descriptions"        psv-descrs)
	     (out "PSV equivalence classes" psv-equivs)
	     (out "Resource usage table"    rut)
	     (out "World at end of node-1"  world-1)
	     (out "World at end of node-2"  world-2)))
	 destination)))))

;;; We want to know such things as whether there's a change in the
;;; number of agenda cycles needed to get a result.

(defun get-plan-statistics () ; -> list of pairs
  (canonical-description-order
    (append
      (am-request :get-plan-statistics))))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
