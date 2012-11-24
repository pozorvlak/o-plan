;;;; File: atm.lsp
;;; Contains: Agenda Table Manager.
;;; Author: Richard Kirby (rbk) and Jeff Dalton
;;; Created: Wed Nov 27 18:38:45 1991
;;; Updated: Sat Jan  2 20:58:44 1999 by Jeff Dalton
;;; Copyright: (c) 1992, 1993, 1994, AIAI, University of Edinburgh

;;; Manages some aspects of agendas and alternatives.  The rest is in
;;; the Controller (AM).

(in-package :oplan)

;;;; Functions that can be redefined by the user:

(proclaim '(notinline atm-agenda-cost-fn
	              atm-agenda-entry-cost-fn
		      atm-state-cost-fn
	              atm-state-rating-fn
	              atm-alt-rating-fn))


;;;; Triggered and untriggered agenda tables (context-layered).

;;; When an AE is triggered, it is put on the triggered-agenda if we have
;;; the authority to process that AE and on the waiting-agenda if we do not.

;;; So, somewhat misleadingly, both the triggered and waiting agendas contain
;;; entries that have been triggered.  Maybe we should call the triggered
;;; agenda the "ready" agenda, or something like that.  /\/

(definit :dm *triggered-agenda* nil)
(definit :dm *waiting-agenda* nil)
(definit :dm *untriggered-agenda* nil)


;;;; Authority

(definit :dm *level-authority* :inf)

;;; The next two variables are used to detect when we move to a context
;;; in which some authority changes have not yet been taken into account.

(definit :dm *authority-timestamp* 0)
(definit :dm *context-authority-timestamp* 0) ;context-layered


;;;; Agenda cost estimates

;;; *Agenda-cost* (context-layered) is an estimated, collective cost
;;; of all entries in both the triggered and untriggered agenda tables.
;;; It is used when rating alternatives.

(definit :dm *agenda-cost* 0)

;;; atm-agenda-entry-cost-fn estimates the cost of a single agenda
;;; entry.  It can return a positive integer or :inf.

(defun atm-agenda-entry-cost-fn (ag)
  (case (ag-type ag)
    (:expand
     (round 4 (fix-max (ag-level ag) 1)))
    (:achieve
     (round 4 (fix-max (ag-level ag) 1)))
    (t
     1)))

;;; atm-agenda-cost-fn estimeates the collective cost of the triggered
;;; and untriggered agenda tables.

(defun atm-agenda-cost-fn ()
  (round (ctxt-symbol-value '*agenda-cost*) 2))


;;;; Agenda table operations.

;;; Messages

(defmessage (:dm :add-agenda-entry) (ae)
  (atm-add-untriggered-agenda-entry ae)
  :ok)

(defmessage (:dm :request-current-agenda) ()
  (unless (= *authority-timestamp*
	     (ctxt-symbol-value '*context-authority-timestamp*))
    (atm-handle-new-authority))
  ;; Triggers are checked here and only here.
  (check-for-triggered-entries)
  (atm-get-triggered-agenda))

(defmessage (:dm :processed-agenda-entry) (ae-id)
  (atm-remove-triggered-agenda-entry ae-id)
  :OK)

(defmessage (:dm :fill-in-alternative) (alt)
  (prog1 (atm-fill-in-alternative alt)
    (whats-going-on "Just added an alternative.")
    (whats-going-on "DATABASE STATE~%==============")
    (print-database)
    ))


;;; Get and set operations

;;; atm-get-untriggered-agenda -- called from triggering code.

(defun atm-get-untriggered-agenda ()
  (ctxt-symbol-value '*untriggered-agenda*))

;;; atm-get-triggered-agenda -- called from message handling
;;; loop when received a :REQUEST-CURRENT-AGENDA message from
;;; the AM.

(defun atm-get-triggered-agenda ()
  (ctxt-symbol-value '*triggered-agenda*))

(defun atm-set-triggered-agenda (agenda)
  (setf (ctxt-symbol-value '*triggered-agenda*) agenda))

(defun atm-get-waiting-agenda ()
  (ctxt-symbol-value '*waiting-agenda*))

(defmessage (:dm :request-waiting-agenda) ()
  (atm-get-waiting-agenda))


;;; Add and remove operations

;;; atm-add-untriggered-agenda-entry -- called from message handling
;;; loop when received a :ADD-AGENDA-ENTRY message from the AM.

(defun atm-add-untriggered-agenda-entry (ag)
  (&incf (ctxt-symbol-value '*agenda-cost*)
	 (setf (ag-cost ag)
	       (atm-agenda-entry-cost-fn ag)))
  (push ag (ctxt-symbol-value '*untriggered-agenda*)))

;;; atm-remove-untriggered-agenda-entry -- called only from triggering
;;; code.  N.B. arg must be the same (eq) ag, not a copy.

(defun atm-remove-untriggered-agenda-entry (ag)
  (setf (ctxt-symbol-value '*untriggered-agenda*)
	(remove-1-eq ag (ctxt-symbol-value '*untriggered-agenda*))))

;;; atm-add-triggered-agenda-entry -- called from triggering code.

(defun atm-add-triggered-agenda-entry (ag)
  (push ag (ctxt-symbol-value '*triggered-agenda*)))

;;; atm-remove-triggered-agenda-entry -- called from message handling
;;; loop when received a :PROCESSED-AGENDA-ENTRY message from the AM.

(defun atm-remove-triggered-agenda-entry (ag-id)
  (check-type ag-id fixnum)
  (let ((ag (find-agenda-entry ag-id (ctxt-symbol-value '*triggered-agenda*))))
    (when ag
      (&decf (ctxt-symbol-value '*agenda-cost*)
	     (ag-cost ag))
      (setf (ctxt-symbol-value '*triggered-agenda*)
	    (remove-1-eq ag (ctxt-symbol-value '*triggered-agenda*)))
      ag-id)))

(defun find-agenda-entry (id agenda)	;/\/: move to agendadef.lsp
  (declare (fixnum id))
  (dolist (ag agenda)
    (when (fix= id (ag-id ag))
      (return ag))))

;;; atm-add-waiting-agenda-entry -- called from triggering code.

(defun atm-add-waiting-agenda-entry (ag)
  (push ag (ctxt-symbol-value '*waiting-agenda*)))


;;; Other operations

;;; atm-empty-tables-p -- called from triggering code to see if the
;;; agendas are now empty.

(defun atm-empty-tables-p ()
  (let ((triggered (ctxt-symbol-value '*triggered-agenda*))
	(waiting (ctxt-symbol-value '*waiting-agenda*))
	(untriggered (ctxt-symbol-value '*untriggered-agenda*)))
    (and (null triggered)
	 (null waiting)
	 (length=1 untriggered))))

;;; why-are-we-waiting -- called by the AM when there's nothing to do.

(defmessage (:dm :why-are-we-waiting) ()
  (let ((triggered (ctxt-symbol-value '*triggered-agenda*))
	(waiting (ctxt-symbol-value '*waiting-agenda*))
	(untriggered (ctxt-symbol-value '*untriggered-agenda*)))
    (assert (null triggered))
    (append
      (when waiting
	'(:authority))
      (when untriggered			;(length>1 untriggered)?  /\/
        '(:triggers)))))

;;; agenda-status

(defmessage (:dm :agenda-status) ()
  (let ((triggered (ctxt-symbol-value '*triggered-agenda*))
	(waiting (ctxt-symbol-value '*waiting-agenda*))
	(untriggered (ctxt-symbol-value '*untriggered-agenda*)))
    (append
      (when triggered
	'(:triggered))
      (when waiting
	'(:waiting))
      (when untriggered
        '(:untriggered)))))


;;;; Authority operations

;;; /\/: Changing authority can put things on the agenda so that planning
;;; occurs.  But it should be possible to change more than one setting
;;; (e.g. both :level and :phase) before planning.  So, either planning
;;; shouldn't occur when authority increases, but wait for a separate
;;; "you can plan now" message, or the :authority message should be able
;;; to change several settings (with a plist, perhaps, as its argument).

(defmessage (:dm :authority) (type value)
  ;; N.B. The args are checked in the Controller.
  (atm-set-authority type value)
  (atm-handle-new-authority)
  :ok)

(defmessage (:dm :what-authority) (type)
  (ecase type
    (:level *level-authority*)))

(defun atm-set-authority (type value)
  (incf *authority-timestamp*)
  (ecase type
    (:level (setq *level-authority* value))))

(defun atm-have-authority-to-process (ag)
  (&<= (ag-level ag) *level-authority*))

(defun atm-handle-new-authority ()
  ;; Adjust the waiting and triggered agendas.
  (let ((triggered (atm-get-triggered-agenda))
	(waiting (atm-get-waiting-agenda))
	(new-triggered '())
	(new-waiting '()))
    (dolist (trig triggered)
      (if (atm-have-authority-to-process trig)
	  (push trig new-triggered)
	(push trig new-waiting)))
    (dolist (wait waiting)
      (if (atm-have-authority-to-process wait)
	  (push wait new-triggered)
	(push wait new-waiting)))
    (setf (ctxt-symbol-value '*triggered-agenda*)
	  (nreverse new-triggered))
    (setf (ctxt-symbol-value '*waiting-agenda*)
	  (nreverse new-waiting))
    (setf (ctxt-symbol-value '*context-authority-timestamp*)
	  *authority-timestamp*)))


;;;; Alternative and plan-state rating

;;; Alt ratings are used to select the best alternative when a choice
;;; is made.  Lower values are considered better.  That is, the values
;;; represent a cost.

;;; atm-alt-rating-fn estimates the total cost of the best path through
;;; the current state to a solution (as in the A* algorithm).

(defun atm-alt-rating-fn (alt)
  (if (and (alt-alt-agenda alt)		;nil if from switch-alternatives
	   (eq (ag-type (alt-alt-agenda alt))
	       :NO-MORE-ALTERNATIVES))
      ;; Make sure :no-more-alternatives alts are chosen last.
      most-positive-fixnum
    ;; All other alts get a computed rating.
    (atm-state-rating-fn)))

;;; atm-state-rating-fn rates the current plan state.

(defun atm-state-rating-fn ()
  (&+ (atm-state-cost-fn)		;cost to get here
      (atm-agenda-cost-fn)))		;cost from here to a solution

;;; atm-state-cost-fn returns the cost already paid to reach the
;;; current state.

(defun atm-state-cost-fn ()
  ;; Keep simple at moment - just return the number of nodes in the current
  ;; plan state.  NOTE LOW IS BEST!
  (let ((count 0))
    (walk-nodes
      #'(lambda (n)
	  (declare (ignore n))
	  (incf count)))
    (assert (= count (floor (node-end-count) 2)))
    (assert (> count 0))
    (if (> count 0)
	count
      most-positive-fixnum)))

;;; atm-fill-in-alternative -- called from the AM when creating a new alt,
;;; because some of the slots can be filled in only by the DM.

(defun atm-fill-in-alternative (alt)
  (when (ctxt-symbol-value '*context-was-poisoned*)
    (error "Attempt to make alt in poisoned context.~%Alt: ~S." alt))
  (let ((context (context-number *context*)))
    (setf (alt-context alt) context)
    (setf (alt-rating alt) (atm-alt-rating-fn alt))
    (push-context)
    alt))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
