;;;; File: ta-questions.lsp
;;; Contains: DM support for TA-questions
;;; Author: Jeff Dalton
;;; Created: July 1998
;;; Updated: Mon May 31 04:03:03 1999 by Jeff Dalton
;;; Copyright: (c) 1998, AIAI, University of Edinburgh

(in-package :oplan)

(definit :dm *ta-question* nil)

(defun ta-question ()
  (ctxt-symbol-value '*ta-question*))

(defun set-ta-question (q)
  (setf (ctxt-symbol-value '*ta-question*) q))

(defstruct ta-question
  keyword				;e.g. :schema-order
  agenda-entry				;the ae that asked the question
  question-data				;(:question . whatever)
  triggered-agenda			;agenda lists captured by the
  untriggered-agenda			;  :dm :ta-question method
  waiting-agenda			;  when a question is asked
  (answer-p nil)			;set to t when an answer's received
  (answer-data nil))			;an arbitrary object

;;; When a question is asked, we have to make all planning wait for
;;; an answer.  In theory, we could process other agenda entries
;;; while we wait, but that would lead to "unintended" changes in
;;; the planner's behaviour.  Plans might be found in different
;;; orders, or might have different links, and so on.  [But "so on"
;;; to what?  It's not clear.]

(defmessage (:dm :ta-question) (question-kwd agenda-entry data)
  (assert (null (ta-question)))		;there can be only one
  (let ((triggered (atm-get-triggered-agenda))
	(untriggered (atm-get-untriggered-agenda))
	(waiting (atm-get-waiting-agenda)))

    (ta-question-event (list :question question-kwd (ag-body agenda-entry)))

    ;; Remember everything we'll need to know when the question
    ;; is answered.
    (set-ta-question
     (make-ta-question
       :keyword question-kwd
       :agenda-entry agenda-entry
       :question-data data
       :triggered-agenda triggered
       :untriggered-agenda untriggered
       :waiting-agenda waiting))

    ;; Empty the triggered, untriggered, and waiting agendas
    ;; Note that :planner-finished must be added to the untriggered
    ;; agenda.
    (setf (ctxt-symbol-value '*triggered-agenda*)
	  '())
    (setf (ctxt-symbol-value '*untriggered-agenda*)
	  '())
    (setf (ctxt-symbol-value '*waiting-agenda*)
	  '())

    :ok))

#+:undef
(defmessage (:dm :ta-question) (question-kwd agenda-entry data)
  (atm-add-untriggered-agenda-entry agenda-entry)
  :ok)

;;; Usually, the question will eventually be answered.  Here we just
;;; store the answer, so it can be picked up by a KS.

(defmessage (:dm :ta-question-answer) (question-kwd data)
  (let ((q (ta-question)))
    (assert q)
    (assert (eq (ta-question-keyword q) question-kwd))
    (setf (ta-question-answer-p q) t
	  (ta-question-answer-data q) data)
    :ok))

;;; A :question agenda-entry has an :answer trigger, for which
;;; is-triggered-p calls have-question-answer-p.  The trigger
;;; is a list: (:answer question-kwd).  Only one question can
;;; be outstanding in a given context at a given time, but we
;;; check that it has the right question-kwd.

(defun have-question-answer-p (question-kwd)
  (let ((q (ta-question)))
    (assert q)
    (assert (eq (ta-question-keyword q) question-kwd))
    (ta-question-answer-p q)))

;;; A KS can grab the answer after the answer's arrived.  This can be
;;; done only once for a given question, because it causes some agenda
;;; state to be restored and the question record to be erased.
;;; This undoes the effects of a :dm :ta-question message.

(defmessage (:dm :get-question-answer) (question-kwd)
  (let ((q (ta-question)))
    (assert q)
    (assert (eq (ta-question-keyword q) question-kwd))
    (assert (ta-question-answer-p q))

    ;; Undo the effects of the :dm :ta-question message
    (setf (ctxt-symbol-value '*triggered-agenda*)
	  (ta-question-triggered-agenda q))
    (setf (ctxt-symbol-value '*untriggered-agenda*)
	  (ta-question-untriggered-agenda q))
    (setf (ctxt-symbol-value '*waiting-agenda*)
	  (ta-question-waiting-agenda q))
    (set-ta-question nil)

    ;; The question-asking agenda-entry would have been removed from the
    ;; triggered aganda if we hadn't removed the whole agenda before the
    ;; AM received the :kpready message (from when that agenda-entry was
    ;; being processed).  So we remove it now, and the answer-grabbing
    ;; KS should send the AM an :agenda message to put it back on the
    ;; untriggered aganda.
    (atm-remove-triggered-agenda-entry (ag-id (ta-question-agenda-entry q)))

    ;; /\/: Actually an :agenda message would try to assign default
    ;; slot values, which would cause an error, because the entry
    ;; already has an id.  (That would have to change if we regularly
    ;; did KS-staging.)  Anyway, we will, for now, do it ourself.
    ;; It may seem that we may as well do this by not removing it from
    ;; the triggered aganda in the first place (even though this would
    ;; = putting it on the triggered, rather than untriggered, agenda,
    ;; contrary to what's done for KS-staging).  But the KP copies the
    ;; agenda entry when it gets it, so the one on the triggered agenda
    ;; is obsolete (if the KS made any changes to ag slots, which it
    ;; does as part of the question mechanism).
    (atm-add-untriggered-agenda-entry (ta-question-agenda-entry q))

    ;; Return what the KS needs to know.
    (list (ta-question-answer-data q)
	  (ta-question-agenda-entry q))))


;;; History re backtracking
;;;
;;; It can be confusing to answer questions while the planner is
;;; backtracking, especially if there's nothing that indicates
;;; explicitly that backtracking has occurred or how far back
;;; it's gone.  So we try to do something about that here.
;;;
;;; A "question event" is a description of a significant ta-question-
;;; related event.  The events definied so far are:
;;;
;;;   (:question question-kwd agenda-body)
;;;
;;; There's also a "marker" that can appear among the events:
;;;
;;;   (:backtrack-point)
;;;
;;; The question "trail" is a list of the question-events that have
;;; occurred in the current context or any of its ancestors, ordered
;;; most-recent-first.  It is context-layered and so always shows the
;;; path (through question events) that led to the current context.
;;;
;;; The question "history" is a list of trails, with the first element
;;; of the history normally being eq to the current trail.  But the
;;; history is *not* context-layered.  When the system backtracks, it
;;; goes back to an earlier context.  The current trail will therefore
;;; become shorter, but the first element of the history will not.
;;; That allows us to detect that backtracking has occurred.
;;;
;;; We handle backtracking by pushing '(:backtrack-point) onto the
;;; front of the current trail and then pushing that trail onto
;;; the front of the history.  That restores the eq state, but
;;; the history now contains one more element.  The new element
;;; (eq to the current trail) shows where we are now, after
;;; backtracking; the previous element shows where we were just
;;; before backtracking; and the (:backtrack) entry on the
;;; current trail marks the point reached by backtracking so that
;;; we can still see it after more question events been added.
;;;
;;; Note that any two history elements will have a common initial
;;; segment: the question events that had occurred in their closest
;;; common ancestor context.  The history therefore describes a
;;; tree.

(definit :dm *ta-question-history* (list nil))	;not context-layered

(definit :dm *ta-question-trail* nil)		;context-layered

(defmacro ta-question-history ()
  '*ta-question-history*)

(defmacro ta-question-trail ()
  '(ctxt-symbol-value '*ta-question-trail*))

(defmessage (:dm :get-question-history) ()
  (ta-question-history))

(defun ta-question-event (e)
  (unless (eq (first (ta-question-history)) (ta-question-trail))
    ;; We've backtracked to before the last event.
    (push '(:backtrack-point) (ta-question-trail))
    (push (ta-question-trail) (ta-question-history))
    (require-eq-history-and-trail :after-backtrack-adjustment))
  ;; Record the event
  (push e (ta-question-trail))
  (setf (ta-question-history)
	(cons (ta-question-trail) (rest (ta-question-history))))
  (require-eq-history-and-trail :after-event-recorded))

(defun require-eq-history-and-trail (when)
  (unless (eq (first (ta-question-history)) (ta-question-trail))
    (error "Mixup in TA-question history ~S." when)))
  
;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
