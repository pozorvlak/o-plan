;;;; File: alternatives.lsp
;;; Contains: "Alternative Manager"
;;; Author:  Jeff Dalton and Richard Kirby
;;; Created: Fri Nov  2 13:37:42 1990
;;; Updated: Sat Jan  2 21:41:17 1999 by Jeff Dalton
;;; Copyright: (c) 1995, AIAI, University of Edinburgh


(in-package :oplan)


;;;; The alternatives.

;;; N.B. The alt table is not context-layered.

(definit :am *alternatives* nil)

(definit :am *no-more-alternatives-alt* nil)


;;;; Message handlers

(defmessage (:am :alt-agenda) (alt-agenda)
  (post-alternative alt-agenda))

(defmessage (:am :post-no-more-alternatives) ()
  (post-alternative
    (make-agenda-entry
      :body '(:no-more-alternatives)
      :trigger t))
  (let ((nma-alt (first *alternatives*)))
    (assert (eq :no-more-alternatives (ag-type (alt-alt-agenda nma-alt))))
    (setq *no-more-alternatives-alt* nma-alt)
    (alt-id nma-alt)))

(defmessage (:am :get-alternatives) ()
  (atm-get-alts-agenda))

(defmessage (:am :begin-alt-transaction) ()
  (whats-going-on "Begin alt transaction")
  (atm-begin-alt-transaction))

(defmessage (:am :commit-alt-transaction) ()
  (whats-going-on "Commit alt transaction")
  (atm-commit-alt-transaction))

(defmessage (:am :abort-alt-transaction) ()
  (whats-going-on "Abort alt transaction")
  (atm-abort-alt-transaction))

(defmessage (:am :pick-alt) (reason)
  (dev-level-case
    (:detail  (dev-debug "Asked to pick alt because:~%    ~S" reason))
    (:minimal (dev-debug "Poison: ~S" (second reason))))
  (pick-an-alternative reason)
  :ok)

(defmessage (:am :delete-alt) (id)
  (atm-delete-alternative id))

(defmessage (:am :get-alt) (id)
  (atm-get-alternative id))


;;;; Basic operations

(defun atm-delete-alternative (alt-id)
  (setq *alternatives*
	(delete-1-eq (atm-get-alternative alt-id)
		     *alternatives*))
  nil)

(defun atm-get-alts-agenda ()
  *alternatives*)

(defun atm-get-alternative (alt-id)
  (check-type alt-id fixnum)
  (dolist (a *alternatives*
	     (error "There is no alternative named ~S." alt-id))
    (when (fix= (alt-id a) alt-id)
      (return a))))

(defun atm-get-number-of-alternatives ()
  (length *alternatives*))

(defun atm-clear-alts-agenda ()
  (setq *alternatives* nil))

(defun atm-flush-alternatives ()
  (setq *alternatives* (last *alternatives*)))


;;;; Alt transactions

;;; Alt transactions: adding alts with the ability to comit or abort
;;; Used, at present, only by KS-USER and KS-BIND.

(defvar *alt-transaction-open-p* nil)
(defvar *saved-alternatives* nil)

(defun atm-begin-alt-transaction ()
  (assert (not *alt-transaction-open-p*))
  (setq *saved-alternatives* (copy-list *alternatives*)
	*alt-transaction-open-p* t)
  t)

(defun atm-commit-alt-transaction ()
  (assert *alt-transaction-open-p*)
  (setq *saved-alternatives* nil
	*alt-transaction-open-p* nil)
  t)

(defun atm-abort-alt-transaction ()
  (assert *alt-transaction-open-p*)
  (setq *alternatives* *saved-alternatives*
	*saved-alternatives* nil
	*alt-transaction-open-p* nil)
  t)


;;;; Alternative management.

(definit :am *alt-id-counter* 0)

(defun post-alternative (alt-agenda)
  (assign-alt-agenda-defaults alt-agenda)
  (let ((alt (create-alt-record alt-agenda)))
    (push alt *alternatives*)
    (alt-id alt)))

(defun create-alt-record (alt-agenda)
  ;; N.B. Pushes a context.
  (db-request :fill-in-alternative
     (make-alternative
        :id (new-alt-id)
	:alt-agenda alt-agenda
	:processing (ag-id *ae-being-processed*))))

(defun new-alt-id ()
  ;; /\/ Used to use (pop-gensym "ALT-").
  (incf *alt-id-counter*))


;;;; Adding constraints added to the task

(defun atm-add-to-alternatives (additions)
  (assert (not *alt-transaction-open-p*))
  (dolist (alt *alternatives*)
    (unless (eq :no-more-alternatives (ag-type (alt-alt-agenda alt)))
      (setf (alt-additions alt)
	    (append (alt-additions alt) additions)))))

(defun process-alt-additions (alt)
  ;; Post an :add-to-task.
  (funcall (ipc-get-handler :am :event)
    (make-agenda-entry
      :trigger t
      :body `(:add-to-task :all-of ,@(alt-additions alt)))))


;;;; Move to a (different) alternative

;;; This happens after a poison and at certain other times.
;;; See choose-alt for how the "reason" is handled.

(defun pick-an-alternative (reason)
  ;; The reason is typically (:POISON-STATE keyword object...).
  (incf *n-alts-chosen*)
  (when (eq (car reason) :poison-state)
    (incf *n-poisons*))
  (let* ((alts (atm-get-alts-agenda))
	 (alt (choose-alt reason alts)))
    ;; We've now picked an alternative.
    (setq *slice-remaining* *agenda-slice*) 	;fresh start
    ;; Go back to the context that had the alternative.
    (db-request :SET-CONTEXT (alt-context alt))
    ;; Check consistency
    (when (db-request :CONTEXT-WAS-POISONED)
      (cerror "Continue anyway"
	      "Alt in poisoned context.~%Poisoned because ~A~%Alt: ~S"
	      (db-request :CONTEXT-WAS-POISONED)
	      alt))
    ;; Immediately push to a new context to preserve the alternative context.
    (db-request :PUSH-CONTEXT)
    ;; We have to combine the deletion of the agenda entry that was
    ;; being processed with the addition of the alternative agenda
    ;; entry to avoid triggering :planner-finished by mistake, which
    ;; would happen if the entry being deleted was the last entry in
    ;; the context.  Note that we can't simply add before deleting,
    ;; because the two entries might have the same id and the wrong
    ;; one (or both!) might be deleted.
    (if (alt-alt-agenda alt)
	(db-request :EXCHANGE-AGENDA-ENTRY
		    (alt-processing alt)
		    (alt-alt-agenda alt))
      ;; This is an alt created by switch-alternatives, so both the
      ;; alt-agenda and processing slots are nil.
      (assert (null (alt-processing alt))))
    ;; Handle anything added to the task since this alt was created.
    (when (alt-additions alt)
      (process-alt-additions alt))
    (atm-delete-alternative (alt-id alt))
    (request-current-agenda)))


;;;; Find the best alternative

;;; /\/: It may work to take all :BIND and :OR alts and take the best
;;; from them.  Most recent is suspect in :OR since it may not be the
;;; alt that contains other branches from the tree but instead the
;;; alt that reachieves "any old how".  Indeed, it may be a problem
;;; in general that that alt is more recent?

;;; /\/: A problem with going depth-first for, say, :COULD-NOT-BIND,
;;; thinking it will only invlove BINDs, is that the poison may be from
;;; an OR.

(defun choose-alt (reason alts)
  (check-alts-most-recent-first alts)
  (ecase (car reason)
    ((:agenda-slice-exhausted :cycle-count-limit-exceeded)
     ;; Take best-rated alt.  Forget about depth-firstness.
     (find-best-rated-alt alts))
    ((:poison-state)
     (case (cadr reason)
       ((:could-not-find-binding
	 :could-not-bind
	 :could-not-restrict
	 :could-not-add-link
	 :empty-or-tree
	 )
	;; Consider most recent alt
	(consider-most-recent-alts reason alts))
       (t
	(let ((most-recent (car alts))
	      (poisoner (ae-that-poisoned)))
	  (cond ((eql (alt-processing most-recent)
		      poisoner)
		 (dev-warn "Taking ~S alt from poisoner ~S"
			   (ag-type (alt-alt-agenda most-recent))
			   poisoner)
		 most-recent)
		(t
		 ;; Take best-rated alt
		 (find-best-rated-alt alts)))))))))

(defun ae-that-poisoned ()		;probably
  (let* ((poison-info (db-request :context-was-poisoned))
	 (agenda-id (car poison-info)))
    ;; /\/: poison-info, and hence agenda-id, will be nil for a replan.
    agenda-id))

(defun consider-most-recent-alts (reason alts)
  (declare (ignore reason))
  (let ((most-recent-first alts)
	(best-rated (find-best-rated-alt alts)))
    (dolist (a most-recent-first
	       best-rated)		;default
      (let ((alt-ae (alt-alt-agenda a)))
	(when (null alt-ae)
	  (return best-rated))
	(case (ag-type alt-ae)
	  ((:bind)
	   (dev-warn "Taking most recent ~S" (ag-type alt-ae))
	   (return a))
	  ((:or)
	   (if (or-is-reachieve-p alt-ae)
	       (dev-warn "Skipping reachieve")
	     (progn
	       (dev-warn "Taking most recent ~S" (ag-type alt-ae))
	       (return a))))
	  (t
	   (return best-rated)))))))

(defun or-is-reachieve-p (ae)
  ;; See set-up-alternative-with-opposite in KS-OR
  (assert (eq :or (car (ag-body ae))))
  (let* ((or-tree (or (cddddr (ag-body ae))
		      (return-from or-is-reachieve-p nil)))
	 (branch (car (or-tree-branches or-tree)))
	 (actions (or-branch-actions branch))
	 (action-1 (car actions)))
    (eq (car action-1) :reachieve)))

#+:fix-destructuring-bind
(defun or-is-reachieve-p (ae)
  ;; See set-up-alternative-with-opposite in KS-OR
  (assert (eq :or (car (ag-body ae))))
  (destructuring-bind (cond contribs satisfy-p &rest or-tree)
                      (cdr (ag-body ae))
    (declare (ignore cond contribs satisfy-p))
    (let* ((branch (car (or-tree-branches or-tree)))
	   (actions (or-branch-actions branch))
	   (action-1 (car actions)))
      (eq (car action-1) :reachieve))))	

(defun find-best-rated-alt (alts)
  (find-best alts #'(lambda (a b) (&< (alt-rating a) (alt-rating b)))))

(defun check-alts-most-recent-first (alts)
  (for-adjacent-elements
    #'(lambda (a b)
	(assert (> (alt-id-number a) (alt-id-number b))))
    alts))

(defun alt-id-number (alt)
  ;; /\/: Now alt ids are numbers, not "ALT-" symbols
  (alt-id alt)
  #+:undef
  (or (get (alt-id alt) 'alt-id-number)
      (setf (get (alt-id alt) 'alt-id-number)
	    (read-from-string
	      (symbol-name (alt-id alt)) nil nil :start 4))))


;;;; Switching alternatives w/o a poison

;;; /\/: When the DM "fills in" an alt, it records the current context in
;;; the alt and pushes a context.  This preserves the alt's context.  When
;;; pick-an-alternative picks an alt, it also pushes a context.  Here we
;;; add an alt then immediately pick one (typically a different one).
;;; So the new context created when we add the alt is never used.

(defun maybe-switch-alternatives ()
  (if (atm-exists-better-alternative-p)
      (switch-alternatives)
    (progn
      ;; Since we'd just return to the current state anyway, we'll
      ;; just give it a fresh slice w/o going via alternatives.
      (dev-debug :user-request "No better alternative")
      (dev-warn "No better alternative")
      (setq *slice-remaining* *agenda-slice*))))

(defun switch-alternatives ()
  (dev-debug :user-request "Switching alternatives")
  (dev-warn "Switching alternatives")
  ;; Create an alt so we can return to this part of the search space.
  (let ((alt (make-alternative
	       :id (new-alt-id)
	       :alt-agenda nil
	       :processing nil)))
    (push (db-request :fill-in-alternative alt)
	  *alternatives*)
    (pick-an-alternative '(:agenda-slice-exhausted))))

;;; atm-exists-better-alternative-p rates the current state and
;;; compares it to the best available alternative.

(defparameter *switch-penalty* 0)

(defun atm-exists-better-alternative-p ()
  (and *alternatives*
       (&< (&+ (alt-rating (car *alternatives*))
	       *switch-penalty*)
	   (db-call 'atm-state-rating-fn))))


;;;; Alt agenda display

(defun do-display-alt-agenda (stream)
  (format stream "~%The Alternatives Agenda Table:~%")
  (let ((*print-case* :downcase)
	(*print-pretty* t)
	(*print-level* 4)
	(*print-length* 4))
    (dolist (alt (atm-get-alts-agenda))
      (let ((ae (alt-alt-agenda alt)))
	(xp-format stream
         "ALT-~D: ~:IContext: ~A Processing AE-~D ~:_Agenda: AE-~D/~D: ~:_~A~%"
	    (alt-id alt)
	    (alt-context alt)
	    (alt-processing alt)
	    (ag-id ae)
	    (ag-stage ae)
	    (ag-body ae))))))


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
