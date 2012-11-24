;;;; File: triggers.lsp
;;; Contains: Code for handling agenda entry triggering.
;;; Author: Richard Kirby (rbk) and Jeff Dalton
;;; Created: Thu Jul 11 13:57:25 1991
;;; Updated: Sun Mar 14 06:18:01 1999 by Jeff Dalton
;;; Copyright: (c) 1992, AIAI, University of Edinburgh

(in-package :oplan)

#| This is kept very simple at the moment. Each time the DM handles a query, it
calls check-for-triggered-entries which then looks through all the untriggered
agenda entries and checks to see if their triggers are triggered. This will be
improved to be reactive, eg when bind a variable then check the bind triggers
to see if any agenda entries are now triggered - not check any and every
trigger!
|#

;;; Well, that _was_ the plan.  Now we check triggers only when the
;;; AM requests a copy of the plan aggenda.  This is sometimes called
;;; "lazy triggering".  It reduces the need for clever ways to
;;; determine which triggers might need checking.

;;; One subtlety is that, all else being equal, the AM prefers older
;;; agenda entries.  Since AM searches for the best entry each time,
;;; and then both the AM and the DM have to remove it, it would be
;;; nice if older entries were earlier in the triggered agenda.
;;; Since they're pushed onto the untriggered agenda (and hence
;;; are later in that list) and then pushed onto the triggered one,
;;; they will be earlier *if* we don't do another implicit reversal
;;; along the way.  Hence our avoidance of push when building the
;;; triggered-entries list.  /\/

(defparameter *trim-agenda-or-trees* nil)

(defun check-for-triggered-entries ()
  "Checks for triggered entries. If there are any, then add them to the
   triggered agenda table, and then notify the AM that its copy is out of
   date."
  (let ((table (atm-get-untriggered-agenda))
	(triggered-entries nil)
	(+ae+))
    (declare (special +ae+))		; /\/
    ;; Find newly triggered entries.
    (setq triggered-entries
	  (loop for entry in table
		when (progn
		       (setq +ae+ entry)
		       (is-triggered-p (ag-trigger entry)))
		collect entry))
    ;; Put the newly triggered entries on the triggered or waiting agenda.
    (dolist (entry triggered-entries)
      (atm-remove-untriggered-agenda-entry entry)
      (if (atm-have-authority-to-process entry)
	  (atm-add-triggered-agenda-entry entry)
	(atm-add-waiting-agenda-entry entry)))
    ;; Trim or-trees in the new triggered agenda
    (when *trim-agenda-or-trees*
      (trim-agenda-or-trees))))

(defun is-bound-p (psv-actor)
  (psv-has-value-p psv-actor))

(defun is-triggered-p (trigger)
  (declare (special +ae+))
  (cond ((eq trigger t)
	 t)
	((eq (car trigger) :BIND)
	 (is-bound-p (cdr trigger)))
	((eq (car trigger) :AND)
	 (every #'is-triggered-p (cdr trigger)))
	((eq (car trigger) :OR)
	 (some #'is-triggered-p (cdr trigger)))
	((eq (car trigger) :EMPTY)
	 ;; Check to see if the agenda is empty and only one thing on
	 ;; the untriggered agenda table (this entry), in which case we
	 ;; assume the planner has completed.
	 (atm-empty-tables-p))
	((eq (car trigger) :WAIT-ON-EFFECT)
	 ;; Checks that there are no EXPAND and ACHIEVE triggered
	 ;; agenda entries that could possibly cause the effect
	 ;; specified to be added.
	 (not (check-for-effect (cdr trigger) nil (ag-id +ae+))))
	((eq (car trigger) :ANSWER)
	 (have-question-answer-p (cadr trigger)))
	(t
	 ;; Trigger might be a fact, so lets check it.
	 ;; Look for at least one gost entry (irrespective of condition type)
	 ;; which has a value other than :undef.
	 (let ((gen (generator (list* 'gost actorsym trigger) actorsym)))
	   (do ((entry (try-next gen) (try-next gen)))
	       ((null entry) nil)
	     (unless (eq (value entry) :undef)
	       (return t)))))))

(defun check-for-effect (effect bindings &optional (ae-being-checked -1))
  ;; Checks to see if the agenda has any EXPAND or ACHIEVE agenda entries
  ;; that could possibly cause the effect required to be added to the plan
  ;; state.
  ;; NOTE: Should we also check the untriggered agenda table? But if so, we
  ;; might need to exclude checking a particular agenda entry (namely if this
  ;; is called from is-triggered-p then the agenda entry with this trigger).
  (declare (ignore bindings))
  (check-type ae-being-checked fixnum)
  (dolist (ae (atm-get-triggered-agenda))
    (unless (fix= (ag-id ae) ae-being-checked)
      (when (ae-might-have-effect-p ae effect)
	(return-from check-for-effect t))))
  ;; /\/: used to have: (unless (null (atm-get-triggered-agenda)) ...)
  (dolist (ae (atm-get-untriggered-agenda))
    (unless (fix= (ag-id ae) ae-being-checked)
      (when (ae-might-have-effect-p ae effect)
	(return-from check-for-effect t)))))

(defun ae-might-have-effect-p (ae effect)
  (case (car (ag-body ae))
    (:EXPAND
     (member (car effect)
	     (get-possible-effects-from-expanding (caddr (ag-body ae)))))
    (:ACHIEVE
     (member (car effect)
	     (get-possible-effects-from-achieving
	      (con-pattern (cadr (ag-body ae))))))
    (:OR
     (let ((first-action
	    (car (or-branch-actions
		  (car (or-tree-branches
			(or-body-or-tree (ag-body ae))))))))
       (when (eq (car first-action) :REACHIEVE)
	 (let ((cond (cdadr first-action)))
	   (and (eql (con-type cond) 'achievable)
		(member
		 (car effect)
		 (get-possible-effects-from-achieving
		  (con-pattern cond))))))))))

(defun or-body-or-tree (or-body)
  (let ((or-tree (cddddr or-body)))
    (check-type or-tree or-tree)
    or-tree))

(defun trim-agenda-or-trees ()
  (let ((agenda (copy-list (atm-get-triggered-agenda))))
    (do ((tail agenda (cdr tail)))
	((null tail))
      (when (eq (ag-type (car tail)) :OR)
	(setf (car tail)
	      (trim-agenda-or (car tail)))))
    (atm-set-triggered-agenda agenda)))

(defun trim-agenda-or (ae)
  (let ((or-tree (cdddr (ag-args ae))))
    (when (null or-tree)
      (return-from trim-agenda-or ae))
    (check-type or-tree or-tree)
    (let ((trimmed-tree (trim-or-tree or-tree)))
      (if (and trimmed-tree
	       (eql (or-tree-branch-n or-tree)
		    (or-tree-branch-n trimmed-tree)))
	  ;; No change.
	  ae
	;; Make a new agenda entry.
	;; Note that if the trimmed-tree is null, we set the agenda
	;; entry's branch-1 to 1 so that it will be processed asap.
	(let* ((new-ae (copy-agenda-entry ae))
	       (args (ag-args ae))
	       (cond (first args))
	       (contribs (second args))
	       (satisfy-p (third args)))
	  (dev-warn "~&Trim from ~S to ~S for ~S~%"
		    (or-tree-branch-n or-tree)
		    (if trimmed-tree (or-tree-branch-n trimmed-tree) 0)
		    cond)
	  (setf (ag-branch-1 new-ae)
		(if trimmed-tree (or-tree-branch-1 trimmed-tree) 1))
	  (setf (ag-body new-ae)
		`(:OR ,cond ,contribs ,satisfy-p . ,trimmed-tree))
	  new-ae)))))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
