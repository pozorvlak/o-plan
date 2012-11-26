;;;; File: agendas.lsp
;;; Contains: The agent agenda and the Controller's side of the plan agenda
;;; Author: Jeff Dalton and Richard Kirby
;;; Created: April 1990
;;; Updated: Wed Jun  9 02:51:22 1999 by Jeff Dalton
;;; Copyright: (c) 1996, AIAI, University of Edinburgh


(in-package :oplan)


;;;; The agent agenda

;;; Kept as a sorted list, higher-priority entries first.

(defvar *agent-agenda* nil)		;owned by AM; not context-layered

(defmessage (:am :event) (agenda)
  (assign-event-defaults agenda)
  (add-to-agent-agenda agenda))

(defun-inline agent-agenda-p ()
  *agent-agenda*)

(defun agent-agenda-entries ()
  *agent-agenda*)

(defun add-to-agent-agenda (ae)
  (check-agenda-entry ae)
  (setq *agent-agenda*
	(merge 'list *agent-agenda* (list ae) #'> :key #'ag-priority)))

(defun-inline first-from-agent-agenda ()
  (car *agent-agenda*))

(defun-inline take-from-agent-agenda ()
  (pop *agent-agenda*))


;;;; The plan agenda

;;; Note than an existing agenda entry is sometimes reposted, for
;;; instance at a KS stage boundary.

(definit :am *agenda* nil)	;copy of plan agenda owned by DM

(defmessage (:am :agenda) (agenda)
  (if (ag-id agenda)			;may be reposting of an existing entry
      (assert (ag-priority agenda))	;yes -- try a minimal consistency check
    (assign-agenda-defaults agenda))	;no -- assign defaults
  (db-request :ADD-AGENDA-ENTRY agenda))

(defun-inline plan-agenda-p ()
  *agenda*)

(defun-inline first-from-plan-agenda ()
  (car *agenda*))

(defun-inline take-from-plan-agenda ()
  (setq *planning* t)
  (when *slice-remaining*
    (decf *slice-remaining*))
  (pop *agenda*))

(defun untriggered-agenda-p ()
  (db-request :untriggered-entries-exist-p))


;;;; Picking an agenda entry to process

(defun get-agenda-entry-to-process ()
  (cond ((not (plan-agenda-p))
	 ;; Nothing on the plan agenda
	 (if (agent-agenda-p)
	     ;; But something on the agent agenda
	     (take-from-agent-agenda)
	   ;; Neither plan nor agent agenda has anything for us to do.
	   ;; This isn't supposed to happen.
	   (error "Nothing to do.")))
	;; Something on the plan agenda ...
	((agent-agenda-p)
	 ;; ... and on the agent agenda -- see which has higher priority;
	 ;; prefer agent agenda if there's a tie.  Warn in plan agenda
	 ;; has higher priority.
	 (let ((plan-entry (first-from-plan-agenda))
	       (agent-entry (first-from-agent-agenda)))
	   (if (>= (ag-priority agent-entry) (ag-priority plan-entry))
	       (take-from-agent-agenda)
	     (progn
	       (warn "Plan agenda ~S has higher priority than agent ~S."
		     (ag-type plan-entry) (ag-type agent-entry))
	       (take-from-plan-agenda)))))
	(t
	 ;; ... but not on the agent agenda -- take plan agenda entry.
	 (take-from-plan-agenda))))


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
