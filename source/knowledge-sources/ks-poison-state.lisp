;;;; File: KS-POISON-STATE.lsp
;;; Contains: KS for poisoning the plan state.
;;; Author: Jeff Dalton and Richard Kirby (rbk)
;;; Created: Fri Jun 22 10:57:36 1990
;;; Updated: Wed Jan 22 03:00:37 1997 by Jeff Dalton
;;; Copyright: (c) 1992, 1194, AIAI, University of Edinburgh

(in-package :oplan-knowledge-source)

;;; The user can ask for KS-USER to be scheduled "in the alternative",
;;; so that it is invoked after the alternative has been chosen and
;;; "switched to".  This is done by simply posting :USER :USER-REQUEST
;;; to the agent agenda.  The call to pick-alternative sends a message
;;; to the AM telling it to switch to an alternative.  Earlier messages
;;; (such as the one about :USER) will be processed first.  Processing
;;; the :USER message does nothing more than add an entry to the agent
;;; agenda, and the agent agenda is not affected by switching to an
;;; alternative.  Therefore, the :USER entry will be on the agent agenda
;;; after we switch to the chosen alternative.

;;; Note that when we schedule KS-USER it goes on the _agent_ agenda.
;;; That's why it survives the change to a different alternative that
;;; happens when pick-an-alternative is called.

(defvar *ks-user-in-alternative-p*)	;to remember whether we've done it

(defun KS-POISON-STATE (ag)
  (handle-poisoning (ag-body ag)))

;;; Handle-poisoning is called by ks-poison-state and ks-replan.

(defun handle-poisoning (body)
  (let ((why (cdr body)))
    (dev-debug :minimal "Poisoning plan state because ~A~%" why)

    (unless (or (db-request :CONTEXT-WAS-POISONED)
		(eq (car why) :REPLANNING))
      (cerror "Poison context and continue"
	      "KS-POISON invoked in a context that wasn't poisoned.")
      (db-request :POISON-CONTEXT `(:BY-KS-POISON ,why)))

    (when (eq :ask *poison-handler-mode*)
      (let ((*ks-user-in-alternative-p* nil))
	(loop
	  (ecase (poison-handler-menu-choice why)
	    (:cont
	     (return))
	    (:user
	     (post-agent-agenda '(:USER :USER-REQUEST))
	     (setq *ks-user-in-alternative-p* t))
	    (:mode
	     (flip-mode '*poison-handler-mode*))
	    (:break
	     (break "Poison handler"))))))

    (whats-going-on
      "~%*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*~%~
        CHOOSING AN ALTERNATIVE")

    (when (eq (car why) :REPLANNING)
      (ipc-send :AM :REPLANNING))

    ;; Send the whole ag-body as the reason for picking an alt.
    ;; That way the reason begins with :poison-state which may
    ;; distinguish it from other reasons.
    (pick-alternative body)))

(defun pick-alternative (reason)
  (ipc-ask :AM :PICK-ALT reason))

(defun poison-handler-menu-choice (reason)
  ;; /\/ Do we need xp-format and *print-pretty*=t here?
  ;; /\/ Can we, given that it might put a newline in the header.
  (let ((*print-pretty* nil)
	(*print-length* 4)
	(*print-level* 2))
    (menu-request
      `("-heading" 
	,(format nil "Poison because ~(~{~A~^ ~}~)" reason)
	"Handle and continue=:cont"
	,@(unless *ks-user-in-alternative-p*
	    '("Request KS-USER in alternative=:user"))
	,(format nil "Set poison handler mode to: ~(~A~)=:mode"
	             (opposite-mode *poison-handler-mode*))
	"-line"
	"Break in=:break"))))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;  JWD   02 Apr 94	Added handling of *poison-handler-mode*
;;;
