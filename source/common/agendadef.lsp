;;;; File: agendadef.lsp
;;; Contains: Agenda entry definition.
;;; Author: Richard Kirby (rbk) and jeff Dalton
;;; Created: Sun Apr 15 21:01:23 1990
;;; Updated: Tue Jan 28 21:34:48 1997 by Jeff Dalton
;;; Copyright: (c) 1992, 1193, 1994, AIAI, University of Edinburgh

;;; /\/: Need to standardize on ag or ae for agenda-entries.  If it's ae,
;;; then the :conc-name should change as well.

(in-package :oplan)

;;; Agenda entries

(defstruct (agenda-entry (:conc-name ag-))
  "The agenda record."
  id		; A symbol of the form AE-n
  body
  branch-1
  branch-n
  (stage 0)
  level
  cost
  priority
  trigger
  info)

(defun-inline ag-type (ag) (car (ag-body ag)))

(defun-inline ag-args (ag) (cdr (ag-body ag)))

;;; Events go on the agent agenda

(defstruct (event (:include agenda-entry))
  )


;;; Alternatives

(defstruct (alternative (:conc-name alt-))
  "The alternative agenda record."
  id		; The alternative agenda record id.
  alt-agenda 	; The agenda rec that contains alternatives.
  context 	; The context number to revert to.
  rating 	; Value used to rate alternatives.
  processing 	; The agenda rec that posted the alternative.
  additions     ; Constraints added to the task
)

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
