;;;; File: semi-simulated-time.lisp
;;; Contains: Alternative world event handler
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: May 1995
;;; Updated: Tue Mar 23 02:06:01 1999 by Jeff Dalton
;;; Copyright: (c) 1995, AIAI, University of Edinburgh


;;; This file is not normally used and is not part of the defsystem
;;; for the world system.  So it's not automatically compiled either.


(in-package :oplan-world)

;;;; New event handler

;;; When possible, this version pretends that events happen at their
;;; due-times by setting the sim-clock to the due-time of the event
;;; being dispatched.  However, we never let the clock go backwards
;;; from a time t once something has happened at t.  ("Something
;;; happens" when an event is dispatched or a message handled.)

(defun sst-world-event-handler (self)
  (let ((*print-case* :downcase))	;/\/: for our output

    ;; Handle user input.  Who knows how long that takes?
    (handle-all-user-input)

    ;; Get sim-t = the present simulated time but then set the clock back.
    ;; That's the only time the clock goes backwards.
    (let ((old-t (current-simulated-time))
	  (sim-t (get-simulated-time)))
      (set-simulated-time old-t)

      ;; Dispatch events due before sim-t.  Those events will happen
      ;; at their due-times unless we'd need to move the clock backwards.
      (when (the-clock-is-running)
	(dispatch-ready-events (1- sim-t)))

      ;; Set the clock to sim-t and handle all messages.
      ;; The messages therefore happen at sim-t.
      (advance-sim-clock sim-t t)
      (handle-all-messages self)

      ;; Dispatch events due at or before sim-t, then update the clock, etc.
      ;; Note that the messages we just handled may have scheduled some
      ;; events that were due before sim-t.  But they have to happen at
      ;; sim-t, because we can't move the clock backwards.
      (when (the-clock-is-running)
	(dispatch-ready-events sim-t)
	(update-world-clock)
	(display-world-agenda *agenda-window*)
	(set-alarm-clock self)))))

(defun advance-sim-clock (new-time &optional must-succeed-p)
  (let ((old-time (current-simulated-time)))
    (if (>= new-time old-time)
	(set-simulated-time new-time)
      (if must-succeed-p
	  (error "Can't make clocks go backwards.")
	old-time))))


(defun install-sst-controller ()

  ;; New world-event-handler
  (advice+ 'world-event-handler 'sst
    #'(lambda (previous)
	(declare (ignore previous))
	#'sst-world-event-handler))

  ;; Modify dispatch-world-event
  (advice+ 'dispatch-world-event 'sst
    #'(lambda (previous)
	#'(lambda (e)
	    (advance-sim-clock (world-event-due-time e))
	    (funcall previous e))))

  )

; (install-sst-controller)


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
