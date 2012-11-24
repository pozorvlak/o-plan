;;;; File: world-services.lsp
;;; Contains: Core routines for the world simulation
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: Tue Aug 03 1993
;;; Updated: Sun Aug 22 19:51:22 1999 by Jeff Dalton
;;; Copyright: (c) 1993, 1994, AIAI, University of Edinburgh

;;; Contents:
;;;  * Introductory remarks.
;;;  * The World agenda.
;;;  * Basic world-event types and operations.
;;;  * Time control --> see support/sim-clock.lsp.
;;;  * Initialization.
;;;  * The main event-handler.
;;;  * Message definitions.
;;;  * Message sending.
;;;  * User commands.
;;;  * World history operations.
;;;  * World state operations.
;;;  * Clock display.

(in-package :oplan-world)

;;;; Introduction

;;; The World runs a discrete event simulation and handles messages from
;;; the Exec.  These messages can arrange for a world-event to happen at
;;; a particular point in simulated time, ask about the current state of
;;; the world, and so on.  The World does not necessarily send anything
;;; to the Exec; that depends on what world-event types are defined.

;;; In principle, the World is a separate agent to the right of the
;;; Exec, but in practice can be implemented as if it were an Exec
;;; component.  This is similar to the relationship between the
;;; Task Assigner and the Planner.  At present, both a built-in
;;; World and a standalone World exist.  This file is shared.

;;; The World and Exec communicate via :rightin and :rightout, where
;;; these names take their meanings relative to the Exec.  That is,
;;; the Exec receives messages from the World via :rightin and sends
;;; to the World via :rightout.  However, this happens behind the
;;; scenes.  Messages from the Exec are automatically converted to
;;; pprocess messages and placed in the World pprocess's event-queue.
;;; Messages to the Exec are sent by calling send-to-exec rather
;;; than sending explicitly to :rightin.

;;; Simulated time is scaled real time: a fixed number of seconds of
;;; simulated time pass for each second of real time.  The Exec also
;;; manages simulated time, and since the Exec tells the World what
;;; real-time origin and scale factor to use, the two should be able
;;; to keep in step.

;;; N.B. If you use a scale-factor that is large for the frequency
;;; of events (e.g. 1 second = 1 day with things happening every
;;; few minutes), the World will have trouble keeping up.  Moreover,
;;; when we start dispatching events from the world agenda, we look
;;; at the current simulated time once only, at the start.  That is,
;;; we assume the time required to dispatch events is insignificant
;;; in the simulated world's terms.  If this isn't the case, the
;;; simulator wouldn't be able to keep up anyway.

;;; For more information on time management, see support/sim-clock.lsp.

;;; As an example, consider a simple case of plan execution.
;;; The principal message the Exec would send the World is:

;;;   :BEGIN-ACTION action-pattern eft lft

;;; The action is assumed to start at the simulated time when the message
;;; is received, and the World enqueues an event to mark the action's
;;; finish.  This finish event can be killed by the user or fail in other
;;; ways.  So, in effect, the World never initiates actions; it just tracks
;;; when (and whether) they are completed successfully.  However, the
;;; World could do more than this if appropriate messages were defined.

;;; The basic protocol for running a simulation is then for the Exec to:
;;;   1. Send an :INIT message to the World.
;;;   2. Maybe send some :BEGIN-ACTION messages so the action-finish
;;;      events will already be enqueued when the clock starts.  These
;;;      actions start at simulated time 0.
;;;   3. Send a :START-CLOCK message.
;;;   4. Maybe send some more :BEGIN-ACTION messages while the
;;;      simulation is running.  These actions start right away.
;;;   5. Send a :STOP-CLOCK or :INIT message to stop the simulation.

;;; N.B. The World deals in ordinary messages as supported by defmessage
;;; and ipc-handle-message, not mcc-messages.

;;; The World keeps its own track of simulated time, so that it can
;;; be used independently of the Exec.


;;;; The World agenda and world-events

;;; The world agenda is a queue of world-events ordered by increasing
;;; due-time.  An Event is "dispatched" (ie, happens) as soon as the
;;; World notices that its due-time has arrived.

;;; Any effects, of any sort, that the event is supposed to have are
;;; controlled by the succeed- and fail-funs (functions) of the event;
;;; so effects are not built into the World and can be different for
;;; different events or different classes of event.

;;; It's important not to be misled by the words "succeed" and "fail".
;;; Success simply means that the event happened when it was supposed
;;; to happen (ie, at its due-time) and should have whatever effects
;;; it was supposed to have.  Failure means that it *doesn't happen*.

;;; An event is dispatched by removing it from the World agenda and
;;; calling its succeed-fun.  If something wants to intervene so that
;;; the event does not occur, it should remove the event from the agenda
;;; and call its fail-fun, giving a reason for the failure.  One cause
;;; of failure is the user command to "kill" an event.  In this case,
;;; the reason will be :killed-by-user.

;;; Note that event-success does not imply success in any larger sense.
;;; For instance, the event could be the failure of some piece of machinery
;;; due to excessive wear.  Success in our sense (event-success) means
;;; that this failure occurs, whereas failure in our sense (event-failure)
;;; means that it doesn't.

;;; Or consider an event that happens too late so far as the Exec is
;;; concerned.  The Exec may count this a failure.  But the way this is
;;; typically handled in the World is to give the event this late time
;;; as its due-time.  If the event occurs at that due-time, rather than
;;; not happening at all, the World counts it a success.  On the other
;;; hand, making an event happen before its due-time might be implemented
;;; as a kind of event-failure by removing the event from the agenda and
;;; calling its fail-fun, given a cooperative fail-fun.  This isn't quite
;;; the sense of event-failure we have in mind, because the event still
;;; "happens", but it might make sense in some applications.

;;; Note that if you want to change the due-time of an event, you should
;;; unschedule it, change the due-time, then schedule it again.  Merely
;;; changing the due-time while the event remained in the agenda would be
;;; wrong, because the agenda might no longer be in order of increasing
;;; due-times.


;;;; Basic world-event types and operations.

;;; /\/: Replace *agenda-matches-display-p* by a slot in a subclass of queue?

(defvar *world-agenda* nil)

(definit :world *event-count* 0)
(definit :world *agenda-matches-display-p* nil)

(defstruct world-event

  ;; Slots for all worlds

  (number (incf *event-count*))	;sometimes used to identify the event
  (due-time    'not-a-due-time)	;simulated time when event should occur
  (description "<some event>")	;description of what will happen
  (succeed-fun 'not-a-succfun)	;function called on event when it succeeds
  (fail-fun    'not-a-failfun)	;function called on event+reason when it fails
  (hidden-p    nil)		;true to keep it out of the agenda display

  ;; Slots for Pacifica-style worlds (/\/ should be a separate class)

  (start-time  'not-a-startime)	;simulated time the event begins
  (action      'not-an-action)	;name of the event
  (args        'not-args)	;variables used in event processing
  (reason-for-failure		;explanation of why an event has failed
   'not-a-failure-reason)

  )

(defun get-scheduled-events ()		;for user code
  (queue-contents *world-agenda*))

(defun schedule-world-event (event)
  (setq *agenda-matches-display-p* nil)
  (enqueue-increasing event *world-agenda* :key #'world-event-due-time))

(defun unschedule-world-event (event)
  (setq *agenda-matches-display-p* nil)
  (delqueue-if #'(lambda (e) (eq e event)) *world-agenda*))

(defun succeed-world-event (event)
  (funcall (world-event-succeed-fun event) event))

(defun fail-world-event (event &key reason)
  (funcall (world-event-fail-fun event) event reason))


;;;; Initialization

;;; World-init is called when the Exec sends us an :init message.
;;; This should happen before any other messages are sent.  If it
;;; doesn't, some things may not work.

(defun world-init ()
  (init-clock)
  (clear-world-clock)
  (clear-screen *history-window*)
  (clear-screen *agenda-window*)
  (clear-screen *interact*)
  (setq *world-agenda* (make-queue))	;lets us check that an init was done
  (initialize-variable-group :world)
  (clear-world-state)
  )


;;;; The main event-handler

;;; World-event-handler is called whenever the world has something
;;; to do.

;;; User input is handled first, because the user was reacting to the
;;; old state of the world, not the new one we're about to create, and
;;; because the user could have typed the input some time ago.  Users
;;; would be annoyed if they killed an event only to see it go sailing
;;; away unharmed because the world dispatches events before looking
;;; at user input.

;;; There's also a reason for handling messages before events are
;;; dispatched.  It allows messages to set up world events that should
;;; already have happened and have them happen now rather than the
;;; next time the World pprocess runs.  In addition, it makes sense
;;; to keep event information around while we're processing messages
;;; rather than send it away before we start.

;;; All messages are handled, rather than just one, so that we can
;;; keep the world as up-to-date as possible.  (Standard pprocesses
;;; typically handle only one message, then return to give other
;;; pprocesses a chance to run.)

;;; It may seem that, in principle, messages to the world should be
;;; stamped with their true arrival times so that they can be processed
;;; in the right order relative to events on the world agenda; but
;;; instead we can just imagine that it takes messages so long to
;;; get here that they don't arrive until we look at them.

;;; This sort of reasoning is less plausible for user input, because
;;; the user's controlling events in the world; communication delays
;;; don't really come into it.

(defun world-event-handler (self)
  (let ((*print-case* :downcase))	;/\/: for our output
    (handle-all-user-input)
    (get-simulated-time)		;who knows how long user input takes
    (handle-all-messages self)
    (when (the-clock-is-running)
      (dispatch-ready-events (current-simulated-time))
      (update-world-clock)
      (display-world-agenda *agenda-window*)
      ;; /\/: Will the current-simulated-time still be close enough?
      ;; Does it matter?  Well, we may sleep for 1 sec when we should
      ;; try to run again right away.
      (set-alarm-clock self))))

;;; User input.

(defun handle-all-user-input ()
  (let ((*readtable* *world-command-readtable*)
	(*package* (find-package :oplan-world)))
    (while (listen *interact*)
      (handle-world-user-command))))	;<-- in a separate section, below

;;; Messages from the Exec et al.

(defun handle-all-messages (self)
  (while (next-event-p self)
    (ipc-handle-message self (next-event self))))

;;; Make world events happen.

(proclaim
  '(notinline dispatch-ready-events	;so they can be advised
	      dispatch-world-event))

(defun dispatch-ready-events (sim-t)
  ;; Dispatch events due at or before sim-t.  Note that no time
  ;; conversions are required for agenda entries, because their
  ;; due-times are already simulated times.
  (let ((agenda *world-agenda*))
    (unless agenda
      (error "No World agenda -- was the World sent :INIT?"))
    (while (and (not (empty-queue-p agenda))
		(>= sim-t
		    (world-event-due-time
		      (first (queue-contents agenda)))))
      (dispatch-world-event (dequeue agenda)))))

(defun dispatch-world-event (e)
  (when (or *show-hidden-events*
	    (not (world-event-hidden-p e)))
    (setq *agenda-matches-display-p* nil))
  (succeed-world-event e))		;that's it!

;;; Tell the user how things stand.

(defun display-world-agenda (stream)
  (unless *agenda-matches-display-p*
    (clear-screen stream)
    (dolist (event (queue-contents *world-agenda*))
      (display-world-event event stream))
    (setq *agenda-matches-display-p* t)))

(defun display-world-event (event stream)
  (when (or *show-hidden-events*
	    (not (world-event-hidden-p event))
	    (not (eq stream *agenda-window*)))
    (format stream "#~3D ~A -> ~A~%"
	    (world-event-number event)
	    (seconds->minimal-time-string (world-event-due-time event))
	    (world-event-description event))))

;;; Remember the future.

;;; /\/: Since the p-process sleep granularity is 1 second, we
;;; have to sleep for at least 1 second if we want to sleep at all,
;;; but it's not clear that a min sleep of 1 second is desirable.
;;; So we opt for maybe not aleeping at all (i.e., no min sleep).

;;; As in the Diary, the basic plan is to call
;;;   (set-to-sleep-until
;;;     (simulated->universal-time next-due-time))
;;; But we may want to wake up sooner in order to keep the world clock
;;; moving.  Note that, unlike set-to-sleep-for, set-to-sleep-until
;;; does not have to get the current primitive real time (a system call).

(defun set-alarm-clock (self)
  (let ((clock-due-time
	 (+ (current-simulated-time)
	    (real->simulated-seconds 2))))
    (if (empty-queue-p *world-agenda*)
	(set-to-sleep-until
	  (simulated->universal-time clock-due-time)
	  self)
      (let ((next-due-time
	     (world-event-due-time
	      (first (queue-contents *world-agenda*)))))
	(set-to-sleep-until
	  (simulated->universal-time (min clock-due-time next-due-time))
	  self)))))
      

;;;; Message definitions

(defmessage (:world :init) ()
  (world-init))

(defmessage (:world :start-clock) (universal-time simulated-time scale-factor)
  (start-clock universal-time simulated-time scale-factor)
  (update-world-clock))

(defmessage (:world :stop-clock) ()
  (stop-clock))


;;;; Message sending

(defun send-to-exec (&rest agenda-body)
  (ipc-send :micro-exec :world-event agenda-body))



;;;; User commands

;;; A command must be typed on a single line.  It is read by read-line
;;; and then extracted from the resulting string.  In this way we can
;;; avoid having to wait for the user to finish typing.  We also try
;;; to avoid signalling an error, preferring merely to print a message
;;; in the World interaction window.  (See tell-world-user.)

;;; What we really want to do for "break" is to:  (/\/)
;;;  * Stop the simulation at time t (whatever the current time is).
;;;  * Tell the Exec to stop at time t.
;;;  * Send ourself a :break message, so the Exec can run
;;;    before we actually break.
;;;  * After the break, send the Exec a restart message.

(defparameter *world-user-command-help*
  '(("k n"    "kill event #n")
    ("d n t"  "delay event #n by <time_spec> t")
    ("s n t"  "(re)schedule event #n at <time_spec> t")
    ("h"      "print this help message")
    ("?"      "print this help message")
    ()
    ("world"  "show all p = v in the current state of the world")
    ("break"  "enter a Lisp break loop")))

(defun handle-world-user-command ()
  (let* ((command (read-line *interact*))
	 (command-stream (make-string-input-stream command)))
    (flet ((next ()
	     (read command-stream nil nil))
	   (time-spec ()
	     (or (parse-time-spec command-stream)
		 (progn (tell-world-user "Bad time-spec in ~S." command)
			(return-from handle-world-user-command)))))
      (let ((cmd (next)))
	(case cmd
	  (k (kill-event (next)))
	  (d (delay-event (next) (time-spec)))
	  (s (sched-event (next) (time-spec)))
	  (h (world-help *interact*))
	  (? (world-help *interact*))
	  (world
	   (display-world-state *interact*))
	  (break
	   (break "World"))
	  (otherwise
	   (format *interact* "~&Unknown world command: \"~S\".~%~
				 Type h or ? for help.~%"
		   cmd)))))))

(defun world-help (stream)
  (dolist (h *world-user-command-help*)
    (if h
	(apply #'format stream "~&~2T ~A ~10T ~A~%" h)
      (terpri stream)))
  (terpri stream))

(proclaim '(notinline extract-event))

(defun kill-event (n)			;kill event #n
  (let ((killed-event (extract-event n)))
    (when killed-event
      (tell-world-user "ok")
      (fail-world-event killed-event :reason :killed-by-user))))

(defun delay-event (n seconds)		;delay event #n by some seconds
  ;; Note that the delay can be < 0.
  (let ((event (extract-event n)))
    (when event
      (tell-world-user "ok")
      (incf (world-event-due-time event) seconds)
      (schedule-world-event event))))

(defun sched-event (n due-time)		;(re)schedule event #n
  (let ((event (extract-event n)))
    (when event
      (tell-world-user "ok")
      (setf (world-event-due-time event) due-time)
      (schedule-world-event event))))

;;; Extract-event is called by World user commands to remove an event
;;; from the agenda.  This allows it to be discarded, or rescheduled
;;; at a different time.  Merely changing the due-time while the event
;;; was still in the agenda would be wrong, because the agenda might
;;; no longer be in order of increasing due-time.

(defun extract-event (n) ; -> event or nil
  (unless (numberp n)
    (tell-world-user "\"~S\" is not a number." n)
    (return-from extract-event nil))
  (let ((event (delqueue-if #'(lambda (e) (= (world-event-number e) n))
			    *world-agenda*)))
    (cond (event event)
	  ((<= 1 n *event-count*)
	   (tell-world-user "Too late for event ~S." n)
	   nil)
	  (t
	   (tell-world-user "There was never an event numbered ~S." n)
	   nil))))

(defun tell-world-user (format-string &rest format-args)
  (format *interact* "~&~?~%" format-string format-args))


;;;; World history

;;; The history contains those things thought worth recording, rather
;;; than everything that happens.

(defun display-history (format-string &rest format-args)
  (format *history-window* "~11@A ~?~%"
	  (simulated-time-string (current-simulated-time))
	  format-string format-args))


;;;; World state

(defun display-world-state (stream)
  (let ((*package* (find-package :oplan))) ;get pkg prefixes right in output
    (format stream "~&World at ~A:~%"
      (simulated-time-string (current-simulated-time)))
    (loop for (pat . val) in (get-world-state)
	  do (format stream "~&   ~S = ~S~%" pat val))
    (terpri stream)
    (values)))


;;;; Clock display

;;; The clock displays the current simulated time in a one-line window
;;; that is by default at the upper right corner of the screen.  Alternative
;;; positions can be set in a config file or by the :world-clock-geometry
;;; parameter.  The config file takes precedence (/\/).

;;; N.B. we want the clock to be wide enough that the title can appear
;;; in full.  This is typically wider than we need just to display the
;;; time.

(defvar *default-world-clock-geometry* "18x1-20+20")

(defvar *world-clock-window* nil)

(defvar *displayed-world-clock-time* 0)

(defun ensure-world-clock-window (&optional (window-args nil))
  (unless *world-clock-window*
    (unless (x-get-stream :world-clock)
      (x-open-and-register-io-win :world-clock
        ;; N.B. later args seem to take precedence
        (append
          (list "-sl" "0")		;save no scrolled-off lines
	  (or window-args
	      (is-get-window-args :world-clock)
	      (list "-title" "World Clock"
		    "-name" "World Clock"
		    "-geometry" (or (get-parameter :world-clock-geometry)
				    *default-world-clock-geometry*)
		    "-fn" "fixed")))))
    (setq *world-clock-window* (x-get-stream :world-clock))))

(defun update-world-clock ()
  (let ((clock *world-clock-window*)
	(now (current-simulated-time)))
    (when (/= now *displayed-world-clock-time*)
      (setq *displayed-world-clock-time* now)
      (terpri clock)
      (write-string "   " clock)
      (write-string (seconds->time-string now) clock)
      (force-output clock))))

(defun clear-world-clock ()
  (clear-screen *world-clock-window*))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
