;;;; File: micro-exec-world.lsp
;;; Contains: The generic world-model used by the Micro-Exec.
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: February 1995
;;; Updated: Sun Jun 13 19:54:45 1999 by Jeff Dalton
;;; Copyright: (c) 1995, AIAI, University of Edinburgh

(in-package :oplan-world)

;;; Contents:
;;;  * Introductory description.
;;;  * New event handler.
;;;  * Events.
;;;  * Messages.
;;;  * "Configuration".
;;;  * Guard function.
;;;  * Success
;;;     - Succeed function.
;;;     - Describe-plan-step.
;;;     - Real success.
;;;  * (real) Failure.
;;;  * (real) Partial failure.
;;;  * Unexpected (unplanned) events.
;;;     - Getting events from the user.
;;;     - Help message.
;;;  * Always facts
;;;  * Utilities.
;;;

;;;; The micro-exec world
;;;
;;; The micro-exec world is a WorldSim world that can be used when
;;; executing plans from any O-Plan domain.  It is normally used
;;; with the "micro-exec", a simple execution agent that is built
;;; into the O-Plan planner, but it could also be used in other ways.
;;;
;;; /\/: fill in.
;;;


;;;; New event handler

;;; When possible, this version pretends that events happen at their
;;; due-times by setting the sim-clock to the due-time of the event
;;; being dispatched.  However, we never let the clock go backwards
;;; from a time t once something has happened at t.  "Something
;;; happens" when an event is dispatched.  (/\/: Is that right?)

;;; This handler also calls (guard-fun e) before dispatching e.
;;; If the guard-fun returns true, the event is dispatched as usual.
;;; Otherwise, the event is put back in the agenda and tried again
;;; in the next cycle.

(defparameter *clock-mode* :step)

(defvar *delayed-events* '())

(defun micro-exec-world-event-handler (self)
  (let ((*print-case* :downcase)	;/\/: for our output
	(*delayed-events* '()))

    ;; Handle user input.
    (handle-all-user-input)

    ;; Handle all messages.  Note that the clock will *not* be up-to-date.
    ;; Indeed, if message handlers look at the clock, they may see a time
    ;; from before the message was sent.
    (handle-all-messages self)

    (when (the-clock-is-running)

      ;; Get sim-t = the present simulated time but then set the clock back.
      ;; That's the only time the clock goes backwards.
      (let ((old-t (current-simulated-time))
	    (sim-t (get-simulated-time)))
	(when (eq *clock-mode* :step)
	  (set-simulated-time old-t))

	;; Dispatch events due at or before sim-t.  The current simulated
	;; time will track the event due-times unless we'd need to move
	;; the clock backwards.
	(dispatch-ready-events sim-t)

	;; Set the clock to the "real" current simulated time: sim-t.
	(advance-sim-clock sim-t t)

	;; Put any events delayed by their guard-funs back in the agenda.
	;; Nreverse gets them put back in their original order.
	;; And we protect *agenda-matches-display-t* in case all
	;; events have been delayed.  Otherwise, taking them out
	;; and putting them back would count as a change.
	(when *delayed-events*
	  (setq *delayed-events* (nreverse *delayed-events*))
	  (let ((*agenda-matches-display-p* *agenda-matches-display-p*))
	    (mapc #'schedule-world-event *delayed-events*)))

	;; Change the clock display, etc.
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


(defun advice-replace (fn-name advice-name new-fn)
  (advice+ fn-name advice-name
    #'(lambda (previous)
	(declare (ignore previous))
	new-fn)))


(defun install-micro-exec-controller ()

  ;; Replace world-event-handler
  (advice-replace 'world-event-handler 'micro-exec
    #'micro-exec-world-event-handler)

  ;; Modify dispatch-world-event
  (advice+ 'dispatch-world-event 'micro-exec
    #'(lambda (previous)
	#'(lambda (e)
	    (advance-sim-clock (world-event-due-time e))
	    (if (plan-step-p e)
		(if (funcall (plan-step-guard-fun e) e)
		    (funcall previous e)
		  (push e *delayed-events*))
	      (funcall previous e)))))

  )

(install-micro-exec-controller)



;;;; Events

;;; A plan-step corresponds to a node-end in the Planner.  It represents
;;; the start or completion of an action.

(defstruct (plan-step
	    (:include world-event))
  (guard-fun nil)		;fn called to see if event must be delayed
  node				;e.g.: node-3
  end				;either :begin or :end
  type				;e.g.: action, dummy
  pattern			;a list
  earliest-time			;int >= 0
  latest-time			;int >= 0, or positive infinity
  effects			;list of (p v) pairs
  conditions)			;list of (p v) pairs


;;;; Messages

(defmessage (:world :always) (pv-pairs)
  (loop for (pattern value) in pv-pairs
	do (set-world-always-value pattern value)))

(defmessage (:world :execute)
            (node end type pattern earliest-time latest-time
		  effects conditions)
  (schedule-world-event
    (make-plan-step
      :due-time earliest-time
      :description
        (format nil "~12S ~5A: ~S." node end pattern)
      :node node
      :end end
      :type type
      :pattern pattern
      :earliest-time earliest-time
      :latest-time latest-time
      :effects effects
      :conditions conditions
      :succeed-fun 'plan-step-success
      :fail-fun 'plan-step-failure
      :guard-fun 'initial-plan-step-guard)))


;;;; "Configuration"

;;; Filters
;;;
;;; A plan step filter has the form (aspect pattern action),
;;; where
;;;   aspect  is :action or :effect
;;;   pattern is a pattern as described below
;;;   action  is :ask, :succeed, :fail, or nil.
;;;
;;; The action is optional and defaults to nil.
;;;
;;; In a pattern, $, ??, $name, and ?name match anything,
;;; and * matches any sequence of list elements.
;;;
;;; The initial list of filters is equivalent to having no filters.
;;; It is:
;;;   (:action ($end $pattern) nil)
;;;   (:effect ($p $v) nil)
;;;
;;; When a plan-step "succeeds" (ie, when its succeed-fun is called),
;;; it is matched against each filter in turn.  The first filter that
;;; matches determines the "filter-action" for that step.  If no filter
;;; matches, the filter-action is nil.  Filter-actions are interpreted
;;; as follows:
;;;   :ask        Ask the user what do to.
;;;   :succeed    Report success back to the Exec.
;;;   :fail       Report failure back to the Exec.
;;;   nil         Obey *plan-step-success-mode*: if mode = :ask, ask
;;;                the user what to do; if mode = :auto, report success.
;;;
;;; The way a filter is matched against a plan-step is determined by
;;; the aspect listed in the filter:
;;;   :action     Match the filter's pattern against a list containing
;;;                the node-end and action-pattern of the plan step.
;;;   :effect     Match the filter's pattern against each of the step's
;;;                effects, where an effect is a list (pattern value).
;;;                The filter matches if its pattern matches any of the
;;;                effects.
;;;

(import 'oplan::$)

(definit :world *plan-step-filters*
  '((:action (oplan::$end oplan::$pattern) nil)
    (:effect (oplan::$p oplan::$v) nil)))

(defun configure-micro-exec-world ()
  (if *plan-step-filters*
      (tell-world-user "Current-filters:~%~{~3T~S~%~}" *plan-step-filters*)
      (tell-world-user "No current filters"))
  (tell-world-user "Enter a new set [end with a blank line]:")
  (multiple-value-bind (good bad)
      (check-plan-step-filters (read-from-lines *interact*))
    (when bad
      (tell-world-user "Syntax errors in:~%~{~3T~S~%~}" bad)
      (tell-world-user "Accepted: ~%~{~3T~S~%~}" good))
    (setq *plan-step-filters* good)))

(defun plan-step-filter-action (step) ; -> one of :ask, :succeed, :fail, nil
  (dolist (filter *plan-step-filters*)
    (destructuring-bind (aspect pattern &optional action) filter
      (when (match-plan-step-aspect aspect step pattern)
	(ecase action
	  ((:ask :succeed :fail nil)
	   (return action)))))))

(defun match-plan-step-aspect (aspect step pattern)
  (ecase aspect
    (:action
     (match pattern
	    (list (plan-step-end step)
		  (plan-step-pattern step))))
    (:effect
     (loop for effect in (plan-step-effects step)
	   thereis (match pattern effect)))))

(defun check-plan-step-filters (filters)
  (let ((winners '())
	(losers '()))
    (dolist (filter filters)
      (if (and (or (match '(?aspect ?pattern ?action) filter)
		   (match '(?aspect ?pattern) filter))
	       (member (first filter) '(:action :effect))
	       (member (third filter) '(:ask :succeed :fail nil)))
	  (push filter winners)
	  (push filter losers)))
    (values
      (nreverse winners)
      (nreverse losers))))


;;;; Guard function

;;; The guard-fun of a plan-step checks whether the conditions of the
;;; step have been satisfied.  If they have, the step is dispatched
;;; so that it's succeed-fun is called.

;;; If some conditions have not been satisfied, a menu is put up to
;;; ask the user what to do.  If the user says to wait until the
;;; conditions are satisfied, the plan-step is put back in the agenda.
;;; On subsequent cycles, the guard-fun will be called again but
;;; will keep the step in the agenda until one or more of the unsatisfied
;;; conditions has been satisfied -- at which point it will again ask 
;;; the user what to do.

;;; If the user says not to wait (either initially or when asked later on),
;;; then the step will be dispatched and its succeed-fun called.  So if
;;; you want to cause the step to fail, you must first say not to wait.

;;; How to change your mind after deciding an event should wait:
;;;
;;; If the user decides to wait for some unsatisfied conditions,
;;; and nothing happens to satisfy any of the conditions, then
;;; the event will stay in the agenda without the user being asked
;;; again whether to wait.  If the user later wants to be asked
;;; whether to keep waiting, this can be accomplished by entering
;;; a command that delays or reschedules the event.  See the
;;; advice+ on extract-event, below.

;;; Waiting for conditions vs "delivery points":
;;;
;;; Note that when a node-end is delayed in the World because
;;; some of its conditions are not yet satisfied it will tend
;;; to be "released" as soon as the effects are satisfied,
;;; which (in the case of a supervised condition) will be
;;; before the "delivery point" is executed.  This is even
;;; though the delayed node-end is linked after the delivery
;;; point in the plan.

(defun initial-plan-step-guard (step)
  (let ((unsat (step-unsatisfied-conditions step)))
    (if (null unsat)
	t
      (plan-guard-user-decision step unsat unsat))))

(defun plan-step-guard (step conds-to-wait-for)
  (let ((unsat (step-unsatisfied-conditions step)))
    (cond ((null unsat)
	   ;; The conditions have all been satisfied.
	   t)
	  ((equal unsat conds-to-wait-for)
	   ;; None of the conditions have been satisfied since the
	   ;; last time we checked.
	   nil)
	  (t
	   ;; Some conditions have been satisfied
	   (plan-guard-user-decision
	     step
	     conds-to-wait-for
	     unsat)))))

(defun plan-guard-user-decision (step conds-to-wait-for unsat)

  ;; Conds-to-wait-for and unsat are eq iff we're called by
  ;; initial-plan-step-guard.  And if they're not eq we can
  ;; assume they're not equal either.

  (describe-step-unsatisfied-conditions step unsat)

  (unless (eq conds-to-wait-for unsat)
    (tell-world-user
      "But some conditions have now been satisfied:~%~:{~3T~S = ~S~%~}"
      (stable-set-difference conds-to-wait-for unsat :test #'equal)))

  (loop
    (ecase (plan-guard-user-menu-choice step conds-to-wait-for unsat)
      (:go
       (return t))
      (:wait
       (setf (plan-step-guard-fun step)
	     #'(lambda (step)
		 (plan-step-guard step unsat)))
       (return nil))
      (:describe
       (describe-plan-step step))
      (:world
       (display-world-state *interact*)))))


(defun plan-guard-user-menu-choice (step conds-to-wait-for unsat)
  (menu-request
    `("-heading" ,(concat-string
		    (if (eq conds-to-wait-for unsat)
			"Unsatisfied"
		        "Still unsatisfied")
		    " conditions for "
		    (plan-step-description step))
      "Wait for conditions=:wait"
      "Don't wait=:go"
      "-line"
      "Describe=:describe"
      "Show world state=:world")))

(defun step-unsatisfied-conditions (step)
  (loop for c in (plan-step-conditions step)
	for (p v) = c
	unless (or (world-always-p p v)
		   (world-fact-p p v))
	collect c))

(defun describe-step-unsatisfied-conditions (step unsat)
  (tell-world-user
    "Plan step ~A ~A ~A has unsatisfied conditions:~%~:{~3T~S = ~S~%~}"
    (plan-step-node step)
    (plan-step-end step)
    (plan-step-pattern step)
    unsat))


(defun install-micro-exec-command-mods ()

  (advice+ 'extract-event 'micro-exec
    #'(lambda (previous)
	#'(lambda (n)
	    (let ((event (funcall previous n)))
	      (when (and event (plan-step-p event))
		(setf (plan-step-guard-fun event)
		      'initial-plan-step-guard))
	      event))))

  )

(install-micro-exec-command-mods)


;;;; Success

;;; "Success" means the step made it off the agenda w/o being killed.
;;; The user might, however, still want it to fail, and we give the
;;; user various options.

(definit :world *plan-step-success-mode* :ask) ;either :auto or :ask

(defun plan-step-success (step)
  (ecase (plan-step-filter-action step)
    (:ask
     (plan-step-user-decision step))
    (:succeed
     (plan-step-real-success step))
    (:fail
     (plan-step-failure step :killed-by-filter))
    ((nil)
     (ecase *plan-step-success-mode*
       (:auto (plan-step-real-success step))
       (:ask (plan-step-user-decision step))))))

(defun plan-step-user-decision (step)
  (loop
    (ecase (plan-step-success-menu-choice step)
      (:describe
       (describe-plan-step step))
      (:succeed
       (return (plan-step-real-success step)))
      (:fail
       (return (plan-step-failure step :killed-by-user)))
      (:partial-fail
       (return (plan-step-partial-failure step)))
      (:event-first
       (get-world-event-from-user)
       (schedule-world-event step)	;so guard-fun is run again
       (return nil))
      (:event-after
       (schedule-get-world-event-from-user))
      (:world
       (display-world-state *interact*))
      (:configure
       (configure-micro-exec-world))
      (:flip-mode
       (flip-mode '*plan-step-success-mode*))
      (:break
       (break "Plan step")))))

(defun plan-step-success-menu-choice (step)
  (menu-request
    `("-heading" ,(plan-step-description step)
      "Describe=:describe"
      "Succeed=:succeed"
      "Fail=:fail"
      "Partial fail=:partial-fail"
      "Event first=:event-first"
      "Event after=:event-after"
      "-line"
      "Show world state=:world"
      "Configure=:configure"
      ,(format nil
	  "Set mode to: ~(~A~)=:flip-mode"
	  (opposite-mode *plan-step-success-mode*))
      "-line"
      "Break in=:break")))


;;; Describing a plan-step

(defun describe-plan-step (step)
  ;; Description
  (tell-world-user "Plan step ~A ~A ~A at ~A"
    (plan-step-node step)
    (plan-step-end step)
    (plan-step-pattern step)
    (simulated-time-string
     (current-simulated-time)))
  ;; Due time
  (tell-world-user "Due time: ~A"
    (simulated-time-string
     (plan-step-due-time step)))
  ;; Conditions
  (when (plan-step-conditions step)
    (tell-world-user "Conditions: ~:{~%~3T~S = ~S~}"
      (plan-step-conditions step))
    ;; Unsatisfied conditions
    (let ((unsat (step-unsatisfied-conditions step)))
      (if unsat
	  (tell-world-user "Unsatisfied conditions: ~:{~%~3T~S = ~S~}"
	    unsat)
	(tell-world-user "All conditions satisfied."))))
  ;; Effects
  (when (plan-step-effects step)
    (tell-world-user "Effects: ~:{~%~3T~S = ~S~}"
      (plan-step-effects step)))
  ;; Done
  (tell-world-user "")
  (values))


;;; Real success

(defun plan-step-real-success (step)
  ;; Note in the history display
  (display-history "~A" (plan-step-description step))
  ;; Effects take place
  (loop for (pattern value) in (plan-step-effects step)
	unless (has-world-always-value-p pattern)
	do (set-world-pattern-value pattern value))
  ;; Tell the Exec
  (report-to-exec
     :success
     (current-simulated-time)
     (plan-step-node step)
     (plan-step-end step)
     (plan-step-pattern step)))


;;; (real) Failure

(defun plan-step-failure (step reason &optional (failed-effects nil partial-p))
  (display-history "~A;~%~26TFailed because ~S"
		   (world-event-description step)
		   reason)
  ;; Maybe some effects take place
  (when partial-p
    (loop for e in (plan-step-effects step)
	  for (p v) = e
	  unless (or (has-world-always-value-p p)
		     (member e failed-effects :test #'equal))
	  do (set-world-pattern-value p v)))
  ;; Tell the Exec
  (report-to-exec
     :failure
     (current-simulated-time)
     reason
     (plan-step-node step)
     (plan-step-end step)
     (plan-step-pattern step)
     (if partial-p
	 failed-effects
       (plan-step-effects step))))

(defun report-to-exec (&rest agenda-body)
  (ipc-write :micro-exec agenda-body))


;;; (real) Partial failure

(defun plan-step-partial-failure (step)
  (tell-world-user "Partial failure of ~A ~A ~A"
    (plan-step-node step)
    (plan-step-end step)
    (plan-step-pattern step))
  (if (plan-step-effects step)
      (tell-world-user
        "Answer \"y\" if the effect succeeds, \"n\" if it fails.")
      (tell-world-user
        "There are no effects, and so it will just fail."))
  (plan-step-failure
    step
    :partially-killed-by-user
    (remove-if #'user-says-effect-happens
	       (plan-step-effects step)))
  (tell-world-user ""))

(defun user-says-effect-happens (effect)
  (destructuring-bind (p v) effect
    (ask-if *interact* "y" "  ~S = ~S" p v)))


;;;; Unexpected (unplanned) events

;;; World-event can be called manually but normally isn't.

(defun world-event (pattern effects)
  ;; /\/: Filter out any effects that have the same pattern as an always.
  (display-history "~A causes~:{~%~26T~A = ~A~}"
		   pattern
		   effects)
  ;; Effects take place
  (loop for (pattern value) in effects
	unless (has-world-always-value-p pattern)
	do (set-world-pattern-value pattern value))
  ;; Tell the Exec
  (report-to-exec
     :world-event
     (current-simulated-time)
     pattern
     effects))


;;; Getting events from the user

(defun get-world-event-from-user ()
  (tell-world-user "Enter a world event.")
  (world-event-help-if-needed)
  (multiple-value-bind (pattern effects)
      (ask-user-for-event-pattern-and-effects)
    (when pattern
      (describe-users-world-event pattern effects)
      (cond ((ask-if *interact* "y" "Ok?")
	     (world-event pattern effects)
	     (tell-world-user ""))
	    ((ask-if *interact* "n" "Try entering it again?")
	     (get-world-event-from-user))
	    (t
	     (tell-world-user ""))))))

(defun schedule-get-world-event-from-user ()
  (schedule-world-event
    (make-world-event
      :due-time -1			;to make sure it's 1st in the queue /\/
      :description "Ask user for a world event"
      :succeed-fun
        #'(lambda (event)
	    (declare (ignore event))
	    (get-world-event-from-user))
      :fail-fun
        #'(lambda (event)
	    event)))
  (display-world-agenda *agenda-window*))

(defun describe-users-world-event (pattern effects)
  (tell-world-user "The event will be ~A." pattern)
  (if (null effects)
      (tell-world-user "With no effects.")
    (let ((always-conflicts
	   (loop for e in effects
		 for (p v) = e
		 when (and (has-world-always-value-p p)
			   (not (world-always-p p v)))
		 collect e))
	  (fact-conflicts
	   (loop for (p v) in effects
		 when (and (world-pattern-has-value-p p)
			   (not (world-fact-p p v)))
		 collect `(,p ,(get-world-pattern-value p) ,v))))
      (tell-world-user "With effects ~:{~%  ~S = ~S~}" effects)
      (when always-conflicts
	(tell-world-user
	  "Some of the effects conflict with always facts ~
           and will be ignored: ~:{~%  ~S = ~S~}"
	  always-conflicts))
      (when fact-conflicts
	(tell-world-user
	  "Some of the effects will change existing values:~
           ~:{~%  ~S = ~S, will = ~S~}"
	  fact-conflicts)))))

(defun ask-user-for-event-pattern-and-effects ()
  (let ((pattern (ask-user-for-event-pattern)))
    (if pattern
	(values pattern
		(ask-user-for-event-effects))
      (tell-world-user "No event.~%"))))

(defun ask-user-for-event-pattern ()
  (loop
    (handler-case
        (let* ((pat-string (ask-for-line *interact* nil "Pattern:"))
	       (items (string->list (or pat-string ""))))
	  (cond ((null items)
		 (return nil))			;no event will occur
		((not (length=1 items))
		 (tell-world-user
		  "The pattern must be a single list."))
		((not (and (consp (car items))
			   (symbolp (caar items))))
		 (tell-world-user
		  "The pattern must be a list that begins with a symbol."))
		(t
		 (return (car items)))))
      (error (c)
	(tell-world-user "Error: ~A" c)
	(tell-world-user "Try again or enter a blank line to quit.~%")))))


(defun ask-user-for-event-effects ()
  (tell-world-user "Effects:")
  (loop for pair = (ask-user-for-one-effect)
	while pair
	collect pair))

(defun ask-user-for-one-effect ()
  (loop
    (handler-case
        (let* ((effect-line (ask-for-line *interact* nil " "))
	       (items (string->list (or effect-line "")))
	       (len (length items)))
	  ;; Secretly allow an = between pattern and value.  /\/
	  (when (and (= len 3) (eq (second items) '=))
	    (setq items (list (first items) (third items))
		  len 2))
	  (cond ((null items)
		 (return nil))
		((not (= len 2))
		 (tell-world-user
		  "Enter a pattern and value on one line."))
		((not (and (consp (car items))
			   (symbolp (caar items))))
		 (tell-world-user
		  "The pattern must be a list that begins with a symbol."))
		(t
		 (return items))))
      (error (c)
	(tell-world-user "Error: ~A" c)
	(tell-world-user "Type an effect or a blank line to finish.")))))


;;; World-event help

(defvar *user-may-need-help-p* t)

(defun world-event-help-if-needed ()
  (when *user-may-need-help-p*
    (if (setq *user-may-need-help-p*
	      (ask-if *interact* "n" "Do you need help?"))
	(give-world-event-help)
	(tell-world-user "OK, I'll remember that.~%"))))

(defun give-world-event-help ()
  (mapc #'(lambda (s) (tell-world-user "  ~A" s))
    '(""
      "You'll be asked first for a pattern, then for effects."
      "Enter the pattern as a list.  Enter an effect as a pattern"
      "(a list) followed by a value (list, symbol, or number),"
      "all on one line.  You can put = between the pattern and value."
      "Use Lisp syntax for all patterns and values."
      "Type a blank line after the last effect.  Example:"
      ""
      "Pattern: (flood)"
      "Effects:"
      "  (status bridge) = out"
      "  (status road) = closed"
      ""
      "If you decide not to enter an event, enter a blank line"
      "as the pattern.  You'll also get a chance to reject the"
      "event after you've supplied both pattern and effects."
      "")))


;;;; Always facts

(define-initializer :world clear-always-for-micro-exec ()
  (clear-world-always))

(defun display-world-always (&optional (stream *interact*))
  (let ((*package* (find-package :oplan))) ;get pkg prefixes right in output
    (loop for (pat . val)
	  in (get-world-always)
	  do (format stream "~&   ~S = ~S~%" pat val))
    (terpri stream)
    (values)))



;;;; Utilities

(defun read-from-lines (stream)
  ;; /\/: Reads only the first item from each line
  (loop for line = (read-line stream)
	until (string= line "")
	collect (read-from-string line)))


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
