;;;; Definitions from the original "simple world"

;;;; Intorduction

;;; As distributed, the World is set up to handle simple plan execution.
;;; The principal message the Exec can send the World is:

;;;   :BEGIN-ACTION action-pattern eft lft

;;; The action is assumed to start at the simulated time when the message
;;; is received, and the World enqueues an event to mark the action's
;;; finish.  This finish event can be killed by the user or fail in other
;;; ways.  So, in effect, the World never initiates actions; it just tracks
;;; when (and whether) they are completed successfully.  However, the
;;; World could do more than this if appropriate messages were defined.

;;; The basic protocol for running a simulation is for the Exec to:
;;;   1. Send an :INIT message to the World.
;;;   2. Maybe send some :BEGIN-ACTION messages so the action-finish
;;;      events will already be enqueued when the clock starts.  These
;;;      actions start at simulated time 0.
;;;   3. Send a :START-CLOCK message.
;;;   4. Maybe send some more :BEGIN-ACTION messages while the
;;;      simulation is running.  These actions start right away.
;;;   5. Send a :STOP-CLOCK or :INIT message to stop the simulation.


;;;; Plan actions

;;; Plan-action is a subclass of world-event.  It represents the
;;; completion of the action of an action node in the plan.

(defstruct (plan-action
	    (:include world-event))
  node
  pattern)


;;;; Messages

(defmessage (:world :begin-action) (node-name pattern eft lft)
  ;; Eft and lft are the earliest and latest (simulated) finish times.
  (declare (ignore lft))
  (display-history "~12S: Begin ~S." node-name pattern)
  (schedule-world-event
    (make-plan-action
      :due-time eft
      :description
        (format nil "~12S: End ~S." node-name pattern)
      :node node-name
      :pattern pattern
      :succeed-fun 'plan-action-success
      :fail-fun 'plan-action-failure)))

(defun plan-action-success (action)
  ;; Note in the history display
  (display-history "~A" (world-event-description action))
  ;; Tell the Exec
  ;; /\/: Don't we need to tell the Exec _when_ it happened?
  (send-to-exec :dispatch-2
		(plan-action-node action)
		(plan-action-pattern action)
		t			;success
		nil))			;no reason

(defun plan-action-failure (action reason)
  (display-history "~A;~%~26TFailed because ~S"
		   (world-event-description action) reason)
  (send-to-exec :dispatch-2
		(plan-action-node action)
		(plan-action-pattern action)
		nil			;failure
		reason))		;reason

;;; End
