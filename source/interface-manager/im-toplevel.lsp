;;;; File: im-toplevel.lsp
;;; Contains: Top-level processing for the IM.
;;; Author: Richard Kirby (rbk) and Jeff Dalton
;;; Created: Thu Apr 12 11:38:24 1990
;;; Updated: Tue Jan 28 22:41:17 1997 by Jeff Dalton
;;; Copyright: (c) 1992, AIAI, University of Edinburgh

(in-package :oplan)

(defun im-startup ()

  ;; This is the initial function for the IM process.  After startup,
  ;; it is not called again.  Instead, im-event-handler is called
  ;; whenever a message arrives.
  
  (is-init)
  ;; Make breaks, etc. go to the debug window.
  (x-redirect-errors :imout)
  (clear-screen *debug-io*)
  (set-up-control-panel)
  (ipc-set-event-handler 'im-event-handler))

(defun im-event-handler (self)
  ;; /\/: May eventually have input-streams -- hence the "when".
  (when (next-event-p self)
    (ipc-handle-message self (next-event self))))

(defmessage (:IM :INIT) ()
  (im-init))

(defun im-init ()
  (clear-screen *debug-io*)
  (dev-debug :information "Initialised")
  :ok)


;;; :CONTROL messages

;;; The handler is called with the message's sender as its 1st arg.
;;; This is not how messages normally work.

(defmessage (:im :control) (sender id &rest args)
  (apply (ipc-get-handler :control id)
         sender
         args))


;;; :EVENT messages

(defmessage (:im :input) (message)
  ;; Check syntax
  (unless (and (consp message)
	       (keywordp (car message)))
    (invalid-command "~S is not a list beginning with a keyword."
		     message))
  ;; N.B. The message is :EVENT rather than :AGENDA so the AM
  ;; knows to use the agent agenda.
  (ipc-send :AM :EVENT
    ;; The ident field will be set by the AM when handling the message.
    ;; A unique id will be assigned, of the form AE-n.
    (make-event :trigger t
		:body message)))


;;; :OUTPUT messages

;;; :Output messages should not be sent directly.  Instead call ipc-send-out.
;;; ipc-send-out assumes everyone can send to the IM, whereas they cannot all
;;; send directly to the TA.

(defmessage (:im :output) (message)
  (if (exists-pprocess :user)
      (ipc-write :user message)
    (ipc-write :ta message)))


;;;; Handlers for :CONTROL messages


(defvar *control-connection-list* nil)	;list of pprocess names


;;; :DYING

;;; /\/ We ought to send messages to any connected agents and any
;;; processes that aren't on the connection list (eg, random p-processes
;;; in the single-process version).  But it's better to do something
;;; that at least gets this agent to exit rather than wait until we
;;; have time to do it right.  Hence the call to exit-oplan.

(defmessage (:control :dying) (sender)
  ;; Forget the process that started us exiting.
  (removef sender *control-connection-list*)
  ;; Now tell all the other processes registered with the IM to die.
  ;; Wait for the results, and then exit.
  (dolist (proc *control-connection-list*)
    (ipc-ask proc :DIE-DIE-DIE))
  (throw :pprocess-main-loop-exit nil)	;/\/ Out, out, out!
  (ipc-terminate))			;NOTREACHED


;;; :REGISTER

(defmessage (:control :register) (sender who)
  (assert (eq sender who))
  (push sender *control-connection-list*)
  :OK)


;;; :INIT

;;; Sends an :INIT messages to all registered processes, except for
;;; the KP that send the :INIT message here, then returns :OK.  Should
;;; be called with ipc-ask-control.

(defmessage (:control :init) (sender)
  ;; Init self.
  (im-init)
  ;; Typical connection-list: (:KP :DM :AM)
  ;; Typical sender: :KP
  (dolist (pproc *control-connection-list*)
    (unless (eq pproc sender)
      (ipc-ask pproc :init)))
  :OK)


;;; :SET-DEBUG-LEVEL

(defmessage (:control :set-debug-level) (sender who level)
  (declare (ignore sender))
  ;; Sends :DEBUG-LEVEL message to the process/processes requested.
  (case who
    ((:ALL)
     (dolist (proc *control-connection-list*)
       (ipc-send-priority-message proc :DEBUG-LEVEL level))
     (dev-set-debug-level level))
    ((:IM)
     (dev-set-debug-level level))
    (otherwise
     (ipc-send-priority-message who :DEBUG-LEVEL level))))


;;; :SINGLE-STEP

(defmessage (:control :single-step) (sender destination value)
  (declare (ignore sender))
  ;; Sends :SINGLE-STEP message to a process.
  (ipc-send-priority-message destination :SINGLE-STEP value))


;;; :INTERRUPT

(defmessage (:control :interrupt) (sender destination)
  ;; Sends :INTERRUPT message to a process.
  (declare (ignore sender))
  (if (eq destination :KP)
      (ipc-send-priority-message :AM :EVENT
	 (make-event
	    :trigger t
	    :body '(:USER :USER-REQUEST)))
    (ipc-send-priority-message
       destination :INTERRUPT)))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;

