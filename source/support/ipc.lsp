;;;; File: ipc.lsp
;;; Contains: Inter-Process Communications interface, single-process version.
;;; Author: Jeff Dalton
;;; Created: Jan 1993
;;; Updated: Sun Feb 28 20:50:43 1999 by Jeff Dalton
;;; Copyright: (c) 1993, 1996 AIAI, University of Edinburgh

(in-package :oplan-ipc)

(use-package :oplan-util)
(use-package :oplan-pseudo-process)

(export
 '(
   ;; Message handlers
   defmessage
   ipc-get-handler
   ipc-exists-handler
   ipc-list-all-handled-messages
   ipc-handle-message

   ;; Process operations
   ipc-whoami
   ipc-set-event-handler
   ipc-terminate
   ipc-register-listen-stream
   ipc-unregister-listen-stream

   ;; Control messages
   ipc-send-control
   ipc-ask-control
   ipc-register-with-im

   ;; Ordinary message sending
   ipc-send
   ipc-write
   ipc-send-priority-message

   ;; Sending planner (agent) output
   ipc-send-out
   ipc-write-out

   ;; Sending planner (agent) input
   ipc-send-to-oplan
   ipc-write-to-oplan

   ;; "Requests" that send a message and wait for a reply
   db-request
   db-call
   am-request				;for KS-CHECK-PLAN /\/

   ;; Low-level "request" support
   ipc-ask

   ;; Running Unix programs
   ipc-run-program

   ;; Copying
   ipc-with-own-copy
   ))


;;;; Message handlers
;;;
;;; Define a handler like this:
;;;
;;;   (DEFMESSAGE (recipient message-id) (message-arg...) form...)
;;;
;;; Think of DEFMESSAGE as defining the meaning of a message, which
;;; includes how it is handled.  The syntax is similar to that of
;;; Flavors DEFMETHOD.  The recipient is either a <pprocess-name> or
;;; an id, such as :COMPONENT-DEFAULT, that isn't actually a pprocess
;;; name.  In that case, IPC-GET-HANDLER will still work, but not
;;; IPC-HANDLE-MESSAGE.
;;;
;;; A pprocess handles a message from its event-queue by calling
;;;
;;;   (IPC-HANDLE-MESSAGE self message)
;;;
;;; Note that message is the entire message object, as defined by the
;;; pprocess message system, not just the message-contents.  However,
;;; the message-id and message-args in the DEFMESSAGE correspond to
;;; the contents, not the entire message.  (If you think about it,
;;; this is reasonable; the other stuff is handled behind the scenes.)
;;;
;;; (IPC-GET-HANDLER recipient-name message-id) is a lower-level way to
;;; look up a message handler without calling the handler or extrating
;;; the message-contents from a message.  It's used chiefly to handle
;;; DB-REQUESTs, which aren't packaged up as pprocess messages.

(defmacro %get-message-handler (recipient-name message-id)
  `(get ,message-id ,recipient-name))

(defun-inline ipc-get-handler (recipient-name message-id)
  (or (%get-message-handler recipient-name message-id)
      (error "No handler for ~S ~S." recipient-name message-id)))

(defun ipc-set-handler (recipient-name message-id handler)
  (setf (%get-message-handler recipient-name message-id) handler)
  (nconcf-new (get recipient-name 'all-handled-messages) message-id)
  handler)

(defsetf ipc-get-handler ipc-set-handler)

(defun ipc-exists-handler (recipient-name message-id)
  (%get-message-handler recipient-name message-id))

(defun ipc-list-all-handled-messages (recipient-name)
  (get recipient-name 'all-handled-messages))


(defmacro defmessage ((recipient-name message-id) formals &body handler-body)
  (let ((handler-name (concat-name "%" recipient-name "-" message-id
				   "-MESSAGE-HANDLER")))
    `(progn
       (defun ,handler-name ,formals
	 ,@handler-body)
       (setf (ipc-get-handler ',recipient-name ',message-id)
	     #',handler-name))))


(defun ipc-handle-message (self message)
  (let* ((recipient-name (pprocess-name self))
	 (contents (message-contents message))
	 (message-id (car contents))
	 (message-body (cdr contents))
	 (handler (or (%get-message-handler recipient-name message-id)
		      (error "No handler in ~S for ~S message: ~S."
			     recipient-name message-id message))))
    ;; Looks ok, so call the handler
    (apply handler message-body)))


;;;; Process operations

(defun ipc-whoami ()
  (check-type *pprocess* pprocess)
  (pprocess-name *pprocess*))

(defun ipc-set-event-handler (handler &key (status :run-on-event))
  (let ((component *pprocess*))
    (check-type component pprocess)
    (setf (pprocess-status component) status
	  (pprocess-run-function component) handler)))

(defun ipc-terminate ()
  (terminate-pprocess *pprocess*)	;just sets the status.
  t)


;;;; Listen-stream operations

(defun ipc-register-listen-stream (stream)
  (let ((p *pprocess*))
    (check-type p pprocess)
    (nconcf-new (pprocess-input-streams p) stream :test #'eq)
    stream))

(defun ipc-unregister-listen-stream (stream)
  (let ((p *pprocess*))
    (check-type p pprocess)
    (deletef stream (pprocess-input-streams p))
    stream))


;;;; Sending :CONTROL messages

;;; A message of the form (:CONTROL sender id . args) is sent to the IM,
;;; and the IM then calls a :CONTROL handler for the message.

(defun ipc-send-control (id &rest args)
  (apply #'ipc-send :im :control (pprocess-name *pprocess*) id args))

(defun ipc-ask-control (id &rest args)
  (apply #'ipc-ask :im :control (pprocess-name *pprocess*) id args))

(defun ipc-register-with-im ()
  "Sends a :register message to the Interface Manager, and then waits for an
   acknowledgment."
  (ipc-ask-control :register (pprocess-name *pprocess*)))



;;;; General message-sending

;;; The difference between ipc-send and ipc-write is intended to be
;;; merely syntactic.

(defun ipc-send (pprocess-name id &rest args)
  (send-to-pprocess (find-pprocess pprocess-name) (cons id args)))

(defun ipc-write (to what)
  "Stuffs what down a socket."
  (send-to-pprocess (find-pprocess to) what))


;;; High-priority messages
;;;
;;; The message is placed at the front of the queue.  This should be
;;; done _only_ for user-initiated events that are allowed to occur at
;;; any time and that should take effect as soon as possible.  Setting
;;; the AM single-step mode or changing a component's debug-level are
;;; examples.

(defun ipc-send-priority-message (recipient id &rest args)
  (send-immediate (find-pprocess recipient) (cons id args)))


;;; Sending output from the planner agent

(defun ipc-send-out (id &rest args)
  (ipc-ask :im :output (cons id args)))	;used to use ipc-send /\/

(defun ipc-write-out (message)
  (ipc-ask :im :output message))	;used to use ipc-send /\/


;;; Sending input to the planer agent

(defun ipc-send-to-oplan (id &rest args)
  (ipc-send :im :input (cons id args)))

(defun ipc-write-to-oplan (message)
  (ipc-send :im :input message))


;;;; DB requests

;;; The DM is called as a subroutine.  It's done as a special case
;;; rather than calling ipc-ask, to be faster and because db-handle-
;;; request has to handle non-lazy triggering (which ipc-handle-request
;;; wouldn't do).

(defvar *dm-process* nil)		;cache

(defun db-request (ident &rest args)
  (let ((dm (ensuref *dm-process* (find-pprocess :DM))))
    ;; Handle our request by pretending to be the DM process.
    (call-in-pprocess-env dm
      #'oplan::db-handle-request ident args)))

(defun-inline db-call (fn &rest args)
  (db-request :apply fn args))


;;;; AM requests

;;; /\/: Used only by KS-CHECK-PLAN?  [No.  E.g. ks-add-to-task]

(defun am-request (ident &rest args)
  (apply #'ipc-ask :am ident args))


;;;; Ipc-ask
;;;
;;; Ipc-ask sends a message to a given pprocess, and then waits for an
;;; answer.
;;;
;;; It should be used with caution in the single-process version,
;;; because all p-processes but the recipient will be unable to run
;;; until the answer has been returned.

(defun ipc-ask (process-name ident &rest args)
  (let ((p (find-pprocess process-name)))
    (call-in-pprocess-env p
      #'ipc-handle-request ident args)))

;;; Lower-level "request" support

;;; The message handler for the request is called without actually
;;; sending a message, to be faster.

(defun ipc-handle-request (ident args)
  (let ((receiver-name (pprocess-name *pprocess*)))
    ;; Take care of any messages before ours.
    (process-pending-events)
    ;; Handle our message and return the result.
    (apply (ipc-get-handler receiver-name ident) args)))

(defun process-pending-events ()
  ;; Must be called in the special variable env of *pprocess*.
  (while (next-event-p *pprocess*)
    (funcall (pprocess-run-function *pprocess*)
	     *pprocess*)))



;;;; Run a unix process and return an io stream

(defvar *stream->pid-table* '())	;for debugging, etc.

(defun ipc-run-program (name &rest args)
  (multiple-value-bind (stream pid)
      (apply #'oplan-util:unix-process-io name args)
    (setf (getf *stream->pid-table* stream) pid)
    stream))



;;;; Copying

;;; When a component needs to modify an object obtained from another
;;; component, it should make a copy of its own and use that instead.
;;; However, if ipc automatically provided the recipient with its own
;;; copy (as it would if each component was a separate Unix process
;;; communicating via sockets), then it shouldn't have to waste effort
;;; by making another copy.  Consequently, "own copies" should be
;;; made using a construct that can be redefined to avoid copying
;;; when such copies are unnecessary.  Hence the following macro.
;;; In the single-process O-Plan,
;;;
;;; (ipc-with-own-copy (<var> <init-form> <copy-form>) [<body>])
;;;
;;; returns the value of the last form in <body>, if a body is supplied,
;;; or else the value of <copy-form>.  In the body, <var> is bound to 
;;; the value of <copy-form>; in <copy-form> it is bound to the value
;;; of <init-form>.  (Let's hope this isn't too confusing.)
;;;
;;; <copy-form> should do nothing other than make a copy of the object.
;;; In a version of IPC that automatically supplied a recipient with its
;;; own copy, <copy-form> would be ignored and in effect replaced by
;;; (identity <var>>).

(defmacro ipc-with-own-copy ((var init-form copy-form) &body body)
  `(let ((,var ,init-form))
     (let ((,var ,copy-form))
       ,@(or body
	     `(,var)))))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
