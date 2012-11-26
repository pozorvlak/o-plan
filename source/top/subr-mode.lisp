;;;; File: subr-mode.lisp
;;; Contains: Subr mode
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: October 1994
;;; Updated: Sat May 17 03:50:45 1997 by Jeff Dalton
;;; Copyright: (c) 1994 - 1997 AIAI, University of Edinburgh

(in-package :oplan)

;;; Subr (subroutine) mode

;;; After the command-line arguments and the init file have been processed,
;;; O-Plan either starts up normally by calling run or else starts up
;;; "as a subroutine" by calling enter-subr-mode.  It does the latter
;;; when the :subr parameter is true.

;;; That :subr is true also ensures that there's no TA p-process at
;;; all and no repl p-process unless enter-subr-mode creates one.
;;; This turns out to mean that when enter-subr-mode calls run, and
;;; run calls pprocess-main-loop, there's "nothing to do" after the
;;; component p-processes start up.  So pprocess-main-loop, and
;;; hence run, return.  Enter-subr-mode then creates a :user p-process.
;;; The :user p-process exits pprocess-main-loop whenever it receives
;;; a message or times out, as described below.

;;; That completes the setup.  Enter-subr-mode then calls the code
;;; that uses O-Plan as a subroutine, as specified by the :do
;;; parameter.  That code can interact with O-Plan by repeating
;;; the following steps:
;;;   1. Send a message to O-Plan.
;;;   2. Call pprocess-main-loop.
;;;   3. When pprocess-main-loop returns, process any messages that
;;;      have been sent to the user p-process.

;;; Low and high level routines for doing this, setting timeouts, etc
;;; can be found in program-interface.lisp.

;;; The value of :do is either nil or a string containing an expression.
;;; If it's a string, the string is read and evaluated; otherwise a break
;;; loop is entered.

;;; N.B. Since we expect pprocess-main-loop to exit when exit-subr-mode
;;; calls run, no pprocess can have an input stream.  So we can't, for
;;; instance, have a control panel.  /\/

(defvar *subr-mode-exit-actions* '())	;list of thunks

(defun set-up-subr-mode ()
  (set-parameter :subr t))

(defun enter-subr-mode ()
  (setq *wait-for-pprocess-input-streams-p* nil)
  (run)					;start up components, etc.
  (setq *pprocess* (make-user-pprocess))
  (unwind-protect
      (if (get-parameter :do)
	  (eval (read-from-string (get-parameter :do)))
	(subr-mode-repl))
    (mapc #'funcall
	  *subr-mode-exit-actions*)))

(defun subr-mode-repl ()
  #+kcl
    (setq *break-enable* t)
  (break "Subr mode"))

(defun make-user-pprocess ()
  (new-pprocess :user
    :run-function 'user-pprocess-run-function
    :status :run-on-event))

(defun add-exit-action (thunk)
  (nconcf *subr-mode-exit-actions* (list thunk)))

;;; The user pprocess exits the pprocess-main-loop when it receives
;;; a message, leaving the message in the event queue.  The message
;;; is usually handled by procedures in program-interface.lisp.

;;; If the user pprocess runs and there's no message in its queue, it
;;; assumes that a timeout has occurred and signals a timeout condition.

;;; A timeout is not an error in itself, but in this case it's an
;;; error if it's not handled.  So if the user pprocess times out,
;;; up to two conditions may be signalled: first a timeout, then
;;; an error if the timeout isn't handled.

;;; Note that the wakeup-time of a pprocess is cleared when the pprocess
;;; is run.  To maintain a timeout across such events, we maintain a
;;; separate record on the user pprocess's plist.

(define-condition timeout (simple-condition) ())

(define-condition timeout-not-handled (simple-error) ())

(defun user-pprocess-run-function (self)
  (cond ((next-event-p self)
	 (setf (pprocess-wakeup-time self)
	       (getf (pprocess-plist self) :timeout))
	 (throw :pprocess-main-loop-exit nil))
	(t
	 ;; Assume timeout
	 (signal 'timeout
	   :format-string "User process timed out.")
	 (signal-error 'timeout-not-handled
	   :format-string "A user process timeout was not handled."))))

;;; Setting and cancelling user pprocess timeouts

(defun set-user-pprocess-timeout (seconds)
  (let ((user (find-pprocess :user)))
    (if seconds
	(set-to-sleep-for seconds user)
      (setf (pprocess-wakeup-time user) nil))
    (setf (getf (pprocess-plist user) :timeout)
	  (pprocess-wakeup-time user))))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
