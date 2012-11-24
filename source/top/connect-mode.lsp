;;;; File: connect-mode.lsp
;;; Contains: Connected-mode
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: 31 March 1993
;;; Updated: Sat May 17 03:50:32 1997 by Jeff Dalton
;;; Copyright: (c) 1993 - 1997, AIAI, University of Edinburgh

(in-package :oplan)

;;; Connect[ed] mode

;;; Initsystem provides is-set-connect, which is used to record that we're
;;; in connect-mode, and is-connect-p which is used to test whether we are.

;;; Our task here is (1) to reassign some of the standard Lisp stream
;;; variables (such as *terminal-io*) so that they no longer use the
;;; stdin or stdout of the Lisp process, (2) to open a window that can
;;; be used for those standard stream variables and for interaction
;;; with the repl process, (3) to make *agent-io* refer to the original
;;; stdin and stdout, and (4) to create a connector "TA" process that
;;; connects *agent-io* to the IM.

;;; Tasks (1)-(3) are performed by set-up-connect-mode, which is called
;;; by oplan-top-level when it processes the "-connect" command-line
;;; argument.  Set-up-connect-mode manipulates *terminal-io* itself.
;;; It calls open-repl-window to open the window, and open-repl-window
;;; calls x-redirect-errors to change *debug-io*, *trace-output*, and
;;; *error-output*.  It's a bug if these changes are not completed before
;;; any output is directed to any of the standard streams involved,
;;; so these changes should happen as soon as possible.

;;; /\/: What about *standard-input*, *standard-output*, and *query-io*?
;;; In many Lisps they are synonym-streams for *terminal-io*, but what
;;; if they aren't?

;;; Task (4) is performed by connect-to-agent-io.  This procedure
;;; should not be called until after the IM exists.  Otherwise it may
;;; try to send something to the IM before there is anything there to
;;; receive it.  (This could happen when we're run as the Exec,
;;; because there will be some input ready in *agent-io* almost as
;;; soon as Lisp starts running.)

;;; Connect-to-agent-io also writes the "agent ready message" to *agent-io*.
;;; It's important that this not happen until after we've successfully
;;; put up a window.  If we're creating windows at all, and we're not
;;; in subr mode, the repl window should already be up before connect-
;;; to-agent-io is called.  It might be better to have the IM send the
;;; message, or to wait until all windows are up (remember that that
;;; happens in the 1st cycle of pprocess-main-loop, not before), but
;;; it's also desirable to have the connect mode stuff (which includes
;;; the ready message) all in one place.  So all things considered,
;;; connect-to-agent-io is a reasonable choice.


;;; Interaction between the :windows, :interactive, and :connect parameters

;;; If the :windows parameter is set to nil, that may happen either
;;; before of after set-up-connect-mode is called.  For instance,
;;; the command-line arguments may contain "-no -windows" either
;;; before or after "-connect".

;;; If :windows is set to nil before set-up-connect-mode, then it
;;; won't be possible to create the repl window.  In that case,
;;; we set :interactive to nil and try to turn off all component
;;; trace/debug output.

;;; If :windows is set to nil after set-up-connect-mode, then the
;;; repl window will be created, even though other windows cannot
;;; be created later on.  In this case, :interactive does not have
;;; to be nil, and component output can go to the repl window
;;; (just as it would go to the Lisp interaction window if :windows
;;; was nil and we werent in connect mode).

(defvar *agent-io* nil)			;comms with the Task Assignment agent

(defvar *repl-window* nil)		;just to remember it
(defvar *initial-terminal-io* nil)	;just to remember it

(defun set-up-connect-mode ()
  (is-set-connect t)
  (protect-terminal-io))

(defun protect-terminal-io ()
  (setq *agent-io* *terminal-io*)	;/\/ portable?
  (setq *initial-terminal-io* *terminal-io*)
  (cond ((get-parameter :windows)
	 (setq *repl-window* (open-repl-window))
	 (setq *terminal-io* *repl-window*))
	(t
	 ;; No repl window.  We don't consider this interative.
	 (set-parameter :interactive nil)
	 ;; Output off.  Since components don't yet exist, we can
	 ;; get away with changing the level they'll pick up when
	 ;; they're created.
	 (setq *component-initial-debug-level* :nothing)
	 ;; We may as well set the global current debug level too.
	 ;; [This sets the global level because we're not in a
	 ;; component's pprocess env.]
	 (dev-set-debug-level :nothing))))

(defun open-repl-window ()
  (x-open-and-register-io-win :repl-io
    (or (is-get-window-args :repl-io)
	(list "-title" "O-Plan Lisp interaction"
	      "-n" "Lisp Listener")))
  (x-redirect-errors :repl-io)
  (x-get-stream :repl-io))


(defun connect-to-agent-io ()

  ;; If there's no repl window, block output to *terminal-io*.
  (unless (x-get-stream :repl-io)
    #+:undef
    (setq *terminal-io* 'cannot-output-to-terminal-io-in-connect-mode))

  ;; Create a TA process
  (new-pprocess :ta
    :input-streams (list *agent-io*)
    :status :run-on-event
    :run-function 'agent-io-event-handler)

  ;; Say we're ready.
  (ipc-write-agent-ready *agent-io*)
  )

(defun agent-io-event-handler (self)
  (while (next-event-p self)
    (print-readably (message-contents (next-event self)) *agent-io*)
    (terpri *agent-io*)
    (force-output *agent-io*))
  (while (listen *agent-io*)
    (let ((msg (read-safely *agent-io*)))
      #+kcl
        (pprocess::really-clear-whitespace *agent-io*)
      (ipc-write-to-oplan msg))))

(defparameter *agent-ready-line* "<<<Ready>>>")

(defun ipc-write-agent-ready (stream)
  (terpri stream)
  (write-line *agent-ready-line* stream)
  (finish-output stream))

;;; End
