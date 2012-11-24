;;;; File: kcl-fork.lsp
;;; Contains: KCL process forker
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: September 1995
;;; Updated: Sat Nov  9 21:55:23 1996 by Jeff Dalton
;;; Copyright: (c) 1995, AIAI, University of Edinburgh

(in-package :oplan)

(import 'pprocess::make-message)
(import 'pprocess::repeat-pprocess-cycle)

(use-package :fork)


;;; N.B. works only with [A]KCL / GCL.

;;; N.B. Be sure to use a real select, not the pseudo one, because
;;; pseudo-select sleeps for 1 second between checks for input, which
;;; can make things rather slow.


;;; This file provides a way for individial pprocesses (typically
;;; "components") to be split off into their own Unix process, by 
;;; forking the original O-Plan process.  A number of existing
;;; procedures have to be redefined, or advised, for this to work;
;;; so that's done when the first fork is requested (actually when
;;; the first pipes are set up).

;;; The separate processes still contain pprocesses that work in
;;; pretty much the usual way.  However, they initially contain only
;;; one (working) pprocess, and communication with the rest of O-Plan
;;; is via pipes.

;;; A pprocess should not be forked until all pprocesses it might
;;; communicate with exist.  This is because we need to know all its
;;; connections in order to create all the necessary pipes.

;;; /\/: At present, only certain standard components (not all pprocesses)
;;; are allowed to fork, and, when forked, a component is given pipes to
;;; all other forkable components.  This is right for the IM, AM, DM, and
;;; KP, but not for the TA.

;;; /\/: We call the two-way stream used by one pprocess to send and
;;; receive from another a "pipe".  It actually contains ends of two
;;; Unix pipes: one for sending to the other process and one for receiving
;;; from it.  The other process has a two-way stream that contains the
;;; other ends of the two Unix pipes.  So the word "pipe" might mean a
;;; Unix pipe or one of the two-way streams, depending on context.
;;; This is not ideal and should be changed if anyone thinks of something
;;; better.  "Pipe-stream" always means one of the two-way streams.

;;; Note that pipes between pprocesses in the same Unix process
;;; don't really work, because there's a limit to how much can
;;; be written to a pipe before the sender blocks.


;;;; New pprocess slots

;;; (remote-p p) is true in processes from which p has been removed by
;;; forking.  That is, p and the current process are in separate Unix
;;; processes.

(defun-inline pprocess-pipe-slots (p)
  (ensuref (getf (pprocess-plist p) 'pipe-slots)
	   (make-pprocess-pipe-aux)))

(define-struct-extension (pprocess-pipe-aux
			  :accessor-prefix pprocess-
			  :extension-prefix pprocess-pipe-aux-
			  :base-accessor pprocess-pipe-slots)
  ;; E.g. (pprocess-x p) == (pprocess-pipe-aux-x (pprocess-pipe-slots p))
  (forked-p nil)		;has been forked?  [true only in child]
  (remote-p nil)		;in a different unix process?
  (dest-to-pipe-table '())	;plist mapping pprocs to pipe-streams
  (input-pipes '())
  (reply-messages '()))		;list of (pipe . message) pairs

(defmacro pprocess-dest-pipe (p dest)
  ;; Get the pipe-stream that pprocess p uses to write to pprocess dest.
  `(getf (pprocess-dest-to-pipe-table ,p) ,dest))

;;; Table mapping a pipe-stream to the pprocess that reads and writes it.

(defvar *pipe-owner-table* (make-hash-table :test #'eq))

(defmacro pipe-owner (pipe)
  `(gethash ,pipe *pipe-owner-table*))

;;; List of all pipe-streams that this Unix process should try to read.
;;; Recomputed on both sides after every fork.

(defvar *active-pipe-streams* '())

;;; Remember child pids just in case.

(defvar *child-pids* '())

;;; Remember the original *terminal-io* just in case.

(defvar *original-terminal-io* *terminal-io*)

;;; We need to "throw then do"...

(defvar *reentry-actions* '())

(defun add-reentry-action (thunk)
  (nconcf *reentry-actions* (list thunk)))


;;;; Fork-pprocess

(defparameter *forkable-pprocess-names* '(:ta :am :im :dm :kp)
  "The names of all pprocesses that can be forked.")

(defun fork-pprocess (name)
  (unless (member name *forkable-pprocess-names*)
    (error "Can't fork ~S, only one of ~S." name *forkable-pprocess-names*))
  (let ((p (find-pprocess name)))
    (unless (component-p p)
      (error "Can't fork ~S, because it's not a component pprocess." p))
    (let ((debug-io (pprocess-symbol-value p '*debug-io*)))
      (assert (xterm-stream-pid debug-io)))
    (convert-to-pipes p)
    (add-reentry-action
      #'(lambda ()
	  (let ((child-pid (fork)))
	    (if child-pid
		(fork-pprocess-parent p child-pid)
	      (fork-pprocess-child p)))))
    (throw :pprocess-cycle-exit nil)))

(defun fork-pprocess-parent (p child-pid)
  (nconcf *child-pids* (list child-pid))
  (turn-off-remote-pprocess p)
  (determine-active-pipe-streams)
  ; (throw :pprocess-cycle-exit nil)
  )

(defun fork-pprocess-child (p)
  ;; Make more i/o go to p's xterm.
  (setq *terminal-io* (pprocess-symbol-value p '*debug-io*))
  (setf (pprocess-forked-p p) t)
  (dolist (other *all-pprocesses*)
    (unless (eq other p)
      (turn-off-remote-pprocess other)))
  (determine-active-pipe-streams)
  ; (call-in-pprocess-env p
  ;   #'(lambda () (break "Here we are.")))
  ; (setq *all-pprocesses* (list p))
  ; (throw :pprocess-cycle-exit nil)
  )

(defun turn-off-remote-pprocess (p)
  (setf (pprocess-remote-p p) t)
  (setf (pprocess-status p) :finished)
  (mapc #'close (pprocess-input-pipes p))
  ;; Close some input streams we think we can close.
  ;; KCL won't let us close standard input (and maybe some others).
  ;; We also have to be careful to protect *terminal-io*.  When a pprocess
  ;; is forked, *terminal-io* is given the value of *debug-io* and hence
  ;; refers to the main xterm of the forked pprocess.  So we don't want
  ;; to close *terminal-io*; but, since one of the lisp-listener pprocess's
  ;; input streams is a synonym for *terminal-io*, we will end up closing
  ;; it when we turn off the lisp listener.  Hence the temporary binding
  ;; to a null io-stream.
  (dolist (s (pprocess-input-streams p))
    (unless (<= (stream-fd (deref-stream s :input)) 2)
      (let ((*terminal-io* (make-null-io-stream)))
	(close s))))
  ;; Try to avoid killing (when we exit) the main xterm associated
  ;; with the remote process.
  (when (component-p p)
    (let* ((debug-io (pprocess-symbol-value p '*debug-io*))
	   (pid (xterm-stream-pid debug-io)))
      (when pid
	;; Iff there's a pid, we have a stream to an xterm.
	;; Make it somebody else's problem that it might need to be killed.
	(removef pid oplan-util::*xterm-pids*)
	(close debug-io))))
  p)

(defun remove-pprocess (name)
  (let ((p (find-pprocess name)))
    (setq *all-pprocesses* (remove-1-eq p *all-pprocesses*))
    p))

(defun determine-active-pipe-streams ()
  (setq *active-pipe-streams*
	(mapcan #'(lambda (p)
		    (unless (pprocess-remote-p p)
		      (copy-list (pprocess-input-pipes p))))
		*all-pprocesses*)))


;;;; Setting up pipe communications

(defun convert-to-pipes (pproc)
  (dolist (forkable *forkable-pprocess-names*)
    (let ((other (exists-pprocess forkable)))
      (when other
	(unless (eq other pproc)
	  (ensure-pipe-between pproc other))))))

(defun ensure-pipe-between (pproc1 pproc2)
  (when (symbolp pproc1) (setq pproc1 (find-pprocess pproc1)))
  (when (symbolp pproc2) (setq pproc2 (find-pprocess pproc2)))
  (ensure-redefinitions-for-pipes)
  ;; See if pipes are already in place.
  (if (pprocess-dest-pipe pproc1 pproc2)
      ;; If a pipe is used in one direction, it must be used in both.
      (assert (pprocess-dest-pipe pproc2 pproc1))
    ;; Set up symmetric pipes
    (multiple-value-bind (for-pproc1 for-pproc2) (make-pipe-set)
      (establish-pipe pproc1 pproc2 for-pproc1)
      (establish-pipe pproc2 pproc1 for-pproc2)
      t)))

(defun establish-pipe (pproc other-pproc pipe-stream)
  (assert (null (pipe-owner pipe-stream)))
  (setf (pipe-owner pipe-stream) pproc)
  (setf (pprocess-dest-pipe pproc other-pproc) pipe-stream)
  (push pipe-stream (pprocess-input-pipes pproc))
  pipe-stream)


;;;; Redefinitions for pipe communications

;;; Here's how it works.

;;; PProcesses send messages to other (named) pprocesses ... /\/

;;; So for communication via pipes, each pprocess has a mapping from
;;; destination pprocess objects to pipes.  When a message is sent,
;;; the destination is looked up in the sender's table.  If a pipe has
;;; been assigned, then the message is written to the pipe; otherwise,
;;; it is sent in the usual way.  So much for sending.

;;; For receiving, each pipe has an "owner": the pprocess that reads
;;; from it and writes to it.  The pprocess mechanism is modified to
;;; look at all pipes owned by active pprocesses ... /\/ ...

;;; We also have to say how we handle the case when the sender requires
;;; (and will wait for) a reply.  Then the message, M, is sent inside a
;;; :request message.  The :request handler calls the ordinary handler for
;;; M and sends back the result as the reply.  The original sender waits
;;; for the reply to appear on the appropriate pipe.  Note that :request
;;; messages are used only when we're using pipes.  That's why the handlers
;;; are defined here, below.

(defun ensure-redefinitions-for-pipes ()

  ;; For receiving messages -- won't take effect until we
  ;; throw to :pprocess-cycle-exit, which happens before a fork.
  (advice+ 'repeat-pprocess-cycle 'pipe-communications
    #'(lambda (previous)
	(declare (ignore previous))
	#'(lambda ()
	    (while *reentry-actions*
	      (funcall (pop *reentry-actions*)))
	    (loop
	      (run-1-pprocess-cycle)))))

  ;; When a message is sent
  (advice+ 'send-message 'pipe-communications
    #'(lambda (previous)
	(declare (ignore previous))
	#'(lambda (sender receiver contents)
	    (check-type sender (or pprocess null))
	    (check-type receiver pprocess)
	    (or (send-via-pipe-if-possible sender receiver contents)
		(send-direct-if-possible sender receiver contents)
		(error "Can't send from ~S to ~S~%~
                        Contents: ~S"
		       sender receiver contents)))))

  ;; send-immediate
  (advice+ 'send-immediate 'pipe-communications
    #'(lambda (previous)
	#'(lambda (receiver contents)
	    (let ((sender *pprocess*))
	      (or (send-via-pipe-if-possible sender receiver contents)
		  (funcall previous receiver contents))))))

  ;; db-request
  (advice+ 'db-request 'pipe-communications
    #'(lambda (previous &aux (DM (find-pprocess :DM)))
	#'(lambda (ident &rest args)
	    (let ((pipe (pprocess-dest-pipe *pprocess* DM)))
	      (if pipe
		  (pipe-request pipe DM ident args)
		(apply previous ident args))))))

  ;; ipc-ask
  (advice+ 'ipc-ask 'pipe-communications
    #'(lambda (previous)
	#'(lambda (process-name ident &rest args)
	    (let* ((receiver (find-pprocess process-name))
		   (pipe (pprocess-dest-pipe *pprocess* receiver)))
	      (if pipe
		  (pipe-request pipe receiver ident args)
		(apply previous process-name ident args))))))

  ;; terminate-pprocess
  (advice+ 'terminate-pprocess 'pipe-communications
    #'(lambda (previous)
	#'(lambda (&optional (p *pprocess*))
	    (when (pprocess-forked-p p)
	      (setq *active-pipe-streams* nil))	;gets us to exit /\/
	    (funcall previous p))))

  ;; Self
  (advice+ 'ensure-redefinitions-for-pipes 'start-doing-nothing
    #'(lambda (previous)
        (declare (ignore previous))
	#'(lambda () nil)))

  )


;;;; New definitions needed by the redefinitions above

(defun send-via-pipe-if-possible (sender receiver contents)
  ;; Returns true if it sends the message and false if it doesn't.
  (when sender
    (let ((pipe (pprocess-dest-pipe sender receiver)))
      (when pipe
	(send-via-pipe
	  pipe
	  (make-message
	    :sender (and sender (pprocess-name sender))
	    :contents contents))))))

(defun send-direct-if-possible (sender receiver contents)
  ;; Returns true if it sends the message and false if it doesn't.
  (unless (pprocess-remote-p receiver)
    (enqueue (make-message :sender sender :contents contents)
	     (pprocess-event-queue receiver))))

(defun pipe-request (pipe receiver ident args)
  (let ((sender-name (and *pprocess* (pprocess-name *pprocess*))))
    (send-via-pipe
      pipe
      (make-message
        :sender sender-name
	:contents 
	  (list :request
		(make-message :sender sender-name
			      :contents (cons ident args)))))
    (if (pprocess-remote-p receiver)
	(wait-for-reply pipe receiver ident args)
      (run-receiver-and-expect-reply pipe receiver ident args))))

(defun-inline reply-p (message)
  (and (consp message) (eq (car message) :%reply)))

(defun wait-for-reply (pipe receiver ident args)
  ;; Wait for a reply from the "reveiver" of the :request message.
  ;; Since there should never be > 1 outstanding request, anything
  ;; we get that begins with :%reply should be the reply we're after.
  (declare (ignore receiver ident args))
  (let ((self *pprocess*))
    (letf (((pprocess-status self) :suspended))
      (loop
        ;; Let other things run while we're waiting.
	(run-1-pprocess-cycle)
	(when (pprocess-reply-messages self)
	  (return))))
    ;; Seem to have a reply.
    (let* ((reply (pop (pprocess-reply-messages self)))
	   (reply-pipe (car reply))
	   (reply-message (cdr reply))
	   (reply-contents (cadr reply-message)))
      (assert (null (pprocess-reply-messages self)))
      (assert (reply-p reply-message))
      (assert (eq reply-pipe pipe))
      reply-contents)))

(defun run-receiver-and-expect-reply (pipe receiver ident args)
  ;; N.B. The receiver is _not_ in a separate Unix process, so
  ;; it doesn't run unless we let it.
  (declare (ignore ident args))
  (until (listen pipe)
    ;; /\/: Call run-as-subroutine so the run-lights are right?
    (run-pprocess receiver))
  (let ((reply (read-from-pipe pipe)))
    (assert (eq (car reply) :%reply))
    (cadr reply)))


;;; :request message handlers

(defmessage (:DM :REQUEST) (message)
  (let ((pipe (pprocess-dest-pipe *pprocess*
		   (find-pprocess (message-sender message)))))
    (assert pipe)
    (send-via-pipe
      pipe
      (list :%reply
	    (db-answer (car (message-contents message))
		       (cdr (message-contents message)))))))


(defmessage (:AM :REQUEST) (message)
  (let ((pipe (pprocess-dest-pipe *pprocess*
		   (find-pprocess (message-sender message)))))
    (assert pipe)
    (send-via-pipe
      pipe
      (list :%reply
	    (ipc-handle-message *pprocess* message)))))


;;;; Pipe primitives

(defun make-pipe-set ()
  (let ((pipe1 (pipe))			;from A to B
	(pipe2 (pipe)))			;from B to A
    (values
      (make-two-way-stream		;for A
        (fdopen (car pipe2) :input)	;  from B
	(fdopen (cdr pipe1) :output))	;  to B
      (make-two-way-stream		;for B
        (fdopen (car pipe1) :input)	;  from A
	(fdopen (cdr pipe2) :output)))));  to A

;;; Writing to pipes

(defun send-via-pipe (pipe obj)
  (print-readably obj pipe)
  (force-output pipe)
  obj)

;;; Reading from pipes

(defun read-from-pipe (stream)
  (read-safely stream))


;;;; /\/: Run-1-pprocess-cycle

(import '(pprocess::forget-time
	  pprocess::wakeup-and-run-pprocess
	  pprocess::all-monitored-streams
	  pprocess::minimum-timeout))

(defun run-1-pprocess-cycle ()
  (let ((running nil))
    (forget-time)
    (dolist (p *all-pprocesses*)
      (ecase (pprocess-status p)
	(:run-on-event
	 (when (or (not (empty-queue-p (pprocess-event-queue p)))
		   (and (pprocess-wakeup-time p)
			(>= (get-time) (pprocess-wakeup-time p)))
		   (and (pprocess-input-streams p)
			(some #'listen (pprocess-input-streams p))))
	   (setq running t)
	   (run-pprocess p)))
	(:run
	 (setq running t)
	 (run-pprocess p))
	(:suspended
	 (when (and (pprocess-wakeup-time p)
		    (>= (get-time) (pprocess-wakeup-time p)))
	   (setq running t)
	   (wakeup-and-run-pprocess p)))
	(:finished)
	(:new)))
    (when (not running)
      ;; /\/: Do something even if running when there are pipe-streams?
      (let ((streams (append *active-pipe-streams*
			     (all-monitored-streams *all-pprocesses*)))
	    (timeout (minimum-timeout *all-pprocesses*)))
	;; /\/: Won't there always be active pipe streams?
	(cond (*active-pipe-streams*
	       (get-messages-from-pipes streams timeout))
	      (streams
	       (select-input-p streams timeout))
	      (timeout
	       (sleep timeout))
	      ;; /\/: :pproccess-main-loop-exit might not exist?  (check)
	      (t (throw :pprocess-main-loop-exit :done)))))))

(defun get-messages-from-pipes (streams timeout)
  ;; The streams may initially include non-pipes, but only on the
  ;; first iteration.
  (loop
    (let ((ready-streams (select-input streams timeout)))
      ;; Any input available?  If not, it's probably a timeout.
      (unless ready-streams
	(return))
      ;; Read and deliver the first message from any pipe-stream that has one.
      (dolist (stream ready-streams)
	(let ((destination (pipe-owner stream)))
	  (when destination
	    ;; This stream is a pipe-stream.
	    (let ((message (read-from-pipe stream)))
	      (if (reply-p message)
		  ;; A reply to some :request
		  (push (cons stream message)
			(pprocess-reply-messages destination))
		;; An ordinary message
		(enqueue message (pprocess-event-queue destination)))))))
      ;; See if there's anything more in the pipe-streams.
      (setq streams *active-pipe-streams*
	    timeout 0))))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
