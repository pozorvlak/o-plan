;;;; File: top-level.lsp
;;; Contains: O-Plan top-level 
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: March 1993
;;; Updated: Sat Feb  2 01:49:32 2008 by Jeff Dalton
;;; Copyright: (c) 1993 - 1997, AIAI, University of Edinburgh

;;; O-Plan's top-level / main program.

;;; Save-oplan is used to save an executable image when building O-Plan
;;; and it arranges for oplan-top-level to be called when the image is
;;; executed.

;;; O-Plan can run in several "modes".  Normally, O-Plan provides a
;;; Task Assigner (TA) process that allows the user to select commands
;;; from a menu.  There are commands that tell O-Plan to initialize,
;;; to load TF files, to find a plan for a given task, and so on.

;;; It's also possible for the TA role to be taken by an external Unix
;;; process -- "connect[ed] mode" -- or by Lisp code loaded into O-Plan --
;;; "subroutine mode".  The two non-default modes are selected by 
;;; command-line arguments.

;;; To run O-Plan in conected mode, use the command-line argument "-connect".
;;; The test for this case in O-Plan code is "(is-connect-p)".

;;; In connected mode, standard input and output are used for communicating
;;; with an external agent acting as the TA.  In the default "disconnected"
;;; mode, they are used for a read-evel-print loop ("Lisp listener") process.
;;; By running O-Plan in connected mode, it is possible to run it as a Unix
;;; process and communicate with it via pipes.  This is the way the Planner
;;; normally runs the Exec.

;;; To run O-Plan in subroutine mode, use the command-line argument "-subr".
;;; The test for this case is "(is-subr-p)".

;;; In subroutine mode, O-Plan does one of two things.  If the :do
;;; parameter has a string value, the value is read and evaluated;
;;; then O-Plan exits.  If the :do parameter is null, a read-eval-print
;;; loop is entered instead.  In both cases, the program interface
;;; (various Lisp procedures) can be used to control O-Plan.


(in-package :oplan)

(export 'save-oplan)


;;;; Save-oplan creates an executable image

;;; Save-image is implementation-dependent.

(defun save-oplan (filename)
  (oplan-util:save-image filename #'oplan-top-level))


;;;; Semi-portable top-level

;;; It's only semi-portable because it needs to do various implementation-
;;; dependent things such a process command-line arguments.

(defun oplan-top-level ()
  (catch :oplan-top-level-exit
    (handler-bind (((or serious-condition warning)
		    #'last-resort-condition-handler))
      (top-level-init)
      (unwind-protect
	  (cond ((is-subr-p)
		 (enter-subr-mode))
		((is-server-p)
		 (run-as-server))
		((is-connect-p)
		 (run))
		(t
		 (time (run))))
        #-:no-c-code
	(terminate-xterms-if-necessary))
      #+:allegro
      (progn (terpri) (force-output))))
  (exit-lisp 0))

(defun exit-oplan ()
  ;; Exits the O-Plan top level, which normally leads the Lisp process
  ;; exiting.
  (throw :oplan-top-level-exit nil))


;;; Initialization

(defun top-level-init ()

  ;; /\/: Maybe bind *package* etc rather than assign.
  ;; /\/: Need some of these values whenever O-Plan pprocesses are running.
  (setq *package* (find-package :oplan))
  ;; /\/: Other vars here?

  (set-parameter :process-model :single-process)

  ;; Set up other parameter defaults
  (is-init-parameter-defaults)

  ;; Process some values from environment variables as more defaults.
  (is-init-env-parameters)

  ;; Set the base directory for our systems.  This is done before
  ;; processing command-line arguments to that the args can load systems.
  (set-system-base-directory 'everything
    (simple-defsystem::relative-merge "source" (get-parameter :oplan-dir))
    :recursive t
    :silent t)

  ;; Process argv
  (process-command-line-arguments)

  ;; Load init file if wanted.
  (unless (get-parameter :noinit)
    (load-init-file "oplan-init" :oplan)))


;;; Command-line arguments

;;; Note that when processing command-line argument names such as "-load"
;;; we call string-upcase before interning (see string->keyword), thus
;;; implicitly assuming that the internal case is upper.  Franz Inc users
;;; beware.

(defun process-command-line-arguments ()
  (set-parameter :image-name (argv 0))
  (let ((i 1))
    (labels ((arg ()
	       (argv i))
	     (pop-arg ()
	       (prog1 (arg) (incf i)))
	     (arg->keyword (string)
	       (string->keyword (subseq string 1))))
      (loop
        (when (null (arg))
	  (return))
	(case (intern (string-upcase (arg)))
	  (-tfc      (pop-arg) (oplan-tf-compiler:run-as-tfc i))
	  (-load     (pop-arg) (load-most-recent (pop-arg)))
	  (-break    (pop-arg) (break "~A" '-break))
	  (-eval     (pop-arg) (eval (read-from-string (pop-arg))))
	  (-noinit   (pop-arg) (set-parameter :noinit t))
	  (-config   (pop-arg) (is-set-config-file (pop-arg)))
	  (-connect  (pop-arg) (set-up-connect-mode))
	  (-subr     (pop-arg) (set-up-subr-mode))
	  (-lisp     (pop-arg) (set-up-lisp-mode))
	  (-server   (pop-arg) (set-up-server-mode))
	  (-http     (pop-arg) (set-up-http-mode))
	  (-netscape (pop-arg) (set-up-netscape-mode))
	  (-load-system
	             (pop-arg) (is-load-system (pop-arg)))
	  ((-no -not)
	   ;; Used to set parameters to nil (false)
	   ;; /\/: Perhaps shouldn't require - or : on the parameter?
	   ;; /\/: Perhaps - or : should be an error.
	   (let ((negator (pop-arg))
		 (parameter (arg)))
	     (if (and parameter (find (char parameter 0) "-:"))
		 (set-parameter (arg->keyword (pop-arg)) nil)
	       (warn "~S was not followed by a parameter to negate"
		     negator))))
	  (t
	   ;; Other -name or :name args are given to set-parameter
	   ;; as keywords.  The parameter value will be a string.
	   (cond ((string= (arg) "")
		  (warn "Skipping null argument."))
		 ((find (char (arg) 0) "-:")
		  (let* ((arg (pop-arg))
			 (value (pop-arg)))
		    (if value
			(set-parameter (arg->keyword arg) value)
		      (warn "No value for ~A" arg))))
		 (t
		  (warn "Skipping unknown argument: ~A" (pop-arg))))))))))

(defun set-up-lisp-mode ()
  (set-parameter :windows nil)
  (set-up-subr-mode))


;;;; The "last resort" condition handler

(defvar *lrch-attempt* 0)

(defun last-resort-condition-handler (c)
  (unless (get-parameter :interactive)
    (incf *lrch-attempt*)
    (case *lrch-attempt*
      (1
       ;; Print a message and exit.  If a condition is signalled before
       ;; exit-oplan causes an unwind (e.g. if it happens while printing
       ;; the error message), then this handler will run again, but with
       ;; an attempt count of 2.
       (if (is-connect-p)
	   ;; Connect mode.
	   (locally (declare (special *agent-io*))
	     (print-readably
	        `(:fatal-error ,(type-of c) ,(princ-to-string c))
		*agent-io*)
	     (terpri *agent-io*)
	     (finish-output *agent-io*))
	 ;; Not connect mode.
	 (format *error-output*
		 "~&~%The following condition was not handled:~%~% ~A~%~%"
		 c))
       (exit-oplan))
      (2
       ;; This time we won't try printing a message.
       (exit-oplan))
      (t
       ;; It looks like the ordinary Lisp mechanisms are not going
       ;; to get us out.  We could try to call the C exit, but
       ;; right now we don't know how.  This seems to be pretty
       ;; close, though.  /\/
       (exit-lisp -1)))))


;;;; Run

;;; The code above is basically setup before, and environment around,
;;; a call to "run".

;;; The patch loader refers to parameters that can be set by environment
;;; variables, by command-line arguments, and by code in the oplan-init
;;; file, which is why we don't load patches sooner.

;;; /\/: But need to load patches before entering subr mode.

(defun run ()
  (is:is-patch-loader ".")		;load from source/./patch
  (is:is-configure)			;handle .config file
  (set-up-components)
  (when (is-connect-p)
    (connect-to-agent-io))		;must be after the IM exists
  (set-up-repl)
  (run-lights-if-wanted)
  (handler-bind ((error (top-level-error-handler)))
    (pprocess-main-loop)))
  
(defun set-up-repl ()
  (when (and (not (is-subr-p)) 
	     (get-parameter :interactive)
	     (implies (is-connect-p) (x-get-stream :repl-io)))
    (new-repl
      :start-fn 'print-repl-greeting)))

(defun run-lights-if-wanted ()
  (when (and (get-parameter :run-lights)
	     (get-parameter :windows))
    (let ((configured-args-if-any (is-get-window-args :run-lights)))
      (when configured-args-if-any
	(run-lights-on configured-args-if-any)))))

;;; An error handler that presents some options to the user

(defun top-level-error-handler ()
  (if (and (is-connect-p) (not (get-parameter :interactive)))
      #'non-interactive-connect-error-handler
    #'standard-top-level-error-handler))

(defun standard-top-level-error-handler (err)
  (when (and *pprocess*
	     (not (typep *pprocess* 'repl))
	     (not (is-subr-p))
	     (get-parameter :interactive))
    (format *debug-io* "~&~%Lisp error: ~A~%~%" err)
    (case (menu-request
	    `("-heading" ,(format nil "Lisp error in process ~S"
			   (pprocess-name *pprocess*))
	      "Allow the error=:allow"
	      "Return to scheduler=:return"
	      "Force global reset=:reset"))
      (:allow
       nil)				;decline
      (:return
       (throw :pprocess-cycle-exit nil))
      (:reset
       (force-reset)))))

(defun non-interactive-connect-error-handler (err)
  (declare (special *agent-io*))
  (when *pprocess*			;else decline

    (let ((message-type
	   (if (or *errors-are-harmless* (typep err 'harmless-error))
	       :harmless-error
	     :error)))

      ;; Tell the external TA about the error
      (print-readably
         `(,message-type ,(type-of err) ,(princ-to-string err))
	 *agent-io*)
      (terpri *agent-io*)
      (finish-output *agent-io*)

      ;; Try to get back to a reasonably well-defined state.
      (if (eq message-type :harmless-error)
	  (throw :pprocess-cycle-exit nil)
	;; This should result in an (:init-ok) message to the TA.
	(force-reset)))))


;;; Greeting functions

(defun print-repl-greeting (repl-pprocess)
  (terpri (repl-io repl-pprocess))
  (print-oplan-greeting (repl-io repl-pprocess)))

(defun print-oplan-greeting (stream)
  (format stream
	  "~&O-Plan version ~A~%~
             Release date: ~9,,,'0@A~%~
             Build date:   ~9,,,'0@A~%"
	  user::*oplan-version*
	  user::*oplan-release-date*
	  (oplan-load-date)))

(defun get-oplan-load-date () ; -> (date month-name year)
  (declare (notinline simple-defsystem:system-load-date)) ;in case redefined
  (oplan-time-util:decode-date-short
    (simple-defsystem:system-load-date
      (simple-defsystem:find-system 'oplan::oplan))))

(defun oplan-load-date ()
  (format nil "~:(~{~A~^-~}~)" 
	  (multiple-value-bind (day month year)
			       (values-list (get-oplan-load-date))
	    (list day month (format nil "~2,,,'0@A" year)))))


;;;; Component definition

;;; Set-up-components creates the component processes.  Note that some
;;; processes such as the TA that are not, strictly speaking, components
;;; of the planning / exec agent are nonetheless treated as components
;;; here.

;;; In the single-process version, component is a subtype of pprocess
;;; (pseudo-process), and the order in which pprocesses are created is
;;; also the order in which they are considered for running.

;;; [/\/ There's a hidden dependency in the order chosen below that should
;;; eventually be dealt with in some other way.  The DM is before the AM
;;; so that when the IM sends the DM an :interrupt message, the DM will
;;; handle the message when running in the normal way and not when called
;;; as a subroutine by the AM (which might happen were their order reversed).

;;; This works because the IM is never called as a subroutine.  So if it
;;; was able to send the message, it must have been called directly by
;;; pprocess-main-loop, and the same will be true of the DM -- if it runs
;;; between the IM and the AM.  If the AM gets there first, it might call
;;; the DM as a subroutine, and the DM would see the :interrupt when in
;;; that state instead.

;;; This matters because we want pprocesses to handle :interrupts when
;;; it's easy for them to let other pprocesses run while they're waiting
;;; for user input.  (It should be possible for > 1 pprocess to be in
;;; an interrupt loop at once, though we don't yet take much advantage
;;; of this.)  If a pprocess is running as a subroutine, nothing else
;;; can run until it returns.  So it would have to defer the point
;;; where it enters its interrupt loop by setting a flag, checking the
;;; flag whenever it's running as the top-level pprocess, etc.

;;; Of course, this reasoning would fall apart if, for instance, the
;;; DM ever called the AM as a subroutine (:single-step for the AM is
;;; very like :interrupt for the DM); but it never does.  It may seem
;;; unsafe to rely on this, but calling a pprocess as a subroutine is
;;; so restrictive that it has to be reserved for a small number of
;;; special cases or else the whole structure of the single-process
;;; system would be unworkable. /\/]

(defun set-up-components ()

  (unless (or (is-connect-p) (is-subr-p))
    (start-component :ta
      :directory "external"))

  (start-component :im
    :directory "interface-manager")

  (start-component :dm
    :directory "database-manager")

  (start-component :am
    :directory "agenda-manager")

  (start-component :kp
    :directory "ks-platform")

  (when (is-get-config-entry :micro-exec)
    (start-component :micro-exec
      :directory "exec"))

  (when (is-get-config-entry :world)
    (start-component :world
      :directory "world"))

  )

(defun start-component (name &rest initargs)
  (let ((c (apply #'new-component name initargs)))
    ;; Initialize-component sets up windows, etc, then assigns a
    ;; new run-function that performs all subsequent activity.
    (setf (pprocess-run-function c) #'initialize-component
	  (pprocess-status c) :run)
    c))


;;; Component initialization

;;; Component is a subclass of pprocess.

;;; Initialization can exploit the cyclic scheduling of the pseudo-process
;;; mechanism: each pprocess is run once per cycle and always in the
;;; same order (except where one process calls another as a subroutine).
;;; Consequently, when a pprocess is running it can get away with assuming
;;; that all other runnable pprocesses will run before it gets to run again.
;;; (This mattered back when we had (pseudo-) sockets, because a pprocess
;;; that wanted to send a message via a socket needed to know that the
;;; socket would exist and that the intended recipient would be attached
;;; to the socket.)

;;; Components start with status :run so that they can initialize
;;; themselves.  Initialization happens on the first cycle.  (When
;;; sockets are used, all sockets are created during this cycle, but
;;; no messages are sent.)  Initialization also changes the component's
;;; run-function and status.  After the first cycle, all components 
;;; have status :run-on-event and a run-function that will handle one
;;; or more events (usually only one), then return.  These run-functions
;;; no longer depend on cyclic scheduling.

;;; Some pprocesses take input from streams as well as handling ipc events.
;;; For instance, the TA listens for commands (menu selections) typed by the
;;; user.  The pprocess-input-streams mechanism allows this to be done without
;;; (explicit) polling.  (The implementation might end up polling if a true
;;; select function is not available, but this is not visible at the level of
;;; pprocesses.)

(defun initialize-component (self)

  (dev-set-debug-level *component-initial-debug-level*)

  ;; We don't need separate component patch directories in the 
  ;; single-process version, and it's simpler and faster not to
  ;; have them.
  ; (load-component-patches self)
  (call-component-startup self))

(defun call-component-startup (self)
  (funcall
    (concat-name (component-name self) '-startup)))

#+:undef
(defun load-component-patches (self)
  ;; N.B. Would load KP patches twice if there were two knowledge platforms.
  (is:is-patch-loader (component-directory self)))

;;; End
