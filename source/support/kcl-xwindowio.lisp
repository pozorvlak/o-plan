;;;; File: kcl-xwindowio.lsp
;;; Contains: Code for using X windows for IO.
;;; Author: Richard Kirby (rbk) and Jeff Dalton
;;; Created: Fri Jan 12 17:25:00 1990
;;; Updated: Mon Jan 24 18:17:55 2000 by Jeff Dalton
;;; Copyright: (c) 1992, 1993, AIAI, University of Edinburgh

(in-package :oplan-util)

(export '(xterm-stream-pid))

;;; *xterm-debug* is a stream for debugging output produced when
;;; trying to start xterms.  It's normally bound to a stream that
;;; discards all output, obtained by calling make-broadcast-stream
;;; with no arguments.

(defvar *xterm-debug* (make-broadcast-stream))

;; A readmacro for Clines.
(eval-when (compile)
  (set-macro-character #\%
     #'(lambda (stream char)
	 (declare (ignore char))
	 (values (read-line stream)))))

(Clines
 % static int fd_from_filedescr(lispstream)
 %   object lispstream;
 % {
 %   return(fileno(lispstream->sm.sm_fp));
 % }
)

(defentry fd-from-filedescr (object)
  (int fd_from_filedescr))

#+FreeBSD
(defun set-term-modes (tty-name)
  (system (concat-string "stty -f " tty-name " sane")))

#+(and sun bsd)
(defun set-term-modes (tty-name)
  (system (concat-string "stty > " tty-name " sane")))

#+:linux
(defun set-term-modes (tty-name)
  (system (concat-string "stty < " tty-name " sane")))


;;; Changes for 1.2 release:
;;;  * More error checks.
;;;  * Moved close of pseudo-term
;;;  * Uses FORK:PROCESS instead of SYSTEM.
;;;  * PROGified it so we can do (go FAIL) and (go RETRY).
;;;  * Waits for xterm.  For some reason, the READ was getting an EOF
;;;    instead of waiting until some output was produced, perhaps because
;;;    the xterm was exiting.  We now wait until input is available
;;;    (according to LISTEN) before calling READ and also check for
;;;    failed xterms.

;;; Note that a RETRY may use the same pseudo-terminal that was used
;;; before.  This seems to work (so far).  This suggests that we might be
;;; able to just rerun xterm without looking for a new pty.

;;; The following shell command is useful for seeing what's happening
;;; with xterm.  Substitite $OPLANTMPDIR for oplan-tmp.
;;;   gnugrep xterm oplan-tmp/*.out oplan-tmp/exec/*.out

;;; When debugging, note that you can run /aiai/bin/X.V11R5/xterm
;;; via "trace", and that "trace" can easily be exchanged for "env"
;;; as a quick way to turn off the trace output.  Here's what it
;;; would look like:

#|
    ;; Run xterm
    (setq xterm-pid
	  (car
	    (fork:process "env"		;or use "trace"
	      :args `("/aiai/bin/X.V11R5/xterm"
		      ,(concatenate 'string "-S"
			pseudo-term-letter
			pseudo-term-number
			(prin1-to-string (fd-from-filedescr pseudo-term)))
		      ,@(if (listp args) args (break-args args))))))
|#

(defun run-xterm-for-io (&optional (args ""))
  "Opens a window for input and output. 
   Arguments:
     args - A string or list containing extra args for xterm.
   Returns:
     A bidirectional stream, and the device name of the tty device."
  
  (declare (type (or string list) args))
  
  (prog (pseudo-term io-term pseudo-term-name io-term-name
         pseudo-term-letter pseudo-term-number status xterm-pid)
    ;; Find a pseudo terminal to use.
    RETRY
    (block found-a-term
      (dolist (letter '("p" "q" "r"))
	(dolist (number '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" 
			  "a" "b" "c" "d" "e" "f"))
	  (if (and (setq pseudo-term
			 (open (setq pseudo-term-name
				     (concatenate 'string
						  "/dev/pty"
						  letter 
						  number))
			       :direction :io
			       :if-exists :append
			       :if-does-not-exist nil))
		   (progn
		     (setq pseudo-term-letter letter)
		     (setq pseudo-term-number number)
		     (setq io-term-name
			   (concatenate 'string "/dev/tty"
					letter number))
		     (setq io-term
			   (open io-term-name :direction :io
				 :if-exists :append
				 :if-does-not-exist nil))))
	      (return-from found-a-term))))
      (error "Can't find a free pseudo terminal."))
    (set-term-modes io-term-name)
    (close io-term)

    (format *xterm-debug* "~&Using /dev/pty~A~A.  "
	    pseudo-term-letter pseudo-term-number)

    ;; Run xterm
    (setq xterm-pid
	  (car
	    (fork:process "xterm"
	      :args `(,(concatenate 'string "-S"
			pseudo-term-letter
			pseudo-term-number
			(prin1-to-string (fd-from-filedescr pseudo-term)))
		      ,@(if (listp args) args (break-args args))))))
    
    ;; (close pseudo-term)

    ;; For some reason the following doesn't work on the Suns.
    ;; Listen always returns true and read seems to read the output
    ;; that the planner sent (chiefly messages about how you should
    ;; type 1 or 8).  /\/
    ;; (setq io-term
    ;;       (open io-term-name :direction :io :if-exists :append))

    (setq io-term
	  (make-two-way-stream
	     (open io-term-name :direction :input)
	     (open io-term-name :direction :output :if-exists :append)))
    
    ;; Wait for some gumpf from xterm, so that we know that it has started ok.
    (let ((failures 0)
	  input)
      (loop
        (when (listen io-term)
	  (setq input (read io-term nil :xeof))
	  (unless (eq input :xeof)
	    (return)))
	(incf failures)
	(if (= failures 1)
	    (format *xterm-debug* "Waiting for xterm. ")
	  (format *xterm-debug* "."))
	(sleep 1)
	(setq status (fork:waitpid-no-hang xterm-pid))
	(when (= (car status) xterm-pid)
	  (go FAIL))))
    ;; Success!
    (format *xterm-debug* " Have xterm; pid = ~S.~%" xterm-pid)
    (close pseudo-term)			;<<< Moved from above
    (set-term-modes io-term-name)
    (register-xterm-pid xterm-pid io-term)
    (return
      (values io-term io-term-name))
    FAIL
    (format *xterm-debug* "~&xterm failed.~%")
    (format *xterm-debug* "~&xterm status: ~S.~%" status)
    (close pseudo-term)
    (close io-term)
    (go RETRY)))


;;; Child process operations

;;; Record of xterm pids

(defvar *xterm-pids* '())

(defvar *xterm-stream-to-pid-table* '())

(defun register-xterm-pid (pid io-term)
  (assert (not (member pid *xterm-pids*)))
  (setq *xterm-pids* (nconc *xterm-pids* (list pid)))
  (setq *xterm-stream-to-pid-table*
	(nconc *xterm-stream-to-pid-table*
	       (list (cons io-term pid))))
  t)

(defun xterm-stream-pid (stream)
  (lookup stream *xterm-stream-to-pid-table*))


;;; The xterms exit if they get an INT signal.  HUP isn't enough on Suns.

(defun terminate-xterms-if-necessary ()
  (let ((live-pids (remove-if-not #'still-running-p *xterm-pids*)))
    (when live-pids
      (system
        (format nil "kill -INT ~{~D~^ ~}" live-pids)))))

(defun still-running-p (pid)
  (let ((status (fork:waitpid-no-hang pid)))
    (= 0 (car status))))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
