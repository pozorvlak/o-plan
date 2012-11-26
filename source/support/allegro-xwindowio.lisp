;;;; File: allegro-xwindowio.lisp
;;; Contains: Allegro code for using X windows for IO.
;;; Author: Jeff Dalton
;;; Created: April 1994
;;; Updated: Mon May 12 04:22:38 1997 by Jeff Dalton
;;; Copyright: (c) 1994, AIAI, University of Edinburgh

(in-package :oplan-util)

;;; *xterm-debug* is a stream for debugging output produced when
;;; trying to start xterms.  It's normally bound to a stream that
;;; discards all output, obtained by calling make-broadcast-stream
;;; with no arguments.

(defparameter *xterm-debug* (make-broadcast-stream))

(defparameter *xterm-name* "xterm")

(defun set-term-modes (tty-name)
  ;; Note that the redirection (> or <) depends on the version of stty.
  #-solaris
  (system (concat-string "stty sane > " tty-name))
  #+solaris
  ;(system (concat-string "stty sane < " tty-name))
  (system (concat-string "/usr/ucb/stty sane > " tty-name)))

(defun run-xterm-for-io (&optional (args ""))
  "Opens a window for input and output. 
   Arguments:
     args - A string or list containing extra args for xterm.
   Returns:
     A bidirectional stream, and the device name of the tty device."

  (prog (pseudo-term pseudo-term-letter pseudo-term-number
	 io-term io-term-name)
    ;; Find a pseudo terminal to use.
    RETRY
    (block found-a-term
      (dolist (letter '("p" "q" "r"))
	(dolist (number '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" 
			  "a" "b" "c" "d" "e" "f"))
	  (setq pseudo-term
		(ignore-errors
		  (open (concatenate 'string "/dev/pty" letter number)
			:class 'stream:bidirectional-terminal-stream
			:direction :io
			:if-exists :append
			:if-does-not-exist nil)))
	  (when pseudo-term
	    (setq pseudo-term-letter letter)
	    (setq pseudo-term-number number)
	    (setq io-term-name
		  (concatenate 'string "/dev/tty" letter number))
	    (return-from found-a-term))))
      (error "Can't find a free pseudo terminal."))

    (format *xterm-debug* "~&Using /dev/pty~A~A, fd ~A; "
	    pseudo-term-letter pseudo-term-number
	    (excl:stream-output-fn pseudo-term))

    (format *xterm-debug* "~&Stream: ~S~%" pseudo-term)

    ;; Run xterm
    (multiple-value-bind (pid)
	(run-by-hand
	  *xterm-name*
	  (concatenate 'simple-vector
	     (list
	       (concatenate 'string "-S"
		pseudo-term-letter
		pseudo-term-number
		(prin1-to-string (excl:stream-output-fn pseudo-term))))
	     (if (listp args) args (break-args args))))
      (format *xterm-debug* "xterm pid = ~S.~%" pid)
      (register-xterm-pid pid))

    #+:undef
    (close pseudo-term)			;child process should have it by now

    (setq io-term
	  (open io-term-name
		:class 'stream:bidirectional-terminal-stream
		:direction :io
		:if-exists :append))
    
    ;; Set terminal modes
    (set-term-modes io-term-name)
    
    ;; Wait for some gumpf from xterm, so that we know that it has started ok.
    (format *xterm-debug* "Trying to read...~%")
    (read io-term nil :eof)
    
    ;; Success!
    (close pseudo-term)			;/\/ moved from above
    ; (set-term-modes io-term)
    (return
      (values io-term io-term-name))))


;;; Fork/exec

;;; /\/: We need to run xterm by hand, because run-shell-command closes
;;; all the fds above (I think) 2, and we need to leave one open for xterm.
;;; Versions of Allegro before 4.3 did not do this, but they had various
;;; bugs that make it too much trouble to get O-Plan working.

(ff:defforeign 'fork :arguments '() :return-type :integer)

(ff:defforeign 'execvp              
    :arguments '(string (simple-array simple-string (*)))
    :return-type :integer)

(defun run-by-hand (file argv)
  (let ((pid (fork)))
    (cond ((= pid -1)
	   (error "fork failed"))
	  ((= pid 0)
	   ;; child
	   ; (format t "~&Exec ~S ~S~%" file argv)
	   (execvp file
		   (concatenate 'simple-vector
		       (list file) argv (list 0)))
	   (format t "~%~%~%Exec of ~S failed.~%Argv:~S~%~%~%" file argv)
	   (exit-lisp -1))
	  (t
	   ;; parent
	   ; (format t "~&child pid = ~S~%" pid)
	   pid))))


;;; Child process operations

;;; Record of xterm pids

(defvar *xterm-pids* '())

(defun register-xterm-pid (pid)
  (assert (not (member pid *xterm-pids*)))
  (setq *xterm-pids* (nconc *xterm-pids* (list pid))))

;;; The xterms exit if they get an INT signal.  HUP isn't enough on Suns.

(defun terminate-xterms-if-necessary ()
  (let ((live-pids (remove-if-not #'still-running-p *xterm-pids*)))
    (when live-pids
      (system
        (format nil "kill -INT ~{~D~^ ~}" live-pids)))))


;;; (WAITPID-NO-HANG pid) returns (pid . status) or (0 . ?) if
;;; the status is not yet available.  (-1 . ?) indicates an error
;;; of some sort, including the case where a signal was received
;;; by the calling process.
;;;
;;;   int waitpid(pid, statusp, options)
;;;   int pid;
;;;   int *statusp;
;;;   int options;
;;;
;;; From <sys/wait.h>:
;;;
;;;   #define WNOHANG         1       /* dont hang in wait */
;;;

(export '(still-running-p))

(defun still-running-p (pid)
  (eql 0 (sys:os-wait t			;nohang = t
		      pid)))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
