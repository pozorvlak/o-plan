;;;; File: lucid-xwindowio.lisp
;;; Contains: LCL code for using X windows for IO.
;;; Author: Jeff Dalton
;;; Created: February 1993
;;; Updated: Tue Apr 22 17:00:26 1997 by Jeff Dalton
;;; Copyright: (c) 1993, AIAI, University of Edinburgh

(in-package :oplan-util)

;;; *xterm-debug* is a stream for debugging output produced when
;;; trying to start xterms.  It's normally bound to a stream that
;;; discards all output, obtained by calling make-broadcast-stream
;;; with no arguments.

(defvar *xterm-debug* (make-broadcast-stream))

(lcl:def-foreign-function (unix-open (:name "_open")) path flags mode)

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
  
  (declare (type (or string list) args))
  
  (prog (pseudo-term pseudo-term-letter pseudo-term-number
	 io-term io-term-name)
    ;; Find a pseudo terminal to use.
    RETRY
    (block found-a-term
      (dolist (letter '("p" "q" "r"))
	(dolist (number '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" 
			  "a" "b" "c" "d" "e" "f"))
	  (let ((pty-fd
		 (unix-open (concatenate 'string
			       "/dev/pty" letter number)
			    2		;rw
			    0)))
	    (when (> pty-fd 0)
	      (setq pseudo-term
		    (lcl:make-lisp-stream :input-handle pty-fd
					  :output-handle pty-fd))
	      (setq pseudo-term-letter letter)
	      (setq pseudo-term-number number)
	      (setq io-term-name
		    (concatenate 'string "/dev/tty" letter number))
	      (return-from found-a-term)))))
      (error "Can't find a free pseudo terminal."))

    (format *xterm-debug* "~&Using /dev/pty~A~A; "
	    pseudo-term-letter pseudo-term-number)

    ;; Run xterm
    (multiple-value-bind (comms-stream error-stream exit-status pid)
	(lcl:run-program "xterm"
	   :arguments
	     `(,(concatenate 'string "-S"
		 pseudo-term-letter
		 pseudo-term-number
		 (prin1-to-string
		  (lcl:extract-stream-handle pseudo-term :output)))
	       ,@(if (listp args) args (break-args args)))
	   :wait nil)
      (declare (ignore comms-stream error-stream exit-status))
      (register-xterm-pid pid)
      (format *xterm-debug* "xterm pid = ~S.~%" pid))

    (close pseudo-term)			;child process should have it by now

    (let ((tty-fd (unix-open io-term-name 2 0)))
      (when (= tty-fd -1)
	(error "Can't open ~S." io-term-name))
      (setq io-term
	    (lcl:make-lisp-stream :input-handle tty-fd
				  :output-handle tty-fd
				  :auto-force t)))

    ;; Set terminal modes
    (set-term-modes io-term-name)
    
    ;; Wait for some gumpf from xterm, so that we know that it has started ok.
    (read io-term nil :eof)
    ;; Success!
    ; (set-term-modes io-term)
    (return
      (values io-term io-term-name))))


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
;;; Of course, it's different in Solaris 2.

; (in-package :oplan-util)

(export '(waitpid-no-hang still-running-p))

(defconstant WNOHANG #-solaris 1 #+solaris #o100)

(lcl:def-foreign-function (%waitpid
			    (:language :c)
			    (:name "_waitpid")
			    (:return-type :signed-32bit))
  (pid :signed-32bit)
  (statusp (:pointer :signed-32bit))
  (options :signed-32bit))

(defun waitpid-no-hang (pid)
  (let ((statusp (lcl:make-foreign-pointer :type '(:pointer :signed-32bit))))
    (let ((result
	   (%waitpid pid statusp WNOHANG)))
      (cons result (lcl:foreign-value statusp)))))

(defun still-running-p (pid)
  (= 0 (car (waitpid-no-hang pid))))


;;; /\/: We should really do this in some central place.

(lcl:load-foreign-libraries nil)

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
