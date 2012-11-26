;;;; File: xwindowio.lisp
;;; Contains: Code for using X windows for IO.
;;; Author: Richard Kirby (rbk)
;;; Created: Fri Jan 12 17:25:00 1990
;;; Updated: Mon Mar 23 15:55:37 1998 by Jeff Dalton
;;; Copyright: (c) 1992, AIAI, University of Edinburgh

(in-package :oplan-xwindowio :nicknames '(:x :oplan-x))

(import 'dev:dev-redirect-errors)

(use-package :oplan-util)

#+(or Allegro Liquid)
(import 'oplan-util::int-char)

(export '(x-open-io-win
	  x-open-and-register-io-win
	  x-get-stream
          x-get-device-name
	  x-redirect-errors
	  clear-screen

	  ;; Some simple, standard menus
	  notify-user
	  ask-user-if

	  ;; Asking questions in a window
	  *persistent-query-window*
	  with-query-window
	  do-with-query-window
	  tell-user
	  query-user

	  ;; Low-level utilities
	  kill-xterm
	  newest-xterm-pid))

(defvar *x-register* nil
  "An assoc list of window symbolic ids agaisnt streams and device names.")

(defun x-open-io-win (&optional (args ""))
  "Opens a window for input and output. 
   Arguments:
     args - A string or list containing extra args for xterm.
   Returns:
     A bidirectional stream, and the device name of the tty device."
  (if (get-parameter :windows)
      (oplan-util:run-xterm-for-io args)
    (values *terminal-io* "no device")))

(defun x-open-and-register-io-win (tag &optional (args ""))
  "Opens an X window using x-open-io-win, and stores the stream against tag in
   *x-register*.
   Arguments:
     tag - symbolic name refering to the X window.
     args - optional arguments passed to the xterm command.
   Results:
     t or nil.
   Side Effects:
     An X IO Window will be opened, and a Lisp stream associated with it
     via the *x-register* table."
  
  (declare (type keyword tag)
	   (type (or string list) args))
  
  (multiple-value-bind (stream device) (x-open-io-win args)
    (setq *x-register* (acons tag (cons stream device)
			      *x-register*))
    t))

(defun x-get-stream (tag)
  "Returns the stream for the tag.
   Arguments:
     tag - symbolic name of the stream.
   Results:
     a stream or nil.
   Side Effects: NONE."
  (cadr (assoc tag *x-register*)))

(defun x-get-device-name (tag)
  "Returns the device name for tag.
   Arguments:
     tag - symbolic name of the stream.
   Results:
     a device name (as a string) or nil.
   Side Effects: NONE."
  (cddr (assoc tag *x-register*)))

(defun x-redirect-errors (what)
  "Syntactic sugar over dev-redirect-errors, which looks up the actual
   stream which the developerlib function requires."
  (let ((str (x-get-stream what)))
    (if (streamp str)
	(dev-redirect-errors str))))

(defun clear-screen (&optional (s *standard-output*))
  (if (get-parameter :windows)
      (clear-xterm s)
    ()))

(defun clear-xterm (&optional (s *standard-output*))
  (declare (type stream s))
  (write-char (int-char 27) s)
  (write-char #\[ s)
  (write-char #\H s)
  (write-char (int-char 27) s)
  (write-char #\[ s)
  (write-char #\2 s)
  (write-char #\J s)
  (force-output s))


;;;; Menus

(defun notify-user (message &rest format-args)
  (menu-request
    `("-heading"
      ,(apply #'format nil message format-args)
      "ok")))

(defun ask-user-if (message &rest format-args)
  (menu-request
    `("-heading"
      ,(apply #'format nil message format-args)
      "Yes=:yes"
      "No=:no")))


;;;; Asking questions in a window

;;; With-query-window can be used for simple questions and for
;;; questions that may require several exchanges so that an error
;;; message can be printed, etc.

;;; Ordinarily, tell-user and query-user should not be called
;;; except inside a with-query-window.

;;; /\/: There are a number of different ask-user routines in O-Plan.
;;; Need to generalize.  E.g. for each component, a query stream.
;;; Or maybe one window, controlled by the IM.  (You can imagine the
;;; query mechanism defined here as working that way.)

(defparameter *persistent-query-window* t)

(defvar *query-window* nil)
(defvar *query-window-pid* nil)

(defmacro with-query-window (&body forms)
  `(do-with-query-window #'(lambda () . ,forms)))

(defun do-with-query-window (thunk)
  (unwind-protect
      (funcall thunk)
    (when *query-window*
      (if *persistent-query-window*
	  (format *query-window* "~&~%-----~%~%")
	(kill-query-window)))))

(defun tell-user (message &rest format-args)
  (let ((io (ensure-query-window)))
    (apply #'format io message format-args)))

(defun query-user (question &rest format-args) ; -> string
  (let ((io (ensure-query-window)))
    (format io "~&~?" question format-args)
    (read-line io)))
	 
(defun ensure-query-window ()
  (ensuref *query-window*
    (let ((w (run-xterm-for-io
	       (list "-title" "Query window"
		     "-geometry" "80x5"))))
      ;; /\/: Assumes the newest xterm is the right one.
      (setq *query-window-pid* (newest-xterm-pid))
      (clear-screen w)
      w)))

(defun kill-query-window ()
  (when *query-window*
    (assert (integerp *query-window-pid*))
    (kill-xterm *query-window-pid*)
    (setq *query-window-pid* nil
	  *query-window* nil)))

(defun kill-xterm (pid)
  (assert (member pid oplan-util::*xterm-pids*))
  (when (still-running-p pid)
    (system (format nil "kill -INT ~D" pid)))
  (deletef pid oplan-util::*xterm-pids*)
  t)

(defun newest-xterm-pid ()
  (last-element oplan-util::*xterm-pids*))


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
