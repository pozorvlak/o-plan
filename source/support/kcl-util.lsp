;;;; File: kcl-util.lsp
;;; Contains: AKCL-specific utilities
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: February 1993
;;; Updated: Fri Feb  1 23:58:46 2008 by Jeff Dalton
;;; Copyright: (c) 1992, 1993, AIAI, University of Edinburgh

;;; Some AKCL-specific utilities

;;; A version of this file should exist for each supported CL.

(in-package :oplan-util)


;;; Random Unix stuff

(defun argc ()
  (si:argc))

(defun argv (n) ; -> string or nil
  (if (< n (si:argc))
      (si:argv n)
    nil))

(defun exit-lisp (&optional (status 0))
  (if (fboundp 'exit)
      (exit status)
    (bye status)))

#-no-c-code
(eval-when (compile load)
  (defentry exit (int) (void exit)))

(defun getenv (string) ; -> string or nil
  (si:getenv string))

(defun working-directory-pathname ()
  (truename "."))

(defun change-working-directory (namestring)
  (si:chdir namestring))


;;; (system shell-command) is provided by the LISP package.


;;; Saving an executable image

(defun save-image (filename top-level-fn)
  (setf (symbol-function 'si:top-level)
	top-level-fn)
  (si:save-system filename))


;;; Pretty printer

;;; Install Richard C. Waters' XP Pretty Printer.

(xp::install :package (find-package :oplan-util))

;;; Set xp-format to be the same as the xp format routine.

(setf (symbol-function 'xp-format) #'xp::format))

(defun set-pretty-printer (type-specifier fn)
  (xp::set-pprint-dispatch type-specifier fn))


;;; Interface to xmenu.

;;; Changes for 1.2 release:
;;;  * Fork:wait calls added.
;;;  * Calls our-run-process instead of si:run-process.
;;;  * Separate menu-request procedure.

(defvar *xmenu-pathname* nil)

#-no-c-code
(defun menu-request (xmenu-args &key (read-function #'read))
  (ensure-xmenu-pathname)
  (let ((process-stream (apply #'fork:process-receive
			       *xmenu-pathname*
			       (apply-xmenu-defaults xmenu-args))))
    (unwind-protect
	 (funcall read-function process-stream)
      (close process-stream :abort t)
      (fork:wait))))

(defun apply-xmenu-defaults (xmenu-args)
  (if (or (null (get-parameter :menu-geometry))
	  (member "-geometry" xmenu-args :test #'string=))
      xmenu-args
    (list*
      "-geometry" (get-parameter :menu-geometry)
      xmenu-args)))

(defun ensure-xmenu-pathname ()
  (or *xmenu-pathname*
      (setq *xmenu-pathname*
	    (concatenate 'string (get-parameter :oplan-dir) "/bin/xmenu"))))


;;; General function for getting an i/o stream for communication with
;;; a Unix process.

#-no-c-code
(defun unix-process-io (program-name &rest args)
  (multiple-value-bind (pid read-stream write-stream error-stream)
      (values-list
        (fork:process program-name
		      :args args
		      :input :stream
		      :output :stream))
    (declare (ignore error-stream))
    (values
      (make-two-way-stream read-stream write-stream)
      pid)))


;;; Unix-process-finish is called when a child process should have
;;; terminated.  In some Lisps, such as KCL, it calls wait.

#-no-c-code
(defun unix-process-finish (pid)
  (declare (ignore pid))
  (fork:wait))


;;; End
