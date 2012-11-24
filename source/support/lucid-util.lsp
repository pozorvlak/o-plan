;;;; File: lucid-util.lsp
;;; Contains: Lucid-specific utilities
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: February 1993
;;; Updated: Sun Feb 28 20:51:31 1999 by Jeff Dalton
;;; Copyright: (c) 1992, 1993, AIAI, University of Edinburgh

;;; Some Lucid-specific utilities

;;; A version of this file should exist for each supported CL.

(in-package :oplan-util)

#+:Liquid
(progn

  (export '(signal-error))

  (defun signal-error (datum &rest arguments)
    (apply #'error datum arguments))

  (defun condition-p (x) (typep x 'condition))

  (defun int-char (i)
    (lispworks:int-char i))

)


;;; Random Unix stuff

(defun argc ()
  (error "No argc"))

(defun argv (n) ; -> string or nil
  (lcl:command-line-argument n))

#-Liquid
(defun exit-lisp (&optional (status 0))
  (lcl:quit status))

#+Liquid
(defun exit-lisp (&optional (status 0))
  (lcl:quit :status status))

(defun getenv (string) ; -> string or nil
  (lcl:environment-variable string))	;settable in Lucid

(defun working-directory-pathname ()
  (lcl:working-directory))

(defun change-working-directory (namestring)
  (setf (lcl:working-directory) namestring))


;;; System -- runs a shell command given as a string.

(defun system (cmd)
  (lcl:run-program "sh" :arguments `("-c" ,cmd)))


;;; Saving an executable image

(defun save-image (filename top-level-fn)
  (lcl:disksave filename
    :restart-function top-level-fn
    :full-gc t
    :verbose t))


;;; Pretty printer

(setf (symbol-function 'xp-format) #'format)

(defun set-pretty-printer (type-specifier fn)
  (lcl:set-pprint-dispatch type-specifier fn))


;;; Interface to xmenu.

;;; It looks like LCL knows to wait (eventaully) for programs run
;;; with :wait nil.  Keeps a record of the child processes, perhaps?

(defvar *xmenu-pathname* nil)

(defun menu-request (xmenu-args &key (read-function #'read))
  (ensure-xmenu-pathname)
  (let ((process-stream
	 (lcl:run-program *xmenu-pathname*
			  :arguments (apply-xmenu-defaults xmenu-args)
			  :output :stream
			  :wait nil)))
    (unwind-protect
	 (funcall read-function process-stream)
      (close process-stream :abort t))))

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

(defun unix-process-io (program-name &rest args)
  (multiple-value-bind (io-stream error-stream exit-status pid)
      (lcl:run-program program-name
		       :arguments args
		       :input :stream
		       :output :stream
		       :wait nil)
    (declare (ignore error-stream exit-status))
    (values
      io-stream
      pid)))

;;; Unix-process-finish is called when a child process should have
;;; terminated.  In some Lisps, it calls wait.  In Lucid CL, that's
;;; handled automatically.

(defun unix-process-finish (pid)
  (declare (ignore pid))
  nil)


;;; End
