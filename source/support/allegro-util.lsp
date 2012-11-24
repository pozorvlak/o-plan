;;;; File: allegro-util.lsp
;;; Contains: Allegro-specific utilities
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: April 1994
;;; Updated: Fri Apr 16 18:05:03 1999 by Jeff Dalton
;;; Copyright: (c) 1994, AIAI, University of Edinburgh

;;; Some Allegro-specific utilities

;;; A version of this file should exist for each supported CL.

;;; Thanks to George Ferguson <ferguson@cs.rochester.edu> for help
;;; with the Allegro version of map-structure and to bugs@franz.com
;;; for confirming that run-shell-command closed fds and hence was
;;; unsuitable for running our xterms.

(in-package :oplan-util)

(export '(signal-error))


;;; In Allegro, signal-error is just another name for error.  In
;;; implementations that do not have the CL condition system built in,
;;; it may be something else.

(defun signal-error (datum &rest arguments)
  (apply #'error datum arguments))


;;; INT-CHAR, because X3J13 deleted it and Allegro tries to follow
;;; the proposed standard.  Fortunately, Allegro retained INT-CHAR
;;; in a backwards-compatibility package.

(defun int-char (i)
  (cltl1:int-char i))

(defun get-setf-method (place)
  (cltl1:get-setf-method place))


;;; Random Unix stuff

(defun argc ()
  (system:command-line-argument-count))

(defun argv (n) ; -> string or nil
  (if (< n (argc))
      (system:command-line-argument n)
    nil))

(defun exit-lisp (&optional (status 0))
  (excl:exit status :quiet t))

(defun getenv (string) ; -> string or nil
  (system:getenv string))

(defun working-directory-pathname ()
  (excl:current-directory))

(defun change-working-directory (namestring)
  (excl:chdir namestring))


;;; System -- runs a shell command given as a string.

(defun system (cmd)
  (excl:shell cmd))


;;; Saving an executable image

;;; /\/: When the saved images runs, certain standard variables are
;;; set to default values.  This happens even though we're using
;;; *restart-app-function*.  Since we do not always want the standard
;;; default values, we wrap the top-level-fn in some code that restores
;;; the values that were in force when the image was created.  It
;;; might be better to have some explicit way of establishing "O-Plan"
;;; values, but this is the easiest way to get things going in Allegro,
;;; because we can keep setting values the way we have been for other
;;; Common Lisps.

#+:allegro-v5.0
(defun save-image (filename top-level-fn)
  (excl:gc nil)
  (excl:gc nil)
  (excl:gc t)
  (setq excl:*restart-app-function*
	(wrap-for-allegro-restart top-level-fn))
  (setq excl:*read-init-files* nil)			;probably right /\/
  (setq excl:*print-startup-message* nil)
  (tpl:setq-default excl:*print-nickname* nil)
  (excl:dumplisp
    :name filename
    :ignore-command-line-arguments nil
    :suppress-allegro-cl-banner t))

#-:allegro-v5.0
(defun save-image (filename top-level-fn)
  (excl:gc nil)
  (excl:gc nil)
  (excl:gc t)
  (setq excl:*restart-app-function*
	(wrap-for-allegro-restart top-level-fn))
  (setq excl:*read-init-files* nil)			;probably right /\/
  (setq excl:*print-startup-message* nil)
  (excl:dumplisp
    :name filename
    :checkpoint nil
    :ignore-command-line-arguments t))

(defun wrap-for-allegro-restart (f)
  (let ((alist '()))
    (do-external-symbols (s (find-package '#:common-lisp))
      (when (and (boundp s)
		 (not (constantp s))
		 (not (streamp (symbol-value s))))
	(push (cons s (symbol-value s)) alist)))
    #'(lambda (&rest whatever)
	(dolist (binding alist)
	  (set (car binding) (cdr binding)))
	(apply f whatever))))


;;; Pretty printer

(setf (symbol-function 'xp-format) #'format)

(defun set-pretty-printer (type-specifier fn)
  (set-pprint-dispatch type-specifier fn))


;;; Interface to xmenu.

;;; It looks like LCL knows to wait (eventaully) for programs run
;;; with :wait nil.  Keeps a record of the child processes, perhaps?
;;; But we need an explicit wait in Allegro.

(defvar *xmenu-pathname* nil)

(defun menu-request (xmenu-args &key (read-function #'read))
  (ensure-xmenu-pathname)
  (multiple-value-bind (process-stream error-stream pid)
      (excl:run-shell-command
         (concat-exec-command *xmenu-pathname*
			      (apply-xmenu-defaults xmenu-args))
	 :output :stream
	 :input nil
	 :error-output nil
	 :wait nil)
    (declare (ignore error-stream))
    (unwind-protect
	 (funcall read-function process-stream)
      (close process-stream :abort t)
      (unix-process-finish pid))))

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
  (multiple-value-bind (io-stream error-stream pid)
      (excl:run-shell-command
         (concat-shell-command program-name args)
	 :input :stream
	 :output :stream
	 :error-output nil
	 :wait nil)
    (assert (eq error-stream nil))
    (values
      io-stream
      pid)))

;;; Unix-process-finish is called when a child process should have
;;; terminated.  In some Lisps, it has to call wait.

(defun unix-process-finish (pid)
  (sys:os-wait nil			;nohang = nil
	       pid))


;;; Unlike the Lisps in which O-Plan started out, Allegro CL's
;;; function for running a Unix process expects the entire command as
;;; a single string rather than having one string that's the program
;;; name, plus a list of argument strings.  So we have to take all of
;;; those strings and produce a single string from them.  This is
;;; difficult to get right, since we may need to add quotes or whatever.
;;; So we don't try to get it perfectly right, just to do something
;;; that will probably work for every case we actually try, namely to
;;; put double quotes around all arguments.

(defun concat-shell-command (program-name args)
  (apply #'concatenate 'string
	 "exec "
	 program-name
	 (mapcan #'(lambda (arg)
		     ;; space open-" arg close-"
		     (list " \"" arg "\""))
		 args)))

;;; /\/: At least in Allegro 4.3, we can use a simple vector instead.
;;; The 0th elt of the vector and the rest of the vector will be used
;;; as the two args to execvp.

;;; /\/: Unfortunately, Lisp sometimes dies horribly if we use this.

(defun concat-exec-command (program-name args)
  (concatenate 'simple-vector (list program-name program-name) args))


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;

