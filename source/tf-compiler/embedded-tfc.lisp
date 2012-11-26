;;;; File: embedded-tfc.lsp
;;; Contains: Top-level for the embedded TF syntax checker (tfc)
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: 03 March 1994
;;; Updated: Wed Apr 21 20:08:27 1999 by Jeff Dalton
;;; Copyright: (c) 1994, AIAI, University of Edinburgh

(in-package :oplan-tf-compiler)

;;; Tfc is a TF syntax-checker ("c" for "check").  Actually, it calls
;;; the full TF compiler so that you get "semantic" checks as well.
;;; Tfc does not normally have an executable image of it's own.
;;; Instead, it's built into the O-Plan image and is invoked by the
;;; "-tfc" command-line argument.  All arguments after "-tfc" are
;;; interpreted by Tfc, not by  O-Plan.
;;;
;;; Tfc can therefore be called from the Unix shell by one of the following:
;;;
;;;   oplan -tfc [-q] [-l [-e]] filename
;;;   oplan -tfc [-q] [-l [-e]] -d directory
;;;
;;; "-q" is for "quiet" and suppresses progress reports.
;;; "-l" is for "levels" and produces a level analysis.
;;; "-e" can be used with "-l" to have the level analysis
;;;      list the effect names for each action (to make it
;;;      easier to see where the effect-levels come from).
;;;      This does not include the effects of subactions.
;;;
;;; If the filename is the single character "-", TF forms will be
;;; read from standard-input.  Progress reports will be turned off
;;; automatically.
;;;
;;; If no additional arguments are supplied after "-tfc", Tfc
;;; enters a loop in which you can type Tfc commands.  E.g
;;;
;;;    shell% oplan -tfc
;;;    tfc> -q -l snark.tf
;;;    tfc>
;;;
;;; To exit, type "q" (not "-q") as a command.
;;;
;;; There's also a tfc function that can be called from the O-Plan
;;; Lisp interaction window.  /\/: Does this work?
;;;

;;; Entry point from the O-Plan top-level

(defun run-as-tfc (arg-offset)
  ;; First some required setup
  ;; [None at present.]
  ;; Now we can actually run
  (let ((args (argv->arglist arg-offset)))
    (if args
	(apply #'tfc args)
      (tfc-top-level))
    (oplan::exit-oplan)))

(defun argv->arglist (offset)
  ;; /\/: Can't use argc, because Lucid doesn't have one.
  (loop for i from offset while (argv i)
	collect (argv i)))


;;; A top-level

(defun tfc-top-level ()
  (loop
    (princ "tfc> ")
    (let ((args (break-args (read-line))))
      (when (string= (first args) "q")
	(return))
      (apply #'tfc args))
    (terpri)))


;;; The tfc function

(defun tfc (&rest args)
  (tf-compiler-init)
  (let ((*running-as-tfc* t)		;if anyone cares
	(*print-case* :downcase)
	(*print-pretty* t)
	(*package* (find-package :oplan))
	(*progress-reports* t)
	(*level-report* nil)
	(*level-report-include-action-effects* nil)
	(directory nil)
	(filename nil)
	(stdin nil)
	(i 0))

    ;; Process args
    (labels ((arg () (nth i args))
	     (pop-arg () (prog1 (arg) (incf i))))
      (when (string-equal (arg) "-q")
	(setq *progress-reports* nil)
	(pop-arg))
      (when (string-equal (arg) "-l")
	(setq *level-report* t)
	(pop-arg))
      (when (string-equal (arg) "-e")
        (setq *level-report-include-action-effects* t)
	(pop-arg))
      (cond ((string-equal (arg) "-d")
	     (pop-arg)
	     (setq directory (pop-arg)))
	    ((string-equal (arg) "-")
	     (pop-arg)
	     (setq *progress-reports* nil)
	     (setq stdin t))
	    (t
	     (setq filename (pop-arg))))
      (unless (null (arg))
	(usage-error)
	(return-from tfc)))

    ;; Do as requested.
    (cond (directory (tfc-directory directory))
	  (filename (tfc-file filename))
	  (stdin (tfc-stdin))
	  (t (usage-error)))
    
    (values)))

(defun usage-error ()
  (tfc-error
    "Usage: tfc [-q] [-l [-e]] filename~%~
        ~6t tfc [-q] [-l [-e]] -d directory~%"))

(defun tfc-error (message &rest format-args)
  (format *error-output* "~&~?~%" message format-args))

(defun tfc-summary (n-errors n-warnings)
  (format *error-output* "~&~S error~:p ~S warning~:p~%" n-errors n-warnings))

(defun tfc-file (filename)
  (let ((path (merge-pathnames filename (make-pathname :type "tf"))))
    (if (probe-file path)
	(multiple-value-bind (result n-errors n-warnings)
	    (compile-from-tf-file path)
	  (declare (ignore result))
	  (tfc-summary n-errors n-warnings))
      (tfc-error "Can't find file ~S" (namestring path)))))

(defun tfc-stdin ()
  (multiple-value-bind (result n-errors n-warnings)
      (compile-tf-stream *standard-input*)
    (declare (ignore result))
    (tfc-summary n-errors n-warnings)))

(defun tfc-directory (directory)
  (let ((path (pathname (directory-name directory))))
    (if (probe-file path)
	(compile-test-files path :messages t)
      (tfc-error "Can't find directory ~S" (namestring path)))))

(defun directory-name (name)
  (if (eql (char name (1- (length name))) '#\/)
      name
    (concatenate 'string name "/")))


(defun tfc-compile-tf-stream-error-handler (c) ;see compile-tf-stream
  (declare (ignore c))
  (clear-input *standard-input*)	;in case we're doing tfc-stdin
  nil)					;let whatever happens, happen.


;;; End

