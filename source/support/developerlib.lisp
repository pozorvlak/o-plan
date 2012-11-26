;;;; File: developerlib.lsp
;;; Contains: Routines for development use.
;;; Author: Richard Kirby (rbk)
;;; Created: Thu Feb  1 13:59:27 1990
;;; Updated: Thu Nov 14 00:55:26 1996 by Jeff Dalton
;;; Copyright: (c) 1992, AIAI, University of Edinburgh

;;;; /\/: Much of this code is obsolete, though sometimes still used.

(in-package :oplan-developerlib :nicknames '(:dev :oplan-dev))

(use-package :oplan-util)
(use-package :oplan-ipc)

(export '(dev-redirect-errors
	  dev-set-debug-level
	  *dev-debug-level*
	  dev-format
	  dev-debug
	  dev-note
	  dev-level-case
	  dev-error
	  dev-warn
	  *dev-warn-action*))


;;;; Global variables

(defvar *dev-debug-level* 10
  "A number representing the current debug level.")

(eval-when (eval compile load)

  (defvar *dev-debug-levels* (make-hash-table)
    "A hash table of debug level keywords -> numbers.")

  (mapc #'(lambda (entry)
	    (destructuring-bind (level value) entry
	      (setf (gethash level *dev-debug-levels*) value)))
	'((:all            10)
	  (:everything     10)		;control panel setting
	  (:detail          8)		;control panel setting
	  (:information     7)
	  (:debug           7)		;from monitors
	  (:trace           6)		;control panel setting
	  (:note            6)
	  (:minimal         5)		;control panel setting
	  (:warning         2)		;control panel setting
	  (:non-fatal-error 2)
	  (:error           1)		;control panel setting
	  (:fatal-error     1)
	  (:emergency       1)
	  (:user-request    1)
	  (:nothing         0)		;control panel setting
	  (:force-out       0)
	  (:none            0))))


;;;; Initialisation

(defun dev-redirect-errors (stream)
  "Changes the various error, trace and output streams."
  (setq lisp:*debug-io* stream)
  (setq lisp:*trace-output* (make-synonym-stream 'lisp:*debug-io*))
  (setq lisp:*error-output* (make-synonym-stream 'lisp:*debug-io*)))


;;;; Utilities

;;; Contains functions and macros used by the following sections.

(eval-when (eval compile load)

  (defun dev-level-name->number (name)
    (let ((level-number (gethash name *dev-debug-levels*)))
      (if (typep level-number 'fixnum)
	  level-number
        (error "Invalid dev-debug level: ~S" name))))
)

;;;; Debugging

(defun dev-set-debug-level (level)
  (when (symbolp level)
    (setq level (dev-level-name->number level)))
  (setq *dev-debug-level* level))


;;; Dev-format is the primitive level-checking output operation.
;;; Unlike dev-debug and dev-note, dev-format does not prefix the
;;; message with a module name, nor does it automatically add a
;;; newline at the end.
;;;
;;; Dev-format is a macro rather than a function so that the level
;;; test will be in-line and so that none of the arguments will be
;;; evaluated if the level test fails.  We'd like it to be as fast
;;; as possible when no output will occur so that it can be used in
;;; places where efficiency matters.
;;;
;;; The level name is converted to a number when the macro is expanded,
;;; so it must be a known name, not an expression that evaluates to a
;;; known name.

(defmacro dev-format (level message &rest format-args)
  (if (eq level :fatal-error)
      `(dev-fatal-error ,message ,@format-args)
    (let ((level-number (dev-level-name->number level)))
      `(when (fix<= ,level-number
		    *dev-debug-level*)
	 (xp-format *trace-output* ,message ,@format-args)
	 (force-output *trace-output*)))))


;;; dev-debug is the standard way to output at a particular debug level.

(defmacro dev-debug (level message &rest format-args)
  `(dev-note (ipc-whoami) ,level ,message ,@format-args))


;;; dev-note is like dev-debug but allows the source module-name to
;;; be specified.  So dev-note is used when the name of the currrent
;;; process won't do as the module name.

(defmacro dev-note (source level message &rest format-args)
  ;; /\/: Maybe want a fresh-line ("~&") at the start.
  ;; /\/: Used to use "~A: ~@?~&" instead of the concat-string.
  `(dev-format ,level ,(concat-string "~A: " message "~&")
	       ,source
	       ; ,message
	       ,@format-args))


;;; Sometimes you want different output for different debug levels.
;;; Dev-level-case is the way to do it.  The clause for the most
;;; detailed level that's allowed to output (because the numeric
;;; value of its level is <= *dev-debug-level*) will be selected.
;;; Other clauses will have no effect.

;;; Syntax: (dev-level-case (<level name> <form> ...) ...)

;;; Within the forms, calls to dev-format, dev-debug, and dev-note
;;; are written without specifying a level.  Instead, the level is
;;; taken automatically from the clause that contains the form.

;;; Clauses can be written in any order; they are sorted to put
;;; clauses for more detailed (higher numbered) levels 1st.  

;;; Example:

;;;   (dev-level-case
;;;     (:detail  (dev-debug "Low-level details"))
;;;     (:trace   (dev-debug "The usual, basic information"))
;;;     (:mimimal (dev-debug "Hardly anything.")))

(defmacro dev-level-case (&rest clauses)

  ;; Minimal syntax check.
  (dolist (c clauses)
    (unless (and (consp c) (keywordp (car c)))
      (error "Bad dev-level-case clause: ~S." c))
    ;; /\/: Disallow all the levels that aren't about how much detail to show?
    (let ((level (car c)))
      (when (eq level :fatal-error)
	(error "Level ~S not allowed in dev-level-case." level))))

  ;; Sort clauses to put more detailed levels 1st.
  (let ((sorted-clauses
	 (stable-sort (copy-list clauses)
		      #'>
		      :key #'(lambda (clause)
			       (dev-level-name->number (car clause))))))

    ;; Check that there aren't separate clauses for the same
    ;; numeric level.
    (for-adjacent-elements
       #'(lambda (a b)
	   (when (= (dev-level-name->number (car a))
		    (dev-level-name->number (car b)))
	     (error "Two dev-level-case clauses for the same numeric level:~%~
		     ~S, and~%~S." a b)))
       sorted-clauses)

    ;; Expand with locally redefined output macros.
    `(macrolet ((dev-debug (message &rest format-args)
		  `(dev-note (ipc-whoami) ,message ,@format-args))
		(dev-note (source message &rest format-args)
		  `(dev-format ,(concat-string "~&~A: " message "~&")
			       ,source
			       ,@format-args))
		(dev-format (message &rest format-args)
		  `(progn (xp-format *trace-output* ,message ,@format-args)
			  (force-output *trace-output*))))
       (cond
	     ,@(mapcar #'expand-dev-level-case-clause
		       sorted-clauses)))))

(defun expand-dev-level-case-clause (clause)
  (let ((level (car clause))
	(body (cdr clause)))
    ;; Construct a cond clause
    `((fix<= ,(dev-level-name->number level)
	     *dev-debug-level*)
      ,@body)))


;;; Dev-fatal-error is always called for dev-debug messages when the
;;; level is :fatal-error.  It tries to make sure the user, or the code
;;; that's using O-Plan in subroutine mode, notices the error.  Dev-
;;; fatal-error is not normally called directly; instead, it's used via
;;; one of the macros above.

(defvar *direct-user-to-fatal-error* t)

(defun dev-fatal-error (message &rest format-args)
  (cond ((get-parameter :interactive)
	 (apply #'xp-format *trace-output* message format-args)
	 (force-output *trace-output*)
	 (when *direct-user-to-fatal-error*
	   (ecase (menu-request
		    `("-heading"
		      ,(format nil "~A printed an error message" (ipc-whoami))
		      "I've seen it=:ok"
		      "Don't bother me again=:get-lost"))
	     (:ok)
	     (:get-lost
	      (setq *direct-user-to-fatal-error* nil)))))
	(t
	 (error "Error in ~A: ~?" (ipc-whoami) message format-args))))


;;; Errors

(defun dev-error (ident msg)
  "Just some sugar for the call to error."
  (error "~A: ~A~&" ident msg))


;;; A kind of warning

;;; /\/: It's the call to dev-debug below that requires the various
;;; eval-whens above.

(defvar *dev-warn-action* nil)

(defun dev-warn (message &rest format-args)
  (case *dev-warn-action*
    ((nil)
     )
    ((:dev-warn)
     (dev-debug :warning "~?" message format-args))
    ((:warn)
     (warn "~A: ~?" (ipc-whoami) message format-args))
    ((:break)
     (break "~A: ~?" (ipc-whoami) message format-args))))

;;;; ---------------------------- Change History ----------------------------
;;;; (Initials) (Date of change)           (Comment)
