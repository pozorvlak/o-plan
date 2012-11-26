;;;; File: components.lsp
;;; Contains: Definition of the component type
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: February 1993
;;; Updated: Sun Feb 28 19:55:55 1999 by Jeff Dalton
;;; Copyright: (c) 1992, 1993, AIAI, University of Edinburgh

;;; Component defintion

(in-package :oplan-components)	;/\/ do we need/want a separate package?

(use-package :oplan-developerlib)
(use-package :pprocess)
(use-package :oplan-ipc)

(export '(component
	  *component-specials*
	  *component-initial-debug-level*
	  new-component
	  component-name
	  component-p
	  component-status
	  component-run-function
	  component-specials
	  component-directory))

(export '(*pprocess*))

(defvar *component-specials*
  '(lisp:*debug-io*
    lisp:*trace-output* 
    lisp:*error-output*
    oplan-dev:*dev-debug-level*
    oplan-ctxt:*context*
    ))

(defvar *component-initial-debug-level* :minimal) ;should match control panel

(defstruct (component (:constructor %make-component)
		      (:print-function print-pprocess)
		      (:include pprocess
			 (specials *component-specials*)))
  (directory (error "No directory for component."))
  )

(defun new-component (name &rest other-initargs)
  (let ((c (apply #'make-a-pprocess #'%make-component
		  :name name other-initargs)))
    (register-pprocess c)
    (install-component-default-message-handlers c)
    c))

(defun install-component-default-message-handlers (c)
  (let ((c-name (component-name c)))
    (dolist (message-id (ipc-list-all-handled-messages :COMPONENT-DEFAULT))
      (unless (ipc-exists-handler c-name message-id)
	; (format t "~&defining ~S for ~S~%" message-id c-name)
	(setf (ipc-get-handler c-name message-id)
	      (ipc-get-handler :COMPONENT-DEFAULT message-id))))))


;;; Default message handlers

(defmessage (:COMPONENT-DEFAULT :DEBUG-LEVEL) (level)
  (dev-set-debug-level level))

(defmessage (:COMPONENT-DEFAULT :DIE-DIE-DIE) ()
  (ipc-terminate))

(defmessage (:COMPONENT-DEFAULT :SINGLE-STEP) (value)
  (declare (ignore value))
  nil)

(defmessage (:COMPONENT-DEFAULT :INIT) ()
  (break "?")
  :OK)

(defmessage (:COMPONENT-DEFAULT :INTERRUPT) ()
  nil)

;;; End
