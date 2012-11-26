;;;; File: separate-world.lisp
;;; Contains: Code for starting a separate world simulator from the Exec.
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: November 1994
;;; Updated: Tue Mar 23 02:04:33 1999 by Jeff Dalton
;;; Copyright: (c) 1994, AIAI, University of Edinburgh

(in-package :oplan-world)

(defvar *world-io* nil)

(defun start-separate-world ()
  (setq *world-io* (run-separate-world))
  (new-connector :world-io
    :io-stream *world-io*
    :socket-to-read :worldin
    :socket-to-write :worldout
    :input-guard 'guard-rightin-from-separate-world)
  )

(defun run-separate-world ()
  (let ((shell-command (get-parameter :separate-world)))
    (assert (stringp shell-command))
    (destructuring-bind (cmd &rest args) (break-args shell-command)
      (apply #'ipc-run-agent
	 :world
         (if (find #\/ cmd)
	     cmd
	   (concat-string (get-parameter :oplan-dir) "/bin/" cmd))
	 "-connect"
	 args))))

(defun guard-rightin-from-separate-world (self input)
  ;; Should look like (:WORLD-EVENT (id . args))
  ;; /\/: Another reason to put the simple matcher in the support system.
  (declare (ignore self))
  (if (and (consp input) (keywordp (car input)))
      #+:undef
      (and (consp input)
	   (eq (car input) :world-event)
	   (consp (cadr input))
	   (symbolp (caadr input)))
      (values t input)
    (error "Invalid input from separate world: ~S" input)))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
