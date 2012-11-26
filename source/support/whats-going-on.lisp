;;;; File: whats-going-on.lisp
;;; Contains: A simple log of whats happening.
;;; Author: Richard Kirby (rbk)
;;; Created: Mon Dec  9 11:01:49 1991
;;; Updated: Tue Jul  9 00:30:38 1996 by Jeff Dalton
;;; Copyright: (c) 1992, AIAI, University of Edinburgh

(in-package :oplan-developerlib)

(use-package :oplan-util)

(export '(whats-going-on
	  *print-whats-going-on-p*))

;;; /\/: Maybe whats-going-on should be a macro, to eliminate the
;;; procedure call when we're not recording anything.  Proclaiming
;;; it inline may do the trick in some Lisps.  See below.  Only
;;; the initial check is inline, to keep down the amount of code
;;; involved.

(defvar *whats-going-on-file* nil)

(defvar *whats-going-on-stream* nil)

(defun ensure-whats-going-on ()
  ;; If the stream exists, assume it's open and return; otherwise...
  (unless *whats-going-on-stream*
    ;; Open a whats-going-on-stream.
    ;; Use the default name if no name has been assigned.
    (unless *whats-going-on-file*
      (setq *whats-going-on-file*
	    (concat-string (get-parameter :oplan-tmp-dir)
			   "/whats-going-on."
			   (getenv "USER"))))
    ;; If the file already exists, rename it to provide a backup.
    ;; We could use call OPEN with :if-exists :new-version or :rename
    ;; instead, if we could rely on Common Lisp to do something reasonable,
    ;; but these things can be no-ops.  /\/
    (when (probe-file *whats-going-on-file*)
      (rename-file *whats-going-on-file*
		   (concat-string *whats-going-on-file* ".BAK")))
    ;; Open it.
    (setq *whats-going-on-stream*
	  (open *whats-going-on-file*
		:direction :output
		:if-exists :error
		:if-does-not-exist :create))))

(defvar *print-whats-going-on-p* nil)

(defun-inline whats-going-on (fmt &rest args)
  (when *print-whats-going-on-p*
    (record-whats-going-on fmt args)))

(defun record-whats-going-on (fmt args)
  (ensure-whats-going-on)
  (let ((out *whats-going-on-stream*))
    (let ((*print-case* :downcase)
	  (*print-pretty* t)
	  (*print-level* nil)
	  (*print-length* nil))
      (apply #'format out fmt args)
      (terpri out)
      (terpri out)
      (force-output out))))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;

