;;;; File: viewer-package.lisp
;;; Contains: Plan/World viewer package definition
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: October 1996
;;; Updated: Tue Oct  7 04:53:11 1997 by Jeff Dalton
;;; Copyright: (c) 1996, AIAI, University of Edinburgh

(in-package :oplan-plan-world-viewer)

(use-package :oplan-tf)

(use-package :oplan-x)
(use-package :oplan-pseudo-process)		;/\/ too low-level?
(use-package :oplan-util)
(use-package :oplan-time-util)
(use-package :oplan-ipc)

;; /\/: Was for is-get-window-args.  Now we just say is:is-get-window-args
;;      when we need it in code.
; (use-package :oplan-initsystem)

(import '(oplan::node-1 oplan::node-2 oplan::unknown))

(export '(pw-get-plan-for-viewing
	  pw-get-world-for-viewing
	  pw-handle-view
	  *pw-viewing-window*))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
