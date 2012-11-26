;;;; File: world-package.lisp
;;; Contains: World package definition
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: November 1994
;;; Updated: Sun Jun 13 19:12:57 1999 by Jeff Dalton
;;; Copyright: (c) 1994, AIAI, University of Edinburgh

;;; Package definition

(in-package :user)

(defpackage :oplan-world

  (:use #+:cltl2 :common-lisp
        #-:cltl2 :lisp
	#+lucid  :clos

	:simple-defsystem
	:oplan-util
	:oplan-time-util
	:oplan-developerlib
	:oplan-pseudo-process
	:oplan-sim-clock
	:oplan-world-state-util
	:oplan-components
	:oplan-ipc
	:oplan-xwindowio
	:oplan-initsystem)

  (:export
     #:start-separate-world
     #:world-event-handler
     #:compile-known-worlds))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
