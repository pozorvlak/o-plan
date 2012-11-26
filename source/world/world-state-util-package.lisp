;;;; File: world-state-util-package.lisp
;;; Contains: World-state-util package definition
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: June 1999
;;; Updated: Wed Sep  8 01:11:41 1999 by Jeff Dalton
;;; Copyright: (c) 1999, AIAI, University of Edinburgh

;;; Package definition

(in-package :user)

(defpackage :oplan-world-state-util

  (:use #+:cltl2 :common-lisp
        #-:cltl2 :lisp
	:oplan-util
	:oplan-developerlib)

  (:export

     ;; World state
     #:*world-state*			;so we can bind it
     #:clear-world-state
     #:set-world-pattern-value
     #:get-world-pattern-value
     #:world-pattern-has-value-p
     #:world-fact-p
     #:get-world-state

     ;; World and always
     #:get-world-value

     ;; Always facts
     #:*world-always-table*		;so we can bind it
     #:clear-world-always
     #:set-world-always-value
     #:get-world-always-value
     #:has-world-always-value-p
     #:world-always-p
     #:get-world-always)

  )

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
