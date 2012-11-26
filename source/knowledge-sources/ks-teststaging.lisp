;;;; File: KS-TESTSTAGING.lisp
;;;; Version: %W%
;;;; Contains: Tests the staging code.
;;;; Author: Richard Kirby (rbk@ebay)
;;;; Created: Wed Sep 19 17:28:31 1990
;;;; Updated: Fri Oct 25 18:31:48 1996 by Jeff Dalton
;;;; Doc Ref: 
;;;; Author Institution: AIAI
;;;; Origin: 
;;;; Terms: 
;;;; Permissions: 
;;;; Release Date: 
;;;; Released By: 
;;;; Release Package: 
;;;; Copyright: 

(in-package :oplan-knowledge-source)

(defun KS-TESTSTAGING (ag)
  (stage-manager (one two) (three) ag
		 (setq one 1 two 2 three 3)
		 (dev-debug :user-request "Stage 0")
		 (dev-debug :user-request "one = ~S, two = ~S, three = ~S"
					one two three)
		 (stage t)
		 (dev-debug :user-request "Stage 1" :user-request)
		 (dev-debug :user-request "one = ~S, two = ~S, three = ~S"
					one two three)
		 (setq one "one")
		 (stage t)
		 (dev-debug :user-request "Stage 2" :user-request)
		 (dev-debug :user-request "one = ~S, two = ~S, three = ~S"
					one two three)
		 (setq two "two")
		 (stage t)
		 (setq three "foo")
		 (dev-debug :user-request "Last Stage" :user-request)
		 (dev-debug :user-request "one = ~S, two = ~S, three = ~S"
					one two three)))


;;;; ---------------------------- Change History ----------------------------
;;;; (Initials) (Date of change)           (Comment)
