;;;; File: standard-tests.lsp
;;; Contains: Standard tests for automatic testing.
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: 26 May 1993
;;; Updated: Wed Mar 17 00:08:45 1999 by Jeff Dalton
;;; Copyright: (c) 1992, 1993, AIAI, University of Edinburgh

(in-package :oplan-autotester)


;;; Blocks domains

;;; The stack_abc_2 tasks used to require depth-1st alt choice
;;; or else the planner would struggle to find a solution.  /\/
;;; The standard test sequence does not include the abc_2 tasks.

(defparameter *blocks-1-test-sequence*	;missing stack_abc_2
  (list
    (make-plan-test
      :domain     "blocks-1"
      :task       "task_stack_abc")
    (make-plan-test
      :domain     "blocks-1"
      :task       "task_stack_cba")
    (make-plan-test
      :domain     "blocks-1"
      :task       "task_stack_bac")
   ))

(defparameter *blocks-1-abc-2-test-sequence*
  (list
    (make-plan-test
      :domain     "blocks-1"
      :task       "task_stack_abc_2")
   ))

(defparameter *blocks-2-test-sequence*	;missing stack_abc_2
  (list
    (make-plan-test
      :domain     "blocks-2"
      :task       "task_stack_abc")
    (make-plan-test
      :domain     "blocks-2"
      :task       "task_stack_cba")
    (make-plan-test
      :domain     "blocks-2"
      :task       "task_stack_bac")
   ))

(defparameter *blocks-2-abc-2-test-sequence*
  (list
    (make-plan-test
      :domain     "blocks-2"
      :task       "task_stack_abc_2")
   ))


;;; House domains

(defparameter *house-1-test-sequence*
  (list
    (make-plan-test
      :domain     "house-1"
      :task       "task_build_house"
      :replans    1
      :exhaustive t)
   ))

(defparameter *house-2-test-sequence*
  (list
    (make-plan-test
      :domain     "house-2"
      :task       "task_build_house"
      :replans    1
      :exhaustive t)
   ))

(defparameter *house-3-test-sequence*
  (list
    (make-plan-test
      :domain     "house-3"
      :task       "task_build_large_house"
      :replans    1
      :exhaustive t)
   ))

(defparameter *house-4-test-sequence*
  (list
    (make-plan-test
      :domain     "house-4"
      :task       "task_build_house"
      :replans    2
      :exhaustive t)
    (make-plan-test
      :domain     "house-4"
      :task       "task_build_house_to_time_0"
      :replans    0
      :exhaustive t)
    (make-plan-test
      :domain     "house-4"
      :task       "task_build_house_to_time_1"
      :replans    1
      :exhaustive t)
    (make-plan-test
      :domain     "house-4"
      :task       "task_build_house_to_time_2"
      :replans    2
      :exhaustive t)
   ))


;;; Space platform

(defparameter *space-platform-test-sequence*
  (list
    (make-plan-test
      :domain     "space-platform"
      :task       "task_space_platform")
    (make-plan-test
      :domain     "space-platform"
      :task       "task_small_space_platform")
    (make-plan-test
      :domain     "space-platform"
      :task       "task_large_space_platform")
   ))


;;; Eusat

(defparameter *eusat-test-sequence*
  (list
    (make-plan-test
      :domain     "eusat"
      :task       "task_mission_objectives_1")
    (make-plan-test
      :domain     "eusat"
      :task       "task_mission_objectives_2")
   ))

(defparameter *eusat-test-sequence-w/replans*
  (list
    (make-plan-test
      :domain     "eusat"
      :task       "task_mission_objectives_1"
      :replans 2
      :exhaustive t)
    (make-plan-test
      :domain     "eusat"
      :task       "task_mission_objectives_2"
      :replans 2
      :exhaustive nil)
   ))


;;; Pacifica

(defparameter *pacifica-test-sequence*	;/\/ omits Castaway
  (list
    (make-plan-test
      :domain     "pacifica"
      :task       "task_operation_blue_lagoon")
    (make-plan-test
      :domain     "pacifica"
      :task       "task_operation_paradise")
   ))


;;; Missionaries and cannibals

(defparameter *mission-test-sequence*
  (list
    (make-plan-test
      :domain     "mission"
      :task       "task_mc_problem")
   ))


;;; Three pigs

(defparameter *three-pigs-test-sequence*
  (list
    (make-plan-test
      :domain     "three-pigs"
      :task       "task_build_house")
    (make-plan-test
      :domain     "three-pigs"
      :task       "task_build_secure_house"
      :replans 1
      :exhaustive t)
    (make-plan-test
      :domain     "three-pigs"
      :task       "task_better_build_secure_house"
      :replans 1
      :exhaustive t)
    (make-plan-test
      :domain     "three-pigs"
      :task       "task_build_cheap_house")
    (make-plan-test
      :domain     "three-pigs"
      :task       "task_build_cheap_secure_house"
      :replans 0
      :exhaustive t)
   ))


;;; The "standard" test sequence

;;; The standard test sequence includes all the "traditional" domains
;;; and is used to ensure continuity with versions of O-Plan before 2.2.

(setq *standard-test-sequence*
      (append *blocks-1-test-sequence*
	      *blocks-2-test-sequence*
	      *house-1-test-sequence*
	      *house-2-test-sequence*
	      *house-3-test-sequence*
	      *house-4-test-sequence*
	      *space-platform-test-sequence*
	      *eusat-test-sequence*))


;;; The "full" test sequence.

;;; The full sequence started as the one that included all cases we
;;; thought were reasonably auto-testable and that more or less work
;;; correctly.  Howver, once it became a standand benchmark for
;;; comparing different versions of O-Plan, it was frozen in its
;;; current form.  The all-cases test is now the "super test".

(defun default-replans (default-n &rest test-sequences) ; -> test-sequence
  (flet ((add-replans-if-none (test)
	   (if (plan-test-replans test)
	       test			;replans already specified
	     (let ((copy (copy-plan-test test)))
	       (setf (plan-test-replans copy) default-n
		     (plan-test-exhaustive copy) nil)
	       copy))))
    (mapcan #'(lambda (seq)
		(mapcar #'add-replans-if-none seq))
	    test-sequences)))

(setq *full-test-sequence*
      (append 
              ;; Traditional domains, but with some replans that aren't
              ;; in the standard test sequence.
              (default-replans 2
		  *blocks-1-test-sequence*
		  *blocks-2-test-sequence*)
	      *house-1-test-sequence*
	      *house-2-test-sequence*
	      *house-3-test-sequence*
	      *house-4-test-sequence*
	      (default-replans 2
		  *space-platform-test-sequence*)
	      *eusat-test-sequence-w/replans*
	      ;; Newer domains.
	      *pacifica-test-sequence*
	      *mission-test-sequence*
	      *three-pigs-test-sequence*
	      ))

;;; Some nonstandard tests that can be run after the standard tests.

(defparameter *nonstandard-test-sequence*
  (append     *pacifica-test-sequence*
	      *mission-test-sequence*
	      *three-pigs-test-sequence*
	      ))

;;; End
