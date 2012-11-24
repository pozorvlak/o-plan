;;;; File: extra-tests.lsp
;;; Contains: More tests for automatic testing.
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: Mon 3 July 1995
;;; Updated: Wed Mar 17 01:58:59 1999 by Jeff Dalton
;;; Copyright: (c) 1992, 1993, AIAI, University of Edinburgh

(in-package :oplan-autotester)

(export 'run-extra-test-sequence)
(export 'extra-test)
(export 'socap-test)
(export 'use-cm-test)

(export 'run-super-test-sequence)

(defparameter *extra-test-sequence*

  (list

    ;; The "Island Rescue" Web demo.
    (make-plan-test
      :domain     "island-rescue-web-demo"
      :task       "task_pacifica_evacuation"
      :replans    1
      :exhaustive nil)

    ;; Missionaries and cannibals version 1
    (make-plan-test
      :domain     "mission"
      :task       "task_mc_problem"
      :replans    2
      :exhaustive nil)

    ;; Missionaries and cannibals version 2
    (make-plan-test
      :domain     "mission-with-only-compute"
      :task       "task_mc_problem"
      :replans    2
      :exhaustive nil)

    ;; Missionaries and cannibals version 3
    (make-plan-test
      :domain     "mission-forward-meta"
      :task       "task_mc_problem"
      :replans    2
      :exhaustive nil)

    ;; Software development
    (make-plan-test
      :domain     "software-development"
      :task       "task_information_system"
      :replans    1
      :exhaustive nil)

    ;; SOCAP
    ;; Redefines assign-priority.
    #|
    (make-plan-test
      :domain     "socap-test"
      :task       "task_deter_border_incursions_only")

    (make-plan-test
      :domain     "socap-test"
      :task       "task_deter_border_incursions_and_provide_defence")
    |#
    
    ;; Pacifica 2nd year resource demo
    (make-plan-test
      :domain     "pacifica-3"
      :task       "task_operation_columbus")

    (make-plan-test
      :domain     "pacifica-3"
      :task       "task_operation_columbus_mixed_transports")

    (make-plan-test
      :domain     "pacifica-3"
      :task       "task_operation_columbus_ground_transports_only")

    (make-plan-test
      :domain     "pacifica-3"
      :task       "task_operation_columbus_helicopters_only")

    ))

;;; A long function name

(defun run-extra-test-sequence (&rest keyword-args)
  (apply #'run-test-sequence *extra-test-sequence*
	 keyword-args))

;;; And a short one

(defun extra-test (&rest test-keys)
  (apply #'run-extra-test-sequence test-keys))



;;;; SOCAP tests

(defparameter *socap-test-sequence*

  (list

    (make-plan-test
      :domain     "socap-test"
      :task       "task_deter_border_incursions_only")

    (make-plan-test
      :domain     "socap-test"
      :task       "task_deter_border_incursions_and_provide_defence")

    ))

(defun socap-test (&rest test-keys)
  (apply #'run-test-sequence *socap-test-sequence*
	 test-keys))


;;;; Use CM tests

(defparameter *use-cm-test-sequence*

  (list

    (make-plan-test
      :domain "use-example"
      :task "task_get_to_work_via_expansion")

    (make-plan-test
      :domain "use-example"
      :task "task_get_to_work_via_conditions")

    (make-plan-test
      :domain "island-use-example"
      :task "task_pacifica_evacuation")

    (make-plan-test
      :domain "island-use-example"
      :task "task_pacifica_evacuation_with_no_time_limit")

    #+:undef
    (make-plan-test
      :domain "island-use-example"
      :task "task_pacifica_evacuation_with_too_little_time"
      :replans    0
      :exhaustive t)

    (make-plan-test
      :domain "island-use-example"
      :task "task_pacifica_evacuation_with_even_less_time"
      :replans    0
      :exhaustive t)

    ))

(defun use-cm-test (&rest test-keys)
  (apply #'run-test-sequence *use-cm-test-sequence*
	 test-keys))


;;;; Multiple-answer compute

(defparameter *multiple-answer-compute-test-sequence*

  (list

    (make-plan-test
      :domain "multiple-answer-compute-example"
      :task "task_example_1"
      :replans 2
      :exhaustive t)

    (make-plan-test
      :domain "multiple-answer-compute-example"
      :task "task_example_2"
      :replans 1
      :exhaustive t)

    (make-plan-test
      :domain "multiple-answer-compute-example"
      :task "task_example_3"
      :replans 1
      :exhaustive t)

    ))


;;;; Vars in values

;;; A problem that drive a bug in the handling of variables on the
;;; v side of p = v. 

(defparameter *vars-in-value-test-sequence*

  (list

    (make-plan-test
      :domain "gf-pacifica"
      :task "task_simple_accumulation_test"
      :replans 1
      :exhaustive t)

    (make-plan-test
      :domain "gf-pacifica"
      :task "task_test_choose_two_trucks")

    ))


;;;; Route-finding

(defparameter *trains-test-sequence*

  (list

    (make-plan-test
      :domain "trains-1"
      :task "task_base_problem"
      :replans 1
      :exhaustive t)

    (make-plan-test
      :domain "trains-1"
      :task "task_example_1"
      :replans 2
      :exhaustive nil)

    (make-plan-test
      :domain "trains-1"
      :task "task_example_2"
      :replans 2
      :exhaustive nil)

    ))


;;;; Space-platform with corrected condition types

(defparameter *space-platform-2-test-sequence*
  (list
    (make-plan-test
      :domain     "space-platform-2"
      :task       "task_space_platform")
    (make-plan-test
      :domain     "space-platform-2"
      :task       "task_small_space_platform")
    (make-plan-test
      :domain     "space-platform-2"
      :task       "task_large_space_platform")
   ))


;;;; The TF for the "go places and do things" COA-matrix-server Web demo

(defparameter *gpdt3-test-sequence*
  (list
    (make-plan-test
      :domain "gpdt3-web-demo"
      :task "task_this_is_coa_2")
    (make-plan-test
      :domain "gpdt3-web-demo"
      :task "task_this_is_coa_2_2")
    (make-plan-test
      :domain "gpdt3-web-demo"
      :task "task_this_is_coa_3")
    (make-plan-test
      :domain "gpdt3-web-demo"
      :task "task_this_is_coa_4")
    (make-plan-test
      :domain "gpdt3-web-demo"
      :task "task_this_is_coa_5")
    (make-plan-test
      :domain "gpdt3-web-demo"
      :task "task_this_is_coa_5_clear")
   ))

(defparameter *gpdt3-all-task-types-test-sequence*
  (list
    (make-plan-test
      :domain "gpdt3-web-demo"
      :task "task_do_all_task_types_clear")
    (make-plan-test
      :domain "gpdt3-web-demo"
      :task "task_do_all_task_types_rain")
    (make-plan-test
      :domain "gpdt3-web-demo"
      :task "task_do_all_task_types_storm")
   ))


;;;; Super-test

(defun run-super-test-sequence (&rest test-keys)
  (apply #'run-test-sequence
	 (append *full-test-sequence*
		 *extra-test-sequence*
		 *socap-test-sequence*
		 *use-cm-test-sequence*
		 *multiple-answer-compute-test-sequence*
		 *vars-in-value-test-sequence*
		 *trains-test-sequence*
		 ;; New tests as of March 1999.
		 *space-platform-2-test-sequence*
		 *blocks-1-abc-2-test-sequence*
		 *blocks-2-abc-2-test-sequence*
		 *gpdt3-test-sequence*
		 *gpdt3-all-task-types-test-sequence*)
	 test-keys))

;;; End
