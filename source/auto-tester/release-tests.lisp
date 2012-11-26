;;;; File: release-tests.lisp
;;; Contains: Release tests for automatic testing.
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: July 1994
;;; Updated: Sun Nov 10 01:50:35 1996 by Jeff Dalton
;;; Copyright: (c) 1994, AIAI, University of Edinburgh

(in-package :oplan-autotester)

(setq *release-test-sequence*

  (default-replans 3
   (list

    ;; BBB
    (make-plan-test
      :domain     "bbb"
      :task       "task_blue_badge_in_red_box"
      :replans    3
      :exhaustive t)
    (make-plan-test
      :domain     "bbb"
      :task       "task_red_badge_in_red_box"
      :replans    4
      :exhaustive t)

    ;; Blocks-1
    (make-plan-test
      :domain     "blocks-1"
      :task       "task_stack_abc")
    (make-plan-test
      :domain     "blocks-1"
      :task       "task_stack_abc_2")
    (make-plan-test
      :domain     "blocks-1"
      :task       "task_stack_cba")
    (make-plan-test
      :domain     "blocks-1"
      :task       "task_stack_bac")

    ;; Blocks-2
    (make-plan-test
      :domain     "blocks-2"
      :task       "task_stack_abc")
    (make-plan-test
      :domain     "blocks-2"
      :task       "task_stack_abc_2")
    (make-plan-test
      :domain     "blocks-2"
      :task       "task_stack_cba")
    (make-plan-test
      :domain     "blocks-2"
      :task       "task_stack_bac")

    ;; House-1
    (make-plan-test
      :domain     "house-1"
      :task       "task_build_house"
      :replans    1
      :exhaustive t)

    ;; House-2
    (make-plan-test
      :domain     "house-2"
      :task       "task_build_house"
      :replans    1
      :exhaustive t)

    ;; House-3
    (make-plan-test
      :domain     "house-3"
      :task       "task_build_large_house"
      :replans    1
      :exhaustive t)

    ;; House-4
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

    ;; Space-platform
    (make-plan-test
      :domain     "space-platform"
      :task       "task_space_platform")
    (make-plan-test
      :domain     "space-platform"
      :task       "task_small_space_platform")
    (make-plan-test
      :domain     "space-platform"
      :task       "task_large_space_platform")

    ;; Eusat
    (make-plan-test
      :domain     "eusat"
      :task       "task_mission_objectives_1"
      :replans    2
      :exhaustive t)
    (make-plan-test
      :domain     "eusat"
      :task       "task_mission_objectives_2"
      :replans    3
      :exhaustive nil)

    ;; Pacifica-1
    (make-plan-test
      :domain     "pacifica-1"
      :task       "task_operation_blue_lagoon")
    (make-plan-test
      :domain     "pacifica-1"
      :task       "task_operation_paradise")

    ;; Pacifica-2
    (make-plan-test
      :domain     "pacifica-2"
      :task       "task_operation_columbus"
      :replans    2
      :exhaustive nil)

    ;; Pacifica-3
    (make-plan-test
      :domain     "pacifica-3"
      :task       "task_operation_columbus"
      :replans    1
      :exhaustive nil)

    ;; Pacifica-4
    (make-plan-test
      :domain     "pacifica-4"
      :task       "task_operation_columbus"
      :replans    1
      :exhaustive nil)

    ;; Missionaries and cannibals version 1
    (make-plan-test
      :domain     "m-and-c-1"
      :task       "task_mc_problem")

    ;; Missionaries and cannibals version 2
    (make-plan-test
      :domain     "m-and-c-2"
      :task       "task_mc_problem")

    ;; House-5 (uses strictly consumable resources)
    (make-plan-test
      :domain     "house-5"
      :task       "task_build_house")
    (make-plan-test
      :domain     "house-5"
      :task       "task_build_secure_house"
      :replans    1
      :exhaustive t)
    (make-plan-test
      :domain     "house-5"
      :task       "task_build_cheap_house")
    (make-plan-test
      :domain     "house-5"
      :task       "task_build_cheap_secure_house"
      :replans 0
      :exhaustive t)

    ;; Spanner
    (make-plan-test
      :domain     "spanner"
      :task       "task_spanner_1"
      :replans    1
      :exhaustive t)
    (make-plan-test
      :domain     "spanner"
      :task       "task_spanner_1"
      :replans    1
      :exhaustive t)

    ;; Software development
    #+:undef
    (make-plan-test
      :domain     "software-development"
      :task       "task_information_system"
      :replans    1
      :exhaustive nil)

    )))

;;; End
