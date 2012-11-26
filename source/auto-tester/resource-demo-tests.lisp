;;;; File: resource-demo-tests.lisp
;;; Contains: Release tests for automatic testing.
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: July 1994
;;; Updated: Sun Nov 10 01:50:50 1996 by Jeff Dalton
;;; Copyright: (c) 1994, AIAI, University of Edinburgh

;;; N.B. Not loaded automatically as part of the auto-tester system.

(in-package :oplan-autotester)

(export '(run-resource-demo-test-sequence))

(setq *resource-demo-test-sequence*

  (list

    ;; Pacifica-3
    (make-plan-test
      :domain     "pacifica-3"
      :task       "task_operation_columbus"
      :replans    2
      :exhaustive nil)

    (make-plan-test
      :domain     "pacifica-3"
      :task       "task_operation_columbus_mixed_transports"
      :replans    2
      :exhaustive nil)


    (make-plan-test
      :domain     "pacifica-3"
      :task       "task_operation_columbus_ground_transports_only"
      :replans    2
      :exhaustive nil)

    (make-plan-test
      :domain     "pacifica-3"
      :task       "task_operation_columbus_helicopters_only"
      :replans    2
      :exhaustive nil)

    ))

;;; Run-resource-demo-test-sequence runs all cases we want to check
;;; before a release.

(defun run-resource-demo-test-sequence (&rest keyword-args)
  (apply #'run-test-sequence *resource-demo-test-sequence*
	 :sanity-check-only t
	 keyword-args))

;;; End
