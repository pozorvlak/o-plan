;;;; File: manual-interface.lisp
;;; Contains: Manual interface to single-process O-Plan
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: October 1994
;;; Updated: Mon Jul 13 22:41:04 1998 by Jeff Dalton
;;; Copyright: (c) 1994, AIAI, University of Edinburgh

(in-package :oplan)

;;;; Manual interface

;;; Whether or not we're in connect-mode, we may want to send some
;;; messages to O-Plan "by hand".  This section contains some routines
;;; that are useful in common cases of that sort.


;;; Some messages

(defun add-action (pattern)
  (ipc-write-to-oplan
    `(:add-to-task :action ,pattern)))


;;;; Force-reset

;;; Attempt to stop the planner from doing whatever it's currently
;;; doing and then to reinitialize it.  Might be called from an error
;;; "break".

;;; If a KP is active, it will send a :KPREADY when it's unwound,
;;; because of an unwind-protect.

;;; /\/: Assumes only one KP.

(defun force-reset ()
  (declare (special *ready-kps*))
  (dolist (p *all-pprocesses*)
    (setf (queue-contents (pprocess-event-queue p)) nil))
  (unless (or *ready-kps*
	      (pprocess-is-active-p (find-pprocess :kp)))
    (push ':KP *ready-kps*))
  (ipc-write-to-oplan '(:init))
  (throw :pprocess-cycle-exit nil))


;;;; Testing

;;; Sanity checks
;;;
;;; The number of errors will be sent to the TA.
;;; Error messages will appear in the Lisp interaction window.

(defun sanity-check ()
  (ipc-write-to-oplan '(:check-plan)))


;;; Module tests

(defun run-module-tests (&optional (module :all-tests))
  (load "test-systems.lisp")
  (load-test-modules)
  (funcall (read-from-string "RUN-ALL-TESTS") module))


;;; A quick way to run the auto-tester.

;;; Also, "(go-faster)" can be used to make things faster.

(defun standard-test (&rest test-keys)
  (run-test-from-scratch "atest:run-standard-test-sequence" test-keys))

(defun full-test (&rest test-keys)
  (run-test-from-scratch "atest:run-full-test-sequence" test-keys))

(defun release-test (&rest test-keys)
  (run-test-from-scratch "atest:run-release-test-sequence" test-keys))

(defun super-test (&rest test-keys)
  ;; This is supposed to "really be everything" that's regarded
  ;; as a stable test.  So it changes and isn't best for timing
  ;; comparisons.
  (run-test-from-scratch "atest:run-super-test-sequence" test-keys))

(defun run-test-from-scratch (fn-name-string test-keys)
  (go-faster)
  (set-up-test)
  (apply (read-from-string fn-name-string) test-keys)
  nil)

(defun set-up-test ()
  (load-system 'auto-tester :recursive nil))

(defun go-faster ()
  #+kcl (sys:use-fast-links t)
  (run-lights-off)
  (output-off))

(defun output-off ()
  ;; /\/: This works only if all conponents are in the same Lisp image.
  (dolist (p *all-pprocesses*)
    (when (component-p p)
      (call-in-pprocess-env p
	#'(lambda ()
	    (dev-set-debug-level :none))))))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
