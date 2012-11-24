;;;; File: auto-tester.lsp
;;; Contains: Controller, etc. for automatic testing.
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: 14 March 1993
;;; Updated: Sat May 22 17:49:00 1999 by Jeff Dalton
;;; Copyright: (c) 1992, 1993, AIAI, University of Edinburgh

;;;; Controller, etc. for automatic testing.

;;; The auto-tester can be used to check plans against files containing
;;; saved plan descriptions.  It can also be used to generate the saved
;;; plan descriptions and to run the sanity-checker.

;;; The auto-tester works by taking over the Task Assigner and then
;;; handling communication with the planner itself.  The TA is given
;;; a run-function that simply passes messages between the test
;;; controller and the planner.  The test controller is similar to
;;; a finite state automaton (see fsa.lsp).  It manages the sequence
;;; of message exchanges required to get the planner to run a test,
;;; return a description of the plan, etc.

;;; To run tests or generate saved solutuions, load the auto-tester system
;;; from the Lisp listener and then call one of the run-test functions, such
;;; as run-test, run-test-sequence, or run-standard-test-sequence, defined
;;; later in this file.  E.g.:
;;;
;;;   form> (load-system 'auto-tester :recursive nil)
;;;
;;;   form> (atest:run-full-test-sequence)

;;; /\/: Some shortcuts for running the auto-tester are defined in
;;; manual-interface.lsp.

(in-package :oplan-autotester :nicknames '(:atest))

(use-package :oplan-util)
(use-package :oplan-pseudo-process)
(use-package :oplan-xwindowio)
(use-package :oplan-fsa)
(use-package :oplan-ipc)

(export '(test-controller))		;type name
(export '(make-plan-test))
(export '(run-test
	  run-test-sequence
	  run-standard-test-sequence
	  run-full-test-sequence
	  run-release-test-sequence))

(defvar *test-window*)

(defvar *standard-test-sequence* nil)
(defvar *full-test-sequence* nil)
(defvar *release-test-sequence* nil)
(defvar *test-directory* "./test-results")

(defvar *test-sequence* nil)		;most recent test sequence used

(defvar *generate* nil)			;generate test results?
(defvar *sanity-check* nil)		;run the sanity checker?
(defvar *sanity-check-only* nil)

;;; Some statistics

;;; /\/: The totals are kept only when we're doing comparison tests,
;;; not when we're generating new solutions or when we're performing
;;; only sanity checks.

;;; /\/: This stuff should probably be part of the test-controller struct.

(defvar *domain-count* 0)
(defvar *plan-count* 0)
(defvar *statistics-totals* '())


;;; Test-controller setup and TA takeover

(defun ensure-test-controller ()
  (or (exists-pprocess :test-controller)
      (set-up-test-controller)))

(defun set-up-test-controller ()
  (setq *test-window* (x-get-stream :tainout))
  (new-test-controller :test-controller)
  (find-pprocess :test-controller))

(defun take-over-ta ()
  (let ((ta (find-pprocess :ta)))
    (assert (eq (pprocess-status ta) :run-on-event))
    (assert (eq (pprocess-run-function ta)
		'oplan-task-assigner::ta-event-handler))
    (assert (not (next-event-p ta)))
    (setf (pprocess-run-function ta) 'ta-controlled-handler)
    (shiftf (getf (pprocess-plist ta) :saved-input-streams)
	    (pprocess-input-streams ta)
	    nil)
    ta))

(defun restore-ta ()
  (let ((ta (find-pprocess :ta)))
    (assert (eq (pprocess-status ta) :run-on-event))
    (assert (eq (pprocess-run-function ta) 'ta-controlled-handler))
    (assert (not (next-event-p ta)))
    (setf (pprocess-run-function ta) 'oplan-task-assigner::ta-event-handler)
    (shiftf (pprocess-input-streams ta)
	    (getf (pprocess-plist ta) :saved-input-streams)
	    nil)
    ta))

(defun ta-controlled-handler (self)
  ;; Becomes the run-function for the :TA p-process.
  ;; Messages to :ta are sent to :test-controller and vice versa.
  (when (listen *test-window*)
    (cerror "Ignore it." "Unexpected TA input.")
    (return-from ta-controlled-handler nil))
  (let ((e (next-event self)))
    (ecase (pprocess-name (message-sender e))
      (:im
       (ipc-write :test-controller (message-contents e)))
      (:test-controller
       (ipc-write-to-oplan (message-contents e))))))


;;; Tests

;;; A test specifies a domain, a task, a number of replans, and whether
;;; or not the test is exhaustive.  A list of files in which to store
;;; (or find) solutions may also be specified.

;;; N.B. NIL as the number of replans means to avoid replanning altogether.
;;; A number, n, means to replan n times.  Zero is therefore equivalent to
;;; NIL so far as the number of replans that will be performed is concerned.
;;; Zero differs from NIL in its implications for exhaustive tests.

;;; For an exhaustive test, the number of replans is also the number
;;; of solutions.  An exhaustive test should continue until we run out
;;; of alternative plans.  So if there are k solutions, we should
;;; replan k times (there's the first plan, then k-1 other solutions,
;;; then one more replan to get the out-of-alternatives message).
;;; This is a nice coincidence, but may not be exactly what's
;;; expected.

;;; Anyway, if the plan-test-replans is a number and exhaustive is
;;; true, the controller will expect to find exactly that number of
;;; solutions and it will count as failure if it doesn't.  If you 
;;; just want to check the first solution and completely ignore the
;;; possibility of replanning, use NIL as the number of replans.  If
;;; you want to look at the first k solutions when there are more than
;;; k, use k-1 as the number of replans and make exhaustive = nil.

;;; Note that the exhaustive flag is checked only if the number-of-
;;; replans is a number.  If you expect zero solutions, define the
;;; test with :replans 0 and :exhaustive t.

;;; If the plan-test-solutions list is not specified it will be
;;; generated from the domain name, the task name, and the solution
;;; numbers (starting from 1).

(defstruct plan-test
  domain				;a string
  task					;a string
  (replans nil)				;a natural number or nil
  (exhaustive t)			;a boolean
  (solutions '())			;a list of file names
  )

(defun plan-test-number-of-solutions (test)
  (let ((replans (plan-test-replans test))
	(exhaustive (plan-test-exhaustive test)))
    (cond ((null replans) 1)		;just one plan
	  (exhaustive replans)		;see explanation above
	  (t (1+ replans)))))		;1 solution plus n replans

(defun ensure-plan-test-solutions (test)
  (or (plan-test-solutions test)
      (setf (plan-test-solutions test)
	    (generate-solution-file-names test))))

(defun generate-solution-file-names (test)
  (let ((dom (plan-test-domain test))
	(task (plan-test-task test)))
    (loop for i from 1 to (plan-test-number-of-solutions test)
	  collect
	    (format nil "~(~A:~A:~A.plan~)" dom task i))))


;;; The test-controller pprocess type

;;; See the support file fsa.lsp for a description the test-controller-fsa.

(defstruct (test-controller
	     (:constructor %make-test-controller)
	     (:conc-name controller-)
	     (:print-function print-pprocess)
	     (:include pprocess
		(status :suspended)
		(run-function 'test-controller-event-handler)))
  (fsa 'not-an-fsa)
  (current-test nil)
  (remaining-replans 0)
  (test-solutions '())			;for the current test
  (remaining-tests '())			;remaining tests in a sequence
  (continue-p nil)			;to record result from continue-test-p
  )

(defmacro controller-state (controller)		;a pseudo-accessor
  `(fsa-state (controller-fsa ,controller)))

(defun new-test-controller (name &rest args)
  (let ((controller
	 (register-pprocess
	  (apply #'%make-test-controller
		 :name name
		 args))))
    (setf (controller-fsa controller)
	  (make-test-controller-fsa controller))
    controller))


;;; Test-controller event-handler

;;; When an event arrives, the controller moves to a new state; it
;;; then performs some actions associated with the new state.  This is
;;; described in more detail below, in the "states and transitions"
;;; section.

(defun test-controller-event-handler (self)
  (let ((e (next-event)))
    (validate-test-controller-event self e)
    (advance-fsa-state (controller-fsa self) (message-contents e))
    (display-test-status self)
    (do-test-state-actions self)))

(defun validate-test-controller-event (self e)
  (case (pprocess-name (message-sender e))
    (:ta)
    (otherwise
     (case (car (message-contents e))
       (:start-test
	(assert (or (typep (message-sender e) 'repl)
		    (eq (message-sender e) self))))
       (otherwise
	(assert (eq (message-sender e) self)))))))

(defun do-test-state-actions (self)
  (do-fsa-state-actions (controller-fsa self)))

;;; Test status display

(defun display-test-status (self)
  (let ((test (controller-current-test self))
	(replans (controller-remaining-replans self)))
    (clear-screen *test-window*)
    (format *test-window* "Automatic test~%")
    (format *test-window* "Domain: ~A~%Task: ~A~%"
	    (plan-test-domain test)
	    (plan-test-task test))
    (format *test-window* "Replans: ~S~%~%" replans)
    (format *test-window* "Test state: ~(~A~)~%"
	    (controller-state self))
    (force-output *test-window*)))


;;; Main test operations

;;; Run-test-sequence is used to tell the test-controller to run a
;;; series of tests.  It is called with a list of tests and optionally
;;; some keyword parameters:
;;;
;;;   :directory <string>
;;;      The name of a directory for storing test results.  If the
;;;      name contains a "/", it is used as-is; otherwise it is taken
;;;      relative to $OPLANDIR/source/.
;;;
;;;   :generate <true or false>
;;;      True to create files of test results for use in future tests.
;;;      False (the default) to compare against existing files.
;;;
;;;   :sanity-check <true or false>
;;;      True to run the sanity checker on any plans we get.
;;;      False is the default.
;;;
;;;   :sanity-check-only <true or flase>
;;;      True to only sanity-check.  Makes :sanity-check true as well.
;;;      false is the default.
;;;
;;; N.B. The parameter values supplied become the defaults for
;;; subsequent test runs.  /\/: This may not be a very good idea.
;;;

(defun run-test-sequence (tests &key (directory *test-directory*)
				     (generate *generate*)
				     (sanity-check *sanity-check*)
				     (sanity-check-only *sanity-check-only*))
  (let ((controller (ensure-test-controller)))
    ;; /\/ Maybe the directory, etc should be slots in the controller.
    ;; /\/ Should we be recording the parameter values?  We have to do
    ;; /\/ something like that, because the tests are not in the dynamic
    ;; /\/ extent of this procedure.
    (setq *test-sequence* tests
	  *test-directory* (normalize-test-directory directory)
	  *generate* generate
	  *sanity-check* (or sanity-check sanity-check-only)
	  *sanity-check-only* sanity-check-only
	  *domain-count* 0
	  *plan-count* 0
	  *statistics-totals* '())
    (unless (probe-file *test-directory*)
      (error "Can't find directory ~S." *test-directory*))
    (unless (and (eq (pprocess-status controller) :suspended)
		 (not (next-event-p controller)))
      (error "~S is not ready to run tests." controller))
    (setf (pprocess-status controller) :run-on-event)
    (take-over-ta)
    (set-current-test controller (first tests))
    (setf (controller-remaining-tests controller)
	  (rest tests))
    (start-test controller)))

(defun normalize-test-directory (namestring) ; -> pathname
  (if (pathnamep namestring)
      namestring			;assume already ok
      (pathname
        (if (find '#\/ namestring)
	    (concat-string namestring "/")
	    (concat-string (get-parameter :oplan-dir)
			   "/source/" namestring "/")))))

;;; Run-test is used to tell the test-controller to run a single test.

(defun run-test (test &rest keyword-args)
  (apply #'run-test-sequence (list test) keyword-args))

;;; Run-standard-test-sequence runs a standard set of tests.

(defun run-standard-test-sequence (&rest keyword-args)
  (apply #'run-test-sequence *standard-test-sequence* keyword-args))

;;; Run-full-test-sequence runs all cases we think are reasonably
;;; auto-testable and suitable for repeated checks during development
;;; (i.e. likely to be fairly stabile across the changes we're likely
;;; to make.

(defun run-full-test-sequence (&rest keyword-args)
  (apply #'run-test-sequence *full-test-sequence* keyword-args))

;;; Run-release-test-sequence runs all cases we want to check before
;;; a release that don't take too long to complete.

(defun run-release-test-sequence (&rest keyword-args)
  (apply #'run-test-sequence *release-test-sequence*
	 :sanity-check-only t
	 keyword-args))


;;; Test initiation and completion

(defun set-current-test (controller test)
  (setf (controller-current-test controller)
	test)
  (setf (controller-remaining-replans controller)
	(plan-test-replans test))
  (setf (controller-test-solutions controller)
	(ensure-plan-test-solutions test))
  test)

(defun start-test (controller)
  (setf (controller-state controller) 'start)
  (send-to-pprocess controller
    (list :start-test
	  (controller-current-test controller))))

(defun prepare-for-replan (controller)
  (pop (controller-test-solutions controller))
  (when (controller-remaining-replans controller)
    ;; I.e. the plan-test-replans wasn't nil.
    (decf (controller-remaining-replans controller))))

(defun run-next-test (self)
  (cond ((controller-remaining-tests self)
	 (set-current-test self (pop (controller-remaining-tests self)))
	 (start-test self))
	(t (end-test-sequence))))

(defun end-test-sequence ()
  (let ((controller (find-pprocess :test-controller)))
    (restore-ta)
    (oplan-task-assigner::ta-initialise)
    (setf (pprocess-status controller) :suspended)
    (report-statistics-totals)))

(defun report-statistics-totals ()
  (let ((e-dc (expected-domain-count *test-sequence*))
	(e-pc (expected-plan-count *test-sequence*)))
    (unless (and (= *domain-count* e-dc) (= *plan-count* e-pc))
      (format t "~&~%Expected ~S domains and ~S plans, but ..." e-dc e-pc)))
  (format t "~&~%Totals for ~S domains and ~S plans:"
	  *domain-count* *plan-count*)
  (loop for (s . v) in *statistics-totals*
	do (format t "~& ~3T ~S ~23T = ~S" s v)
	finally (format t "~2%")))

(defun expected-domain-count (test-seq)
  (length
   (remove-duplicates test-seq :key #'plan-test-domain :test #'string=)))

(defun expected-plan-count (test-seq)
  (loop for test in test-seq sum (plan-test-number-of-solutions test)))


;;; States and transitions

;;; When a planner output is received as an event E, the controller is
;;; in a state S .  The FSA defined below is used to find the next state,
;;; S', from S and (message-contents E).  Then do-test-state-actions is
;;; called to get the FSA to perform the actions associated with S'.
;;; This typically involves sending a message to the planner.

;;; Note that when the controller is in state S and receives some planner
;;; output, it had already done the actions for S (on the previous cycle)
;;; and was waiting to move to the next state.

;;; When the planner won't provide some suitable output to move the
;;; controller to its next state, the controller sends a message to
;;; itself.  State transitions for these messages are treated in the
;;; same way as for planner output.

(defun make-test-controller-fsa (controller)
  (flet ((test ()
	   (controller-current-test controller)))
    (fsa (matching)
      (start
	(error "Actions for START state should be handled elsewhere")
	((:start-test $test)
	 --> initialize-planner))
      (initialize-planner
	(send-to-planner :init)
	((:init-ok)
	 --> input-tf))
      (input-tf
	(send-to-planner :domain (plan-test-domain (test)))
	((:domain ($tf-file-name . $task-names))
	 --> set-task))
      (set-task
	(send-to-planner :set-task (plan-test-task (test)))
	((:finished)
	 --> get-plan)
	((:no-more-alternatives)
	 --> no-plan))
      (get-plan
        (send-to-planner :check-plan :get-plan-description
	   ;; Place to put unexpected result
	   (solution-file "test-result.plan")
	   ;; Expected result
	   (if (or *generate* *sanity-check-only*)
	       nil
	     (solution-file (car (controller-test-solutions controller)))))
	((:checked :description $description)
	 --> check-plan))
      ;; The sanity-check should occur after the plan description
      ;; has been checked to avoid changing the "statistics".  For
      ;; instance, the AM cycle required for the sanity check will
      ;; change the AM's elapsed cycle count.
      (check-plan
        (let ((result (lookup '$description matching)))
	  (assert (or (list-beginning :ok result)
		      (string= (pathname-name result) "test-result")))
	  (let ((continue-p (continue-test-p controller result)))
	    (setf (controller-continue-p controller) continue-p)
	    (if *sanity-check*
		(send-to-self :sanity-check)
	      (if continue-p
		  (send-to-self :replan)
		  (send-to-self :test-complete)))))
	((:sanity-check)
	 --> sanity-check)
	((:replan)
	 --> replan)
	((:test-complete)
	 --> end))
      (sanity-check
        ;; The check-plan state could have sent :check-plan to the planner,
        ;; but we want a different state name for the test status display.
        (send-to-planner :check-plan)
	((:checked $errors)
	 --> check-sanity-check))
      (check-sanity-check
        (let ((errors (lookup '$errors matching)))
	  (if (continue-from-sanity-check-p controller errors)
	      (send-to-self :replan)
	      (send-to-self :test-complete)))
	((:replan)
	 --> replan)
	((:test-complete)
	 --> end))
      (replan
        (progn
	  (prepare-for-replan controller)
	  (send-to-planner :replan))
	((:finished)
	 --> get-plan)
	((:no-more-alternatives)
	 --> no-plan))
      (no-plan
        (progn
	  (check-no-plan controller)
	  (send-to-self :test-complete))
	((:test-complete)
	 --> end))
      (end
	(run-next-test controller)))))

(defun send-to-planner (&rest message-contents)
  (send-to-pprocess (find-pprocess :ta) message-contents))

(defun send-to-self (&rest message-contents)
  (send-to-pprocess *pprocess* message-contents))

;;; Test evaluation

;;; Continue-test-p checks for failures and returns true or false to
;;; indicate whether the test should continue (by replanning) or not.

(defun continue-test-p (self check-result) ; -> true or false
  ;; Called when a plan has been found.
  ;; Check the plan; return true to replan.
  (let ((solution (car (controller-test-solutions self)))
	(replans (controller-remaining-replans self))
	(exhaustive (plan-test-exhaustive (controller-current-test self))))
    ;; Two sorts of failures can occur.  We might have a solution
    ;; when we should be out of alternatives, or we might have an
    ;; incorrect solution.
    (cond ((and (eql replans 0) exhaustive)
	   (test-failure self "too many solutions")
	   nil)
	  (*generate*
	   (record-plan self solution check-result)
	   (and replans (> replans 0)))
	  (*sanity-check-only*
	   (report-for-sanity-check-only self check-result)
	   (and replans (> replans 0)))
	  ((check-plan self solution check-result)
	   (and replans (> replans 0)))
	  (t
	   (test-failure self "incorrect plan")
	   (if *sanity-check*
	       (and replans (> replans 0))
	     nil)))))

(defun solution-file (solution) ; -> pathname
  (merge-pathnames solution *test-directory*))

;;; Check-plan compares a saved and presumably correct plan description
;;; ("solution") with a description of the plan we've just generated.
;;; Both descriptions will be in files and when read by file->list must
;;; match the following structure.

(defstruct (test-plan-description (:type list) (:conc-name tpd-))
  stats
  action-levels
  effect-levels
  nodes
  tome
  gost
  psv-descriptions
  psv-equiv-classes
  rut
  world-1				;at node-1
  world-2)				;at node-2

;;; "Check-result" will be either a list (:OK stats) or a file name.  If
;;; it's (:OK stats), this means that KS-CHECK-PLAN has already found that
;;; the correct plan description and the test description are EQUAL.  When
;;; the result is not :OK, CHECK-PLAN compares the descriptions itself in
;;; order to report what the differences are.  CHECK-PLAN may use a less
;;; strict test than EQUAL; see TPD-EQUAL.

(declaim (notinline tpd-equal))		;redefinable

(defun check-plan (self solution check-result)
  (assert (not (null solution)))
  (count-domains-and-plans self)
  (when (list-beginning :ok check-result) ;see if the plan was as expected
    (setq *statistics-totals*
	  (add-alists *statistics-totals* (second check-result)))
    (return-from check-plan t))
  (let ((correct-plan (file->list (solution-file solution)))
	(test-plan (file->list check-result))
	(ok t))
    (unless (= (length correct-plan)
	       (length test-plan)
	       (length (make-test-plan-description)))
      (setq ok nil)
      (cerror "Continue anyway." "Lengths differ."))
    (setq *statistics-totals*
	  (add-alists *statistics-totals* (tpd-stats test-plan)))
    (flet ((check (accessor description)
	     (let ((correct (funcall accessor correct-plan))
		   (test (funcall accessor test-plan)))
	       (multiple-value-bind (same-p differences)
		                    (tpd-equal correct test description)
		 (unless same-p
		   (test-report self "Difference in ~A." description)
		   (when (equal description "Statistics")
		     (report-statistics self correct test))
		   (setq ok nil))
		 (when differences
		   (report-differences self differences description))))))
      (check #'tpd-stats             "Statistics")
      (check #'tpd-action-levels     "Action levels")
      (check #'tpd-effect-levels     "Effect levels")
      (check #'tpd-nodes             "Nodes")
      (check #'tpd-tome              "TOME")
      (check #'tpd-gost              "GOST")
      (check #'tpd-psv-descriptions  "PSV descriptions")
      (check #'tpd-psv-equiv-classes "PSV equivalence classes")
      (check #'tpd-rut               "Resource usage table")
      (check #'tpd-world-1           "World at node-1")
      (check #'tpd-world-2           "World at node-2")
      ok)))

(defun tpd-equal (correct test description) ; -> T/F, ((old . new) ...)
  (declare (ignore description))
  (values (equal correct test) '()))

(defun report-differences (self differences item-description)
  (test-report self "Differences for ~A are:" item-description)
  (dolist (diff (canonical-description-order differences))
    (let ((old (car diff))
	  (new (cdr diff)))
      (test-report self " ~3T ~S ~15T --> ~25T ~S" old new))))

(defun report-statistics (self old new)
  (dolist (stat new)
    (let* ((key (car stat))
	   (new-val (cdr stat))
	   (old-val (lookup key old)))
      (test-report self " ~3T ~S ~23T = ~S, ~33T was ~S"
	 key new-val old-val)))
  (let ((missing-old-stats
	 (stable-set-difference old new :key #'car)))
    (when missing-old-stats
      (test-report self "Old stats no longer present: ~S"
		   missing-old-stats))))

(defun report-for-sanity-check-only (self check-result)
  (let ((test-plan (file->list check-result)))
    (report-statistics self nil (tpd-stats test-plan))
    t))

(let ((current-domain nil))
  (defun count-domains-and-plans (self)
    (let ((d (plan-test-domain (controller-current-test self))))
      (incf *plan-count*)
      (when (or (= *plan-count* 1)			;1st plan
		(not (string= d current-domain)))	;change in domain
	(setq current-domain d)
	(incf *domain-count*)))))

(defun add-alists (a b)
  (cond ((null a) b)
	((null b) a)
	(t (mapcar #'(lambda (a.e b.e)
		       (assert (eq (alist-key a.e) (alist-key b.e)))
		       (cons (alist-key a.e)
			     (+ (alist-value a.e) (alist-value b.e))))
		   a
		   b))))

;;; Check-no-plan is called when we receive a :no-more-alternatives
;;; message from the Planner.  This is what we expect at the end of
;;; an exhaustive test.  In other cases, it counts as a failure.

(defun check-no-plan (self)
  (unless (and (eql 0 (controller-remaining-replans self))
	       (plan-test-exhaustive (controller-current-test self)))
    ;; Failure
    (test-failure self "missing solution")))

;;; Continue-from-sanity-check-p is called after a sanity check.
;;; It returns true if the current test should continue by replanning
;;; and false of the controller should go on to the next test instead.

(defun continue-from-sanity-check-p (self n-errors)
  (let ((expected-to-continue-p (controller-continue-p self)))
    ;; We can't continue if we're not expected to, but it's conceivable
    ;; that we might not continue even though we were expected to (e.g.
    ;; if there were too many errors).
    (cond ((> n-errors 0)
	   (test-failure self "failed sanity check")
	   expected-to-continue-p)
	  (t
	   expected-to-continue-p))))

;;; Test failure and other reports

(defun test-failure (self reason)
  (ensure-test-is-identified self)
  (warn "~A" reason)
  (format t "~&"))

(defun test-report (self format-string &rest format-args)
  (ensure-test-is-identified self)
  (format t "~&~?~%" format-string format-args))

(let ((last-identified-test nil)
      (last-remaining-replans nil))
  (defun ensure-test-is-identified (self)
    (let ((test (controller-current-test self))
	  (remaining-replans (controller-remaining-replans self)))
      (unless (and (eq test last-identified-test)
		   (eql remaining-replans last-remaining-replans))
	(let ((dom (plan-test-domain test))
	      (task (plan-test-task test))
	      (replans (plan-test-replans test)))
	  (format t "~&~%Test ~A ~A ~A:~%"
		    dom
		    task
		    (if (null replans)
			;; Only one solution is attempted, so this
			;; must be it.
			1
		      ;; Suppose we've got the 1st plan and the test
		      ;; specifies n replans.  Remaining-replans will
		      ;; still be n, and we want the result to be 1.
		      (1+ (- replans remaining-replans))))
	  (setq last-identified-test test
		last-remaining-replans remaining-replans))))))

;;; Recording plan descriptions

(defun record-plan (self solution plan-file)
  (assert (not (null solution)))
  (assert (string= (pathname-name plan-file) "test-result"))
  (let ((solution-file (solution-file solution)))
    ;; Solution-file may already exist and hence contain a plan description.
    ;; Plan-file must exist and must contain a description we just generated.
    (cond ((null (probe-file solution-file))
	   ;; No existing solution
	   (rename-file plan-file solution-file))
	  ((check-plan self solution-file plan-file)
	   ;; Existing solution is the same
	   (test-report self "No change in solution."))
	  (t
	   ;; Existing solution is different
	   (ensure-test-is-identified self)
	   (warn "The existing solution is different")
	   (backup-by-rename solution-file)
	   (rename-file plan-file solution-file)))))

(defun backup-by-rename (name)
  (let* ((path (pathname name))
	 (type (pathname-type path))
	 (backup-type
	  (make-pathname :type (concat-string type "~")))
	 (backup-path
	  (merge-pathnames backup-type path)))
    (when (probe-file backup-path)
      (delete-file backup-path))
    (rename-file path backup-path)))

;;; End
