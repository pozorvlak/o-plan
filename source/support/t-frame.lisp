;;;; File: t-frame.lisp
;;; Contains: Test framework
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: March 1992
;;; Updated: Sun Feb 28 19:48:52 1999 by Jeff Dalton
;;; Copyright: (c) 1992, 1993, 1994 AIAI, University of Edinburgh

(cl:defpackage #+:oplan :oplan-test-support
	       #-:oplan :test
  (:export #:*all-test-modules*
	   #:find-test-module #:exists-test-module #:tm-descendants
	   #:print-test-module-tree
	   #:run-all-tests
	   #:test-thunk			;for test wrappers

	   #:*failed-tests*

	   #:define-test-module #:in-test-module
	   #:define-test-wrapper #:standard-test-wrapper
	   #:define-test-group #:==> #:run-all-tests))

(cl:in-package #+:oplan :oplan-test-support
	       #-:oplan :test)

#+:oplan
(use-package :oplan-util)

;;; Tests are divided into a tree of "test modules" and within modules
;;; into "test groups".  The module :ALL-TESTS is the root of the module tree.
;;;
;;; (RUN-ALL-TESTS [module-name]) runs all tests in the subtree that
;;; contains the indicated module and all of its descendants.  The
;;; module-name defaults to :ALL-TESTS.  The tests in a child module are
;;; always run before those of its parent, which reflects the general
;;; rule that lower-level or more basic tests are tried first.  Within
;;; a module, test groups are tried in the order in which they were
;;; first defined.
;;;
;;; (DEFINE-TEST-MODULE module-name [parent-name]) defines a test module.
;;; The parent-name defaults to :ALL-TESTS.
;;;
;;; (IN-TEST-MODULE module-name) sets the module in which subsequent
;;; test groups are defined.
;;;
;;; (DEFINE-TEST-WRAPPER module-name parameters &BODY body) defines a
;;; function of one argument, typically called TEST, that is used to run
;;; a test when it's from the indicated module.  The wrapper is so-called
;;; because it can set up an environment "around" the test.  It's not
;;; necessary to define a test wrapper for each module, because a module
;;; will inherit the wrapper of its parent.  If no other ancestor provides
;;; a wrapper, a module will inherit STANDARD-TEST-WRAPPER from :ALL-TESTS.
;;; Wrappers are discussed in more detail on the "Tests" section below.
;;;
;;; An example may make it clearer how a test module is defined.  The
;;; TF Compiler's parser-tests are a good example because they require
;;; a wrapper.  The module definition begins as follows:
;;;
;;;   (define-test-module :tf-parser :tf-compiler)
;;;
;;;   (in-test-module :tf-parser)
;;;
;;;   (define-test-wrapper :tf-parser (test)
;;;     (let ((*package* (find-package :tf-compiler)))
;;;       (funcall (test-thunk test))))
;;;
;;;   ... test group definitions ...
;;;
;;; DEFINE-TEST-GROUP defines a function that performs a set of related
;;; tests.  The function takes a optional argument that tells it which
;;; of its tests to perform.  This argument can be :ALL, to run all tests;
;;; a number, to run a single test; or a list of numbers.  The first test
;;; in a group is given number 0.  Test group functions are usually called
;;; via RUN-ALL-TESTS, but they can also be called directly.
;;;
;;; The syntax for defining a test group is:
;;;
;;;   (DEFINE-TEST-GROUP group-spec test-description*)
;;;
;;; where
;;;
;;;   group-spec --> name | (name group-parameter*)
;;;   group-parameter --> :WITH let-list
;;;   let-list --> { variable | (variable initform) }*
;;;
;;;   test-description --> (test-form test-parameter*)
;;;   test-parameter --> ==> result-form
;;;                   |  :ERRORS errors-form
;;;                   |  :TEST test-function-form
;;;
;;; The parameter ":WITH let-list" is used to define variables local to
;;; the test group function.  Tests are defined inside a LET that binds
;;; those variables.  The variables are initialized on entry to the group
;;; function (not for each test).
;;;
;;; The value of the result-form is the value that the test-form is
;;; expected to return.  This expected result is compared with the actual
;;; result using the function that is the value of the test-function-form.
;;; The function is called with the actual result as its first argument
;;; and the expected result as its second argument.  The order is defined
;;; so that test authors will be able to use asymmetric preducates such 
;;; as TYPEP as :TEST funcions.
;;;
;;; The test function defaults to EQUAL.  However, there are some special
;;; cases in which the test function is not called.  If the expected result
;;; is :ANY, then all result values are accepted.  If it is :TRUE, then all
;;; true (i.e., non-NIL) values are accepted.  :FALSE is equivalent to NIL.
;;; :ANY is the default.
;;;
;;; Note that the result-form and the test-function-form are evaluated.
;;; For example:
;;;
;;;   (define-test-group read-tests
;;;     ((read-from-string "abc")     ==> 'abc :test #'eq)
;;;     ((read-from-string "(+ 1 2)") ==> '(+ 1 2)))
;;;
;;; The value of the errors-form specifies the number of errors that are
;;; expected.  Possible values are a number, to require exactly that many
;;; errors; :ANY to allow any number of errors, including zero; and :SOME
;;; to require that there be at least one error.  The default is 0.
;;;
;;; Ondinarily, the maximum number of errors that can occur is 1, because
;;; the first error is captured by a handler-case in the test wrapper,
;;; ending the test.  But it's possible for a test wrapper and a test
;;; to cooperate in running up a higher total.  This is explained further
;;; under "Tests" below.


;;; Parameters

(defparameter *test-io* (make-synonym-stream '*terminal-io*))

(defvar *all-test-modules* nil)

(defvar *test-module* nil)		;module being defined

(defvar *module-being-tested* nil)	;module being tested

(defvar *test-wrapper* 'not-a-test-wrapper)

(defvar *failure-count* 0)		;accumulated by run-all-tests

(defvar *failed-tests* '()		;accumulated by run-all-tests
  "List of (module-name group-name test-id) lists.")


;;; Tests

;;; The source code of each test becomes a function of zero arguments --
;;; a thunk -- that is called to run the test.  DEFINE-TEST-GROUP is
;;; defined so that when a test-group definition is compiled the thunks
;;; will be compiled functions.

;;; Test thunks are not called directly.  Instead, DEFINE-TEST-GROUP
;;; defines a function of the same name as the test group.  Tests can
;;; be run only by calling the test group function.  Moreover, the
;;; test thunks are closures created in the body of the function and
;;; can therefore refer to local variables that the function binds.
;;; The local variables are specified by DEFINE-TEST-GROUP's :WITH
;;; parameter.

;;; However, even the test group function does not call test thunks
;;; directly.  Instead it calles a test wrapper defined by the test
;;; group's test module.  The test wrapper is responsible for setting
;;; up any required environment (such as *package*), calling the thunk,
;;; and returning two values: the value returned by the thunk and the
;;; number of errors that occurred while the thunk computed that value.

;;; Typically, the wrapper sets up handlers for any errors that might
;;; occur, and it may use restarts to allow the thunk to continue after
;;; an error.  That's one way in which the error count might be > 1.
;;; Another way would be for the wrapper to allow the test to return
;;; an error count.  That's what happens in the TF Compiler's parser
;;; tests.  Standard-test-wrapper (below) allows at most 1 error.  When
;;; an error occurs, it returns the condition that was signalled as the
;;; value and 1 as the error count.

(defstruct test
  source
  (thunk 'not-a-test-thunk)
  (test 'equal)				;not #'equal so will see trace etc.
  (result :any)
  (errors 0)
  (expected-to-fail nil))		;for testing the test framework


;;; Test modules

(defstruct (test-module (:conc-name tm-) (:print-function print-test-module))
  (name (error "No name for test module"))
  (parent nil)
  (children '())
  (test-groups '())
  (test-wrapper nil))			;nil means inherit

(defun print-test-module (module stream depth)
  (declare (ignore depth))
  (format stream "#<test-module ~S>" (tm-name module)))

(defun exists-test-module (name)
  (find name *all-test-modules* :key #'tm-name :test #'string=))

(defun find-test-module (name)
  (or (exists-test-module name)
      (error "There is no test module named ~S." name)))

(defun ensure-test-module (name &rest initargs)
  (or (exists-test-module name)
      (progn
	(setq *all-test-modules*
	      (nconc *all-test-modules*
		     (list (apply #'make-test-module :name name initargs))))
	(find-test-module name))))

(ensure-test-module :all-tests		;default parent module
  :test-wrapper 'standard-test-wrapper)

(defun tm-descendants (tm)
  ;; Returns a list of tm and all its descendants with children
  ;; (hence lower-level) modules always before their parents.
  ;; Note that we arrange for the module :all-tests to have all test
  ;; modules as descendants.
  (reverse (tsort tm #'tm-children)))

(defun print-test-module-tree (&optional (root :all-tests))
  ;; List children, indented, under parent.
  (label walk ((at (find-test-module root))
	       (level 0))
    (format t "~&~vT~S~%" (* 3 level) (tm-name at))
    (dolist (c (tm-children at))
      (walk c (1+ level))))
  (values))

(defun run-all-tests (&optional (module :all-tests))
  (setq *failed-tests* '())
  (let ((*failure-count* 0)
	#+:undef (*failed-tests* '()))
    (mapc #'run-tm-tests
	  (tm-descendants
	   (find-test-module module)))
    (format *test-io* "~&~D Failed tests.~%" *failure-count*)
    *failed-tests*))

(defun run-tm-tests (module)
  (format *test-io* "~&~%Module: ~S~%" (tm-name module))
  (mapc #'funcall (tm-test-groups module)))


;;; Module definitions

;;; It should be possible to reload module definitions and to change
;;; them in various ways when reloading them.

;;; It should also be possible to compile module and test group
;;; definitions.  So we have to establish the name->module mapping
;;; at compile- as well as load-time so that IN-TEST-MODULE will work.
;;; In practice, we establish the whole module tree, but not much of
;;; its contents.

(defmacro define-test-module (name &optional (parent :all-tests))
  `(eval-when (eval compile load)
     (def-test-module ',name ',parent)))

(defun def-test-module (name parent-name)
  (let ((module (ensure-test-module name))
	(parent (find-test-module parent-name)))
    (unless (eq parent (tm-parent module))
      (when (tm-parent module)
	;; Changing parent.
	(let ((old-parent (tm-parent module)))
	  (warn "Changing parent of test module ~S from ~S to ~S."
		name (tm-name old-parent) (tm-name parent))
	  (setf (tm-children old-parent)
		(delete module (tm-children old-parent)))))
      ;; Set module's parent.
      (setf (tm-parent module) parent)
      ;; Add module to the list of its parent's children.
      (assert (not (member module (tm-children parent))))
      (setf (tm-children parent)
	    (nconc (tm-children parent) (list module))))
    module))


;;; (IN-TEST-MODULE name) determines the module in which subsequent
;;; test groups are defined.

(defmacro in-test-module (name)
  `(eval-when (eval compile load)
     (setq *test-module* (find-test-module ',name))))


;;; Test wrappers

;;; The role of wrappers in running tests is described in the "Tests"
;;; section above.

;;; Test wrappers may call STANDARD-TEST-WRAPPER so that it can do
;;; most of the work.

;;; (DEFINE-TEST-WRAPPER module-name parameters &BODY body)

(defmacro define-test-wrapper (module-name parameters &body body)
  (let ((wrapper-name (make-test-wrapper-name module-name)))
    `(progn
       (setf (tm-test-wrapper (find-test-module ',module-name))
	     ',wrapper-name)
       (defun ,wrapper-name ,parameters
	 . ,body))))

(defun get-test-wrapper (module)
  ;; The wrapper can be inherited.
  (do ((m module (tm-parent m)))
      ((null m) nil)
    (let ((w (tm-test-wrapper m)))
      (when w (return w)))))

(defun make-test-wrapper-name (module-name)
  (concat-name module-name "-TEST-WRAPPER"))

(defun standard-test-wrapper (test)
  (handler-case (values
		  (funcall (test-thunk test))
		  0)
    (error (c)
      ;; Report the error if no error was expected.
      (when (eql (test-errors test) 0)
        (format *test-io* "~&~@(~A~): ~A~%" (type-of c) c))
      ;; Return the error as the value and 1 as the number of errors.
      (values c 1))))


;;; Test groups

;;; /\/: It would be better to have test group structs rather than
;;; only functions.  When there's only a function, the function can
;;; know its module, but we can't tell from outside what it is.

;;; /\/: The group function knows the module only so we can call the
;;; function directly, not just via RUN-ALL-TESTS.

(defmacro define-test-group (name &rest tests)
  (apply #'(lambda (name &key ((:with let-list)))
	     `(progn
	        (add-tm-test-group ',name *test-module*)
	        (defun ,name (&optional (-which-tests- :all))
		  (let ,let-list
		    (run-requested-tests
		       ',(tm-name *test-module*)
		       ',name
		       -which-tests-
		       ,@(mapcar #'expand-test tests))))))
	 (if (consp name) name (list name))))

(defun expand-test (test-description)
  (let* ((source (car test-description))
	 (parameters (cdr test-description))
	 (result-form (getf parameters '==> :any)))
    (remf parameters '==>)
    `(make-test :source ',source
                :thunk #'(lambda () ,source)
                :result ,result-form
                ,@parameters)))

(defun run-requested-tests (module-name group-name which-tests &rest tests)
  (let* ((*module-being-tested* (find-test-module module-name))
	 (*test-wrapper* (get-test-wrapper *module-being-tested*)))
    (format *test-io* "~&~@(~S~): " group-name)
    (cond ((eq which-tests :all)
	   (do ((remaining-tests tests (cdr remaining-tests))
		(i 0 (1+ i)))
	       ((null remaining-tests) nil)
	     (run-test group-name i (first remaining-tests))))
	  ((numberp which-tests)
	   (run-test group-name which-tests (elt tests which-tests)))
	  (t
	   (dolist (n which-tests)
	     (run-test group-name n (elt tests n)))))
    (format *test-io* "~&")))


;;; Adding test groups to test modules

;;; At present, a group can't be moved from one module to another.
;;; Indeed, it might be an error if something like that seems to be
;;; happening, and for now at least that's how we'll always regard it.

;;; Note that define-test-group uses the group name as-is rather
;;; than constructing a name that includes the module name.  That's
;;; another reason it's important to notice something that looks
;;; like a module change.

(defmacro test-group-module (name)
  `(get ,name 'test-group-module))

(defun add-tm-test-group (group &optional (module *test-module*))
  ;; Group is a name, module a module struct.
  (let ((current-module (test-group-module group)))
    (when (and current-module
	       (not (eq current-module module)))
      (error "Attempt to move test group ~S from ~S to ~S."
	     group current-module module))
    ;; Add group if not already there.
    (cond ((member group (tm-test-groups module))
	   (assert (eq current-module module)))
	  (t
	   (setf (test-group-module group) module)
	   (setf (tm-test-groups module)
		 (nconc (tm-test-groups module) (list group)))))))


;;; Running tests

(defun run-test (group-name id test)
  ;; N.B. It's up to the test wrapper to catch errors and count them.
  ;; The number of errors is the 2nd value returned by the wrapper.
  (format *test-io* "~S; " id)
  (force-output *test-io*)
  (let ((expected-result (test-result test))
	(expected-errors (test-errors test))
	(equality-test (test-test test))
	(failed nil)
	(*print-pretty* t))
    (multiple-value-bind (result errors) (funcall *test-wrapper* test)
      ;; Check that result is as expected.
      (unless (or (eq expected-result :any)
		  (and (eq expected-result :true)
		       result)			;even if it's an error? /\/
		  (and (eq expected-result :false)
		       (not result))
		  (handler-case (funcall equality-test result expected-result)
		    (error (c)
		      (format *test-io*
			"Can't compare with expected result because:~%~A"
			c)
		      nil)))
	(setq failed t)
	(format *test-io* "~&Failed test ~S: ~S.~%~
                           -- Result was: ~S,~%~
                           -- but should be: ~S~
                           ~@[~%-- when compared by: ~S~].~%"
		id (test-source test)
		result expected-result
		(if (or (eq equality-test 'equal) (eq equality-test #'equal))
		    nil
		    equality-test))
	(force-output *test-io*))
      ;; Check that errors are as expected.
      (unless (or (eq expected-errors :any)
		  (and (eq expected-errors :some)
		       (numberp errors)
		       (plusp errors))
		  (and (eql expected-errors 0)
		       (null errors))
		  (eql expected-errors errors))
	(setq failed t)
	(format *test-io* "~&Failed test ~S: ~S.~%~
                           -- Expected ~S errors, but found ~S.~%"
		id (test-source test) expected-errors errors)
	(force-output *test-io*))
      ;; Handle cases that are expected to fail.
      (when (test-expected-to-fail test)
	(format *test-io*
		(if failed "-- Expected to fail.~%"
		           "-- Expected to fail but didn't.~%"))
	(setq failed (not failed)))
      (when failed
	(incf *failure-count*)
	(let ((failure-record
	       (list (tm-name *module-being-tested*)
		     group-name
		     id)))
	  (unless (member failure-record *failed-tests* :test #'equal)
	    (setq *failed-tests*
		  (nconc *failed-tests* (list failure-record))))))
      nil)))


;;;; Utilities

#-:oplan
(progn

(defun concat-name (s1 s2 &rest more)
  (intern (apply #'concatenate 'string
		 (string s1) (string s2) (mapcar #'string more))))

(defun concat-string (s1 s2 &rest more)
  (apply #'concatenate 'string
	 (string s1) (string s2) (mapcar #'string more)))

;;; Topological sort (returning ancestors before descendants).

(defun tsort (root children-fn &key (hash-test #'eq)) ; -> list(node)
  (let ((marks (make-hash-table :test hash-test))
	(result '()))
    (macrolet ((mark (node) `(gethash ,node marks)))
      (labels
	  ((walk (at)
	     (ecase (mark at)
	       ((:start)
		(error "Cycle involving ~S." at))
	       ((:finish)
		;; already processed
		)
	       ((nil)
		(setf (mark at) :start)
		(dolist (child (funcall children-fn at))
		  (walk child))
		(push at result)
		(setf (mark at) :finish)))))
	(walk root)
	result))))

); end #-:oplan

;;; End
