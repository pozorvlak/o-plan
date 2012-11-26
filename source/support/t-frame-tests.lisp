;;;; File: t-frame-tests
;;; Contains: Tests of the test framework
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: June 1994
;;; Updated: Sun Feb 28 19:37:24 1999 by Jeff Dalton
;;; Copyright: (c) 1994, AIAI, University of Edinburgh

(in-package #+:oplan :oplan-test-support
	    #-:oplan :test)

(define-test-module :t-frame-tests)
(in-test-module :t-frame-tests)

(defvar *wrapper-to-test* 'standard-test-wrapper)

(define-test-wrapper :t-frame-tests (test)
  (funcall *wrapper-to-test* test))

;;; These tests should normally be the first ones done.

(define-test-group test-result-tests
  ('a ==> 'a)
  ('a ==> :any)
  ('a)					;same as ==> :any
  ('a ==> 'a :errors 0)
  ('a ==> 'a :errors :any)
  ('a ==> :any :errors :any)
  ('a :errors :any)			;assumes ==> :any

  ;; :True and :false
  ('a ==> :true)
  (10 ==> :true)
  (nil ==> :false))

(define-test-group test-predicate-tests
  ;; The :test predicate is called with the result as its first
  ;; argument and the expected result as its second argument.
  ('a  ==> 'a  :test #'eq)
  ("a" ==> "A" :test #'string-equal)
  ('a  ==> 'symbol :test #'typep))

(define-test-group test-error-tests
  ((error "Test")  :errors 1)
  ((error "Test")  :errors :some)
  ((error "Test") ==> 'simple-error :test #'typep :errors 1)

  ;; Check that assert works as well as error.
  ((assert (= 1 2)) :errors 1)

  ;; Check that the test framework doesn't break when we have an error
  ;; when there's an expected value.
  ((error "Test") ==> 'something
   :errors 1
   :expected-to-fail t)

  ;; Check that we can have an error and an expected value even
  ;; though the :test could not compare an error object with the
  ;; expected value.  (The standard wrapper will return the
  ;; error object as the value.)
  ((error "Test") ==> 1 :test #'=
   :errors 1
   :expected-to-fail t))

(defvar *-a-* nil)

(define-test-group (test-group-with-tests
		      :with ((*-a-* 10) (a 'apple) (b 20)))
  ((boundp '*-a-*)       ==> :true)
  ((symbol-value '*-a-*) ==> 10)
  (*-a-* ==> 10)
  (  a   ==> 'apple)
  (  b   ==> 20))

(define-test-group test-module-tests
  ((exists-test-module :all-tests)     ==> :true)
  ((exists-test-module :t-frame-tests) ==> :true)
  ((test-module-p (exists-test-module :all-tests))     ==> :true)
  ((test-module-p (exists-test-module :t-frame-tests)) ==> :true)
  ((eq (exists-test-module :all-tests)
       (find-test-module :all-tests))
   ==> :true)
  ((eq (exists-test-module :t-frame-tests)
       (find-test-module :t-frame-tests))
   ==> :true)
  ((eq (find-test-module :all-tests)
       (car *all-test-modules*))
   ==> :true)
  ((eq (find-test-module :all-tests)
       (car (last (tm-descendants (find-test-module :all-tests)))))
   ==> :true)
  ((member (find-test-module :t-frame-tests)
	   (tm-descendants (find-test-module :all-tests)))
   ==> :true)
  ((set-eql *all-test-modules*
	    (tm-descendants (find-test-module :all-tests)))
   ==> :true)
  )

#-:oplan
(defun set-eql (set1 set2)
  ;; Set-eql dertermines whether to two sets are equal when the elements
  ;; can be compared by eql.
  (null (set-exclusive-or set1 set2)))

(define-test-group test-wrapper-existence-tests
  ((tm-test-wrapper (find-test-module :all-tests))
   ==> 'standard-test-wrapper)
  ((tm-test-wrapper (find-test-module :t-frame-tests))
   ==> 't-frame-tests-test-wrapper))

;;; /\/: Should find a way to test get-test-wrapper too.

(define-test-group (test-wrapper-tests
		    :with ((*wrapper-to-test*
			    #'(lambda (test)
				(let ((*a* 10))
				  (declare (special *a*))
				  (funcall #'standard-test-wrapper test))))))
    ((locally (declare (special *a*))
       *a*)
     ==> 10)
  )

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
