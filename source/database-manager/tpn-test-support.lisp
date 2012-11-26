;;; File:    tpn-test-support
;;; Purpose: Supports tests of the time point network manager
;;; Author:  Jeff Dalton <J.Dalton@ed.ac.uk>
;;;          AI Applications Institute, University of Edinburgh
;;; Updated: Sun Jan  3 19:14:29 1999 by Jeff Dalton

;;; A typical test has the form:
;;;   - Start with an empty net.
;;;   - Add some points.
;;;   - Add some constraints, checking whether an invalidity
;;;     is detected.
;;;   - Check the final min and max values of the points.

;;; The function TEST-NET makes it easier to write such tests.

;;; At present, points are affected only by adding constraints, not by
;;; changing a min or max directly.  This should suffice for version 3.x
;;; of O-Plan but perhaps not for later versions.

;;; There are no tests for deleting points or constraints, because
;;; the TPN algorithm doesn't support deletions or indeed anything that
;;; loosens contraints.


; (in-package :oplan-tpn)
(in-package :oplan-nodes)


;;; Test utilities

;;; (TEST-NET point-descriptions constraint-descriptions &key (reset t))
;;;
;;; (TEST-NET '((name min max) ...)             ; points
;;;           '((name1 name2 min max) ...))     ; constraints
;;;    ==> ((name final-min final-max) ...)     ; if success
;;; or ==> (failed-constraint ...)              ; if failure
;;;
;;; The keyword parameter :RESET determines whether to start a
;;; new net (:RESET T) or extend the one used in the previous test
;;; (:RESET NIL).  It defaults to T.  :RESET NIL should be used
;;; with caution, because it prevents tests from being independent.
;;;
;;; The point names used in a call to TEST-NET are only a convention
;;; of the test package; the actual TPN code doesn't know anything
;;; about them.  It is assumed that the names are unique within a net.
;;; Don't reuse a name in the expectation that this will change or
;;; replace the actual TPN point; it won't.
;;;
;;; TEST-NET adds the points to the net and then tries to add the
;;; constraints, in order, one by one.  If a constraint cannot be
;;; added w/o introducing an inconsistency, TEST-NET puts it in a
;;; list of failed constraints and then tries the next one.  If all
;;; the constraints succeed, TEST-NET returns a description of the
;;; points in the net; otherwise it returns descriptions of the
;;; failed constraints.  Both point and constraint descriptions
;;; have the same form as in the arguments to TEST-NET.
;;;
;;; A different sort of return value can occur in a couple of cases.
;;; When a constraint produces an inconsistency, the TPN manager is
;;; required to restore the net to the state it was in before the
;;; manager tried adding the constraint.  (An inconsistency may not
;;; be discovered until after some changes implied by the constraint
;;; have propagated through the net.)  TEST-NET checks that the max
;;; and min values of all points (that it knows about) have been
;;; restored correctly.  If not, it stops trying constraints and
;;; immediately returns a value of the form
;;;  
;;;    (:BAD-POINT-VALUES culprit actual-values correct-values)
;;;
;;; where the culprit is the constraint that led to the problem,
;;; and the point values have the usual form (name min max).
;;;
;;; So there is an implicit check, after each failure, that the net
;;; has been restored correctly.  Similar checks are made in a few
;;; other cases.

(defstruct test-net
  "TPN information held at the test level."
  (point-alist '())
  (point-names '())
  (constraint-tags '())
  (correct-point-values '()))

(defvar *test-net* nil "The current test net.")

(defun name->point (name &optional (net *test-net*))
  (cdr (or (assoc name (test-net-point-alist *test-net*))
	   (error "No point named ~S." name))))

(defun test-net (point-descriptions constraint-descriptions
		 &key (reset t))
  (when reset
    (setq *test-net* (make-test-net))
    (reset-tpn))
  (catch 'test-net
    (extend-net *test-net* point-descriptions)
    (let ((failed-constraints
	   (remove-if #'try-constraint constraint-descriptions)))
      (or failed-constraints
	  (list-point-values *test-net*)))))

(defun check-point-values (net suspect)
  (let ((correct (test-net-correct-point-values net))
	(actual (list-point-values net)))
    (unless (equal actual correct)
      (throw 'test-net
	(list :bad-point-values suspect actual correct)))))

(defun extend-net (net point-descriptions)
  (let ((new-point-alist
	 (mapcar #'(lambda (descr)
		     (let ((name (first descr))
			   (min (second descr))
			   (max (third descr)))
		       (cons name
			     (tpn-add-time-point min max))))
		 point-descriptions)))
    (setf (test-net-point-alist net)
	  (append (test-net-point-alist net) new-point-alist))
    (setf (test-net-point-names net)
	  (append (test-net-point-names net) (mapcar #'car new-point-alist)))
    (setf (test-net-correct-point-values net)
	  (append (test-net-correct-point-values net) point-descriptions))
    (check-point-values net :initial-setup)
    net))

(defun try-constraint (descr &optional (net *test-net*)) ; -> success?
  (let ((pre-point (name->point (first descr) net))
	(post-point (name->point (second descr) net))
	(min (third descr))
	(max (fourth descr)))
    (let ((constraint
	   (tpn-add-time-constraint pre-point post-point min max)))
      (cond (constraint
	     (push constraint (test-net-constraint-tags net))
	     (setf (test-net-correct-point-values net)
		   (list-point-values net)))
	    (t (check-point-values net descr)))
      constraint)))

;;; Routines for looking at the results.

(defun list-point-values (&optional (net *test-net*))
  (mapcar #'(lambda (name)
	      (let ((tp-s (tpn-get-tpoint (name->point name))))
		(list name (tpoint-min tp-s) (tpoint-max tp-s))))
	  (test-net-point-names net)))

;;; Low-level utilities

(defun infinity ()
  *infinity*)

(defun reset-tpn ()
  (tpn-clear-tpn))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
