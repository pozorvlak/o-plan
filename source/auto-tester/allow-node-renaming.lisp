;;;; File: allow-node-renaming.lisp
;;; Contains: Auto-tester patch for some cases where node names may change
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: August 1994
;;; Updated: Sun Nov 10 01:50:13 1996 by Jeff Dalton
;;; Copyright: (c) 1994, AIAI, University of Edinburgh

;;; Call node-renaming-on to cause tpd-equal to allow and report
;;; changes in node names and node-renaming-off to change it back.

;;; (node-renaming-on) happens automatically when this file is loaded.

;;; This version assumes that the renaming leaves nodes so that
;;; equivalent nodes have the same position in the node description;
;;; in effect, equivalence is found by taking nodes in the order in
;;; which they were created (which is the same as the numeric ordering
;;; of their numbers).

(in-package :oplan-autotester)

(export '(node-renaming-on
	  node-renaming-off))

(defun node-renaming-on ()
  (advice+ 'tpd-equal 'allow-node-renaming
    #'(lambda (previous)
	(declare (ignore previous))
	;; When we compare Nodes, we set the rename map.  In all other
	;; cases, we use the current map to rename "NODE-" symbols on
	;; the test side and then call EQUAL.
	(let ((rename-map nil))
	  #'(lambda (correct test item-description)
	      (cond ((string= item-description "Nodes")
		     (multiple-value-bind (result renames)
			 (equal-with-1-1-renames
			    correct
			    test
			    :renamable-p #'node-name-p)
		       (setq rename-map renames)
		       (values result renames)))
		    (t
		     (values
		       (equal (canonical-description-order
			        (sublis rename-map
					correct))
			      (canonical-description-order
			        test))
		       '()))))))))

(node-renaming-on)			;when loaded

(defun node-renaming-off ()
  (advice- 'tpd-equal 'allow-node-renaming))

(defun node-name-p (x)
  (and (symbolp x)
       (let ((x-s (symbol-name x)))
	 (and (> (length x-s) 5)
	      (char= #\N (schar x-s 0))
	      (char= #\O (schar x-s 1))
	      (char= #\D (schar x-s 2))
	      (char= #\E (schar x-s 3))
	      (char= #\- (schar x-s 4))))))

;;; End
