;;;; File: or-trees.lisp
;;; Contains: Definition of or-tree and or-branch structures
;;; Author: Jeff Dalton (retaining some of RBK's version)
;;; Created: Mon Dec  2 15:33:15 1991
;;; Updated: Fri Nov 29 00:00:49 1996 by Jeff Dalton
;;; Copyright: (c) 1992, 1996 AIAI, University of Edinburgh

;;;; KS-OR info record.

(in-package :oplan-or-trees)

(use-package :oplan-util)

(export 'null-or-tree)
(export 'merge-or-trees)

#|

    Or-trees used to look like this:

    or-tree ::= (choice*)
    choice ::= (or-info-rec . or-tree)

    "Choice" is as in "1st choice", "2nd choice", etc: one of a
    number of possibilities.

    An or-tree is processed by picking one choice and posting the 
    others in an alternative.

    A choice is processed by performing the actions indicated by the
    or-info-rec and then processing the or-tree.

|#
#|

The actions field is a list of lists, of the form:
(<keyword> . <args>)
where <keyword> is one of:
:LINK - with two args giving the node-ends to link in the order from, to.
:BIND - with two args giving the variable to bind, and the value to bind it to.
:RESTRICT - with two args giving the variable to restrict, and the value to
restrict it to.
:REACHIEVE - with the condition as the first argument, and the current
contributor list as the second, which can then be used to decide on the next
appropriate method to use for resatisfying the condition.
:UPDATE - Updates the contributors of the condition to be the first argument.

|#

(defstruct-export or-tree		;~= a list of "choices"
  (branches '())
  (branch-1 0)
  (branch-n 0))

(defstruct-export or-branch		;was ~= an or-info-rec
  (actions '())
  (subtree nil))

#+akcl
(eval-when (compile)
  (si::freeze-defstruct 'or-tree)
  (si::freeze-defstruct 'or-branch))


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
