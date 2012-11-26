;;;; File: or-tree-functions.lsp
;;; Contains: Basic operations on or-trees
;;; Author: Jeff Dalton
;;; Created: March 1996
;;; Updated: Mon Dec  9 03:37:59 1996 by Jeff Dalton
;;; Copyright: (c) 1996, AIAI, University of Edinburgh

(in-package :oplan-or-trees)

(defun-export new-or-tree (branches)
  (assert (listp branches))
  (assert (every #'or-branch-p branches))
  (and branches
       (make-or-tree
	 :branches branches
	 :branch-1 (length branches)
	 :branch-n (loop for b in branches
			 sum (let ((s (or-branch-subtree b)))
			       (if s
				   (or-tree-branch-n s)
				 1))))))

(defun-inline null-or-tree (tree)
  (or (null tree)
      (null (or-tree-branches tree))))

(defun-export copy-entire-or-tree (or-tree)
  (new-or-tree
    (mapcar #'(lambda (b)
		(make-or-branch
		  :actions (copy-tree (or-branch-actions b))
		  :subtree (if (or-branch-subtree b)
			       (copy-entire-or-tree (or-branch-subtree b))
			     nil)))
	    (or-tree-branches or-tree))))


;;;; Comparing or-trees

(defun-export equal-or-trees (t1 t2)
  (cond ((eq t1 t2) t)
	((null t1) nil)
	((null t2) nil)
	(t (and (eql (or-tree-branch-1 t1) (or-tree-branch-1 t2))
		(eql (or-tree-branch-n t1) (or-tree-branch-n t2))
		(every #'equal-or-branches
		       (or-tree-branches t1)
		       (or-tree-branches t2))))))

(defun-export equal-or-branches (b1 b2)
  (or (eq b1 b2)
      (and (equal (or-branch-actions b1) (or-branch-actions b2))
	   (equal-or-trees
	    (or-branch-subtree b1)
	    (or-branch-subtree b2)))))


;;;; Tree properties

(defun-export some-work-to-do-p (or-tree)
  ;; Returns t if there are some bindings, restrictions, or linkings to
  ;;be made, or some deletors to remove.
  (check-type or-tree (or null or-tree))
  (and or-tree
       (or-tree-branches or-tree)	;/\/ do we need this?
       (some #'(lambda (branch)
		 (or (or-branch-subtree branch)
		     (notevery #'(lambda (action) (eq (car action) :UPDATE))
			       (or-branch-actions branch))))
	     (or-tree-branches or-tree))))


;;;; Displaying or-trees in a more or less readable form.

(defun-export display-or-tree (tree)
  (label walk ((tree tree)
	       (level 0))
    (when tree
      (format t "~&~vT#<or ~S ~S>:"
	      (* level 3) (or-tree-branch-1 tree) (or-tree-branch-n tree))
      (loop for branch in (or-tree-branches tree)
	    as count from 0
	    do (display-or-branch-actions branch count level)
	       (walk (or-branch-subtree branch)
		     (1+ level))))))

(defun display-or-branch-actions (branch count level)
  (let ((actions (or-branch-actions branch))
	(tab (* level 3)))
    (format t "~&~vT~S: ~S~%" tab count (car actions))
    (dolist (action (cdr actions))
      (format t "~&~vT   ~S~%" tab action))))


;;;; Pretty printing functions.

(defun pretty-print-or-tree (stream or-tree)
  (xp-format stream "#<or ~S ~S>"
	     (or-tree-branch-1 or-tree)
	     (or-tree-branch-n or-tree)))

(set-pretty-printer 'or-tree #'pretty-print-or-tree)


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
