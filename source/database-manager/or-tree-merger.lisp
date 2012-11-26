;;;; File: or-tree-merger.lisp
;;; Contains: Or-tree trim, merge, and forced-action functions
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: March 1996
;;; Updated: Fri Sep 11 16:39:19 1998 by Jeff Dalton
;;; Copyright: (c) 1996, AIAI, University of Edinburgh

(in-package :oplan)

;;; This file provides several ways to "reduce" or-trees by and-ing
;;; them with other constraints.  All return new trees when any changes
;;; are made, rather than modifying the trees given as arguments.  None
;;; remove redundant branches, remove redundant actions, or "lift" the
;;; subtrees of branches that have no (non-redundant) actions.  (So,
;;; e.g., merging a tree with itself will _not_ return a tree equal
;;; to the original, except perhaps in special cases such as null trees.)

;;; (trim-or-tree or-tree) removes branches that are inconsistent
;;; with the current plan state.

;;; (merge-or-trees or-trees) combines or-trees into a single tree,
;;; removing any branch that conflicts with actions on the path that
;;; leads to the branch.  Merging can result in large trees when there
;;; are few conflicts.  E.g. merging two trees with branch-n of n and m,
;;; respectively, may result in a tree with branch-n = n*m.

;;; (trim-or-tree-against-actions or-tree actions) removes branches that
;;; are inconsistent with the given actions.

;;; (or-tree-forced-actions or-tree) returns a list of the actions
;;; that appear on every path through the tree.  Such lists are useful
;;; as arguments to trim-or-tree-against-actions.


;;;; Or-tree trimmer

;;; Removes branches that are inconsistent with the current plan state.

(defun trim-or-tree (or-tree) ; -> or-tree or nil
  (new-or-tree (trim-or-branches (or-tree-branches or-tree))))

(defun trim-or-branches (branches)
  (loop for b in branches
	for r = (trim-or-branch b)
	when r collect r))

(defun trim-or-branch (branch) ; -> or-branch or nil
  (let ((actions (or-branch-actions branch))
	(subtree (or-branch-subtree branch)))
    (if (every #'action-looks-ok-p actions)
	(if subtree
	    (let ((trimmed-subtree (trim-or-tree subtree)))
	      (if trimmed-subtree
		  (make-or-branch :actions actions :subtree trimmed-subtree)
		nil))
	  (make-or-branch :actions actions :subtree nil))
      nil)))

(defun action-looks-ok-p (action)
  (let ((fn (ecase (car action)
	      (:link     #'link-action-looks-ok-p)
	      (:bind     #'bind-action-looks-ok-p)
	      (:restrict #'restrict-action-looks-ok-p)
	      ((:reachieve
		:update)
	       nil))))
    (if fn
	(or (apply fn (cdr action))
	    (progn
	      (dev-warn "Trimming ~S" action)
	      nil))
      t)))

(defun link-action-looks-ok-p (from-etag to-etag)
  (and (gop-link-looks-ok-p from-etag to-etag)
       (not (gop-precedes to-etag from-etag))))

(defun bind-action-looks-ok-p (var val)
  (assert (psv-p var))
  (let ((*bindings* nil))		;just in case /\/
    (if (psv-p val)
	(psv-can-match-psv-p var val)
      (psv-can-have-value-p var val))))

(defun restrict-action-looks-ok-p (var val)
  (assert (psv-p var))
  (let ((vr (psv-get-value var))
	(vl (value-if-psv val)))
    (or (eq vr :undef)
	(eq vl :undef)			;11 Sep 98 [jd]
	(not (eql vr vl)))))

(defun value-if-psv (x)
  (if (psv-p x) (psv-get-value x) x))


;;;; Or-tree merger

;;; The trees are AND-ed.

;;; /\/: This is a relatively simple version, because constraints aren't
;;; propagated as we go along a path through the tree.  For instance,
;;; if actions in the tree link NE-1 before NE-2 and NE-2 before NE-3,
;;; the merger won't notice that this implies that NE-1 is before NE-3.

;;; /\/: It might make sense to do some analysis of conflict possibilities
;;; and be able to return more than one tree if merging them doesn't look
;;; like a good idea.  For instance, if two trees don't refer to the same
;;; variables at all, then there won't be any binding conflicts between
;;; them.

(defun merge-or-trees (trees) ; -> or-tree
  (dolist (tree trees)
    (check-type tree (or null or-tree)))
  (if (null trees)
      nil
    (if (length=1 trees)
	(car trees)
      (let ((sorted-trees (stable-sort (copy-list trees) #'or-merge-lessp)))
	(or-tree-merger (car sorted-trees) (cdr sorted-trees) '())))))

(defun or-merge-lessp (t1 t2)
  (< (if (= 1 (or-tree-branch-1 t1)) 1 (or-tree-branch-n t1))
     (if (= 1 (or-tree-branch-1 t2)) 1 (or-tree-branch-n t2))))

(defun or-tree-merger (tree more-trees path) ; -> or-tree
  (if (null-or-tree tree)
      ;; No way means no way.
      nil
    ;; At this point we're basically copying a tree.
    (let ((surviving-branches
	   (filter-or-branches tree more-trees path)))
      (if surviving-branches
	  (new-or-tree surviving-branches)
	nil))))

(defun filter-or-branches (tree more-trees path) ; -> list of branches
  (delete nil
	  (mapcar #'(lambda (branch)
		      (filter-or-branch branch more-trees path))
		  (or-tree-branches tree))))

(defun filter-or-branch (branch more-trees path)
  (if (any-or-action-conflict (or-branch-actions branch) path)
      nil
    (let* ((subtree (or-branch-subtree branch))
	   (filtered-subtree
	    (if subtree
		(or-tree-merger
		  subtree more-trees (cons branch path))
	      (if more-trees
		  (or-tree-merger
		    (car more-trees) (cdr more-trees) (cons branch path))
		nil))))
      (if (and (null filtered-subtree)
	       (or subtree more-trees))
	  nil
	(make-or-branch
	  :actions (or-branch-actions branch)
	  :subtree filtered-subtree)))))

(defun any-or-action-conflict (actions path)
  ;; The path is a list of branches.  In effect, all actions
  ;; along the path will be ANDed, which may be impossible.
  (dolist (action actions nil)
    (when (ecase (car action)
	    (:link
	     (link-action-conflict-p action path))
	    (:bind
	     (bind-action-conflict-p action path))
	    (:restrict
	     (restrict-action-conflict-p action path))
	    ((:reachieve
	      :update)
	     nil))
      (return t))))

(defun link-action-conflict-p (action path)
  (let ((from-ne (second action))
	(to-ne (third action)))
    (dolist (branch path)
      (dolist (path-action (or-branch-actions branch))
	(case (car path-action)
          (:link
	   (let ((path-from-ne (second path-action))
		 (path-to-ne (third path-action)))
	     (when (and (equal path-from-ne to-ne)
			(equal path-to-ne from-ne))
	       (return-from link-action-conflict-p t)))))))))

(defun bind-action-conflict-p (action path)
  (let ((var (second action))
	(val (third action)))
    (dolist (branch path)
      (dolist (path-action (or-branch-actions branch))
	(case (car path-action)
          (:bind
	   (let ((path-var (second path-action))
		 (path-val (third path-action)))
	     (when (and (eq path-var var)
			(not (eql path-val val)))
	       (return-from bind-action-conflict-p t))))
	  (:restrict
	   (let ((path-var (second path-action))
		 (path-val (third path-action)))
	     (when (and (eq path-var var)
			(eql path-val val))
	       (return-from bind-action-conflict-p t)))))))))

(defun restrict-action-conflict-p (action path)
  (let ((var (second action))
	(val (third action)))
    (dolist (branch path)
      (dolist (path-action (or-branch-actions branch))
	(case (car path-action)
          (:bind
	   (let ((path-var (second path-action))
		 (path-val (third path-action)))
	     (when (and (eq path-var var)
			(eql path-val val))
	       (return-from restrict-action-conflict-p t)))))))))

(defun walk-path-actions (path fn)
  ;; Return true as soon an fn returns true.
  ;; If fn never returns true, return false.
  (dolist (branch path)
    (dolist (path-action (or-branch-actions branch))
      (let ((val (funcall fn path-action)))
	(when val
	  (return-from walk-path-actions val))))))



;;;; Forced actions

;;; Returns a list of actions that appear on every path through the tree.

(defun or-tree-forced-actions (or-tree)
  (if (null-or-tree or-tree)
      nil
    (rolling-intersection
      (or-tree-branches or-tree)
      :key #'or-branch-forced-actions
      :test #'equal)))

(defun or-branch-forced-actions (b)
  (let ((actions (or-branch-actions b))
	(subtree (or-branch-subtree b)))
    (if subtree
	(union actions (or-tree-forced-actions subtree) :test #'equal)
      actions)))

;;; Rolling-intersection should be better than using reduce because
;;; it can quit as soon as it gets the empty set.

(defun rolling-intersection (sets &key (key #'identity) (test #'eql))
  (if (null sets)
      '()
    (label roll ((result (funcall key (first sets)))
		 (more-sets (rest sets)))
      (if (or (null result) (null more-sets))
	  result
	(roll (intersection result (funcall key (first more-sets)) :test test)
	      (rest more-sets))))))


;;;; Trim branches that conflict with forced actions

;;; This is just trim-or-tree, but written as one function and with
;;; a different test for whether branches are trimmed.  It uses the
;;; action-conflict test from the or-tree merger, hence the fake-path.

(defun trim-or-tree-against-actions (or-tree actions)
  (let ((fake-path (list (make-or-branch :actions actions))))
    (label trim ((tree or-tree))
      (new-or-tree
        (loop for b in (or-tree-branches tree)
	      for r =
	        (let ((acts (or-branch-actions b))
		      (sub (or-branch-subtree b)))
		  (if (any-or-action-conflict acts fake-path)
		      nil
		    (if sub
			(let ((trimmed-sub (trim sub)))
			  (if trimmed-sub
			      (make-or-branch :actions acts
					      :subtree trimmed-sub)
			    nil))
		      (make-or-branch :actions acts :subtree nil))))
	    when r collect r)))))


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
