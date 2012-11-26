;;;; File: or-tree-tests.lisp
;;; Contains: Tests of some or-tree algorithms
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: November 1996
;;; Updated: Sat Dec  7 16:33:37 1996 by Jeff Dalton
;;; Copyright: (c) 1996, AIAI, University of Edinburgh

(in-package :oplan)

(define-test-module :or-trees)

(in-test-module :or-trees)

(define-test-group (or-tree-utility-tests
		     :with ((a-tree
			     (new-or-tree
			       (list
				 (make-or-branch
				   :actions '((:bind a v1)))
				 (make-or-branch
				   :actions '((:bind b v2))
				   :subtree
				     (new-or-tree
				       (list
					 (make-or-branch
					   :actions '((:bind a v3)))))))))))

  ((equal-or-trees a-tree a-tree)
   ==> :true)

  ((equal-or-trees a-tree (copy-entire-or-tree a-tree))
   ==> :true)

  ((equal-or-trees			;tests equal-or-trees and tree->or-tree
     a-tree
     (tree->or-tree
       '(( ((:bind a v1)) . nil )
	 ( ((:bind b v2))
	   . (( ((:bind a v3)) . nil ))))))
   ==> :true)

  ((equal-or-trees			;can we detect non-equal trees?
     a-tree
     (tree->or-tree
       '(( ((:bind a v1)) . nil )
	 ( ((:bind b v2))
	   . (( ((:bind a v4)) . nil ))))))
   ==> :false)

  ((equal-or-trees			;equivalent but not equal
     a-tree
     (new-or-tree (list (make-or-branch :subtree a-tree))))
   ==> :false)

  ((or-tree-possible-values 'a a-tree)
   ==> '(v1 v3)
   :test #'set-eql)

  ((or-tree-possible-values 'b a-tree)
   ==> '(v2)
   :test #'set-eql)

  )


(define-test-group or-tree-merge-tests

  ((or-tree-possible-values
     'a
     (merge-or-trees
       (list
	 (tree->or-tree
	   '(( ((:bind a v1)) . nil )
	     ( ((:bind a v2)) . nil )))
	 (tree->or-tree
	   '(( ((:bind a v3)) . nil )
	     ( ((:bind b v4))
	       . (( ((:bind a v2)) . nil ))))))))
   ==> '(v2))

  ((or-tree-possible-values
     'a
     (merge-or-trees
       (list
	 (tree->or-tree
	   '(( ((:bind a 1)) . nil )
	     ( ((:bind a 2)) . nil )
	     ( ((:bind a 3)) . nil )
	     ( ((:bind a 4)) . nil )))
	 (tree->or-tree
	   '(( ((:bind a 1)) . nil )
	     ( ((:bind a 2)) . nil )
	     ( ((:bind a 3)) . nil )))
	 (tree->or-tree
	   '(( ((:bind a 2)) . nil )
	     ( ((:bind a 3)) . nil )
	     ( ((:bind a 4)) . nil ))))))
   ==> '(2 3)
   :test #'set-eql)

  )


(define-test-group or-tree-merge-to-null-tests

  ((null-or-tree (merge-or-trees '()))
   ==> :true)

  ((null-or-tree
     (merge-or-trees
       (list
	 (tree->or-tree
	   '(( ((:bind a v1)) . nil )
	     ( ((:bind a v2)) . nil )))
	 (tree->or-tree
	   '(( ((:bind a v3)) . nil )
	     ( ()
	       . (( ((:bind a v4)) . nil ))))))))
   ==> :true)

  )


(define-test-group or-tree-forced-action-tests

  ((or-tree-forced-actions
     (tree->or-tree
       '(( ((:bind a 1)) . nil )
	 ( ((:bind a 2)) . nil )
	 ( ((:bind a 3)) . nil ))))
   ==> '())

  ((or-tree-forced-actions
     (tree->or-tree
       '(( ((:bind a 1)) . nil )
	 ( ((:bind a 1)) . nil )
	 ( ((:bind a 1)) . nil ))))
   ==> '((:bind a 1)))

  ((or-tree-forced-actions
     (tree->or-tree
       '(( ((:bind a v1)) . nil )
	 ( ((:bind b v2))
	   . (( ((:bind a v1)) . nil ))))))
   ==> '((:bind a v1)))

  )


(define-test-group (or-tree-trim-against-actions-tests
		     :with ((a-tree
			     (tree->or-tree
			      '(( ((:bind a v1)) . nil )
				( ((:bind b v2))
				  . (( ((:bind a v3)) . nil ))))))))

  ((trim-or-tree-against-actions a-tree '())
   ==> a-tree
   :test #'equal-or-trees)

  ((trim-or-tree-against-actions a-tree '((:bind a v3)))
   ==> (tree->or-tree
	'(
	  ( ((:bind b v2))
	    . (( ((:bind a v3)) . nil )))))
   :test #'equal-or-trees)

  ((trim-or-tree-against-actions a-tree '((:bind b v2)))
   ==> a-tree
   :test #'equal-or-trees)

  ((trim-or-tree-against-actions a-tree '((:bind a v4)))
   ==> nil)

  )


;;; Utilities

(defun tree->or-tree (tree)
  (if (null tree)
      nil
    (new-or-tree
      (mapcar #'(lambda (branch)
		  (make-or-branch :actions (car branch)
				  :subtree (tree->or-tree (cdr branch))))
	      tree))))

(defun or-tree-possible-values (var-name or-tree)
  ;; Assumes that there are no conflicting bindings as we descend.
  (let ((possibles '()))
    (walk-or-tree
      or-tree
      :branch-fn
        #'(lambda (b)
	    (dolist (a (or-branch-actions b))
	      (when (eq (first a) :bind)
		(let ((var (second a))
		      (val (third a)))
		  (when (eq var var-name)
		    (nconcf-new possibles val)))))))
    possibles))

(defun walk-or-tree (or-tree &key (tree-fn #'identity)
			          (branch-fn #'identity)
				  (order :post))
  (label walk ((at or-tree))
    (when (eq order :pre)
      (funcall tree-fn at)
      (mapc branch-fn (or-tree-branches at)))
    (dolist (b (or-tree-branches at))
      (when (or-branch-subtree b)
	(walk (or-branch-subtree b))))
    (when (eq order :post)
      (funcall tree-fn at)
      (mapc branch-fn (or-tree-branches at)))))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
