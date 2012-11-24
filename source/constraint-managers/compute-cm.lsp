;;;; File: compute-cm.lsp
;;; Contains: The manager for compute constraints
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: November 1996
;;; Updated: Sat Dec  7 23:44:49 1996 by Jeff Dalton
;;; Copyright: (c) 1996, AIAI, University of Edinburgh

(in-package :oplan)

;;; /\/: The rules for TF should say that a compute might be evaluated
;;; more than once, because implementations might sometimes do so.  We
;;; manage to avoid that most of the time, but if a compute can be
;;; evaluated as a filter, but does not bind any variables, then it
;;; will be evaluated again later on.  We avoid a repeat eval in other
;;; cases by rewriting the condition to note that it has already been
;;; evaluated -- see the use of :computed in expand-support.lsp.


;;;; The CM

(define-constraint-manager compute-cm (simple-cm))

(register-constraint-manager compute-cm
  :constraint-types '(compute))


;;;; Constraint information

(defvar *compute-functions* '())

(defvar *multiple-answer-p-table* (make-hash-table :test #'eq))

(defmacro multiple-answer-p (fn-name)
  `(gethash ,fn-name *multiple-answer-p-table*))


;;;; Initialization

(defmethod cm-init-constraints ((self compute-cm))
  nil)

(defmethod cm-new-domain ((self compute-cm) domain)
  (let ((cf-dcls (domain-compute-functions domain)))
    (clrhash *multiple-answer-p-table*)
    (dolist (cf cf-dcls)
      (when (compute-function-multiple-answer-p cf)
	(setf (multiple-answer-p (car (compute-function-call-pattern cf)))
	      t)))
    (setq *compute-functions* cf-dcls)))


;;;; Filtering

;;; If we can't (yet) evaluate the condition, becuase some variables
;;; in the pattern are not yet bound, we just return t.  The condition
;;; will be evaluated properly later on, if the schema passes the other
;;; filters.  We also do this if the compute function returns multiple
;;; answers, because the filtering code can't yet make much use of an
;;; or-tree.  /\/

(defmethod cm-eval-filter ((self compute-cm) cond bindings)
  (or (multiple-answer-p (car (con-pattern cond)))
      (not (can-evaluate-compute-p cond bindings))
      (evaluate-compute-filter cond bindings)))

(defun can-evaluate-compute-p (cond bindings)
  ;; Check that all vars in the pattern have values
  (fully-instantiable-p (con-pattern cond) bindings))

(defun evaluate-compute-filter (cond bindings)
  ;; Returns true or false or an or-tree.
  (let ((p (con-pattern cond))
	(v (con-value cond)))
    (dev-debug :trace "Eval compute filter ~W = ~W~%" p v)
    ;; It is assumed that all vars in p have values.
    (let ((fn-result (apply #'funcall (fully-instantiate p bindings))))
      (case v
	;; True and false are special cases
	((true)  (not (null fn-result)))
	((false) (null fn-result))
	(t
	 ;; Otherwise we match the fn-result against v.
	 (let ((binds (obmatch3 fn-result v (copy-tree bindings))))
	   (if (atom binds)
	       binds			;presumably t or nil
	     (make-or-tree-from-bindings
	       (set-difference binds bindings
		 :test
		 #'(lambda (new old)
		     (and (eq (var-name new) (var-name old))
			  (eq (var-value new) (var-value old)))))))))))))

(defun make-or-tree-from-bindings (binds)
  (new-or-tree
    (list
      (make-or-branch
        :actions (make-or-actions-from-bindings binds)))))

(defun make-or-actions-from-bindings (binds)
  (mapcar #'(lambda (b)
	      (assert (not (eq (var-value b) :undef)))
	      `(:bind ,(var-name b) ,(var-value b)))
	  binds))


;;;; Adding a constraint

;;; /\/: It's not clear that we should actually bind variables
;;; rather than create an or-tree.

(defmethod cm-add-constraint ((self compute-cm) cond)
  (let ((p (con-pattern cond))
	(v (con-value cond)))
    (if (multiple-answer-p (car p))
	(evaluate-compute-multiple p v)
      (evaluate-compute p v))))

(defun evaluate-compute (pattern value)
  (dev-debug :trace "Eval compute ~W = ~W~%" pattern value)
  (let ((a-pat (psv-actorise-pattern pattern))
	(a-val (psv-actorise-pattern value)))
    (assert (fully-instantiated-p a-pat))
    (let ((fn-result (apply (car a-pat) (cdr a-pat))))
      (case value
	;; True and false are special cases
	((true)  (not (null fn-result)))
	((false) (null fn-result))
	(t
	 ;; Otherwise we match the fn-result against value.
	 (let ((binds (obmatch3 fn-result a-val nil)))
	   (if (atom binds)
	       binds			;presumably t or nil
	     (make-or-tree-from-bindings binds))))))))


;;; Multiple answers

;;; There are a couple of questions.  (1) If no answers are returned,
;;; does this count as failure, or as a vacuous success?  (2) If only
;;; some answers match the value pattern, should the failures be filtered
;;; out, or should they cause the whole condition to fail?

;;; The reasoning on (2) is that we should treat the answers as branches
;;; of an or-tree, and non-viable branches are normally filtered out
;;; (one way or another -- when merging trees, in ks-or, etc).  Or
;;; imagine that we put off calling the matcher for an answer until
;;; we needed to consider the corresponding branch (a kind of lazy
;;; matching).  We should be able to proceed with one branch even
;;; though a later one would fail.

;;; Then, for (1), we can cansider what should happen if the last
;;; remaining branch of an or-tree failed.  That would be a failure
;;; of the whole tree, not a vacuous success.  This matches OR,
;;; SOME, etc, in Common Lisp.

;;; So at least one match must succeed, and failures are filtered out.

(defun evaluate-compute-multiple (pattern value)
  (dev-debug :trace "Eval compute multiple ~W = ~W~%" pattern value)
  (let ((a-pat (psv-actorise-pattern pattern))
	(a-val (psv-actorise-pattern value)))
    (assert (fully-instantiated-p a-pat))
    (let ((fn-result (apply (car a-pat) (cdr a-pat))))
      (case value
	;; True and false are special cases and, for multiple answers,
	;; not very useful.
	((true)  (notevery #'null fn-result))
	((false) (some #'null fn-result))
	(t
	 ;; Otherwise we build and or-tree.
	 (match-all-ways a-val fn-result))))))

(defun match-all-ways (pat values) ; -> or-tree
  ;; There are no variables in the values, and all variables in the
  ;; pattern are (given actors for) PSVs.
  (new-or-tree
    (loop for val in values
	  for binds = (obmatch3 pat val nil)
	  when binds
	  collect
	   (make-or-branch
	     :actions
	       (make-or-actions-from-bindings
		 (if (eq binds t) '() binds))))))


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
