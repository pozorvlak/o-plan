;;;; File: world-state-cm.lisp
;;; Contains: The CM interface to the TGM
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: November 1996
;;; Updated: Sun Jun 20 01:03:38 1999 by Jeff Dalton
;;; Copyright: (c) 1996, AIAI, University of Edinburgh

(in-package :oplan)


;;;; DM message handlers

(defmessage (:DM :CHECK-GOST) (cond bindings)
  (declare (ignore bindings))
  (let (act::*bindings*)
    (apply #'tgm-add-condition cond)))

(defmessage (:DM :ADD-GOST) (args)
  (list (apply #'tgm-commit-to-adding-condition args)))

(defmessage (:DM :ABORT-ADD-TOME-OR-GOST) ()
  (list (tgm-abort-adding-cond-or-effect)))

(defmessage (:DM :UPDATE-CONTRIBUTORS) (cond contributors)
  (now-have-cond cond)			; also sometimes done in ks-or /\/
  (tgm-update-contributors cond contributors))

(defmessage (:DM :HANDLE-ADDING-CONDITION-INTERACTION) (cond interact)
  (ca-agenda-wrapper
    #'(lambda (cond)
	(or (handle-adding-cond-interaction cond interact)
	    (ca-post-failure)))
    cond))

(defmessage (:DM :QA-ALL) (node-id)
  (let ((node-tag (intern (concatenate 'string "NODE-" node-id)
			  (find-package "OPLAN"))))
    (when (get-node node-tag)
      ;; OK -- the node exists.
      (qa-all-at-node `(,node-tag :end)))))

(defmessage (:DM :GET-TOME) ()
  (tgm-make-a-list-of-the-tome))

(defmessage (:DM :GET-GOST) ()
  (tgm-make-a-list-of-the-gost))


;;;; The CM

(define-constraint-manager tgm (simple-cm))

(register-constraint-manager tgm 
  :constraint-types '(world-state))


;;;; Initialization

(defmethod cm-init-constraints ((self tgm))
  (initialize-variable-group :tgm)
  (call-initializers :tgm))

(defmethod cm-new-domain ((self tgm) domain)
  (add-always-facts (domain-always domain)))


;;;; Filtering

(defmethod cm-eval-filter ((self tgm) condition bindings)
  (let ((result (tgm-check-condition-within-bindings condition bindings)))
    (when (list-beginning :satisfy result)
      (setq result (cadr result)))
    result))


;;;; Adding constraints

;;; Note that the CM handles multiple constraints itself, unlike what
;;; it does for filtering.

(defmethod cm-add-constraints ((self tgm) conditions)
  (ca-agenda-wrapper #'add-world-state-conditions conditions))

(defun add-world-state-conditions (conds)
  (dolist (cond conds t)
    (whats-going-on "Adding ~:@(~A~) condition~%" (con-type cond))
    (let ((interact (try-adding-cond cond)))
      (unless (handle-adding-cond-interaction cond interact)
	(ca-post-failure)
	(return nil)))))

(defun handle-adding-cond-interaction (cond interact)
  (let ((qa-result (tgm-satisfy-qa-result interact)))
    (if (or-tree-p qa-result)
	(if (some-work-to-do-p qa-result)
	    (progn
	      (note-expected-cond cond)
	      (ca-post-agenda `(:OR ,cond nil t ,qa-result))
	      (really-add-cond cond)
	      t)
	  (progn
	    (really-add-cond cond)
	    (tgm-update-contributors
	       cond
	       (find-contributors
		 (car (or-tree-branches qa-result))))
	    t))
      (if (or qa-result (eq (con-type cond) 'supervised))
	  (progn
	    (really-add-cond cond)
	    t)
	(progn
	  (tgm-abort-adding-cond-or-effect)
	  nil)))))

;;; /\/: Rename to -condition rather than -cond (but there are -condition
;;; versions in knowledge-sources/condition-support.lisp

(defun try-adding-cond (cond)
  (let (act::*bindings*)
    (apply #'tgm-add-condition cond)))

(defun really-add-cond (cond)
  (apply #'tgm-commit-to-adding-condition cond))


;;;; Sanity checks

;;; /\/: Most TGM-related sanity checks are still in sanity-checker.lisp
;;; for historical reasons.

(defmethod cm-check-constraints ((self tgm))
  (check-expected-cond-table))


;;;; Constraint descriptions

(defmethod cm-constraint-descriptions ((self tgm))
  '())


;;;; Table of pending / outstanding conditions (still w/o GOST entries)

;;; Sometimes we construct and or-tree for a condition, and post an :or
;;; agenda entry, before all the effects that might interact with the
;;; condition are in the plan.  The or-tree handles only the effects
;;; that were in place when the or-tree was created.  Once the GOST entry
;;; for the cond exists (and has contributors), we'll do the right thing
;;; when adding later effects; but until then adding effects (that might
;;; interact) isn't safe.

;;; This table can be used to check whether we are in such an unsafe
;;; state for any given effect-name.  The table maps an effect name
;;; to a list of still-outstanding conditions.

(definit :dm *expected-cond-table* (make-hash-table :test #'eq))

(defun note-expected-cond (cond)
  (let* ((effect-name (car (con-pattern cond)))
	 (already (ctxt-gethash effect-name *expected-cond-table*)))
    (assert (not (member cond already :test #'equal)))
    (setf (ctxt-gethash effect-name *expected-cond-table*)
	  (cons cond already))))

(defun now-have-cond (cond)
  (let* ((effect-name (car (con-pattern cond)))
	 (expected (ctxt-gethash effect-name *expected-cond-table*)))
    (when (member cond expected :test #'equal)
      (setf (ctxt-gethash effect-name *expected-cond-table*)
	    (remove cond expected :test #'equal)))))

(defun expected-cond-p (pattern)
  (let ((effect-name (car pattern)))
    (not (null (ctxt-gethash effect-name *expected-cond-table*)))))

(defun check-expected-cond-table ()
  (ctxt-maphash
    #'(lambda (effect-name conds)
	(unless (null conds)
	  (check-error "Still waiting for conds:~%~S" conds)))
    *expected-cond-table*))


;;;; Achieve parent-nodes and after-points

;;; /\/: If an achieve cond were achieve P = V at End after End in Parent,
;;; hence something like (achieve p v at-end after-end parent-node)
;;; internally, this wouldn't be necessary, but it's difficult to change
;;; the format of achieve conds to contain additional information
;;; (because of pattern-matching and maybe other things in the
;;; way the GOST is handled) and difficult to change what's in
;;; the agenda-entries (e.g. in the info field), because places
;;; outside ks-achieve know what's there.

;;; Record the node that was being expanded when each achieve was
;;; being introduced.

(definit :dm *achieve-parent-table* (make-hash-table :test #'equal))

(defun record-achieve-parent (cond parent)
  (assert (eq :absent (ctxt-gethash cond *achieve-parent-table* :absent)))
  (setf (ctxt-gethash cond *achieve-parent-table*)
	(if (eq parent 'node)
	    nil
	  parent)))

(defun get-achieve-parent (cond)
  (let ((parent (ctxt-gethash cond *achieve-parent-table* :absent)))
    (assert (not (eq parent :absent)))
    parent))

;;; After-points, along the same lines.

(definit :dm *achieve-after-point-table* (make-hash-table :test #'equal))

(defun record-achieve-after-point (cond after-point)
  (assert (eq :absent (ctxt-gethash cond *achieve-after-point-table* :absent)))
  (setf (ctxt-gethash cond *achieve-after-point-table*)
	(if (eq after-point 'node)
	    nil
	  after-point)))

(defun get-achieve-after-point (cond)
  (let ((after-point (ctxt-gethash cond *achieve-after-point-table* :absent)))
    (assert (not (eq after-point :absent)))
    after-point))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
