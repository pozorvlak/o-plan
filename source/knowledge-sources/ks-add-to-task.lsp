;;;; File: KS-ADD-TO-TASK.lsp
;;; Contains: KS-ADD-TO-TASK
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: 01 June 1995
;;; Updated: Fri May 21 22:08:53 1999 by Jeff Dalton
;;; Copyright: (c) 1995, AIAI, University of Edinburgh

(in-package :oplan-knowledge-source)

;;; KS-ADD-TO-TASK can get as (ag-body event):
;;;
;;;   (:ADD-TO-TASK addition)
;;;
;;; Additions defined so far:
;;;
;;; :ACTION pattern
;;;
;;;    Adds a node and expands.
;;;
;;; :ALL-OF (addition)...
;;;
;;;    Example: (:add-to-task :all-of (:action p1) (:initially p2 v))
;;;
;;;    Later additions in the list can depend, to some extent, on earlier
;;;    ones.  For example, a time-window might refer to a node created
;;;    for an action.
;;;
;;;    Recursive (nested) used of :ALL-OF is supported.
;;;
;;; :INITIALLY pattern value
;;;
;;;    Adds an initial effect, i.e. at end_of node-1.
;;;
;;; :TASK task-schema-name
;;;
;;;    Adds the constraints specified by the task schema.  The name
;;;    may be given as a string or as a symbol.  Internally, the
;;;    names are standard-internal-case (ie, upper-case) symbols
;;;    such as TASK_BUILD_HOUSE, but they can be written in lower-case
;;;    in the usual way or even as lower-case strings.
;;;
;;;    Note that a task can be added only if a task has already been
;;;    established by KS-SET-TASK and KS-EXPAND-TASK.  What this
;;;    amounts to is that option-1, and the start and finish nodes
;;;    (node-1 and node-2, respectively) must already exist.
;;;
;;;    If a very simple task in put in initially (one that just 
;;;    creates the start and finish nodes), task-addition allows
;;;    one to have, in effect, completely different tasks in different
;;;    siblings of option-1, which would otherwise not be possible.
;;;
;;;    Task-addition also allows the addition of all the constraints
;;;    that can be specified in tasks, even if there is no independent
;;;    support for them in KS-ADD-TO-TASK.
;;;
;;; :TIME-WINDOW from-node-end to-node-end min max
;;;
;;;    Adds a time-window / order constraint between two node-ends.
;;;    A node-end has the form (node-name end), where the node-name
;;;    is something like node-3-2 and the end is :begin or :end.
;;;    The from-node-end can also be the special symbol :zero to
;;;    specify an "absolute" constraint relative to time = 0.
;;;    Min and max must be nonnegative integers (though max can
;;;    also be :inf) and are interpreted as seconds.
;;;    

(defun KS-ADD-TO-TASK (event)
  (assert (eq (car (ag-body event)) :ADD-TO-TASK))
  (let ((addition (cdr (ag-body event))))

    ;; Make sure the context allows additions.
    (unless (am-request :can-add-to-option-p)
      (invalid-command "Cannot add to the current option"))

    ;; Flatten :ALL-OFs to raise up any nested instances.
    (when (list-beginning :all-of addition)
      (setq addition (cons :all-of (flatten-all-ofs (cdr addition)))))

    (add-to-task addition)
    
    ;; We have added something, so we need to make sure a :finished
    ;; message will eventually be sent to the TA.
    (unless (db-request :agenda-status)
      (post-agenda '(:PLANNER-FINISHED)
		   :trigger '(:EMPTY)))

    ;; Add the constraints to the current option and all its
    ;; alternatives.  We do this after calling add-to-task
    ;; so that syntax checks, etc, will have been performed.
    (am-request :add-to-option
      ;; Need to pass a list of additions.
      (if (list-beginning :all-of addition)
	  (cdr addition)		;assumes :all-of flattening above
	(list addition)))))

;;; We match against a pattern to check the number of arguments
;;; and then apply the corresponding function, because we want to
;;; signal a harmless error for mere syntax errors, if we can arrange
;;; to do so.

(defparameter *task-addition-table*
  '(((:action $p)                 . add-action-to-task)
    ((:all-of *)                  . add-all-of-to-task)
    ((:initially $p $v)           . add-initial-effect-to-task)
    ((:task $name)                . add-task-to-task)
    ((:time-window $f $t $mn $mx) . add-time-window-to-task)))

(defun add-to-task (addition)
  (let ((case (task-addition-method addition)))
    (if case
	(apply (alist-value case) (cdr addition))
      (invalid-command "Invalid addition to task: ~S" addition))))

(defun task-addition-method (addition)
  (assoc addition *task-addition-table*
	 :test #'(lambda (add pat) (match pat add))))


;;; :ALL-OF

(defun add-all-of-to-task (&rest additions)

  ;; First do a basic syntax check so that we find out about
  ;; such errors before we've added anything and hence can
  ;; signal a harmless error.
  (let ((losers (remove-if #'task-addition-method additions)))
    (when losers
      (invalid-command "Invalid task additions: ~S" losers)))

  ;; Note that we intentionally do not do a complete check, because
  ;; we want to allow such things as the addition of a time-window
  ;; that refers to a node created for an added action that was earlier
  ;; in the same :all-of list.

  ;; Make the additions to the task.  A harmless error that's after
  ;; the 1st addition is turned into something more serious.
  (when additions
    (add-to-task (first additions))
    (dolist (add (rest additions))
      (handler-case (add-to-task add)
	(harmless-error (e)
          (error "Error after first task addition: ~A" e))))))

(defun flatten-all-ofs (additions)
  (mapcan #'(lambda (a)
	      (if (list-beginning :all-of a)
		  (flatten-all-ofs (rest a))
		(list a)))
	  additions))


;;; :ACTION

(defun add-action-to-task (pattern)

  ;; See if any schemas look like they'll expand the action represented
  ;; by the pattern.
  (let ((schemas (get-expand-schemas pattern)))
    (when (null schemas)
      (invalid-command "No schemas will expand ~S" pattern)))

  ;; Add a node to represent the new action.
  (let ((node-tag
	 (db-call 'ads-add-node
		  :type 'action
		  :pattern pattern
		  :reason '(:expand))))

    ;; Link it in.
    (db-call 'link-etags-else-error
       (or (db-call 'global-after-point)
	   (etag 'node-1 :end))
       (etag node-tag :begin))
    (db-call 'link-etags-else-error
       (etag node-tag :end)
       (etag 'node-2 :begin))

    ;; Post an EXPAND to expand it
    (post-expand `(:EXPAND ,node-tag ,pattern))))


;;; :INITIALLY

(defun add-initial-effect-to-task (pattern value)

  ;; Check syntax
  (unless (and (consp pattern) (symbolp (car pattern)))
    (invalid-command "Cannot add invalid effect ~S = ~S" pattern value))
  
  ;; Make sure we know the level of the effect.
  (unless (ignore-errors (db-call 'effect-level (car pattern)))
    (invalid-command "Cannot add unknown effect ~S." (car pattern)))

  ;; Try adding the effect.
  (let ((effect (effect pattern value (etag 'oplan::node-1 :end))))
    (handle-cm-result
      (db-request :add-constraints 'effect (list effect)))))


;;; :TASK

(defun add-task-to-task (task-name)
  (let ((schema-name 
	 (if (symbolp task-name)
	     task-name
	   (intern (string-upcase task-name)))))

    ;; Make sure there's a schema for this task.
    (unless (db-request :EXISTS-NAMED-SCHEMA schema-name)
      (invalid-command "There is no task named \"~A\"." task-name))

    ;; Post an :expand-task
    (post-agenda `(:EXPAND-TASK ,schema-name :task-addition)
      :trigger t
      :level 0)))


;;; :TIME-WINDOW

(defun add-time-window-to-task (from to min max)
  ;; Check args
  (unless (or (eq from :zero) (valid-etag-p from))
    (invalid-command "Invalid from-node-end in time-window: ~S" from))
  (unless (valid-etag-p to)
    (invalid-command "Invalid to-node-end in time-window: ~S" to))
  (unless (and (integerp min) (>= min 0))
    (invalid-command "Invalid min in time-window: ~S" min))
  (unless (or (infp max) (and (numberp max) (>= max 0)))
    (invalid-command "Invalid max in time-window: ~S" max))

  ;; Now we can try to add the constraint.
  (unless (kp-add-constraint 'time-window
	     (construct-time-constraint
	        (if (eq from :zero) :abst0 from)
		to
		(time-window min max))
	     :or-handler :reject)
    (throw :ks-exit nil)))

(defun valid-etag-p (candidate)
  (handler-case (db-call 'get-node-end candidate)
    (error ()
      nil)
    (:no-error (node-end)
     (declare (ignore node-end))
     t)))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
