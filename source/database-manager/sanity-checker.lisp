;;;; File: sanity-checker.lisp
;;; Contains: Consistency and reasonableness checks.
;;; Author: Richard Kirby (rbk) and Jeff Dalton
;;; Created: Wed Jul 22 12:03:49 1992
;;; Updated: Mon Jun 14 02:56:44 1999 by Jeff Dalton
;;; Copyright: (c) 1992, 1993, 1994, AIAI, University of Edinburgh

(in-package :oplan)

;;; The sanity-checker is used after the planner has claimed that it
;;; has finished to check that the plan is indeed consistent and that
;;; various data structures are as they should be.
;;;
;;; To request a sanity check, type the following to the REPL:
;;;
;;;   (ipc-write-to-oplan '(:check-plan))
;;;
;;; or the equivalent
;;;
;;;   (sanity-check)
;;;
;;; A description of the tests performed and the results obtained will
;;; be printed in the Lisp interaction window.  When all tests are complete,
;;; the total number of errors will be reported in the TA window.
;;;
;;; The sanity-checker can also be run by the auto-tester by specifying
;;; either
;;;
;;;   :sanity-check t
;;;
;;; or
;;;
;;;   :sanity-check-only t
;;;
;;; when running a test sequence.
;;;
;;;
;;; /\/: Some more checks to do:
;;;
;;;  * Node ends have unique indexes.
;;;  * The :begin of each node is linked before the :end.
;;;  * Nodes are linked between the ends of their parents.
;;;  * Contributors are before the condition node-ends
;;;  * PSV not-sames are obeyed.
;;;  * Begin_of node-1 has no predecessors, end_of node-2 no successors,
;;;    and no other node-ends are without both predecessors and successors.
;;;  * There are no cycles in the node orderings.  This can be checked
;;;    by calling node-ends-between on begin_of node-1 and end_of node-2.
;;;    (It will signal an error.)
;;;    Then check that all node-ends are in the node-ends-between result.
;;;  * Recomputing the TPN from scratch produces the same values.
;;;  * Consumable resources are within the original "overall" bounds.
;;;  * Simulated execution that tracks condition and effect pv-pairs.
;;;


;;; Check definitions, etc.

(defparameter *all-checks* '())

(defvar *check* 'sanity-checker)	;check now running

(defvar *check-number* 0)		;increases by one for each check

(defvar *check-errors* 0)		;incremented by check-error

(defvar *check-out* (make-synonym-stream '*terminal-io*))

(defmacro define-check (name parameters description &rest body)
  (check-type name symbol)
  (check-type description string)
  `(progn
     (add-check ',name)
     (setf (check-description ',name) ',description)
     (defun ,name ,parameters
       ,@body)))

(defmacro check-description (check) `(get ,check 'check-description))

(defun add-check (name)
  (nconcf-new *all-checks* name :test #'eq))


;;; The sanity-checker.

(defun check-plan ()			;called from db-answer
  (sanity-checker))

(defun sanity-checker (&optional (*check-out* *check-out*))
  (unless (eq (pprocess-name *pprocess*) :dm)
    (error "The sanity checker must run in the DM, not in the ~S process."
	   (pprocess-name *pprocess*)))
  (clear-check-identification)
  (let ((*print-case* :downcase)
	(*check-number* 1)
	(total-errors 0))
    (dolist (*check* *all-checks*)
      (let ((*check-errors* 0))
	(handler-case (funcall *check*)
	  (error (e)
	    (check-report "Error while checking: ~A" e)
	    (incf *check-errors*)))
	(when (> *check-errors* 0)
	  (check-report "Check ~D -- ~D errors" *check-number* *check-errors*))
	(incf total-errors *check-errors*)
	(incf *check-number*)))
    (when (> total-errors 0)
      (check-report "Total errors = ~D" total-errors))
    total-errors))
  

;;; Check reports

(defun check-report (format-string &rest format-args)
  (ensure-check-is-identified)
  (xp-format *check-out* "~&~?~%" format-string format-args))

(defun check-error (format-string &rest format-args)
  (incf *check-errors*)
  (check-report "Error: ~?" format-string format-args))

(let ((checker-identified-p nil)
      (last-identified-check nil))

  (defun clear-check-identification ()
    (setq checker-identified-p nil
          last-identified-check nil))

  (defun ensure-check-is-identified ()
    (unless checker-identified-p
      (format *check-out* "~&~%SANITY CHECKER:~%")
      (setq checker-identified-p t))
    (unless (or (eq *check* last-identified-check)
		(eq *check* 'sanity-checker))
      (format *check-out* "~&~%Check ~D ~A:~%[~A]:~%"
	      *check-number* *check* (check-description *check*))
      (setq last-identified-check
	    *check*))))


;;;; Sanity checks

;;; Check that there are no alternatives for poisioned contexts.

(define-check check-for-poisoned-alts ()
  "Alternatives in poisoned contexts"
  (dolist (alt (am-request :get-alternatives))
    (in-context (find-context (alt-context alt))
      #'(lambda ()
	  (let ((reason (ctxt-symbol-value '*context-was-poisoned*)))
	    (when reason
	      (check-error "Alt ~S in poisoned context ~S~%~
                            Reason: ~S"
			   (alt-id alt) (alt-context alt) reason)))))))

;;; Foreach TOME entry
;;;   Check that the effect node-end is in the current context.

(define-check check-effect-node-end-context ()
  "TOME node-ends in present contex"
  (map-over-tome
    #'(lambda (entry value)
	(let ((node-end (tgm-tome-node-end entry)))
	  (unless (node-end-in-present-context-p node-end)
	    (check-error "~A = ~A:~%~A not in this context"
			 entry value
			 (etag-node node-end)))))))

;;; Foreach GOST entry
;;;   Check the condition node-end is in the current context.

(define-check check-condition-node-end-context ()
  "GOST node-ends in present context"
  (map-over-gost
    #'(lambda (entry value)
	(let ((node-end (tgm-gost-node-end entry)))
	  (unless (node-end-in-present-context-p node-end)
	    (check-error "~A = ~A:~%~A not in this context."
			 entry value
			 (etag-node node-end)))))))

;;; Foreach GOST entry
;;;   Get the list of contributors.
;;;   Check each contributor node-end is in the current context.
;;;   If nil or :NONE then a problem

(define-check check-contributors-node-end-contexts ()
  "GOST contributors node-ends in present context"
  (map-over-gost
    #'(lambda (entry value)
	(let ((bad-node-ends
	       (remove-if
		 #'(lambda (contributor)
		     (node-end-in-present-context-p (car contributor)))
		 value)))
	  (when bad-node-ends
	    (check-error "~A = ~A:~%~A not present in this context"
			 entry value bad-node-ends))))))

;;; Foreach GOST entry
;;;   Foreach contributor
;;;     Check that a TOME entry with pattern matching the condition is present.

(define-check check-that-contributors-provide-effects ()
  "GOST contributors node-ends provide effect"
  (map-over-gost
    #'(lambda (entry value)
	(let ((cond-type (tgm-gost-condition-type entry))
	      (cond-pattern (tgm-gost-pattern entry))
	      (cond-value (tgm-gost-value entry))
	      (missing-contributors '())
	      (bad-contributors '()))
	  (dolist (contributor value)
	    (let* ((node-end (car contributor))
		   (tome-entry (tgm-make-tome-entry cond-pattern node-end))
		   (tome-value (value tome-entry)))
	      (if (eq cond-type 'supervised)
		  (unless (node-or-child-provides-effect-p
			   node-end cond-pattern cond-value)
		    (push tome-entry missing-contributors))
		;; Not supervised
		(ecase (node-provides-effect-p
			node-end cond-pattern cond-value)
		  ((nil)
		   (push tome-entry missing-contributors))
		  ((:ok))
		  ((:deletes)
		   (push (list node-end tome-value) bad-contributors))))))
	  (when missing-contributors
	    (check-error
		"~A = ~A:~%~
                   ~{~6T ~A not present in this context~%~}"
		entry value (reverse missing-contributors)))
	  (when bad-contributors
	    (check-error
		"~A = ~A:~%~
                   ~6T Contributors with the wrong effect value:~
                   ~6T   ~:{~A --> ~A~%~}"
		entry value (reverse bad-contributors)))))))

;; Node-provides-effect-p returns:
;;   nil if the node-end has no effect that matches cond-pattern,
;;   :ok if the node-end has an effect that matches both cond-pattern
;;          and cond-value,
;;   :deletes if the node-end has an effect that matches cond-pattern
;;               but not cond-value.

(defun node-provides-effect-p (node-end cond-pattern cond-value)
  (map-over-matching-tome
    cond-pattern
    #'(lambda (entry value)
	(when (equal (tgm-tome-node-end entry) node-end)
	  (return-from node-provides-effect-p
	    (if (obmatch3 value cond-value nil)
		:ok
		:deletes)))))
  nil)

(defun node-or-child-provides-effect-p (node-end cond-pattern cond-value)
  ;; Called only for supervised conditions.
  ;; Make sure the node-end isn't something unexpected.
  (assert (and (consp node-end) (null (cddr node-end))))
  (let ((node-name (symbol-name (etag-node node-end))))
    (map-over-matching-tome
       cond-pattern
       #'(lambda (entry value)
	   (let ((tome-node-name
		  (symbol-name (etag-node (tgm-tome-node-end entry)))))
	     (when (and (eql 0 (search node-name tome-node-name))
			(obmatch3 value cond-value nil))
	       (return-from node-or-child-provides-effect-p
		 t)))))
    nil))

;;; Foreach GOST entry
;;;   Foreach TOME entry which matches the condition pattern
;;;                            but differs in value
;;;     Check that the TOME entry is not within the ranges set up
;;;                by the contributors.

(define-check check-that-deleters-cannot-delete ()
  "Deleters out of GOST ranges"
  (map-over-gost
    #'(lambda (entry value)
	(let ((cond-pattern (tgm-gost-pattern entry))
	      (cond-value (tgm-gost-value entry))
	      (cond-node-end (tgm-gost-node-end entry))
	      (cond-contributors value)
	      (bad-deleters '()))
	  (map-over-tome
	    #'(lambda (entry value)
		(let* ((tome-pattern (tgm-tome-pattern entry))
		       (tome-node-end (tgm-tome-node-end entry))
		       (tome-value value)
		       (bindings (obmatch3 tome-pattern cond-pattern nil)))
		  ;; If this TOME entry conflicts with the GOST entry
		  ;; we are considering, then check that it is outside
		  ;; the range.
		  (when (and bindings
			     (not (obmatch3 tome-value cond-value bindings)))
		    (unless (or (equal tome-node-end cond-node-end)
				(not (oplan-tgm::tgm-node-end-within-range-p
				      tome-node-end cond-node-end
				      cond-contributors)))
		      (push (cons entry value) bad-deleters))))))
	  (dolist (deleter bad-deleters)
	    (check-error
	       "~A = ~A~%is within the range of~%~A = ~A"
	       (car deleter) (cdr deleter) entry value))))))

;;; Foreach PSV
;;;   Check that the PSV has a value.

(define-check check-psv-values ()
  "Check that all PSVs have a value"
  (flet ((check-psv-has-value (tag body)
	   (when (eql (oplan-psv::psv-body-value body) :undef)
	     (check-error "~A has no value." tag))))
    (map-over-psvs #'check-psv-has-value)))

;;; Foreach PSV
;;;   Check that not-sames are obeyed.

(define-check check-psv-not-sames ()
  "Check that all PSV not-sames lists are obeyed"
  (map-over-psvs
    #'(lambda (tag body)
	(let ((val (oplan-psv::psv-body-value body)))
	  (mapc #'(lambda (ns-tag)
		    (when (eql val (psv-get-value ns-tag))
		      (check-error "~A and ~A = ~A despite a not-same"
				   tag ns-tag val)))
		(oplan-psv::psv-body-not-sames body))))))

;;; Check ne-link-distance

(define-check check-ne-link-distance ()
  "Check ne-link-distance."
  (let ((table (find-longest-path-lengths
		 (oplan-nodes::earliest-node-end) #'ne-post-ends)))
    (walk-node-ends
      #'(lambda (ne)
	  (let ((link-dist (ne-link-distance ne)))
	    (unless (and (numberp link-dist)
			 (= link-dist (gethash ne table)))
	      (check-error "Wrong link-distance to ~S: ~S, should be ~S."
			   ne link-dist (gethash ne table))))))))

;;; Simulate execution

(define-check check-by-simulating-execution ()
  "Check by simulating execution."
  (handler-case (run-exec-simulation)
    (condition (c)
      (check-error "~A" c))))

;;; Checks by plug-in constraint managers

(define-check ask-cms-to-check-constraints ()
  "Check constraints"
  (walk-registered-constraint-managers #'cm-check-constraints))


;;; Utilities

;; (map-over-tome fn) --
;; Applies fn to each TOME entry that is in the current context.
;; fn takes two arguments: the TOME entry, and the TOME value.

(defun map-over-tome (fn)
  (let ((tome-entries (generator '(TOME ?? ??) '??))
	tome-value)
    (do ((tome-entry (try-next tome-entries) (try-next tome-entries)))
	((null tome-entry))
      (setq tome-value (psv-actorise-pattern (value tome-entry)))
      (setq tome-entry (psv-actorise-pattern tome-entry))
      (unless (eq tome-value :undef)
	(funcall fn tome-entry tome-value)))))

(defun map-over-matching-tome (pat fn)
  (assert (equal pat (psv-actorise-pattern pat)))
  (let ((tome-entries (generator '(TOME ?? ??) '??))
	tome-value)
    (do ((tome-entry (try-next tome-entries) (try-next tome-entries)))
	((null tome-entry))
      (setq tome-value (psv-actorise-pattern (value tome-entry)))
      (setq tome-entry (psv-actorise-pattern tome-entry))
      (unless (or (eq tome-value :undef)
		  (not (obmatch3 pat (tgm-tome-pattern tome-entry) nil)))
	;; /\/: Do we need to pass along the bindings from the obmatch?
	(funcall fn tome-entry tome-value)))))

;; (map-over-gost fn) --
;; Applies fn to each GOST entry that is in the current context.
;; fn takes two arguments: the GOST entry, and the GOST value.

(defun map-over-gost (fn)
  (let ((gost-entries (generator '(GOST ?? ?? ?? ??) '??))
	gost-value)
    (do ((gost-entry (try-next gost-entries) (try-next gost-entries)))
	((null gost-entry))
      (setq gost-value (value gost-entry))
      (setq gost-entry (psv-actorise-pattern gost-entry))
      (unless (eql gost-value :undef)
	(funcall fn gost-entry gost-value)))))

(defun map-over-psvs (fn)
  (maphash #'(lambda (tag body-ctxt-list)
	       (let ((body (deref-in-context body-ctxt-list)))
		 (when body
		   (funcall fn tag body))))
	   oplan-psv::*psv-table*))

(defun node-end-in-present-context-p (node-end)
  (or (eql node-end :ALWAYS)
      (let ((node-tag (etag-node node-end)))
	(typep (get-node node-tag)
	       'plan-node))))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
