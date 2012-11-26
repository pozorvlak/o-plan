;;;; File: kp-supportlib.lisp
;;; Contains: Various functions for use by KS writers.
;;; Author: Richard Kirby (rbk)
;;; Created: Fri Jun 22 09:02:34 1990
;;; Updated: Fri May 21 22:43:26 1999 by Jeff Dalton
;;; Copyright: (c) 1992, AIAI, University of Edinburgh


(in-package :oplan-knowledge-platform)


(defvar *ag*)				;used by ACHIEVE and EXPAND

(defvar *kp-debug-p* nil)


;;;; Poisons

;;; Some util fns post a :poison-state and then signal.

(define-condition posted-poison (simple-condition) ())


;;;; Posting agenda entries and alternatives
  
;; Macro sometimes used when making a copy for posting alternatives.
(defmacro with-ag-copy ((var ag) &body body)
  `(let ((,var (copy-agenda-entry ,ag)))
     ,@body))

(defun post-agenda (what &rest initargs)
  (let ((agenda-entry (apply #'make-agenda-entry :body what initargs)))
    (if *kp-debug-p* (format t "~%post-agenda - posting~%~A~%" agenda-entry))
    (when (eq (car what) :POISON-STATE)
      (db-request :POISON-CONTEXT
        (ag-id *kp-agenda-entry-being-processed*)
	(cdr what))
      (whats-going-on "Bad plan state:")
      (db-request :PRINT-DATABASE))
    (whats-going-on "Posting agenda:~%~S" agenda-entry)
    (ipc-send :AM :AGENDA agenda-entry)))

(defun post-alternatives (agenda-entry)
  (whats-going-on "Posting alternative:~%~S~%" agenda-entry)
  (ipc-ask :AM :ALT-AGENDA agenda-entry))

(defun post-agent-agenda (what)
  (whats-going-on "Posting agent agenda ~S~%" what)
  (ipc-send :AM
	    :EVENT
	    (make-agenda-entry		;make-event ? /\/
	      :trigger t
	      :body what)))

;;; /\/: What level should conditions, and ORs have?  The effect-level of
;;; their pattern?  The level of the expand that introduced them?  Something
;;; else (such as 1+ the expand level)?  An argument against it being the
;;; effect level (in the current system) is that we don't bother with a
;;; ks-condition if we don't need to wait for something, nor do we delay
;;; in constructing the or-tree for an effect.  Since we do these things
;;; while expanding, it sounds like the level ought to be the expand level.
;;; Conditions delayed until all effects are in might plausable have the
;;; effect level, though.

;;; Note that a :level of nil means that the AM will assign a default.

(defun post-or (cond contributors for-satisfying-p or-tree)
  (assert (= (length (or-tree-branches or-tree))
	     (or-tree-branch-1 or-tree)))
  (post-agenda `(:OR ,cond ,contributors ,for-satisfying-p . ,or-tree)
     :level nil				;was :inf /\/
     :branch-1 (or-tree-branch-1 or-tree)))

(defun assign-or-level (ag)
  ;; Called by KS-OR.
  (setf (ag-level ag) nil)		;was :inf /\/
  ag)

(defun post-expand (body &rest initargs)
  ;; Need a more abstract way to deal with expand bodies /\/
  (assert (eq (car body) :expand))
  (let ((pattern (third body)))
    (apply #'post-agenda
	   body
	   :level (action-level (car pattern))
	   ; :branch-1 (db-request :number-of-expand-schemas pattern)
	   initargs)))

(defun post-achieve (body &rest initargs)
  ;; Need a more abstract way to deal with achieve bodies /\/
  (assert (eq (car body) :achieve))
  (let ((cond (second body)))
    (apply #'post-agenda
	   body
	   :level (effect-level (car (con-pattern cond)))
	   initargs)))

(defun post-condition (body &rest initargs)
  ;; Need a more abstract way to deal with condition bodies /\/
  (assert (eq (car body) :condition))
  (let ((cond (second body)))
    (apply #'post-agenda
	   body
	   :level (effect-level (car (con-pattern cond)))
	   initargs)))

(defun post-bind (body)
  (assert (eq (car body) :bind))
  (let ((psv-name (second body)))
    (assert (psvar-p psv-name))
    (let* ((possibles (get-bindings psv-name))
	   (n-possibles (if (consp possibles) (length possibles) 0)))
      ;; N.B. n-possible might be zero if no values were left when
      ;; the PSV was created.  Also possibles might be :already-bound.
      ;; In both cases (consp possibles) is false and we let branch-1 = 1.
      (post-agenda body
	:level nil			;was :inf /\/
	:branch-1 (max 1 n-possibles)))))


;;;; Adding constraints

(defun kp-add-constraint (constraint-type constraint &key or-handler)
  (let ((cm-result (db-request :ADD-CONSTRAINT constraint-type constraint)))
    (cond ((null cm-result)
	   (post-agenda `(:POISON-STATE :CONSTRAINT-FAILED ,constraint))
	   nil)
	  ((eq cm-result t)
	   t)
	  ((every #'or-tree-p cm-result)
	   (dolist (or-tree cm-result t)
	     (if or-handler
		 (funcall or-handler or-tree)
	       (post-or constraint nil nil or-tree))))
	  (t
	   (error "Bogus kp-add-constraint result for ~S:~% ~S."
		  constraint cm-result)))))

(defun kp-add-constraint-block (constraint-type constraints &key or-handler)
  ;; We can't tell which constraint(s) failed.
  (let ((cm-result (db-request :ADD-CONSTRAINTS constraint-type constraints)))
    (cond ((null cm-result)
	   (post-agenda `(:POISON-STATE :CONSTRAINT-BLOCK-FAILED
					:TYPE ,constraint-type))
	   nil)
	  ((eq cm-result t)
	   t)
	  ((every #'or-tree-p cm-result)
	   (if (eq or-handler :reject)
	       (error "Unexpected or-tree for constraint-type ~S."
		      constraint-type)
	     (dolist (or-tree cm-result t)
	       (if or-handler
		   (funcall or-handler or-tree)
		 (post-or `(,constraint-type) nil nil or-tree)))))
	  (t
	   (error "Bogus kp-add-constraint-block result for type ~S:~% ~S."
		  constraint-type cm-result)))))


;;;; Some DB requests

;; Adds the <type> of restriction to <psv> with <value>.
(defun add-restriction-to-psv (psv type value)
  (db-request :ADD-VAR-RESTRICTION psv type value))

(defun get-bindings (var)
  (db-request :GET-VAR-BINDINGS var))

(defun set-binding (var value)
  (db-request :SET-VAR-BINDING var value))


;;;; Utilities

#+:undef
(defun pattern-function (pattern)
  "Returns the function (the first fixed-word) of the pattern"
  (car pattern))

#+:undef
(defun pattern-args (pattern)
  "Returns a list of the arguments to the function of the pattern"
  (cdr pattern))


;;;; Stage manager

#|
stage-manager takes the following form:
(stage-manager (<state-vars>) (<local-vars>)
               arg
               <stage 0 forms>
	       (stage <why>)
	       <stage 1 forms>
	       (stage <why>)
	       <stage 2 forms>
	       (stage <why>)
	       ...
	       (stage <why>)
	       <stage n forms>)

and converts it to the following form:

(block .stage-out.
  (let ((<state-var 1> (nth 0 (ag-info arg)))
	(<state-var 2> (nth 1 (ag-info arg)))
	...
	(<state-var n> (nth n-1 (ag-info arg)))
	<local-vars>)
  (tagbody
     (case (ag-stage arg)
       (0)
       (1 (go 1))
       (2 (go 2))
       ...
       (n (go n)))
   <stage 0 forms>
     (if <why> (progn
		 (stage-agenda arg 1 (list <state-vars>))
		 (return-from .stage-out.)))
   1
   <stage 1 forms>
     (if <why> (progn
		 (stage-agenda arg 2 (list <state-vars>))
		 (return-from .stage-out.)))
   2
   ...
     (if <why> (progn
		 (stage-agenda arg n (list <state-vars>))
		 (return-from .stage-out.)))
   <stage n forms>)))

|#

(defun stage-agenda (agenda-entry stage state)
  (setf (ag-stage agenda-entry) stage)
  (setf (ag-info agenda-entry) state)
  (ipc-send :AM :AGENDA agenda-entry))

(defmacro stage-manager (state-vars local-vars agenda-entry &rest stages)
  (flet ((is-stage-form-p (x) (and (listp x) (eq (car x) 'stage))))
    (let ((stage-num 1)
	  (state-var 0)
	  var-bindings-list case-list stage-body-list)
      (dolist (var state-vars)
	(push (list var `(nth ,state-var (ag-info ,agenda-entry)))
	      var-bindings-list)
	(incf state-var 1))
      (setq var-bindings-list (append local-vars var-bindings-list))
      (dolist (form stages)
	(if (is-stage-form-p form)
	    (progn
	      ;; Do the case statement.
	      (push (list stage-num (list 'go stage-num)) case-list)
	      ;; Do the staging.
	      (push (list 'if (cadr form)
			  (list 'progn
				(list 'stage-agenda
				      agenda-entry stage-num
				      `(list ,@state-vars))
				(list 'return-from '.stage-out.)))
		    stage-body-list)
	      (push stage-num stage-body-list)
	      (incf stage-num 1))
	    (push form stage-body-list)))
      `(block .stage-out.
	(let ,var-bindings-list
	  (tagbody
	     (case (ag-stage ,agenda-entry)
	       (0)
	       ,@(nreverse case-list))
	     ,@(nreverse stage-body-list)))))))

;; Just a test staging rule!
(defun if-want-real-time-response-p ()
  nil)

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
