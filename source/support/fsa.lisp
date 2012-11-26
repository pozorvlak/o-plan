;;;; File: fsa.lsp
;;; Contains: FSA-like state transition handlers
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: April 1993
;;; Updated: Tue Jul  9 00:20:30 1996 by Jeff Dalton
;;; Copyright: (c) 1992, 1993, AIAI, University of Edinburgh

;;; Support for states and transitions in the style of finite state automata.

;;; This code was written for simplicity rather than speed and is not,
;;; in fact, very efficient.  For instance, the matching required for
;;; state transitions could have been compiled into Lisp but, instead,
;;; the patterns are stored as a table and processed using a simple
;;; pattern matcher.

(in-package :oplan-fsa)

(use-package :oplan-util)

;; The fsa-case macro
(export '(fsa-case -->))

;; FSA macro and struct
(export '(fsa fsa-state fsa-transition-table fsa-matching))

;; Operations on FSA structs
(export '(advance-fsa-state do-fsa-state-actions))


;;; States and transitions

;;; The prototypical application is the auto-tester, which works as
;;; follows:

;;; When a planner output is received as an event E, the controller is
;;; in a state S.  A transition table is used to find the next state,
;;; S', from S and (message-contents E).  Then the actions associated
;;; with S' are performed.  The actions typically include sending a
;;; message to the planner.

;;; Note that when the controller is in state S and receives some planner
;;; output, it has already done the actions for S and is waiting to move to
;;; the next state.

;;; When the planner won't provide some suitable output to move the
;;; controller to its next state, the controller sends a message to
;;; itself.  State transitions for these messages are treated in the
;;; same way as for planner output.

;;; This package provides two ways to define the states, transitions,
;;; and actions for a controller of this sort:
;;;  1. The FSA-CASE macro defines a case statement in which each clause
;;;     specifies the actions and transitions for a state.
;;;  2. The FSA macro defines an FSA struct to which the operations
;;;     ADVANCE-FSA-STATE and DO-FSA-STATE-ACTIONS can be applied.

;;; The two macros have similar syntax.  The case macro is more self-
;;; contained and can be used in places where it would be awkward to
;;; refer to a structure.  The structure, on the other hand, has an
;;; accessible state and transition table, which can help when debugging.

;;; Both macros produce a transition table that has an entry for each
;;; state.  The entry for a state S has the form (S (pattern --> S')...),
;;; one transition of the form (pattern --> S') for each possible S'.
;;; Here is the procedure used to find S' plus an a-list that indicates
;;; how a message matched the pattern for S'.

(defun find-next-state (transition-table current-state message)
         ; -> new-state, matching
  (dolist (trans (get-state-transitions transition-table current-state))
    (let* ((trans-match (match '($pattern --> $state) trans))
	   (pattern (lookup '$pattern trans-match))
	   (new-state (lookup '$state trans-match)))
      (unless trans-match
	(error "Can't parse transition ~S." trans))
      (let ((matching (match pattern message)))
	(when matching
	  (return (values new-state matching)))))))

(defun get-state-transitions (transition-table state)
  (let ((entry (assoc state transition-table)))
    (if entry
	(cadr entry)			;/\/ s.b. cdr ??
      (error "Unknown controller state ~S." state))))


;;; The FSA-CASE macro

(defmacro fsa-case ((match-var state-place input-form)
		    initial-form
		    &rest clauses)
  (flet ((clause-state (clause) (car clause))
	 (clause-form (clause) (cadr clause))
	 (clause-transitions (clause) (cddr clause)))
    (let ((transition-table
	   (mapcar #'(lambda (clause)
		       `(,(clause-state clause) ,(clause-transitions clause)))
		   clauses)))
      `(let ((.transition-table. ',transition-table)
	     (.current-state. ,state-place)
	     (.message. ,input-form))
	 (multiple-value-bind (.new-state. ,match-var)
	     (find-next-state .transition-table.
			      .current-state.
			      .message.)
	   (when (null .new-state.)
	     (error "Can't find new state from ~S matching ~S."
		    .current-state. .message.))
	   (assert ,match-var)
	   (setf ,state-place .new-state.)
	   ,initial-form
	   (case .new-state.
	     ,@(mapcar #'(lambda (clause)
			   `((,(clause-state clause)) ,(clause-form clause)))
		       clauses)))))))

;;; An example, though not one where fsa-case is very appropriate:

#+:undef
(defun fsa-factorial (n)
  (let ((state 'start)
	(message nil)
	(result 1))
    (flet ((send (m) (setq message m))
	   (receive () message))
      (send `(,n !))
      (loop
	(fsa-case (matching	;variable to be bound to match a-list
		   state	;setf-able place containing state value
		   (receive))	;form returning value to match against
	  ;; 1st a form that is evaluated after the new state has been
	  ;; assigned but before the new state's actions are evaluated.
	  (format t "~&State ~S, message ~S matched to ~S~%"
		    state message matching)
	  ;; Now the cases ...
	  (start
	    nil			;no actions
	    (($n !) --> fact))
	  (fact
	    (let ((number (lookup '$n matching)))
	      (setq result (* number result))
	      (send `(,(1- number) !)))
	    ((0 !) --> done)
	    (($n !) --> fact))
	 (done
	    (return result)))))))


;;; Macro (FSA (match-var) clause...).
;;;
;;; A clause has the form (state-name action-form transition...).
;;;
;;; We would typically do:
;;;   (defun make-test-controller-fsa ()
;;;     (fsa (matching)		; <-- note var to hold a-list
;;;       clause...))
;;;
;;; Then the test-controller's event-handler would contain:
;;;
;;;   (advance-fsa-state (controller-fsa self)
;;;                      (message-contents (next-event self)))
;;;   (describe-current-state self)
;;;   (do-fsa-state-actions (controller-fsa self))

;;; Trivial example:

#+:undef
(fsa (matching)
   (:start (print :start)
     (:a --> :a))
   (:a (print :a)
     (:a --> :a)
     (:b --> :b))
   (:b (print :b)
     ($ --> :a)))

(defstruct fsa
  state
  transition-table
  matching
  state-action-fn)

(defmacro fsa ((match-var) &rest clauses)
  (flet ((clause-state (clause) (car clause))
	 (clause-form (clause) (cadr clause))
	 (clause-transitions (clause) (cddr clause)))
    (let ((transition-table
	   (mapcar #'(lambda (clause)
		       `(,(clause-state clause) ,(clause-transitions clause)))
		   clauses)))
      `(make-fsa
	 :state :start
	 :transition-table ',transition-table
	 :matching t
	 :state-action-fn
	   #'(lambda (.state. ,match-var)
	       (assert ,match-var)
	       (case .state.
		 ;; Make cases of the form ((state-name) action-form)
		 ,@(mapcar #'(lambda (clause)
			       `((,(clause-state clause))
				 ,(clause-form clause)))
			   clauses)))))))

(defun advance-fsa-state (fsa input)
  (multiple-value-bind (new-state matching)
      (find-next-state (fsa-transition-table fsa)
		       (fsa-state fsa)
		       input)
    (when (null new-state)
      (error "Can't find new state from ~S matching ~S."
	     (fsa-state fsa) input))
    (setf (fsa-matching fsa) matching
	  (fsa-state fsa) new-state)))

(defun do-fsa-state-actions (fsa)
  (funcall (fsa-state-action-fn fsa)
	   (fsa-state fsa)
	   (fsa-matching fsa)))

;;; End
