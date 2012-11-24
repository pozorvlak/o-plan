;;;; File: coa-planning-support.lsp
;;; Contains: Planning code for COA matrix Web demos
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: September 1998
;;; Updated: Mon Sep  6 12:08:16 1999 by Jeff Dalton
;;; Copyright: (c) 1997, 1998, 1999, AIAI, University of Edinburgh

(in-package :oplan)

;;; This file provides a coa-based layer on top of the O-Plan program
;;; interface.  Such a layer could be independent of the Web interface,
;;; but at present it's not.  /\/

;;; A COA is in some ways a more elaborate version of an option.
;;; Like an option, it can be used to hold a plan while other plans
;;; are developed.  However, where an option represents a particular
;;; plan state in the planner, a COA represents something more
;;; abstract: a plan that may not yet exist and that may be developed
;;; in stages and thus correspond to different options (and hence
;;; different plan states) at different times.  Moreover, an option
;;; is, outside the planner, just a name, while a COA is a struct
;;; that contains a great deal of information, including a plan 
;;; description and plan-evaluation results.  All of the information
;;; about a COA available via the Web matrix interface can be obtained
;;; without referring back to the planner, because it is either
;;; contained in the COA struct or else derived from something
;;; that's contained in the struct.

;;; Two options are associated with each COA.  The base-option is
;;; a child of option-1 and is created at the same time as the COA.
;;; The current-option is a descendent of the base-option and
;;; represents the COA's current plan.  The current-option changes
;;; as the plan is developed.

;;; COAs can be manipulated in ways that make options, and O-Plan
;;; more generally, awkward to use.  The main "problem" is that
;;; O-Plan's authority settings are global, rather than tied to
;;; options, but when working with COAs, it is natural to have
;;; different authority settings for different COAs.  For instance,
;;; the user might want to develop one COA to level 3 and another
;;; to level 2.  We therefore store authority values in the COA
;;; structs and, whenever we want O-Plan to do something with
;;; a COA's current-option, we have to be careful to give O-Plan
;;; the right authority settings without ever letting them be
;;; to high for whatever option was current in O-Plan before.
;;; This is handled by (move-planner-to-coa-option coa) which
;;; leaves the :level authority at -1 "just in case".

;;; (The fear is that some planning will occur when it's not expected,
;;; either when O-Plan's authority is changed or when O-Plan is told
;;; to change its current option, because the authority is higher
;;; then when this option was last current, or for some other reason.)


;;;; How things work  /\/: Complete and rewrite?

;;; When the demo is initialized (by initialize-session-data), ask O-Plan
;;; to plan for an "empty task", a task that contains start and finish
;;; nodes, and nothing else.   This creates option-1.

;;; When a COA is defined:

;;; When we're asked to plan for a COA:

;;; When a COA is split:

;;; When we're asked to add constraints to a COA:


;;;; COA data

;;; /\/: Coa-parameters contains values from the most recent form that
;;; might define actions -- either a COA definition form or a constraint
;;; addition form.  It is just to make coa-arg and situation-arg work,
;;; and to hold (query-arg :n) for figuring out which COA the form was
;;; about.  The relevant information should be extracted a.s.a.p. and 
;;; put into coa-actions-to-add or coa-initial-effects.

;;; The coa-number can change when a coa is split.  The coa-id never
;;; changes.  The id should therefore be used in such things as URLs
;;; that should work across a split.

(defvar *n-top-level-coas* 0)

(defvar *coas* nil "A list of all COAs")

(defvar *coa* nil "The current COA")

(defstruct coa
  id			      ;an int >= 1
  number		      ;a string such as "2.1.3"
  change-p		      ;changed in a way that matters for splitting?
  base-option		      ;the name of a child of option-1
  current-option	      ;the name of a descendent of the base-option
  history		      ;a list of event descriptions, for debugging.
  initial-effects	      ;a list of pv-pairs
  added-actions		      ;the actions that have been added
  actions-to-add	      ;the actions to add when next told to plan
  authority		      ;a-list for :level and :phase values
  authority-history	      ;list of a-lists, one a-list per plan request
  definition-parameters	      ;hash-table-alist from COA definition form
  parameters		      ;hash-table-alist from most recent def form
  plan-status		      ;nil, :complete, :partial, :question, or :failure
  plan-statistics	      ;obtained right after planning
  plan-description	      ;a list of node descriptions
  plan-world-2                ;a list of (p v) pairs from qa-all at end_of 2
  evaluations		      ;a-list of (name . eval-result) pairs
  raw-evaluations             ;the a-list as it came from O-Plan
  replan-control	      ;a replan-control struct
  planner-can-exec-p          ;true or false -- Planner user's authority
  exec-control		      ;an exec-control struct or nil
  (owner *user*)	      ;a user-role object
  (visibility :owner)	      ;t, nil, :owner, or a "ghost name" string
  (ghost nil)                 ;nil or a coa-id
  (question-kwd nil)	      ;the kind of question asked by O-Plan
  (question-agenda nil)	      ;the agenda "body" of the asker
  (question-data nil)	      ;information relevant to the question
  (question-history nil)      ;for debugging
  (return-p t)		      ;true if it's to be returned from planning
  )

(defun get-coa (id)
  (or (find id *coas* :key #'coa-id)
      (error "There is no COA ~S" id)))

(defun new-coa ()
  ;; Call only for new top-level COAs, not for splits.
  (let* ((n (1+ (length *coas*)))
	 (c (make-coa :id n
		      :number (int->string (incf *n-top-level-coas*)))))
    (coa-history-event c :created-as (coa-number c))
    (nconcf *coas* (list c))
    c))

;;; Some status predicates

(defun coa-defined-p (coa)		;pseudo-accessor
  (not (null (coa-definition-parameters coa))))

(defun coa-active-p (coa)
  (and (coa-defined-p coa)
       (not (eq (coa-plan-status coa) :failure))))


;;; Visibility

;;; For each COA, the following possibilities are supported:
;;;   - invisible to everyone
;;;       (coa-visibility coa) = nil
;;;   - visible to one user (the owner) and a ghost to the other
;;;       (coa-visibility coa) = a string (the ghost name)
;;;   - visible to one user (the owner) and invisible to the other
;;;       (coa-visibility coa) = :owner
;;;   - visible to everyone
;;;       (coa-visibility coa) = t

(defun coa-visible-p (coa)
  (let ((v (coa-visibility coa)))
    (and v
	 (implies (eq v :owner)
		  (eq (coa-owner coa) *user*)))))

(defmacro do-visible-coas ((var) &rest forms)
  `(dolist (,var *coas*)
     (when (coa-visible-p ,var)
       ,@forms)))

(defun visible-coas ()
  (remove-if-not #'coa-visible-p *coas*))

(defun coa-ghost-p (coa &optional (to-user *user*))
  (and (stringp (coa-visibility coa))
       (not (eq (coa-owner coa) to-user))))

(defun coa-visible-name (coa)
  (concat-string "COA-" (coa-visible-number coa)))

(defun coa-visible-number (coa)
  (if (coa-ghost-p coa) (coa-visibility coa) (coa-number coa)))


;;; Transferring a coa

;;; Always call give-coa, not give-coa-to-planner or give-coa-to-ta.
    
(defun give-coa (coa to-user)
  (web-note "~&Giving COA ~A (id ~S) to ~S.~%~%"
	    (coa-number coa) (coa-id coa) (user-name to-user))
  (ecase (user-name to-user)
    (:planner (give-coa-to-planner coa))
    (:task-assigner (give-coa-to-ta coa)))
  coa)

(defun give-coa-to-planner (coa)
  ;; When giving a COA to the planner, make it visible to the
  ;; Planner and a ghost to the TA.
  (setf (coa-owner coa) *planner-user*
	(coa-visibility coa) (coa-number coa)
	(coa-ghost coa) (coa-id coa)))

(defun give-coa-to-ta (coa)
  ;; When giving a COA to the TA, make it visible to the TA
  ;; and invisible to the Planner.  Moreover, if it has a ghost,
  ;; and the ghost is still a ghost, make the ghost invisible.
  (setf (coa-owner coa) *ta-user*
	(coa-visibility coa) :owner)
  (when (coa-ghost coa)
    (let ((ghost (get-coa (coa-ghost coa))))
      (setf (coa-ghost coa) nil)	;we're done with it
      (when (coa-ghost-p ghost *ta-user*)
	(setf (coa-visibility ghost) :owner)))))

;;; COA history
;;;
;;; A COA's plan may have been obtained by a complex process of splitting,
;;; replanning, and authority changes (not to mention answers to questions
;;; from O-Plan).  The COA's history is a record of this process, for use
;;; when wondering "where did this plan come from?" or "how did I get this
;;; coa?".
;;;
;;; The history events have the following forms:
;;;
;;;   (:CREATED-AS coa-number)
;;;   (:RENAMED new-number)
;;;   (:PLAN)
;;;   (:AUTHORITY settings)
;;;   (:ADD-ACTIONS actions)
;;;   (:ADD-TASK task-name)
;;;   (:SPLIT-FROM coa-number (ID coa-id) :AND-CALLED coa-number)
;;;   (:REPLAN-FROM coa-number (ID coa-id) :AND-CALLED coa-number)
;;;   (:ANSWER question-kwd question-agenda minimal-answer-data)
;;;   (:AUTO-REPLAN replan-number :BECAUSE reason)
;;;   (:AUTO-REPLAN :SELECTS option :WITH description :BECAUSE reason)
;;;
;;; :SPLIT-FROM and :REPLAN-FROM are recorded after the source coa
;;; has been renamed.
;;;
;;; :ADD-ACTIONS is recorded when the actions are actually added 
;;; just before planning, not when the add-actions form is submitted.
;;;
;;; :ADD-TASK is also recorded when the task is actually added,
;;; just before planning.
;;;
;;; COA histories are displayed on the server status page.
;;;

(defun coa-history-event (coa event-type &rest event-args)
  (ecase event-type
    ((:created-as :renamed :plan :authority :add-actions :add-task
      :split-from :replan-from :answer :auto-replan
      :exec :request-repair :resume-exec)
     (appendf1 (coa-history coa)
	       (cons event-type event-args)))))

;;; Plan issues

;;; The name "plan-issue" was chosen over "issue", because "issue"
;;; looked more likely to be given other meanings elsewhere.

;;; At present, all plan-issues are produced by plan evaluators.

;;; We eventually decided that issues should be visible even when
;;; their source evaluations are not.

(defun coa-plan-issues (coa)
  ;; /\/: Recomputed each time for historical reasons
  (mapcan #'(lambda (e)
	      (copy-list (eval-result-issues (alist-value e))))
	  (coa-evaluations coa)))

(defun coa-plan-issues-p (coa)
  (dolist (e (coa-evaluations coa) nil)
    (when (eval-result-issues (alist-value e))
      (return t))))

(defun coa-outstanding-plan-issues (coa)
  (remove-if #'plan-issue-done-p (coa-plan-issues coa)))


;;; Getting coa parameters

;;; E.g. (coa-arg coa 1 :city) looks up the :m1-city parameter.

;;; coa-arg-exists can be used in while loops to determine when we've
;;; reached mission numbers for which no parameter exists.  If an arg
;;; exists for mission number m, then args exist for missions 1 .. m.

(defun coa-arg (coa mission-number name)
  (lookup
    (coa-arg-key mission-number name)
    (coa-parameters coa)))

(defun coa-arg-exists (coa mission-number name)	; regardless of its value
  (assoc
    (coa-arg-key mission-number name)
    (coa-parameters coa)))

(defun coa-arg-key (mission-number name)
  (string->keyword (coa-arg-name mission-number name)))

(defun coa-arg-name (mission-number name)
  (concat-string "M" (int->string mission-number) "-" name))


;;; The initial situation applies to all missions, hence no mission parameter.

(defun situation-arg (coa name)
  (lookup name (coa-definition-parameters coa)))


;;; Per-COA files and the corresponding URLs

(defun coa-filename (coa name)
  (session-filename (format nil "coa-~A-~A" (coa-id coa) name)))

(defun coa-filename-url (coa name)
  (session-filename-url (format nil "coa-~A-~A" (coa-id coa) name)))


;;; Evaluations

;;; *Evaluations* lists the evals that _can_ be visible.  There may be
;;; other evals, not in the list.  (user-visible-evaluations *user*)
;;; lists the evals that will currently be shown.

(defparameter *evaluations*
  '((:number-of-actions   "actions in plan")
    (:plan-levels         "levels in plan")
    (:plan-actions-length "longest path length")
    (:duration            "minimum duration")
    (:n-psv-object-types  "object types")
    (:n-psv-values        "object values")
    (:effectiveness       "effectiveness")))

(defstruct (plan-eval (:type list))
  name					;a keyword
  description)				;a string

(defun visible-evaluation-p (e)
  ;; e is an alist-entry, typically (name . eval-result-struct).
  (member (plan-eval-name e)
	  (user-visible-evaluations *user*)
	  :key #'plan-eval-name))

(defun user-invisible-evaluations (user)
  (stable-set-difference *evaluations*
			 (user-visible-evaluations user)
			 :key #'plan-eval-name))

;;; Eval-results are obtained after planning.  The URL or report-fn + args
;;; are for "drill down".

(defstruct eval-result
  ;; If there's a URL, report-fn, report-args, and issues
  ;; must all be null.
  short-description			;string for the matrix cell
  url					;a URL or nil
  report-fn				;fn name to be applied to report-args
  report-args				;arglist
  issues)				;list of issues

(defun coa-plan-eval-result (coa e)
   (or (lookup (plan-eval-name e) (coa-evaluations coa))
       (error "Can't find evaluation ~S for COA ~A."
	      (plan-eval-name e) (coa-number coa))))

;;; Evaluation issues

(defvar *plan-issue-id* -1)

(defstruct plan-issue
  (id (incf *plan-issue-id*))		;for use in query-args
  contents				;a list containing a description
  done-p)				;has it been dealt with?

(defun plan-issue-verb (i)		;pseduo-accessor
  (car (plan-issue-contents i)))

(defun plan-issue-note-p (i)		;pseduo-accessor
  (eq (plan-issue-verb i) 'note))


;;;; Replan authority
;;;
;;; If we're allowed to automatically replan, we'll look for a plan
;;; that satisfies our filter requirements.  If we don't find one
;;; before :max-auto-replans, we'll take the best of the plans we've
;;; seen so far.
;;;
;;; /\/: Unfortunately, it won't always be clear which plan is "best",
;;; because different plans might be best according to different
;;; filter criteria.
;;;
;;; Keywords:
;;;
;;;   :auto-replan-p			;true or false
;;;   :max-auto-replans			;int
;;;   :filter-combiner			;:any-of or :all-of
;;;   :duration-filter			;int, a number of hours
;;;   :effectiveness-filter		;int, a percent
;;;

(defparameter *replan-authority-keywords*
  '(:auto-replan-p :max-auto-replans :filter-combiner
    :duration-filter :effectiveness-filter))

(defvar *replan-authority-table* (make-hash-table :test #'eq))

(defun replan-authority (kwd)
  (assert (member kwd *replan-authority-keywords*))
  (gethash kwd *replan-authority-table*))

(defun set-replan-authority (kwd value)
  (assert (member kwd *replan-authority-keywords*))
  (setf (gethash kwd *replan-authority-table*) value))

(defsetf replan-authority set-replan-authority)

(defun grant-replan-authority (&rest initargs)
  (walk-plist #'set-replan-authority initargs))

(grant-replan-authority			;defaults
  :auto-replan-p nil
  :max-auto-replans 5
  :filter-combiner :all-of
  :duration-filter 18
  :effectiveness-filter 70)

(defun raw-evals-satisfy-replan-filter (evals)
  (>= (lookup :effectiveness evals)
      (replan-authority :effectiveness-filter)))

(defun better-raw-evals-p (new-evals old-evals)
  (> (lookup :effectiveness new-evals)
     (lookup :effectiveness old-evals)))

;;; The replan-control struct

;;; This lets us keep information about the best plan found so far
;;; by auto-replanning while letting the corresponding coa slots
;;; contain the current information expected by e.g. question-
;;; answering.  It should also make it easier for us to generalize
;;; the current approach to return more than one plan, later on.

(defstruct (replan-control (:conc-name rc-))
  coa-id				;for debugging
  (replan-count 0)
  current-option
  plan-status
  plan-statistics
  plan-description
  plan-world-2
  evaluations
  raw-evaluations)

(defun coa-auto-replan-count (coa)
  (let ((rc (coa-replan-control coa)))
    (check-type rc replan-control)
    (assert (eql (rc-coa-id rc) (coa-id coa)))
    (rc-replan-count rc)))

(defun set-coa-auto-replan-count (coa c)
  (let ((rc (coa-replan-control coa)))
    (check-type rc replan-control)
    (assert (eql (rc-coa-id rc) (coa-id coa)))
    (setf (rc-replan-count rc) c)))

(defsetf coa-auto-replan-count set-coa-auto-replan-count)

(defun coa-has-a-favorite-plan-p (coa)
  (let ((rc (coa-replan-control coa)))
    (check-type rc replan-control)
    (assert (eql (rc-coa-id rc) (coa-id coa)))
    (not (null (rc-plan-description rc)))))

(defun better-raw-evals-for-coa-p (coa new-raw-evals)
  (let ((rc (coa-replan-control coa)))
    (check-type rc replan-control)
    (assert (eql (rc-coa-id rc) (coa-id coa)))
    (better-raw-evals-p new-raw-evals (rc-raw-evaluations rc))))

(defun install-replan-values-in-coa (coa)
  (let ((rc (coa-replan-control coa)))
    (check-type rc replan-control)
    (assert (eql (rc-coa-id rc) (coa-id coa)))
    (setf (coa-current-option coa) (rc-current-option rc)
	  (coa-plan-status coa) (rc-plan-status rc)
	  (coa-plan-statistics coa) (rc-plan-statistics rc)
	  (coa-plan-description coa) (rc-plan-description rc)
	  (coa-plan-world-2 coa) (rc-plan-world-2 rc)
	  (coa-evaluations coa) (rc-evaluations rc)
	  (coa-raw-evaluations coa) (rc-raw-evaluations rc))))


;;;; Define a COA

;;; Here's where we use the information from the COA definition form.

;;; We provide a kind of rolling default for at least some coa-definition
;;; parameters by remembering he most recently defined COA so that its
;;; coa-definition-parameters can provide the defaults.

(defvar *most-recently-defined-coa* nil)

(defun define-coa ()
  (parse-query-args)
  (convert-query-args *coa-definition-parameters*)
  (let ((coa (get-coa (query-arg :n))))
    ;; Don't allow redefinition if the user has somehow reused the URL
    (when (coa-defined-p coa)
      (bogus-matrix-request-error))
    ;; Remember the Coa so its defintion query-args can be used
    ;; as a "rolling default".
    (setq *most-recently-defined-coa* coa)
    ;; New COAs should split down rather than across.
    (setf (coa-change-p coa) t)
    ;; Record parameters from the form.
    (setf (coa-definition-parameters coa)
	  (setf (coa-parameters coa)
		(hash-table-alist *query-arg-table*)))
    ;; Initialize the COA
    (setf (coa-initial-effects coa)
	  (get-coa-initial-effects coa))
    (setf (coa-added-actions coa)
	  nil)
    (setf (coa-actions-to-add coa)
	  (get-coa-task-actions coa))
    (set-coa-base-option coa)
    (add-coa-initial-effects coa)
    (capture-current-context-for-coa coa)
    coa))


;;;; Split COA

(defun split-coa (&optional (coa (get-coa (query-arg :n))))
  (unless (coa-active-p coa)
    (bogus-matrix-request-error))
  (web-note "~&Splitting COA ~A~%~%" (coa-number coa))
  (let ((new (make-coa-sibling coa)))
    (coa-history-event new :split-from (coa-number coa) `(id ,(coa-id coa))
		           :and-called (coa-number new))
    ;; The new coa gets its own current-option, a twin of its sibling's.
    (move-planner-to-coa-option coa)
    (send-to-oplan :status-after
      '(:twin-option))
    (receive-else-error '(:option $twin)
      #'(lambda (twin)
	  (check-status-after-move-to-coa-option coa)
	  (setf (coa-current-option new) twin)))
    (web-note-option-tree)))

(defun make-coa-sibling (coa)
  ;; N.B. Modifies coa.
  (web-note "~&Creating a sibling of COA ~A (id ~S)~%~%"
	    (coa-number coa) (coa-id coa))
  (let ((new (copy-coa coa)))
    (setf (coa-id new) (1+ (length *coas*)))
    (setf (coa-number new) nil)
    (setf (coa-change-p new) nil)
    (setf (coa-current-option new) nil)
    (setf (coa-history new) nil)
    (setf (coa-return-p new) t)
    ;; Siblings of ghosts are not ghosts
    (when (stringp (coa-visibility coa))
      (setf (coa-visibility new) :owner))
    ;; Straighten out the names
    (cond ((coa-change-p coa)
	   ;; The coa has changed since created, so we split it in two.
	   (let ((base-number (coa-number coa)))
	     (set-coa-number coa (concat-string base-number ".1"))
	     (set-coa-number new (concat-string base-number ".2"))
	     (coa-history-event coa :renamed (coa-number coa))
	     (setf (coa-change-p coa) nil)))
	  (t
	   ;; The coa has not changed, so we put the new coa at the
	   ;; same level, leaving the original coa as-is.
	   (set-coa-number new
	     (find-next-coa-number (coa-number coa)))))
    ;; Add the new coa to the list of all coas.
    (setq *coas* (insert-new-coa new))
    ;; N.B. Our caller has to set the new coa's current option
    ;; and should call web-note-option-tree.
    new))

(defun set-coa-number (coa number)
  ;; Checks that the number is unique
  (when (coa-number coa)
    (web-note "~&COA ~A will now have number ~A~%~%" (coa-number coa) number))
  (assert (not (find-coa-numbered number)))
  (setf (coa-number coa)
	number))

(defun find-coa-numbered (number)
  (find number *coas* :key #'coa-number :test #'string=))

(defun find-next-coa-number (base-number)
  ;; If number is 3.1.2, we want 3.1.n for the first n > 2
  ;; such that coa 3.1.n does not already exist.
  (multiple-value-bind (base n) (break-string-at-last #\. base-number)
    (assert (> (length n) 0))
    (loop for i from (string->int n) do
      (let ((num (concat-string base "." (int->string i))))
	(unless (find-coa-numbered num)
	  (return-from find-next-coa-number num))))))

(defun insert-new-coa (new-coa)
  ;; Makes sure the *coas* list is in order of increasing coa-number
  (let ((n (coa-number new-coa)))
    (label scan ((coas *coas*))
      (if (null coas)
	  (list new-coa)
	(if (coa-number-lessp n (coa-number (car coas)))
	    (cons new-coa coas)
	  (cons (car coas) (scan (cdr coas))))))))

(defun coa-number-lessp (n1 n2)		;needn't be very fast
  (list-lessp #'<
	      (mapcar #'string->int (break-string-at #\. n1))
	      (mapcar #'string->int (break-string-at #\. n2))))


;;;; Add constraints to a COA

;;; /\/: For now, only actions can be added

(defun add-constraints-to-coa ()
  (parse-query-args)
  (convert-query-args *coa-addition-parameters*)
  (let ((coa (get-coa (query-arg :n))))
    (unless (coa-active-p coa)
      (bogus-matrix-request-error))
    (setf (coa-change-p coa) t)
    (setf (coa-parameters coa)
	  (hash-table-alist *query-arg-table*))
    (appendf (coa-actions-to-add coa)
	     (get-coa-task-actions coa))
    ;; Information from any earlier plan is now invaid.
    (clear-coa-current-plan coa)))


;;;; Set COA authority

(defparameter *coa-authority-parameters*
  '((:n     (:int* 0)   "COA number")
    (:level (:level)    "plan level authority")))

(defun set-coa-authority ()
  (parse-query-args)
  (convert-query-args *coa-authority-parameters*)
  (let ((coa (get-coa (query-arg :n))))
    (unless (and (coa-active-p coa)
		 (makes-sense-to-change-authority-for-coa-p coa))
      (bogus-matrix-request-error))
    (let ((authority `((:level . ,(query-arg :level)))))
      (coa-history-event coa :authority authority)
      (setf (coa-authority coa) authority))))



;;;; Plan for a COA

(defparameter *coa-planning-time-limit* 60) ;seconds

(define-condition no-plan (simple-condition) ())

(defun plan-for-coa (&optional (coa (get-coa (query-arg :n))))
  ;; Check that the user hasn't gone back to an old URL.
  (unless (and (coa-active-p coa) (makes-sense-to-plan-for-coa-p coa))
    (bogus-matrix-request-error))
  ;; Clear any existing plan info
  (clear-coa-current-plan coa)
  ;; Plan
  (handler-case (ask-for-plan-for-coa coa)
    (timeout ()		  
      (error "Planning took more than the time limit of ~S seconds"
	     *coa-planning-time-limit*))))

;;; Clear out information about the current plan, if there is one.

(defun clear-coa-current-plan (coa)
  (setf (coa-plan-statistics coa) nil
	(coa-plan-description coa) nil
	(coa-plan-world-2 coa) nil
	(coa-raw-evaluations coa) nil
	(coa-evaluations coa) nil
	(coa-replan-control coa)
	  (make-replan-control :coa-id (coa-id coa))))

;;; Is there a plan (suitable for viewing)?

;;; have-plan-for-coa-p should return true only if the "View" entry
;;; in the matrix should be a link, meaning that there are views we
;;; want the user to be able to look at.

;;; For status :partial or :complete, we pick one of the several things
;;; that should be non-nil after we've found a plan and that are reset
;;; to nil when we drop the plan by clear-coa-current-plan.

;;; Note that we can't check only the status, because we keep the status
;;; even when we drop the current plan by calling clear-coa-current-plan.

;;; Nor can we check only whether (coa-plan-description coa) is not null,
;;; because it might not be null when the status is :question.

(defun have-plan-for-coa-p (coa)
  (ecase (coa-plan-status coa)
    ((:partial :complete)
     (not (null (coa-plan-description coa))))
    ((:question :failure nil)
     nil)))

(defun plan-failure-for-coa-p (coa)
  ;; Should return true only if we tried to get a plan and failed.
  ;; /\/: The reason code doesn't just check the plan-status directly
  ;; is that it might turn out that we need a more sophisticated test.
  (eq (coa-plan-status coa) :failure))


;;;; COA option-related operations

;;; These routines are used when defining a COA, splitting a COA, or
;;; planning for a COA.

(defvar *planning-coa* nil)		;for checking

(defun set-coa-base-option (coa)
  (setq *planning-coa* coa)
  (web-note-set-option 'option-1)
  (send-to-oplan :status-after '(:set-option option-1))
  (receive-else-error '(:option option-1))
  (receive-else-error '(:status ()))
  (send-to-oplan :status-after '(:push-option))
  (receive-else-error '(:option $opt)
    #'(lambda (opt)
        (receive-else-error '(:status ()))
	(web-note "~&COA ~A (id ~S) base option: ~S~%~%"
		  (coa-number coa) (coa-id coa) opt)
	(setf (coa-base-option coa) opt)
	(web-note-option-tree)
	opt)))

(defun add-coa-initial-effects (coa)
  (assert (eq coa *planning-coa*))
  (check-oplan-current-option (coa-base-option coa))
  (set-authority :level :inf)		; :inf? /\/
  (apply #'send-to-oplan :add-to-task :all-of
    (mapcar #'(lambda (pv-pair) (cons :initially pv-pair))
	    (coa-initial-effects coa)))
  (assert (receive-plan-status)))

(defun capture-current-context-for-coa (coa)
  (assert (eq coa *planning-coa*))
  (send-to-oplan :push-option)
  (receive-else-error '(:option $push-opt)
    #'(lambda (push-opt)
	;; Record the current option as the COA's current option.
	;; It should be the same as the result of the :push.
	(set-coa-current-option coa)
	(assert (eq (coa-current-option coa) push-opt)))))

(defun set-coa-current-option (coa)
  ;; Record the planner's current option.
  (assert (eq coa *planning-coa*))
  (send-to-oplan :get-option)
  (receive-else-error '(:option $opt)
    #'(lambda (opt)
	(web-note "~&COA ~A current option: ~S~%~%" (coa-number coa) opt)
	(setf (coa-current-option coa) opt)
	(web-note-option-tree))))

(defun check-oplan-current-option (expected-name)
  (send-to-oplan :get-option)
  (receive-else-error `(:option ,expected-name)))

;;; ask-for-plan-for-coa should handle all task-additions in a uniform
;;; way rather than handling only adding actions, or adding a task, and
;;; treating them as special cases.

(defun ask-for-plan-for-coa (coa)
  (setq *planning-coa* coa)
  (setf (coa-change-p coa) t)
  (let ((new-actions (coa-actions-to-add coa)))
    ;; Record the new actions as added.
    (appendf (coa-added-actions coa)
	     new-actions)
    (setf (coa-actions-to-add coa) nil)
    (setf (coa-plan-statistics coa) nil)
    ;; Move to the coa's current option.
    (move-planner-to-coa-option coa)
    ;; Reset agenda counters, so the planning statistics will be right,
    ;; and so we get a fresh start at the cycle count limit.
    (send-to-oplan :reset-agenda-counters)
    (receive-else-error '(:ok))
    ;; Add task
    (let ((task (lookup :task (coa-definition-parameters coa))))
      (when (and task (null (coa-plan-status coa)))
	(assert (null new-actions))
	(coa-history-event coa :add-task task)
	(send-to-oplan :add-to-task :task task)
	(receive-else-error :nothing)))
    ;; Add actions.
    (when new-actions
      (coa-history-event coa :add-actions new-actions)
      (apply #'send-to-oplan :add-to-task :all-of
        (mapcar #'(lambda (act) (list :action act))
		new-actions))
      (receive-else-error :nothing))
    ;; Set the authority for this COA.
    (set-planner-authority-for-coa coa)
    ;; See if we have a plan.
    (coa-history-event coa :plan)
    (pprocess-main-loop)		;a chance to use the new authority
    (get-planning-results coa)))

(defun get-planning-results (coa)
  (let ((plan-status
	 (cond ((receive-from-oplan '(:finished))
		:complete)
	       ((receive-from-oplan '(:no-more-alternatives))
		:failure)
	       ((receive-from-oplan '(:waiting (:authority :triggers)))
		:partial)
	       ((and (eql 0 (lookup :level (coa-authority coa)))
		     (receive-from-oplan :nothing))
		:partial)
	       ((receive-question-from-oplan coa)
		:question)
	       ((receive-else-error '$status
		  #'(lambda (status)
		      (error "Bad status after planning: ~S" status)))))))
    ; (let ((*print-pretty* t))
    ;   (web-note "~%~%Alts:~%~S~%~%" *alternatives*))

    ;; Auto-replanning replaces the normal processing below.
    (when (and (replan-authority :auto-replan-p)
	       (not (eq plan-status :question)))
      (return-from get-planning-results
	(handle-auto-replan coa plan-status)))

    (web-note "~&Plan status: ~S~%~%" plan-status)
    (setf (coa-plan-status coa) plan-status)
    (setf (coa-plan-statistics coa) (request-plan-statistics-list))
    (web-note "~&Statistics: ~S~%~%" (coa-plan-statistics coa))

    ;; In each plan-status case:
    ;;  - Maybe push an option to capture the plan.
    ;;  - Get some information about the plan.
    ;;  - Return success (true) or failure (false).
    (ecase plan-status
      ((:partial :complete)
       (web-note-success)
       (capture-current-context-for-coa coa)
       (setf (coa-plan-description coa) (request-plan-view-list))
       (setf (coa-evaluations coa) (get-coa-plan-evaluations coa))
       (setf (coa-plan-world-2 coa) (request-world-view-list "2"))
       t)
      ((:question)
       (setf (coa-plan-description coa) (request-plan-view-list))
       (setf (coa-evaluations coa) (get-coa-plan-evaluations coa))
       t)
      (:failure
       (web-note-failure "no plan was possible")
       ;; Clear plan info in case we grabbed some while answering a :question
       (clear-coa-current-plan coa)
       nil))))

(defun handle-auto-replan (coa plan-status)
  (web-note "~&Handling status ~S for COA ~A (id ~S) ~
              when auto-replan is enabled.~%~%"
	    plan-status (coa-number coa) (coa-id coa))

  ;; The :failure case
  (when (eq plan-status :failure)
    (web-note-failure "no (further) plan was possible")
    ;; If we already have a plan, keep it; else look like a normal failure
    (cond ((coa-has-a-favorite-plan-p coa)
	   (web-note "~&Keeping existing plan~%~%")
	   (install-replan-values-in-coa coa)
	   (auto-replan-selects-history-event coa :out-of-alternatives)
	   (return-from handle-auto-replan t))
	  (t
	   (setf (coa-plan-status coa) plan-status)
	   (setf (coa-plan-statistics coa) (request-plan-statistics-list))
	   (web-note "~&Statistics: ~S~%~%" (coa-plan-statistics coa))
	   (return-from handle-auto-replan nil))))

  ;; We have a new plan, but we don't want to create an option for
  ;; it unless its the best so far.  This is just to avoid creating
  ;; an option we know we won't need.  The cost of an option is
  ;; chiefly the space taken by its list of alternatives.
  (let* ((rc (coa-replan-control coa))
	 (saved-raw-evals (coa-raw-evaluations coa))
	 (stats (request-plan-statistics-list))
	 (plan-descr (request-plan-view-list))
	 (evals (get-coa-plan-evaluations coa))	;sets coa-raw-evaluations /\/
	 (new-raw-evals (coa-raw-evaluations coa))
	 (world-2 (request-world-view-list "2"))
	 (win-p (raw-evals-satisfy-replan-filter new-raw-evals)))
    (setf (coa-raw-evaluations coa) saved-raw-evals)
    (check-type rc replan-control)

    (web-note-success)
    (web-note "~&Statistics: ~S~%~%" stats)
    (web-note "~&Effectiveness: ~S~%~%" (lookup :effectiveness new-raw-evals))

    ;; If this is the first plan, or if it satisfies the filter,
    ;; or if it's the best plan so far, save it in the coa.
    (when (or win-p
	      (not (coa-has-a-favorite-plan-p coa))
	      (better-raw-evals-for-coa-p coa new-raw-evals))

      (web-note "~&Best plan so far for this auto-replan sequence.~%~%")
      (setf (coa-plan-status coa) plan-status)
      (setf (coa-plan-statistics coa) stats)

      ;; Capture the current context as a new option, but then put O-Plan
      ;; back at the parent (ie, to where we were before creating this new
      ;; option) in case we want to do some more replanning.
      (capture-current-context-for-coa coa)
      (setf (rc-current-option rc) (coa-current-option coa))
      (move-planner-to-parent-option-for-replanning coa)

      (setf (rc-plan-status rc) plan-status)
      (setf (rc-plan-statistics rc) stats)
      (setf (rc-plan-description rc) plan-descr)
      (setf (rc-raw-evaluations rc) new-raw-evals)
      (setf (rc-evaluations rc) evals)
      (setf (rc-plan-world-2 rc) world-2))

    ;; Should we do some more replanning?
    (cond (win-p
	   ;; We're done!
	   (web-note "~&Replan filter satisfied~%~%")
	   (install-replan-values-in-coa coa)
	   (auto-replan-selects-history-event coa :satisfies-filter)
	   t)
	  ((< (coa-auto-replan-count coa) (replan-authority :max-auto-replans))
	   ;; We can and should replan.
	   (incf (coa-auto-replan-count coa))
	   (web-note "~&Replan number ~S~%~%" (coa-auto-replan-count coa))
	   (coa-history-event coa
             :auto-replan (coa-auto-replan-count coa) :because
	     `(:effectiveness = ,(lookup :effectiveness new-raw-evals)))
	   (send-to-oplan :replan)
	   (get-planning-results coa))
	  (t
	   ;; We can't do any more replans, so we're stuck with
	   ;; the best plan so far.
	   (web-note "~&Maxed out on replans~%~%")
	   (install-replan-values-in-coa coa)
	   (auto-replan-selects-history-event coa :reached-max-replans)
	   t))))

(defun auto-replan-selects-history-event (coa &rest reason)
  (apply #'coa-history-event coa
    :auto-replan :selects (coa-current-option coa)
    :with `(:effectiveness =
	     ,(lookup :effectiveness (coa-raw-evaluations coa)))
    :because reason))

(defun move-planner-to-coa-option (coa)
  (let ((coa-option (coa-current-option coa)))
    (web-note "~&Set option = ~S from COA ~A (id ~S)~%~%"
	      coa-option (coa-number coa) (coa-id coa))
    ;; Make sure we don't start planning when moving from a COA that's
    ;; grated more authority.
    (set-authority :level -1)
    ;; Move to the coa's current option.
    (send-to-oplan :status-after `(:set-option ,coa-option))
    (receive-else-error `(:option ,coa-option))
    ;; Check the status
    (check-status-after-move-to-coa-option coa)
    ;; Return the coa
    coa))

(defun check-status-after-move-to-coa-option (coa)
  (receive-else-error '(:status $status)
    #'(lambda (status)
	(ecase (coa-plan-status coa)
	  ((nil :complete)
	   (match-else-error '() status))
	  ((:partial)
	   (match-else-error '(:waiting (:authority :triggers)) status))
	  ((:question)
	   (match-else-error '(:waiting (:triggers)) status))
	  ((:failure)
	   (error "Moving to COA ~A (id ~S) option after plan failure."
		  (coa-number coa) (coa-id coa)))))))

(defun set-planner-authority-for-coa (coa)
  ;; /\/: Assume only level authority is working.
  (push (coa-authority coa)
	(coa-authority-history coa))
  (set-authority :level (or (lookup :level (coa-authority coa)) :inf)))


;;;; Replanning

;;; Replan-for-coa handles :replan URLs.

;;; This parallels the (complicated) code for planning.

;;; This version creates a new coa, rather than relying on the user
;;; to do that, when desired, by using "split".

(defun replan-for-coa (&optional (coa (get-coa (query-arg :n))))
  ;; Check that the user hasn't gone back to an old URL.
  ;; [See write-planning-coa-matrix-plan-or-replan-row.]
  (unless (and (coa-active-p coa)
	       (not (makes-sense-to-plan-for-coa-p coa))
	       (have-plan-for-coa-p coa))
    (bogus-matrix-request-error))
  ;; Create a sibling of the specified coa.
  (let ((new (make-coa-sibling-for-replan coa)))
    (coa-history-event new :replan-from (coa-number coa) `(id ,(coa-id coa))
		           :and-called (coa-number new))
    ;; Make the replan coa the coa we're working on -- for obey-path-action /\/
    (web-note "~&Changing coa id from ~S to ~S~%~%"
	      (query-arg :n) (coa-id new))
    (set-coa new)
    ;; Clear any existing plan info
    (clear-coa-current-plan new)
    (handler-case (ask-for-replan-for-coa new)
        (timeout ()		  
	  (error "Replanning took more than the time limit of ~S seconds"
		 *coa-planning-time-limit*)))))

(defun make-coa-sibling-for-replan (coa)
  ;; This makes a sibling in the same way as split-coa.
  (let ((new (make-coa-sibling coa)))
    ;; We'll give the new coa the same current option as its sibling
    ;; To replan, we move to the parent option, replan, then make
    ;; a new option that becomes the new coa's current option.
    (setf (coa-current-option new)
	  (coa-current-option coa))
    (web-note-option-tree)
    new))

(defun ask-for-replan-for-coa (coa)

  (setq *planning-coa* coa)
  ;; unlike for planning, we don't (setf (coa-change-p coa) t)

  ;; We know there can be no actions to add because of the not makes-sense-...
  ;; check above.  [See the definition of makes-sense-to-plan-for-coa-p.]
  ;; But we have the let anyway to parallel the planning code.
  (let ((new-actions (coa-actions-to-add coa)))
    (assert (null new-actions))

    ;; Move to the coa's current option.  This first sets :level authority
    ;; to -1 to ensure that no planning occurs.
    (move-planner-to-coa-option coa)

    ;; Reset agenda counters, so the planning statistics will be right,
    ;; and so we get a fresh start at the cycle count limit.
    ;; /\/: It's not clear that we really need to do this when replanning. 
    (send-to-oplan :reset-agenda-counters)
    (receive-else-error '(:ok))

    ;; At this point, if we were planning, we'd set the correct authority
    ;; for this coa and then call pprocess-main-loop.  We allow planning
    ;; only in cases where actions have been added, or authority has
    ;; increased; and so planning will start as soon as we transfer
    ;; control to O-Plan by calling pprocess-main-loop.  But for
    ;; replanning, we should have to explicitly send a :replan message.
    ;; Unfortunately, it's not clear just what state O-Plan is in,
    ;; because we have to move to the parent option.  Fortunately,
    ;; the call to move-planner-to-coa-option above ensures that no
    ;; planning will occur until we allow it.

    ;; Move to the parent option
    (move-planner-to-parent-option-for-replanning coa)

    ;; Set the authority for this COA.
    ;; /\/: Change set-planner-authority-for-coa if necessary, so that
    ;;      we can call it here instead of doing things by hand.
    (push (coa-authority coa)
	  (coa-authority-history coa))
    ;; /\/: Assume only level authority is working.
    (let ((level (or (lookup :level (coa-authority coa)) :inf)))
      (send-to-oplan :status-after
        `(:authority :level ,level))
      (receive-else-error `(:authority :level ,level))
      ;; We want to make sure O-Plan won't start planning.
      (check-status-after-move-to-coa-option coa))

    ;; Ask for a replan
    (send-to-oplan :replan)

    ;; The rest should be just like when we're planning.
    (get-planning-results coa)))

(defun move-planner-to-parent-option-for-replanning (coa)
  (web-note "~&Moving to parent option for replanning.~%~%")
  (send-to-oplan :status-after
    '(:pop-option))
  (receive-else-error '(:option $parent)
    #'(lambda (parent)
	(check-status-after-move-to-coa-option coa)
	(set-coa-current-option coa)
	(assert (eq (coa-current-option coa) parent)))))


;;;; Answering a question

(defun receive-question-from-oplan (coa)
  (receive-from-oplan '(:question $kwd $ag-body $data)
    :succeed
      #'(lambda (kwd ag-body data)
	  ;; A status should follow
	  (receive-else-error '(:waiting (:triggers)))
	  ;; Remember everything about the question
	  (setf (coa-question-kwd coa) kwd
		(coa-question-agenda coa) ag-body
		(coa-question-data coa) data)
	  t)))

(defun answer-question-for-coa (coa answer-data)
  ;; Check that the user hasn't gone back to an old URL.
  ;; [See write-planning-coa-matrix-plan-or-replan-row.]
  (unless (and (coa-active-p coa)
	       (eq (coa-plan-status coa) :question))
    (bogus-matrix-request-error))
  ;; Back to planning.
  ;; /\/: We should remember whether it was planning or replanning.
  ;; /\/: We should decrease the time limit by the time spent so far.
  (handler-case (resume-planning-for-coa coa answer-data)
      (timeout ()		  
	(error "[Re]planning took more than the time limit of ~S seconds"
	       *coa-planning-time-limit*))))

(defun resume-planning-for-coa (coa answer-data)

  (setq *planning-coa* coa)

  ;; We know there can be no actions to add, but we have the let anyway
  ;; to parallel the planning code.
  (let ((new-actions (coa-actions-to-add coa)))
    (assert (null new-actions))

    (web-note "~&Option now is: ~S~%~%" (ask-oplan :get-option))

    ;; Move to the coa's current option.  This first sets :level authority
    ;; to -1 to ensure that no planning occurs.
    (move-planner-to-coa-option coa)

    ;; Set the authority for this COA.
    ;; /\/: Change set-planner-authority-for-coa if necessary, so that
    ;;      we can call it here instead of doing things by hand.
    (push (coa-authority coa)
	  (coa-authority-history coa))
    ;; /\/: Assume only level authority is working.
    (let ((level (or (lookup :level (coa-authority coa)) :inf)))
      (send-to-oplan :status-after
        `(:authority :level ,level))
      (receive-else-error `(:authority :level ,level))
      ;; We want to make sure O-Plan won't start planning.
      (check-status-after-move-to-coa-option coa))

    ;; Send the answer.
    ;; Note the the coa-history-event is earlier so that we can
    ;; record only minimal answer-data, which is question-specific.
    (send-to-oplan :answer (coa-question-kwd coa) answer-data)

    ;; Keep track of what we've done.
    (question-has-been-answered coa answer-data)

    ;; The rest should be just like when we're planning.
    (get-planning-results coa)))

(defun question-has-been-answered (coa answer-data)
  (nconcf1 (coa-question-history coa)
	   (list (shiftf (coa-question-kwd coa) nil)
		 (shiftf (coa-question-agenda coa) nil)
		 (shiftf (coa-question-data coa) nil)
		 answer-data))
  #+:undef
  (web-note "~&COA ~A (id ~S) question-history:~%~S~%~%"
	    (coa-number coa) (coa-id coa) (coa-question-history coa)))


;;;; Some utilities

(defun match-else-error (pat dat)
  (or (match pat dat)
      (error "Expected ~S, but found ~S." pat dat)))

(defun web-note-set-option (opt-name)
  (web-note "~&Set option: ~S~%~%" opt-name))

(defun web-note-option-tree ()
  (send-to-oplan :get-option-tree)
  (receive-else-error '(:option $tree)
    #'(lambda (option-tree)
	(web-note "~&Option tree:~%~%")
	(print-option-tree-description option-tree *web-notes*))))

;;; Option tree description

;;; After def 1, plan, def 2, split, the tree looks like this:

#|
root-option
  option-1
    option-1-1                           (coa 1 (id 1) base)
      option-1-1-1
        option-1-1-1-1                   (coa 1 (id 1) current)
    option-1-2
       (coa 2.1 (id 2) base)
       (coa 2.2 (id 3) base)
      option-1-2-1                       (coa 2.1 (id 2) current)
      option-1-2-2                       (coa 2.2 (id 3) current)
|#

(defun print-option-tree-description (option-tree &optional (stream t))
  (label walk ((tree option-tree)
	       (indent 0))
    (let* ((opt (car tree))
	   (uses (option-use-in-coas opt)))
      (case (length uses)
        (0 (format stream "~&~vT~A" indent opt))
	(1 (format stream "~&~vT~A~40T ~A" indent opt (car uses)))
	(t (format stream "~&~vT~A" indent opt)
	   (dolist (u uses)
	     (format stream "~&~vT~A" (+ indent 3) u))))
      (dolist (child (cdr tree))
        (walk child (+ indent 2)))))
  (web-note "~%~%"))

(defun option-use-in-coas (opt)
  ;; The number of COAs should be small enough for linear search.
  (let ((notes '()))
    (dolist (coa *coas* notes)
      (let ((n (coa-number coa))
	    (i (coa-id coa)))
	(when (eq opt (coa-base-option coa))
          (nconcf1 notes `(coa ,n (id ,i) base)))
	(when (eq opt (coa-current-option coa))
	  (nconcf1 notes `(coa ,n (id ,i) current)))))))


;;;; Plan evaluation

;;; This is a rather long section, because it contains some plan evaluators,
;;; plus code for explaining eval results when the user "drills down".

;;; Get plan evaluations

(defun get-coa-plan-evaluations (coa)
  ;; /\/: May want to check that everything in *evaluations* is there.
  (send-to-oplan :eval-plan)
  (receive-else-error '(:evaluation $evaluation-alist)
    #'(lambda (evals)
	(setf (coa-raw-evaluations coa) evals) ;for debugging
	; (web-note "~&Raw Evals: ~S~%~%" evals)
	(loop for e in evals
	      for r = (make-standard-eval-result coa evals e)
	      when r
	        collect (cons (alist-key e) r)))))

;;; The standard evaluations returned when we send :eval-plan to
;;; O-Plan are:
;;;
;;;   :number-of-nodes      The number of nodes in the plan.
;;;   :plan-length          The number of links in the longest path from
;;;                           begin_of node-1 to end_of node-2.
;;;   :duration             The earliest finish time of end_of node-2 minus
;;;                           the earliest start time of begin_of node-1.
;;;   :psv-object-types     A list of all PSV object types, with duplicates
;;;                           removed.
;;;   :psv-values           A list of all PSV values, with duplicates removed.
;;;   :n-psv-object-types   The number of different PSV object types.
;;;   :n-psv-values         The number of different PSV values.
;;;
;;; And we define:
;;;
;;;   :number-of-actions    The number of action nodes in the plan.
;;;   :plan-actions-length  The number of links in the longest path, but
;;;                           ignoring non-action nodes.
;;;   :exhausted-psv-types  A list of all PSV object types such that every
;;;                           value in the type has been used by a PSV.
;;;   :effectiveness        A numeric measure of effectiveness (percent),
;;;                           depends on the domain.
;;;   :plan-levels          A p-list of :expansion-tree, :depth-list, and
;;;                           :waiting-expansions (see below).
;;;   :timeline             /\/
;;;

(defun make-standard-eval-result (coa evals e)
  (let ((key (alist-key e))
	(value (alist-value e)))
    (ecase key
      (:number-of-nodes nil)		; :number-of-actions is used instead
      (:plan-length nil)		; :plan-actions-length is used instead
      (:number-of-actions
       (make-eval-result
	 :short-description (int->string value)
	 ; was :url (coa-filename-url coa "narrative.txt")
	 :url (coa-path-action-url :narrative coa)))
      (:plan-actions-length
       (make-eval-result
	 :short-description (int->string value)))
      (:duration
       (make-duration-eval-result (car value)))
      ((:psv-object-types
	:psv-values
	:exhausted-psv-types)
       ;; Used in other results rather than becoming results themselves.
       nil)
      (:n-psv-object-types
       (make-eval-result
	 :short-description (int->string value)
	 :report-fn 'explain-value-list
	 :report-args
	   (list value "object-types used" (lookup :psv-object-types evals))
	 :issues
	   (psv-object-type-issues evals)))
      (:n-psv-values
       (make-eval-result
	 :short-description (int->string value)
	 :report-fn 'explain-value-list
	 :report-args
	   (list value "object values used" (lookup :psv-values evals))))
      (:effectiveness
       (make-effectiveness-eval-result value))
      (:plan-levels
       (make-plan-level-eval-result value))
      (:timeline			;/\/
       nil))))

(defun explain-value-list (value description items)
  (if (= value 0)
      (html-line "There were no ~A." description)
    (progn
      (html-line "~A ~A:" value description)
      (html-block "blockquote"
	(html-line "~{~S~^, ~}."
	  (canonical-description-order
	    items))))))


;;;; New plan evaluators

;;; The number of actions

(define-plan-evaluator number-of-actions ()
  (let ((count 0))
    (walk-nodes
      #'(lambda (node)
	  (when (eq (n-type node) 'action)
	    (incf count))))
    (record-eval :number-of-actions count)))

;;; The length of the longest path, treating non-actions as invisible.

(define-plan-evaluator plan-actions-length ()
  (let* ((ne-distance-table
	  (find-longest-path-lengths
	    (get-node-end (etag 'node-1 :begin))
	    #'ne-post-action-ends))
	 (len (gethash (get-node-end (etag 'node-2 :end))
		       ne-distance-table)))
    ;; Should have begin_of 1 --> end_of 1 --> 1st action --> ... 
    ;;   ... --> begin_of 2 --> end_of 2, and hence 4 extra links.
    (record-eval
      :plan-actions-length (- len 4))))

(defun ne-post-action-ends (ne)
  (let ((result '()))
    (dolist (end (ne-post-ends ne))
      (if (not (eq (n-type (ne-node end)) 'dummy))
	  (pushnew end result)
	(dolist (e (ne-post-action-ends end))
	  (pushnew e result))))
    (nreverse result)))

;;; Type exhaustion

(define-plan-evaluator type-exhaustion ()
  (record-eval :exhausted-psv-types
    (canonical-description-order
      (find-exhausted-psv-types))))

(defun find-exhausted-psv-types ()
  (let ((value-table (make-hash-table :test #'eq)))
    ;; Fill table mapping type names to used values
    (map-over-psvs
      #'(lambda (tag body)
	  (declare (ignore tag))
	  (let ((type (oplan-psv::psv-body-type body))
		(value (oplan-psv::psv-body-value body)))
	    (nconcf-new (gethash type value-table '())
			value))))
    ;; Find the types for which all values have been used.
    (let ((exhausted '()))
      (maphash #'(lambda (type used)
		   (when (subsetp (psv-get-possibles-for-type type) used)
		     (push type exhausted)))
	       value-table)
      exhausted)))

(defun psv-object-type-issues (evals)
  (let ((exhausted (lookup :exhausted-psv-types evals)))
    (mapcar #'(lambda (type)
		(make-plan-issue
		  :contents
		    `(note "plan uses every value in type"
			   ,type
			   ,(psv-get-possibles-for-type type))))
	    exhausted)))

;;; Duration

;;; This is not a new evaluator, but we have to construct an "interesting"
;;; result for it.  Since it's related to the current effectiveness measure,
;;; we put it here.

(defun make-duration-eval-result (seconds)
  (when (null (situation-arg *coa* :time-limit))	;/\/
    (return-from make-duration-eval-result
      (make-eval-result
        :short-description
	  ;; /\/: seconds->description gives "" for 0
	  (if (zerop seconds)
	      "0"
	    (format nil "~A" (seconds->description seconds))))))
  (let* ((time-limit (situation-arg *coa* :time-limit)) ; hours
	 (deadline (* time-limit 60 60))) 		; seconds
    ;; /\/: allow float rounded to 10ths?
    (make-eval-result
      :short-description 
        (format nil "~D hrs" (/ seconds 60 60))
      :issues
        (cond ((> seconds deadline)
	       (list
		 (make-plan-issue
		   :contents
		     '(modify requirements or resources
		       because time limit exceeded))))))))

;;; Effectiveness

;;; The idea is to find a mean effectiveness over all action nodes,
;;; where the effectiveness of a node is 100% if its eft is 0 and 0%
;;; if its eft is >= a deadline:

;;;   ((-100 / deadline) * (min eft deadline)) + 100

;;; But it turns out that in gpdt3 the low-level actions tend to
;;; have zero duration, so they in effect cluster at the beginnings
;;; of the larger actions that contain them and hence increase the
;;; effectiveness probably more than is right.  So for now I'll
;;; exclude actions that appear to have zero duration.  /\/
;;; [Taken out at JohnL's request.]

;;; /\/: Faster version

(define-plan-evaluator effectiveness ()
  (when (null (situation-arg *coa* :time-limit)) 	;/\/
    (record-eval :effectiveness 100)
    (return-from effectiveness-plan-evaluator))
  (let* ((time-limit (situation-arg *coa* :time-limit)) ; hours
	 (deadline (* time-limit 60 60)) 		; seconds
	 (slope (/ -100d0 deadline))
	 (action-count 0)
	 (sum 0d0))
    (declare (double-float slope sum)
	     (fixnum action-count))
    (dolist (node (list-nodes))
      (when (and (eq (n-type node) 'action)
		 #+:undef
		 (not (n-duration-looks-zero-p node)))
	(let* ((eft (tpoint-min (n-end-tpoint node)))
	       (f (number->double (min eft deadline))))
	  (declare (double-float f))
          (setq action-count (fix1+ action-count)
		sum (df+ sum (df+ (df* slope f) 100d0))))))
    (record-eval :effectiveness
		 (round sum action-count))))

(defun n-duration-looks-zero-p (node)
  (let ((est (tpoint-min (n-begin-tpoint node)))
	(eft (tpoint-min (n-end-tpoint node))))
    (= est eft)))

#|
(define-plan-evaluator effectiveness ()
  (let ((action-count 0)
	(effectiveness-sum 0d0))
    (walk-nodes
      #'(lambda (node)
	  (when (eq (n-type node) 'action)
	    (incf action-count)
	    (incf effectiveness-sum
		  (action-effectiveness node)))))
    (record-eval :effectiveness
		 (round effectiveness-sum action-count))))

(defun action-effectiveness (node)
  ;; An action that can finish at 0 gets 100%, one >= the deadline gets 0%.
  (let ((eft (tpoint-min (n-end-tpoint node))))
    (+ (* (/ -100d0 *effectiveness-deadline*)
	  (min eft *effectiveness-deadline*))
       100d0)))
|#

#+:undef
(define-plan-evaluator effectiveness ()
  (record-eval :effectiveness (random 101)))

(defun make-effectiveness-eval-result (value)
  (make-eval-result
    :short-description (format nil "~D%" value)
    :issues
      (cond ((< value 50)
	     (list
	       (make-plan-issue
		 :contents
		   '(modify requirements or resources
		     because effectiveness is less than "50%"))))
	    ((< value 75)
	     (list
	       (make-plan-issue
		 :contents
		   '(note "effectiveness less than 75%")))))))


;;; Plan levels

(define-plan-evaluator plan-levels ()
  (let* ((expansion-tree (get-expansion-tree))
	 (waiting-expansions (get-waiting-expansions))
	 (depth-list (expansion-depth-list expansion-tree waiting-expansions)))
    (record-eval :plan-levels
      `(:expansion-tree ,expansion-tree
	:depth-list ,depth-list
	:waiting-expansions ,waiting-expansions))))

(defun make-plan-level-eval-result (value)
  (let ((depth-list (getf value :depth-list))
	(waiting-expansions (getf value :waiting-expansions)))
    (make-eval-result
      :short-description (expansion-depth-string depth-list)
      :report-fn 'explain-plan-level-eval
      :report-args value
      :issues
        ;; /\/: In other domains, there might also be non-expansions
        ;; waiting for authority.
        (when waiting-expansions
          (list
	    (make-plan-issue
	      :contents '(note "further expansion is possible")))))))

(defun expansion-depth-string (depth-list)
  (format nil "~{~D~^, ~}"
    (remove-duplicates
      (mapcar #'third depth-list)
      :from-end t)))

(defun explain-plan-level-eval
    (&key expansion-tree depth-list waiting-expansions)
  (html-button-bar
    `(("Action level table" ,(path-action-url :action-levels))
      ("TF file"            ,(demo-tf-url *demo*))))
  ;; Find expandable top-level nodes
  (let ((expandable-nodes (mapcar #'first waiting-expansions)))
    ;; Level table
    (html-tag-line "h2" "Current levels for top-level actions")
    (when waiting-expansions
      (html-block "p"
        "The level is the maximum action level of subnodes that are"
	"fully expanded."))
    (html-aiai-table ()
      (html-item "tr align=center"
        (html-item "th" "Node")
	(html-item "th" "Action")
	(html-item "th" "Level")
	(html-item "th" "Complete?"))
      (loop for (tag pattern depth) in depth-list do
        (html-item "tr"
          (html-item "td align=center"
	    (html-format "~A" (sequence-after "NODE-" (string tag))))
	  (html-item "td align=left"
	    ;; Was: (html-format "~A" (action-description-string pattern))
	    (html-xp-format "<tt>{~{~:W~^ ~}}</tt>" pattern))
	  (html-item "td align=center"
	    (html-format "~A" depth))
	  (html-item "td align=center"
	    (html-format "~:[yes~;no~]"
	      (member tag expandable-nodes))))))
    ;; Expansion tree
    (html-tag-line "h2" "Expansion tree")
    (when waiting-expansions
      (if (eq (coa-plan-status *coa*) :question)
	  (html-block "p"
	    "Notes that are not fully expanded are written in italics.")
	(html-block "p"
          "Nodes that would be expanded if the planner were given greater"
	  "authority are written in italics.")))
    (html-pre-box
      (print-expansion-tree expansion-tree waiting-expansions *html-out*))))

(defun print-expansion-tree (expansion-tree waiting-expansions
			     &optional (stream t))
  ;; /\/: ~& doesn't seem to get XP to go to a fresh line.
  (let ((xp:*print-right-margin* nil))
    (label walk ((tree expansion-tree)
		 (tab 0))
      (dolist (branch tree)
	(destructuring-bind (tag pattern subtree) branch
	  (xp-format stream
		     (if (waiting-expansion-p tag waiting-expansions)
			 "~vT<i>~A {~{~:W~^ ~}}</i>~%"
		         "~vT~A {~{~:W~^ ~}}~%")
		     tab tag pattern)
	  (when subtree
	    (walk subtree
		  (+ tab 3))))))))

;;; The expansion-depth list has an entry for each top-level node.
;;; An entry has the form (node-tag pattern depth), where depth is
;;; the greatest action level in the expansion tree below node-tag.

;;; A node that has an :expand on the agenda is counted at its
;;; level - 1, so that the level indicates the level of full expansion.

;;; Presumably, if there's a subtree, there's no waiting expansion.

(defun expansion-depth-list (expansion-tree waiting-expansions)
  (mapcar #'(lambda (branch)
	      (destructuring-bind (tag pattern subtree) branch
		(list tag
		      pattern
		      (if subtree
			  (tree-max-action-depth subtree waiting-expansions)
			(if (waiting-expansion-p tag waiting-expansions)
			    0
			  1)))))
	  expansion-tree))

(defun tree-max-action-depth (expansion-tree waiting-expansions)
  (let ((depth 1))
    (label walk ((tree expansion-tree))
      (dolist (branch tree)
	(destructuring-bind (tag pattern subtree) branch
	  (declare (ignore pattern))
	  (if (null subtree)
	      (let ((level ; (action-level (car pattern))
		             (n-level (get-node tag))))
		(setq depth
		      (max depth
			   (if (waiting-expansion-p tag waiting-expansions)
			       (1- level)
			     level))))
	    (walk subtree)))))
    depth))

;;; The expansion tree shows the action nesting present in the plan.
;;; The tree is a list of entries of the form (node-tag pattern subtree).

(defun get-expansion-tree ()
  ;; Get a table: node tag -> list of child node structs.
  ;; /\/: n-child-tags exists but the slot's never filled in.
  (let ((node->children-table
	 (transpose-graph (list-nodes) 
			  #'(lambda (n) (list (n-parent-tag n))))))
    ;; Adjust the table to list the children in order of increasing
    ;; node number.
    (maphash #'(lambda (tag children)
		 (setf (gethash tag node->children-table)
		       (sort children #'node-lessp)))
	     node->children-table)
    ;; Build the expansion tree.
    (flet ((children (tag) (gethash tag node->children-table)))
      (label tree ((roots (nice-children 'nil #'children)))
	(if (null roots)
	    '()
	  ;; for each root collect (root-tag root-pattern subtree)
	  (mapcar #'(lambda (root)
		      (list (n-tag root)
			    (oplan-psv:psv-actorise-pattern (n-pattern root))
			    (tree (nice-children (n-tag root)
						 #'children))))
		  roots))))))

(defun nice-children (tag children-fn)
  ;; Reach through non-actions.
  ;; children-fn: tag -> list_of nodes
  (let ((children (funcall children-fn tag)))
    (sort (reach-through-nonactions children children-fn)
	  #'node-lessp)))

(defun reach-through-nonactions (nodes children-fn)
  (mapcan #'(lambda (n)
	      (if (eq (n-type n) 'action)
		  (list n)
		(nice-children (n-tag n) children-fn)))
	  nodes))

#+:undef
(defun get-expansion-tree ()
  ;; Get a table: node tag -> list of child node structs.
  ;; /\/: n-child-tags exists but the slot's never filled in.
  (let ((node->children-table
	 (transpose-graph (list-nodes) 
			  #'(lambda (n) (list (n-parent-tag n))))))
    ;; Adjust the table to exclude non-action nodes and to list the
    ;; children in order of increasing node number.
    (maphash #'(lambda (tag children)
		 (setf (gethash tag node->children-table)
		       (sort (remove 'action children
				     :test-not #'eq
				     :key #'n-type)
			     #'node-lessp)))
	     node->children-table)
    ;; Build the expansion tree.
    (flet ((children (tag) (gethash tag node->children-table)))
      (label tree ((roots (children 'nil)))
	(if (null roots)
	    '()
	  ;; for each root collect (root-tag root-pattern subtree)
	  (mapcar #'(lambda (root)
		      (list (n-tag root)
			    (oplan-psv:psv-actorise-pattern (n-pattern root))
			    (tree (children (n-tag root)))))
		  roots))))))


;;; List of (top-node-tag node-tag pattern) for nodes that have an 
;;; :expand on the waiting agenda /\/: or the untriggered agenda.
;;; The top-node-tag indicates the top-level node reached by following
;;; parent links.  This makes it easy to determine which top-level
;;; nodes are not yet fully expanded.

;;; Changed to check the triggered agenda too, becuase this is no longer
;;; called only when planning is complete-within-the-authority-granted.
;;; ALso had to take into account the possibility that we're in the
;;; midst of a TA-question.  [JD 14 May 99]

(defun get-waiting-expansions ()
  (let ((expansions '()))
    (dolist (issue (get-all-agenda-entries-for-waiting-expansions))
      ; (web-note "~&Issue: ~S~%" issue)
      (when (eq (ag-type issue) :expand)
        (let* ((node (second (ag-body issue)))
	       (pattern (oplan-psv:psv-actorise-pattern
			  (third (ag-body issue))))
	       (top-node (get-root-node node)))
	  (push (list top-node node pattern)
		expansions))))
    (sort expansions
	  #'(lambda (a b)
	      (node-lessp (get-node (first a))
			  (get-node (first b)))))))

(defun get-all-agenda-entries-for-waiting-expansions ()
  (let ((q (ta-question)))
    (if q
	(append (ta-question-triggered-agenda q)
		(ta-question-untriggered-agenda q)
		(ta-question-waiting-agenda q))
      (append (atm-get-waiting-agenda) 
	      (atm-get-untriggered-agenda)
	      (atm-get-triggered-agenda)))))

(defun waiting-expansion-p (node-tag waiting-expansions)
  (find node-tag waiting-expansions :key #'second))

(defun get-root-node (tag)
  (let ((at tag))
    (loop
      (let ((parent (n-parent-tag (get-node at))))
	(if parent
	    (setq at parent)
	  (return at))))))


;;; Timeline

(define-plan-evaluator timeline ()
  (record-eval :timeline
    (mapcar (partial1 #'mapcar #'make-timeline-ne-description)
	    (node-ends-in-execution-stages))))

#+:undef
(define-plan-evaluator timeline ()
  (record-eval :timeline
    (mapcar #'make-timeline-ne-description
	    (node-ends-in-execution-order))))

(defun make-timeline-ne-description (ne)
  `(:node    ,(ne-node-tag ne)
    :end     ,(ne-end ne)
    :type    ,(n-type (ne-node ne))
    :pattern ,(oplan-psv:psv-actorise-pattern (n-pattern (ne-node ne)))
    :level   ,(n-level (ne-node ne))
    :est     ,(tpoint-min (ne-time-point ne))
    :ldist   ,(ne-link-distance ne)))


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
