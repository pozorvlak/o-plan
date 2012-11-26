;;;; File: KS-BIND.lsp
;;; Contains: Bind up variables.
;;; Author: Jeff Dalton and Richard Kirby (rbk)
;;; Created: Mon Jul 22 16:25:23 1991
;;; Updated: Sun May 30 22:45:08 1999 by Jeff Dalton
;;; Copyright: (c) 1992, 1994 AIAI, University of Edinburgh

(in-package :oplan-knowledge-source)

;;; KS-BIND can get as (ag-body ag):
;;;
;;; :BIND psv-name
;;;

(defun KS-BIND (ag)
  (STAGE-MANAGER
      (bindings			;possible values in an alternative
       force-auto)		;true in an alt that shouldn't ask the user
      (var-name)
      ag

   ;; There's only one stage, but we still save some state for
   ;; use in alternatives.

   (setq var-name (cadr (ag-body ag)))

   ;; If bindings is set then this is from an alternative invocation, so
   ;; no need to get the possibles.
   (cond (bindings
	  ;; Bring the PSV struct up to date.
	  ;; /\/: Are we always more up-to-date than the struct?
	  (unless (db-request :set-var-possibles var-name bindings)
	    (post-agenda 
	     `(:POISON-STATE :COULD-NOT-RESTRICT ,var-name :TO ,bindings))
	    (return-from ks-bind)))
	 (t
	  ;; Ask the DM for possible bindings.
	  (dev-debug :detail "Asking DM for bindings for ~S" var-name)
	  (setq bindings (get-bindings var-name))))

   ;; Now bindings is either :already-bound or a list of possible vaues.
   (cond ((eql bindings :already-bound)
	  (let ((value (db-request :get-var-value var-name)))
	    (dev-debug :trace "~S is already bound to ~S" var-name value)
	    (whats-going-on "Variable ~S is already bound." var-name value)))
	 ((null bindings)
	  (whats-going-on "No possible bindings for ~S, so POISON PLAN."
			  var-name)
	  (post-agenda `(:POISON-STATE :COULD-NOT-FIND-BINDING ,var-name)))
	 ((not (consp bindings))
	  (error "Strange value for bindings ~S." bindings))
	 ((length=1 bindings)		; (= (length bindings) 1)
	  (set-binding-else-poison var-name (car bindings)))
	 ;; Now know (> (length bindings) 1)
	 ((or force-auto (eq *psv-binding-mode* :auto))
	  (auto-bind ag var-name bindings))
	 (t
	  ;; Ask the user, either via KS-USER or by sending a TA :question.
	  ;; Set the possibles cache so it looks right and so
	  ;; the user sees only the values that are left.
	  ;; Note that we're going to set the cache to contain
	  ;; more than one value, because otherwise we wouldn't
	  ;; need to ask the user.
	  ;; /\/: If this fails, someone had better investigate.
	  ;; /\/: It wouldn't be necessary if the user never saw
	  ;;      any of the :BINDs that came from alternatives.
	  ;;      That is, if in alts we passed on force-auto.
	  (assert (db-request :set-var-possibles var-name bindings))
	  (if (and (get-parameter :interactive)
		   (not (get-parameter :test-ta-questions)))
	      ;; Interactive -- post a :USER entry.			      
	      ;; /\/: Should we use the agent agenda?
	      ;;      Or can :USER be on either?
	      (post-agenda
	       `(:USER :SYSTEM-REQUEST
		       :BIND ,var-name))
	    ;; Not interactive -- ask the TA by sending a :question message.
	    (ipc-send :am :question :psv-binding
		      ag
		      (encode-psv-binding-question var-name bindings)))))))

(defun auto-bind (ag var-name bindings)
  (let ((choice (car bindings))
	(other-values (cdr bindings)))

    ;; When we first enter this function, bindings contains more than
    ;; one value, but later on it may not.
    (when (null other-values)
      (set-binding-else-poison var-name choice)
      (return-from auto-bind))

    (dev-level-case
      (:trace (dev-debug "Binding ~S to ~S" var-name choice))
      (:detail
       (dev-debug "Automatically using first choice.~%~
                   Binding ~S to ~S~%~
                   ~S are the alternatives."
		  var-name choice other-values)))

    (whats-going-on "More than one applicable binding for ~A~%~
                     so post the following as alternatives.~%~A"
		    var-name other-values)

    ;; Bind var-name to choice and if that fails try the next value.
    ;; First we post an alternative for the values other than choice.
    ;; Then, if we cannot bind to choice, we get rid of that alternative
    ;; and try the next value.
    (let ((pre-change-context (db-request :get-context)))
      (am-request :begin-alt-transaction)
      ;; Post an alt.  This will push a context.
      (with-ag-copy (ag ag)
	(setf (ag-info ag)
	      (list other-values		;values to consider
		    nil))			;not force-auto
	(setf (ag-stage ag) 0)
	(setf (ag-branch-1 ag) (length other-values))
	(post-alternatives ag))
      (if (set-binding var-name choice)
	  ;; Ok, we're outa here, leaving the alt in place.
	  (am-request :commit-alt-transaction)
	(progn
	  ;; The chosen value loses.  Back out and try the next
	  (am-request :abort-alt-transaction)
	  (db-request :pop-context)
	  (assert (eql (db-request :get-context) pre-change-context))
	  (assert (not (db-request :context-was-poisoned)))
	  (dev-warn "We finally win in KS-BIND! -- ~S can't be ~S"
		    var-name choice)
	  (auto-bind ag var-name other-values))))))

(defun set-binding-else-poison (var-name value)
  (dev-debug :trace "Binding ~A to ~A" var-name value)
  (whats-going-on "Binding ~S to ~S" var-name value)
  (unless (set-binding var-name value)
    (dev-warn "Failed to set ~S to ~S." var-name value)
    (post-agenda `(:POISON-STATE :COULD-NOT-FIND-BINDING
		   ,var-name :TO ,value))))


;;; Support for the :psv-binding question

;;; We're going to send
;;;
;;;   (:QUESTION :PSV-BINDING (:BIND psv-name) descriptions)
;;;
;;; where psv-name is the name of the PSV this instance of ks-bind is
;;; supposed to bind, and descriptions are plists of the same kind used
;;; for ks-user.  Here we provide the descriptions part in the form 
;;; (:QUESTION description...), this :QUESTION being a convention we
;;; happen to use internally.  It's removed before anything's sent
;;; out to the TA.

(defun encode-psv-binding-question (var-name bindings)
  (declare (ignore var-name bindings))
  (cons :question			;convention, removed later
	;; Get var descriptions as in ks-user
	(remove-if #'var-has-value-p
		   (db-request :get-psvs-for-user))))

;;; What we expect back is
;;;
;;;   (:ANSWER :PSV-BINDING var-descriptions)
;;;
;;; Each var description is a plist, so the answer will look something
;;; like this:
;;;
;;;   (:ANSWER :PSV-BINDING ((:TAGS psv-names :VALUES possible-values)...))
;;;
;;; from which ((:TAGS psv-names :VALUES possible-values)...) is given to
;;; ks-bind-accept-answer as the data.  
;;;
;;; Each "psv-names" is a list of psv tags which all represent the same 
;;; (unified) variable.  Unification may have resulted in variables
;;; (psv-body structs) with more than one tag; hence this complication.
;;; It is not necessary to list all of a variable's tags; any one will do.
;;;
;;; The corresponding "possible-values" is a list of the values that
;;; should be tried for that variable, in the order in which they should
;;; be tried.  The lists may be a subset of the values currently thought
;;; to be possible for that variable, but it's an error if there are any
;;; extra values.  In ks-user, any valid values not in the list would be
;;; tried in a separate alternative.  Here we just pretend that they were
;;; included, but at the end of the list.  In short, the "possible-values"
;;; are the preferred values, in decreasing order of preference.
;;; /\/: For now, anyway.
;;;
;;; Note that here we are running as ks-question and the original
;;; :BIND agenda entry, passed to ks-bind-accept-answer as its "ae"
;;; parameter, has been put on the untriggered agenda.  (Of course,
;;; it should soon be triggered.)  We have to set things up so that
;;; it does not ask the :question again.  

(defun ks-bind-accept-answer (ae question-kwd data)
  (ecase question-kwd
    (:psv-binding
     (let* ((psv-name (second (ag-body ae)))
	    (props (find-var-with-tag psv-name data)))

       ;; psv-name names the PSV that our ks-bind was supposed to bind.
       ;; props is that var's description plist from the :answer data.
       #+:undef
       (unless props
	 (error ":psv-binding :answer did not mention ~S." psv-name))

       ;; Adjust the possibles-caches of all the listed variables.
       (dolist (v data)
	 (let* ((tag (first (getf v :tags)))
		(preferred-values (getf v :values))
		(possible-values (get-bindings tag))
		(preferred-order 
		 (preferred-order preferred-values possible-values)))
	   (unless (db-request :set-var-possibles tag preferred-order)
	     (post-agenda
	       `(:POISON-STATE :COULD-NOT-RESTRICT ,psv-name
			       :TO ,preferred-order))
	     (throw :ks-exit nil))))

       ;; Give our :bind agenda entry a new list of possible values
       ;; and tell it not to ask the question again.  Note that we've
       ;; already adjusted the :bind's psv's possibles-cache, above,
       ;; and hence would have detected various problems that we can
       ;; now assume do not obtain.  We can also assume that that psv's
       ;; possibles-cache already has the values in the preferred order.
       (let ((possible-values (get-bindings psv-name)))
	 (setf (ag-info ae)
	       (list ;; The possible values, in the preferred order.
		     possible-values
		     ;; Force automatic binding
		     t)))))))

(defun preferred-order (preferred-values all-values)
  ;; Move the preferred-values to the front of the list.
  (let ((left-alone (stable-set-difference all-values preferred-values))
	(invalid (stable-set-difference preferred-values all-values)))
    (when invalid
      (error "Invalid values ~S cannot be preferred." invalid))
    (append preferred-values left-alone)))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
;;;  JWD  10 April 94	Added the code that uses alt-transactions to
;;;			let us try values until one works or we run out.
;;;
;;;  JWD   May 99	Implemented the :psv-binding TA question.
;;;
;;;  JWD   May 99	Changed to :set-var-possibles at the start of
;;;			ks-bind if we already have a list of bindings.
;;;
