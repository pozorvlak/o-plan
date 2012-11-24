;;;; File: KS-USER.lsp
;;; Contains: A basic KS-USER.
;;; Author: Jeff Dalton
;;; Created: Wed Jun  3 07:36:59 1992
;;; Updated: Sun May 23 17:32:46 1999 by Jeff Dalton
;;; Copyright: (c) 1994, AIAI, University of Edinburgh

(in-package :oplan-knowledge-source)

;;; KS-USER can get as (ag-body event):
;;;
;;; :USER :USER-REQUEST
;;;
;;;    The user has decided to intervene.  Present a menu of options; etc.
;;;
;;; :USER :SET-POSSIBLES psv-name list-of-values
;;;
;;;    A request to reduce the set of values in a PSV's possibles-cache
;;;    to the ones specified.  It appears only in alternatives produced
;;;    by KS-USER when it takes values out of a possibles cache at the
;;;    user's request.  The values excluded by the user get their chance
;;;    in the alternative.
;;;
;;;    It may seem that KS-BIND ought to do this, but it's really a
;;;    distinct operation.  KS-BIND has to bind a variable, not just
;;;    restrict it.  A :BIND in the agenda means a variable must be
;;;    bound.  A :USER :SET-POSSIBLES merely keeps the search space
;;;    complete by giving some less-favored values a chance.
;;;
;;;    The other side of this is that when a KS-USER is acting for a
;;;    KS-BIND (via :ASK binding mode) we do post a :BIND as the
;;;    alternative.
;;;
;;; :USER :SYSTEM-REQUEST :BIND variable-name
;;;
;;;    KS-BIND wants the user to bind a variable.
;;;
;;; Note that we don't keep any interesting state in the stage or info
;;; fields of KS-USER agenda entries.  Instead, we rely entirely on the
;;; arguments (in the body).  So the code that processes an agenda entry
;;; that might be placed back on the agenda has to do so in a way that
;;; will also work the 2nd time around.
;;;

(defvar *ks-user-io* nil)

(defvar *var-name-to-bind* nil)		;only if request from KS-BIND

(defun KS-USER (event)
  (let* ((*var-name-to-bind* nil)
	 (body (ag-body event))
	 (args (cddr body)))
    (declare (ignore args))
    (assert (eq (first body) :USER))
    (ecase (second body)
      (:USER-REQUEST    (user-intervention event))
      (:SET-POSSIBLES   (user-set-possibles event))
      (:SYSTEM-REQUEST  (system-request-to-user event)))))


;;;; User intervention

;;; N.B. Get-current-variables must be called after any operation that
;;; might change any properties of a PSV.

;;; The :continue exit from user-intervention-loop allows us to
;;; reschedule ks-user, then exit, so that other processes have a
;;; chance to run.  It used to be used so that the pw-viewer process
;;; could handle a plan or world view when the user asked for one,
;;; but the pw-viewer can now just be called as a subroutine.

(defvar *current-variables* nil)	;descriptions of all PSVs
(defvar *open-variables* nil)		;descriptions of PSVs w/ value :undef

(defun user-intervention (event)
  (let (*current-variables*
	*open-variables*)
    (get-current-variables)
    (ecase (user-intervention-loop)
      (:done)
      (:continue
       (incf (ag-stage event))		;/\/ not really needed
       (ipc-send :AM :EVENT event)))))

(defun get-current-variables ()
  (setq *current-variables* (db-request :get-psvs-for-user)
	*open-variables*    (remove-if #'var-has-value-p *current-variables*)))

(defun user-intervention-loop () ; -> :done or :continue
  (loop
    (ecase (ks-user-menu-choice)
      (:plan-view
       (ks-user-plan-view))
      (:world-view
       (ks-user-world-view))
      (:bind
       (ks-user-bind-variables))
      (:modes
       (ks-user-set-modes))
      (:break
       (ks-user-break))
      (:poison
       (when (ks-user-poison)
	 (return :done)))
      (:quit
       (when (ks-user-quit)
	 (return :done))))))

(defun ks-user-menu-choice ()
  (menu-request
    `("-heading"
      ,(if *var-name-to-bind*
	   (format nil "User intervention to bind ~A" *var-name-to-bind*)
	   "User intervention")
      "Plan View=:plan-view"
      "World View=:world-view"
      ,@(when *current-variables*
	  '("Bind Variables=:bind"))
      "Set modes=:modes"
      "Break in=:break"
      "Poison=:poison"
      "-line"
      "QUIT=:quit")))			;/\/ is quit the right name?  "Ok"?


;;; Start with some short options...


;;; Break-in

(defun ks-user-break ()
  (ensure-ks-user-window)
  (let ((*standard-input* *ks-user-io*)
	(*standard-output* *ks-user-io*)
	(*debug-io* *ks-user-io*)
	(*query-io* *ks-user-io*)
	(*trace-output* *ks-user-io*)
	(*error-output* *ks-user-io*))
    (break "User Intervention")))


;;; Poison

(defun ks-user-poison () ; -> true to poison and quit, else false
  (cond ((db-request :REQUEST-ALTS-AGENDA)
	 (ecase (ask-user-if "Do you really want to poison?")
	   (:yes (post-agenda '(:POISON-STATE :USER-DECISION))
		 t)
	   (:no nil)))
	(t
	 (notify-user "There is no alternative")
	 nil)))


;;; Quit

(defun ks-user-quit () ; -> true to quit, false to not
  (if *var-name-to-bind*
      (quit-request-to-bind-p)
    t))


;;; Plan view

(defun ks-user-plan-view ()
  (ensure-ks-user-window)		;so we control how it's set up
  (let ((*pw-viewing-window* :ks-user-io))
    (pw-handle-view :plan (pw-get-plan-for-viewing))))


;;; World view

(defun ks-user-world-view ()
  (ensure-ks-user-window)		;so we control how it's set up
  (let ((*pw-viewing-window* :ks-user-io)
	(node-id (ask-user-for-world-view-node)))
    (pw-handle-view :world node-id (pw-get-world-for-viewing node-id))))


(defun ask-user-for-world-view-node ()
  (ask-ks-user-user "World view at end_of node-"))


;;; Set modes

(defun ks-user-set-modes ()
  (loop
    (ecase (set-modes-menu-choice)
      (:bind-mode
       (flip-mode '*psv-binding-mode*))
      (:schema-mode
       (flip-mode '*schema-selection-mode*))
      (:poison-mode
       (flip-mode '*poison-handler-mode*))
      (:ok
       (return)))))

(defparameter *modes*
  '((:bind-mode   "binding"          *psv-binding-mode*)
    (:schema-mode "schema selection" *schema-selection-mode*)
    (:poison-mode "poison handler"   *poison-handler-mode*)))

(defun set-modes-menu-choice ()
  (menu-request
    `("-heading" "Set Modes"
      ,@(mode-menu-lines *modes*)
      "-line"
      "OK=:ok")))

(defun mode-menu-lines (modes)
  (let ((name-width (max-value #'length (mapcar #'second modes)))
	(value-width #.(max-value #'length '("auto" "ask"))))
    (mapcar
      #'(lambda (m)
	  (let* ((key (first m))
		 (name (second m))
		 (value (symbol-value (third m))))
	    (format nil "~@(~v@A mode is ~vA - set to ~vA=~S~)"
		    name-width name
		    value-width value
		    value-width (opposite-mode value)
		    key)))
      modes)))


;;; Bind variables

;;; Give the user a chance to bind or restrict some variables.

;;; We need to remember the pre-change context for an abort.  We can't
;;; just pop the context to abort, because each change pushes a new
;;; context (and so we might have to go back several steps).

(defvar *pre-change-context* nil)	;number of context before changes
(defvar *changes* nil)			;descriptions of any changes made

(defun ks-user-bind-variables ()
  (let ((*pre-change-context* nil)
	(*changes* '()))
    (assert *current-variables*)	;used to (notify-user "No Variables")
    (ks-user-bind-variables-loop)))

(defun ks-user-bind-variables-loop ()
  (loop
    (catch :bind-option-exit		;throw here to exit an option
      (ecase (bind-menu-choice)
	(:describe-open-vars
	 (ks-user-describe-variables "Open Variables" *open-variables*))
	(:describe-all-vars
	 (ks-user-describe-variables "All Variables" *current-variables*))
	(:describe-changes
	 (assert *changes*)
	 (ks-user-describe-changes))
	(:select
	 (unless (ks-user-restrict-variable)
	   (assert *changes*)
	   (ks-user-abort-var-changes)
	   (return)))
	(:commit
	 (assert *changes*)
	 (ks-user-commit-var-changes)
	 (return))
	(:abort
	 (assert *changes*)
	 (ks-user-abort-var-changes)
	 (return))
	(:quit
	 (assert (null *changes*))
	 (return))))))

(defun bind-menu-choice ()
  (menu-request
    `("-heading" "Binding Options"
      ,@(when *open-variables*
	  '("Describe Open Variables=:describe-open-vars"))
      ,@(when (some #'var-has-value-p *current-variables*)
	  '("Describe All Variables=:describe-all-vars"))
      ,@(when *changes*
	  '("Describe Changes Made=:describe-changes"))
      ,@(when *open-variables*
	  '("Select a Variable to Bind=:select"))
      "-line"
      ,@(if *changes*
	    '("COMMIT to changes made=:commit"
	      "ABORT and undo changes made=:abort")
	  '("QUIT=:quit")))))

;;; Describe variables

(defun ks-user-describe-variables (heading vars)
  (ensure-ks-user-window)
  (format *ks-user-io* "~&~%~A:~%" heading)
  (dolist (v vars)
    (ks-user-describe-var v *ks-user-io*)))

(defun ks-user-describe-var (v &optional (stream *standard-output*))
  (let ((*print-case* :downcase)
	(*print-pretty* t))
    (xp-format stream
        "~%~@<~{~W~^, ~}: ~7I~:_~
              Names = ~W, ~:_~
              Type = ~W, ~:_~
              Value = ~W, ~:_~
              Sources = ~W, ~:_~
              Restrictions = ~W, ~:_~
              Not sames = ~W, ~:_~
              Possibles cache = ~W;~
              ~:>~%"
	(getf v :tags)
	(remove-duplicates (mapcar #'first (getf v :original-names)))
	(getf v :type)
	(getf v :value)
	(getf v :sources)
	(getf v :restrictions)
	(getf v :not-sames)
	(getf v :possibles-cache))))

;;; Var change management

(defun ks-user-begin-var-change (description)
  (when (null *changes*)
    (assert (null *pre-change-context*))
    (setq *pre-change-context* (db-request :get-context))
    (am-request :begin-alt-transaction))
  (setq *changes* (nconc *changes* (list description))))

(defun ks-user-commit-var-changes ()
  (assert *changes*)
  (whats-going-on "User commits to:~%~{~3T~S~%~}" *changes*)
  (am-request :commit-alt-transaction)
  )

(defun ks-user-abort-var-changes ()
  (assert *changes*)
  (assert *pre-change-context*)
  (whats-going-on "User aborts:~%~{~3T~S~%~}" *changes*)
  (db-request :set-context *pre-change-context*)
  (am-request :abort-alt-transaction)
  (get-current-variables)
  )

(defun ks-user-describe-changes ()
  (assert *changes*)
  (ensure-ks-user-window)
  (let ((*standard-output* *ks-user-io*)
	(*print-case* :downcase)
	(*print-pretty* t))
    (format t "~&~%Changes made:~2%")
    (dolist (c *changes*)
      (xp-format t "~&~W~%" c))))

;;; Restrict / bind a variable

;;; The user can change the values in the possibles cache, but only
;;; to remove values or to reorder them.

;;; A failure automatically aborts any earlier changes that the user
;;; hasn't yet committed to and returns to the top-level KS-USER menu.

(defun ks-user-restrict-variable () ;-> true (success) or false (failure)
  (let* ((open-vars *open-variables*)
	 (chosen-tag (var-menu-choice open-vars)))
    (if (eq chosen-tag :quit)
	t				;success, of a sort
      (let ((chosen-var (find-var-with-tag chosen-tag open-vars)))
	(assert (not (null chosen-var)))
	(ks-user-describe-variables
	   "Selected variable"
	   (list chosen-var))
	(let* ((new-possibles (read-possibe-values chosen-var))
	       (excluded (stable-set-difference
			   (getf chosen-var :possibles-cache)
			   new-possibles)))
	  ;; We now have a variable and a valid set of values for its
	  ;; possibles cache.  We're therefore going to try to change
	  ;; the variable,
	  (ks-user-begin-var-change
	     `(:restrict ,chosen-tag :to ,new-possibles
			             :excluding ,excluded))
	  ;; If there are any excluded values, post an alternative that
	  ;; will given them their chance.  The alternative will use the
	  ;; current context as its context and will push a new context
	  ;; in which we'll make the change.
	  (when excluded
	    (post-alternatives
	      (if (eq *var-name-to-bind* chosen-tag)
		  ;; Make sure the variable gets bound, not just restricted,
		  ;; because we're replacing a KS-BIND for this variable.
		  (make-agenda-entry
		    :body `(:BIND ,chosen-tag)
		    :trigger t
		    :info (list excluded nil))
		  ;; Can just restrict.  The KS-BIND for this variable is
		  ;; still out there somewhere and will make sure it's bound.
		  (make-agenda-entry
		    :body `(:USER :SET-POSSIBLES ,chosen-tag ,excluded)
		    :trigger t))))
	  ;; Change the variable.  If the change is rejected, indicate
	  ;; this by returning false; else return true.
	  (cond ((db-request :set-var-possibles chosen-tag new-possibles)
		 ;; It worked.
		 (get-current-variables)
		 (ks-user-message "~&~%Done.~%")
		 t)
		(t
		 ;; Didn't work.
		 ;; The abort procedure will call get-current-variables.
		 (ks-user-message "~&~%Failed.~%")
		 nil)))))))

(defun var-menu-choice (vars)
  (menu-request
    `("-heading" "Pick a variable"
      ,@(mapcar #'var-menu-line vars)
      "-line"
      "QUIT=:quit")))

(defun var-menu-line (v)
  (destructuring-bind (tf-var schema) (first (getf v :original-names))
    (format nil "~A (~A from ~A)=~A"
	    (first (getf v :tags))
	    tf-var
	    schema
	    (first (getf v :tags)))))

(defun read-possibe-values (var)
  (let ((current-possibles (getf var :possibles-cache)))
    (loop
      (let ((values-line (ask-ks-user-user "Values: ")))
	(when (null values-line)
	  (ks-user-message "~&~%OK, we'll leave the variable alone.~%")
	  (throw :bind-option-exit nil))
	(let* ((values (parse-values-line values-line))
	       (errors (set-difference values current-possibles)))
	  (when (null errors)
	    (return values))
	  (ks-user-message "~%Invalid values: ~S~%" errors))))))

(defun parse-values-line (line)
  ;; Line should contain a sequence of values separated by spaces.
  ;; At present, there's no error recovery if a Lisp syntax error
  ;; occurs.  /\/
  (stream->list (make-string-input-stream line)))


;;;; :SET-POSSIBLES from an alternative

;;; At present, we just tell the user we're doing this rather than enter
;;; a proper KS-USER interaction.

;;; /\/: If we were to enter an interaction *after* changing the variable,
;;; we'd need some state in the info field to keep us from changing it again,
;;; and the comment about state near the start would have to change.

(defun user-set-possibles (event)
  (let ((psv-name (third (ag-body event)))
	(excluded (fourth (ag-body event))))
    (notify-user "Restricting ~A to ~A" psv-name excluded)
    (unless (db-request :set-var-possibles psv-name excluded)
      (post-agenda
        `(:POISON-STATE :COULD-NOT-RESTRICT ,psv-name :TO ,excluded)))))
	

;;;; System requests

;;; (ag-body event) should be :USER :SYSTEM-REQUEST request, where
;;; request is one of:
;;;
;;;   :BIND psv-name
;;;

(defun system-request-to-user (event)
  (let ((request (cddr (ag-body event))))
    (ecase (car request)
      (:bind (request-to-bind event (cadr request))))))

;;; :BIND psv-name

(defun request-to-bind (event psv-name)
  (setq *var-name-to-bind* psv-name)
  (user-intervention event))

(defun quit-request-to-bind-p () ; -> true to quit, false to not
  (if (var-has-value-p (find-var-with-tag *var-name-to-bind*))
      ;; The PSV has been given a value, so it's ok to quit as instructed.
      t
    (ecase (ask-user-if "Quit without binding ~A?" *var-name-to-bind*)
      (:NO
       ;; The user now realizes a mistake has been made -- so don't quit.
       nil)
      (:YES
       ;; The user doesn't want to bind the PSV after all, so post
       ;; a :BIND what will.
       (post-agenda `(:BIND ,*var-name-to-bind*)
	   :trigger t
	   :stage   0
	   :info    (list nil		;ask DM for possibles
			  t))		;force auto mode
       ;; And quit KS-USER.
       t))))


;;;; Schema choice

;;; /\/: At present, this comes direct from sort-schema-alternatives
;;; which is called by KS-EXPAND and KS-ACHIEVE.  Eventually, EXPAND
;;; and ACHIEVE should put a :USER entry on the agenda, and then
;;; KS-USER should put back an EXPAND or ACHIEVE.  When that happens,
;;; we could, if we wanted, allow the user to get a plan or world
;;; view while deciding what schema to select.

(defun ks-user-schema-choice (schemas-and-bindings)
  (describe-schema-options schemas-and-bindings)
  (let ((best (schema-menu-choice schemas-and-bindings)))
    (cond ((string= best ":auto")
	   (flip-mode '*schema-selection-mode*)
	   schemas-and-bindings)
	  ((string= best ":none")
	   (ecase (ask-user-if "Are you sure you want to poison?")
	     (:yes
	      (post-agenda '(:POISON-STATE :USER-REJECTED-SCHEMAS))
	      ;; /\/: Eventually should let something higher-level decide
	      ;; /\/: Or at least use the condition system.
	      (throw :ks-exit nil))
	     (:no
	      (ks-user-schema-choice schemas-and-bindings))))
	  (t
	   (let ((best-schema-and-bindings
		  (find best
			schemas-and-bindings
			:key #'(lambda (s+b) (schema-name (car s+b)))
			:test #'string=)))
	     ;; Make sure nothing's gone wrong.  It shouldn't be possible
	     ;; for the find to fail if schema-menu-choice returned a
	     ;; valid value, but maybe it didn't return one.
	     (unless best-schema-and-bindings
	       (cerror "Use the schema listed first"
		       "Can't find ~S." best)
	       (setq best-schema-and-bindings (car schemas-and-bindings)))
	     ;; Provide some feedback.  Remember that the user may have
	     ;; moused the wrong item by accident.  This will at least
	     ;; make that clear.
	     (let ((name (schema-name (car best-schema-and-bindings))))
	       (ks-user-message "~&~%Ok, it's \"~A\"~%" name)
	       (whats-going-on "User picks schema ~S" name))
	     ;; Put the selected schema first, retaining the others
	     ;; in their original order.
	     (cons best-schema-and-bindings
		   (remove-1-eq
		    best-schema-and-bindings
		    schemas-and-bindings)))))))

(defun describe-schema-options (schemas-and-bindings)
  (ensure-ks-user-window)
  (let ((*standard-output* *ks-user-io*)
	(*print-case* :downcase)
	(*print-pretty* t))
    (format t "~&~%Possible schemas for AE-~S:~%  ~S:~%"
	    (ag-id *ag*)
	    (ag-body *ag*))
    (dolist (s+b schemas-and-bindings)
      (let ((schema (car s+b))
	    (bindings (cdr s+b)))
	(xp-format t "~%  ~A~%  expands ~W~%"
	    (schema-name schema)
	    (schema-expands schema))
	(when bindings
	  (xp-format t "  with ~<~I~:@{~W = ~W; ~:_~}~:>~%"
	      bindings))))))

(defun schema-menu-choice (schemas-and-bindings)
  (let ((schema-names
	 (mapcar #'(lambda (schema+bindings)
		     (schema-name (car schema+bindings)))
		 schemas-and-bindings)))
    (menu-request
      `("-heading" "Pick the best schema"
	,@schema-names
	"-line"
	"None of the above=:none"
	"-line"
	"Set mode back to auto=:auto")
      :read-function 'read-line)))


;;;; Utilities

(defun var-has-value-p (v)
  (not (eq (getf v :value) :undef)))

(defun find-var-with-tag (tag &optional (vars *current-variables*))
  (find tag vars :key #'(lambda (v) (getf v :tags))
	         :test #'member))

(defun ask-ks-user-user (question &rest format-args) ; -> string or nil
  (ensure-ks-user-window)
  (let ((io *ks-user-io*))
    (format io "~&~%~?" question format-args)
    (let ((answer (read-line io)))
      (if (string= answer "")
	  nil
	answer))))

(defun ks-user-message (format-string &rest format-args)
  (ensure-ks-user-window)
  (format *ks-user-io* "~?" format-string format-args))

(defun ensure-ks-user-window ()
  (unless *ks-user-io*
    (x-open-and-register-io-win :ks-user-io
       (or (is-get-window-args :ks-user-io)
	   '("-title" "Planner User (KS-USER)"
	     "-n" "User")))
    (setq *ks-user-io* (x-get-stream :ks-user-io))
    (clear-screen *ks-user-io*))
  ;; By now, we'd better have one.
  (assert (streamp *ks-user-io*))
  *ks-user-io*)
    
;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
