;;;; File: matrix-server-exec-support.lisp
;;; Contains: Support code for COA matrix Web demos
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: April 1998
;;; Updated: Mon Feb 14 02:58:49 2000 by Jeff Dalton
;;; Copyright: (c) 1999, AIAI, University of Edinburgh

(in-package :oplan)


;;; /\/: Should be consistent about whether the coa id is in the
;;; action URL or not.

;;; /\/: We turn schema selection mode to :ask when a repair is
;;; requested and ought to turn it back to whatever it was before
;;; at some point.

;;; /\/: Have a way to clear out all exec stuff.  Clear it when the TA
;;; selects a COA for execution.  Maybe clear it too when the user decides
;;; to quit execution.  Remember in a COA if it has been executed (this
;;; is one of the things that will be cleared) so we can say "Resume"
;;; rather than "Exec" after a repair.  (The has-been-executed flag
;;; will be copied when a COA sibling is made, and hence when "Replan"
;;; is used during plan repair.)  [We've kind of done this.]


;;; New COA history events:
;;;
;;;   (:EXEC)
;;;   (:REQUEST-REPAIR)
;;;   (:RESUME-EXEC)
;;;


;;;; The class

(defclass matrix-server-exec-mixin (matrix-server-demo)
  ((executing-coa
    :accessor demo-executing-coa
    :initform nil)))

(in-local-defun-class matrix-server-exec-mixin *demo*)


;;;; The exec-control struct

;;; One of these is installed in a coa struct when we begin to
;;; execute the coa's plan.

(defvar *exec-control* 'not-an-exec-control)

(defstruct (exec-control (:conc-name ec-))
  coa-id				;for debugging
  exec-plan
  (now           0)
  (always        (make-hash-table :test #'equal))
  (world-state   (make-hash-table :test #'equal))
  (step-table    (make-hash-table :test #'equal))
  (exec-report   (make-tconc))
  (broken-plan-p nil)
  )

(defun coa-exec-plan (coa)
  (ec-exec-plan (coa-exec-control coa)))

(defmacro with-coa-exec-state ((coa) &body forms)
  `(let* ((*exec-control* (coa-exec-control ,coa))
	  (*world-always-table* (ec-always *exec-control*))
	  (*world-state* (ec-world-state *exec-control*))
	  (*exec-step-table* (ec-step-table *exec-control*)))
     ,@forms))

(defun exec-report (type &rest args)
  (tconc (ec-exec-report *exec-control*) 
	 (list* type (ec-now *exec-control*) args)))

(defun coa-partially-executed-p (coa)
  ;; We need a test that tells us whether we're executing for the
  ;; first time or resuming execution.  /\/: The exec-control struct
  ;; will be copied if a sibling COA is made (e.g. when replanning),
  ;; so maybe this will do for a test:
  (not (null (coa-exec-control coa))))


;;;; The exec-step struct

;;; Each instance represents a "step" in the execution of the plan.
;;; Most steps will correspond directly to a node-end.

;;; /\/: May want to include level, parent-tag.

(defstruct (exec-step (:print-function print-exec-step))
  tag				;usually an etag such as (node-3 :end)
  type                          ;e.g.: action, dummy, event
  pattern                       ;a list
  predecessors			;a list of exec-steps [initially just tags]
  successors			;a list of exec-steps [initially just tags]
  earliest-time                 ;int >= 0
  latest-time                   ;int >= 0, or positive infinity
  status			;nil or a keyword
  effects                       ;list of (p v) pairs
  failed-effects		;list of (p v) pairs
  conds				;list of conditions as GOST entries
  iconds)			;list of implicit conditions as GOST entries

(defun print-exec-step (step stream depth)
  (declare (ignore depth))
  (format stream "#<exec-step ~A ~A ~A ~A>"
	  (exec-step-tag step)
	  (exec-step-status step)
	  (exec-step-type step)
	  (exec-step-pattern step)))

(defvar *exec-step-table* 'not-an-exec-step-table)

(defun find-exec-step (tag)
  (gethash tag *exec-step-table*))

(defun install-exec-step (e)
  (setf (gethash (exec-step-tag e) *exec-step-table*) e))


;;;; Status colors

;;; These colors are taken from ACP3.

(defparameter *exec-status-color-alist*
  '((nil         . "#FFFFFF")		;white
    (:ready      . "#FFCC66")		;orange
    (:executing  . "#99FF99")		;green
    (:finished   . "#99FFFF")		;blue
    (:will-fail  . "silver")		;silver, a kind of grey
    (:failed     . "#FF6699")		;red
    (:impossible . "#FF6699")))		;red

(defun exec-status->color (name)
  (or (lookup name *exec-status-color-alist*)
      (error "Unknown exec status: ~S." name)))


;;;; The TA matrix

;;; We add an "Execute plan" row at the bottom.

(defun-local write-coa-matrix ()
  (html-aiai-table ()
    (write-coa-matrix-top-row)
    (write-coa-matrix-split-row)
    (write-coa-matrix-add-constraints-row)
    (write-coa-matrix-authority-row)
    (write-coa-matrix-plan-row)
    (write-coa-matrix-eval-rows)
    (write-coa-matrix-issues-row) ; /\/: Or should issues be another eval row?
    (write-coa-matrix-view-row)
    (write-coa-matrix-exec-row)
    ))

(defun-local write-coa-matrix-exec-row ()
  (html-item "tr align=center"
    (html-item "th align=right" "Execute plan:")
    (do-visible-coas (coa)
      (html-matrix-cell (coa)
	(cond ((have-plan-for-coa-p coa)
	       (if (demo-executing-coa *demo*)
		   ;; Only one exec at a time /\/
		   (html-line "Exec")
		 (html-anchor (coa-path-action-url :ta-exec coa) "Exec")))
	      ((plan-failure-for-coa-p coa)
	       (html-anchor (coa-path-action-url :no-plan coa) "No Plan"))
	      (t
	       (html-line ".")))))))


;;;; The :ta-exec action

;;; This is the request we get when the TA selects an "Exec" link
;;; from the "Execute plan" row of the TA matrix.  The COA is given
;;; to the Planner user who will manage the execution and any plan
;;; repairs that it requires.

(setf (get :ta-exec :path-action) 'ta-exec-path-action)

(defun-local ta-exec-path-action ()
  (which-coa)

  ;; Clear out any existing exec information so that we won't be
  ;; confused by it later on.
  (clear-existing-exec-info)

  ;; Give the Planner user authority to execute the COA.
  (setf (coa-planner-can-exec-p *coa*) t)

  ;; Give the COA to the Planner
  (give-coa *coa* *planner-user*)
  (when *single-user-mode-p*
    (set-user *planner-user*))

  ;; Back to the matrix page.
  (redirect-to-matrix-page))

(defun-local clear-existing-exec-info ()
  (clear-oplan-exec-state)
  (dolist (coa *coas*)
    (setf (coa-planner-can-exec-p coa) nil)
    (setf (coa-exec-control coa) nil)))

(defun-local clear-oplan-exec-state ()
  ;; /\/: This should really happen because of a message sent to O-Plan.
  (setq *ne-exec-aux-table* nil)	;is that right? /\/
  (clear-world-state)			;should we? /\/
  (clear-world-always))			;should we? /\/


;;;; The Planner matrix

;;; If the Planner user has the authority to execute any of the plans
;;; the user currently owns, we add an "Execute plan" row near the
;;; bottom.

;;; When there are COAs the Planner can execute, any other COAs owned
;;; by the Planner become invisible.

(defun-local write-planning-coa-matrix ()
  (let* ((executable (remove-if-not #'coa-planner-can-exec-p (visible-coas)))
	 (*coas* (or executable *coas*)))
    (html-aiai-table ()
      (write-planning-coa-matrix-top-row)
      (write-planning-coa-matrix-planner-advice-row)
      (write-planning-coa-matrix-add-constraints-row)
      (write-planning-coa-matrix-authority-row)
      (write-planning-coa-matrix-plan-or-replan-row)
      (write-planning-coa-matrix-eval-rows)
      (write-planning-coa-matrix-issues-row)
      (write-planning-coa-matrix-view-row)
      (when executable
	(write-planning-coa-matrix-exec-row))
      (write-planning-coa-matrix-return-plan-row)
      )))

(defun-local write-planning-coa-matrix-exec-row ()
  (html-item "tr align=center"
    (html-item "th align=right" "Execute plan:")
    (do-visible-coas (coa)
      (html-matrix-cell (coa)
	(cond ((have-plan-for-coa-p coa)
	       (if (demo-executing-coa *demo*)
		   ;; Only one exec at a time /\/
		   (html-line "Exec")
		 ;; Exec or resume
		 (if (coa-partially-executed-p coa)
		     (html-anchor (coa-path-action-url :resume-exec coa)
				  "Resume")
		   (html-anchor (coa-path-action-url :exec-setup coa)
				"Exec"))))
	      ((plan-failure-for-coa-p coa)
	       (html-anchor (coa-path-action-url :no-plan coa) "No Plan"))
	      (t
	       (html-line ".")))))))


;;;; The :exec-setup action

;;; This is the request we get when the user selects an "Exec" link
;;; from the "Execute plan" row of the Planner's matrix.

;;; The result is a page containing a form that allows the user to
;;; specify world state and other factors that affect execution and
;;; then to request that execution begin.

;;; Note that this is supposed to happen only when execution first
;;; starts, not when restarted after a repair.  Restarts should
;;; result in :resume-exec requests.

;;; /\/: For now, we won't actually go to the setup page.
;;; Instead, we'll assume the user selected "Text / HTML".

(setf (get :exec-setup :path-action) 'exec-setup-path-action)

#+:undef
(defun-local exec-setup-path-action ()
  (which-coa)
  (write-coa-exec-setup-page))

(defun-local exec-setup-path-action ()
  (which-coa)
  (setf (query-arg :interface) "Text / HTML")
  (exec-plan-path-action-body))


;;;; The exec-setup page

;;; The page provides a form for specifying anything that's relevant
;;; to plan execution and a "submit" button that sends an :exec-plan
;;; request to begin execution.

(defun-local write-coa-exec-setup-page (&aux (coa *coa*))
  (html-standard-page
      (:title (title-for-user-and-coa coa "Execution Setup")
       :centered-title-header t)
    (html-block "p"
      "This page will allow the user to specify world-state and"
      "other factors that affect plan execution.")
    (html-colored-area "white"
      (html-form "post" (path-action-url :exec-plan)
        ;; Allow the user to specify the interface type.
	"Interface: "
        (html-select-from-list
          :name :interface
	  :size 1
	  :options
	    '((:selected "Text / HTML")
	      "Applet"))
	;; Put in the coa id as a hidden :n parameter.
	(html-line "<input type=\"hidden\" name=\"n\" value=\"~D\">"
		   (coa-id coa))
	(html-matrix-form-buttons "Begin execution")))))


;;;; The :exec-plan action

;;; The Planner user has decided to execute a plan.

;;; All other COA owned by the Planner user are deleted.

(defparameter *exec-plan-parameters*
  '((:n          (:int 0)    "COA number")
    (:interface  (:text)     "Interface type")))

(setf (get :exec-plan :path-action) 'exec-plan-path-action)

(defun-local exec-plan-path-action ()
  (parse-query-args)
  (convert-query-args *exec-plan-parameters*)
  (exec-plan-path-action-body))

(defun-local exec-plan-path-action-body ()
  ;; This routine is an entry point that does not parse and convert
  ;; query-args for when we want to skip the setup page.
  (assert (null (demo-executing-coa *demo*)))

  (let* ((coa (get-coa (query-arg :n)))
	 (ec (make-exec-control :coa-id (coa-id coa))))

    ;; "Delete" all other COAs owned by the current user.
    (dolist (c *coas*)
      (when (and (not (eq c coa))
		 (eq (coa-owner c) *user*))
	(setf (coa-visibility c) nil)))

    ;; Install a new exec-control struct in the coa.
    (setf (coa-exec-control coa) ec)

    ;; Only one exec at a time.
    (setf (demo-executing-coa *demo*) coa)

    ;; Note what we're doing
    (web-note "~&Exec for COA ~A (id ~S)~%~%" 
	      (coa-number coa) (coa-id coa))
    (coa-history-event coa :exec)

    ;; Move to the coa's current option.  This first sets :level authority
    ;; to -1 to ensure that no planning occurs.
    (move-planner-to-coa-option coa)

    ;; Get the always facts, because we'll need them in order to
    ;; maintain the world state correctly.
    (send-to-oplan :get :always-facts)
    (receive-else-error '(:always-facts $facts)
      #'(lambda (facts)
	  (with-coa-exec-state (coa)
	    (loop for (p v) in facts
		  do (set-world-always-value p v)))))

    ;; Get the execution plan.  This also tells O-Plan to set up
    ;; for execution in the current option.
    (send-to-oplan :get :exec-plan)
    (receive-else-error '(:exec-plan $plan)
      #'(lambda (plan)
	  (setf (ec-exec-plan ec) plan)))

    ;; Write applet or text exec page.
    (cond ((string= (query-arg :interface) "Applet")
	   ;; Applet
	   (write-coa-exec-plan-execlet-page coa))
	  (t
	   ;; Text / HTML
	   (prepare-for-text-exec coa)
	   (write-coa-exec-plan-text-page coa)))))

;;; Preparation for Text / HTML interface

(defun-local prepare-for-text-exec (coa)
  (with-coa-exec-state (coa)

    (web-note "~&Exec-plan: ~S~%%" (coa-exec-plan coa))

    (install-exec-plan-steps coa)

    ;; Initially, all status values should be nil.
    (dolist (stage (coa-exec-plan coa))
      (dolist (step stage)
	(assert (null (exec-step-status step)))))

    ;; Set the status of the steps in the first stage to :ready.
    (dolist (step (first (coa-exec-plan coa)))
      (assert (exec-step-ready-p step))
      (setf (exec-step-status step) :ready))))

(defun-local install-exec-plan-steps (coa)
  ;; Put exec-steps in a table indexed by tag
  (dolist (stage (coa-exec-plan coa))
    (mapc #'install-exec-step stage))

  ;; Replace tags in predecessor and successor lists by the
  ;; corresponding exec-step structs.
  (dolist (stage (coa-exec-plan coa))
    (dolist (step stage)
      (setf (exec-step-predecessors step)
	    (mapcar #'find-exec-step (exec-step-predecessors step)))
      (setf (exec-step-successors step)
	    (mapcar #'find-exec-step (exec-step-successors step))))))


;;; Tell O-Plan how to answer :get :always-facts and :get :exec-plan
;;; messages.

;;; /\/: :get :exec-plan is the wrong way to do what it does, though,
;;; because it doesn't just get the current exec-plan -- it's what
;;; makes O-Plan set up for execution.  A :get :exec-plan should not
;;; do anything more than get some current data.

(setf (get :always-facts :ks-get-plugin) 'ks-get-always-facts)

(defun ks-get-always-facts ()
  (ipc-send-out :always-facts (db-call 'get-always-facts)))

(setf (get :exec-plan :ks-get-plugin) 'ks-get-exec-plan)

(defun ks-get-exec-plan ()
  (ipc-send-out :exec-plan (db-call 'get-exec-plan)))


;;;; The :resume-exec action

;;; The Planner user has selected a repaired plan.  Since more than one
;;; may have been created, by replanning, we again delete all other COA
;;; owned by the user.  This is to avoid confusion later on.

(setf (get :resume-exec :path-action) 'resume-exec-path-action)

(defun-local resume-exec-path-action ()
  (which-coa)

  (assert (coa-partially-executed-p *coa*))
  (assert (coa-exec-control *coa*))

  ;; Only one exec at a time
  (assert (null (demo-executing-coa *demo*)))
  (setf (demo-executing-coa *demo*) *coa*)

  ;; Note what we're doing
  (web-note "~&Resuming exec for COA ~A (id ~S)~%~%"
	    (coa-number *coa*) (coa-id *coa*))
  (coa-history-event *coa* :resume-exec)

  ;; "Delete" all other COAs owned by the current user.
  (dolist (c *coas*)
    (when (and (not (eq c *coa*))
	       (eq (coa-owner c) *user*))
      (setf (coa-visibility c) nil)))

  ;; Get a new exec plan and adjust our state accordingly.
  (resume-coa-execution *coa*)

  ;; Write an exec-control page.
  (redirect-to-exec-control-page))

;;; After some bookkeeping, above, we're ready to get the new plan
;;; from O-Plan.  It's nontrivial to install this plan, because
;;; some of it will already have been executed.

(defun-local resume-coa-execution (coa)
  (let* ((old (coa-exec-control coa))
	 (new (make-exec-control
	        :coa-id (coa-id coa)
		:always (ec-always old))))

    ;; Install the new exec control struct
    (setf (coa-exec-control coa) new)

    (with-coa-exec-state (coa)

      ;; Move to the coa's current option.  This first sets :level authority
      ;; to -1 to ensure that no planning occurs.
      (move-planner-to-coa-option coa)

      ;; We'll just assume that the always facts have not changed.  /\/

      ;; Get the execution plan
      (send-to-oplan :get :exec-plan)
      (receive-else-error '(:exec-plan $plan)
        #'(lambda (plan)
	    (setf (ec-exec-plan new) plan)))

      (web-note "~&Exec-plan: ~S~%%" (coa-exec-plan coa))

      (install-exec-plan-steps coa)

      ;; Initially, all status values should be nil or :finished.
      (dolist (stage (coa-exec-plan coa))
	(dolist (step stage)
	  (assert (member (exec-step-status step) '(nil :finished)))))

      ;; We simulate execution of the ones that are :finished, so
      ;; that our world state will be correct.  Note that this is
      ;; *not* the usual simulation but a much simpler one.
      (dolist (stage (coa-exec-plan coa))
        (dolist (step stage)
	   (when (eq (exec-step-status step) :finished)
	     ;; Current time
	     (setf (ec-now *exec-control*) (exec-step-earliest-time step))
	     ;; Effects
	     (loop for (p v) in (exec-step-effects step)
		   unless (has-world-always-value-p p)
		   do (set-world-pattern-value p v))
	     ;; Maybe successors are now :ready
	     (loop for s in (exec-step-successors step)
		   when (exec-step-ready-p s)
		   do (setf (exec-step-status s) :ready)))))

      ;; /\/: May want to check that the world state is the same as
      ;; it was before (in the old exec-control struct).

      ;; Set the status of the steps in the first stage to :ready
      ;; if they are currently nil.
      ;; /\/: This should probably not be necessary
      (dolist (step (first (coa-exec-plan coa)))
	(when (null (exec-step-status step))
	  (setf (exec-step-status step) :ready)))

      ;; Check that the entire plan can execute.
      (let ((broken (find-steps-with-broken-conds)))
	(when broken
	  (web-note "~&Broken steps: ~S~%~%" broken)
	  (error "Plan still broken after plan repair."))))))


;;;; The exec-plan text page = the execution control page

;;; We write this page when an :exec-plan request specifies "Text / HTML"
;;; as the interface.

;;; :exec-control requests also take us (back) to this page, and that
;;; allows us to use the page as a "focus".

(setf (get :exec-control :path-action) 'exec-control-path-action)

(defun-local exec-control-path-action ()
  (let ((coa (demo-executing-coa *demo*)))
    (unless coa
      (bogus-matrix-request-error))
    (write-coa-exec-plan-text-page coa)))

;;; We can redirect to the page too.

(defun redirect-to-exec-control-page ()
  (send-http-redirection-response *http-request* *http-io*
    :status-code 302			;moved temporarily
    :to (concat-string (path-action-url :exec-control)
		       "#focus")))

;;; Here's where we actually write the HTML

(defvar *exec-control-page-message-fn* nil)

(defvar *wrote-exec-control-focus-p* nil)

(defun-local write-coa-exec-plan-text-page (coa)
  (unless (eq coa (demo-executing-coa *demo*))
    (bogus-matrix-request-error))
  (set-user-focus-page *user* "Execution page" (path-action-url :exec-control))
  (html-standard-page (:title (title-for-user-and-coa coa "Plan Execution")
		       :centered-title-header t)
    (let ((*wrote-exec-control-focus-p* nil))
      (with-coa-exec-state (coa)
        (write-exec-control-page-buttons coa)
	(write-any-exec-control-page-message)
        (html-paragraph
          (html-tag "b" "Current time: ~A"
	    (seconds->minimal-time-string (ec-now *exec-control*))))
	(html-tag "h2" "Execution steps")
	(write-exec-control-table coa)
	(when (ec-broken-plan-p *exec-control*)
	  (write-request-repair-form coa))))))

(defun-local write-exec-control-page-buttons (coa)
  (html-button-bar
    `(("TF file"                 ,(demo-tf-url *demo*))
      ("Server status"           ,(path-action-url :server-status))
      ("Plan views"              ,(coa-path-action-url :view-plan coa))
      ("Current state"           ,(coa-path-action-url :exec-world-state coa))
      ("Quit execution"          ,(path-action-url :quit-exec)))))

(defun-local write-exec-control-page-focus ()
  (assert (null *wrote-exec-control-focus-p*))
  (html-output "<a name=\"focus\"></a>")
  (setq *wrote-exec-control-focus-p* t))

(defun-local write-any-exec-control-page-message ()
  (when *exec-control-page-message-fn*
    (unless *wrote-exec-control-focus-p*
      (write-exec-control-page-focus))
    (unwind-protect
	(funcall *exec-control-page-message-fn*)
      (setq *exec-control-page-message-fn* nil))))

(defun-local write-request-repair-form (coa)
  (html-form "post" (coa-path-action-url :request-repair coa)
    (html-line "<input type=\"submit\" value=\"~A\">"
	       "Request plan repair")))

(defun-local write-exec-control-table (coa)
  (let* ((exec-plan (coa-exec-plan-for-display coa))
	 (rows (loop for stage in exec-plan sum (length stage)))
	 (focus-stage (find-exec-control-focus-stage exec-plan))
	 (now -1))
    (html-aiai-table ()
      (dolist (stage exec-plan)
	(let ((stage-est (exec-step-earliest-time (first stage))))
	  (dolist (step stage)
	    (html-block "tr"
	      ;; Time
	      (cond ((/= stage-est now)
		     (setq now stage-est)
		     (assert (eq step (first stage)))
		     (html-item `("td align=right valign=top"
				  ("rowspan" ,(length stage)))
		       (html-output (seconds->minimal-time-string now))))
		    ((eq step (first stage))
		     (html-item `("td align=right"
				  ("rowspan" ,(length stage)))
		       "&nbsp;")))
	      ;; Description
	      (html-item (format nil "td bgcolor=~S"
				 (exec-status->color (exec-step-status step)))
	        (html-output (exec-step-description-string step))
		;; Focus point
		(when (and (not *wrote-exec-control-focus-p*)
			   (eq stage focus-stage)
			   (eq step (first stage)))
		  (write-exec-control-page-focus)))
	      ;; Blank column
	      (when (and (eq stage (first exec-plan))
			 (eq step (first stage)))
		(html-item `("td width=3" ("rowspan" ,rows))
		  "&nbsp;"))
	      ;; "Edit" link, blank if edit not allowed
	      ;; /\/: We call it "Setup" now
	      (html-item "td"
		(if (member (exec-step-status step)
			    '(nil :ready :will-fail :failed :impossible))
		    (html-anchor
	              (coa-exec-step-url :exec-step coa step)
		      "Setup")
		  (html-output (make-nbsps 7))))
	      ;; Blank column
	      (when (and (eq stage (first exec-plan))
			 (eq step (first stage)))
		(html-item `("td width=3" ("rowspan" ,rows))
		  "&nbsp;"))
	      ;; "Exec" link, blank if exec not allowed
	      (html-item "td"
		(if (member (exec-step-status step) '(nil :ready :will-fail))
		    (html-anchor
		      (coa-exec-step-url :exec-to coa step)
		      "Exec")
		  (html-output (make-nbsps 7)))))))))
    (assert *wrote-exec-control-focus-p*)))

(defun find-exec-control-focus-stage (exec-plan)
  ;; The exec-plan should be one for display -- already filtered.
  ;; We want the last stage at which there's a time change that's
  ;; before the first stage that contains a non-:finished step.
  ;; That's the same as the last completely :finished stage at
  ;; which there's a time change.
  ;; /\/: Wrong for domains in which the time seldom changes.
  (let ((maybe nil)			;running candidate
	(now -1))
    (dolist (stage exec-plan)
      (let ((stage-est (exec-step-earliest-time (first stage)))
	    (finished-p (null (find :finished stage :key #'exec-step-status
				    :test-not #'eq))))
	(cond ((not finished-p)
	       (return))
	      ((/= stage-est now)
	       (setq now stage-est
		     maybe stage)))))
    (or maybe (first exec-plan))))

(defun-local coa-exec-step-url (action coa step)
  (path-action-url action
    (format nil "~D/~A/~A"
      (coa-id coa)
      (etag-node (exec-step-tag step))
      (etag-end (exec-step-tag step)))))

(defun-local exec-step-description-string (step)
  (format nil "~A ~As"
	  (ecase (exec-step-type step)
	    (start "Plan start")
	    (finish "Plan finish")
	    (dummy
	     (format nil "Dummy ~A" (etag-node (exec-step-tag step))))
	    (action
	     (princ-to-string (exec-step-pattern step)))
	    (event
	     (format nil "World event ~A" (exec-step-pattern step))))
	  (etag-end (exec-step-tag step))))

(defun make-nbsps (n)
  (big-string-concat (loop repeat n collect "&nbsp;")))


;;; Filtering an exec plan for display

(defun-local coa-exec-plan-for-display (coa)
  (filter-exec-plan-for-display (coa-exec-plan coa)))

(defun-local filter-exec-plan-for-display (exec-plan)
  (loop for stage in exec-plan
	for filtered-stage =
	    (loop for step in stage
		  unless (filter-exec-step-for-display-p step)
		  collect step)
	when filtered-stage
	collect filtered-stage))

(defun-local filter-exec-step-for-display-p (step)
  (eq (exec-step-type step) 'dummy)		;for now /\/
  #+:undef
  (and (eq (exec-step-type step) 'dummy)
       (null (exec-step-effects step))
       (null (exec-step-conds step))
       (null (exec-step-iconds step))))


;;;; The :exec-step action

;;; This is the resquest that results when the user selects a link
;;; in the exec-control table.
;;;
;;; The URLs look like this:
;;;
;;;   .../coa-id/node-name/begin-or-end
;;;

(setf (get :exec-step :path-action) 'exec-step-path-action)

(defun-local exec-step-path-action ()
  (which-coa)
  (with-coa-exec-state (*coa*)
    (let ((step (which-exec-step)))
      (write-coa-exec-step-page *coa* step))))

(defun-local which-exec-step ()
  ;; Assumes which-coa had been called to set *coa* and that
  ;; the *path-args* are from a coa-exec-step URL.
  (unless (eq *coa* (demo-executing-coa *demo*))
    (bogus-matrix-request-error))
  (let* ((node-name (intern (string-upcase (second *path-args*))
			    (find-package :oplan)))
	 (end (string->keyword (third *path-args*)))
	 (tag (etag node-name end))
	 (step (find-exec-step tag)))
    step))


;;;; The coa-exec-step page

;;; This allows the user to examine a step, mark it for failure, etc.

(defun-local write-coa-exec-step-page (coa step)
  (html-standard-page
      (:title (title-for-user-and-coa coa "Execution Step")
       :centered-title-header t)
    (write-coa-exec-step-page-buttons coa)
    (html-tag "h2" "~:(~A~)" (exec-step-description-string step))
    (write-exec-step-description step)
    (html-line "<p>")
    (html-colored-area "white"
      (when (member (exec-step-status step) '(nil :ready))
	(html-form "post" (coa-exec-step-url :exec-to coa step)
	  (html-matrix-form-buttons
	    (if (member (exec-step-status step) '(nil :ready))
		"Execute to this point"
	      :no-submit)
	    :other-submit-buttons
	      (if (member (exec-step-status step) '(nil :ready))
		  '(("fail" "Mark for failure"))
		'())
	    :return-to :focus)))
      (unless (eq (exec-step-status step) :finished)
	(write-coa-exec-step-event-addition-form coa step)))))

(defun-local write-coa-exec-step-page-buttons (coa)
  (html-button-bar
    `(("TF file"                 ,(demo-tf-url *demo*))
      ("Current state"           ,(coa-path-action-url :exec-world-state coa))
      )))

;;; The description of an exec-step

(defun-local write-exec-step-description (step)

  ;; Effects
  (when (exec-step-effects step)
    (html-tag "h2" "Effects")
    (html-pv-table (exec-step-effects step)))

  ;; Conditions
  (when (exec-step-conds step)
    (html-tag "h2" "Conditions")
    (html-condition-table (exec-step-conds step)))

  ;; Implicit conditions
  (when (exec-step-iconds step)
    (html-tag "h2" "Implicit Conditions")
    (html-condition-table (exec-step-iconds step))))

(defun html-condition-table (conds)
  ;; /\/: It would be nice if we knew the contributor(s).
  ;; /\/: We should indicate whether the cond is already sat or not.
  ;; /\/: Using html-tag for a "th" isn't colored by html-aiai-table.
  (html-aiai-table ()
    (html-item "tr align=left"
      (html-item "th" "Pattern")
      (html-item "th" "Desired value")
      (html-item "th" "Current value"))
    (dolist (c conds)
      (let ((p (tgm-gost-pattern c))
	    (v (tgm-gost-value c)))
        (html-item "tr"
	  (html-item "td"
            (html-tag-line "tt" "~A"
              (html-encode-pre-string (princ-to-string p))))
	  (html-item "td"
            (html-tag-line "tt" "~A"
              (html-encode-pre-string (princ-to-string v))))
	  (html-item "td"
            (html-tag-line "tt" "~A"
              (html-encode-pre-string
	        (princ-to-string
		  (get-world-value p "<no value>"))))))))))


;;;; A form for adding world events to the exec plan

(defun-local write-coa-exec-step-event-addition-form (coa step)
  (html-tag "h2" "World Event")
  (html-box
    (html-form "post" (coa-exec-step-url :add-world-event coa step)
      (html-paragraph
        (html-line "<input type=\"submit\" value=\"~A\"> "
		   "Place event")
        (html-select (:name "order" :size 1)
	  '(:selected "Before")
	  "After")
	(html-format "~@(~A~)" (exec-step-description-string step)))
      (html-block "table"
        (html-item "tr align=left valign=top"
	  (html-tag "th" "Pattern:")
	  (html-item "td"
	    (html-output
	      "<input size=72 name=\"pattern\">")))
	(html-item "tr align=left valign=top"
	  (html-tag "th" "Effects:")
	  (html-item "td"
	    (html-block "textarea name=\"effects\" rows=4 cols=72"))))
      (html-line "<input type=\"reset\" value=\"~A\"> "
		 "Undo changes to form"))))


;;;; The :add-world-event action

;;; /\/: Should define :pattern and :pattern-value query-arg types.

(defparameter *add-world-event-parameters*
  '((:order   (:keyword) "Before or after current step")
    (:pattern (:text)    "Event pattern")
    (:effects (:text)    "Event effects")))

(setf (get :add-world-event :path-action) 'add-world-event-path-action)

(defun-local add-world-event-path-action ()
  (parse-query-args)
  (convert-query-args *add-world-event-parameters*)
  (which-coa)
  (with-coa-exec-state (*coa*)
    (let ((*readtable* oplan-util::*safe-readtable*))
      (let ((step (which-exec-step))
	    (direction (query-arg :order))
	    (pattern (get-pattern-from-string (query-arg :pattern)))
	    (effects (get-effects-from-string (query-arg :effects))))

	;; Add the event
	(add-world-event *coa* step direction pattern effects)

	;; Back to the execution control page
	(redirect-to-exec-control-page)))))

(defun get-pattern-from-string (s)
  (let ((items (string->list s)))
    ;; The pattern must be a single list that begins with a symbol.
    (cond ((not (length=1 items))
	   (error "The pattern must be a single list."))
	  ((not (and (consp (car items))
		     (symbolp (caar items))))
	   (error "The pattern must be a list that begins with a symbol."))
	  (t
	   (car items)))))

(defun get-effects-from-string (s)
  (let ((lines (stream->lines (make-string-input-stream s))))
    (mapcar #'get-effect-from-string lines)))

(defun get-effect-from-string (s)
  (let* ((items (string->list s))
	 (len (length items)))
    ;; An = between pattern and value is optional.
    (when (and (= len 3) (eq (second items) '=))
      (setq items (list (first items) (third items))
	    len 2))
    (cond ((not (= len 2))
	   (error "Line ~S does not have form pattern = value." s))
	  ((not (and (consp (car items))
		     (symbolp (caar items))))
	   (error
	     "The pattern in ~S is not a list that begins with a symbol."
	     s))
	  (t
	   items))))


;;;; Adding a world event

(defun add-world-event (coa step direction pattern effects)
  (declare (ignore coa))
  (let ((event (make-world-event-step step pattern effects)))
    (install-exec-step event)		;so we can find it from URLs
    (setf (ec-exec-plan *exec-control*)
	  (insert-event-in-exec-plan 
	    (ec-exec-plan *exec-control*)
	    event step direction))))

(defun insert-event-in-exec-plan (plan event step direction)
  (mapcar #'(lambda (stage)
	      (if (member step stage)
		  (insert-event-in-exec-stage stage event step direction)
		stage))
	  plan))

(defun insert-event-in-exec-stage (stage event step direction)
  (mapcan #'(lambda (s)
	      (if (eq s step)
		  (ecase direction
		    (:before (list event step))
		    (:after  (list step event)))
		(list s)))
	  stage))

(defun make-world-event-step (step pattern effects)
  ;; /\/: :occur so that exec-step-description-string will say "occurs"
  (make-exec-step
    :tag (list (gentemp "EVENT-") :occur)
    :type 'event
    :pattern pattern
    :effects effects
    :earliest-time (exec-step-earliest-time step)
    :status :ready))


;;;; The :exec-to action

;;; This is what happens when the user selects an "Exec" link in
;;; exec control table.

(defparameter *exec-to-parameters*
  '((:fail   (:optional (:text))   "failure flag")))

(setf (get :exec-to :path-action) 'exec-to-path-action)

(defun-local exec-to-path-action ()
  (parse-query-args)
  (convert-query-args *exec-to-parameters*)
  (which-coa)
  (with-coa-exec-state (*coa*)
    (let ((step (which-exec-step)))
      (cond ((query-arg :fail)
	     ;; Set up step for failure
	     (setf (exec-step-status step) :will-fail)
	     ;; /\/: Only total failure for now
	     (setf (exec-step-failed-effects step)
		   (exec-step-effects step)))
	    (t
	     ;; Execute everything up to the indicated step
	     (simulate-coa-execution *coa* :to-step step)))
      ;; Back to the execution control page
      (redirect-to-exec-control-page))))


;;;; Our execution simulator

;;; N.B. must be called inside a with-coa-exec-state.

(define-condition exec-failure (simple-condition) ())

(defun simulate-coa-execution (coa &key to-step)
  (check-type to-step exec-step)
  (handler-case
      (dolist (stage (coa-exec-plan coa))
	(dolist (step stage)
          (case (exec-step-status step)
	    (:finished)			;skip
	    (:failed)			;skip
	    (:impossible)		;skip
	    (:ready
	     (simulate-exec-step step))
	    (:will-fail
	     (simulate-exec-step step)
	     (error ":will-fail step ~S failed to fail." step)))
	  (when (eq step to-step) 
	    (return-from simulate-coa-execution t))))
    ;; Exit if an exec-failure is signalled.
    (exec-failure)))

(defun simulate-exec-step (step)
  ;; Current time
  (setf (ec-now *exec-control*) (exec-step-earliest-time step))
  (ecase (exec-step-type step)
    ((start finish dummy action)
     (ecase (exec-step-status step)
       (:ready     (simulate-exec-step-success step))
       (:will-fail (simulate-exec-step-failure step))))
    ((event)
     (simulate-world-event-exec-step step))))

;;; Successful execution of one step

(defun simulate-exec-step-success (step)
  (exec-report :success (exec-step-tag step) (exec-step-pattern step))
  ;; Effects
  (loop for e in (exec-step-effects step)
	for (p v) = e
	unless (has-world-always-value-p p)
	do (set-world-pattern-value p v))
  ;; Status now :finished
  (setf (exec-step-status step) :finished)
  ;; Some successors may now be ready
  (loop for s in (exec-step-successors step)
	when (exec-step-ready-p s)
	do (setf (exec-step-status s) :ready)))

(defun exec-step-ready-p (step)
  (and (null (exec-step-status step))
       (loop for pre in (exec-step-predecessors step)
	     always (eq (exec-step-status pre) :finished))))

;;; Complete or partial failure

;;; /\/: For now, if a step fails, we assume all steps linked after it,
;;; directly or indirectly, become :impossible.  We probably ought to
;;; mark them :impossible only if some condition would actually be broken.

(defun simulate-exec-step-failure (step)
  (let ((failed-effects (exec-step-failed-effects step)))
    (exec-report :failure (exec-step-tag step) (exec-step-pattern step)
		 failed-effects)
    (setf (ec-broken-plan-p *exec-control*) t)
    ;; Effects
    (loop for e in (exec-step-effects step)
	  for (p v) = e
	  unless (or (has-world-always-value-p p)
		     (member e failed-effects))		;N.B. test = eql
	  do (set-world-pattern-value p v))
    ;; Status now :failed
    (setf (exec-step-status step) :failed)
    ;; All successors, direct or indirect, are now impossible
    (walk-depth-first (exec-step-successors step)
      :children-fn #'exec-step-successors
      :visitor
        #'(lambda (s) (setf (exec-step-status s) :impossible)))
    ;; Let anyone know of the failure who cares.
    (signal 'exec-failure)))

(defun walk-depth-first (roots &key children-fn visitor)
  (let ((marks (make-hash-table :test #'eq)))
    (macrolet ((mark (e) `(gethash ,e marks)))
      (labels ((walk (at)
		 (ecase (mark at)
		   ((:start)   (error "Cycle at ~S." at))
		   ((:finish)) ; already processed
		   ((nil)
		    (setf (mark at) :start)
		    (funcall visitor at)
		    (mapc #'walk (funcall children-fn at))
		    (setf (mark at) :finish)))))
	(mapc #'walk roots)))))

;;; Execution of world events

;;; This is tricky, because we want to determine what (if any) conditions
;;; are broken.

;;; If any are broken, the affected steps are marked :impossible.

;;; /\/: We also mark all steps linked directly or indirectly after
;;; the affected steps as :impossible too, but perhaps we ought to
;;; do that only if their conditions would be affected by earlier
;;; :impossibles.  This is the same issue as for complete or partial
;;; failure above.

(defun simulate-world-event-exec-step (step)
  (exec-report :world-event (exec-step-pattern step) (exec-step-effects step))
  
  ;; Effects
  (loop for e in (exec-step-effects step)
	for (p v) = e
	unless (has-world-always-value-p p)
	do (set-world-pattern-value p v))

  ;; Status now :finished
  (setf (exec-step-status step) :finished)

  ;; Find any later steps that have conditions that can no longer be
  ;; satisfied because (a) the contributor has already executed, and
  ;; (b) the pattern's value is no longer what the condition wants
  ;; (presumably becuase this world-event has changed the value).
  ;; /\/: Check the presumption.
  (let ((broken (find-steps-with-broken-conds)))
    (when broken
      (setf (ec-broken-plan-p *exec-control*) t)
      (walk-depth-first broken
	:children-fn #'exec-step-successors
        :visitor
          #'(lambda (s) (setf (exec-step-status s) :impossible))))

    ;; Nonetheless, some successors may now be ready.
    (loop for s in (exec-step-successors step)
	  when (exec-step-ready-p s)
	  do (setf (exec-step-status s) :ready))

    ;; If anything was broken, tell anyone who cares
    (when broken
      (signal 'exec-failure))))


;;; Find-steps-with-broken-conds is a mini-simulator that can be used to
;;; check whether all not-yet-:finished steps could execute successfully.
;;; It returns a list of the steps that had any conds (or iconds) that
;;; were not satisfied at the time the step's execution was simulated.

;;; /\/: It should probably skip event steps if we allow unexecuted
;;; world events to persist across a repair (or automatically reinstall
;;; them afterwards).

(defun find-steps-with-broken-conds ()
  (let ((*world-state* (copy-hash-table *world-state* :test #'equal))
	(broken (make-tconc)))
    (dolist (stage (coa-exec-plan *coa*))
      (dolist (step stage)
	(unless (eq (exec-step-status step) :finished)
	  (unless (and (every #'sim-exec-cond-satisfied-p
			      (exec-step-conds step))
		       (every #'sim-exec-cond-satisfied-p
			      (exec-step-iconds step)))
	    (tconc broken step))
	  (loop for (p v) in (exec-step-effects step)
		unless (has-world-always-value-p p)
		do (set-world-pattern-value p v)))))
    (tconc-contents broken)))

(defun copy-hash-table (table &key (test #'eql))
  (let ((new (make-hash-table :test test)))
    (maphash #'(lambda (k v)
		 (setf (gethash k new) v))
	     table)
    new))


;;;; The :request-repair action

(setf (get :request-repair :path-action) 'request-repair-path-action)

(defun-local request-repair-path-action ()
  (which-coa)

  (web-note "~&Request reapir for COA ~A (id ~S)~%~%" 
	      (coa-number *coa*) (coa-id *coa*))
  (coa-history-event *coa* :request-repair)

  (setf (demo-executing-coa *demo*) nil) ;no longer executing this coa

  (with-coa-exec-state (*coa*)

    ;; Move to the coa's current option.  This first sets :level authority
    ;; to -1 to ensure that no planning occurs.
    (move-planner-to-coa-option *coa*)

    ;; Send an execution report
    (send-to-oplan :exec-report
      (tconc-contents (ec-exec-report *exec-control*)))

    ;; Clear the execution report so we'll build a new one if we
    ;; execute further later on.
    (setf (ec-exec-report *exec-control*) (make-tconc))

    ;; O-Plan will tell us if it thinks any plan repair is needed.
    (receive-else-error '(:exec-report $status)
      #'(lambda (status)
	  (ecase status
	    (:ok
	     (handle-no-repair-needed *coa*))
	    (:broken
	     (handle-broken-plan *coa*)))))))


;;; If the failed step(s?) had no effects, no repair will be needed.

;;; /\/: We probably ought to get a new plan from O-Plan in any case,
;;; to check, and because world events will have been expanded into
;;; two node ends (rather than just being one exec-step, they'll each
;;; be two).  Moreover, partial failures will change the list of effects
;;; on the partially-failed node-ends.

(defun-local handle-no-repair-needed (coa)
  ;; Now we're executing again
  (setf (demo-executing-coa *demo*) coa)
  ;; Consider it fixed.
  (setf (ec-broken-plan-p *exec-control*) nil)
  ;; :Failed steps become :finished, and :impossible actions
  ;; become :ready or nil.
  (dolist (stage (coa-exec-plan coa))
    (dolist (step stage)
      (case (exec-step-status step)
	((:failed)
	 (setf (exec-step-status step) :finished))
	((:impossible)
	 (setf (exec-step-status step) nil) 	;for ready-p test /\/
	 (setf (exec-step-status step)
	       (if (exec-step-ready-p step) :ready nil))))))
  ;; Tell the user how well things went.
  (setq *exec-control-page-message-fn*
	#'(lambda ()
	    (html-paragraph
	      "<b>Failure has no impact - no repair needed</b>")))
  ;; Return to the exec control page
  (redirect-to-exec-control-page))


;;; Some :fix entries are now on the agenda.

;;; This is our equivalent of plan-for-coa and ask-for-plan-for-coa
;;; from coa-planning-support.lisp.

;;; We also have to put a :planner-finished on the agenda and
;;; adjust the coa's plan-status.

(defun-local handle-broken-plan (coa)

  ;; We have added something, so we need to make sure a :finished
  ;; message will eventually be sent to the TA.
  (post-agenda '(:PLANNER-FINISHED) :trigger '(:EMPTY))

  ;; Make the status :partial, because more planning is possible.
  (setf (coa-plan-status coa) :partial)

  ;; Tell ks-fix not to ask the user wither to fix by achieve or
  ;; by doing nothing.
  (setq *fix-mode* :auto)

  ;; Make sure we ask the user which schema to use for a ks-fix.  /\/
  (setq *schema-selection-mode* :ask)

  ;; This is the plan-for-coa part:

  ;; Clear any existing plan info
  (clear-coa-current-plan coa)
  ;; Plan
  (handler-case (plan-for-repair-for-coa coa)
    (timeout ()		  
      (error "Planning took more than the time limit of ~S seconds"
	     *coa-planning-time-limit*)))
  ;; Back to the user
  (if (eq (coa-plan-status coa) :question)
      (write-question-answering-page)
    (redirect-to-matrix-page)))

(defun-local plan-for-repair-for-coa (coa)
  (setq *planning-coa* coa)
  (setf (coa-change-p coa) t)
  (assert (null (coa-actions-to-add coa)))
  (setf (coa-plan-statistics coa) nil)		;??? /\/

  ;; Move to the coa's current option.
  (move-planner-to-coa-option coa)

  ;; Reset agenda counters, so the planning statistics will be right,
  ;; and so we get a fresh start at the cycle count limit.
  (send-to-oplan :reset-agenda-counters)
  (receive-else-error '(:ok))

  ;; Set the authority for this COA.
  (set-planner-authority-for-coa coa)

  ;; See if we have a plan.
  (coa-history-event coa :plan)
  (pprocess-main-loop)		;a chance to use the new authority
  (get-planning-results coa))


;;;; The :quit-exec action

;;; /\/: May want to tell the planner to clear out all exec stuff,
;;; or at least the node-end exec- status stuff, so that we can exec
;;; afresh if we want to.

(setf (get :quit-exec :path-action) 'quit-exec-path-action)

(defun quit-exec-path-action ()
  (setf (demo-executing-coa *demo*) nil)
  (clear-existing-exec-info)
  (redirect-to-matrix-page))


;;;; The :exec-world-state action

(setf (get :exec-world-state :path-action) 'exec-world-state-action)

(defun-local exec-world-state-action ()
  (which-coa)
  (with-coa-exec-state (*coa*)
    (html-standard-page
        (:title (title-for-user-and-coa *coa* "Current World State")
	 :centered-title-header t)
      (html-tag-line "h2" "Current state")
      (if (zerop (hash-table-count *world-state*))
	  (html-paragraph "The current state is empty.")
	(html-pv-table
	 (canonical-description-order
	  (loop for p being each hash-key of *world-state*
		using (hash-value v)
		collect (pv-pair p v)))))
      (when (> (hash-table-count *world-always-table*) 0)
	(html-tag-line "h2" "Always")
	(html-pv-table
	 (canonical-description-order
	  (loop for p being each hash-key of *world-always-table*
	      using (hash-value v)
	      collect (pv-pair p v))))))))


;;;; "Internal" execution support

;;; Get-exec-plan -- set up for execution and return a list of
;;; exec-step structs.

(defun get-exec-plan ()
  (set-up-for-execution)
  (make-exec-plan))

(defun set-up-for-execution ()
  (init-ne-exec-slots)
  (init-ne-effect-lists)
  (init-ne-condition-lists))

(defun make-exec-plan ()
  (mapcar #'(lambda (stage) (mapcar #'make-exec-step-from-node-end stage))
	  (node-ends-in-execution-stages :actions-only nil)))

(defun make-exec-step-from-node-end (ne)
  (let ((n (ne-node ne)))
    (make-exec-step
      :tag (ne-tag ne)
      :type (n-type n)
      :pattern (fully-instantiate-psvs (n-pattern n))
      :predecessors (mapcar #'ne-tag (ne-pre-ends ne))
      :successors (mapcar #'ne-tag (ne-post-ends ne))
      :earliest-time (tpoint-min (ne-time-point ne))
      :latest-time (tpoint-max (ne-time-point ne))
      :status (case (ne-exec-status ne)
		(:finished :finished)
		(otherwise nil))
      :effects (fully-instantiate-psvs (ne-effects ne))
      :conds (fully-instantiate-psvs (ne-conds ne))
      :iconds (fully-instantiate-psvs (ne-iconds ne)))))


;;;; KS-Exec-Report

(defvar *repair-needed-p* nil)

(defun ks-exec-report (event)
  (let ((report (first (ag-args event)))
	(*repair-needed-p* nil))
    (let ((*dev-debug-level* (oplan-dev::dev-level-name->number :trace))
	  (*trace-output* *web-notes*))	;for http mode /\/
      (mapc #'process-exec-report-record report))
    (ipc-send-out :exec-report
      (if *repair-needed-p*
	  :broken
	:ok))))

(defun process-exec-report-record (r)
  (web-note "~&Exec rec: ~S~%" r)
  (destructuring-bind (type &rest args) r
    (ecase type
      (:success
       (destructuring-bind (time etag pattern) args
	 (exec-report-success time etag pattern)))
      (:failure
       (destructuring-bind (time etag pattern failed-effects) args
	 (exec-report-failure time etag pattern failed-effects)))
      (:world-event
       (destructuring-bind (time pattern effects) args
	 (exec-report-world-event time pattern effects))))))


;;;; Our version of ks-execution-success 

(defun exec-report-success (time etag pattern)
  (declare (ignore time pattern))

  ;; Mark the node-end as having been executed.
  (db-call 'set-etag-exec-status etag :finished))


;;;; Our version of ks-execution-failure

(defun exec-report-failure (time etag pattern failed-effects)
  (declare (ignore time))

  (block KS-EXECUTION-FAILURE

    (dev-debug :trace "Failed to execute ~A ~W" etag pattern)

    ;; Mark the node-end has having been executed.
    (db-call 'set-etag-exec-status etag :finished)
    
    ;; Any failed effects?
    (when (null failed-effects)
      ;; No.  We're done.
      (dev-debug :trace "No failed effects, so off we go.")
      ; (tell-exec-to-execute-next-fringe etag)
      (return-from KS-EXECUTION-FAILURE))

    ;; Yes, some failed effects.
    (dev-debug :trace
      ;; /\/ Flatten effects list because in at least some Lisps
      ;; ~^ doesn't do the right thing in a ~:{ ... ~}.
      "Failed effects: ~%~{~7T~W = ~W~^~%~}"
      (flatten-one-level failed-effects))

    ;; Cancel the TOME entries for the failed effects.
    (db-call 'cancel-failed-tome-entries etag failed-effects)

    ;; Any affected GOST?
    (let ((affected-gost (db-call 'find-broken-gost-entries
				  etag failed-effects)))

      (when (null affected-gost)
	;; No GOST cares.  We're outa here.
	(dev-debug :trace "Though there are failed effects, no on cares.")
	; (tell-exec-to-execute-next-fringe etag)
	(return-from KS-EXECUTION-FAILURE))

      ;; Yes, some affected GOST.
      (dev-debug :trace "Affected GOST:~%~{~7T~W~^~%~}" affected-gost)

      ;; Check for multiple-contributors, which we can't handle yet. /\/
      (when (some #'gost-has-multiple-contributors-p affected-gost)
	(cerror "Continue plan execution."
		"Some of the affected GOST entries have > 1 contributor.")
	; (tell-exec-to-execute-next-fringe etag)
	(return-from KS-EXECUTION-FAILURE))

      ;; If we reach this point, we have some "truly broken" GOST ranges.
      ;; We have to replace them by agenda entries that will arrange for
      ;; the conditions to be satisfied in another way.

      ;; Find or create a point that's linked after everything that's
      ;; been executed so far.
      (let ((after-point (get-execution-after-point)))

	(dev-debug :trace "After-point = ~W" after-point)

	;; Post a FIX agenda entry for each truly broken GOST range
	;; altering the contributor or removing the range as required.
	(dolist (g affected-gost)
	  (fix-broken-gost g after-point))

	;; /\/: Let ks-exec-report know
	(setq *repair-needed-p* t)

	;; Make sure we don't backtrack into alts created before
	;; plan execution began.
	; (block-backtracking)

	;; Post agenda enties that will continue plan execution.
	; (post-agenda `(:CONTINUE-EXECUTION ,etag))
	#+:undef
	(post-agenda `(:CONTINUE-EXECUTION ,after-point))))))


;;;; Our version of ks-unexpected-world-event

(defun exec-report-world-event (time pattern effects)
  (declare (ignore time))
  
  (block KS-UNEXPECTED-WORLD-EVENT

    (dev-debug :trace "Unexpected world event: ~A" pattern)
    (when effects
      (dev-debug :trace "Effects: ~%~{~7T~W = ~W~^~%~}"
		 (flatten-one-level effects)))

    ;; Create a node to represent the event.
    (let* ((event-tag
	    (db-call 'ads-add-node
		     :type 'event
		     :pattern pattern
		     :reason '(:exec))))

      (dev-debug :trace "Event-point = ~W" event-tag)

      ;; Mark the event node as having been executed.
      (db-call 'set-node-exec-status event-tag :finished)

      ;; Link it after everything that has already executed and before
      ;; everything that hasn't.
      (db-call 'pinch-plan-at event-tag :finished)

      (let ((affected-gost (db-call 'event-broken-gost-entries effects)))

	(when (null affected-gost)
	  ;; No GOST cares.  We're outa here.
	  (dev-debug :trace "The event does not affect the plan.")
	  ;; But first put in the event's effects.
	  ;; /\/: Should use the effect_node_end from the TF defaults.
	  (add-etag-effects (etag event-tag :end) effects)
	  (return-from KS-UNEXPECTED-WORLD-EVENT))

	;; Yes, some affected GOST.
	(dev-debug :trace "Affected GOST:~%~{~7T~W~^~%~}" affected-gost)

	;; Check for multiple-contributors, which we can't handle yet. /\/
	(when (some #'gost-has-multiple-contributors-p affected-gost)
	  (cerror "Continue plan execution."
		  "Some of the affected GOST entries have > 1 contributor.")
	  (return-from KS-UNEXPECTED-WORLD-EVENT))

	;; If we reach this point, we have some "truly broken" GOST ranges.
	;; We have to replace them by agenda entries that will arrange for
	;; the conditions to be satisfied in another way.

	;; Post a FIX agenda entry for each broken truly GOST range,
	;; altering the contributor or removing the range as required.
	(let ((after-point (etag event-tag :end)))
	  (dolist (g affected-gost)
	    (fix-broken-gost g after-point)))

	;; Put in the event's effects
	;; /\/: Should use the effect_node_end from the TF defaults.
	(add-etag-effects (etag event-tag :end) effects)

	;; /\/: Let ks-exec-report know
	(setq *repair-needed-p* t)

	;; Make sure we don't backtrack into alts created before
	;; plan execution began.
	; (block-backtracking)

	;; Arrange to continue execution.
	; (post-agenda `(:CONTINUE-EXECUTION ,(etag event-tag :end)))

	;; And we're done.
	nil))))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
