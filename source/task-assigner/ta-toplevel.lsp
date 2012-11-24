;;;; File: ta-toplevel.lsp
;;; Contains: Task Assigner.
;;; Author: Richard Kirby (rbk) and Jeff Dalton
;;; Created: Tue Jul 10 13:20:48 1990
;;; Updated: Tue Mar 23 00:06:34 1999 by Jeff Dalton
;;; Copyright: (c) 1992, AIAI, University of Edinburgh

(in-package :oplan-task-assigner)

(use-package :oplan-util)
(use-package :oplan-developerlib)
(use-package :oplan-pseudo-process)
(use-package :oplan-ipc)
(use-package :oplan-xwindowio)
(use-package :oplan-initsystem)
(use-package :oplan-plan-world-viewer)

(import '(oplan::make-event))

(import 'ta-startup :oplan)		;/\/ for call-component-startup

;;; At the top of our window, we have something like this:
;;;
;;;   Status: plan option 1 - planning ...
;;;   Domain: pacifica
;;;   Task: Operation_Blue_Lagoon
;;;   Authority: plan(all=inf), execute=(all=no)
;;;

(defvar *ta-iostream* nil)

(defvar *status* "planner uninitialised") 	;but see ta-initialize
(defvar *domain* "none")
(defvar *task*   "none")
(defvar *plan-option* nil)
(defvar *plan-phase-authority* :all)		;node number or all
(defvar *plan-level-authority* :inf)		;level number
(defvar *execute-phase-authority* :all)		;node number or all
(defvar *execute-authority* :no)		;yes or no

(defvar *available-tasks* '())			;list of task names

(defparameter *status-lines*
  ;; See print-from-descriptions for an explanation.
  '( ("Status: " (when *plan-option* "plan option " *plan-option* " - ")
                 *status*)
     ("Domain: " *domain*)
     ("Task: " *task*)
     ("Authority: "
       "plan(" *plan-phase-authority* "=" *plan-level-authority* "), "
       "execute(" *execute-phase-authority* "=" *execute-authority* ")" )
   ))

(defparameter *choice-options*
  '( (1 :init        "Initialise Planner")
     (2 :input-tf    "Input TF")
     (3 :set-task    "Set Task")
     (4 :add-to-task "Add to Task")
     (5 :plan-view   "Plan View")
     (6 :world-view  "World View")
     (7 :replan      "Replan")
     (8 :execute     "Execute Plan")
     (9 :quit        "Quit") ))

(defvar *choices* '(:init :quit))


;;; TA main program

(defun ta-startup ()
  
  ;; This is the initial function for the TA process.  After startup,
  ;; it is not called again.  Instead, ta-event-handler is called
  ;; whenever a message arrives.
  
  (is-init)
  (ta-initialise)

  ;; Make breaks, etc. go to the debug window.
  (x-redirect-errors :tainout)

  ;; Add :status :run to the ipc-set-event-handler if no listen
  ;; stream is registered.
  (ipc-set-event-handler 'ta-event-handler)
  (ipc-register-listen-stream *ta-iostream*)
  )

(defun ta-initialise ()
  (setq *ta-iostream* (x-get-stream :tainout))
  (setq *status*
	(format nil "Version ~A ~A uninitialised"
		user::*oplan-version*
		user::*oplan-release-date*))
  (setq *choices* '(:init :quit))
  (present-options *choices*))

(defun ta-event-handler (self)
  (when (next-event-p self)
    (ipc-handle-message self (next-event self)))
  (ta-monitor))


;;;; Messages

(defmessage (:TA :INIT-OK) ()
  (setq *choices* '(:init :input-tf :quit))
  (setq *status* "planner-initialized")
  (setq *plan-option* 1)
  (setq *plan-level-authority* :inf)
  (setq *domain* "none")
  (setq *task* "none")
  (setq *available-tasks* '())
  (present-options *choices*))

(defmessage (:TA :DOMAIN) (result)
  ;; Result is (<pathname-name of the TF file> . <list of task names>)
  ;; or nil.
  (if (null result)
      (progn
	(setq *status* "domain tf error")
	(dev-debug :fatal-error "Couldn't load domain."))
    (let ((domain (car result))
	  (tasks (cdr result)))
      (setq *status* "domain loaded")
      (when tasks
	(setq *choices* '(:init :input-tf :set-task :quit))
	(add-to-available-tasks tasks))
      (if (equal *domain* "none")
	  (setq *domain* domain)
	  (setq *domain* (concat-string *domain* "+" domain)))))
  (present-options *choices*))

(defmessage (:TA :FINISHED) ()
  (setq *status* "planner finished")
  (setq *choices*
	(if (execution-is-allowed-p)
	    '(:init :input-tf :add-to-task :plan-view :world-view :replan
	      :execute :quit)
	    '(:init :input-tf :add-to-task :plan-view :world-view :replan
	      :quit)))
  (present-options *choices*))

(defun execution-is-allowed-p ()
  (if (parameter-set-p :allow-exec)
      (get-parameter :allow-exec)
    (is-get-config-entry :micro-exec)))

(defmessage (:TA :NO-MORE-ALTERNATIVES) ()
  (setq *status* "planner out of alternatives")
  (setq *choices* '(:init :quit))
  (present-options *choices*))

(defmessage (:TA :WAITING) (why)
  ;; /\/: Need to change the status if start planning again,
  ;; but, right now, the TA doesn't know when authority changes.
  (setq *status* (format nil "waiting for ~{~A~^, ~}" why))
  (present-options *choices*))

(defmessage (:TA :CHECKED) (&rest result)
  ;; For when someone manually calls :CHECK-PLAN
  (display-message-and-wait *ta-iostream*
    "Plan check result: ~{~S~^ ~}." result)
  (present-options *choices*))

(defmessage (:TA :PLAN-VIEW) (net)
  (pw-handle-view :PLAN net))

(defmessage (:TA :WORLD-VIEW) (node-id pv-pairs)
  (pw-handle-view :WORLD node-id pv-pairs))

(defmessage (:TA :AUTHORITY) (what value)
  (ecase what
    (:LEVEL
     (if (&numberp value)
	 (setq *plan-level-authority* value)
       (error "Illegal level authority: ~S." value))
     (present-options *choices*))))

(defmessage (:TA :OPTION) (&rest whatever)
  (display-message-and-wait *ta-iostream*
    "Received: ~S." (cons :option whatever))
  (present-options *choices*))


;;;; User commands

(defun ta-monitor ()
  (when (listen *ta-iostream*)
    (let* ((input (read *ta-iostream*))
	   (keyword (and (integerp input) (option input :to :keyword))))
      (cond ((member keyword *choices*)
	     (process-user-command keyword)
	     (present-options *choices*))
	    (t
	     (format *ta-iostream* "~%Please choose a number from ~A :- "
	       (mapcar #'(lambda (c) (option c :to :number))
		       *choices*)))))))

(defun process-user-command (keyword)
  (funcall (ipc-get-handler :ta-user keyword)))


;;; Initialise the planner to a null state.

(defmessage (:ta-user :init) ()
  (ipc-send-to-oplan :INIT)
  (setq *available-tasks* '())
  (setq *choices* '(:quit)))

;;; Specify the domain that the planner will work in.

(defmessage (:ta-user :input-tf) ()
  (let ((domain (get-tf-file-from-user)))
    (when domain
      (ipc-send-to-oplan :DOMAIN domain))))

;;; Specify the task to plan.

(defmessage (:ta-user :set-task) ()
  (let ((task (get-task-from-user)))
    (when task
      (ipc-send-to-oplan :SET-TASK task)
      (setq *task* (subseq task 5))		;minus the initial "task_"
      (setq *status* "planning ...")
      (setq *choices* '(:plan-view :world-view :quit)))))

;;; Add to the task.

(defmessage (:ta-user :add-to-task) ()
  (let ((addition (get-task-addition-from-user)))
    (when addition
      (apply #'ipc-send-to-oplan :ADD-TO-TASK addition)
      ;; /\/: Should probably change status, etc.
      )))

;;; View the current state of the plan.

(defmessage (:ta-user :plan-view) ()
  (ipc-send-to-oplan :GET :PLAN-VIEW))

;;; View the world at a particular point of the plan.

(defmessage (:ta-user :world-view) ()
  (let ((node-id (get-node-id-from-user)))
    (when node-id
      (ipc-send-to-oplan :GET :WORLD-VIEW node-id))))

;;; Replan.

(defmessage (:ta-user :replan) ()
  (setq *status* "replanning ...")
  (setq *choices* '(:plan-view :world-view :quit))
  (setq *execute-authority* :no)
  (ipc-send-to-oplan :REPLAN))

;;; Send the plan for execution.

(defmessage (:ta-user :execute) ()
  (setq *status* "executing ...")
  (setq *execute-authority* :yes)
  (ipc-send-to-oplan :EXECUTE))

;; Quit the planner.

(defmessage (:ta-user :quit) ()
  (when (really-quit-p)
    (ipc-send-to-oplan :KILL)
    (setq *status* "exiting ...")
    (setq *choices* '(:quit))
    ;; Stop running after this cycle.
    (terminate-pprocess)))


;;; Option lookup

(defun option (index &key (to :option))
  (let ((opt
	 (find index *choice-options*
	       :key (etypecase index (integer #'first) (keyword #'second)))))
    (ecase to
      (:number (first opt))
      (:keyword (second opt))
      (:description (third opt))
      (:option opt))))


;;; Present-options
;;;
;;; Active-choices holds a list of numbers of choices that are valid.
;;;

(defun present-options (&optional (active-choices *choices*))
  (clear-screen *ta-iostream*)
  (print-from-descriptions *ta-iostream* *status-lines*)
  (format *ta-iostream* "~%")
  (dolist (option *choice-options*)
    (destructuring-bind (number keyword description) option
      (format *ta-iostream* "~A ~D) ~A~%"
	      (if (member keyword active-choices) "*" " ")
	      number
	      description)))
  (format *ta-iostream* "~%Please choose a number:- ")
  (force-output *ta-iostream*))


;;; The micro-language used for status lines provides for lists of
;;; line descriptions.  Each line description is a list of <d-element>s.
;;; A <d-element> can be:
;;;
;;;   A string -- printed as is, w/o escape chars or quotes
;;;   A symbol -- treated as a variable; the symbol-value is
;;;               printed w/o escape chars or quotes.
;;;   A list   -- a control sequence.
;;;
;;; The only valid control sequence is:
;;;
;;;   (when <guard-variable> <d-element>*)
;;;      If the value of the guard-variable is true, the d-elements
;;;      are processed in order; otherwise the d-elements are ignored.
;;;

(defun print-from-descriptions (stream descriptions)
  (let ((*print-case* :downcase))
    (dolist (descr descriptions)
      (print-from-descr stream descr)
      (terpri stream))))

(defun print-from-descr (stream descr)
  (dolist (d descr)
    (etypecase d
      (string (princ d stream))
      (symbol (princ (symbol-value d) stream))
      (list
       (ecase (car d)
	 ((when)
	  (let ((guard-var (cadr d))
		(sub-descr (cddr d)))
	    (when (symbol-value guard-var)
	      (print-from-descr stream sub-descr)))))))))


;;;; Menu requests.

;;; All menus have an escape entry (usually "QUIT") in case the user
;;; wants to get rid of the menu without making a selection.  When we
;;; ask the user a question by displaying the question in the TA window
;;; and calling read-line, the user can escape by typing return (thus
;;; producing a null string).

;;; Ask-user is useful when implementing the latter convention.

(defun ask-user (question &rest format-args) ; -> string or nil
  (clear-screen *ta-iostream*)
  (apply #'ask-ta-user question format-args))

(defun ask-ta-user (question &rest format-args)
  (format *ta-iostream* "~&~%~?" question format-args)
  (let ((answer (read-line *ta-iostream*)))
    (if (string= answer "")
	nil
      answer)))

(defun tell-ta-user (format-string &rest format-args)
  (format *ta-iostream* "~&~?~%" format-string format-args))


;;; Get TF file.

;;; The TF directory starts as $OPLANTFDIR but can be changed.
;;; The Planner will use $OPLANTFDIR as a pathname default, so
;;; we can send it a simple name (as a pathname) if that's still
;;; the current directory.  Otherwise, we send a full pathname.

;;; Note that the :cd option takes the directory name relative
;;; the current directory of the O-Plan process (ie, the directory
;;; it was started in from the shell) while :enter takes the file
;;; name relative to the current TF directory.

;;; We call DIRECTORY each time we're about to put up the menu,
;;; because a new TF file might have been added since the last
;;; time we looked.

;;; /\/: However, if we've just changed TF dir, we'll do two in a
;;; row when we could get away with one.  See change-tf-directory.

(defvar *current-tf-dir* nil)

(defun get-tf-file-from-user () ; -> pathname or nil
  (when (null *current-tf-dir*)
    (setq *current-tf-dir* (concat-string (get-parameter :oplan-tf-dir) "/")))
  (let ((tf-paths nil)
	(tf-names nil)
	(name nil))
    (while (null name)
      (setq tf-paths (find-all-tf-files *current-tf-dir*))
      (setq tf-names (sort (mapcar #'pathname-name tf-paths) #'string<))
      (unless tf-names
	(display-message-and-wait *ta-iostream*
	    "No domain files found in:~%~A~%" *current-tf-dir*))
      (let ((domain
	     (big-menu-request
	       `("-heading" ,(if tf-names "Domains to choose from:" "Options:")
		 ,@tf-names
		 "-line"
		 "Change directory=:cd"
		 "Enter filename=:enter"
		 "-line"
		 "QUIT=:quit")
	       :read-function #'read-line)))
	(cond ((equal domain ":quit")
	       (return-from get-tf-file-from-user nil))
	      ((equal domain ":enter")
	       (setq name (enter-tf-file-name)))
	      ((equal domain ":cd")
	       (change-tf-directory))
	      ;; If we get this far, one of the tf-names was chosen 
	      ;; rather than some option such as :cd or :quit.
	      ((equal (pathname *current-tf-dir*)
		      (pathname
		       (concat-string (get-parameter :oplan-tf-dir) "/")))
	       ;; We're in the default TF dir, so use only the name.
	       (setq name (make-pathname :name domain)))
	      (t
	       ;; We're not in the default TF dir, so use a full pathname.
	       (setq name
		     (find domain tf-paths :key #'pathname-name
			                   :test #'string=))
	       (assert (not (null name)))))))
    name))

(let ((saved-dir nil)
      (saved-write-date nil)
      (saved-result nil))
  (defun find-all-tf-files (dir)
    (let ((write-date (file-write-date dir)))
      (cond ((and (equal dir saved-dir)
		  (equal write-date saved-write-date))
	     saved-result)
	    (t
	     (setq saved-dir dir
		   saved-write-date write-date
		   saved-result (find-all-files dir "tf")))))))

(defun enter-tf-file-name () ; -> pathname or nil
  (let ((tf (ask-user "Enter name of TF file: ")))
    (when (null tf)
      (return-from enter-tf-file-name nil))
    (setq tf (merge-pathnames
	       (merge-pathnames tf (make-pathname :type "tf"))
	       *current-tf-dir*))
    (cond ((probe-file tf) tf)
	  (t (display-message-and-wait *ta-iostream*
	        "There is no file ~S." tf)
	     nil))))

(defvar *old-tf-dirs* '())

(defun change-tf-directory () ; works via side effects
  (let ((dir (get-tf-dir-from-user)))
    (when (null dir)
      (return-from change-tf-directory))
    (unless (eql #\/ (char dir (1- (length dir))))
      (setq dir (concat-string dir "/")))
    (cond ((not (probe-file dir))
	   (display-message-and-wait *ta-iostream*
	     "There is no ~S." dir))
	  ((null (find-all-tf-files dir))
	   (display-message-and-wait *ta-iostream*
	     "There are no TF files in ~S." dir))
	  (t
	   ;; Looks OK.
	   (setq *old-tf-dirs* (remove dir *old-tf-dirs* :test #'equal))
	   (push *current-tf-dir* *old-tf-dirs*)
	   (setq *current-tf-dir* dir)))))

(defun get-tf-dir-from-user () ; -> string or nil
  (if (null *old-tf-dirs*)
      (ask-user "Enter directory name: ")
    (let ((dir (menu-request
		 `("-heading" "Directories to choose from:"
		   ,@*old-tf-dirs*
		   "-line"
		   "Enter directory name=:enter"
		   "-line"
		   "QUIT=:quit")
		 :read-function #'read-line)))
      (cond ((equal dir ":quit")
	     nil)
	    ((equal dir ":enter")
	     (ask-user "Enter directory name: "))
	    (t
	     dir)))))


;;; Get task

;;; We return the name of a task schema as a string, or nil if the
;;; user quits.

(defun get-task-from-user () ; -> string or nil
  (assert (not (null *available-tasks*)))
  (let ((task (string-downcase
	       (menu-request
		 `("-heading" "Task to choose from:"
		   ,@*available-tasks*
		   "-line"
		   "QUIT")
		 :read-function #'read-line))))
    (if (string-equal task "quit")
	nil
      task)))

(defun add-to-available-tasks (more-tasks)
  ;; /\/: Should use replacing-merge from the TF compiler.
  (setq *available-tasks*
	(append (set-difference *available-tasks* more-tasks :test #'equal)
		more-tasks)))


;;; For "Add to Task"

(defun get-task-addition-from-user ()
  (ecase (task-addition-menu-choice)
    (:action
     (loop
       (let ((pattern (ask-user-for-action-pattern)))
	 (when (null pattern)
	   (return nil))
	 (tell-ta-user "Action: ~(~S~)" pattern)
	 (when (ask-if *ta-iostream* "y" "Ok?")
	   (return
	     `(:action ,pattern))))))
    (:quit
     nil)))

(defun task-addition-menu-choice ()
  (menu-request
    '("-heading" "Add to Task"
      "Add an Action=:action"
      "-line"
      "Quit=:quit")))

(defun ask-user-for-action-pattern ()
  ;; Try to make sure we get something that looks like a pattern
  (clear-screen *ta-iostream*)
  (loop
    (let ((line  (ask-ta-user "Action pattern: ")))
      (when (null line)
	(return nil))
      (handler-case (values (read-from-string line))
        (error (c)
	  (tell-ta-user "Error: ~A" c)
	  (tell-ta-user "Try again or enter a blank line to quit.~%"))
	(:no-error (pattern)
	  (if (and (consp pattern)
		   (symbolp (car pattern)))
	      (return pattern)
	    (tell-ta-user
	      "The pattern must be a list that begins with a symbol.")))))))


;;; Get node name for world view

;;; The user should type a node number, such as "3-9" for node-3-9.

(defun get-node-id-from-user () ; -> string or nil
  (ask-user "  View at end_of node-"))


;;; Quit?

(defconstant quit-choices '("-heading" "Quit" "Yes" "-line" "No"))

(defun really-quit-p ()
  (equal (menu-request quit-choices :read-function #'read-line)
	 "Yes"))


;;; Other utilities

(defun display-message-and-wait (stream msg &rest args)
  (clear-screen stream)
  (apply #'format stream msg args)
  (format stream "~%~%~%~%~%Hit <return> to continue")
  (read-line stream))

(defun not-implemented-yet (stream)
  (display-message-and-wait stream "~%Not implemented yet~%"))


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
