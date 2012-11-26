;;;; File: am-toplevel.lisp
;;; Contains: Top-level processing for the AM.
;;; Author: Jeff Dalton and Richard Kirby
;;; Created: Sun Apr 15 20:21:56 1990
;;; Updated: Mon May 31 00:57:43 1999 by Jeff Dalton
;;; Copyright: (c) 1992, 1993, 1994, AIAI, University of Edinburgh

;;; /\/: There should be a mechanism for keeping track of statistics,
;;; clearing them, etc.

;;; /\/: Some things give the impression that we can handle multiple
;;; KPs, but in fact lots of things won't work if we try.

(in-package :oplan)

(defvar *am-single-step* nil)	;true for single-step mode

(defvar *cycle-count* 0)	;number of AEs processed; never reset

(defvar *cycle-count-base*	;for the elapsed-cycles statatistic
  *cycle-count*)		;  reset on init or replan

(defvar *cycle-count-limit* 500)
(defvar *cycle-count-barrier*	;reset on init or replan
  (+ *cycle-count* *cycle-count-limit*))

(defparameter *agenda-slice* nil)
(defvar *slice-remaining* *agenda-slice*)

(defvar *n-alts-chosen* 0)	;statistic
(defvar *n-poisons* 0)		;statistic

(defvar *ae-being-processed* nil)
(defvar *ready-kps* nil)

(definit :am *planning* nil)	;controls the :waiting message


;;;; Startup and event-handler

(defun am-startup ()

  ;; This is the initial function for the AM process.  After startup,
  ;; it is not called again.  Instead, am-event-handler is called
  ;; whenever a message arrives.

  (is-init)
  ;; Make breaks, etc. go to the debug window.
  (x-redirect-errors :amout)
  (clear-screen *debug-io*)
  ;; Register with IM.
  (ipc-register-with-im)
  (ipc-set-event-handler 'am-event-handler))

(defmessage (:AM :INIT) ()
  (clear-screen *debug-io*)
  (call-initializers :AM)
  (initialize-variable-group :AM)
  ;; /\/: Make the following part of the :AM variable group
  (setq *slice-remaining* *agenda-slice*)
  (setq *cycle-count-base* *cycle-count*
	*cycle-count-barrier* (+ *cycle-count* *cycle-count-limit*)
	*n-alts-chosen* 0
	*n-poisons* 0)
  (dev-debug :information "Initialised")
  :ok)

;;; We can't swich alts when the KP is in the middle of processing an
;;; agenda entry, because the KS might send the AM some messages that
;;; make sense only in the current alt.  (E.g. it might ask the AM to
;;; add an agenda entry.)  So we consider switching only when (kp-ready-p)
;;; is true.  Since there's only one KP this is sufficient.

;;; /\/: The cycle-count probably should be incremented only when
;;; taking from the plan agenda, but it should still be tested before
;;; the single-step handler is called (so the user doesn't decide what
;;; to do without knowing that the limit's been reached).

(defun am-event-handler (self)
  (while (next-event-p self)
    (ipc-handle-message self (next-event self)))
  (when (kp-ready-p)
    (when (and *slice-remaining*
	       (<= *slice-remaining* 0)
	       ;; Don't switch if a poison ae will be coming along.
	       (not (db-request :context-was-poisoned)))
      (maybe-switch-alternatives))
    (if (or (plan-agenda-p)
	    (agent-agenda-p))
	(process-an-agenda-entry)
      ;; Maybe we're stuck
      (when *planning*
        (let ((why (db-request :why-are-we-waiting)))
	  (when why
	    (setq *planning* nil)
	    (dev-debug :warning "Waiting for ~{~A~^, ~}" why)
	    (ipc-send-out :waiting why)))))))

(defun process-an-agenda-entry ()
  (incf *cycle-count*)
  (when (> *cycle-count* *cycle-count-barrier*)
    (handle-cycle-count-barrier))
  (when *am-single-step*
    (am-single-step-handler))
  (let ((ae (get-agenda-entry-to-process)))
    (setq *ae-being-processed* ae)
    (dev-debug :minimal "Cycle ~S: Processing ae-~S/~S,~(~A~)"
	       *cycle-count* (ag-id ae) (ag-stage ae) (ag-level ae))
    (if (ipc-exists-handler :agent (ag-type ae))
	(process-agenda-entry-in-controller ae)
      (send-agenda-entry-to-waiting-kp ae))))

(defun process-agenda-entry-in-controller (ae)
  ;; Note that we do this only when a KP is ready, even though
  ;; we don't use the KP.
  (dev-debug :trace "ae-~W/~W,~A: ~W" 
	     (ag-id ae) (ag-stage ae) (ag-level ae) (ag-body ae))
  (apply (ipc-get-handler :agent (ag-type ae)) (ag-args ae))
  ;; Get the AM to run again now that we're done.
  (ipc-send :am :amready))

(defun send-agenda-entry-to-waiting-kp (ae)
  (let ((a-kp (pop *ready-kps*)))
    ;; a-kp is something like :KP-1
    (when (kp-ready-p)
      (error "Can't handle > 1 ready KPs."))
    ;; The KP will send back a :kpready message when it's finished.
    (ipc-write a-kp
       `(:AGENDA ,ae))))


;;;; Taking too long

(defun handle-cycle-count-barrier ()
  (let ((option (if (get-parameter :interactive)
		    (ask-user-about-barrier)
		  :error)))
    (ecase option
      (:continue
       (incf *cycle-count-barrier* *cycle-count-limit*))
      (:unlimit
       (setq *cycle-count-barrier* most-positive-fixnum)) ;good enough /\/
      (:quit
       ;; Pretend we ran out of alternatives after a poison.
       ;; We need at least 2 cycles (no-more-alts and the init)
       ;; to get out.
       (incf *cycle-count-barrier* 10)
       (atm-flush-alternatives)
       (pick-an-alternative '(:cycle-count-limit-exceeded)))
      (:error
       ;; /\/: In subr more, it should be possible to recover.
       ;; /\/: Maybe in connect mode too.
       (error "Cycle count limit ~S exceeded."
	      *cycle-count-limit*)))))

(defun ask-user-about-barrier ()
  (menu-request
    `("-heading" ,(format nil "Cycle count limit ~S exceeded."
		              *cycle-count-limit*)
      ,(format nil "Allow another ~S cycles=:continue" *cycle-count-limit*)
      "Remove limit=:unlimit"
      "Give up=:quit")))


;;;; AM message handlers

(defmessage (:am :finished-planning) (status)
  (ecase status
    ((:finished :no-more-alternatives)
     (setq *planning* nil))))

(defmessage (:am :replanning) ()
  (setq *cycle-count-base* *cycle-count*
	*cycle-count-barrier* (+ *cycle-count* *cycle-count-limit*)
	*n-alts-chosen* 0
	*n-poisons* 0))

(defmessage (:am :elapsed-cycles) ()
  (- *cycle-count* *cycle-count-base*))

(defmessage (:am :get-plan-statistics) ()
  `((:am-cycles         .  ,(- *cycle-count* *cycle-count-base*))
    (:n-alts-chosen     .  ,*n-alts-chosen*)
    (:n-poisons         .  ,*n-poisons*)
    (:n-alts-remaining  .  ,(n-alts-remaining)) ))

(defun n-alts-remaining ()
  (atm-get-number-of-alternatives))

;;; :kpready

;;; When the agenda entry was taken from the _agent_ agenda, we don't
;;; want to tell the DM to remove it from the triggered aganda table,
;;; because it isn't in the table.  The DM can handle this, but it
;;; still shouldn't happen.  That's one reason why agent agenda 
;;; entries have a different type: event rather than agenda-entry.

(defmessage (:am :kpready) (kp)
  (when *ae-being-processed*
    (unless (event-p *ae-being-processed*)
      (db-request :PROCESSED-AGENDA-ENTRY (ag-id *ae-being-processed*)))
    (request-current-agenda)
    (setq *ae-being-processed* nil))
  (pushnew kp *ready-kps*))

(defun kp-ready-p ()
  *ready-kps*)

(defmessage (:am :amready) ()
  (assert *ae-being-processed*)
  (assert (event-p *ae-being-processed*))
  (setq *ae-being-processed* nil)
  (request-current-agenda))


;;;; Agent messages


;;; Agenda counters

(defmessage (:agent :reset-agenda-counters) ()
  (setq *cycle-count-base* *cycle-count*
	*cycle-count-barrier* (+ *cycle-count* *cycle-count-limit*)
	*n-alts-chosen* 0
	*n-poisons* 0)
  (ipc-send-out :ok))


;;; Planner "settings"

(defmessage (:agent :set-parameters) (&rest assignments)
  (handler-case (progn (walk-plist #'set-setting assignments)
		       (ipc-send-out :ok))
    (error (e)
      (invalid-command "set-parameters: ~A" e))))

(defmessage (:agent :get-parameters) (&rest names)
  (handler-case (mapcan #'(lambda (name) (list name (get-setting name)))
		       names)
    (error (e)
      (invalid-command "get-parameters: ~A" e))
    (:no-error (values)
      (ipc-send-out :values values))))

(defun set-setting (name value)
  (declare (special *schema-selection-mode* *psv-binding-mode*))
  ;; Mostly special cases, I'm afraid.
  (ecase name
    (:schema-selection-mode		; /\/: Should send to the KPs
     (ecase value
       ((:auto :ask)
	(setq *schema-selection-mode* value))))
    (:psv-binding-mode			; /\/: Should send to the KPs
     (ecase value
       ((:auto :ask)
	(setq *psv-binding-mode* value))))))

(defun get-setting (name)
  (declare (special *schema-selection-mode* *psv-binding-mode*))
  ;; Mostly special cases, I'm afraid.
  (ecase name
    (:schema-selection-mode *schema-selection-mode*)
    (:psv-binding-mode      *psv-binding-mode*)))


;;; Authority

(defmessage (:agent :authority) (type value)
  (case type
    (:level
     (cond ((&numberp value)
	    (db-request :authority :level value)
	    (ipc-send-out :authority :level value))	;confirm
	   (t
	    (harmless-error "Illegal level authority: ~S." value))))
    (otherwise
     (harmless-error "Illegal authority type: ~S." type))))

(defmessage (:agent :what-authority) (type)
  (case type
    (:level
     (ipc-send-out :authority :level (db-request :what-authority :level)))
    (otherwise
     (harmless-error "Illegal authority type: ~S." type))))


;;; For getting the status after an authority or option message.

(defmessage (:agent :status-after) (message)
  (unless (and (consp message)
	       (keywordp (car message)))
    (invalid-command "Submessage ~S is not a list beginning with a keyword."
		     message))
  (let ((handler (ipc-get-handler :agent (car message))))
    (unless handler
      (invalid-command "Can't handle :status-after submessage ~S." message))
    ;; Process the submessage
    (apply handler (cdr message))
    ;; Send a :status message.
    (funcall (ipc-get-handler :agent :status))))


;;; Plain status request

(defmessage (:agent :status) ()
  (let ((agenda-status (db-request :agenda-status)))
    (ipc-send-out :status
      (append
        (when (agent-agenda-p)
	  '(:handling-events))
	(cond ((member ':triggered agenda-status)
	       '(:planning))
	      (agenda-status
	       `(:waiting ,(db-request :why-are-we-waiting)))
	      (t
	       '()))))))


;;; TA-questions

;;; First, a KS sends us a question.  We get things set up in the DM
;;; and then send the question out to the TA.

;;; N.B. The KS should not do anything interesting after sending
;;; this message, because anything that affects the agendas will
;;; probably not work correctly.

(defmessage (:am :question) (question-kwd agenda-entry data)

  ;; Enforce the convention that the data begin with :question.
  (assert (list-beginning :question data))

  ;; Put us into a state in which no planning will happen until
  ;; the question is answered.
  (db-request :ta-question question-kwd agenda-entry data)

  ;; Arrange for something to happen when it is answered, by posting
  ;; a :question agenda-entry.
  (post-question question-kwd agenda-entry data)

  ;; And we'll need one of these.
  (db-request :add-agenda-entry
    (assign-agenda-defaults
      (make-agenda-entry :body '(:planner-finished)
			 :trigger '(:empty))))

  (if (get-parameter :test-ta-questions)
      ;; Instantly answer -- assumes we can just send the data back,
      ;; but with :question removed from the front.
      (progn
	(xp-format t "~&Automatic answer to ~W:~%~3T~:W~%"
		   question-kwd (ag-body agenda-entry))
	(db-request :ta-question-answer question-kwd (cdr data)))
    ;; Send the question to the TA.
    ;; Note that we 1st take :question off the front.
    (ipc-send-out :question question-kwd
		  (ag-body agenda-entry)
		  (cdr data))))

(defun post-question (question-kwd agenda-entry data)
  (let ((q (make-agenda-entry
	     :body `(:question ,question-kwd)
	     :trigger `(:answer ,question-kwd)
	     :info (list agenda-entry data)))) ;why not?
    (assign-agenda-defaults q)
    (db-request :ADD-AGENDA-ENTRY q)))

#+:undef ; simple test version that instantly answers
(defmessage (:am :question) (question-kwd agenda-entry data)
  (assert (list-beginning :question data))
  (setf (car data) :answer-to-question)
  ;; /\/: Don't assign agenda defaults, because it already has an id, etc.
  ; (funcall (ipc-get-handler :am :agenda) agenda-entry)
  (db-request :ADD-AGENDA-ENTRY agenda-entry))


;;; Evantually, we may get an answer back from the TA.

;;; The TA should not be expecting an immediate reply to its answer
;;; but will eventually get something if planning starts up again.

(defmessage (:agent :answer) (question-kwd data)
  (db-request :ta-question-answer question-kwd data))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;

