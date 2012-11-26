;;;; File: am-services.lsp
;;; Contains: General code for the AM.
;;; Author: Jeff Dalton and Richard Kirby (rbk)
;;; Created: Fri Nov  2 13:37:42 1990
;;; Updated: Sun Sep  5 00:02:39 1999 by Jeff Dalton
;;; Copyright: (c) 1992, 1993, 1994, AIAI, University of Edinburgh

(in-package :oplan)

;; :SINGLE-STEP handler, which presents a menu to the user for interrogating
;; the agenda tables, and for subverting the agenda orderings.

(defmessage (:AM :SINGLE-STEP) (value)
  (setq *am-single-step* value))

(defun am-single-step-handler ()
  (sort-plan-agenda)
  (loop
    (ecase (am-single-step-menu-choice)
      (:triggered
       (do-display-agenda *debug-io*)
       (terpri *debug-io*))
      (:waiting
       (do-display-waiting-agenda *debug-io*)
       (terpri *debug-io*))
      (:untriggered
       (do-display-untriggered-agenda *debug-io*)
       (terpri *debug-io*))
      (:alt
       (do-display-alt-agenda *debug-io*)
       (terpri *debug-io*))
      (:top
       (return))
      (:any
       (when (do-process-any-agenda-entry *debug-io*)
	 (return)))
      (:user
       (let ((ae (make-agenda-entry :trigger t :body '(:user :user-request))))
	 (assign-event-defaults ae)
	 (add-to-agent-agenda ae)))
      (:break
       (break "Return to continue with AM Interrogation."))
      (:quit
       (setq *am-single-step* nil)
       (return)))))

(defun am-single-step-menu-choice ()
  (menu-request
    `("-heading"
      ,(format nil "AM Single Step in cycle ~A" *cycle-count*)
      "Display the Agenda Table=:triggered"
      "Display the Waiting Agenda Table=:waiting"
      "Display the Untriggered Agenda Table=:untriggered"
      "Display the Alternatives Agenda Table=:alt"
      "Process the top agenda entry=:top"
      "Process any agenda entry=:any"
      "Schedule KS-USER=:user"
      "BREAK IN=:break"
      "-line"
      "Quit single step mode=:quit")))

(defun do-display-agenda (stream)
  (do-display-agent-agenda stream)
  (do-display-triggered-agenda stream))

(defun do-display-agent-agenda (stream)
  (when (agent-agenda-p)
    ;; /\/: Priorities?
    (format stream "~%The Agent Agenda:~%")
    (let ((*print-case* :downcase)
	  (*print-pretty* t)
	  (*print-level* 4)
	  (*print-length* 4))
      (dolist (ae (agent-agenda-entries))
	(do-display-agenda-entry ae stream)))))

(defun do-display-triggered-agenda (stream)
  (format stream "~%The Agenda Table:~%")
  (let ((*print-case* :downcase)
	(*print-pretty* t)
	(*print-level* 4)
	(*print-length* 4))
    (dolist (ae *agenda*)
      (do-display-agenda-entry ae stream))))

(defun do-display-waiting-agenda (stream)
  (format stream "~%The Waiting Agenda Table:~%")
  (let ((*print-case* :downcase)
	(*print-pretty* t)
	(*print-level* 4)
	(*print-length* 4))
    (dolist (ae (db-request :REQUEST-WAITING-AGENDA))
      (do-display-agenda-entry ae stream))))

(defun do-display-untriggered-agenda (stream)
  (format stream "~%The Untriggered Agenda Table:~%")
  (let ((*print-case* :downcase)
	(*print-pretty* t)
	(*print-level* 4)
	(*print-length* 4))
    (dolist (ae (db-request :REQUEST-UNTRIGGERED-AGENDA))
      ;; /\/: What about priorities?  OK as-is?
      (do-display-agenda-entry ae stream))))

(defun do-display-agenda-entry (ae stream)
  (xp-format stream
      "~&~:@(AE-~A~)/~A: ~
       ~:I~:_Priority ~A, ~:_Level ~A, ~:_Branch-1 ~A, ~:_Branch-n ~A, ~
          ~:_Cost ~A, ~:_Trigger ~A: ~:_~A, ~:_Info ~A~%"
      (ag-id ae)
      (ag-stage ae)
      (ag-priority ae)
      (ag-level ae)
      (ag-branch-1 ae)
      (ag-branch-n ae)
      (ag-cost ae)
      (ag-trigger ae)
      (ag-body ae)
      (ag-info ae)))

(defun do-process-any-agenda-entry (io)
  ;; /\/: We lack a mechanism for putting a plan ae ahead of the agent
  ;; agenda, or even to take a particular agent ae.
  (when (agent-agenda-p)
    (tell-am-user io "Can't when the agent agenda is not empty.")
    (return-from do-process-any-agenda-entry nil))
  (let* ((n (ask-am-user io "AE-"))
	 (id n)				; was (concat-name "AE-" n)
	 (ae (and (typep id 'fixnum)
		  (find-agenda-entry id *agenda*))))
    (cond (ae
	   (setq *agenda*
		 (cons ae
		       (remove-1-eq ae *agenda*)))
	   t)
	  (t
	   (tell-am-user io "No ~A" id)
	   nil))))
    
(defun ask-am-user (io question &rest format-args)
  ;; /\/ There's a similar routine with the same name in the TA.
  ;; /\/ And others, in other places.
  (format io "~&~?" question format-args)
  (read-line io))

(defun tell-am-user (io message &rest format-args)
  (format io "~&~?~%" message format-args))


;;; Agenda priorities

(defparameter *agenda-priorities*
  '((:KILLJOY              . 10000)
    (:KILL                 . 1000)	;new for 2.0 /\/
    (:INIT                 . 100)
    (:QUESTION             . 90)	;new in 3.2
    (:SET-TASK             . 80)
    (:USER                 . 70)
    (:GET                  . 70)
    (:EXTRACT              . 70)
    (:POISON-STATE         . 65)
    (:NO-MORE-ALTERNATIVES . 65)	;doesn't _need_ high priority
    (:EXPAND-TASK          . 60)
    (:ADD-TO-TASK          . 60)
    (:OR                   . 55)
    (:EXPAND               . 50)
    (:ACHIEVE              . 40)
    (:FIX                  . 40)
    (:CONDITION            . 30)
    (:COMPUTE              . 30)	;same as :CONDITION
    (:BIND                 . 20)))

(defparameter *default-agenda-priority* 10)

(defun-inline get-agenda-priority (agenda)
  (or (cdr (assoc (car (ag-body agenda)) *agenda-priorities*))
      *default-agenda-priority*))

#| Future note:
For CONDITION agenda entries, probably want to rate unsupervised tieups higher
than only_use_for_query tieups.
|#

(proclaim '(notinline assign-agenda-priority))

(defun assign-agenda-priority (ag)
  (let ((priority (get-agenda-priority ag)))
    (if (ag-priority ag)
	(assert (= (ag-priority ag) priority))
      (setf (ag-priority ag)
	    priority))))

(definit :am *agenda-id-counter* 0)

(defun assign-agenda-id (ag)
  (unless (ag-id ag)
    (setf (ag-id ag) (incf *agenda-id-counter*)
	             #+:undef (pop-gensym "AE-"))))

(defun assign-agenda-defaults (ag)
  (macrolet ((default ((accessor item) value) ;could use ensuref instead /\/
	       `(unless (,accessor ,item)
		  (setf (,accessor ,item) ,value))))
    (assert (null (ag-id ag)))
    (assert (null (ag-priority ag)))
    (assign-agenda-id ag)
    (assign-agenda-priority ag)
    (assert (implies (numberp (ag-branch-1 ag))
		     (member (car (ag-body ag)) '(:or :bind :poison))))
    (case (ag-type ag)
      (:poison-state
       (default (ag-trigger  ag) t)
       (default (ag-branch-1 ag) 1)
       (default (ag-branch-n ag) 1)
       (default (ag-level    ag) -1))
      (t
       (default (ag-trigger  ag) t)
       (default (ag-branch-1 ag) :inf)	;/\/
       (default (ag-branch-n ag) :inf)	;/\/
       (default (ag-level    ag) (&+ 0 (current-agenda-level))))) ;was 1+ /\/
    ag))

(defun assign-event-defaults (ag)
  ;; /\/: id and priority not always null because KS-USER can send its
  ;;      agenda-entry back to the AM.
  ;(assert (null (ag-id ag)))
  ;(assert (null (ag-priority ag)))
  (assign-agenda-id ag)
  (assign-agenda-priority ag)
  (macrolet ((default ((accessor item) value)
	       `(unless (,accessor ,item)
		  (setf (,accessor ,item) ,value))))
    (default (ag-trigger  ag) t)
    (default (ag-branch-1 ag) 1)
    (default (ag-branch-n ag) 1)
    (default (ag-level    ag) -1)
    ag))

(defun assign-alt-agenda-defaults (ag)
  (assign-agenda-id ag)
  (assign-agenda-priority ag)
  (macrolet ((default ((accessor item) value)
	       `(unless (,accessor ,item)
		  (setf (,accessor ,item) ,value))))
    ;(default (ag-trigger  ag) t)
    (default (ag-branch-1 ag) :inf)	;/\/
    (default (ag-branch-n ag) :inf)	;/\/
    (default (ag-level    ag) (&+ 0 (current-agenda-level))) ;was 1+ /\/
    ag))

(defun check-agenda-entry (ag)
  ; (assert (and (symbolp (ag-id ag)) (not (null (ag-id ag)))))
  (assert (typep (ag-id ag) 'fixnum))
  (assert (not (null (ag-trigger ag))))
  (assert (numberp (ag-priority ag)))
  (assert (and (consp (ag-body ag)) (symbolp (car (ag-body ag)))))
  (assert (or (numberp (ag-level ag))
	      (and (member (ag-type ag) '(:or :bind :user))
		   (infp (ag-level ag)))))
  (assert (&numberp (ag-branch-1 ag)))
  (assert (&numberp (ag-branch-n ag)))
  ag)

(defun current-agenda-level ()
  (let ((ae *ae-being-processed*))
    (if ae
	(or (ag-level ae) -1)
      -1)))
  
#+:lose
(defun calculate-agenda-entries-priorities ()
  (mapc #'assign-priority *agenda*)
  (setq *agenda*
	(stable-sort *agenda*
		     #'(lambda (x y)
			 ;; /\/: Not a <: 2nd compare s.b. only if = priority
			 (or (> (ag-priority x) (ag-priority y))
			     (< (read-from-string (symbol-name (ag-id x))
						  nil nil :start 3)
				(read-from-string (symbol-name (ag-id y))
						  nil nil :start 3)))))))

;;; /\/: If agenda entries have uniquely numbered ids, stable-sort
;;; shouldn't be necessary.  Also, we shouldn't have to call read-
;;; from-string each time!  So...

#+:undef
(defun request-current-agenda ()
  (ipc-with-own-copy (table (db-request :REQUEST-CURRENT-AGENDA)
			    (copy-list table))
    (mapc #'check-agenda-entry table)
    (setq *agenda*
	  (sort table #'ag>))))

(defun request-current-agenda ()
  (let ((table (db-request :REQUEST-CURRENT-AGENDA)))
    (mapc #'check-agenda-entry table)
    (setq *agenda* table)
    (put-plan-agenda-best-first)
    *agenda*))

(defun sort-plan-agenda ()
  ;; We don't do ipc-with-own-copy when we get the agenda, so we
  ;; have to copy it if we want it sorted.
  (setq *agenda* (sort (copy-list *agenda*) #'ag>)))

(defun put-plan-agenda-best-first ()
  ;; Must be called before first- or take-from-plan-agenda.
  ;; /\/: We used to sort the agenda.  Doing this is faster, but
  ;; it needs to look a bit like the old way of doing things.
  (when *agenda*
    (let ((best (find-best *agenda* #'ag>)))
      (unless (eq (car *agenda*) best)
	(setq *agenda* (cons best (remove-1-eq best *agenda*)))))))

#+:undef
(defun ag> (ag1 ag2)
  (or (> (ag-priority ag1) (ag-priority ag2))
      (and (= (ag-priority ag1) (ag-priority ag2))
	   (< (ag-id-number ag1) (ag-id-number ag2)))))

(defun ag> (a b)			;is a better than b?
  ;; For each criterion:
  ;;   if a is better, return true;
  ;;   if b is better, return false;
  ;;   otherwise (they're equal), go on the the next criterion.
  (let ((twa (total-winner-p a))
	(twb (total-winner-p b)))
    (cond
      ;; These guys are way cool.
      ((and twa (not twb))		;a is better
       t)
      ((and twb (not twa))		;b is better
       nil)
      ;; Priority next, at least to ensure :poisons win.
      ;; Also, [unsupervised] :conditions need to be late
      ;; or we fail to find some solutions.  (Weak triggers?)
      ((fix> (ag-priority a) (ag-priority b))
       t)
      ((fix> (ag-priority b) (ag-priority a))
       nil)
      ;; Older is better.
      (t
       (fix< (ag-id-number a) (ag-id-number b))))))

(defun total-winner-p (a)
  (and (&= 1 (ag-branch-1 a))
       (not (and (eq (ag-type a) :or)
		 (or-is-reachieve-p a)))))

(defun ag-id-number (ag)
  ;; /\/: Agenda ids are now numbers
  (ag-id ag)
  #+:undef
  (or (get (ag-id ag) 'ag-id-number)
      (setf (get (ag-id ag) 'ag-id-number)
	    (read-from-string (symbol-name (ag-id ag))
			      nil nil :start 3))))


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;

