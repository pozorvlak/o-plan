;;;; File: program-interface.lisp
;;; Contains: Programmatic interface to single-process O-Plan
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: October 1994
;;; Updated: Sat Jan 25 02:40:30 1997 by Jeff Dalton
;;; Copyright: (c) 1994, AIAI, University of Edinburgh

(in-package :oplan)


;;;; The programmatic interface is used in subr (subroutine) mode.

(defun pi-check-state (checking-for)
  (unless (and (pprocess-p *pprocess*)
	       (eq (pprocess-name *pprocess*) :user))
    (error "~A called when *pprocess* was not :user" checking-for)))


;;;; High-level interface

;;; Planning and replanning

(defun plan-for (domain-name task-name)
  (send-to-oplan :init)
  (receive-else-error '(:init-ok))
  (send-to-oplan :domain domain-name)
  (receive-else-error '(:domain ($tf-file-name . $task-names)))
  (send-to-oplan :set-task task-name)
  (receive-plan-status))

(defun replan ()
  (send-to-oplan :replan)
  (receive-plan-status))

(defun receive-plan-status ()
  (receive-else-error '($status)
    #'(lambda (status)
	(ecase status
	  (:finished t)
	  (:no-more-alternatives nil)))))


;;; Generating a series of plans by replanning

(defun generate-plans (domain-name task-name
		       &optional (how-many :all)
		                 (thunk #'(lambda () nil)))
  ;; How-many is the number of plans to get, not the number
  ;; of times to replan.
  (check-type how-many (or integer (member :all)))
  (if (eq how-many :all)
      (when (plan-for domain-name task-name)
	(loop
	  (funcall thunk)
	  (unless (replan)
	    (return t))))
    (when (and (> how-many 0) (plan-for domain-name task-name))
      (loop
        (funcall thunk)
	(decf how-many)
	(unless (and (> how-many 0) (replan))
	  (return t))))))

(defun generate-plans-as-options (domain-name task-name
				  &optional (how-many :all))
  (let ((options '()))
    (generate-plans domain-name task-name how-many
      #'(lambda ()
	  (send-to-oplan :push-option)
	  (receive-else-error '(:option $name)
	    #'(lambda (name)
		(push name options)
		(send-to-oplan :pop-option)
		(receive-else-error '(:option $parent-name))))))
    (nreverse options)))


;;; Planning by levels

(defun plan-by-levels (domain-name task-name)
  (send-to-oplan :init)
  (receive-else-error '(:init-ok))
  (send-to-oplan :domain domain-name)
  (receive-else-error '(:domain ($tf-file-name . $task-names)))
  (let ((max-level (max-value #'alist-value (request-action-level-alist))))
    (set-authority :level -1)
    (send-to-oplan :set-task task-name)
    (receive-else-error :nothing)
    (loop for level from 0 to max-level do
      (set-authority :level level)
      (pprocess-main-loop)		;a chance to use the new authority
      (cond ((receive-from-oplan '(:finished))
	     (return-from plan-by-levels t))
	    ((receive-from-oplan '(:no-more-alternatives))
	     (return-from plan-by-levels nil))
	    ((receive-else-error '(:waiting $why)))))
    (error "Planing by levels failed somehow.")))


;;; Authority

(defun set-authority (type value)
  (send-to-oplan :authority type value)
  (receive-else-error '(:authority $ty $v)
    #'(lambda (ty v)
	(unless (and (eql ty type) (eql v value))
	  (error "Expected authority ~S = ~S but received ~S = ~S."
		 type value ty v))
	v)))


;;; Plan statistics

(defun request-plan-statistics-list ()
  (send-to-oplan :check-plan :get-plan-statistics)
  (receive-else-error '(:checked :statistics $stats)
    #'(lambda (stats)
	stats)))


;;; Plan views

;;; /\/: Note that request-plan-view-list and request-plan-view expect
;;; different messages in reply.  That's why request-plan-view insists
;;; on non-null viewer-args.  An alternative would be for request-plan-view
;;; to handle both sorts of reply.

(defun request-plan-view-list ()
  (send-to-oplan :get :plan-view)
  (receive-else-error '(:plan-view $net)
    #'(lambda (net)
	net)))

(defun request-plan-view (&rest viewer-args)
  (unless viewer-args
    (error "Request-plan-view called with no arguments."))
  (apply #'send-to-oplan :get :plan-view viewer-args)
  (receive-else-error '(:finished-view)))

(defun request-psgraph (format &rest more-args
			       &key (levels :all)
			       &allow-other-keys)
  (apply #'request-plan-view
      :mode :psgraph
      :format format
      :levels levels
      more-args))


;;; World views

;;; /\/: See uner plan view above for why request-world-view insists
;;; on non-null viewer-args.

(defun request-world-view-list (node-id)
  ;; Node-id is something like 3-2 for end_of node-3-2.
  (check-type node-id string)
  (send-to-oplan :get :world-view node-id)
  (receive-else-error '(:world-view $node-id $p-v-pairs)
    #'(lambda (node-id p-v-pairs)
	(declare (ignore node-id))
	p-v-pairs)))

(defun request-world-view (node-id &rest viewer-args)
  (unless viewer-args
    (error "Request-plan-view called with no arguments."))
  (check-type node-id string)
  (apply #'send-to-oplan :get :world-view node-id viewer-args)
  (receive-else-error '(:finished-view)))


;;; Other requests for plan information

(defun request-action-level-alist ()
  (send-to-oplan :get :action-levels)
  (receive-else-error '(:action-levels $levels)
    #'(lambda (levels)
	levels)))


;;; Time bounds (on a node-end)

(defun request-time-bounds (end-tag) ; -> (min max)
  ;; end-tag is e.g. (node-2 :begin)
  (db-request :funcall 'get-ne-time-bounds end-tag))

(defun get-ne-time-bounds (end-tag)
  (let* ((ne (get-node-end end-tag))
	 (tp (ne-time-point ne)))
    (list (tpoint-min tp) (tpoint-max tp))))


;;; Component output

;;; The component had better not be running when these things happen.

(defun redirect-component-output (component file-name)
  (let ((c (if (symbolp component) (find-pprocess component) component)))
    (check-type c component)
    (let ((stream (open file-name :direction :output)))
      ;; We seem to have a component and a stream, so ...
      (set-component-output-stream c stream)
      ;; Try to make sure the stream is closed when we exit
      (add-exit-action
        #'(lambda () (close stream)))
      stream)))

(defun set-component-output-stream (component stream)
  (let ((c (if (symbolp component) (find-pprocess component) component)))
    (check-type c component)
    (setf (pprocess-symbol-value c '*debug-io*)
	  stream)))

(defun set-component-debug-level (component level)
  (if (eq component :all)
      ;; All
      (dolist (p *all-pprocesses*)
	(when (component-p p)
	  (set-component-debug-level p level)))
    ;; One
    (let ((c (if (symbolp component) (find-pprocess component) component)))
      (check-type c component)
      (call-in-pprocess-env c
        #'(lambda ()
	    (dev-set-debug-level level))))))


;;;; Lower level communications

;;; Ask-oplan and send-to-oplan

(defun ask-oplan (id &rest args)
  (pi-check-state 'ask-oplan)
  (ipc-write-to-oplan (cons id args))
  (pprocess-main-loop)
  (if (next-event-p)
      (message-contents (next-event))
    nil))

(defun send-to-oplan (id &rest args)
  (pi-check-state 'send-to-oplan)
  (ipc-write-to-oplan (cons id args))
  (pprocess-main-loop))


;;; Receive-from-oplan

;;; Note that receive-from-oplan leaves the message in the queue if
;;; it doesn't match the pattern and that the default :fail function
;;; does nothing.  So it's possible to test for different patterns
;;; by writing a sequence of calls to receive-from-oplan, perhaps
;;; with a call to receive-else-error at the end.

;;; Also, since the default :succeed returns true and the default
;;; :fail returns false, receive-from-oplan can be used just to
;;; test whether the message matches a pattern.

(defun receive-from-oplan (pattern &key (succeed 'default-receive-succeed)
			                (fail 'default-receive-fail))
  (pi-check-state 'receive-from-oplan)
  (let* ((message (peek-next-event))
	 (contents (if message (message-contents message) :nothing))
	 (matching (match pattern contents)))
    (cond (matching
	   (unless (eq pattern :nothing)
	     (next-event))
	   (apply succeed (matching-values matching)))
	  (t
	   (funcall fail contents)))))

(defun matching-values (matching)
  ;; /\/: Assumes that the matcher constructs the result in the
  ;; right order (or, rather, the reverse of it).
  (and (consp matching)
       (nreverse (mapcar #'cdr matching))))

(defun default-receive-succeed (&rest values)
  (declare (ignore values))
  t)

(defun default-receive-fail (contents)
  (declare (ignore contents))
  nil)


;;; Receive-else-error

(defun receive-else-error (pattern &optional (succeed
					      'default-receive-succeed))
  (receive-from-oplan pattern
    :succeed succeed
    :fail    (partial1 #'receive-error pattern)))

(defun receive-error (pattern message-contents)
  (error "Recieved ~S from O-Plan when expecting ~S."
	 message-contents pattern))


;;;; Timeouts

;;; The user pprocess will signal a timeout condition when its
;;; wakeup-time arrives.  An error is signalled if the timout isn't
;;; handled.  See subr-mode.lisp for the details.

(defun set-timeout (seconds)
  (pi-check-state 'set-timeout)
  (set-user-pprocess-timeout seconds))

(defun cancel-timeout ()
  (pi-check-state 'cancel-timeout)
  (set-user-pprocess-timeout nil))

(defmacro with-timeout (seconds &body forms)
  `(call-with-timeout ,seconds #'(lambda () ,@forms)))

(defun call-with-timeout (seconds thunk)
  (unwind-protect
       (progn (set-timeout seconds)
	      (funcall thunk))
     (cancel-timeout)))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
