;;;; File: pseudo-process-test.lsp
;;; Contains: A mechanism for simulated multi-tasking
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: February 1993
;;; Updated: Thu Nov 21 20:50:34 1996 by Jeff Dalton
;;; Copyright: (c) 1993, 1994, AIAI, University of Edinburgh

(in-package :oplan-pseudo-process)

(use-package :oplan-test-support)

(define-test-module :pseudo-process-tests)

(in-test-module :pseudo-process-tests)

;;; The main tests are defined as separate functions that can be
;;; run on their own.

(define-test-group pprocess-run-tests
  ((run-pprocess-null-test)
   ==> :done)
  ((values (run-pprocess-test-1))
   ==> :true)
  ((values (run-pprocess-test-2))
   ==> :true))

(define-test-group pprocess-sleep-tests
  ((values (run-pprocess-sleep-test))
   ==> :true))


;;; First, make sure we can handle the null case.

(defun run-pprocess-null-test ()		; should return immediately.
  (let ((*all-pprocesses* nil)
	(*pprocess* nil))
    (pprocess-main-loop)))



;;; One consumer, two producers, producing at different intervals.
;;; Each producer has its own binding of *product*.

(defun run-pprocess-test-1 ()
  (let ((*all-pprocesses* nil)
	(*pprocess* nil)
	(*product* nil)
	(results (make-queue)))
    (declare (special *product*))
    (new-pprocess 'consumer
      :status :run-on-event
      :run-function
        (let ((count 0))
	  #'(lambda (self)
	      (enqueue (message-contents (next-event self))
		       results)
	      (incf count)
	      (when (>= count 10)
		(throw :pprocess-main-loop-exit nil)))))
    (setq *product* 'a)
    (new-pprocess 'producer-1
      :status :run
      :specials '(*product*)
      :run-function
        #'(lambda (self)
	    (declare (ignore self))
	    (send-to 'consumer *product*)))
    (setq *product* 'b)
    (new-pprocess 'producer-2
      :status :run
      :specials '(*product*)
      :run-function
        #'(lambda (self)
	    (declare (ignore self))
	    (send-to 'consumer *product*)
	    (send-to 'consumer *product*)))
    (pprocess-main-loop)
    (values (equal (queue-contents results) '(a b b a b b a b b a))
	    (queue-contents results)
	    (length (queue-contents
		     (pprocess-event-queue (find-pprocess 'consumer)))))
    ))


;;; This time we use messages and status changes to arrange for processing
;;; to stop (rather than do it by throwing to :pprocess-main-loop-exit).

;;; The product is no longer handled as a special variable.

(defun run-pprocess-test-2 ()
  (let ((*all-pprocesses* nil)
	(*pprocess* nil)
	(results (make-queue)))
    (new-pprocess 'consumer
      :status :run-on-event
      :run-function
        (let ((count 0)
	      (known-producers '()))
	  #'(lambda (self)
	      (let ((e (next-event self)))
		(pushnew (message-sender e) known-producers)
		(enqueue (message-contents e) results)
		(incf count)
		(when (>= count 10)
		  (dolist (p known-producers)
		    (send-to-pprocess p :enough))
		  (terminate-pprocess self))))))
    (flet ((make-producer (n product)
	     ;; n products per cycle
	     #'(lambda (self)
		 (when (next-event-p self)
		   (let ((e (next-event self)))
		     (ecase (message-contents e)
		       (:enough
			(terminate-pprocess self)))))
		 (dotimes (i n)
		   (send-to 'consumer product)))))
      (new-pprocess 'producer-1
         :status :run
	 :run-function (make-producer 1 'a))
      (new-pprocess 'producer-2
         :status :run
         :run-function (make-producer 2 'b)))
    (pprocess-main-loop)
    (values (equal (queue-contents results) '(a b b a b b a b b a))
	    (queue-contents results)
	    (length (queue-contents
		     (pprocess-event-queue (find-pprocess 'consumer)))))
    ))

;;; Let's try sleeping

(defun run-pprocess-sleep-test ()
  (let ((*all-pprocesses* nil)
	(*pprocess* nil)
	(output (make-queue)))
    (flet ((make-producer (number-to-produce product sleep-time)
	     #'(lambda (self)
		 (cond ((> number-to-produce 0)
			(enqueue product output)
			;(format t "~S~%" product)
			(decf number-to-produce)
			(set-to-sleep-for sleep-time))
		       (t
			(terminate-pprocess self)))))
	   (now ()
	     (get-primitive-real-time)))
      (new-pprocess 'producer-1
         :status :run-on-event
	 :wakeup-time (now)		;just to get it started
	 :run-function (make-producer 5 'a 1))
      (new-pprocess 'producer-2
         :status :run-on-event
	 :wakeup-time (now)		;just to get it started
         :run-function (make-producer 5 'b 2))
      (pprocess-main-loop)
      ;; Now check the result
      (let ((seq (queue-contents output)))
	(flet ((a+b ()
		 (case (pop seq)
		   (b (eq (pop seq) 'a))
		   (a (eq (pop seq) 'b))
		   (t nil))))
	  (values
	    (and (eq (pop seq) 'a)
		 (eq (pop seq) 'b)
		 (eq (pop seq) 'a)
		 (a+b)			;a and b in either order
		 (eq (pop seq) 'a)
		 (a+b)			;a and b in either order
		 (equal seq '(b b)))
	    (queue-contents output)))))))


;;; The following procedures can be used to set up test situations
;;; that the user can interact with (as opposed to automatic tests
;;; that can run on their own).

(defun clear-pprocess-system ()
  (setq *all-pprocesses* nil
	*pprocess* nil))

(defun set-up-echo-test ()
  (new-pprocess 'echo
    :status :run-on-event
    :run-function
      #'(lambda (self)
	  (let ((m (next-event self)))
	    (send-reply m
			`(thanks for saying ,(message-contents m))))))
  (new-pprocess 'user
    ;; The user can send mail and look at the reply events.
    :status :run
    :run-function
      #'(lambda (self)
	  (declare (ignore self))
	  (format t "~&form> ")
	  (prin1 (eval (read)))))
  t)

(defun set-up-input-stream-test ()
  (new-pprocess 'echo
    :status :run-on-event
    :run-function
      #'(lambda (self)
	  (let ((m (next-event self)))
	    (send-reply m
			`(thanks for saying ,(message-contents m))))))
  (new-pprocess 'user
    ;; The user can send mail and look at the reply events.
    :status :run-on-event
    :input-streams (list 't)		; *terminal-io*
    :run-function
      #'(lambda (self)
	  ;; N.B. won't run until first input is ready.
	  ;; So prompt follows eval to be there for next time.
	  (declare (ignore self))
	  (prin1 (eval (read t)))
	  (clear-input t)		; especially in KCL
	  (format t "~%form> ")))
  t)


(defun set-up-reminder (&optional (delta 5))
  (new-pprocess 'reminder
    :status :run-on-event
    :wakeup-time (get-primitive-real-time) ; just to get started
    :run-function
      #'(lambda (self)
	  (declare (ignore self))
	  (princ ".")
	  (force-output)
	  (set-to-sleep-for delta))))

(defun set-up-suspending-reminder (&optional (delta 5))
  (new-pprocess 'reminder
    :status :suspended
    :wakeup-time (get-primitive-real-time) ; just to get started
    :run-function
      #'(lambda (self)
	  (princ ".")
	  (force-output)
	  (set-to-sleep-for delta)
	  (suspend-pprocess self))))

(defun set-up-wait-until-wakeup-test ()
  (new-pprocess 'echo
    :status :run-on-event
    :run-function
      #'(lambda (self)
	  (let ((m (next-event self)))
	    (send-reply m `(thanks for saying ,(message-contents m)))
	    (wait-until-wakeup))))
  (new-pprocess 'user
    ;; The user can send mail and look at the reply events.
    ;; Don't forget to call (wakeup (find-pprocess 'echo)).
    :status :run
    :run-function
      #'(lambda (self)
	  (declare (ignore self))
	  (format t "~&form> ")
	  (prin1 (eval (read)))))
  t)

;;; End
