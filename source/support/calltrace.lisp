;;; Calltrace.lsp
;;; Copyright (c) 1998, 1999 by Jeff Dalton

;;; This is supposed to be portable code that makes a backtrace
;;; available.

;;; If an error occurs while the body of (with-calltrace form*),
;;; the function calltrace-p will indicate whether any backtrace
;;; information is available, and the function calltrace will
;;; print whatever backtrace information there is.

;;; Applyhook seems to be kind of random.  I can't quite figure out 
;;; what happens in Allegro CL, and GCL-1.1 seems to have bugs.
;;; (I haven't tried Lucid.)  Fortunately, evalhook is probably
;;; better in any case, since it will straightforwardly show us
;;; macro calls and special forms.

;;; /\/: Calltrace-evalhook could reset the stack entry to nil
;;; before returning, but then we'd have to use multiple-value-prog1
;;; to call evalhook.

(in-package :oplan-util)

(export '(*calltrace-stack-size*
	  *calltrace-print-level*
	  *calltrace-print-length*
	  calltrace-p
	  calltrace
	  with-calltrace))

(defparameter *calltrace-stack-size* 500)

(defvar *pdl* '())

(defvar *p* *pdl*)		;-> tail of *pdl*, (car *p*) = 1st free entry

(defparameter *calltrace-print-level* 3)
(defparameter *calltrace-print-length* 4)

(defun calltrace-p ()
  (not (eq *p* *pdl*)))

(defun calltrace (&key (print-level *calltrace-print-level*)
		       (print-length *calltrace-print-length*)
		       (print-pretty t)
		       (output *trace-output*))
  (let ((*print-level* print-level)
	(*print-length* print-length)
	(*print-pretty* print-pretty))
    (format output "~&Calltrace:~%")
    (do ((pdl *pdl* (cdr pdl)))
	((eq pdl *p*))
      (xp-format output "~W~%" (car pdl)))))

(defmacro with-calltrace (&body forms)
  `(progn
     (ensure-calltrace-pdl)
     (let ((*evalhook* 'calltrace-evalhook))
       ,@forms)))

(defun ensure-calltrace-pdl ()
  (when (null *pdl*)
    (setq *pdl* (make-list (1+ *calltrace-stack-size*))
	  *p* *pdl*)))

(defun calltrace-evalhook (form env)
  (assert *p*)
  (when (null (cdr *p*)) (error "Out of calltrace stack"))
  (cond ((consp form)
	 (setf (car *p*) form)
	 (let ((*p* (cdr *p*)))
	   (evalhook form #'calltrace-evalhook nil env)))
	(t
	 (evalhook form nil nil env))))

;;; End
