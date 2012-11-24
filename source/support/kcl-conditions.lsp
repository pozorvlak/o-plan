;;;; File: kcl-conditions.lsp
;;; Contains: "Fake" CL condition system for KCL
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: June 1994
;;; Updated: Wed Jun 23 23:13:54 1999 by Jeff Dalton
;;; Copyright: (c) 1994, AIAI, University of Edinburgh

;;; This is a minimal condition system for KCL and AKCL which do not have
;;; the standard CL condition system built-in.

(in-package :oplan-util)

;; Condition types
(export '(condition simple-condition serious-condition))
(export '(error simple-error))
(export '(warning simple-warning))

;; Defining form
(export '(define-condition))		;N.B.: a very limited version

;; Constructor
(export '(make-condition))

;; Operations on condition objects
(export '(condition-p condition-format-string condition-format-arguments))

;; Condition slots
; (export '(format-string format-arguments))

;; Signalling operations
(export '(signal signal-error))

;; Condition handling
(export '(handler-bind handler-case ignore-errors))

;; Restarts
; /\/: No restart support

;; Debugging
(export '(show-nested-errors))


;;; Defining condition types

;;; We support only a very limited form of define-condition.  There can
;;; be only one parent type and there is no way to specify slots or options.
;;; This is promarily because define-condition takes defclass-style slot
;;; specs which are too much trouble for us to handle when we can't call on
;;; defclass.

;;; N.B. The slots arg is no longer optional (ANSI vs CLtL II).

(defmacro define-condition (name parent-types slots &rest options)
  (unless (= (length parent-types) 1)
    (error "Condition type ~S can have only one parent." name))
  (unless (and (null slots) (null options))
    (error "Condition type ~S cannot have slots or options." name))
  `(define-condition-struct (,name (:include ,(car parent-types)))))

;;; Define-condition-struct is internal.

(defmacro define-condition-struct (spec &rest slots)
  (let* ((name (if (consp spec) (car spec) spec))
	 (opts (if (consp spec) (cdr spec) nil))
	 (make-fn (make-make-fn name)))
    `(progn
       (setf (condition-make-fn ',name) ',make-fn)
       (defstruct (,name
		   (:print-function print-condition)
		   (:constructor ,make-fn)
		   ,@opts)
	 ,@slots))))

(defmacro condition-make-fn (cond-type) `(get ,cond-type 'condition-make-fn))

(eval-when (eval compile load)
  (defun make-make-fn (name)
    (intern (concatenate 'string "MAKE-" (symbol-name name)))))

(defun print-condition (cond stream depth)
  (declare (ignore depth))
  (if (or *print-escape* (null (condition-format-string cond)))
      (format stream "#<~S \"~?\">"
	      (type-of cond)
	      (or (condition-format-string cond) "")
	      (condition-format-arguments cond))
    (apply #'format stream
	   (condition-format-string cond)
	   (condition-format-arguments cond))))

;;; Constructor

(defun make-condition (type &rest slot-initializations)
  (apply (or (condition-make-fn type)
	     (error "~S is not a condition type." type))
	 slot-initializations))


;;; Built-in condition types

;;; I think it's only simple-{condition,warning,error} that are defined
;;; to have the format slots.  But to do that with only single inheritance,
;;; we have to introduce the slots at a common ancestor, with condition
;;; as the obvious choice.

(defstruct (condition
	     (:constructor %make-condition)
	     (:print-function print-condition))
  ;; Need to define condition by hand because the constructor can't
  ;; be called make-condition.
  (format-string nil)
  (format-arguments nil))

(setf (condition-make-fn 'condition) '%make-condition)

(define-condition-struct (simple-condition  (:include condition)))
(define-condition-struct (serious-condition (:include condition)))
(define-condition-struct (error             (:include serious-condition)))
(define-condition-struct (simple-error      (:include error)))
(define-condition-struct (warning           (:include condition)))
(define-condition-struct (simple-warning    (:include warning)))


;;; Signalling

;;; *Handlers* is a list of lists of handler bindings, one list of bindings
;;; for each dynamically nested handler-bind.  Each binding has the form
;;; (typespec . function).  Each handler-bind rebinds *handlers*, adding
;;; its list of bindings to the front.

(defvar *handlers* '())

(defun signal (datum &rest arguments)
  (let ((c (condition-from-args 'signal 'simple-error datum arguments)))
    (do ((handlers *handlers* (cdr handlers)))
	((null handlers) nil)
      ;; The car of handlers is a list of handler bindings.
      ;; The cdr of handlers is the value *handlers* had when those
      ;; bindings were established.  We have to restore that value
      ;; when trying the bindings.
      (let ((*handlers* (cdr handlers)))
	;; Try each binding in turn.
	(dolist (binding (car handlers))
	  (let ((typespec (car binding))
		(handler (cdr binding)))
	    ;; Call the handler if the typespec matches.
	    (when (typep c typespec)
	      (funcall handler c))))))))

(defun signal-error (datum &rest arguments)
  "Signal a condition and enter the debugger if its not handled."
  (let ((c (condition-from-args 'signal-error 'simple-error datum arguments)))
    (signal c)
    (break "No handler for ~S" c)))

(defun condition-from-args (caller default-condition-type datum arguments)
  (cond ((condition-p datum)
	 (if (null arguments)
	     datum
	   (error "~S called with condition ~S;~%~
                   Arguments should be NIL, not ~S."
		  caller datum arguments)))
	((and (symbolp datum) (subtypep datum 'condition))
	 (apply #'make-condition datum arguments))
	((stringp datum)
	 (make-condition default-condition-type
	   :format-string datum
	   :format-arguments arguments))
	(t
	 (error "~S called with illegal arguments (~S ~{~S~^ ~})"
		caller datum arguments))))


;;; Handler-bind

(defmacro handler-bind (bindspecs &rest forms)
  ;; A bindspec has the form (typespec handler-expr).
  (flet ((handler-bindspec->cons (bindspec)
	   (destructuring-bind (typespec handler) bindspec
	     `(cons ',typespec ,handler))))
    `(let ((*handlers*
	    (cons (list ,@(mapcar #'handler-bindspec->cons
				  bindspecs))
		  *handlers*)))
       ,@forms)))

;;; Handler-case

(defmacro handler-case (expression &rest clauses)
  (if (assoc :no-error clauses)
      (expand-no-error-handler-case expression clauses)
    `(block .handler-case-exit.
       (funcall				;call thunk returned by handler
	 (block .handler-case-unwind.
	   (handler-bind ,(mapcar #'hc-clause->handler-binding clauses)
	     (return-from .handler-case-exit.
	       ,expression)))))))

(defun hc-clause->handler-binding (clause)
  (destructuring-bind (typespec var &rest forms) clause
    (let ((handler-body
	   ;; Unwind the dynamic env by returning a thunk
	   `(return-from .handler-case-unwind. #'(lambda () ,@forms))))
      (if var
	  `(,typespec #'(lambda ,var ,handler-body))
	  `(,typespec
	    #'(lambda (.ignore.)
		(declare (ignore .ignore.))
		,handler-body))))))

(defun expand-no-error-handler-case (expression clauses)
  (let* ((no-err-clause (assoc :no-error clauses))
	 (other-clauses (remove no-err-clause clauses)))
    (unless (eq no-err-clause (car (last clauses)))
      (error "In handler-case, :no-error must be last."))
    (destructuring-bind (lambda-list &rest forms) (cdr no-err-clause)
      ;; Almost straight from CLtL II:
      ;; But seems to drive a compiler bug sometimes in GCL 1.1 /\/
      #+:undef
      `(block .error-return.
	 (multiple-value-call #'(lambda ,lambda-list . ,forms)
	   (block .normal-return.
	     (return-from .error-return.
	       (handler-case (return-from .normal-return. ,expression)
		 ,@other-clauses)))))
      ;; So we'll try this:
      `(block .handler-case-return.
	 (let ((.normal-values. nil))
	   (tagbody
	      (return-from .handler-case-return.
		(handler-case
		    (progn (setq .normal-values.
				 (multiple-value-list ,expression))
			   (go OK))
		  ,@other-clauses))
	    OK
	      (return-from .handler-case-return.
		(apply #'(lambda ,lambda-list . ,forms)
		       .normal-values.))))))))

;;; Ignore-errors

(defmacro ignore-errors (&rest forms)
  `(handler-case (progn ,@forms)
     (error (c) (values nil c))))


;;; Intercepting standard KCL errors

;;; /\/: We try to detect endlessly nested errors, because there seem
;;; to be bugs in GCL that cause them.  However, we're not very clever
;;; this and don't rebind *nested-errors* back to () when entering
;;; a break loop.

(export '*nested-errors*)		;the user may want to look at it.

(defvar *earlier-nested-errors* '())
(defvar *nested-errors* '())

(defun new-error-handler (error-name correctable-p function-name
			  continue-format-string error-format-string
			  &rest args)
  (let ((*nested-errors*
	 (cons (list function-name
		     error-format-string continue-format-string
		     args)
	       *nested-errors*)))
    (when (>= (length *nested-errors*) 5)
      (let ((*earlier-nested-errors*
	     (cons *nested-errors* *earlier-nested-errors*))
	    (*nested-errors*
	     '()))
	(sys:use-fast-links nil)		;maybe this helps /\/
	;; Try to avoid any complex output routines
	(terpri *debug-io*)
	(write-line "5 nested errors; current error is:" *debug-io*)
	(write-line error-format-string *debug-io*)
	;; Maybe even break does too much.
	(lisp::assign-standard-io-syntax) 	;maybe this helps /\/
	(with-standard-io-syntax
	  (setq *package* (find-package :oplan))
	  (paranoid-break "Help!"))))

    ;; First signal to see if anyone wants to handle the error.
    (apply #'signal error-format-string args)

    ;; Then call the standard GCL error handler.
    (apply #'original-error-handler
	   error-name correctable-p function-name
	   continue-format-string error-format-string
	   args)))

(defun show-nested-errors
    (&optional (n (length (first *earlier-nested-errors*))))
  (let ((e (first *earlier-nested-errors*)))
    (dotimes (i n)
      (write-line (primitive-conversion-to-string (pop e)) *debug-io*))))

(defun paranoid-break (message-string)
  (write-line message-string *debug-io*)
  (write-line "Type a <form>, \"continue\", or \"break\"" *debug-io*)
  (while t
    again
    (write-string "command> " *debug-io*)
    (let ((cmd (read-line *debug-io*)))
      (when (string= cmd "")
	(go again))
      (when (string= cmd "continue")
	(return))
      (when (string= cmd "break")
        (break "Help!")
	(go again))
      (write-line
        (primitive-conversion-to-string (eval (read-from-string cmd)))
	*debug-io*)
      (terpri *debug-io*))))

(defun primitive-conversion-to-string (x)
  (big-string-concat
    (string-list x)))

(defun string-list (x)
  (cond ((stringp x)
	 (list "\"" x "\""))
	((symbolp x)
	 (list (symbol-name x)))
	((consp x)
	 (let ((parts (make-tconc)))
	   (tconc parts "(")
	   (lconc parts (string-list (pop x)))
	   (loop
	     (cond ((null x) (return))
		   ((atom x) (tconc parts " . ")
		             (lconc parts (string-list x))
			     (return))
		   (t (tconc parts " ")
		      (lconc parts (string-list (pop x))))))
	   (tconc parts ")")
	   (tconc-contents parts)))
	((numberp x)
	 (list "<a number>"))
	(t
	 (list "<a " (symbol-name (type-of x)) ">"))))

(unless (fboundp 'original-error-handler)
  (setf (symbol-function 'original-error-handler)
	(symbol-function 'si:universal-error-handler)))

(setf (symbol-function 'si:universal-error-handler)
      (symbol-function 'new-error-handler))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
