;;;; File: context.lsp
;;; Contains: A Context Mechanism
;;; Author: Jeff Dalton, based on a version by Ken Currie
;;; Created: Thu Jan 24 14:21:58 1991
;;; Updated: Sat Jun 19 21:26:02 1999 by Jeff Dalton
;;; Copyright: (c) 1993, AIAI, University of Edinburgh

;;; *************************************************************************
;;; *                                                                       *
;;; *  CONTEXT.LSP  -  A Context Mechanism                                  *
;;; *                                                                       *
;;; *  This module has its origins in the work on Contexts in POP-2 by      *
;;; *  Harry Barrow.                                                        *
;;; *                                                                       *
;;; *                                                                       *
;;; *************************************************************************

;;; Simple context mechanism, a rational reconstruction of KWC's "obcontext".
;;; The algorithms are essentailly the same, but the code is completely new
;;; and "micro-optimized".  Written by Jeff Dalton.

(in-package :oplan-ctxt)

(use-package :oplan-util)

(export '(;; Variables
	  *context*
	  *global-context*

	  ;; Functions
	  new-context
	  find-context

	  context-p
	  context-number
	  context-parent
	  context-children
	  
	  push-context pop-context

	  in-context
	  delete-context
	  access-in-context update-in-context deref-in-context
	  print-context-tree

	  ;; Defining forms
          define-context-accessor

	  ;; Context-layered accessors
	  ctxt-symbol-value
	  ctxt-gethash
	  ctxt-puthash
	  ctxt-maphash
	  ))


;;; Macro to context layer functions

;;; /\/: The field accessor is (fun)called exactly once, to get the
;;; contents of the slot.  It might be better to call it here, in-line,
;;; instead of in access-in-context.  [Ok, let's ... jwd 04 mar 94]

(defmacro define-context-accessor (name field-accessor)
  `(progn
     (proclaim '(inline ,name))
     (defun ,name (o)
       (deref-in-context (,field-accessor o)))
     #+:undef
     (defun ,name (o)
       (access-in-context #',field-accessor o))
     (defsetf ,name (o) (new-val)
       `(update-in-context #'(lambda (o v)
			       (setf (,',field-accessor o) v))
			   #',',field-accessor
			   ,o
			   ,new-val))))

;;; Some accessors

;;; ctxt-symbol-value

(define-context-accessor ctxt-symbol-value symbol-value)

;;; ctxt-gethash

;;; /\/: deref-in-context uses :undef for "no value exists", so
;;; we'll do the same here when determining what the second returned
;;; value should be.

(defun ctxt-gethash (key table &optional default)
  (multiple-value-bind (value found-p) (gethash key table default)
    (if found-p
	(let ((v (deref-in-context value)))
	  (if (eq v :undef)
	      (values default nil)	;not found
	    (values v t)))		;found
      (values default nil))))		;not found

(defun ctxt-puthash (key table value)
  (update-in-context #'(lambda (k v) (setf (gethash k table) v))
		     #'(lambda (k) (gethash k table :undef))
		     key
		     value))

(defsetf ctxt-gethash ctxt-puthash)

(defun ctxt-maphash (fn table)
  (maphash #'(lambda (key value)
	       (let ((v (deref-in-context value)))
		 (unless (eq v :undef)
		   (funcall fn key v))))
	   table))


;;; Contexts

(defstruct (context (:print-function print-context))
  (number 0 :type fixnum)
  (parent nil)
  (children '()))

#+akcl
(eval-when (compile)
  (si::freeze-defstruct 'context))

(defvar *context-count* 0)

(defvar *global-context* (make-context)
  "The initial root context.")

(defvar *root-contexts* (list *global-context*)
  "List of contexts with no parent context.")

(defvar *context* *global-context*
  "The current context.")


;;; Context printing

(defun print-context (context stream depth)
  (declare (ignore depth))
  (format stream "#<context ~D>" (context-number context)))


;;; Make a new context

(defun new-context (parent)
  (incf *context-count*)
  (check-type *context-count* fixnum)
  (let ((new-context
	 (make-context :number *context-count*
		       :parent parent
		       :children nil)))
    (if (context-p parent)
	(push new-context (context-children parent))
	(push new-context *root-contexts*))
    new-context))


;;; Return a context with a given number, n.

;;; The search is depth-first, left to right.  However, since a context
;;; always has a higher number than its parent, we do not need to search
;;; the descendents of any context with a number > n.  (The descendents
;;; will also have numbers > n.)  

(defun find-context (n)
  (declare (fixnum n))
  (label c-search ((contexts *root-contexts*))
    (dolist (c contexts)
      (cond ((= (context-number c) n)
	     (return-from find-context c))
	    ((< (context-number c) n)
	     (c-search (context-children c))))))
  (error "No context numbered ~D." n))


;;; Context push and pop

(defun push-context ()
  (setq *context* (new-context *context*)))

(defun pop-context ()
  (when (and (context-p *context*)
	     (not (null (context-parent *context*))))
    (setq *context* (context-parent *context*))))


;;; Apply in context

(defun in-context (c fn &rest args)
  #+akcl (declare (:dynamic-extent args))
  (let ((*context* c))
    (apply fn args)))


;;; Delete a context and all its descendents

(defun delete-context (context)
  ;; /\/: For now, we do nothing.
  ;; /\/: Should it pop context if the coontext to delete is
  ;;      the current context?
  (declare (ignore context))
  t)


;;; Access in context

;;; /\/: A cval could contain a 1-element cache for the value most
;;; recently looked-up.  This would win if lookups significantly
;;; outnumbered updates and context switches.  N.B.  It would cache
;;; the current context (and the value), not the context that
;;; established the value.  [Done -- jwd 04 mar 94]

(defstruct cval
  "Context value.  Contains a sorted alist indexed by context.  The
   alist is never null.  Newer (higher numbered) contexts appear first."
  alist
  cached-context			;most recently seen context
  cached-value)				;and the associated value

#+akcl
(eval-when (compile)
  (si::freeze-defstruct 'cval))

;;; We want the value for the current context or the nearest ancestor
;;; for which a value's been stored.

;;; Newer contexts have higher numbers.  A context is always newer
;;; than its parent.  In the cval alist, entries for newer contexts
;;; appear first.

;;; /\/: A couple of tests are simpler than they might to, since the
;;; simpler version should be faster, at least in GCL 1.1.  In particular,
;;; the end-test for the do is (null target) rather than (not (context-p
;;; target)), and  when skipping alist entries, the test is alist rather
;;; than (consp alist).

;;; /\/: GCL 1.1 still, annoyingly, calls structure-subtype-p for the
;;; cval-p test, even though we've frozen the cval struct.  It would
;;; also call it for the (not (context-p target)) test.

(defun access-in-context (accessor object)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (deref-in-context (funcall accessor object)))

(defun deref-in-context (v)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (if (not (cval-p v))
      ;; A value that isn't a cval doesn't depend on context.
      v
    (locally (declare (type cval v))
      (if (eq (cval-cached-context v) *context*)
	  (cval-cached-value v)
	(let* ((alist (cval-alist v))
	       (value
		;; At the end of the loop, all remaining entries are
		;; for contexts older than the target but perhaps not
		;; older than the target's parent.
		(do ((target *context* (context-parent target)))
		    ((null target) :undef)
		  (declare (type context target))
		  (let ((n (context-number target)))
		    (declare (type fixnum n))
		    ;; Skip enties for contexts newer than the target.
		    (while (and alist (< n (context-number (caar alist))))
		      (setq alist (cdr alist)))
		    (when (null alist)
		      (return :undef))
		    (when (eq target (caar alist))
		      (return (cdar alist)))))))
	  ;; Found the value, so cache it and return.
	  (setf (cval-cached-context v) *context*
		(cval-cached-value v) value))))))

;;; Update in context

(defun update-in-context (updater accessor object new-value)
  (let ((v (funcall accessor object)))
    ;; If the current value, v, is not already context-dependent, then
    ;; make it context-dependent by making it the value for some context.
    ;; If v isn't :undef, save it as the value in the global context.
    ;; Otherwise, just make it the value in the current context, and it
    ;; will be replaced by new-value below.
    (when (not (cval-p v))
      (let ((ctxt (if (eq v :undef) *context* *global-context*)))
	(setq v (make-cval :alist (list (cons ctxt v))))
	(funcall updater object v)))
    (locally (declare (type cval v))
      ;; Cache context and value
      (setf (cval-cached-context v) *context*
	    (cval-cached-value v) new-value)
      ;; Install the value in the alist, adding a new alist entry if necessary
      (let ((alist (cval-alist v))
	    (target *context*)
	    (n (context-number *context*)))
	(declare (fixnum n))
	(assert (not (null alist)))
	(loop
	  (cond ((eq (caar alist) target)
		 (setf (cdar alist) new-value)
		 (return new-value))
		((< (context-number (caar alist)) n)
		 (setf (cdr alist) (cons (car alist) (cdr alist)))
		 (setf (car alist) (cons target new-value))
		 (return new-value))
		((null (cdr alist))
		 (setf (cdr alist) (list (cons target new-value)))
		 (return new-value))
		(t (setq alist (cdr alist)))))))))


;;; Print context tree

;;; May want parameters that specify a root and a depth limit.

(defun print-context-tree ()
  (label walk ((contexts *root-contexts*)
	       (level 0))
    (dolist (c (reverse contexts))
      (format t "~&~vT~S~%" (* 3 level) (context-number c))
      (walk (context-children c)
	    (+ level 1)))))


;;;; HISTORY:
;;
;;  2-Oct-89:  Converted to packages by KWC.
;;  5-Oct-89:  Complete re-write to be compatible with the POP package
;;             used in O-Plan1.
;;  7-Nov-89:  Included a Context Tree Printer, after much debugging.
;;    1991  :  Complete rewrite by JWD who wanted to understand how it
;;             worked and to try various micro-optimizations.
;;   May 93 :  New ('91) version adapted to O-Plan for release 2.0.
;;   Oct 93 :  Minor cleanups.
;;  4 Mar 94:  Added derefincontext and the one-context cache.
;;  8 Mar 94:  Added ctxt-symbol-value.
;; 26 May 94:  Added ctxt-gethash.
;; 13 Apr 96:  Added a no-op delete-context.
;; 19 Nov 96:  Renamed functions and variables to use "-" rather than
;;              running words together (e.g. push-context rather than
;;              pushcontext).
;;             Removed precontext, succontexts, and contextnum.
;;             Made access-in-context just call deref-in-context.
;;             Rewrote deref-in-context using some simpler tests
;;              and making sure the right value is always cached.
;;              (Before, it went wrong if the do-loop ran out.)
;;             Added a local cval declaration to deref-in-context
;;              and update-in-context.
;;  3 Jun 99:  Fixed bug in ctxt-gethash: once a key had a value in any
;;             context, ctxt-gethash would never think it was not present
;;             in the table (and hence will never return nil as its second
;;             value or use the default value given as the 3rd arg).
;; 19 Jun 99:  Added ctxt-maphash.
;;;;

;;; End

