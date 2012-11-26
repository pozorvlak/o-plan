;;;; File: util-macros.lisp
;;; Contains: Useful macros that don't belong anywhere else
;;; Author: Jeff Dalton
;;; Created: February 1993
;;; Updated: Sun Aug  2 13:44:41 1998 by Jeff Dalton
;;; Copyright: (c) 1993, 1994, 1995, 1996 AIAI, University of Edinburgh


(in-package :oplan-util)


;;; Inline functions

;;; Using a special macro is neater than writing a proclamation and a
;;; defun, and it makes it easier for us to exploit other ways (such as
;;; defsubst) to get a function call expanded inline when (as in some
;;; Common Lisps) inline proclamations don't suffice.

(defmacro defun-inline (name parameters &body body)
  `(progn
     (proclaim '(inline ,name))
     (defun ,name ,parameters
       .,body)))


;;; Auto-exporting function and macro definitions

(defmacro defun-export (name parameters &body body)
  `(progn
     (export ',name)
     (defun ,name ,parameters . ,body)))

(defmacro defmacro-export (name parameters &body body)
  `(progn
     (export ',name)
     (defmacro ,name ,parameters . ,body)))


;;; LABEL defines a "named LET" that can be called as a local function.
;;;
;;; (LABEL fname ((var init) ...) form ...)
;;;   ==> (LABELS ((fname (var ...) form ...))
;;;         (fname init ...))

(defmacro label (name init-specs &body forms)
  `(labels ((,name ,(mapcar #'car init-specs)
	      ,@forms))
     (,name ,@(mapcar #'cadr init-specs))))


;;; While and until

(defmacro while (test &body forms)
  `(do () ((not ,test)) . ,forms))

(defmacro until (test &body forms)
  `(do () (,test) . ,forms))


;;; Implies

(defmacro implies (a b)
  `(or (not ,a) ,b))


;;; Working directory change

(defmacro with-working-directory (namestring &body forms)
  `(let ((.wd. (working-directory-pathname)))
     (unwind-protect
	  (progn (change-working-directory ,namestring)
		 ,@forms)
       (change-working-directory .wd.))))


;;; With-unix-process-io provides an io-stream to a process and
;;; arranges to call wait, or whatever's neccessary, at the end.

(defmacro with-unix-process-io ((stream-var program &rest args) &body forms)
  (let ((stream (gensym))
	(pid (gensym)))
    `(multiple-value-bind (,stream ,pid)
         (unix-process-io ,program ,@args)
       (unwind-protect
	    (with-open-stream (,stream-var ,stream)
	      ,@forms)
	 (unix-process-finish ,pid)))))


;;; Setf method / expansion

;;; Here's something to let us work in a greater range of CLs.

(defun get-setf-info (place &rest maybe-env)
  (apply (function #+:ansi-cl get-setf-expansion
		   #-:ansi-cl get-setf-method)
	 place
	 maybe-env))


;;; Deletef

;;; Since we have the item first, rather than the place, we can't
;;; use define-modify-macro directly.  Hence the -from definitions.

(defmacro deletef (item place &rest other-args)
  `(deletef-from ,place ,item . ,other-args))

(define-modify-macro deletef-from (seq &rest other-args) delete-from)

(defmacro delete-from (place item &rest other-args)
  `(delete ,item ,place . ,other-args))

;;; Removef

(defmacro removef (item place &rest other-args)
  `(removef-from ,place ,item . ,other-args))

(define-modify-macro removef-from (seq &rest other-args) remove-from)

(defmacro remove-from (place item &rest other-args)
  `(remove ,item ,place . ,other-args))


;;; Appendf and appendf1

(define-modify-macro appendf (list &rest more-lists) append)

(defmacro appendf1 (place item) `(appendf ,place (list ,item)))


;;; Nconcf, nconcf1, and nconcf-new

(define-modify-macro nconcf (list &rest more-lists) nconc)

(defmacro nconcf1 (place item) `(nconcf ,place (list ,item)))

;;; Nconcf-new can combine finding the last cons in the list with
;;; the search for the item already being present.  So it should be
;;; possible for it to be as efficient as pushnew; and we try to make
;;; it that efficient when the :test is eq.

(define-modify-macro nconcf-new (item &rest member-keys) %nconc-item-if-new)

(defmacro %nconc-item-if-new (place item &rest member-keys
			      &key (test nil test-p) &allow-other-keys)
  ;; Don't call this directly -- it doesn't protect against multiple
  ;; evaluations of subforms of place. But nconcf-new will protect for us.
  (let ((item-var (gensym)))
    (if (and test-p			;have :test
	     (= 2 (length member-keys))	;and only :test
	     (or (equal test ''eq) (equal test '#'eq)))
	;; Optimized eq case.
	`(setf ,place (nconc-item-if-new-eq ,place ,item))
      ;; Otherwise, there's a separate member test.
      `(let ((,item-var ,item))
	 (if (member ,item-var ,place ,@member-keys)
	     ,place
	   (nconcf ,place (list ,item-var)))))))

(defun nconc-item-if-new-eq (list item)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (if (null list)
      (list item)
    (if (eq item (car list))
	list
      ;; When we test for the end of the loop, we've already
      ;; checked whether the car of tail is eq to item, either
      ;; above or in the when.
      (do ((tail list (cdr tail)))
	  ((null (cdr tail))
	   (setf (cdr tail) (list item))
	   list)
	(when (eq (cadr tail) item)
	  (return list))))))


;;; Ensuref is used to get memoized values.  E.g. (ensuref x (make-x))
;;; is equivalent to (or x (setf x (make-x))).  It uses setf methods
;;; to avoid multiple-evaluation, etc.

(defmacro ensuref (place init-form)
  (multiple-value-bind  (temp-vars val-forms store-vars
			 store-form access-form)
        (get-setf-info place)
      (unless (= (length store-vars) 1)
        (error "Ensure can't handle more than one store value in ~S" place))
      ;; could skip binding vars in some cases...
      `(let* ,(mapcar #'list temp-vars val-forms)
	 (or ,access-form
	     (let ((,(car store-vars) ,init-form))
	       ,store-form)))))


;;; letf* and letf from CMU CL.

;;; These macros waste time as opposed to space.  [It said.]

(defmacro letf* (bindings &body body &environment env)
  "Does what one might expect, saving the old values and setting the
  generalized variables to the new values in sequence.  Unwind-protects
  and get-setf-method are used to preserve the semantics one might expect
  in analogy to let*, and the once-only evaluation of subforms."
  (labels ((do-bindings
	    (bindings)
	    (cond ((null bindings) body)
		  (t (multiple-value-bind (dummies vals newval setter getter)
					  (get-setf-info (caar bindings) env)
		       (let ((save (gensym)))
			 `((let* (,@(mapcar #'list dummies vals)
				  (,(car newval) ,(cadar bindings))
				  (,save ,getter))
			     (unwind-protect
			       (progn ,setter
				      ,@(do-bindings (cdr bindings)))
			       (setq ,(car newval) ,save)
			       ,setter)))))))))
    (car (do-bindings bindings))))


(defmacro letf (bindings &body body &environment env)
  "Like letf*, but evaluates all the implicit subforms and new values of all
  the implied setfs before altering any values.  However, the store forms
  (see get-setf-method) must still be evaluated in sequence.  Uses unwind-
  protects to protect the environment."
  (let (temps)
    (labels
      ((do-bindings
	(bindings)
	(cond ((null bindings) body)
	      (t (let ((binding (car bindings)))
		   (multiple-value-bind (dummies vals newval setter getter)
					(get-setf-info (car binding) env)
		     (let ((save (gensym)))
		       (mapcar #'(lambda (a b) (push (list a b) temps))
			       dummies vals) 
		       (push (list save getter) temps)
		       (push (list (car newval) (cadr binding)) temps)
		       `((unwind-protect
			   (progn ,setter
				  ,@(do-bindings (cdr bindings)))
			   (setq ,(car newval) ,save)
			   ,setter)))))))))
      (let ((form (car (do-bindings bindings))))
	`(let* ,(nreverse temps)
	   ,form)))))


;;;; Reinitializing variables in groups

;;; Definit

(defvar *definit-variables* '())

(defmacro get-group-initfun (group-name var-name)
  `(getf (get ,var-name 'initfuns) ,group-name))

(defmacro definit (group-name var-name initform &optional docstring)
  (check-type group-name symbol)
  (check-type var-name symbol)
  (when docstring
    (check-type docstring string))
  `(progn
     (pushnew ',var-name *definit-variables*)
     (setf (get-group-initfun ',group-name ',var-name)
           #'(lambda () ,initform))
     (defvar ,var-name ,initform ,@(when docstring (list docstring)))))

(defun initialize-variable-group (group-name)
  (dolist (v *definit-variables*)
    (let ((initfun (get-group-initfun group-name v)))
      (when initfun
	(set v (funcall initfun))))))


;;;; Initializing functions

(defmacro get-initializers (group-name)
  `(get ,group-name 'initializers))

(defmacro define-initializer (group-name fn-name parameters &rest body)
  (check-type group-name symbol)
  (check-type parameters null)
  `(progn
     (push ',fn-name (get-initializers ',group-name))
     (defun ,fn-name ,parameters
       . ,body)))

(defun call-initializers (group-name)
  (mapc #'funcall (get-initializers group-name)))


;;;; Output macro
;;;
;;; (OUTPUT directive...) is similar to the Franz Lisp "msg" macro.
;;; It is in some ways an alternative to FORMAT.  A directive can be:
;;;   a literal string       --  the string is printed by PRINC
;;;   the symbol //          --  a newline is printer by TERPRI
;;;   some other symbol      --  a variable: the value is printed by PRINC
;;;   a cons which can be:
;;;     a form               --  the value is printed by PRINC
;;;     a special directive  --  see below
;;;   something else         --  treated as a literal and printed by PRINC
;;;
;;; Special directives are defined by DEFINE-OUTPUT-EXPANDER.  This
;;; defines a function that is called with the cdr of the directive
;;; as it's arguments.  The function returns a form that is used
;;; directly in the expansion of OUTPUT.  Predefined special directives
;;; are:
;;;   (:STREAM form)
;;;      redirects output to the value of the form
;;;   (:INCLUDE form)
;;;      places the form in-line in the expansion of OUTPUT.
;;;   (:FORMAT format-string format-arg...)
;;;      becomes a call to FORMAT.
;;;

(defvar *output* (make-synonym-stream '*terminal-io*))

(defmacro output (&rest directives)
  `(let ((*output* *output*))
     ,@(mapcar #'expand-output-directive directives)))

(eval-when (eval compile load)
  
(defun expand-output-directive (d)
  (cond ((stringp d)
	 `(princ ,d *output*))
	((eq d '//)
	 `(terpri *output*))
	((symbolp d)
	 `(princ ,d *output*))
	((consp d)
	 (if (symbolp (car d))
	     (let ((expander (get (car d) :output-expander)))
	       (if expander
		   ;; Special-case expansion
		   (apply expander (cdr d))
		 ;; A random form
		 `(princ ,d *output*)))
	   (error "Illegal output directive: ~S." d)))
	(t
	 ;; A random object
	 `(princ ',d *output*))))

);end eval-when

(defmacro define-output-expander (name parameters &body forms)
  (let ((fn-name (intern (concatenate 'string 
                           "%" (string name) "-OUTPUT-EXPANDER"))))
    `(eval-when (eval compile load)
       (defun ,fn-name ,parameters
	 ,@forms)
       (setf (get ',name :output-expander) ',fn-name))))

;;; :STREAM sets *output* and hence the output destination (dynamically)
;;; within an OUTPUT form.

(define-output-expander :stream (form)
  `(setq *output* ,form))

;;; :INCLUDE gives a form complete control over what output occurs.

(define-output-expander :include (form)
  form)

;;; :FORMAT

(define-output-expander :format (format-string &rest format-args)
  `(format *output* ,format-string ,@format-args))


;;; End
