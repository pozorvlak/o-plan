;;;; File: lisp-prep.lisp
;;; Contains: Lisp-implemmentation-specific prep code
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: November 1994
;;; Updated: Fri Feb  1 22:36:56 2008 by Jeff Dalton
;;; Copyright: (c) 1994, 1995, 1996 AIAI, University of Edinburgh


#+ansi-cl (in-package :common-lisp-user)
#-ansi-cl (in-package :user)



;;;; GCL 2.[012]

#+gcl
(let ((mv (find-symbol "*GCL-MAJOR-VERSION*" :system)))
  (when (and mv (eql (symbol-value mv) 2))
    (push :gcl-2 *features*)
    #+|FreeBSD|
    (push :freebsd *features*)))

;; Try to figure out if we're linux and not already #+:linux
#+(and :gcl (not :linux) (not :sun) (not |FreeBSD|))
(push :linux *features*)

#+:gcl-2
(progn

  (format t "~&GCL-2 init~%")

  (si:use-fast-links nil)
  (proclaim '(optimize (safety 0)))
  (setq si::*multiply-stacks* 4)
  ; (setq *load-verbose* nil)
  ; (sys:catch-bad-signals)

  (import 'defpackage:defpackage :lisp)
  (export 'defpackage:defpackage :lisp)

  (push :no-c-code *features*)

); end #+:gcl-2



;;;; AKCL and GCL-1.1 initialization

#+(and akcl (not :gcl-2))
(progn

  (export 'use-fast-links :si)		;compatibility with gcl-2

  (lisp:use-fast-links nil)
  
  ;; (proclaim '(optimize (safety 2)))
  (proclaim '(optimize (safety 0)))
  (setq si::*multiply-stacks* 4)

  (sys:catch-bad-signals)

  (unintern 'lisp)			;/\/ where did LISP come from?

  (setq *load-verbose* nil)		;/\/ cannot otherwise suppress some
					;    output from loading .o files.
); end #+(and akcl (not :gcl-2))



;;;; All KCL

#+kcl
(progn

  (unless (find-package "CL")
    (assert (not (find-package "COMMON-LISP")))
    (rename-package (find-package "LISP") "LISP" '("CL")))

  (compiler::DEFSYSFUN 'PARSE-INTEGER
      "Lparse_integer" '(T *) '(VALUES T T) NIL NIL)

)



;;;; Lucid CL (LCL) initialization

#+:lucid
(progn

  #+lucid
  (proclaim '(optimize (compilation-speed 0) 	;production mode
	               (speed 2)		;tail-merge off
		       (safety 0)))		;no arity or r/w arg checks

  #+lucid-development
  (proclaim '(optimize (compilation-speed 3)
	               (speed 0)
	               (safety 3)))

  (setq lcl:*redefinition-action* :warn) 	; or :query

  (push "lsp" lcl:*load-source-pathname-types*)

  #+(and solaris (not liquid))
  (setq lcl:*load-binary-pathname-types* (list "s2bin"))

  (setq *load-verbose* nil)

  ;; Avoid conflicts with, e.g., lcl:monitor.
  (setq lcl:*default-make-package-use-list*
	(list (find-package #+liquid "COMMON-LISP" #-liquid "LISP")))
  #-liquid
  (unuse-package :lcl)

  #+liquid
  (setq liquid::*warn-if-toplevel-proclaim* nil)

  #+liquid
  (defmacro in-package (name &rest args)
    `(eval-when (eval compile load)
       (let ((.name. ,name))
         (unless (find-package .name.)
           (make-package .name. ,@args))
         (setq *package*
               (or (find-package .name.)
                   (error "Can't do in-package for name ~S." .name.))))))

  ;; Arrange for some CL additions to work:

  #-liquid
  (progn

    (import '(lcl:defpackage lcl:restart-case lcl:continue
	      lcl:destructuring-bind)
	    :lisp)

    (export '(lcl:defpackage lcl:restart-case lcl:continue
	      lcl:destructuring-bind)
	    :lisp)

    (import '(lcl:defclass lcl:defgeneric lcl:defmethod lcl:make-instance)
	    :lisp)

    (export '(lcl:defclass lcl:defgeneric lcl:defmethod lcl:make-instance)
	    :lisp)

  )

); end #+:lucid



;;;; Allegro 4.3 and 5.0 initialization

;;; N.B. O-Plan does not work in earlier versions of Allegro CL.

;;; /\/: Check that it still works in 4.3.

#+:allegro
(progn

  (push (make-pathname :type "lsp")
	(cddr sys:*load-search-list*))

  (setq excl:*cltl1-in-package-compatibility-p* t)
  (setq comp:*cltl1-compile-file-toplevel-compatibility-p* t)

  (setq lisp:*compile-print* nil)
  (setq lisp:*compile-verbose* nil)

); end #+:allegro


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
