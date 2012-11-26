;;; -*- Package: LISP -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; DEFPACKAGE
;;;
;;; Written Dan Zigmond.
;;;
;;; Origin:
;;; Received in e-mail from Rob.MacLachlan@edu.cmu.cs.slisp.fred,
;;; 17 January 1991, by J.Dalton@uk.ac.ed, who modified it for KCL.
;;; Corrected version received 30 January 1991:
;;;
;;;    That DEFPACKAGE I sent you had not yet been integrated into our
;;;    system, and on doing so, I found that it was rather broken.  A
;;;    fixed version follows.  Note that this code now assumes that
;;;    SHADOW will accept strings, which is another X3J13 cleanup.  If
;;;    you don't have this change, you can use MAKE-SYMBOL to
;;;    symbolify the strings.
;;;

(in-package "LISP")

(export 'defpackage)

(defmacro defpackage (package &rest arguments)
  "Defines a new package called PACKAGE.  ARGUMENTS should a list of forms,
   each of which is one of:
       (:SIZE <integer>)
       (:NICKNAMES {package-name}*)
       (:SHADOW {symbol-name}*)
       (:SHADOWING-IMPORT-FROM <package-name> {symbol-name}*)
       (:USE {package-name}*)
       (:IMPORT-FROM <package-name> {symbol-name}*)
       (:INTERN {symbol-name}*)
       (:EXPORT {symbol-name}*)
   All keywords except :SIZE can be used multiple times."
  (let ((body nil)
	(n-package (gensym))
	(package-name
	 (etypecase package
	   ;; Make sure we have a good package name to use.
	   (string package)
	   (symbol (symbol-name package)))))
    (multiple-value-bind
	(nicknames uses shadows imports shadowed-imports exports interns size)
	(parse-defpackage-keywords arguments n-package)
      ;; We set up the body of the form to return first things first
      ;; for readability, even though (since we're using PUSH) we
      ;; then have to NREVERSE at the end.  The order of operations
      ;; must be: 1. :shadow and :shadowing-import-from
      ;;          2. :use
      ;;          3. :import-from and :return
      ;;          4. :export
      (when shadows
	(push `(shadow (list ,@shadows) ,package-name)
	      body))
      (when shadowed-imports
	(push `(shadowing-import (list ,@shadowed-imports) ,package-name)
	      body))
      (when uses
	(push `(use-package (list ,@uses) ,package-name)
	      body))
      (when imports
	(push `(import (list ,@imports) ,package-name)
	      body))
      (when interns
	(dolist (symbol interns)
	  (push `(intern ,symbol ,package-name)
		body)))
      (when exports
	(push `(export (list ,@exports) ,package-name)
	      body))
      ;;
      ;; We do :nicknames and :sizeat the top (where it's convenient).
      ;; :Size is not implemented very well.  We assume, for absolutely
      ;; no good reason, that approximates 1/5 of the symbols in a
      ;; package will be external.
      ;; /\/: Need eval-when at least in AKCL [jwd]
      `(eval-when (eval compile load)
	 (let ((,n-package
		(or (find-package ,package-name)
		    (make-package
		     ,package-name
		     ,@(if nicknames `(:nicknames (list ,@nicknames)))
		     ,@(if size `(:internal-symbols ,(round size 5/4)
				:external-symbols ,(round size 5)))))))
	   ,@(nreverse body)
	   ,n-package)))))


(defun parse-defpackage-keywords (rest-list n-package)
  "Parses the arguments to DEFPACKAGE.  Returns eight arguments:
       1. A list of the package's nicknames.
       2. A list of the other packages that this package uses.
       3. A list of shadows.
       4. A list of lists of the form (package-name {symbol-name}*)
          describing the symbols to be imported from package-name
	  and placed on the shadowed symbols list.
       5. A list of lists as above of symbols to be imported.
       6. A list of symbols to export.
       7. A list of symbols to intern.
       8. The declared size of the package.
   Nil is returned as any of these eight values if no value is provided
   by the user.  Only mimimal error checking is done here."
  (do* ((symbols-in nil)
	(symbols-out)
	(nicknames nil)
	(uses nil)
	(shadows nil)
	(imports nil)
	(shadowed-imports nil)
	(exports nil)
	(interns nil)
	(size nil)
	(remaining-args rest-list (rest remaining-args))
	(current-keyword (first (first remaining-args))
			 (first (first remaining-args)))
	(current-args (rest (first remaining-args))
		      (rest (first remaining-args))))
       ((endp remaining-args)
	(values nicknames
		uses
		shadows
		imports
		shadowed-imports
		exports
		interns
		size))
    (case current-keyword
      (:nicknames
       (setf nicknames (append nicknames (stringify-symbols current-args))))
      (:use
       (setf uses (append uses (stringify-symbols current-args))))
      (:shadow
       (setf current-args (stringify-symbols current-args))
       (setf symbols-in (append-but-lose-if-overlap symbols-in current-args))
       (dolist (string current-args)
	 (push string shadows)))
      (:shadowing-import-from
       (setf current-args (stringify-symbols current-args))
       (setf symbols-in (append-but-lose-if-overlap symbols-in
						    (rest current-args)))
       (dolist (string (rest current-args))
	 (push `(find-symbol-or-lose ,string ,(first current-args))
	       shadowed-imports)))
      (:import-from
       (setf current-args (stringify-symbols current-args))
       (setf symbols-in (append-but-lose-if-overlap symbols-in
						    (rest current-args)))
       (dolist (string (rest current-args))
	 (push `(find-symbol-or-lose ,string ,(first current-args))
	       imports)))
      (:export
       (setf symbols-out (append-but-lose-if-overlap symbols-out
						     (rest current-args)))
       (dolist (string (stringify-symbols current-args))
	 (push `(intern ,string ,n-package)
	       exports)))
      (:intern
       (setf current-args (stringify-symbols current-args))
       (setf symbols-in (append-but-lose-if-overlap symbols-in current-args))
       (setf symbols-out (append-but-lose-if-overlap symbols-out current-args))
       (setf interns (append interns current-args)))
      (:size
       (if (null size)
	   (if (= (length current-args) 1)
	       (setf size (first current-args))
	       (error "Too many arguments to :SIZE keyword in DEFPACAKGE."))
	   (error ":SIZE keyword used more than once in DEFPACKAGE.")))
      (otherwise
       (error "Bad keyword passed to DEFPACKAGE: ~S." current-keyword)))))

(defun find-symbol-or-lose (symbol package)
  "Tries to find SYMBOL in PACKAGE, but signals a continuable error if
   it's not there."
  (or (find-symbol symbol package)
      (cerror "Ignore this symbol." "Can't find the symbol named ~S in ~S."
	      symbol package)))

(defun stringify-symbols (symbols)
  "Takes a list of symbols and/or strings and returns a list of
   strings using SYMBOL-NAME for any necessary coersion."
  (mapcar #'(lambda (x)
	      (etypecase x
		(string x)
		(symbol (symbol-name x))))
	  symbols))

(defun append-but-lose-if-overlap (list-one list-two &key (test #'string=))
  "APPENDs two lists but screams if they intersect at all.
   Uses STRING= as default test because that's what DEFPACKAGE wants to use."
  (if (intersection list-one list-two :test test)
      (error "Overlap found in argument lists.")
      (append list-one list-two)))

