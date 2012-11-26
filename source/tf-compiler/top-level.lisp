;;;; File: complr.lsp
;;; Contains: Top level of the TF compiler
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: March 1992
;;; Updated: Mon Mar 22 23:33:47 1999 by Jeff Dalton
;;; Copyright: (c) 1992, 1993, 1994, 1995 AIAI, University of Edinburgh

(in-package :oplan-tf-compiler)

;;; Note that there are many places where it looks like we could use
;;; SET-DIFFERENCE.  But we can't (or don't want to) because SET-DIFFERENCE
;;; doesn't guarantee the order of the result.  Consequently, we've defined
;;; a function STABLE-SET-DIFFERENCE.

;;; /\/: It may be better to interleave the two main passes rather
;;; than have such things as the undefined type test happen well after
;;; parsing.  (See how easy the node ref check is, eg. -- in the parser.)
;;; The "semantic" errors might then have line number refs too.


;;; Entry points

;;; Compile-tf-stream is the main entry point to the compiler.

;;; /\/: But the DM actually calls compile-from-tf-file.

;;; /\/: Some comments are needed.

;;; /\/: The check-only parameter isn't necessary a good idea.
;;; It causes the compiler to stop after parsing, but a number of
;;; errors aren't detected until later.  So if you really want to
;;; check a domain definition, you should not use :check-only t.

;;; /\/: It may be best to return NIL as the first value when there
;;; have been errors.

;;; /\/: The TF compiler was supposed to return a list of the new
;;; schemas if adding new TF to already compiled TF, but the list
;;; is no longer returned.  However, making "TF on TF" work properly
;;; may require more than that to change.

(defun compile-tf-stream (stream &optional (input-domain (make-domain))
				           (check-only nil))
  (handler-bind
      ((error
	(if *running-as-tfc*
	    #'tfc-compile-tf-stream-error-handler
	  #'(lambda (c)
	      (dev-debug :force-out "~&Lisp error during compilation:~%~A~%" c)
	      (case (menu-request
		     `("-heading" "Lisp error during compilation"
		       "Ok=:ok"
		       "Break 1st=:break"))
		(:ok)
		(:break (break "TF compiler error")))
	      (incf *error-count*)		;we only _know_ about 1 more
	      (return-from compile-tf-stream
		(values input-domain
			*error-count*
			*warning-count*))))))
    (do-compile-tf-stream stream input-domain check-only)))

(defun do-compile-tf-stream (stream input-domain check-only)
  ;; Parse
  (let* ((output-domain (copy-domain input-domain))
	 (*input-domain* input-domain)
	 (*output-domain* output-domain)
	 (*domain* output-domain)
	 (*tf-stream* stream)
	 (*syntax-check-only* check-only)
	 (*error-count* 0)
	 (*warning-count* 0))

    (set-domain-name output-domain stream)
    ;; N.B. compile-tf rebinds *error-count* and *warning-count*.
    (multiple-value-bind (parse-tree syntax-errors syntax-warnings)
	(compile-tf #'<tf-file> (tf-tokens *tf-stream*))
      (when check-only
	(return-from do-compile-tf-stream
	  (values input-domain
		  syntax-errors
		  syntax-warnings)))

      (setq *error-count* syntax-errors *warning-count* syntax-warnings)

      ;; Generate domain information from the parse tree.
      (construct-domain output-domain parse-tree)

      ;; Produce various tables, etc.
      (analyze-domain output-domain)

      ;; If there are errors, return the domain we were given
      ;; without any modifications.  Otherwise return the modified
      ;; domain.
      (values (if (> *error-count* 0) input-domain output-domain)
	      *error-count*
	      *warning-count*))))

(defun set-domain-name (domain stream)
  (let ((name (handler-case (pathname-name stream) (error () "anonymous"))))
    (setf (domain-name domain)
	  (if (domain-name domain)
	      (concat-string (domain-name domain) "+" name)
	    name))))

;;; For debugging (and other things), we can compile from a string
;;; or a file.

(defun compile-from-tf-string (string &rest other-args)
  (with-open-stream (s (make-string-input-stream string))
    (apply #'compile-tf-stream
	   s
	   other-args)))

(defun compile-from-tf-file (file &rest other-args)
  (with-open-file (s file :direction :input)
    (apply #'compile-tf-stream
	   s
	   other-args)))


;;; Initialization

(define-initializer :dm tf-compiler-init ()
  (initialize-variable-group :tf)
  (call-initializers :tf))



;;; Test files (or, rather, syntax-check them)

;;; Compile-test-files can be used to compile all the ".tf" files in a
;;; directory.  It prints a report of the number of errors in each file
;;; at the end.

;;; Note that you may have to do some work to make sure it interprets
;;; its argument as a directory.  Eg, in KCL you might have to end the
;;; name with a "/", as in: (compile-test-files "tf-examples/").

(defun compile-test-files (directory &key (messages nil))
  (multiple-value-bind (pattern filenames) (directory-files directory "tf")
    (let ((*suppress-messages* (not messages))
	  (error-report '()))
      (dolist (file filenames)
	(tf-compiler-init)
	(when messages
	  (format t "~&--------------------------------------------------~%"))
	(format t "~S:~%" (namestring file))
	(multiple-value-bind (result errors warnings)
	    (compile-from-tf-file (merge-pathnames file pattern))
	  (declare (ignore result))
	  (push (list file errors warnings)
		error-report)))

      ;; Report the number of errors for each file
      (format t "~&--------------------------------------------------~%")
      (format t "~&File ~30T Errors ~40T Warnings")
      (format t "~&---- ~30T ------ ~40T --------")
      (dolist (r (reverse error-report))
	(let ((file (first r))
	      (errors (second r))
	      (warnings (third r)))
	  (if (> warnings 0)
	      (format t "~&~A ~30T ~6A ~40T ~8D"
		      (namestring file) 
		      (if (> errors 0) errors "")
		      warnings)
	    (if (> errors 0)
		(format t "~&~A ~30T ~6D" (namestring file) errors)
	        (format t "~&~A"          (namestring file))))))
      (format t "~%")))
  (values))

(defun directory-files (directory type)
  (let* ((pattern
	  (merge-pathnames (make-pathname :name :wild
					  :type type)
			   (truename directory)))
	 (filenames
	  (mapcar #'(lambda (name)
		      ;; /\/: (enough-namestring name pattern)
		      ;; loses if the name is something like a.b.tf.
		      (make-pathname :name (pathname-name name)))
		  (directory pattern))))
    (values pattern filenames)))

;;; End
