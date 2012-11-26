;;;; File: cgi-env.lsp
;;; Contains: Support for Web / CGI demos
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: October 1994
;;; Updated: Sun May 30 02:02:31 1999 by Jeff Dalton
;;; Copyright: (c) 1994 - 1997 AIAI, University of Edinburgh

(in-package :oplan)

;;; Contents:
;;;  * General framework (description)
;;;  * Web environment / setup
;;;  * Demo classes
;;;  * Tmp directory
;;;     - Routines for creating filenames and URLs
;;;     - Special URLs and CGI URLs
;;;     - Notes file
;;;  * Counters in files
;;;  * Password lookup
;;;  * PATH_INFO parsing
;;;  * Query arg (QUERY_STRING) parsing
;;;  * Query arg checking and conversion
;;;  * HTML output support
;;;     - Links to standard results


;;;; General framework
;;;
;;; This framework is chiefly for non-interactive demos in which O-Plan
;;; is run to produce HTML output.
;;;
;;; For a Web demo D, the O-Plan "source" directory will usually contain
;;; the following files:
;;;   web/D.html           Introduction to the demo plus a FORM
;;;   web/D.cgi            Shell script that runs O-Plan with appropriate args
;;;   web/D-support.lsp    Lisp routines that control the demo
;;;
;;; These files may actually be in one or more subdirectories rather
;;; than directly in "web"; it depends on how much can be the same for
;;; the different hosts on which the demo runs.  For the details, see
;;; web/README and the routines below that construct URLs and file names.
;;; Certain parameters are set by ../lib/web-config.lsp.
;;;
;;; The FORM in D.html will have D.cgi as its ACTION.  How this is
;;; made to work depends on the httpd configuration.  (Perhaps there's
;;; a symbolic link from the CGI bin area to the script, or it may be
;;; necessary to copy the script to the CGI bin area or to take even
;;; more elaborate steps.)
;;;
;;; The D.cgi script will run O-Plan in "subr" mode without windows
;;; and arrange for D-support.lsp to be loaded.  It will also cause a
;;; procedure named D, usually defined in D-support.lsp, to be called.
;;; That procedure is responsible for getting O-Plan to do whatever
;;; the demo requires.  The definition will look something like this:
;;;
;;;    (defparameter *D-parameters* <list of query arg descriptions>)
;;;
;;;    (defun D ()
;;;      (with-web-environment "D"
;;;        (parse-query-args)
;;;        (convert-query-args *D-parameters*)
;;;        ...))
;;;
;;; D should print HTML to the stdout of the Lisp process.  The "html-"
;;; procedures defined in html-output.lsp and below are a convenient
;;; way to do this.  The HTML can contain hypertext links to any files
;;; produced by the demo (such as a PostScript graphs).  Such files
;;; are normally placed in a tmp directory as described in a later
;;; section.
;;;
;;; A typical D.cgi script might be:
;;;
;;;    #!/bin/bash
;;;
;;;    echo Content-type: text/html
;;;    echo
;;;
;;;    cd the-root-oplan-directory/source
;;;
;;;    exec ./web/oplan-cgi -load web/D-support -do "(D)"
;;;
;;; Here the script produces the initial lines of output that precede
;;; HTML text.  Doing this in the script makes it more likely that
;;; some sort of readable output will appear even when things go wrong
;;; on the Lisp side.
;;;
;;; For some demos, the script will do even more of the initial HTML
;;; output.  For instance, it may print the TITLE.
;;;
;;; When the script does not output the content-type header, it is
;;; up to D-support.lsp to do so.  It can do that by calling write-
;;; cgi-response-headers.  However, before that, it should as soon as
;;; possible (setq *cgi-content-type* nil).  That will allow the
;;; error-handling code to determine that the headers have not yet
;;; been sent.  write-cgi-response-headers will set *cgi-content-type*
;;; to the correct value.
;;;
;;; oplan-cgi is a script that runs O-Plan, inserting the command-line
;;; arguments that indicate subr mode, no windows, etc.  It can also
;;; set a time limit.
;;;
;;; Note that it may be necessary to run oplan-cgi using a name such as
;;; "./web/oplan-cgi" that contains a "/", because the current directory
;;; may not be in the httpd user's PATH.
;;;


;;;; Web environment / setup

(defvar *web-demo-name* nil)		;a string that identifies the demo
(defvar *web-notes* nil)		;an open stream or else nil

(defvar *cgi-content-type* "text/html")	;a MIME type or else nil

(defmacro with-web-environment (demo-name &body body)
  `(let ((*web-demo-name* ,demo-name))
     (call-with-web-environment
       #'(lambda () ,@body))))

(defun call-with-web-environment (thunk)
  (check-type *web-demo-name* string)
  (setq *web-notes* nil)
  (catch :web-demo-exit
    (handler-case
        (progn
	  (establish-web-setup)
	  (go-faster)			;may decrease our ability to debug
	  (funcall thunk))
      (condition (c)
	(handler-case
	    (web-demo-report-condition c)
	  (condition (c2)
	    ;; Try to make a note in the log file
	    (web-note "~%Problem reporting condition ~S:~%~A~%~%"
		      c c2)))))))

(defun exit-web-demo ()
  (throw :web-demo-exit nil))

(defun establish-web-setup ()
  ;; /\/: More checks to be filled-in.
  ;; Checks
  (assert (stringp *web-demo-name*))
  ;; Setup
  (set-parameter :oplan-tf-dir
    (concat-string (get-parameter :oplan-dir) "/source/web/demo"))
  (ensure-web-demo-dir)
  (generate-web-demo-id)
  (ensure-web-special-url)
  (ensure-web-cgi-url)
  )

;;; Content-type

(defun write-cgi-response-headers (content-type &rest more-headers)
  (check-type content-type string)
  (output "Content-type: " content-type //)
  (dolist (h more-headers)
    (output h //))
  (output //)
  (setq *cgi-content-type* content-type))


;;; Condition reporter

(defun web-demo-report-condition (c)
  ;; Make a note of the problem.
  (web-note-failure c)
  ;; Tell the user, if possible.
  (cond ((null *cgi-content-type*)
	 ;; Headers have not yet been sent, so we can send them.
	 (write-cgi-response-headers "text/html")
	 (setq *in-html* t)
	 (html-report-condition c))
	((string-equal *cgi-content-type* "text/html")
	 (html-report-condition c))
	((string-equal *cgi-content-type* "text/plain")
	 (format t "~&~%Processing stopped because:~%~%~A~%~%" c))
	(t
	 ;; Can't tell the user.
	 )))


;;;; Demo classes

(defvar *demo* 'not-a-web-demo)

(defclass web-demo ()
  ((name :accessor demo-name
	 :initarg :name
	 :initform *web-demo-name*)
   (tf-file :accessor demo-tf-file
	    :initarg :tf-file)
   (tf-url  :accessor demo-tf-url
	    :initarg :tf-url)))


;;;; Tmp directory for files created by Web demos
;;;
;;; After ensure-web-demo-dir and generate-web-demo-id have been called
;;; (usually by establish-web-setup, above), the following functions
;;; can be called to produce names of temporary files and URLs that
;;; refer to the same files:
;;;
;;;   (web-tmp-filename <name> <type>)
;;;   (web-tmp-url <name> <type>)
;;;
;;; The name describes the file contents; the type is a file type
;;; (extension) such as "ps" for PostScript or "html" for HTML.
;;; The type "txt" is often used for plain (non-HTML) text.
;;;
;;; File names and URLs relative to the main Web demo directory
;;; (which contains the tmp directory as a subdirectory) can be
;;; created by
;;;
;;;   (web-demo-filename <filename>)
;;;   (web-demo-url <filename>)
;;;
;;; However, for these functions to work some setup may be required in
;;; addition to the parameters manipulated here.  This may involve
;;; symbolic links and perhaps even changes to the httpd configuration.
;;; The setup used on AIAI machines is described in Notes/web-setup.
;;;
;;; *web-demo-dir-name* is name of the Web demo directory.  It is normally
;;; "source/web/demo" in the main O-Plan directory.  This directory must
;;; contain a subdirectory called "tmp".
;;;
;;; *web-demo-dir-url* is a URL that refers to the Web demo directory.
;;; Symbolic links might be needed to set this up.
;;;
;;; *web-demo-id* is used to create unique filenames.  The id is
;;; determined by generate-web-demo-id.
;;;
;;; When O-Plan is installed, the "tmp" directory is initially
;;; a symbolic link to "web-tmp" in the main O-Plan directory.
;;; "web-tmp" must be created by by hand.  It should be writable
;;; by everyone (or at least by whatever user is used by the http
;;; daemon, typically "nobody").  Note that the http configuration
;;; may restrict the use of symbolic links.
;;;

(defparameter *web-demo-dir-name* nil)
(defparameter *web-demo-dir-url* nil)

(defvar *web-demo-id* nil)

(defun ensure-web-demo-dir ()
  (unless *web-demo-dir-name*
    (setq *web-demo-dir-name* (default-web-demo-dir-name)))
  (unless *web-demo-dir-url*
    (setq *web-demo-dir-url* (default-web-demo-dir-url))))

(defun default-web-demo-dir-name ()
  (concat-string (get-parameter :oplan-dir) "/source/web/demo"))

(defun default-web-demo-dir-url ()
  (error "The web config file must set *web-demo-dir-url*"))

(defun generate-web-demo-id ()
  (let ((random-state (make-random-state t)))
    (loop
      (let* ((id (int->string (random 10000 random-state)))
	     (guard-name
	      (concat-string *web-demo-dir-name* "/tmp/"
			     *web-demo-name* "-" id "-info"))
	     (guard-stream
	      (open guard-name
		    :direction :output
		    :if-exists nil)))
	(when guard-stream
	  ;; We've found an id all our own.
	  (setq *web-demo-id* id)
	  ;; And we can record some things in the guard file.
	  (with-open-stream (s guard-stream)
	    (let ((*trace-output* s))		;as in use-web-demo-notes-file
	      (record-web-demo-info s)))
	  ;; Re-open the guard file and use it for notes
	  (use-web-demo-notes-file guard-name)
	  ;; Done.
	  (return id))))))

(defun record-web-demo-info (stream)
  (flet ((line (format-string &rest format-args)
	   (format stream "~&~?~%" format-string format-args)))
    (line "~:(~S~) demo" *web-demo-name*)
    (line "")
    ;; O-Plan version
    (print-oplan-greeting stream)
    (line "")
    ;; Environment
    (line "REMOTE_ADDR=~A" (getenv "REMOTE_ADDR"))
    (line "REMOTE_HOST=~A" (getenv "REMOTE_HOST"))
    (line "")
    (line "REQUEST_METHOD=~A" (getenv "REQUEST_METHOD"))
    (line "PATH_INFO=~A" (getenv "PATH_INFO"))
    (line "")
    ;; Query-args
    (describe-query-args-before-parsed stream)
    (line "")
    ;; Parameters
    (line "Parameter values:")
    (loop for (p . v) in (parameter-alist) do
      (line "  ~S = ~S" p v))
    (line "")))

(defun web-tmp-filename (name type)
  ;; N.B. Don't call until after establish-web-setup has been called.
  (concat-string *web-demo-dir-name* "/tmp/"
		 *web-demo-name*
		 "-" *web-demo-id*
		 "-" name
		 "." type))

(defun web-tmp-url (name type)
  ;; N.B. Don't call until after establish-web-setup has been called.
  (concat-string *web-demo-dir-url* "/tmp/"
		 *web-demo-name*
		 "-" *web-demo-id*
		 "-" name
		 "." type))

(defun web-demo-filename (filename)
  ;; N.B. Don't call until after establish-web-setup has been called.
  (concat-string *web-demo-dir-name*
		 "/" filename))

(defun web-demo-url (filename)
  ;; N.B. Don't call until after establish-web-setup has been called.
  (concat-string *web-demo-dir-url*
		 "/" filename))

(defun web-demo-trace-filename ()
  ;; After generate-web-demo-id has determined *web-demo-id*,
  ;; this function will return the name of the guard / info file
  ;; in case we want to send more output to it.
  (concat-string *web-demo-dir-name* "/tmp/"
		 *web-demo-name*
		 "-" *web-demo-id*
		 "-info"))


;;; Special URLs

;;; These refer to a site-specific directory, which is unfortunately
;;; often necessary for anything that involves CGI, since one is typically
;;; at the mercy of whoever controls the httpd configuration.  /\/

;;; The basic idea is that web-demo-url refers to files, typically HTML
;;; files, that are the same for all sites, though the name of the directory
;;; that contains them might differ.  Web-special-url refers to files that
;;; must be diffferent at different sites, e.g. HTML files that contain
;;; links or other references (e.g. form actions) that cannot be to the same
;;; URLs at all sites.  (The mail-comment form referred to below is an
;;; example.)

(defparameter *web-special-url* nil)

(defun ensure-web-special-url ()
  (unless *web-special-url*
    (setq *web-special-url* (default-web-special-url))))
  
(defun default-web-special-url ()
  (error "The web config file must set *web-special-url*"))

(defun web-special-url (filename)
  (concat-string *web-special-url* "/" filename))

(defun web-mail-comment-link (&optional (description "Mail a comment"))
  (html-anchor (web-special-url "comment-form.html")
	       description))


;;; CGI URLs

;;; A CGI URL refers to a CGI executable.  As for special URLs, a
;;; site-specific directory (perhaps the same site-specific directory)
;;; is involved.

(defparameter *web-cgi-url* nil)

(defun ensure-web-cgi-url ()
  (unless *web-cgi-url*
    (setq *web-cgi-url* (default-web-cgi-url))))

(defun default-web-cgi-url ()
  (error "The web config file must set *web-cgi-url*."))

(defun web-cgi-url (filename)
  (concat-string *web-cgi-url* "/" filename))


;;; Notes file

;;; This file is established by the standard setup code (see above).

;;; Trace-output is directed to this file as an aid to debugging.

(defun use-web-demo-notes-file (filename)
  (setq *web-notes*
	(open filename :direction :output
	               :if-exists :append))
  (setq *trace-output* *web-notes*)
  (add-exit-action
    #'(lambda ()
	(close *web-notes*))))

(defun web-note-p ()			;can we write a web-note?
  (and (streamp *web-notes*)
       #+:cltl2
       (open-stream-p *web-notes*)))

(defun web-note (format-string &rest format-args)
  (apply #'format *web-notes* format-string format-args)
  (finish-output *web-notes*))

(defun web-note-success ()
  (web-note "~&RESULT: ~A wins!~%~%" (getenv "REMOTE_HOST")))

(defun web-note-failure (reason)
  ;; N.B. Some "failures" are expected: e.g. when there's supposed
  ;; to be no plan and there isn't one.
  (when (web-note-p)
    (web-note "~&RESULT: ~A loses because ~A.~%~%"
	      (getenv "REMOTE_HOST")
	      reason)))


;;;; Counters (in files)

;;; /\/: Probably doesn't work correctly if > 1 demo is counting
;;; using the same file at the same time.

(defun incf-file-counter (&optional (filename (web-demo-counter)))
  (with-open-file (counter filename
			   :direction :io
			   :if-exists :overwrite
			   :if-does-not-exist :create)
    (let* ((line (read-line counter nil "0"))
	   (n (1+ (string->int line))))
      (assert (file-position counter :start))
      (prin1 n counter)
      (terpri counter)
      n)))

(defun web-demo-counter ()
  ;; N.B. Don't call until after establish-web-setup has been called.
  (concat-string *web-demo-dir-name* "/tmp/"
		 *web-demo-name*
		 "-count"))


;;;; Password lookup

;;; The file contains lines of the form user:password.

(defparameter *password-file* "../lib/web-passwords")

(defvar *passwords* nil)

(defun correct-password-p (user password)
  (ensuref *passwords* (read-password-table))
  (let ((entry (assoc user *passwords* :test #'string-equal)))
    ;; Require that (user password) be in *passwords*.
    (or (and entry
	     (string= (cadr entry) password))
	(progn
	  (web-note-failure "Bogus pasword")
	  nil))))

(defun read-password-table ()
  (mapcar #'(lambda (line)
	      (multiple-value-list
	        (break-string-at-first #\: line)))
	  (stream->lines
	    (open *password-file* :direction :input))))


;;;; PATH_INFO parsing

;;; /\/: There should probably be PATH-INFO and GET-PATH-INFO to match
;;; the way query-args are handled.

(defvar *path-info-cache* nil)

(defun get-path-info ()
  (ensuref *path-info-cache*
    (let ((path (getenv "PATH_INFO")))
      ;; When there's "no" path-info, we sometimes get "", sometimes nil.
      ;; This depends on the HTTP server, or something.  /\/
      (if (or (null path)
	      (string= path ""))
	  '()
	;; Looks like there's some contents.
	(let ((parts (break-string-at #\/ path)))
	  (assert (sequence-begins "/" path))
	  (assert (string= (first parts) ""))	;from the initial "/"
	  (rest parts))))))			;drop the resulting initial ""


;;;; Query arg (QUERY_STRING) parsing

;;; The args are a string: name1=value1&name2=value2&...
;;; The names are converted to keyword symbols by PARSE-QUERY-ARGS
;;; and stored, with their corresponding values, in a hash table.
;;; The values can then be converted from strings to symbols, ints,
;;; etc, by CONVERT-QUERY-ARGS.

;;; After parsing, (QUERY-ARG keyword) returns the corresponding value.

;;; Note that the names and values are encoded using the conventions
;;; for URLs.  We do not do any decoding automatically, since most
;;; args from our forms will avoid most special characters.  Space
;;; turns into "+", and we leave it as "+".  However, the procedure
;;; decode-query-string can be called if necessary.  It replaces + by
;;; space and %xx (% followed by two hex digits) by the corresponding
;;; character.

;;; Parsing ignores the type and value information that is used in
;;; checking and conversion.  A perhaps non-obvious consequence of
;;; this is that an arg that's supposed to be a list of values may
;;; not end up as a list, because a list results only when the 2nd
;;; value comes along (see add-query-value).  Convert-query-args
;;; fixes this, because it knows the type.

;;; Demos that have passwords should call the corresponding parameter
;;; "password" so that we don't note the password in the log file (see
;;; describe-query-args-before-parsed).  [/\/: A more general approach
;;; should be devised.]

(defvar *query-arg-table* (make-hash-table :test #'eq))

(defun query-arg (kwd)
  (gethash kwd *query-arg-table*))

(defun set-query-arg (kwd value)
  (setf (gethash kwd *query-arg-table*) value))

(defsetf query-arg set-query-arg)

(defun parse-query-args (&optional (args (query-arg-string)))
  (check-type args string)
  (clrhash *query-arg-table*)
  (dolist (arg (break-string-at #\& args))
    (let* ((pair (break-string-at #\= arg))
	   (name (first pair))
	   (value (second pair)))
      (add-query-value (string->keyword (string-upcase name))
		       value))))

(defun add-query-value (kwd value)
  (multiple-value-bind (val val-p) (gethash kwd *query-arg-table*)
    ;; If there's already one value, we make a list of values.
    ;; If there's alreay a list, we add to the end.
    (setf (gethash kwd *query-arg-table*)
	  (if val-p
	      (if (consp val) (nconc val (list value)) (list val value))
	    value))))

(defun describe-query-args-before-parsed (stream)
  (flet ((line (format-string &rest format-args)
	   (format stream "~&~?~%" format-string format-args)))
    (line "Query-argument values:")
    (loop for assign
	  in (break-string-at #\& (query-arg-string))
	  do (if (sequence-begins "password=" assign)
		 (line "  password=<secret>")
	       (line "  ~A" assign)))))


;;; Getting the query args

;;; Call QUERY-ARG-STRING which is a memoized GET-QUERY-ARG-STRING.
;;; It handles both GET and POST METHODs.  The arg string should always
;;; be obtained by calling QUERY-ARG-STRING; GET-QUERY-ARG-STRING should
;;; not be called directly.  This is important, because we might
;;; explicitly set what QUERY-ARG-STRING returns (by assigning to
;;; *QUERY-ARG-STRING*), and GET-QUERY-ARG-STRING would ignore this.

(defvar *query-arg-string* nil)

(defun query-arg-string ()
  (ensuref *query-arg-string* (get-query-arg-string)))

(defun set-query-arg-string (string)
  (setq *query-arg-string* string))

(defun get-query-arg-string ()
  (let ((request-method (getenv "REQUEST_METHOD")))
    (cond ((string= "GET" request-method)
	   (getenv "QUERY_STRING"))
	  ((string= "POST" request-method)
	   (get-POST-query-string))
	  (t
	   (error "No query args.")))))

(defun get-POST-query-string (&optional (stream *standard-input*))
  ;; /\/: Should not stop and signal an error when there are no more
  ;; chars, because more might be on the way.  Instead, use select to
  ;; wait with a timeout.
  (let* ((len (string->int (getenv-else-error "CONTENT_LENGTH")))
	 (s (make-string len)))
    (dotimes (i len)
      (let ((char (read-char-no-hang stream))) ;trust no one
	(if char
	    (setf (schar s i) char)
	  (error "Not enough POST input."))))
    s))


;;;; Query arg checking and conversion

;;; Checking and conversion are controlled by argument descriptions
;;; that match the Q-ARG struct defined below.

;;; Type descriptions are:
;;;   (:INT low [high])    An int i such that low <= i [ <= high ].
;;;   (:INT* low [high])   Like :INT but allows "inf" as the upper bound.
;;;   (:NAME n1 n2 ...)    A symbol that is one of n1, n2, ...
;;;   (:NAMES n1 n2 ...)   A list of symbols, each one of n1, n2, ...
;;;   (:STRING)            An arbitrary string.
;;;   (:TEXT)              A "textarea" string.  + and %xx are converted.
;;;   (:SYMBOL)            A symbol.  Like :NAME but no restrictions.
;;;   (:KEYWORD)           A keyword.  The value should not begin with ":".
;;;   (:OPTIONAL type)     Allows the arg to be NIL if not supplied.
;;;   (:CHECKBOX)          Equivalent to (:optional (:name On)).

;;; New types can be defined by define-query-arg-type and define-query-arg
;;; name-type.

;;; /\/: All text types (:name, :names, :string, ...) should have + and %xx
;;; converted.  Right now, only :text does this.

;;; Note that conversion and checking continue with the next argument
;;; after an error.  We try to report all invalid parameters to the user.
;;; This is usually unnecessary, since in many cases (especially SELECT)
;;; a correct HTML FORM shouldn't be able to produce invalid values.
;;; In other cases, users will tend to specify reasonable values unless
;;; trying to break things.

;;; The collected errors are signalled as a new error.  /\/: We should
;;; probably define a new condition type for this case, so that it could
;;; be handled specially at higher levels; but for now, we don't.

(defstruct (q-arg (:type list))
  name					;a keyword
  type					;a type description
  description)				;a string

(defun q-arg-type-name (argd)
  (check-type (q-arg-type argd) cons)
  (car (q-arg-type argd)))

(defun convert-query-args (arg-descriptions)
  (let ((failures '()))
    (dolist (argd arg-descriptions)
      (handler-case (convert-query-arg argd)
	(error (c)
	  (push (list argd c) failures))))
    (when failures
      (error "Errors in query arguments:~%~{  ~S (~A):~%     ~A~^~%~}"
	     (mapcan #'(lambda (failure)
			 (destructuring-bind (argd condition) failure
			   (list (q-arg-name argd)
				 (q-arg-description argd)
				 condition)))
		     failures)))))

(defun convert-query-arg (argd)
  (let ((name (q-arg-name argd))
	(type (q-arg-type argd)))
    (setf (query-arg name)
	  (apply (q-arg-type-converter type)
		 argd
		 (query-arg name)
		 (cdr type)))))

(defmacro define-query-arg-type (name lambda-list &body body)
  (let ((converter-name (concat-name "%" name "-QUERY-ARG-CONVERTER")))
    `(progn
       (setf (get ',name 'query-arg-converter) ',converter-name)
       (defun ,converter-name ,lambda-list
	 ,@body))))

(defun q-arg-type-converter (type)
  (or (get (car type) 'query-arg-converter)
      (error "Unknown query arg type: ~S." type)))

;;; Predefined converters

;;; :int

(define-query-arg-type :int (argd argval low &optional high)
  (unless argval
    (error "No value for ~S." (q-arg-description argd)))
  (let ((i (string->int argval)))
    (if (if high (<= low i high) (<= low i))
	i
      (error "The value of ~S, ~S, is not in the range ~S..~@[~S~]."
	     (q-arg-description argd)
	     argval
	     low
	     high))))

;;; :int*

(define-query-arg-type :int* (argd argval low &optional high)
  ;; "*" can be used instead of "inf".
  (unless argval
    (error "No value for ~S." (q-arg-description argd)))
  (let ((i (string->int* argval)))
    (if (if (numberp i)
	    ;; See if an ordinary int is in range
	    (if high (<= low i high) (<= low i))
	  ;; See if infinity is in range
	  (null high))
	i
      (error "The value of ~S, ~S, is not in the range ~S..~@[~S~]."
	     (q-arg-description argd)
	     argval
	     low
	     high))))

(defun string->int* (string)
  (cond ((string-equal string "inf")
	 :inf)
	((string-equal string "*")
	 :inf)
	(t
	 (string->int string))))

(defun int*-p (x)
  (or (integerp x)
      (eq x :inf)))

;;; :name

(define-query-arg-type :name (argd argval &rest allowed)
  (unless argval
    (error "No value for ~S." (q-arg-description argd)))
  (let ((name (intern (string-upcase argval))))
    (if (member name allowed)
	name
      (error "Invalid value for ~S: ~S;~%~
              Legal values are: ~:(~{~S~^, ~}~)."
	     (q-arg-description argd)
	     argval
	     allowed))))

(defmacro define-query-arg-name-type (type-name allowed-values)
  `(progn
     (setf (get ',type-name 'query-arg-name-type-values) ',allowed-values)
     (define-query-arg-type ,type-name (argd argval)
       (apply (get :name 'query-arg-converter) argd argval ',allowed-values))))

(defun query-arg-name-type-p (type-name)
  (not (eq (get type-name 'query-arg-name-type-values :noprop) :noprop)))

(defun query-arg-name-type-values (type-name)
  (get type-name 'query-arg-name-type-values))

;;; :names

(define-query-arg-type :names (argd argval &rest allowed)
  ;; Allows there to be no names.
  (let ((q-arg-name-converter (get :name 'query-arg-converter)))
    (mapcar #'(lambda (n)
		(apply q-arg-name-converter argd n allowed))
	    (if (listp argval)
		argval
	      (list argval)))))

;;; :string

(define-query-arg-type :string (argd argval)
  (or argval (error "No value for ~S." (q-arg-description argd))))

;;; :text

(define-query-arg-type :text (argd argval)
  (if argval
      (decode-query-string argval)
    (error "No value for ~S." (q-arg-description argd))))

;;; :symbol

(define-query-arg-type :symbol (argd argval)
  (if argval
      (intern (string-upcase argval))
    (error "No value for ~S." (q-arg-description argd))))
  

;;; :keyword

;;; Note that letters are made upper-case and spaces are converted to "-"s.

(define-query-arg-type :keyword (argd argval)
  (if argval
      (string->keyword (replace-subseq "-" " " (decode-query-string argval)))
    (error "No value for ~S." (q-arg-description argd))))

;;; :optional

(define-query-arg-type :optional (argd argval type)
  (if (null argval)
      nil
    (apply (q-arg-type-converter type)
	   argd
	   argval
	   (cdr type))))

;;; :checkbox

(define-query-arg-type :checkbox (argd argval)
  (if (null argval)
      nil
    (let ((name (intern (string-upcase argval))))
      (if (eq name 'on)
	  name
	(error "Checkbox ~S has value ~S instead of \"on\"."
	       (q-arg-description argd)
	       argval)))))


;;; DECODE-QUERY-STRING handles + and %xx decoding.  It returns its
;;; argument unmodified if no decoding is needed.

(defun decode-query-string (input)
  (let ((in-len (length input))
	(in-pos 0)
	(output nil)			;allocated only if needed
	(out-pos 0))			;N.B. out-pos <= in-pos
    (while (< in-pos in-len)
      (let ((char (schar input in-pos)))
	(cond ((char= char #\+)
	       ;; + becomes space
	       (when (null output)
		 (setq output (copy-seq input)
		       out-pos in-pos))
	       (setf (schar output out-pos) #\Space)
	       (incf in-pos)
	       (incf out-pos))
	      ((and (char= char #\%)
		    (< (+ in-pos 2) in-len))
	       (let ((x1 (digit-char-p (schar input (+ in-pos 1)) 16))
		     (x2 (digit-char-p (schar input (+ in-pos 2)) 16)))
		 (cond ((and x1 x2)
			;; %xx becomes the corresponding char
			(when (null output)
			  (setq output (copy-seq input)
				out-pos in-pos))
			(setf (schar output out-pos)
			      (int-char (+ (* 16 x1) x2)))
			(incf in-pos 3)
			(incf out-pos))
		       (t
			;; Not sure if % can occur without xx,
			;; but we allow it.
			(when output
			  (setf (schar output out-pos) char)
			  (incf out-pos))
			(incf in-pos)))))
	      (t
	       ;; An ordinary character: no conversion.
	       (when output
		 (setf (schar output out-pos) char)
		 (incf out-pos))
	       (incf in-pos)))))
    ;; We're done.
    (if output
	(if (= out-pos (length output))
	    output
	  (subseq output 0 out-pos))
      input)))


;;;; HTML output

;;; Links to standard results

(defvar *web-results-wanted* nil)	;fn or list of symbols

(defun html-standard-result-links
    (domain task &optional (*web-results-wanted* #'query-arg))

  ;; A PostScript graph

  (when (web-result-wanted-p :psgraph)
    (request-psgraph :file-one-page
      :title (concat-string domain ":" task)
      :output-file (web-tmp-filename "graph" "ps"))
    (html-item "LI"
      (html-tmp-anchor "graph" "ps" "PostScript graph")
      (html-line "of the plan")))

  ;; Plan narrative

  (when (web-result-wanted-p :narrative)
    (request-plan-view
      :mode :narrative
      :levels :all
      :output-file (web-tmp-filename "narrative" "txt"))
    (html-item "LI"
      (html-tmp-anchor "narrative" "txt" "Plan narrative")))

  ;; KP trace

  (when (web-result-wanted-p :kp-trace)
    (html-item "LI"
      (html-tmp-anchor "kp-trace" "txt" "KP trace output")))

  ;; World at end of node-1
  
  (when (web-result-wanted-p :world-1)
    (request-world-view "1"
      :mode :file
      :output-file (web-tmp-filename "world-1" "txt"))
    (html-item "LI"
      (html-tmp-anchor "world-1" "txt"
        "World state when the plan starts")))

  ;; World at end of node-2

  (when (web-result-wanted-p :world-2)
    (request-world-view "2"
      :mode :file
      :output-file (web-tmp-filename "world-2" "txt"))
    (html-item "LI"
      (html-tmp-anchor "world-2" "txt"
	"World state when the plan finishes"))))

(defun web-result-wanted-p (key)
  (if (functionp *web-results-wanted*)
      (funcall *web-results-wanted* key)
    (member key *web-results-wanted*)))


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
