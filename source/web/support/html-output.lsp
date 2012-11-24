;;;; File: html-output.lsp
;;; Contains: HTML output support for Web / CGI demos
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: October 1994
;;; Updated: Thu May 27 01:38:53 1999 by Jeff Dalton
;;; Copyright: (c) 1994 - 1997 AIAI, University of Edinburgh

(in-package :oplan)

;;; Contents:
;;;
;;;  * HTML output support
;;;     - Basic output routines
;;;     - Blocks
;;;     - Forms
;;;     - Tables
;;;     - Buttons
;;;     - Pages
;;;     - Condition reporter
;;;     - Debugging and information
;;;     - Encoding for PRE blocks

;;; This is a somewhat _ad hoc_ collection of routines that are useful
;;; when outputting HTML.  There's no attempt to systematically cover
;;; HTML in a uniform way, or to completely abstract away from the
;;; textual details of HTML representation.  For instance, you might
;;; write (html-block "ul" ...) and hence have to know that "ul" was
;;; HTML for "unordered list".  (It's arguable that HTML is already
;;; abstract enough.)


;;;; HTML output

;;; HTML output produced by the routines in this section goes to the
;;; stream *html-out*.  Normally this will be the same as *terminal-io*.

;;; The variable *in-html* is used to indicate whether the main output
;;; that might be HTML should actually be HTML.  For instance, suppose
;;; one of the O-plan Web demos is producing output when an error occurs.
;;; The error handler might want to put an error message into the output,
;;; and, if so, will want to know whether to use HTML or plain text.

;;; This is obviously a rether simplistic system.  The "right way" to do
;;; it would probably be to make HTML / not-HTML a property of streams,
;;; rather than a global property of an ill-defined "main output".
;;; But we don't actually need anything that sophisticated.

(defparameter *html-out* nil)		;stream to send HTML output to

(defvar *in-html* t)

(defun ensure-html-out ()
  (declare (special *agent-io*))
  (if *agent-io*
      (error "Can't do HTML output in connect mode.")
    (ensuref *html-out* *terminal-io*)))


;;; Basic output routines

;;; All HTML output normally goes through html-output, html-format,
;;; html-xp-format, or html-line.

;;; Html-text-of can be wrapped around calls to html output routines
;;; to obtain the output as a string.

;;; /\/: The HTML output is "line-oriented", but this can produce less
;;; than perfect results when the HTML is formatted.  For instance, a
;;; newline before a </td> can result in a little extra white space.

(defmacro html-output (&rest directives)
  `(output (:stream (ensure-html-out))
     ,@directives))

(defmacro html-text-of (&rest forms)
  `(let ((*html-out* (make-string-output-stream)))
     ,@forms
     (get-output-stream-string *html-out*)))

(defun html-format (format-string &rest format-args)
  (ensure-html-out)
  (apply #'format *html-out* format-string format-args))

(defun html-xp-format (format-string &rest format-args)
  (ensure-html-out)
  (apply #'xp-format *html-out* format-string format-args))

(defun html-line (format-string &rest format-args)
  (ensure-html-out)
  (format *html-out* "~&~?~%" format-string format-args))

(defun html-tag (tag format-string &rest format-args)
  (check-type tag string)
  (unwind-protect
       (html-format "<~A>~?" tag format-string format-args)
    (html-format "</~A>" tag)))

(defun html-tag-line (tag format-string &rest format-args)
  (check-type tag string)
  (unwind-protect
       (html-format "~&<~A>~?" tag format-string format-args)
    (html-format "</~A>~%" tag)))

(defun html-anchor (url description)
  ;; We have to be careful about where line breaks appear, to avoid
  ;; getting unintended extra space when the HTML is processed.
  (check-type url string)
  (check-type description string)
  (unwind-protect
       (html-format "~&<A HREF=~S~%>~A" url description)
    (html-format "</A>~%")))

(defun html-tmp-anchor (name type description)
  (html-anchor (web-tmp-url name type) description))


;;; Blocks

;;; /\/: Should the code that outputs the begin tag be inside or outside
;;; the unwind-protect?

;;; html-block allows the type and attributes to be specified by
;;; an expression, and the value of the expression can be a string
;;; or a list.  If it's a string, it is take as-is and should be
;;; along the lines of "type name=value...".  A list should be
;;; (type attribute...) where an attribute is either a string
;;; "name=value" or a list (name value).

;;; Note that in practice one can write ("type name=value..." attribute...),
;;; i.e. putting some attributes together with the type, because the whole
;;; thing is converted straightforwardly to one string.

;;; /\/: At least Netscape 3.x will put a little extra white space
;;; before a </th> or </td> if there's a newline before the </th>
;;; or </td>.  So we may need a way to avoid putting newlines
;;; there, which would require changes in html-block and in (for
;;; instance) html-anchor (so it doesn't put a newline after the
;;; </a>) -- or else some different routines to use instead.

;;; Attributes can be automatically added to tags processed by html-block.
;;; E.g. (html-with-added-attributes '(("td" "align" "right")) ...).
;;; When nested, all additions apply.  Longer example:
;;;
;;;  (html-with-added-attributes
;;;      `(("th" "align" "right" "bgcolor" ,*highlight-color*)
;;;        ("td" "align" "right"))
;;;    ...)

(defvar *html-attribute-additions* nil)

(defmacro html-with-added-attributes (adds &body body)
  `(let ((*html-attribute-additions*
	  (append ,adds *html-attribute-additions*)))
     .,body))


;;; Certain short-forms are allowed in the body of a block:

(eval-when (eval compile load)
  (defun process-html-block-type-and-attributes (type-and-attributes)
    (if (stringp type-and-attributes)
	type-and-attributes
      (if (or (symbolp type-and-attributes) (consp type-and-attributes))
	  ;; An expression -- value might be a string or a list.
	  `(convert-html-block-type-and-attributes-value ,type-and-attributes)
	(error "Illegal html-block type-and-attributes: ~S."
	       type-and-attributes))))
  (defun process-html-body-forms (forms)
    (mapcar #'(lambda (form)
		(cond ((stringp form)
		       `(html-line ,form))
		      (t
		       form)))
	    forms))
)

(defmacro html-block (type-and-attributes &body forms)
  ;; E.g. (html-block "table border=1 cellspacing=0" ...)
  `(do-html-block ,(process-html-block-type-and-attributes
		    type-and-attributes)
                  #'(lambda ()
		      . ,(process-html-body-forms forms))))

(eval-when (eval compile load)

  (defun do-html-block (type-and-attributes thunk)
    (check-type type-and-attributes string)
    (multiple-value-bind (type attributes)
	(break-string-at-first #\space type-and-attributes)
      (let ((adds (find-html-attribute-additions type)))
	(if adds
	    (html-line "<~A ~A ~{~A=~S~^ ~}>" type attributes adds)
	  (html-line "<~A>" type-and-attributes)))
      (unwind-protect
	   (funcall thunk)
	(html-line "</~A>" type))))

  (defun find-html-attribute-additions (target-type)
    (loop for (type . adds) in *html-attribute-additions*
	  when (string-equal type target-type)
	  append adds))

  (defun convert-html-block-type-and-attributes-value (type-and-attributes)
    ;; The result is always a string.
    ;; Type-and-attributes must be a string or list.
    ;; A string is taken as-is and will have a form along the lines
    ;; of "type name=value...".
    ;; A list must be (type attribute...) where the attribute
    ;; is a string name=value or a list (name value).
    (if (stringp type-and-attributes)
	type-and-attributes
      (if (consp type-and-attributes)
	  (big-string-concat
	    (cons
	      (string (car type-and-attributes)) 	;type
	      (prepare-html-block-attributes-list-value
	        (cdr type-and-attributes)))) 		;attributes
	(error "No type for html block."))))

  (defun prepare-html-block-attributes-list-value (attributes)
    ;; Returns a list of strings.  The 1st string, and then every
    ;; other string, will be " ".
    (mapcan #'(lambda (attribute)
		(if (stringp attribute)
		    (list " " attribute)
		  (if (consp attribute)
		      (list " "
			    (format nil "~A=~S"
				    (first attribute)
				    (second attribute)))
		    (error "Illegal html-block attribute: ~S"
			   attribute))))
	    attributes))

  (defun add-html-block-attributes (type-and-attributes more-attributes)
    ;; This can be used in macro that have a parameter that specifies
    ;; some more attributes.
    (if (null more-attributes)
	type-and-attributes
      (big-string-concat
        (cons (convert-html-block-type-and-attributes-value
	        type-and-attributes)
	      (if (stringp more-attributes)
		  (list " " more-attributes)
		(if (consp more-attributes)
		    (prepare-html-block-attributes-list-value
		      more-attributes)
		  (error "Illegal html-block attributes: ~S"
			 more-attributes)))))))

)

(defmacro html-item (tag-and-attributes &body forms)
  ;; Used in blocks, e.g. for an <LI> item in a <UL>.
  ;; Expands to an html-block, because we want to include even
  ;; optional end tags.
  `(html-block ,tag-and-attributes . ,forms))


;;; /\/: Should paragraphs be done with html-block?

(defmacro html-paragraph (&body forms)
  `(progn
     (html-line "<P>")
     ,@(process-html-body-forms forms)))


;;; Forms

(defmacro html-form (method action &body forms)
  ;; The method and action forms should have values that are strings.
  `(unwind-protect
       (progn (html-line "<form~% method=~A~% action=~S>" ,method ,action)
              ,@(process-html-body-forms forms))
     (html-line "</form>")))

(defun html-checkbox (name &optional checked-p)
  (html-line "<input type=\"checkbox\" name=\"~A\"~:[~; checked~]>"
	     name checked-p))

(defmacro html-select ((&key name size multiple) &rest options)
  `(html-select-from-list
     :name ,name :size ,size :multiple ,multiple :options (list ,@options)))

(defun html-select-from-list (&key name size multiple options)
  ;; An option-value is a symbol, string, or number.
  ;; An option is an option-value or a list (:selected option-value).
  (unwind-protect
      (progn
        (html-line "<select~@[ name=\"~A\"~]~@[ size=~A~]~:[~; multiple~]>"
          name size multiple)
        (dolist (opt options)
          (if (list-beginning :selected opt)
              (html-line "<option selected>~A" (second opt))
            (html-line "<option>~A" opt))))
    (html-line "</select>")))

(defun mark-selected (item list)
  (mapcar #'(lambda (e) (if (equal e item) `(:selected ,e) e))
	  list))


;;; Tables

(defparameter *aiai-light-orange* "#ffeedd")

(defparameter *aiai-medium-orange* "#ffcc99")

(defparameter *html-table-highlight-color* *aiai-light-orange*)

(defun html-empty-td ()
  (html-item "td" "&nbsp;"))

(defun html-empty-th ()
  (html-item "th" "&nbsp;"))

(defmacro html-with-headers-highlighted (&rest forms)
  `(html-with-added-attributes
       `(("th" "bgcolor" ,*html-table-highlight-color*))
     .,forms))

;;; Standard table format

(defmacro html-aiai-table ((&optional more-attributes) &rest body)
  (let ((attributes "table border=1 cellspacing=0"))
    `(html-block ,(if more-attributes
		      (concat-string attributes " " more-attributes)
		    attributes)
       (html-with-headers-highlighted
	 .,body))))

;;; Query-arg / Parameter tables

(defun html-horizontal-parameter-table
    (query-arg-descriptions &optional (argval #'query-arg))
  (html-aiai-table ()
    ;; A row containing the keys as headings
    (html-item "tr"
      (dolist (argd query-arg-descriptions)
        (html-item "th align=center"
          (html-format "~A" (keyword->text (q-arg-name argd))))))
    ;; A row containing the values as data
    (html-item "tr"
      (dolist (argd query-arg-descriptions)
        (html-item "td align=center"
	  (html-format "~A" (funcall argval (q-arg-name argd))))))))

(defun keyword->text (keyword)
  (string-capitalize (replace-subseq " " "-" (symbol-name keyword))))

;;; Boxes -- 1-entry tables w/ a border

(defmacro html-box (&rest body)
  `(html-block "table border=1 cellspacing=0"
     (html-item "tr"
       (html-item "td"
         . ,body))))

;;; Boxed <pre> output

(defmacro html-pre-box (&rest body)
  `(html-box
     (html-block "pre"
       ""				;/\/ symmetry
       ,@body)))

;;; Invisible boxes

;;; That there are uses for invisible boxes (1-element tables wrapped
;;; around more HTML) is perhaps rather surprising.

;;; /\/: This is really just a more general form of box, since
;;; a border could be specified as a :table attribute.

(defmacro html-invisible-box ((&key table td) &rest body)
  ;; Has a size, which affects width=100% inside it.
  ;; May have other uses...
  `(html-block (add-html-block-attributes 
		 "table cellspacing=0 cellpadding=0"
		 ,table)
     (html-item "tr"
       (html-item (add-html-block-attributes "td" ,td)
         . ,body))))

(defmacro html-1-table ((&key table td) &rest body)
  ;; No attributes are pre-specified.
  `(html-block (add-html-block-attributes "table" ,table)
     (html-item "tr"
       (html-item (add-html-block-attributes "td" ,td)
         . ,body))))

;;; Buttons

(defparameter *html-button-bar-background-color* *html-table-highlight-color*)

(defun html-button-bar (buttons)
  (html-aiai-button-bar buttons))

(defun html-aiai-button-bar (buttons)
  ;; A button is (name url).
  (html-with-added-attributes
      `(("tr" "bgcolor" ,*html-button-bar-background-color*))
    (html-block "table border=1 cellspacing=0 cellpadding=2 width=100%"
      (html-block "tr"
        (html-block "td align=center valign=center"
          (html-block "small"
            (loop for ((name url) . more-p) on buttons
		  do (html-anchor url (nbsp-string name))
		  when more-p
		    do (html-format " | "))))))))

(defun nbsp-string (string)		;makes spaces non-breaking
  (replace-subseq "&nbsp;" " " string))

(defun html-text-button-bar (buttons)
  (html-paragraph
    (loop for (name url) in buttons
	  do (html-text-button name url))))

(defun html-text-button (name url)
  (html-format "[")
  (html-anchor url name)
  (html-format "]"))


;;; Pages

(defparameter *html-page-background-color* "#ffffff") ;white

(proclaim '(notinline html-standard-page-foot))	;redefinable

(defmacro html-standard-page ((&rest keyword-args) &body forms)
  `(do-html-standard-page
     #'(lambda () . ,forms)
     ,@keyword-args))

(defun do-html-standard-page
          (thunk &key title title-header centered-title-header)
  ;; title-header is true to repeat the title as an h1 header.
  ;; centered-title-header is true to have the h1 header centered.
  (check-type *html-page-background-color* string)
  (when title
    (check-type title string)
    (html-tag-line "title" title))
  (html-block (format nil "body bgcolor=~S" *html-page-background-color*)
    (when title
      (cond (title-header
	     (html-tag-line "h1" title))
	    (centered-title-header
	     (html-block "center"
	      (html-tag-line "h1" title)))))
    (funcall thunk)
    (html-standard-page-foot)))

(defun html-standard-page-foot ()
  ())


;;; Condition reporter

(defun html-report-condition (c)
  (html-paragraph
    (html-line "Processing stopped because the following condition")
    (html-line "was signalled:")
    (html-block "PRE"			;in a blockquote? /\/
      (html-format "~A"
	(html-encode-pre-string (princ-to-string c))))))


;;; Debugging and information

(defun html-describe-query-args ()
  (declare (special *query-arg-table*))
  (html-paragraph
    (html-line "Query-argument values:")
    (html-block "PRE"
      (loop for (name . value)
	    in (canonical-description-order
		(hash-table-alist *query-arg-table*))
	    do (html-line "  ~S = ~S" name value)))))

(defun html-print-oplan-greeting ()
  (html-block "PRE" (print-oplan-greeting *html-out*)))

(defmacro html-time (form)
  `(html-block "PRE" (time ,form)))

(defun html-report-plan-statistics (&optional stats)
  (html-paragraph
    (html-line "Planning statistics:")
    (html-block "PRE"
      (loop for (stat . value) in (or stats (request-plan-statistics-list))
	    do (html-line " ~3T ~S ~23T = ~S" stat value)))))


;;; Length for strings containing HTML tags

(defun html-print-length (string)
  ;; Doesn't count chars in <...>.  Assumes no <...>s are nested.
  (declare (string string))
  (let ((in-tag-p nil)
	(len 0))
    (declare (fixnum len))
    (fix-dotimes (i (length string) len)
      (let ((char (schar string i)))
	(declare (character char))
	(cond ((char= char #\<)
	       (setq in-tag-p t))
	      ((char= char #\>)
	       (setq in-tag-p nil))
	      ((not in-tag-p)
	       (setq len (fix1+ len))))))))


;;; Encoding for PRE blocks

(defparameter *html-pre-char-alist*
  '(( #\< . "&lt;"  )
    ( #\> . "&gt;"  )
    ( #\& . "&amp;" )))

(defun html-encode-pre-string (s)
  ;; Converts chars into char sequences as specified by *html-pre-char-alist*.
  (encode-string s *html-pre-char-alist*))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;

