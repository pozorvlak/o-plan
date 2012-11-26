;;;; File: parser.lisp
;;; Contains: Parser for the TF compiler
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: March 1992
;;; Updated: Sun Aug 22 23:32:54 1999 by Jeff Dalton
;;; Copyright: (c) 1992, 1993, 1994 AIAI, University of Edinburgh

(in-package :oplan-tf-compiler)

;;; /\/: Singular/plural errors are common, so we may want a
;;; special check for them.  An error should still be reported,
;;; but the parser could recover immediately without discarding
;;; any tokens.

;;; /\/: Other common errors may be using "-" for "_" or omitting a
;;; required "_".

;;; /\/: Note that the parser can parse virtually the entire TF language,
;;; including constructs that are not implemented in the planner or, indeed,
;;; in the rest of the TF compiler.


;;; Conventions for parsing procedures (in addition to those in
;;; the "descent" file):
;;;
;;;   * Forms and schema clauses are responsible for their final ";".
;;;     This allows them to use it as the :until parameter to zero-
;;;     or one-or-more.
;;;
;;;   * The names for the procedures that correspond to nonterminals
;;;     begin with "<" and end with ">".  This is just a convention
;;;     and has no effect on how the program works.  "_" in the names
;;;     of nonterminals is replaced by "-" unless the "_" is part
;;;     of the TF language.  E.g., <local_vars-clause>.
;;;
;;;   * The order of procedures in this file is generally top-down,
;;;     and within that tends to follow the order in the TF Manual.
;;;
;;;   * The parser constructs an intermediate result, but one in
;;;     which low-level items (such as node descriptions or type defs)
;;;     are already in pretty much the form they'll take when installed
;;;     in a domain or schema struct.
;;;
;;; Note that it is _not_ the case that there is a procedure for every
;;; nonterminal in the TF Manual.  The parser may omit some nonterminals
;;; that are in the Manual, or it may define them slightly differently.
;;; The parser also defines some nonterminals that are not in the Manual,
;;; e.g. one for each top-level TF form and schema clause.  There are
;;; three reasons for this divergence.  First, the grammar in the Manual
;;; was not designed for recursive-descent parsing (or, indeed, for any
;;; particular technology).  Second, the notation used in the Manual and
;;; our tools for Lisp make different things easier to express.  Third
;;; and finally, the parser's (implicit) grammar has some aims that the
;;; Manual's does not, such as arranging for error recovery and for
;;; useful error messages.
;;;
;;; The nonterminal parsing procedures in this file generally have
;;; the following "shape":
;;;
;;;   (DEFUN nonterminal-name (parameter*)
;;;     (LET (variable*)
;;;       ... parsing ...
;;;       ... result construction ...))
;;;
;;; The parsing code typically calls other procedures to parse
;;; subexpressions and assigns the results of those calls to the
;;; variables.


;;; Parser interface

;;; /\/: It may be that these routines should be called "parse-"
;;; instead of "compile-".  However for low-level items (eg
;;; <min_max_specs>), they do in fact return the final result.

(defun compile-tf-file (filename)
  (with-open-file (*tf-stream* filename :direction :input)
    (compile-tf #'<tf-file>
		(tf-tokens *tf-stream*))))

(defun check-tf-file (filename)
  (let ((*syntax-check-only* t))
    (compile-tf-file filename)))

(defun compile-tf-string (nonterminal string)
  (with-open-stream (*tf-stream* (make-string-input-stream string))
    (compile-tf nonterminal
		(tf-tokens *tf-stream*))))

(defun compile-tf (nonterminal generator)
  (let ((*syntax-error-reporter* 'tf-syntax-error-reporter)
	(*recovering* nil)
	(*error-count* 0)
	(*warning-count* 0)
	(*line-number* 1)
	(*include-stack* '()))
    ;; Make sure *package* uses the TF package.
    (unless (member *tf-language-package* (package-use-list *package*))
      (cerror "Continue."
	      "The package ~S does not use the TF-LANGUAGE package."
	      *package*))
    (with-token-generator generator
      (next-token)
      (values (funcall nonterminal)
	      *error-count*
	      *warning-count*))))

(defun unsupported-syntax (plural-description)
  ;; /\/: Doesn't mention include file (if there is one).
  (tf-error "Line ~D: ~(~A~) are not supported."
	    *line-number* plural-description))


;;; Top-level syntax definitions

(defun <tf-file> ()
  (let ((forms (zero-or-more #'<tf-form> :until *end-token*)))
    (remove nil forms)))

(defun <tf-form> ()
  (tf-progress "~A " (token))
  (prog1 (tf-form-dispatch) (tf-sub-progress "~&")))

(defun tf-form-dispatch ()
  (token-case
    ((tf_info)                (<tf_info-form>))
    ((defaults)               (<defaults-form>))
    ((include)                (<include-form>))
    ((plan_viewer)            (<plan_viewer-form>))
    ((world_viewer)           (<world_viewer-form>))
    ((prefer_plans_with)      (<prefer_plans_with-form>))
    ((prefer_schemas)         (<prefer_schemas-form>))
    ((resource_units)         (<resource_units-form>))
    ((resource_types)         (<resource_types-form>))
    ((resource_conversions)   (<resource_conversions-form>))
    ((impossible)             (<impossible-form>))
    ((domain_rules)           (<domain_rules-form>))
    ((compute_condition)      (<compute_condition-form>))
    ((language)               (<language-form>))
    ((types)                  (<types-form>))
    ((always)                 (<always-form>))
    ((constraint_syntax)      (<constraint_syntax-form>))
    ((schema)                 (<schema-form> 'schema
					     'end_schema))
    ((meta_schema)            (<schema-form> 'meta_schema
					     'end_meta_schema))
    ((process_schema)         (<schema-form> 'process_schema
					     'end_process_schema))
    ((meta_process_schema)    (<schema-form> 'meta_process_schema
					     'end_meta_process_schema))
    ((repair_schema)          (<schema-form> 'repair_schema
					     'end_repair_schema))
    ((initially)              (<initially-form>))
    ((initial_resources)      (<initial_resources-form>))
    ((initial_time)           (<initial_time-form>))
    ((task)                   (<task-form>))
    (t (syntax-error "Invalid TF form starting with \"~A\"" (token))
       (must-be '|;|)         ;recover at next semicolon
       nil)))


;;; tf_info

(defparameter *info-words* '(title author date history description))

(defun <tf_info-form> ()
  (let ((lines (one-or-more #'(lambda ()
				(prog1 (<info-line>) (must-be '|;|)))
			    :until 'end_tf_info)))
    (must-be '|;|)
    (cons 'tf_info lines)))

(defun <info-line> ()
  (let (info-word info-text)
    (setq info-word (must-satisfy #'(lambda (w) (member w *info-words*))
				  "a valid info word"))
    (setq info-text (<text-string>))
    (list info-word info-text)))


;;; defaults

(definit :tf *user-defaults* nil
  "A list of a-lists containing defaults.  The most recent entries
   take precedence.  In effect, this list contains a record of all
   the defaults forms that have been processed by the compiler.")

(defparameter *default-defaults*
  `((value                          . true)
    (variable_restriction           . ,*undef*)
    (condition_node_end             . begin_of)
    (condition_contributor_node_end . end_of)
    (achieve_after_point            . ,(node-end :start :end))
    (effect_node_end                . end_of)
    (resource_usage_node_end        . begin_of)
    (time_window_node_end           . begin_of)
    (link_from_node_end             . end_of)
    (link_to_node_end               . begin_of)
    (resource_overall               . ,(range 0 *infinity*))
    (resource_at_node_end           . ,(range 0 0)))) ; ??? /\/

(defun get-default-value (key)
  (cdr (or (some #'(lambda (a-list) (assoc key a-list))
		 *user-defaults*)
	   (assoc key *default-defaults*)
	   (error "Cannot get default value for ~S." key))))

(defun defaulted-component-p (c)
  (member c *default-defaults* :key #'car))

;;; A "defaults" should affect the rest of the compilation, not just
;;; be parsed.  But should take effect only if it parsed w/o errors.

(defun <defaults-form> ()
  (let* ((errors-before *error-count*)
	 (new-defaults
	  (one-or-more #'<default-assignment> :separator '|,| :until '|;|))
	 (new-errors
	  (- *error-count* errors-before)))
    (when (= new-errors 0)
      (push new-defaults *user-defaults*))
    (cons 'defaults new-defaults)))

(defun <default-assignment> ()
  (let (component value)
    (setq component
	  (must-satisfy #'defaulted-component-p "a default component"))
    (must-be '=)
    (ecase component
      ((nil)				;invalid component
       (setq value nil))
      ((value)
       (setq value (<value>)))
      ((variable_restriction)
       (setq value (<variable-restriction>)))
      ((condition_node_end condition_contributor_node_end
	effect_node_end resource_usage_node_end time_window_node_end
	link_from_node_end link_to_node_end)
       (token-case
	 ((begin_of) (setq value 'begin_of))
	 ((end_of)   (setq value 'end_of))
	 (t (syntax-error "\"~A\" is not a valid default for \"~A\"."
			  (token) component))))
      ((achieve_after_point)
       (setq value (<achieve-after-point>)))
      ((resource_overall resource_at_node_end)
       (setq value (<min-max-spec>))))
    (cons component value)))


;;; Include

;;; The *include-stack* contains only filenames.  The first (car)
;;; entry is the file currently being included.  This information
;;; is used when reporting syntax errors.

(defun <include-form> ()
  (let ((filename (<text-string>))
	(already-in-error *recovering*))
    (must-be '|;|)
    ;; Don't look at the include file if errors may have confused us.
    (unless (or already-in-error *recovering*)
      (let ((stream (try-to-find-include-file filename)))
	(if stream
	    (include-from-stream filename stream)
	  (tf-error "Can't find include file ~S." filename)))))
  nil)

(defun try-to-find-include-file (filename)
  (when (> (length filename) 0)
    (open (defaulted-include-pathname filename)
	  :direction :input
	  :if-does-not-exist nil)))

(defun defaulted-include-pathname (filename)
  ;; If the filename begins with "/" or "~", we take it as-is;
  ;; otherwise it should be relative to the directory containing
  ;; the including TF file.
  (if (find (schar filename 0) "/~")
      ;; Absolute name
      (merge-pathnames filename (make-pathname :type "tf"))
    ;; Relative name
    ;; If *tf-stream* does not provide a pathname, e.g. if it's *terminal-io*,
    ;; we use the :oplan-tf-dir parameter as a base instead.
    (let ((base (handler-case (pathname *tf-stream*)
		  (error () nil))))
      (merge-pathnames
        (if base
	    (relative-pathname base filename)
	  (concat-string (get-parameter :oplan-tf-dir) "/" filename))
	(make-pathname :type "tf")))))

(defun relative-pathname (base name)
  ;; Assumes name is a relative name.
  (let ((d1 (pathname-directory base))
	(d2 (pathname-directory name)))
    (when (list-beginning :relative d2)
      (pop d2))
    (merge-pathnames
      (make-pathname :directory (append d1 d2) :defaults name)
      base)))

(defun include-from-stream (filename stream)
  (let ((saved-token *token*)
	(saved-generator *token-generator*)
	(saved-line-number *line-number*)
	(saved-tf-stream *tf-stream*)
	(gen (tf-tokens stream)))
    (tf-sub-progress "~S" filename)
    (push filename *include-stack*)
    (when (streamp *tf-stream*)
      (setq *tf-stream* stream))
    (setq *line-number* 1)
    (setq *token-generator*
	   #'(lambda ()
	       (let ((token (next-include-token gen)))
		 (when (eql token *end-token*)
		   (tf-sub-progress "~&")
		   (tf-progress "end of include ~S~%" filename)
		   (close stream)
		   (pop *include-stack*)
		   (setq *tf-stream* saved-tf-stream
			 *line-number* saved-line-number
			 *token-generator* saved-generator
			 *token* saved-token
			 token *token*))
		 token)))
    (next-token)))

(defun next-include-token (gen)
  (handler-case (funcall gen)
    (error (c)
      (dev-debug :force-out
	 "~&Lisp error while parsing include file ~S:~%~%~A~%~%"
	 (car *include-stack*)
	 c)
      *end-token*)))


;;; plan_viewer

;;; /\/: <program_name> s.b. defined in the TF Manual.

(defparameter *plan-viewer-features*
  '(plan_output levels_output resource_output
    mode_selection link_selection entity_detail tf_input))

(defun <plan_viewer-form> ()
  (cons 'plan_viewer
	(<program-interface-spec> *plan-viewer-features*)))

(defun <program-interface-spec> (feature-list)
  (let (program info features)
    (block parse
      (must-be 'program)
      (must-be '=)
      (setq program (<program-name>))
      (unless (token-is '|,|) (must-be '|;|) (return-from parse))
      (when (token-is 'information)
	(must-be '=)
	(setq info (<text-string>))
	(unless (token-is '|,|) (must-be '|;|) (return-from parse)))
      (setq features
	    (one-or-more #'(lambda ()
			     (<feature-assignment> feature-list))
			 :separator '|,|
			 :until '|;|)))
    (list* (cons 'program program)
	   (cons 'information info)
	   features)))

(defun <program-name> ()
  (must-satisfy #'(lambda (obj) (or (string obj) (symbolp obj)))
		"a program name"))

(defun <feature-assignment> (features)
  (let (feature avail)
    (setq feature (must-be-member features "a known feature" "features"))
    (must-be '=)
    (setq avail (<availability>))
    (cons feature avail)))

(defun <availability> ()
  (token-case
    ((yes) 'yes)
    ((no)  'no)
    (t (syntax-error "Invalid availability: \"~A\"." (token)))))


;;; world_viewer

(defparameter *world-viewer-features* '(snapshot incremental tf_input))

(defun <world_viewer-form> ()
  (cons 'world_viewer
	(<program-interface-spec> *world-viewer-features*)))


;;; prefer_plans_with

(defun <prefer_plans_with-form> ()
  (cons 'prefer_plans_with
	(one-or-more #'(lambda ()
			 (list (<number>) ;/\/: non-ints?  infinity?
			       (<preference-word>)))
		     :separator '|,|
		     :until '|;|)))

(defparameter *plan-features*
  '(earliest_finish_of_plan latest_finish_of_plan number_of_nodes))

(defun <preference-word> ()
  (cond ((eq (token) '|{|)
	 (<resource-spec>))
	(t
	 (must-be-member *plan-features*
	     "a valid plan feature" "plan features"))))

;;; prefer_schemas

(defun <prefer_schemas-form> ()
  (cons 'prefer_schemas
	(one-or-more #'<schema-preference>
		     :separator '|,|
		     :until '|;|)))

(defun <schema-preference> ()
  (let (p+v use-list)
    (setq p+v (<pattern-assignment>))
    (must-be 'use)
    (setq use-list (<name-list>))
    (list p+v use-list)))


;;; resource_units

(defun <resource_units-form> ()
  (cons 'resource_units
	(one-or-more #'<resource-unit-definition>
		     :separator '|,|
		     :until '|;|)))

(defun <resource-unit-definition> ()
  (let (names class)
    (setq names (<synonym-sequence>))
    (setq class (if (token-is '=) (<resource-unit-class>) 'count))
    (list names class)))

(defun <synonym-sequence> ()
  (let ((name (<name>)))
    (if (token-is '/)
	(cons name (<synonym-sequence>))
	(list name))))

(defun <resource-unit-class> ()
  (let ((class (must-be-member '(count size weight set)
		   "a valid resource unit class" "resource unit classes")))
    (cond ((eq class 'set)
	   ;; /\/: This could be a <name-list> except for the "," separator.
	   (must-be '|(|)
	   (list 'set
		 (list-of #'<name> :separator '|,| :until '|)|)))
	  (t class))))

;;; resource_types

(defparameter *resource-classes*
  '(consumable_strictly))		;other classes not yet supported

(defun <resource_types-form> ()
  (cons 'resource_types
	(one-or-more #'<resource-type-definition>
		     :separator '|,|
		     :until '|;|)))

(defun <resource-type-definition> ()
  (let (class resource unit)
    (setq class (<resource-class>))
    (setq resource (<resource-spec>))
    (when (token-is '=)
      (setq unit (<resource-unit>)))
    (resource-type-def class resource unit)))

(defun <resource-unit> ()
  (<name>))

(defun <resource-class> ()
  (must-be-member *resource-classes*
		  "a valid resource class" "resource classes"))


;;; resource_conversions

(defun <resource_conversions-form> ()
  (syntax-error "\"resource_conversions\" has not yet been implemented.")
  (skip-to '|;|)
  (must-be '|;|))


;;; impossible

(defun <impossible-form> ()
  (syntax-error "The \"impossible\" form is no longer supported.~%~
                 Consider \"domain_rules\" instead.")
  (skip-to '|;|)
  (must-be '|;|))


;;; domain_rules

(defun <domain_rules-form> ()
  (cons 'domain_rules
	(one-or-more #'<domain-rule>
		     :separator '|,|
		     :until '|;|)))

(defun <domain-rule> ()
  ;; Be sure to leave the "," or ";" that ends the rule.
  (if (token-is 'forall)
      (<forall-domain-rule>)
      (<simple-domain-rule>)))

;;; When "forall" is present, the rule has the form:
;;;
;;;    forall <variable_name> [ = <variable_restriction> ] ,
;;;           ...
;;;      <pattern> [ = <value> ] [ & <pattern> [ = <value> ] ]
;;;                              ...
;;;                => <pattern> [ = <value> ]
;;;
;;; This is potentially hard to parse.  First we have some things that
;;; start "<variable_name> =", then some that start "<pattern> =".  The
;;; problem is to tell when we reach the end of the variable part and
;;; start the pattern part.  We can do this either by distinguishing
;;; <variable_name> from <pattern> (which turns out to be possible,
;;; because a pattern must be "{<name> ...}"), or by waiting until we
;;; find a "&" or "=>".  We do the former.

(defun <forall-domain-rule> ()
  (let ((vars '()))
    (loop (push (<restriction-assignment>) vars)
	  (must-be '|,|)
	  (unless (var-pattern-p (token))
	    (return)))
    (setq vars (nreverse vars))
    (list 'forall vars (<simple-domain-rule>))))

(defun <simple-domain-rule> ()
  (let (antecedent consequent)
    (setq antecedent
	  (cons 'and (one-or-more #'<pattern-assignment>
				  :separator '&
				  :until '=>)))
    (setq consequent (<pattern-assignment>))
    (list '=> antecedent consequent)))


;;; compute_condition

(defun <compute_condition-form> ()
  (cons 'compute_condition
	(one-or-more #'<compute-function-declaration>
		     :separator '|,|
		     :until '|;|)))

;;; <compute-function-declaration> ::=
;;;    [ multiple_answer ] <compute-function-spec>
;;;        [ depends_on <depends_on-clause> ]

;;; For <compute-function-spec> and <depends_on-clause>, see
;;; the "conditions" schema clause.

(defun <compute-function-declaration> ()
  (let (multiple-answer-p result-pattern call-pattern depends)
    (when (token-is 'multiple_answer)
      (setq multiple-answer-p t))
    (multiple-value-setq (call-pattern result-pattern)
      (<compute-function-spec>))
    (when (token-is 'depends_on)
      (setq depends (<depends_on-clause>)))
    (compute-function call-pattern result-pattern multiple-answer-p depends)))


;;; language

;;;   language <language_name>;
;;;     <language_text>...
;;;   end_language;

;;; The only language handled at present is Lisp, but it's possible for
;;; a TF compiler to handle more than one in the same implementation.
;;; In PopLog, for instance, the compiler might handle Lisp, Pop, Prolog,
;;; etc.

;;; The Lisp macro eval-in-tf-compiler can be used to evaluate code
;;; while compiling rather than have it wait until the DM accepted the
;;; domain.  We want the evaluation to happen soon enough to affect
;;; subsequent parsing (e.g. if it defines a constraint parser), which
;;; is why we evaluate it here rather than later on.  (It might be
;;; thought that we should not eval the code until after we know the
;;; TF file contains no errors, or someting like that, but that's too
;;; later for some of the things we want the code to be able to do.)

;;; The <language_text> is read first as strings, one for each line, and
;;; then converted to other structures if possible.  An alternative would
;;; be to try to read the code directly.  For example, Lisp code could be
;;; read by repeatedly calling READ until it returned END_LANGUAGE.  The
;;; direct approach has some advantages.  We wouldn't have to worry about
;;; maximum string size, for instance.  (See below.)  On the other hand,
;;; we'd have less control over what was read.  A missing close paren
;;; might cause READ to consume the everything to the end of file, or
;;; at least cause us to miss the "end_language".

;;; /\/: Should we allow some whitespace before "end_language"?  Note that
;;; we might have to allow whitespace if, for example, we read Lisp by
;;; just calling READ rather than reading the code as strings first.

(defparameter *language-names* '(lisp))

(defparameter *included-lisp-readtable*
  (make-actor-readtable *standard-readtable* :self-contained t))

(defun <language-form> ()
  (let ((language-name (<language-name>))
	(lines '())
	(line nil))

    ;; Check the language name by hand rather than calling must-be-member
    ;; to avoid invoking error recovery so close to text that is not in
    ;; TF syntax.
    (unless (member language-name *language-names*)
      (token-set-error language-name *language-names*
		       "a language that can be included in TF"
		       "languages"))

    ;; The current token should now be a semicolon.  When checking
    ;; this, we have to be careful not to do anything that would call
    ;; next-token, because the text after the ";" is not in TF syntax.
    (unless (eql (token) '|;|)
      (syntax-error "Found \"~A\" when expecting \";\"." (token)))

    ;; Read lines until we find one that begins with "end_language"
    ;; Note that the first line will be whatever follows the ";".
    ;; It's an error if this is not "".
    (loop (setq line (read-tf-line))
	  (when (or (eql line *end-token*)
		    (eql 0 (search "end_language" line)))
	    (return))
	  (push line lines))
    (setq lines (nreverse lines))
    (next-token)

    (unless (string= (car lines) "")
      (syntax-error "Text after \"language <name>;\" ignored: ~S."
		    (car lines)))

    (let ((forms (convert-language-text language-name (cdr lines))))

      ;; Handle eval-in-tf-compiler
      (dolist (form forms)
	(when (list-beginning 'eval-in-tf-compiler form)
	  (mapc #'eval (cdr form))))

      (list 'language language-name
	    forms))))

(defmacro eval-in-tf-compiler (&rest forms)
  (declare (ignore forms))
  ;; This is what happens outside the TF compiler
  'nil)

(defun <language-name> ()
  (prog1 (token) (next-token)))

(defun read-tf-line ()
  (cond ((streamp *tf-stream*)
	 (prog1 (read-line *tf-stream* nil *end-token*)
	   (newline-reader *tf-stream* '#\newline)))
	(t "")))

(defun convert-language-text (language-name lines)
  (case language-name
    ((lisp)
     (let ((*readtable* *included-lisp-readtable*))
       (read-lisp-from-strings
	 (mapcon #'(lambda (lines)
		     (list (car lines) (string #\newline)))
		 lines))))
    (t lines)))

(defun read-lisp-from-strings (strings)
  (let ((stream (make-string-input-stream (big-string-concat strings)))
	(forms '()))
    (loop (let ((form (read stream nil *eof*)))
	    (when (eq form *eof*)
	      (return))
	    (push form forms)))
    (nreverse forms)))

#+:undef
(defun read-lisp-from-strings (strings)
  ;; If we concatenate strings, the result might exceed the array-total-
  ;; size-limit.  Moreover, the number of strings we can concatenate at
  ;; once is limited by the call-arguments-limit, and since concatenate
  ;; copies all its arguments it would make many copies if we used it
  ;; with reduce.  The number of streams in a concatenated stream is also
  ;; limited by the call-arguments-limit.  Hence the following compromise
  ;; which is still less than ideal.
  (let ((stream (reduce #'make-concatenated-stream
			(mapcar #'make-string-input-stream strings)
			:from-end t))
	(forms '()))
    (loop (let ((form (read stream nil *eof*)))
	    (when (eq form *eof*)
	      (return))
	    (push form forms)))
    (nreverse forms)))


;;; types

;;; types <type_name> = <name_set> | <integer_range> ,
;;;       ... ;
;;;
;;;     <type_name> ::= <name>
;;;      <name_set> ::= ( <name> ... )
;;; <integer_range> ::= ( <integer> .. <integer> )
;;;       <integer> ::= [ <sign> ] <digits>

;;; Revisions June 1999 [JD]:
;;;
;;; types <type_name> = <type_expr> ,
;;;       ... ;
;;;
;;; <type_expr> ::= <type_enumeration>
;;;              |  ?{type name}
;;;              |  ?{and <type_conjunct> ...}
;;;              |  ?{or <type_expr> ...}
;;;
;;; <type_conjunct> ::= <type_expr> | ?{not <type_expr>}
;;;
;;; <type_enumeration> ::= <name_set> | <integer_range> 
;;;

(defun <types-form> ()
  (let ((defs (one-or-more #'<type-definition> :separator '|,| :until '|;|)))
    `(types ,@defs)))

(defun <type-definition> ()
  (let (name expr)
    (setq name (<name>))
    (must-be '=)
    (setq expr (<type-expr>))
    (type-def name expr)))

(defun <type-expr> ()
  (if (token-is '|(|)
      ;; Type enumeration
      (if (numberp (token))
	  ;; Must be an <integer-range>
	  (prog1 (<integer-range-values>) (must-be '|)|))
	;; Like <name-set>
	(list-of #'<name> :until '|)| ))
    ;; A compound expression
    ;; The expression must be an actor, and an actor is read as a
    ;; single token, so we read then check.
    (let ((expr (must-satisfy #'actorp "a valid type expression")))
      (if (valid-type-expr-p expr)
	  expr
	(syntax-error "Invalid type expression.")))))

(defun valid-type-expr-p (expr &key (allow-negation nil))
  ;; N.B. allow-negation is not passed along on recursive calls.
  ;; It applies only at the top level unless explicitly re-established.
  (cond ((not (actorp expr))
	 (and (listp expr)
	      (or (every #'integerp expr)
		  (every #'symbolp expr))))
	((actor-type-p 'type expr)
	 (and (length=1 (actargs expr))
	      (symbolp (first (actargs expr)))))
	((actor-type-p 'not expr)
	 (and allow-negation
	      (length=1 (actargs expr))
	      (valid-type-expr-p (first (actargs expr)))))
	((actor-type-p 'and expr)
	 (every #'valid-type-conjunct-p (actargs expr)))
	((actor-type-p 'or expr)
	 (every #'valid-type-expr-p (actargs expr)))
	(t
	 nil)))

(defun valid-type-conjunct-p (expr)
  (valid-type-expr-p expr :allow-negation t))

(defun actor-type-p (act-type actor)
  (eq (actfn actor) (actor-rename act-type)))

#|
(defun <type-definition> ()
  (let (name members)
    (setq name (<name>))
    (must-be '=)
    (setq members (<type-set>))
    (type-def name members)))

(defun <type-set> ()
  (must-be '|(| )
  (if (numberp (token))
      ;; Must be an <integer-range>
      (prog1
	  (<integer-range-values>)
	(must-be '|)|))
    ;; Like <name-set>
    (list-of #'<name> :until '|)| )))
|#

(defun <integer-range-values> ()
  (let (start end)
    (setq start (<integer>))
    (must-be '|..|)
    (setq end (<integer>))
    (if (> start end)
	(syntax-error "Start greater than end in integer range ~S .. ~S."
		      start end)
      (loop for i from start to end
	    collect i))))


;;; always

(defun <always-form> ()
  (let ((assigns (one-or-more #'<pattern-assignment>
			      :separator '|,|
			      :until '|;|)))
    ;; /\/: Don't allow variables in pattern or value.
    `(always
      ,@(mapcar #'(lambda (p+v)
		    (always-fact (pv-pattern p+v)
				 (pv-value p+v)))
	        assigns))))

;;; constraint_syntax

;;; constraint_syntax <constraint-syntax-spec>, ... ;
;;;
;;; <constraint-syntax-spec> ::= <cluase-name> : <constraint-type>
;;; <clause-name> ::= <name>
;;; <constraint-type> ::= <name>

;;; The constraint_syntax form defines a temporary syntax (for the
;;; current domain only), and it uses the default "general" syntax
;;; for the constraints.  However, if there is already a nontemporary
;;; parser for a given clause-name, it remains nontemporary.  See
;;; register-constraint-parser.

(defun <constraint_syntax-form> ()
  (let ((specs (one-or-more #'<constraint-syntax-spec>
			      :separator '|,|
			      :until '|;|)))
    ;; Define the new sytax so it's available when parsing the
    ;; rest of the file.
    (dolist (spec specs)
      (destructuring-bind (clause-name constraint-type) spec
        (register-constraint-parser clause-name constraint-type)))
    ;; Return the specifications
    `(constraint_syntax
      . ,specs)))

(defun <constraint-syntax-spec> ()
  (let (clause-name constraint-type)
    (setq clause-name (<name>))
    (must-be '|:|)
    (setq constraint-type (<name>))
    (list clause-name constraint-type)))


;;; Schemas

;;; "endschema" is a frequent error for "end_schema", so we recognize
;;; it as the end of a schema, albeit an incorrect one.

(defparameter *schema-end-symbols*
  '(endschema end_schema end_task end_meta_schema
    end_process_schema end_meta_process_schema))

(defparameter *schema-clause-symbols*
  '(instance_of                  vars_relations
    info		         nodes
    vars		         orderings
    expands		         conditions
    only_use_for_effects         effects
    only_use_for_resources       resources
    local_vars		         time_windows))

(defvar *schema-blocking-tokens*
  (append *schema-end-symbols* *schema-clause-symbols*))

(defvar *schema-clauses*
  ;; Stores parse results (trees?) for clauses as they are are parsed.
  ;; Must not be bound at top level.
  )

(defmacro parsing-schema-p ()		;ie, not testing single clauses
  `(boundp '*schema-clauses*))

;;; For *schema-plist* and getf-schema, see com-defs.

(defun <schema-form> (begin-symbol end-symbol)
  (let ((*blocking-tokens* *schema-blocking-tokens*) ;was just end-symbols /\/
	(*schema-clauses* nil)
	(*schema-plist* nil)
	(name nil))
    (setf (getf-schema :type) begin-symbol)
    (setq name (<name>))
    (tf-sub-progress "~A" name)
    (must-be '|;|)
    ;; Below is like (zero-or-more #'<schema-clause> :until end-symbol)
    ;; but with better error handling and recovery.
    (loop (when (member (token) *schema-end-symbols*)
	    (unless (eql (token) end-symbol)
	      (syntax-error "~A \"~A\" ends with \"~A\" instead of \"~A\"."
			    begin-symbol name (token) end-symbol))
	    (next-token)
	    (must-be '|;|)
	    (return))
	  (when (eql (token) *end-token*)
	    (syntax-error "Missing \"~A\" in schema \"~A\"." end-symbol name)
	    (return))
	  (push (<schema-clause>)
		*schema-clauses*))
    (setq *schema-clauses* (nreverse (delete nil *schema-clauses*)))
    ;; Check forward refs if not done by nodes clause.
    (unless (assoc 'nodes *schema-clauses*)
      (check-forward-ref-nodes (lookup 'nodes *schema-clauses*)))
    ;; Result.
    `(schema ,begin-symbol		;type of schema
             ,name
             ,*schema-clauses*
             ,*schema-plist*)))


(defun <schema-clause> ()
  ; (tf-progress "~&~3T~A " (token))
  (tf-sub-progress ".")
  (token-case
    ((instance_of)            (<instance_of-clause>))
    ((instantiates)           (<instantiates-clause>))
    ((info)                   (<info-clause>))
    ((vars)                   (<vars-clause>))
    ((expands)                (<expands-clause>))
    ((only_use_for_effects)   (<only_use_for_effects-clause>))
    ((only_use_for_resources) (<only_use_for_resources-clause>))
    ((local_vars)             (<local_vars-clause>))
    ((vars_relations)         (<vars_relations-clause>))
    ((nodes)                  (<nodes-clause>))
    ((orderings)              (<orderings-clause>))
    ((conditions)             (<conditions-clause>))
    ((effects)                (<effects-clause>))
    ((resources)              (<resources-clause>))
    ((time_windows)           (<time_windows-clause>))
    (t (if (constraint-clause-name-p (token))
	   (<constraint-clause>)
	 (invalid-schema-clause)))))

(defun invalid-schema-clause ()
  (syntax-error "Invalid schema clause starting with \"~A\"." (token))
  (recover-when
    #'(lambda (sym) (or (eql sym '|;|)
			(member sym *schema-clause-symbols*)
			(member sym *schema-end-symbols*)
			(constraint-clause-name-p sym))))
  (when (eql (token) '|;|)
	(next-token))
  nil)


;;; New clause definition: define-constraint-parser

;;; The function should parse one constraint and leave in place any
;;; comma or semicolon that follows the constraint.

;;; If the function supplied to register-constraint-parser is nil,
;;; the registration is temporary (for the current domain only),
;;; unless a permanent registration is already in force.  This
;;; allows the constraint_syntax form to have only a domain-specific
;;; effect.  However, calling register-constraint-parser with a
;;; non-null function (e.g. by using define-constraint-parser)
;;; within a "language lisp" form will have a permanent effect,
;;; which is probably not what we want.  /\/

(defvar *temporary-constraint-clause-names* '())

(defmacro %constraint-parser (clause-name)
  `(get ,clause-name 'constraint-parser))

(defmacro define-constraint-parser
      ((clause-name constraint-type) lambda-list &body body)
  (let ((fn-name (concat-name "<" clause-name '#:-constraint-clause ">")))
    `(progn
       (defun ,fn-name ,lambda-list . ,body)
       (register-constraint-parser ',clause-name ',constraint-type ',fn-name)
       ',fn-name)))

(defun register-constraint-parser (clause-name constraint-type &optional fn)
  ;; See if this is a temporary registration.
  (let* ((existing-definition (%constraint-parser clause-name))
	 (existing-function
	  (and existing-definition 
	       (second existing-definition))))
    (when (and (null fn)
	       (null existing-function))
      (nconcf-new *temporary-constraint-clause-names* clause-name)))
  ;; Adjust the schema blocking-tokens.
  (nconcf-new *schema-blocking-tokens* clause-name)
  ;; And record the parser.
  (setf (%constraint-parser clause-name)
	(list constraint-type fn)))

(define-initializer :tf init-constraint-parsers ()
  ;; Unregister the temporary clause names and remove them from
  ;; the list of schema blocking tokens.
  (dolist (clause-name *temporary-constraint-clause-names*)
    (removef clause-name *schema-blocking-tokens*)
    (setf (%constraint-parser clause-name)
	  nil))
  (setq *temporary-constraint-clause-names*
	nil))


;;; New clause parsing

(defun constraint-clause-name-p (token)
  (and (symbolp token)
       (%constraint-parser token)))

(defun <constraint-clause> ()
  (let ((clause-name (token)))
    (next-token)
    (destructuring-bind (constraint-type
			 NT)
			(%constraint-parser clause-name)
      (list 'constraints
	    (constraint-block
	       constraint-type
	       (one-or-more (or NT 
				#'(lambda ()
				    (<general-constraint> constraint-type)))
			    :separator '|,|
			    :until '|;|))))))

;;; <general-constraint> is the syntax used for constraints in clauses
;;; that have been defined only by the constraint_syntax TF form.
;;; 
;;; <general-constraint> ::= <constraint-body> [<constraint-range>]
;;;
;;; <constraint-body> ::= <pattern-assignment>
;;;
;;; <constraint-range> ::= from <node-end> to <node-end>
;;;                     |  at <node-end>
;;;                     |  across <node>
;;;
;;; "at <node-end>" specifies only the "to" end.  The range defaults
;;; to "at self".
;;;
;;; "across <node>" is equivalent to "from begin_of <node> to end_of <node>"

(defun <general-constraint> (constraint-type)
  (let* ((p+v   (<pattern-assignment>))
	 (range (<constraint-range>)))
    (make-constraint
      :type    constraint-type
      :pattern (pv-pattern p+v)
      :value   (pv-value p+v)
      :from    (first range)
      :to      (second range))))

(defun <constraint-range> () ; -> (from-end to-end)
  (token-case
    ((at)
     (list nil (<node-end> 'condition_node_end)))
    ((across)
     (let ((n (<node>)))
       (list (node-end n :begin) (node-end n :end))))
    ((from)
     (let (from to)
       (setq from (<node-end> 'condition_contributor_node_end))
       (must-be 'to)
       (setq to (<node-end> 'condition_node_end))
       (list from to)))
    (t
     ;; "at self"
     (list nil (default-node-end 'condition_node_end)))))
     


;;; instance_of clause: instance_of <pattern>

(defun <instance_of-clause> ()
  (when (in-meta-schema-p)
    (syntax-error "Found an \"instance_of\" clause in a meta-schema."))
  (let ((meta-schema-pattern (<pattern>)))
    (must-be '|;|)
    (list 'instance_of meta-schema-pattern)))

;;; instantiates clause: instantiates <pattern>

(defun <instantiates-clause> ()
  (unless (in-meta-schema-p)
    (syntax-error "Found an \"instantiates\" clause in a non-meta-schema."))
  (let ((pattern (<pattern>)))
    (must-be '|;|)
    (list 'instantiates pattern)))

(defun in-meta-schema-p ()
  (member (getf-schema :type) '(meta_schema meta_process_schema)))

;;; info clause

(defun <info-clause> ()
  (cons 'info (one-or-more #'<info-line> :separator '|,| :until '|;|)))


;;; vars clause

(defun <vars-clause> ()
  (cons 'vars
	(one-or-more #'<restriction-assignment>
		     :separator '|,|
		     :until '|;|)))

;;; expands clause

(defun <expands-clause> ()
  (cons 'expands
	(prog1 (<action-pattern>)
	  (must-be '|;|))))

(defun <action-pattern> ()
  (<pattern>))				;/\/: ?

;;; only_use_for_effects clause

(defun <only_use_for_effects-clause> ()
  (cons 'only_use_for_effects
	(one-or-more #'<effect-spec>
		     :separator '|,|
		     :until '|;|)))

(defun <effect-spec> ()
  (let ((p+v (<pattern-assignment>)))
    (effect (pv-pattern p+v)
	    (pv-value p+v)
	    (if (token-is 'at)
		(<effect-point>)
		(default-node-end 'effect_node_end)))))

(defun <effect-point> ()
  (<condition-or-effect-point> 'effect_node_end))

;;; only_use_for_resources clause

(defun <only_use_for_resources-clause> ()
  (cons 'only_use_for_resources
	(one-or-more #'<resource-usage-spec>
		     :separator '|,|
		     :until '|;|)))


;;; local_vars clause

(defun <local_vars-clause> ()
  (cons 'local_vars
	(one-or-more #'<restriction-assignment>
		     :separator '|,|
		     :until '|;|)))

;;; vars_relations clause

(defparameter *var-relations* '(= /=))

(defun <vars_relations-clause> ()
  (cons 'vars_relations
	(one-or-more #'<var-relation>
		     :separator '|,|
		     :until '|;|)))

(defun <var-relation> ()
  (let (left relation right)
    (setq left (<variable>))		;N.B. entire actor, not just the name
    (setq relation
	  (must-be-member *var-relations*
	      "a supported variable relation" "relations"))
    (setq right (<variable>))
    (var-relation left relation right)))


;;; nodes clause
;;;
;;; <nodes-clause> ::= <node-specs> ";"
;;;
;;; <node-specs> ::= <node-spec> ["," <node-specs>]
;;;
;;; <node-spec> ::= <node-number> <node-form>
;;;              |  <ordering-block>
;;;
;;; <node-form> ::= dummy | start | finish
;;;              |  action <pattern>
;;;              |  event <pattern>
;;;              |  iterate <iterated-node-form>
;;;              |  foreach <iterated-node-form>
;;;
;;; <iterated-node-form> ::= <iterated-node-type> <pattern> for <iterators>
;;;
;;; <iterated-node-type> ::= action | event
;;;
;;; <iterators> ::= <variable-name> over <iteration-set> [and <iterators>]
;;;
;;; <iteration-set> ::= <pattern-component>
;;;
;;; <ordering-block> ::= sequential <node-specs> end_sequential
;;;                   |  parallel <node-specs> end_parallel
;;;
;;; A "sequential" block specifies that the nodes and blocks it
;;; contains must be linked to enforce the order in which they are
;;; listed.  "Parallel" adds no restrictions of its own but allows
;;; nodes to be grouped within a "sequential".
;;;
;;; The code does not follow the above syntax exactly.  For instance,
;;; some nonterminals -- such as <node-specs> and <node-form> -- can be
;;; handled more effectively in-line.  <node-specs> is always followed
;;; by a terminal symbol and hence can be handled by one-or-more.
;;;
;;; In an order block, we allow a final comma before the "end_" symbol.
;;; This means that all <node-spec>s in the block can be followed by a
;;; comma, though the final comma's not really legal syntax.  We also
;;; supply a comma after the "end_" symbol if it's been omitted.
;;; With these two relaxations of the syntax, all commas can be just
;;; as they would have been if order blocks weren't being used.
;;;

;;; /\/: Should <general-set> be called <pattern-set>?

(defparameter *node-types* '(action event dummy start finish))

(defparameter *non-task-node-types* '(action event dummy))

(defparameter *order-block-end-symbols* '(end_sequential end_parallel))

(defun <nodes-clause> ()
  (let ((specs (one-or-more #'<node-spec> :separator '|,| :until '|;|)))
    (multiple-value-bind (nodes order-blocks)
	(get-nodes-and-order-blocks specs)
      (check-forward-ref-nodes nodes)
      (when (and order-blocks (parsing-schema-p))
	(setf (getf-schema 'order-blocks) order-blocks)
	(setf (getf-schema 'link_from_node_end)
	      (get-default-value 'link_from_node_end))
	(setf (getf-schema 'link_to_node_end)
	      (get-default-value 'link_to_node_end)))
      (cons 'nodes nodes))))

(defun check-forward-ref-nodes (defined-nodes)
  (when (parsing-schema-p)
    (let ((undefined
	   (set-difference (getf-schema 'forward-ref-nodes)
			   (mapcar #'node-number defined-nodes))))
      (when undefined
	(syntax-error "Nodes used before this point but not defined: ~S."
		      (sort (copy-list undefined) #'<))))))

(defun <node-spec> ()
  (token-case
    ((sequential) (<order-block> 'sequential 'end_sequential))
    ((parallel)   (<order-block> 'parallel 'end_parallel))
    (t
     (let ((number (or (<defining-node-number>) 0)))
       (token-case
	 ((iterate) (<iterated-node-form> number :sequence-p t))
	 ((foreach) (<iterated-node-form> number))
	 (t
	  ;; So it's an ordinary, everyday node form.
	  (let ((node-type
		 (must-be-member *node-types*
				 "a valid node type"
				 "node types"))
		(pattern nil))
	    (ecase node-type
	      ((action event)       (setq pattern (<pattern>)))
	      ((dummy start finish) )
	      ((nil)                (assert *recovering*)))
	    (unless (eq (getf-schema :type) 'task)
	      (unless (member node-type *non-task-node-types*)
                (syntax-error "Nodes of type ~A are allowed only in tasks."
			      node-type)))
	    (when (eq (token) 'for)
	      (syntax-error "\"for\" is not allowed without \"iterate\"."))
	    (make-node :number number
		       :type node-type
		       :pattern pattern))))))))

;;; Iterate and foreach

#-:new-syntax-prevails
(defun <iterated-node-form> (number &key (sequence-p nil))
  (let* ((node-type (must-be-member '(action event)
				    "a node type that can be iterated"
				    "node types"))
	 (pattern    (<pattern>))
	 (iterators  (progn
		       (must-be 'for)
		       (<iterators>))))
    (make-node
      :number number
      :type node-type
      :pattern (protect-iterate-vars-in-pattern pattern iterators)
      :iterators iterators
      :sequence-p sequence-p)))

;;; The following might be a better syntax:
;;;
;;; <node-form> ::= dummy | start | finish
;;;              |  action <pattern>
;;;              |  event <pattern>
;;;              |  iterate <iterated-node-form>
;;;              |  foreach <iterated-node-form>
;;;
;;; <iterated-node-form> ::=
;;;    <iterated-node-type> [sequence] <pattern> for <iterators>
;;;
;;; I.e., no foreach.  Instead it's "iterate <node-type> ..." or
;;; "iterate <node-type> sequence ...".

#+:new-syntax-prevails
(defun <iterated-node-form> (number)
  (let* ((node-type (must-be-member '(action event)
				    "a node type that can be iterated"
				    "node types"))
	 (sequence-p (token-is 'sequence))
	 (pattern    (<pattern>))
	 (iterators  (progn
		       (must-be 'for)
		       (<iterators>))))
    (make-node
      :number number
      :type node-type
      :pattern (protect-iterate-vars-in-pattern pattern iterators)
      :iterators iterators
      :sequence-p sequence-p)))

(defun <iterators> ()
  (cons (<iterator>)
	(if (token-is 'and)
	    (<iterators>)
	  nil)))

(defun <iterator> ()
  (list (<variable-name>)
	(progn
	  (must-be 'over)
	  (<value>)
	  #+:undef			;/\/ supposed to be <general-set>
	  (<general-set>))))

(defun protect-iterate-vars-in-pattern (pattern iterators)
  ;; /\/: If we leave the vars as 'given actors, something might
  ;; become confused, e.g. when instantiating the schema.
  (let ((iterate-vars (mapcar #'car iterators)))
    (label protect ((p pattern))
      (if (atom p)
	  p
	(if (given-p p)
	    (let ((var (car (actargs p))))
	      (if (member var iterate-vars :test #'eq)
		  `(:iterate-var ,var)
		p))
	  (recons p (protect (car p))
		    (protect (cdr p))))))))

;;; Order blocks
  
(defun <order-block> (order-type end-symbol)
  (flet ((<node-spec-in-block> ()
	   ;; If the last spec in the block is followed by a comma,
	   ;; we'll see the end-symbol rather than a spec.  In that
	   ;; case, we'll return nil then take the nil out later.
	   ;; Note that at this point we don't care which end-symbol
	   ;; it is.  If it's wrong, one-or-more will complain.
	   (if (member (token) *order-block-end-symbols*)
	       nil
	     (<node-spec>))))
    (let ((specs
	   (one-or-more #'<node-spec-in-block>
			:separator '|,|
			:until end-symbol
			:blocking-tokens
		          (append *order-block-end-symbols*
				  *blocking-tokens*))))
      ;; Supply a comma after the end-symbol if it was omitted.
      (unless (member (token) '(|,| |;|))
	(push-token (token))
	(push-token '|,|)		; equivalent to ...
	(next-token))			; ... (setq *token* '|,|)
      ;; And return
      (cons order-type
	    (remove nil specs)))))

(defun get-nodes-and-order-blocks (specs)
  (let ((nodes '())
	(ords '()))
    (label scan ((specs specs))
      (dolist (s specs)
	(cond ((and (consp s)
		    (or (eq (car s) 'sequential) (eq (car s) 'parallel)))
	       (push s ords)
	       (scan (cdr s)))
	      (t
	       (push s nodes)))))
    (values
      (nreverse nodes)
      (nreverse ords))))


;;; orderings clause

(defun <orderings-clause> ()
  (cons 'orderings
	(one-or-more #'<node-ordering>
		     :separator '|,|
		     :until '|;|)))

(defun <node-ordering> ()
  (let (before after delay)
    (setq before (<node-end> 'link_from_node_end))
    (when (token-is '---)
      (setq delay (<delay-spec>)))
    (must-be '--->)
    (setq after (<node-end> 'link_to_node_end))
    (construct-ordering before after delay)))

(defun <delay-spec> ()
  (<time-bounds-spec>))			;/\/: TF Manual says <min_max_spec>


;;; conditions clause

(defun <conditions-clause> ()
  (cons 'conditions
	(one-or-more #'<condition-statement>
		     :separator '|,|
		     :until '|;|)))

(defun <condition-statement> ()
  (let (condition-type)
    (setq condition-type (token))
    (token-case
      ((supervised)
       (<supervised-condition>))
      ((unsupervised only_use_if only_use_for_query)
       (<limited-condition> condition-type))
      ((achieve)
       (<achieve-condition>))
      ((compute)
       (<compute-condition>))
      (t
       (syntax-error "Unknown condition type: \"~A\"." condition-type)))))

(defun <supervised-condition> ()
  (let (p+v point contributor)
    (setq p+v (<pattern-assignment>))
    (must-be 'at)
    (setq point (<condition-point>))
    (must-be 'from)
    (setq contributor (<contributor-entry>))
    (list 'supervised (pv-pattern p+v) (pv-value p+v) point contributor)))

(defun <contributor-entry> ()		;/\/: (...) or [...]?
  (cond ((token-is '|[| )
	 (one-or-more #'(lambda ()
			  (<node-end> 'condition_contributor_node_end))
		      :until '|]|))
	(t
	 (list (<node-end> 'condition_contributor_node_end)))))

(defun <limited-condition> (condition-type)
  (let (p+v point)
    (setq p+v (<pattern-assignment>))
    (setq point (<optional-condition-point>))
    (list condition-type (pv-pattern p+v) (pv-value p+v) point)))

(defun <achieve-condition> ()
  ;; Some variations from <limited-condition>:
  ;;  - use achievable rather than achieve as the condition type.
  ;;  - the optional "achieve after" point.
  (let (p+v point after-point)
    (setq p+v (<pattern-assignment>))
    (setq point (<optional-condition-point>))
    (setq after-point (<optional-achieve-after-point>))
    (list 'achievable (pv-pattern p+v) (pv-value p+v) point
	  after-point)))

(defun <optional-achieve-after-point> ()
  (if (token-is 'after)
      (<achieve-after-point>)
    (get-default-value 'achieve_after_point)))

(defun <achieve-after-point> ()
  (let* ((end  (token-case
	         ((begin_of) :begin)
		 ((end_of)   :end)
		 (t
		  (get-default-value 'condition_contributor_node_end))))
	 (node (token-case
		 ((start) :start)
		 ((self)  :self)
		 (t       (<node-number>)))))
    (node-end node end)))

(defun <compute-condition> ()
  ;; The "at" point has no significance, but we supply one anyway for
  ;; consistency with other conditions.
  (let (result-pattern call-pattern point depends)
    (multiple-value-setq (call-pattern result-pattern)
      (<compute-function-spec>))
    (setq point (<optional-condition-point>))
    (when (token-is 'depends_on)
      (setq depends (<depends_on-clause>)))
    (setq point (default-node-end 'condition_node_end))
    (list 'compute call-pattern result-pattern point depends)))

(defun <compute-function-spec> () ; -> call-pattern, result-pattern
  ;; parses: {<function-name> [ <value> ... ]} [= <value>]
  ;; /\/: That the value is optional is probably not in the TF Manual.
  ;; /\/: The whole thing could be done by calling <pattern-assignment>
  ;;      except that the 1st word needn't be a valid <name>.
  (let (result-pattern call-pattern)
    (setq call-pattern (<compute-call>))
    (if (token-is '=)
	(setq result-pattern (<value>))
	(setq result-pattern (get-default-value 'value)))
    (values call-pattern result-pattern)))

(defun <compute-call> ()
  (let ((pat (<compute-pattern>)))
    (if (and (consp pat) (symbolp (car pat)))
	pat
      (syntax-error "\"~A\" is not legal as a compute pattern." pat))))

(defun <compute-pattern> ()
  ;; Like <pattern-component> by allows names such as "+" and "-",
  ;; not just valid <name>s.
  (cond ((token-is '|{| )
	 (list-of #'<compute-pattern> :until '|}| ))
	((token-is '|(| )
	 (list-of #'<compute-pattern> :until '|)| ))
	(t
	 (must-satisfy #'(lambda (sym)
			   (or (actor-restriction-p sym)
			       (numberp sym)
			       (symbolp sym)))
		       "a valid compute pattern component"))))

(defun <depends_on-clause> ()
  (unsupported-syntax "\"depends_on\" causes in compute conditions")
  (let ((dep (<compute-dependency>)))
    ;; Must be careful not to consume the final "," or ";".
    (if (find (token) '(|,| |;|))
	(list dep)
      (cons dep (<depends_on-clause>)))))

(defun <compute-dependency> ()
  ;; parses: <pattern> [= <value>] [from <variable-name>]
  (let (p+v from-var)
    (setq p+v (<pattern-assignment>))
    (when (token-is 'from)
      (setq from-var (<variable-name>)))
    (list p+v from-var)))

(defun <optional-condition-point> ()
  (if (token-is 'at)
      (<condition-point>)
      (default-node-end 'condition_node_end)))

(defun <condition-point> ()
  (<condition-or-effect-point> 'condition_node_end))

(defun <condition-or-effect-point> (context)
  (token-case
    ((notepad) 'notepad)
    (t         (<node-end> context))))

;;; effects clause

(defun <effects-clause> ()
  (cons 'effects
	(one-or-more #'<effect-spec>
		     :separator '|,|
		     :until '|;|)))

;;; resources clause

(defun <resources-clause> ()
  (cons 'resources
	(one-or-more #'<resource-usage-spec>
		     :separator '|,|
		     :until '|;|)))

;;; time_windows clause

(defun <time_windows-clause> ()
  (cons 'time_windows
	(one-or-more #'<time-window-spec>
		     :separator '|,|
		     :until '|;|)))

(defun <time-window-spec> ()
  (token-case

    ((duration)
     (let (node duration)
       (setq node (<node>))
       (must-be '=)
       (setq duration (<time-bounds-spec>))
       (construct-time-constraint
	 (node-end node :begin)
	 (node-end node :end)
	 duration)))

    ((delay_between)
     (let (before after delay)
       (setf before (<node-end> 'link_from_node_end))
       (or (token-is 'and) (token-is '|:|))
       (setq after (<node-end> 'link_to_node_end))
       (must-be '=)
       (setq delay (<delay-spec>))
       (construct-time-constraint before after delay)))

    (t
     (let (bounds point)
       (setq bounds (<time-bounds-spec>))
       (setq point (if (token-is 'at)
		       (<at-point> 'time_window_node_end)
		       (default-node-end 'time_window_node_end)))
       (construct-time-constraint :abst0 point bounds)))))


;;;; Initial information for plan generation

;;; initially

(defun <initially-form> ()
  (cons 'initially
	(one-or-more #'<pattern-assignment>
		     :separator '|,|
		     :until '|;|)))

;;; initial_resources

(defun <initial_resources-form> ()
  (cons 'initial_resources
	(one-or-more #'<resource-usage-spec>
		     :separator '|,|
		     :until '|;|)))

;;; initial_time

;;; /\/: Check for variables in the <time-bounds-spec>.

(defun <initial_time-form> ()
  (cons 'initial_time
	(prog1 (if (eq *token* '|;|) nil (<time-bounds-spec>))
	  (must-be '|;|))))


;;;; Task schemas

;;; task

(defun <task-form> ()
  (<schema-form> 'task 'end_task))


;;; Basics: some low-level syntax

(defun <name> ()
  ;; /\/: Should we really allow recovery at the next name?
  ;; After all, the only problem might be a bad char in the name?
  (must-satisfy #'namep "a valid name"))

(defun <number> ()
  (token-case
    ((inf infinity) *infinity*)
    (t (must-satisfy #'numberp "a number"))))

(defun <integer> ()
  (must-satisfy #'integerp "an integer"))

;;; /\/: Do we need to check that node numbers are positive?

(defun <defining-node-number> ()
  (must-satisfy #'integerp "a node number"))

(defun <node-number> ()
  (let ((n (must-satisfy #'integerp "a node number")))
    ;; Now check that it's defined.
    (when (and (parsing-schema-p)
	       (not (null n))
	       (not (member n (lookup 'nodes *schema-clauses*)
			    :key #'node-number)))
      (if (assoc 'nodes *schema-clauses*)
	  ;; Have seen nodes clause, so we can give the error now.
	  (syntax-error "Reference to undefined node number ~D." n)
	;; Else we have to save a forward reference.
	(push n (getf-schema 'forward-ref-nodes))))
    n))

;;; <node> ::= self | <node_number>
;;; N.B. <node> is not in the TF Manual.

(defun <node> ()
  (if (token-is 'self)
      :self
    (<node-number>)))

(defun <text-string> ()
  (must-satisfy #'stringp "a string"))


;;; Sets

(defun <name-set> ()
  (must-be '|(|)
  (list-of #'<name> :until '|)|))

(defun <general-set> ()
  ;; /\/: Should it really be <pattern> and not <value>, as the Manual says?
  (must-be '|(|)
  (list-of #'<value> :until '|)|))

;;; List-of is used for reading syntax such as { ... } and ( ... )
;;; where the list should not cross a statement boundary (ie a ";").

(defun list-of (nonterminal &rest other-args &key separator until)
  (apply #'zero-or-more
	 nonterminal
	 :blocking-tokens
	   (append (if until (list until))
		   (if (not (eq '|,| separator))
		       (list '|,|))
		   '(|;|))
	 other-args))

;;; /\/: Obsolete

(defun <name-list> ()
  (must-be '|(|)
  (list-of #'<name> :until '|)|))


;;; Patterns

;;; /\/: Is "( ... )" allowed as well as "{ ... }" as a pattern?
;;; /\/: Is "{}" allowed as a pattern component?

(defun <pattern> ()
  (let* ((open-char (must-be-member '(|{| |(|)
				    "a pattern bracket" "pattern brackets"))
	 (close-char (case open-char (|{| '|}|) (|(| '|)|))))
    (if (null open-char)
	nil
	(cons (<name>)
	      (list-of #'<pattern-component>
		       :until close-char)))))

(defun <pattern-component> ()
  (cond ((token-is '|{| )
	 (list-of #'<pattern-component> :until '|}| ))
	((token-is '|(| )
	 (list-of #'<pattern-component> :until '|)| ))
	(t
	 (must-satisfy #'(lambda (sym)
			   (or (actor-restriction-p sym)
			       (numberp sym)
			       (namep sym)))
		       "a valid pattern component"))))

;;; <pattern-assignment> ::= <pattern> [= <value>]

(defun <pattern-assignment> ()
  (let (p v)
    (setq p (<pattern>))
    (if (token-is '=)
	(setq v (<value>))
	(setq v (get-default-value 'value)))
    (pv-pair p v)))

(defun <value> () (<pattern-component>))


(defun patternp (obj)
  ;; N.B. does not check subpatterns of actors, but can assume
  ;; some chacking has been done by the read macro for "?".
  (or (symbolp obj)			;/\/: Check for illegal chars?
      (numberp obj)			;/\/: Integer and float only?
      (actorp obj)			;/\/: Import actorp?
      (and (listp obj)
	   (every #'patternp obj))))

(defun fully-instantiated-pattern-p (pat)
  (and (not (eq pat actorsym))		;/\/: import?
       (or (symbolp pat)
	   (numberp pat)
	   (and (listp pat)
		(not (actorp pat))
		(every #'fully-instantiated-pattern-p pat)))))

;;; Variables

;;; A variable is written ?<name>, and this is read as an actor.
;;; We have to extract the name from (?? :*act (given <name>)).

(defun <variable-name> () ; -> symbol
  (let* ((actor (must-satisfy #'var-pattern-p "a variable name"))
	 (name (actor->variable-name actor)))
    (unless (namep name)
      (syntax-error "Invalid characters in a name: \"~A\"." name))
    name))

(defun var-pattern-p (obj)
  (and (actorp obj)
       (eq (actfn obj) 'given)))	;/\/: package of "given"?

(defun actor->variable-name (actor)
  (car (actargs actor)))

(defun variable-name->actor (name)
  (oplan-obase:make-given-actor name))

;;; <variable> returns the whole actor, not just the name

(defun <variable> () ; -> given actor
  (let* ((actor (must-satisfy #'var-pattern-p "a variable name"))
	 (name (actor->variable-name actor)))
    (unless (namep name)
      (syntax-error "Invalid characters in a name: \"~A\"." name))
    actor))

;;; <variable-restriction> ::= <actor-restriction> | undef

(defun <variable-restriction> ()
  (let ((token (must-satisfy #'(lambda (sym)
				 (or (eq sym 'undef)
				     (actor-restriction-p sym)))
			     "a valid variable restriction")))
    (if (or (eq token 'undef)
	    (eq token actorsym))	;treat "??" as undef here
	*undef*
	token)))

(defun <actor-restriction> ()
  (must-satisfy #'actor-restriction-p
		"a valid actor restriction"))

(defun actor-restriction-p (obj)
  (or (eql obj actorsym)		;/\/: import?
      (actorp obj)))

;;; <restriction-assignment> ::= <variable-name> [= <variable-restriction>]

(defun <restriction-assignment> ()
  (let (var restriction)
    (setq var (<variable-name>))
    (setq restriction
	  (if (token-is '=)
	      (<variable-restriction>)
	    (get-default-value 'variable_restriction)))
    (var-def var restriction)))


;;; Expressions
;;;
;;; N.B. "1++2" is a use of the undefined operator "++" and not one plus
;;; positive two.  The same occurs with "+-", "-+", "--", "*+", "*-", etc.
;;;
;;;   <expr> ::= <term> [<addop> <term>] ...
;;;   <term> ::= <factor> [<mulop> <factor>] ...
;;;   <factor> ::= <variable>
;;;             |  <number>
;;;             |  <sign> <factor>
;;;             |  (<expr>)
;;;   <addop> ::= + | -
;;;   <mulop> ::= * | /
;;;   <sign>  ::= + | -
;;;
;;; The binary operators are left associative.

(defun <expression> ()
  (let ((e (<expr>)))
    (if (or (atom e) (var-pattern-p e))
	e
      (unsupported-syntax "non-atomic expressions"))))

(defun <expr> ()
  (let ((*reading-expression* t))
    (<operation-seq> #'<term> '(+ -))))

(defun <term> ()
  (<operation-seq> #'<factor> '(* /)))

(defun <factor> ()
  ;; In effect we have to expand <number> and <variable-name> in line.
  (token-case
    ((+)            (<factor>))
    ((-)            `(- ,(<factor>)))
    (( |(| )        (prog1 (<expr>) (must-be '|)| )))
    ((inf infinity) *infinity*)
    (t
     (cond ((token-satisfies #'numberp))
	   ((var-pattern-p (token))
	    (<variable>))
	   (t
	    (syntax-error "Found \"~A\" in expression." (token)))))))

(defun <operation-seq> (operand operators)
  ;; <op-seq> ::= <operand> [<operator> <operand>] ...
  (let ((operator nil) (fn nil) (arg nil) (args '()))
    (setq arg (funcall operand))
    (setq operator (token))
    (unless (member operator operators)
      (return-from <operation-seq> arg))
    (setq fn operator args (list arg))
    (loop (next-token)
	  (push (funcall operand) args)
	  (setq operator (token))
	  (unless (member operator operators)
	    (return))
	  (unless (eq operator fn)
	    (setq args (list (cons fn (nreverse args)))
		  fn operator)))
    (cons fn (nreverse args))))


;;; Time specifications

;;; <time_spec> ::= <expression> [<time_units>]
;;;              |  [<days> ~] <hours> : <minutes> [: <seconds>]
;;; <time_units> ::= seconds | minutes | hours | days
;;; <days>, <hours>, <minutes> and <seconds> are <integer>s.
;;;
;;; Note that <expression> subsumes the <integer> that might be <days>
;;; or <hours>.  So we parse an <expression> and then see what follows
;;; it.

(defun <time-spec> () ; -> seconds or expr
  (let ((e 0) (factor nil) (days 0) (hours 0) (minutes 0) (seconds 0))
    (flet ((m-s ()
	     (setq minutes (<integer>))
	     (when (token-is '|:|)
	       (setq seconds (<integer>)))))
      (setq e (<expression>))
      (token-case
        ((seconds) (setq factor 1))
	((minutes) (setq factor 60))
	((hours)   (setq factor (* 60 60)))
	((days)    (setq factor (* 24 60 60)))
	((~)       (if (integerp e)
		       (setq days e)
		       (syntax-error "Non-integer days: \"~A\"." e))
	           (setq hours (<integer>))
	           (must-be '|:|)
	           (m-s))
	((|:|)     (if (integerp e)
		       (setq hours e)
		       (syntax-error "Non-Integer hours: \"~A\"." e))
	           (m-s))
	(t         (setq factor 1)))

      ;; If factor is non-nil, we're in the case <expression> [<time_units>].
      ;; We want to multiply the value of the expression by the factor, which
      ;; we can do if the expression is a number.  Otherwise we return an
      ;; expression.  (/\/: Do we?  It's not supported yet.)

      ;; If factor is nil, we must convert to seconds from 0~0:0:0.
      ;; If there have been errors, some of the quantities may be nil.
      (if (numberp factor)
	  (if (numberp e)
	      (* e factor)
	      (if (eq e *infinity*)
		  e
		  (if (null e)
		      nil
		      (time-spec-expression e factor))))
	  (if (and days hours minutes seconds)
	      (time-spec days hours minutes seconds)
	      0)))))

(defun time-spec-expression (e factor)
  (if (not (var-pattern-p e))
      (unsupported-syntax "non-atomic expressions in <time_spec>s")
    (if (/= factor 1)
	(unsupported-syntax
	 "variables with units other than seconds in <time_spec>s")
      e)))


;;; Time bounds specs

;;; Changes:  /\/
;;;
;;;   *  We've taken the parenthesised case out of <time_bounds_pair>
;;;      and put it in <time_bounds_spec>.
;;;
;;;   *  When "begin" does not appear, and both a min and max <time_spec>
;;;      are specified, "and" or ".." must appear between the min and max.
;;;      This is so even when the <time_bounds_spec> is sorrounded by
;;;      parens.  (We could make the "and" or ".." optional in that case,
;;;      but for consistency and error checking it is better not to.

(defun <time-bounds-spec> () ; -> time-window
  (when (token-is '|(|)
    (return-from <time-bounds-spec>
      (prog1 (<time-bounds-spec>) (must-be '|)|))))
  (let (t1 t2)
    (multiple-value-setq (t1 t2) (<time-bounds-pair>))
    (if (token-is 'with)
	(make-time-window :min t1 :max t2 :ideal (<time-preference>))
	(make-time-window :min t1 :max t2))))

(defun <time-preference> ()
  (let (pref-value)
    (must-be-member '(ideal) "a valid time preference"
		    "preference types")
    (must-be '=)
    (setq pref-value (<time-spec>))
    pref-value))

(defun <time-bounds-pair> () ; -> min, max
  (let ((t1 0)
	(t2 *infinity*))
    (token-case
      ((after  >)  (setq t1 (<time-spec>)))
      ((before <)  (setq t2 (<time-spec>)))
      ((occurs_at) (setq t1 (<time-spec>))
                   (setq t2 t1))
      ((between)   (when (token-is 'et) (must-be '=))
                   (setq t1 (<time-spec>))
		   (or (token-is 'and) (token-is '|..|))
		   (when (token-is 'lt) (must-be '=))
		   (setq t2 (<time-spec>)))
      (t (when (token-is 'et) (must-be '=))
	 (setq t1 (<time-spec>))
	 (token-case
	   ((and |..|) (when (token-is 'lt) (must-be '=))
	               (setq t2 (<time-spec>)))
	   (t (setq t2 t1)))))
    (values t1 t2)))


;;; Resource specifications

;;; /\/: <resource-units> is called <resource-unit> in the desription
;;; of the resource_types form.  The information about resources should
;;; be made more uniform and put in one section of the Manual rather
;;; than two.

;;; /\/: The <resource-units> of a <resource-usage-spec> is optional,
;;; but there's no keyword or other punctuation to indicate whether or
;;; not it's present.  It might be thought that we could look at the
;;; list of declared units to determine what names were valid, but
;;; that would prevent us from detecting misspelled names.  We couldn't
;;; distinguish between a misspelled name and other errors.  But the
;;; current approach may be worse.  We assume that any <name> after
;;; the <resource-range> is a <resource-unit> unless it is "at" or
;;; "overall", which would mark the start of a <resource-scope-spec>.
;;; Consequently, (1) we fail to keep the syntactic knowledge of
;;; <resource-scope-spec> in a single place, and (2) we have to hope
;;; that <resource-usage-spec> never occurs in a rule where it can be
;;; followed by a <name> that should not be interpreted as a unit.

(defparameter *resource-usage-synonyms*
  '((set . *) (allocates . -) (deallocates . +)
    (produces . ^) (consumes . v)))

(defun <resource-usage-spec> ()
  (let (keyword resource range units scope at)
    (setq keyword (<resource-usage-keyword>))
    (setq resource (<resource-spec>))
    (must-be '=)
    (setq range (<resource-range>))
    ;; The unit is optional, but it's hard to tell whether it's present.
    (when (and (namep (token))
	       (not (member (token) '(at overall))))
      (setq units (<resource-units>)))
    (multiple-value-setq (scope at) (<resource-scope-spec>))
    (resource-operation keyword resource range units scope at)))

(defun <resource-usage-keyword> () ; -> long-form or nil
  ;; /\/: Why is this optional (hence the possibility of returning nil)?
  (let ((pair (or (assoc (token) *resource-usage-synonyms*)
		  (rassoc (token) *resource-usage-synonyms*))))
    (cond ((null pair)
	   nil)
	  (t (next-token)
	     (car pair)))))

(defun <resource-range> ()
  (if (token-is 'unlimited)
      (range 0 *infinity*)		;/\/ is this right?
    (<min-max-spec>)))

(defun <resource-units> ()		;/\/: compare <resource-unit> above
  (<name>))				;/\/: is this right?

(defun <resource-scope-spec> () ; -> at | overall , node-end
  ;; N.B. acts as optional.
  (token-case
    ((overall)
     (values 'overall (node-end :self :begin)))
    ((at)
     (values 'at (<at-point> 'resource_usage_node_end)))
    (t
     (values 'overall (node-end :self :begin)))))

;;; /\/: An <at_point> is the part of an <at_spec> after the "at".

(defun <at-point> (context)
  (<node-end> context))

;;; <resource_spec> ::= {resource <resource_name>
;;;                               [<resource_qualifier_spec>] ...}

;;; <resource_qualifier_spec> ::= <actor_restriction> ???

(defun <resource-spec> ()
  (let (name qualifiers)
    (must-be '|{| )
    (must-be 'resource)
    (setq name (<name>))
    (setq qualifiers
	  (list-of #'<resource-qualifier> :until '|}|))
    (list* 'resource name qualifiers)))

(defun <resource-qualifier> ()
  (<pattern-component>))		;/\/: how defined?


;;; Numeric bounds

;;; Note that it is not possible for the first <expression> to begin
;;; with "(", because the "(" will be interpreted as "(<min_max_spec>)".

(defun <min-max-spec> () ; -> (range min max)
  (let ((x 0)
	(y *infinity*))
    (token-case
      ((min)     (setq x (<expression>))
                 (when (token-is '|..|)
		   (must-be 'max)
		   (setq y (<expression>))))
      ((>=)      (setq x (<expression>)))
      ((max)     (setq y (<expression>)))
      ((<=)      (setq y (<expression>)))
      ((no none) (setq x 0 y 0))
      ((some)    (setq x 0 y *infinity*))
      (( |(| )   (return-from <min-max-spec>
		   (prog1 (<min-max-spec>) (must-be '|)| ))))
      (t         (setq x (<expression>))
	         (setq y (if (token-is '|..|) (<expression>) x))))
    ;; x and y now contain the lower and upper bounds, respectively.
    (range x y)))


;;; Node ends

;;; <node_end> ::= [<end>] { <node_number> | self }

(defun <node-end> (context)
  (let (end node)
    (setq end
	  (ecase (or (token-satisfies
		       #'(lambda (sym)
			   (member sym '(begin_of end_of))))
		     (find context '(begin_of end_of))
		     (get-default-value context))
	    ((begin_of) :begin)
	    ((end_of)   :end)))
    (setq node
	  (if (token-is 'self) ':self (<node-number>)))
    (node-end node end)))

(defun default-node-end (context)
  (node-end ':self
	    (ecase (get-default-value context)
	      ((begin_of) :begin)
	      ((end_of)   :end))))

;;; End
