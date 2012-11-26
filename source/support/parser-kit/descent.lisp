;;;; File: descent.lsp
;;; Contains: Framework for recursive descent parsers
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: March 1992
;;; Updated: Tue Mar 16 04:17:41 1999 by Jeff Dalton
;;; Copyright: (c) 1992, 1993, 1994 AIAI, University of Edinburgh

;;; Recursive descent parsers can be written almost directly from
;;; LL(1) grammars.  The result is a top-down parser in the form of a
;;; set of procedures, one for each nonterminal symbol in the grammar.
;;; To write such a parser, the programmer adopts conventions for such
;;; things as error recovery and when to advance to the next token, and
;;; then converts the grammar into procedure definitions in a fairly
;;; mechanical way.  This file:
;;;
;;;   1. Documents a set of conventions for writing parsers.
;;;   2. Describes the translation from a subset of LL(1) grammar
;;;      rules to procedures.
;;;   3. Provides the low-level procedures for such things as
;;;      recognizing tokens, signalling errors, and error recovery.
;;;   4. Provides some higher-level procedures for such things as
;;;      parsing a sequence of one or more instances of a nonterminal.
;;;
;;; The procedures are intended to make it easier to write parsers
;;; that follow the documented conventions, easier to read the resulting 
;;; code, and easier to change the conventions.

(in-package :oplan-parser-kit)

;;; /\/: One way to limit the number of tokens that could be consumed
;;; incorrectly by error recovery, or by one-or-more, would be to keep
;;; a list of the :until tokens given to the still active calls to
;;; one-or-more.  The "skip-" procedures, and one-or-more, would return
;;; on encountering any one of these symbols.  A more conservative
;;; version of this idea is provided by the *blocking-tokens* parameter.


;;;; Programming conventions.
;;;
;;; A nonterminal is implemented as a function that consumes zero or
;;; more tokens and then returns some result, such as a parse tree.
;;; Such functions should obey the following conventions:
;;;   * A nonterminal matches an initial segment of the input tokens.
;;;   * When called, a nonterminal can assume that the current token
;;;     has already been set to the first token it should consider.
;;;   * On exit, the current token should be the first one that is
;;;     not part of the segment matched.  That is, it should be the
;;;     first token that the rest of the parser should consider.
;;;
;;; Some additional conventions are required by the error recovery
;;; scheme:
;;;   * If a syntax error is detected, it should be signalled by
;;;     calling the procedure SYNTAX-ERROR.  Note that calls to
;;;     SYNTAX-ERROR usually return, so you should be prepared to
;;;     continue parsing.  If SYNTAX-ERROR does return, you can
;;;     rely on it returning NIL.
;;;   * At a point where a certain token must be present (eg, the THEN
;;;     in an if-statement), require that it be present (ie, check and
;;;     signal an error if not) by calling the the procedure MUST-BE.
;;;     Something similar can be done for classes of tokens by calling
;;;     MUST-SATISFY or MUST-BE-MEMBER.
;;;
;;; These conventions are explained further in the section on error
;;; recovery.
;;;
;;; Another set of conventions concerns how the parsing procedures
;;; interact with the scanner (or "tokenizer").  They obtain the
;;; current token by (TOKEN) and advance to the next by (NEXT-TOKEN).
;;; Tokens can be any Lisp objects, but since MUST-BE makes an EQL
;;; comparison and uses the expected value in an error message, it's
;;; generally a good idea to use Lisp symbols for language keywords
;;; (IF, THEN, CASE, BEGIN, END, etc) and symbols or characters for
;;; certain kinds of punctuation (eg, "{" and "}" as alternatives to
;;; BEGIN and END).  However, some other objects could be used instead
;;; if they were readable when printed and could be meaningfully
;;; compared by EQL.
;;;
;;; Some token classes (eg, binary operators) might be handled by
;;; devoting a nonterminal to them or by defining a 1-arg predicate
;;; and using MUST-SATISFY.  An advantage of the predicate is that it
;;; can also be used to select among the alternatives for a nonterminal
;;; while conforming to the rule that this decision not look beyond
;;; the current token.


;;;; Translating grammar rules to Lisp code.
;;;
;;; To illustrate the coding conventions, we'll consider a subset of
;;; LL(1) grammars that can be translated directly to code and that is
;;; sufficient to cover most of the statement-level cases that arise
;;; in programming languages.
;;;
;;; A given nonterminal may be defined by more than one rule, or by
;;; one rule with several alternatives.  If so, we need to be able to
;;; select among the alternatives without looking beyond the current
;;; token.  It will clearly be possible to do this if all but at most
;;; one of the alternatives begins with a terminal symbol and the
;;; remaining (nonterminal) alternative (if any) must begin in some
;;; other way when expanded.  A rule of that form can be translated 
;;; into a function definition as follows (or by using the equivalent
;;; TOKEN-CASE macro):
;;;
;;;    <NT> ::= t_1 <NT_1> | t_2 <NT_2> | ... | <NT_n>
;;;      ==> (defun <NT> ()
;;;            (case (token)
;;;              ((t_1) (next-token) (<NT_1>))
;;;              ((t_2) (next-token) (<NT_2>))
;;;              ...
;;;              (otherwise <NT_n>)))
;;;
;;; This can clearly be generalized to handle cases where the alternatives
;;; are more complex, and to cases where arbitrary predicates (not just EQL)
;;; are applied to the current token.  In other cases, it will be necessary
;;; to transform the grammar to a form that can be handled directly or to
;;; depart somewhat from LL(1), perhaps by looking ahead more then one token.
;;;
;;; Once we've selected an alternative, we're committed to it and
;;; should report an error if the input doesn't match what we expect.
;;; Some of the more common constructs that occur within alternatives
;;; can therefore be translated as follows:
;;;
;;;    <NT>
;;;      ==>  (<NT>)
;;;
;;;    <literal>
;;;      ==>  (must-be '<literal>)
;;;
;;;    [<literal> <NT>]
;;;      ==>  (when (token-is '<literal>) (<NT>))
;;;
;;;    <NT> [<separator> <NT>]* <end>
;;;      ==>  (one-or-more #'<NT> :separator '<separator> :until '<end>)
;;;
;;; Note that in the last case, both instances of "<NT>" must be replaced
;;; by the same nonterminal.  For more information about the functions
;;; on the Lisp side, see the comments and definitions below.


;;; Scanner interface

;;; The scanner / lexical analyzer provides a token generator which
;;; can be accessed via the procedures TOKEN and NEXT-TOKEN.  TOKEN
;;; returns the current token; NEXT-TOKEN advances to and returns
;;; the next token, making it the new current token.

;;; The generator should be a function of no arguments that returns
;;; the next token when called.  When there are no more tokens, it
;;; should return *END-TOKEN*.  Note that a generator may have to
;;; return the end-token more than once.

;;; The value held in *END-TOKEN* is defined as part of the definition
;;; of the scanner.  Here we just make sure the variable has been declared.

(defmacro with-token-generator (fun &body body)
  `(let ((*token-generator* ,fun)
	 (*token* nil))
     ,@body))

(defvar *token*)
(defvar *token-generator*)
(proclaim '(special *end-token*))

(defun-inline token ()
  *token*)

(defun next-token ()
  (setq *token* (funcall *token-generator*)))

;;; (PUSH-TOKEN sym) makes the next call to *token-generator*
;;; return sym.  Note that this can happen inside a call to
;;; *token-generator*

(defun push-token (sym)
  (let ((gen *token-generator*))
    (setq *token-generator*
	  #'(lambda ()
	      (prog1 sym (setq *token-generator* gen)))))
  sym)


;;; Compiler interface

(defparameter *syntax-error-reporter*
  #'(lambda (&rest error-args)
      (apply #'cerror "Continue compiling." error-args))
  "The procedure called by syntax-error to report an error.")

(defvar *recovering* nil
  "True when trying to recover from a syntax error.")

(defparameter *blocking-tokens* '()
  "Tokens that should not be passed during iteration or error recovery.")

(defvar *error-count* 0
  "The number of errors detected.")

(defvar *warning-count* 0
  "The number of warnings issued.")

;;; The procedure TEST-COMPILE is provided for testing and to illustrate
;;; how the interface might be used.

;;; A typical test call might look like this:
;;;   (test-compile #'<statament>
;;;                 (list-tokens '(if a then b else c)))

(defun test-compile (nonterminal generator)
  (let ((*recovering* nil)
	(*error-count* 0)
	(*warning-count* 0))
    (with-token-generator generator
      (next-token)
      (values (funcall nonterminal)
	      *error-count*))))

(defun list-tokens (lis)		;a useful generator
  #'(lambda ()
      (if (null lis) *end-token* (pop lis))))


;;;; Match primitives and error recovery
;;;
;;; We use a version of the "S-algol error recovery scheme" described
;;; in the book _Recursive Descent Compiling_ by A. J. T. Davie and R.
;;; Morrison (Ellis Horwood, 1981), which in turn comes from a scheme
;;; developed by D. A. Turner for a SASL compiler.
;;;
;;; This scheme is efficient, reasonably easy to use, and can recover
;;; from a number of common 1-symbol errors that involve omitting a
;;; symbol, inserting an extra symbol or replacing one symbol by another
;;; (eg, "thne" instead of "then").  However, it can sometimes throw
;;; away a large amount of the input in an attempt to get back in sync.
;;; In this case the parser will, of course, fail to detect errors in
;;; the discarded text; on the other hand, it will also fail to produce
;;; a series of misleading error messages based on its misunderstanding
;;; of that same text.  If too much (or too little) is discarded in some
;;; common case, it will often be possible to add some special-purpose
;;; processing for that case while still using the general method for
;;; the rest.  (A minor example can be found in the procedure ONE-OR-MORE.)
;;;
;;; The basic idea is take advantage of points where a particular token
;;; or class of tokens must occur.  If the current token is not acceptable,
;;; the normal action would be to report an error.  However, if the parser
;;; is already in an error state, (indicated by the variable *recovering*
;;; being true), it can try to recover instead, by discarding tokens until
;;; an acceptable one appears.  While the parser is waiting to recover
;;; from an error, further error messages are suppressed
;;;
;;; The parser should use SYNTAX-ERROR to report errors and the "MUST-"
;;; procedures to check for required tokens, so that the recovery process
;;; will take place automatically.  In most cases where a required token
;;; is not found, parsing should continue as if the error had not occurred.
;;; In this way, the parser can recover from missing symbols (eg, the
;;; "then" in a if-statement).
;;;
;;; Here's an example.  Suppose the syntax of an "if" statement is
;;; "if <expression> then <statement> fi".  The procedure for parsing
;;; an "if" will include code like the following:
;;;
;;;    (setq test (expression))
;;;    (must-be 'then)
;;;    (setq consequent (statement))
;;;    (must-be 'fi)
;;;
;;; If the only error is a missing or misspelled "then", then calling
;;; (MUST-BE 'THEN) will detect and report the error; the parser can
;;; continue normally and will get out of the *recovering* = true
;;; state when it reaches the "fi" (if not before).
;;; 
;;; If a syntax error occurs while parsing the <expression>, and recovery
;;; has not yet occurred, then calling (MUST-BE 'THEN) will advance to
;;; the next "then".  In many cases, this will be the right action to take.
;;; In others, of course, it may be wrong.  For instance, if the "then" is
;;; missing (or has been parsed as part of the expression), MUST-BE will
;;; advance to the "then" in a different "if" statement later in the source,
;;; which is less than ideal (though probably not disastrous).  However,
;;; given that perfect error recovery is impossible, there will always be
;;; some cases that confuse a parser.  The aim of a recovery scheme is to 
;;; reduce the number of spurious error messages rather than to eliminate
;;; them entirely, and we have to balance the cost of implementing and using
;;; a more complex scheme against its likely benefits.

;;; MUST-BE requires that the current token be a particular token.

(defun must-be (target) ; -> token
  (when *recovering*
    (recover-at target))
  (if (eql (token) target)
      (prog1 (token) (next-token))
    (token-mismatch-error (token) target)))

(defun token-mismatch-error (sym target)
  (syntax-error "Found \"~A\" when expecting \"~A\"." sym target))

;;; MUST-SATISFY requires that the current token satisfy a 1-arg predicate.
;;; The "description" should be a string that begins with an indefinite
;;; article.  For instance: (must-satisfy #'operator-p "a valid operator").

;;; Note that the predicate can be called more than once on a given token.

(defun must-satisfy (pred description) ; -> token
  (when *recovering*
    (recover-when pred))
  (if (funcall pred (token))
      (prog1 (token)
	     (next-token))
    (syntax-error "\"~A\" is not ~A." (token) description)))

;;; MUST-BE-MEMBER is like MUST-SATISFY for set membership.

(defun must-be-member (set singular-description plural-description) ; -> token
  (flet ((pred (sym) (member sym set)))
    (when *recovering*
      (recover-when #'pred))
    (if (pred (token))
	(prog1 (token) (next-token))
      (token-set-error (token) set singular-description plural-description))))

(defun token-set-error (sym set singular-description plural-description)
  (syntax-error "\"~A\" is not ~A.~%Valid ~A are: ~{\"~A\"~^, ~}."
		sym singular-description plural-description set))

;;; SYNTAX-ERROR might not return, either because the error reporter
;;; doesn't return or because the reporter signalled a condition that
;;; caused an unwind.

(defun syntax-error (string &rest format-args)
  (unless *recovering*
    (apply *syntax-error-reporter* string format-args)
    (incf *error-count*)
    (setq *recovering* t)
    nil))

(defun recover-at (target)
  (assert *recovering*)
  (skip-to target)			;put us at target or *end-token*
  (when (eql (token) target)
    (setq *recovering* nil)))

(defun recover-when (pred)
  (assert *recovering*)
  (skip-until pred)
  (when (funcall pred (token))
    (setq *recovering* nil)))


;;; SKIP-TO and SKIP-UNTIL are used to throw away tokens until a given
;;; token, or token satisfying a given predicate, is found.  The current
;;; token remains at that token rather than being advanced one beyond it.

(defun skip-to (target)
  (do () ((or (eql (token) target)
	      (eql (token) *end-token*)
	      (member (token) *blocking-tokens*)))
    (next-token)))

(defun skip-until (pred)
  (do () ((or (funcall pred (token))
	      (eql (token) *end-token*)
	      (member (token) *blocking-tokens*)))
    (next-token)))


;;; TOKEN-IS combines a test and advance.

(defun token-is (target)
  (cond ((eql (token) target) (next-token) t)
	(t nil)))

(defun token-satisfies (pred) ; -> token or nil
  (cond ((funcall pred (token)) (prog1 (token) (next-token)))
	(t nil)))


;;; (TOKEN-CASE clause+) is like (CASE (TOKEN) clause+) except that it
;;; automatically inserts a call to NEXT-TOKEN as the first form in
;;; every clause except the default clause (indicated by T or OTHERWISE).
;;; The default clause should be last, but we leave it to the CASE macro
;;; to check.

(defmacro token-case (&rest clauses)
  `(case (token)
     ,@(mapcar #'(lambda (clause)
		   (if (and (symbolp (car clause))
			    (member (car clause) '(t otherwise)))
		       clause
		     `(,(car clause)
		       (next-token)
		       ,@(cdr clause))))
	       clauses)))


;;; ONE-OR-MORE is used to parse something of the form
;;;
;;;    <NT> [<separator> <NT>]* <end>
;;;
;;; where both instances of <NT> are replaced by the same nonterminal,
;;; <separator> and <end> are literals, and the <separator> is optional.
;;; That is, a sequence of one or more <NT>s, perhaps separated by a
;;; literal such as a comma, and ending in a required literal such as
;;; END or }.
;;;
;;; ZERO-OR-MORE is similar, but allows zero instances of <NT>.
;;;
;;; These functions do things differently depending on whether or not
;;; a separator is specified.  If there is a separator, we have more
;;; local opportunities for error detection and recovery.
;;;
;;; Note that an infinite loop will occur if the iterated nonterminal
;;; repeatedly fails to call NEXT-TOKEN, which can happen when the
;;; current token is a syntax error.  ONE-OR-MORE will move forward
;;; automatically after an error if a :separator was specified, and
;;; nonterminals that test the current token by calling MUST-BE or
;;; MUST-SATISFY will move forward on the next iteration (because
;;; *RECOVERING* will be true), but in other cases the nonterminal
;;; may have to take steps of its own.

;;; /\/: If one-or-more gets to *end-token* without finding its
;;; until-token, the error message should come out even if it's in
;;; recovery mode; but it that case maybe it should say "Can't find"
;;; instead of "Missing".  What about hitting a blocking token before
;;; the until-token?

;;; /\/: It might be better to make N-or-more be macros.  For one thing,
;;; we could detect a missing :UNTIL at compile-time.

(defun zero-or-more (nonterminal &rest key-args
		                 &key (until nil until-p)
				      (blocking-tokens *blocking-tokens*)
				 &allow-other-keys)
  (unless until-p (error "Missing :UNTIL in a call to ZERO-OR-MORE."))
  (cond ((token-is until)
	 '())
	((member (token) blocking-tokens)
	  (syntax-error "Hit \"~A\" before \"~A\"." (token) until))
	((eql (token) *end-token*)
	 (syntax-error "Missing \"~A\"." until))
	(t
	 (apply #'one-or-more nonterminal key-args))))


(defun one-or-more (nonterminal &key (until nil until-p)
				     (separator nil separator-p)
				     (blocking-tokens *blocking-tokens*))
  (unless until-p (error "Missing :UNTIL in a call to ONE-OR-MORE."))
  (if (not separator-p)

      ;; No separator.
      (let ((*blocking-tokens* blocking-tokens)
	    (results '()))
	(loop (push (funcall nonterminal)
		    results)
	      (when *recovering*
		(pop results))		;drop questionable item
	      (when (token-is until)
		(return))
	      (when (member (token) blocking-tokens)
		(syntax-error "Hit \"~A\" before \"~A\"." (token) until)
		(return))
	      (when (eql (token) *end-token*)
		(syntax-error "Missing \"~A\"." until)
		(return)))
	(nreverse results))

      ;; Use separator.  
      (let ((*blocking-tokens* blocking-tokens)
	    (results '()))
	(loop (push (funcall nonterminal)
		    results)
	      (when *recovering*
		(pop results)		;drop questionable item
		(recover-when
		  #'(lambda (sym) (or (eql sym separator) (eql sym until)))))
	      (when (token-is until)
		(return))
	      (when (member (token) blocking-tokens)
		(syntax-error "Hit \"~A\" before \"~A\"." (token) until)
		(return))
	      (when (eql (token) *end-token*)
		(syntax-error "Missing \"~A\"." until)
		(return))
	      ;; Was (must-be separator) but, because of the recover-when
	      ;; above, must-be would only signal an error, not recover.
	      ;; So we may as well give a better error message.  Note
	      ;; that after the error, we go around the loop and call
	      ;; nonterminal again before forcing recovery.  Whether
	      ;; we should do that is not clear, but calling nonterminal
	      ;; again allows the NT to recover, which it sometimes can.
	      (unless (token-is separator)
		(syntax-error "Found \"~A\" when expecting \"~A\" or \"~A\"."
			      (token) separator until)))
	(nreverse results))))

;;; End
