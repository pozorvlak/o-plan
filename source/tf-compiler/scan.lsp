;;;; File: scan.lsp
;;; Contains: TF scanner
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: March 1992
;;; Updated: Tue Jul  9 00:49:43 1996 by Jeff Dalton
;;; Copyright: (c) 1992, 1993, 1994 AIAI, University of Edinburgh

(in-package :oplan-tf-compiler)

;;; From the point of view of the parser, the scanner is a token
;;; generator (see the "descent" file).  A generator that reads
;;; TF tokens from a character stream can be created by calling
;;; (TF-TOKENS stream).

;;; The next TF token is read from a stream by calling READ with
;;; *READTABLE* bound to a readtable produced by MAKE-TF-READTABLE.
;;; The value of *TF-READTABLE* is such a readtable.

;;; Using the TF readtable causes several kinds of tokens to be
;;; returned.  Names (eg, "abc") and operators (eg, ">=", "-", "--->"),
;;; are returned as symbols, punctuation characters (eg, "," and ";")
;;; as single-character symbols and numbers as numbers.  In addition,
;;; some more complex objects, such as <actor_restriction>s, are
;;; returned as single tokens.

;;; The idea behind having names, operators, and punctuation returned
;;; as symbols is for the scanner to provide a natural representation
;;; in Lisp that takes advantage of Lisp's ability to compare symbols
;;; with EQ and that allows a natural grouping of characters (eg, ">="
;;; as one token instead of two, and "{}," as three tokens).

;;; In addition, operators and punctuation are both returned as
;;; symbols to make easier to move a character from one class to the
;;; other.  (Eg, "*" could be punctuation to make "*-" read as two
;;; tokens in "3*-4".)

;;; The grouping of characters into symbols is defined in terms of
;;; character classes.

;;; The TF readtable allows names to be read as symbols in the
;;; usual way.  Letters are converted to upper case, just as they
;;; are in normal CL input.  However, many of the characters that
;;; are normally allowed in CL symbols have special meanings in TF.

;;; Tokens other than names are read by character macros.  The
;;; macro characters are divided into disjoint "character classes".
;;; All characters in a class are given the same function as their
;;; definition, and this function is responsible for reading the
;;; entire token.  For instance, all digits are defined as macros
;;; that read numbers.  Some characters do not fit into any class
;;; and are given special macros of their own.

;;; Operator symbols are constructed by taking a maximal sequence
;;; of adjacent characters in the "operator-char" class.  This is
;;; a natural grouping of characters that does not require that
;;; the valid operators be declared explicitly.  However, it also
;;; means that a sequence such as "*-" will read as a single token.
;;; Consequently, "3*-4" will not be interpreted as "3 times -4".
;;; You would have to write "3 * -4" or "3*(-4)" instead.

;;; In addition to the macro classes, some other classes are defined
;;; for use within macros.  The "exponent markers" used in floating
;;; point numbers are an example.

;;; (Continued.)


;;; The character classes are defined below.  In most cases, there is
;;; both a variable that holds a sequence (usually a string) containing
;;; the characters in the class and a predicate.  Routines in other
;;; files should not look directly at the variables.  Instead they
;;; should call the corresponding predicates.

;;; It is generally possible to move a character from one class
;;; to another provided that no ambiguity is created (either in the
;;; scanner or at higher levels of the parser).  Note however that
;;; the sign characters ("+" and "-") are a special case.  To change
;;; their class may require explicit changes to OPCHAR-P and to
;;; the macro function that reads sign chars.  Dot (".") is also
;;; a special case -- see the code for reading numbers.

;;; As far as the TF readtable is concerned, the characters allowed in
;;; names are defined implicitly as the characters Common Lisp treats
;;; as "constituents" minus any characters defined as macros.  However,
;;; these characters are also defined explicitly by the predicates
;;; NAME-START-P and NAME-CHAR-P.  The parser can use these predicates
;;; to check whether a symbol returned by the scanner is valid as a
;;; <name>.  A NAMEP predicate that checks all the characters in a name
;;; is provided for this purpose.

;;; N.B.:

;;; Re-evaluate the definition of *TF-READTABLE* if any of the
;;; character classes or character macro functions are redefined.
;;; (The best way to do that may be to reload this file.)

;;; Some character macros have to read other characters or objects.
;;; You might think we should call read and read-char with a true value
;;; for the recursive-p parameter, but that can cause Common Lisp to
;;; signal an end of file error regardless of the value of the
;;; eof-error-p parameter.

;;; "{" is a punctuation even though character macros defined elsewhere
;;; in the planner expect it to read a list.  "?" is handled specially
;;; in part so that it can provide a list-reading interpretation of "{"
;;; and "(" to cooperate with the O-Base code for reading actors.

;;; "." is allowed in operators and in numbers, but cannot begin or
;;; end a number.  Indeed, "." must always be between two digits when
;;; it appears in a number.

;;; When "/" is a macro character, it no longer functions as single-escape
;;; inside strings.  Instead, it's treated as an ordinary character and
;;; made part of the string.

;;; In KCL (at least), ":" is still read as the package marker when in
;;; the middle of a name even though ":" has been defined as a terminating
;;; macro.  However it won't read what follows the ":" properly as a
;;; symbol name.  Eg, "user:bill" will produce an error about a symbol
;;; with no name (it seems) not being external in the USER package.

;;; In most cases where we call peek-char, we then read the character.
;;; So it would probably be more efficient to use read/unread instead.


;;; Character classes

(defparameter *punctuation-chars* "(){}[],:"
  "Characters that should normally be read as single-character objects.")

(defparameter *operator-chars* "*^/=><~&."
  "Characters that make up operators that may consist of more than
   one character.  A sequence of characters from this set is normally
   read as a single symbol.")

(defparameter *extra-name-chars* "_%"
  "Characters in addition to letters and digits that can occur in
   (the middle of) identifiers.")

(defparameter *illegal-chars* "\\|#@`'$"
  "Printing characters that are not allowed in source.")

(defparameter *digit-can-begin-name* t
  "True to allow names such as 32nd_x.")

(defparameter *digit-chars* "0123456789")

(defparameter *sign-chars* "+-")

(defparameter *exponent-markers* "esfdlESFDL")

(defparameter *special-char-alist*
  '((#\newline newline-reader)
    (#\;       semicolon-reader)
    (#\?       qmark-reader))
  "An alist of (char function-name) lists.")

;;; Other required values

(defvar *eof* (list :eof)
  "An object to use as an eof-value when calling input procedures.
   Note that the symbol :eof can be used when reading characters.")

(defvar *standard-readtable* (copy-readtable nil)
  "A copy of the standard Common Lisp readtable.")


;;; Predicates for character classes

;;; N.B. These predicates can be called on non-characters used as
;;; eof values.

(defun opchar-p (char)
  (or (find char *operator-chars*)
      (find char *sign-chars*)))

(defun punctuation-char-p (char)
  (or (eql char #\;)			;N.B. ";" counts as punctuation
      (find char *punctuation-chars*)))

(defun digit-p (char)
  (find char *digit-chars*))

(defun exponent-marker-p (char)
  (find char *exponent-markers*))

(defun sign-p (char)
  (find char *sign-chars*))

(defun name-start-p (char)
  (and (characterp char)
       (if *digit-can-begin-name*
	   (alphanumericp char)
	   (alpha-char-p char))))

(defun name-char-p (char)
  (or (and (characterp char) (alphanumericp char))
      (find char *extra-name-chars*)))

;;; Other predicates.

;;; NAMEP checks a symbol to see if it is allowed as a name.
;;; Because the chars allowed in names are disjoint from those
;;; that begin operators, it should be that case that if the
;;; first char of a symbol is right for a name all the others
;;; will be also.

(defun namep (obj)
  (and (symbolp obj)
       (let ((name (symbol-name obj)))
	 (and (> (length name) 0)
	      (name-start-p (char name 0))
	      (every #'name-char-p
		     name)))))

(defun punctuation-symbol-p (sym)
  (and (symbolp sym)
       (= (length (symbol-name sym)) 1)
       (punctuation-char-p (char (symbol-name sym) 0))))


;;; Construct a TF readtable.

(defun make-tf-readtable () ; -> readtable
  (let ((*readtable* (copy-readtable nil)))
    (map nil #'define-punctuation-char *punctuation-chars*)
    (map nil #'define-operator-char *operator-chars*)
    (map nil #'define-illegal-char *illegal-chars*)
    (map nil #'define-digit-char *digit-chars*)
    (map nil #'define-sign-char *sign-chars*)
    (mapc #'(lambda (char-and-fn)
	      (set-macro-character (car char-and-fn)
				   (cadr char-and-fn)))
	  *special-char-alist*)
    *readtable*))

(defun define-punctuation-char (char)
  (set-macro-character char #'punctuation-reader)) 	;terminating

(defun define-operator-char (char)
  (set-macro-character char #'opchar-reader)) 		;terminating

(defun define-illegal-char (char)
  (set-macro-character char #'illegal-char-reader)) 	;terminating

(defun define-digit-char (char)
  (set-macro-character char #'digit-char-reader t)) 	;non-terminating

(defun define-sign-char (char)
  (set-macro-character char #'sign-char-reader nil)) 	;terminating

;;; Punctuation

(defun punctuation-reader (stream char)
  (declare (ignore stream))
  (char-to-symbol char))

(defun char-to-symbol (char)
  (values (intern (string char))))

;;; Operators

(defun opchar-reader (stream char)
  ;; N.B. *package* must be one that uses the TF package.
  ;; N.B. We can't unread the char, becuase it might be one
  ;; from special-dot-reader.
  (let ((chars (list char)))
    (loop (if (opchar-p (peek-char nil stream nil :eof))
	      (push (read-char stream t)
		    chars)
	    (return
	      (values (intern (coerce (nreverse chars) 'string))))))))

(defun illegal-char-reader (stream char)
  ;; Returns no values so that READ will continue.
  (declare (ignore stream))
  (syntax-error "Illegal character: \"~A\"." char)
  (values))


;;; Numbers

;;; In order to allow operators such as "..", we have to have our
;;; own number scanner.

(defun digit-char-reader (stream digit)
  (let ((chars (list digit))
	(valid t)			;valid as a number?
	(dot nil))			;contains decimal point?
    (labels ((peek ()
	       (peek-char nil stream nil :eof))
	     (save ()
	       (push (read-char stream t) chars))
	     (<digits> ()
	       (loop (if (digit-p (peek)) (save) (return))))
	     (get-number ()
	       ;; The entire number has now been read.
	       (setq chars (nreverse chars))
	       (let ((number-string (coerce chars 'string)))
		 (cond (valid
			(string->number number-string))
		       ((and *digit-can-begin-name* (not dot))
			(string->name number-string))
		       (t (syntax-error "Invalid number: \"~A\"."
					number-string))))))

      ;; The syntax is: {digit}+ [dot {digit}+] [exponent]
      ;; where an exponent is: exponent-marker [sign] {digit}+
      ;; Unlike Common Lisp, we require a digit before and after
      ;; any decimal point.
      (<digits>)
      (when (eql (peek) #\.)
	;; Note that "1.e2" will be read as a single token and
	;; then reported as an invalid number.
	(setq dot t)
	(save)
	(cond ((digit-p (peek))
	       (<digits>))
	      ((exponent-marker-p (peek))
	       (setq valid nil))
	      (t
	       (return-from digit-char-reader
		 (special-dot-reader stream (get-number) '#\.)))))
      (when (exponent-marker-p (peek))
	(save)
	(when (sign-p (peek)) (save))
	(if (digit-p (peek))
	    (<digits>)
	  (setq valid nil)))

      ;; We've now read a number, but it may not be properly delimited.
      ;; For instance, if the stream contained "12.3zqx", we're now at "z".
      ;; We want to treat this a an illegal number, not as two tokens
      ;; "12.3" followed by "zqx".  On the other hand, a number can be
      ;; followed by a operator, eg "12.3=>", which should be read as
      ;; two tokens.
      (loop (unless (name-char-p (peek))
	      (return))
	    (save)
	    (setq valid nil))

      ;; Return the number or report error if invalid.
      (get-number))))


;;; An operator beginning with dot that appears while reading a number.
;;; In effect, we have the the sequence <integer> <operator>.  Note
;;; that we've already read the "." and peeked at the character after
;;; it, and so we can't just unread the "." and return the integer.
;;; Instead, we read the operator now and save it for the next time
;;; the token generator is called.  Note that the operator might be
;;; nothing more than a single dot.

(defun special-dot-reader (stream number the-dot)
  (assert (opchar-p the-dot))
  (let ((op (opchar-reader stream the-dot)))
    (prog1 number
           (push-token op))))

;;; String->number

(defun string->number (string)
  (let ((*readtable* *standard-readtable*)
	(*read-default-float-format* 'single-float))
    (values (read-from-string string))))

;;; String->name
;;;
;;; Here's where we produce a name that begins with a series of digits.
;;; N.B. we call string-upcase to match the CL default.  If we ever add
;;; escape characters, we may need something more complex.

(defun string->name (string)
  (values (intern (string-upcase string))))


;;; Signs

;;; Sign chars are ordinary operator chars while reading expressions,
;;; but must also work when a singed number should read as a single
;;; token.  Note that the first token in an expression will be read
;;; before *reading-expression* can be bound to T.

(defparameter *reading-expression* nil
  "True while reading an expression.")

(defun sign-char-reader (stream sign)
  (assert (opchar-p sign))
  (if (or *reading-expression*
	  (not (digit-p (peek-char nil stream nil :eof))))
      (opchar-reader stream sign)
      (digit-char-reader stream sign)))



;;; Newline

(defvar *line-number* nil
  "The (1-origin) number of the line currently being compiled.")

(defun newline-reader (stream char)
  (declare (ignore stream char))
  (when *line-number* (incf *line-number*))
  (values))

;;; Semicolon

(defun semicolon-reader (stream char)
  (declare (ignore char))
  ;; Return #\; if only one ;.
  (unless (eql #\; (peek-char nil stream nil :eof))
    (return-from semicolon-reader (char-to-symbol #\;)))
  (read-char stream t)
  ;; Now have two ;s.  It's an error if there are not three.
  (unless (eql #\; (peek-char nil stream nil :eof))
    (syntax-error "Two semicolons are not allowed."))
  ;; Here we are with three ;s.
  ;; Throw away all chars to end of line, but leave the newline.
  (loop (case (read-char stream nil :eof)
	  (#\newline (unread-char #\newline stream)
		     (return))
	  (:eof (return))))
  (values))


;;; Actor readtable

;;; The actor readtable is used when reading an <actor_restriction>.
;;; An <actor_restriction> always begins with the character "?", which
;;; gives the read macro for "?" the chance to employ the right readtable.

;;; In the actor readtable, "( ... )" and "{ ... }" read as lists even
;;; though parens and brackets might read as punctuation in other contexts
;;; (such as with the ordinary TF readtable).

;;; Ordinarily, an actor readtable is meant to be used only inside
;;; actors, and it won't work for reading ordinary Lisp.  For one
;;; thing, it reads ( ... ) in a way that does not handle dot notation.
;;; An actor readtable of this sort must be bound to *actor-readtable*
;;; and used in conjunction with another readtable, such as the TF
;;; readtable, that makes #\? a macro char that calls qmark-reader.

;;; An actor readtable that can be bound to *readtable* on its own
;;; can be created by specifying :self-contained t.  This returns an
;;; "outer readtable" in which the only additional macro char is #\?.
;;; The outer macro function for #\? binds *actor-readtable* to an
;;; "inner readtable" which is an ordinary actor readtable.

(defun make-actor-readtable (base-readtable &key (self-contained nil))
  (if (not self-contained)
      (extend-readtable-for-actors (copy-readtable base-readtable))
    (let ((outer-readtable (copy-readtable base-readtable))
	  (inner-readtable (copy-readtable base-readtable)))
      (extend-readtable-for-actors inner-readtable)
      (set-macro-character
         #\?
	 #'(lambda (stream char)
	     (let ((*actor-readtable* inner-readtable))
	       (declare (special *actor-readtable*))
	       (qmark-reader stream char)))
	 nil				;terminating
	 outer-readtable)
      outer-readtable)))

(defun extend-readtable-for-actors (readtable)
  (let ((*readtable* readtable))
    (define-punctuation-char #\) )
    (define-punctuation-char #\} )
    (set-macro-character     #\(   (make-list-reader #\( '|)|))
    (set-macro-character     #\{   (make-list-reader #\{ '|}|))
    (set-macro-character     #\?   'question-reader)
    *readtable*))

;;; Question mark

;;; Note that we can use the question-reader defined by OBase.

;;; /\/: Note that it's possible to wrap something around the
;;; the result so that it could be distinguished from other types
;;; of tokens and so that we could control how it's printed.

(defun qmark-reader (stream char)
  (declare (special *actor-readtable*))
  (let ((*readtable* *actor-readtable*))
    (question-reader stream char)))

;;; List readers, for ( ... ) and { ... }.

(defparameter *list-close-symbols* '( |)| |}| ))

(defun make-list-reader (open-char close-symbol)
  #'(lambda (stream char)
      (assert (eql char open-char))
      (let ((elements '())
	    (e nil))
	(loop (setq e (read stream nil *eof*))
	      (cond ((eql e close-symbol)
		     (return (nreverse elements)))
		    ((member e *list-close-symbols*)
		     (syntax-error "Missing \"~A\" or extra \"~A\"."
				   close-symbol e))
		    ((eql e '|;|)
		     (syntax-error "Found \";\" in pattern ~
                                    -- suspect missing \"~A\"."
				   close-symbol)
		     ;; Return the semicolon to exit nested list-readers.
		     (return e))
		    ((punctuation-symbol-p e)
		     (syntax-error "Illegal character in pattern: \"~A\"." e))
		    ((eq e *eof*)
		     (syntax-error "Missing \"~A\"." close-symbol)
		     (return (nreverse elements)))
		    (t
		     (push e elements)))))))


;;; The actual scanner.

(defparameter *tf-readtable* (make-tf-readtable) 
  "The readtable used for reading TF.")

(defparameter *actor-readtable* (make-actor-readtable *tf-readtable*)
  "The readtable used when reading <actor_restriction>s.")

(defvar *end-token* (list :end-token)
  "An eql-unique object used to indicate the end of a stream of tokens.")

(defun tf-tokens (stream) ; -> generator
  #'(lambda ()
      (let ((*readtable* *tf-readtable*))
	(read stream nil *end-token*))))

;;; End
