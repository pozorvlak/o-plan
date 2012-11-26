;;;; File: parser-tests.lsp
;;; Contains: Tests for the TF scanner and parser
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: March 1992
;;; Updated: Mon Jun  7 00:36:10 1999 by Jeff Dalton
;;; Copyright: (c) 1992, AIAI, University of Edinburgh

;;; N.B. When running tests, the current package must be the same
;;; package that was used to load this file -- ie, the TF-COMPILER
;;; package.  This is because the tests contain symbols that will
;;; have been read in that package when this file was loaded.
;;; This does not apply, however, to the tests performed by
;;; compile-test-files.

;;; In general, later tests should be harder, more complex, or
;;; higher-level than earlier ones.

(in-package :oplan-tf-compiler)

(define-test-module :tf-parser :tf-compiler)
(in-test-module :tf-parser)

(define-test-wrapper :tf-parser (test)
  (let ((*package* (find-package :oplan-tf-compiler)))
    ;; Allow the thunk to return the error count as its 2nd value.
    (funcall (test-thunk test))))


;;; Some syntax used in tests.

(defun <token> ()
  (prog1 (token) (next-token)))

(defun <1-token> ()
  (prog1 (token) (next-token) (must-be *end-token*)))

(defun <all-tokens> ()
  (zero-or-more #'<token> :until *end-token*))

(defun string-token (s)
  (compile-tf-string #'<token> s))

(defun string-1-token (s)
  (compile-tf-string #'<1-token> s))

(defun string-tokens (s)
  (compile-tf-string #'<all-tokens> s))


;;; Some useful functions.

(defun nt-sequence (&rest nonterminals)
  #'(lambda ()
      (mapcar #'funcall nonterminals)))

(defun map-1st-value (thunk filter)
  (let ((all-values (multiple-value-list (funcall thunk))))
    (values-list
      (cons (funcall filter (first all-values))
	    (rest all-values)))))

(defun part-apply (fn arg1)
  #'(lambda (&rest args)
      (apply fn arg1 args)))


(defun string-pattern (s)
  ;; This is to read patterns for use in the result-forms of tests.
  ;; Consequently, it signals a Lisp error directly instead of using
  ;; the compiler's error mechanism.
  (let ((*package* (find-package :oplan-tf-compiler)))
    (compile-tf-string #'(lambda ()
			   (prog1 (<pattern>)
			     (unless (eql (token) *end-token*)
			       (error "Bad string pattern: ~S." s))))
		       s)))

;;; First test the test framework with our test wrapper.

(define-test-group tf-compiler-t-frame-tests
  ('a ==> 'a)
  ('a ==> :any)
  ('a ==> 'a :errors :any)
  ('a ==> :any :errors :any)

  ;; :True and :false
  ('a ==> :true)
  (nil ==> :false)

  ;; Now with some error counts
  ((values 'a 0) ==> 'a)
  ((values 'a 0) ==> 'a :errors :any)
  ((values 'a 0) ==> 'a :errors 0)
  ((values 'a 1) ==> 'a :errors :some)
  ((values 'a 1) ==> 'a :errors 1)
  ((values 'a 5) ==> 'a :errors 5))


;;; Test the parser framework

(define-test-group parser-framework-tests
  ((test-compile #'<token>      (list-tokens '()))   ==> *end-token*)
  ((test-compile #'<all-tokens> (list-tokens '()))   ==> '())
  ((test-compile #'<1-token>    (list-tokens '(a)))  ==> 'a)

  ((let ((*suppress-messages* t))
     (test-compile #'<1-token>  (list-tokens '())))  ==> *end-token* :errors 0)

  ((test-compile #'<all-tokens> (list-tokens '(a b c)))   ==> '(a b c))
  ((test-compile #'<all-tokens> (list-tokens '(a (b) c))) ==> '(a (b) c))

  ((string-token "") ==> *end-token*)
  ((string-tokens "") ==> '()))

(define-test-group (push-token-tests :with (gen))
  ((test-compile
      #'(lambda ()
	  (setq gen *token-generator*)
	  (push-token 3)
	  (push-token 2)
	  (let ((tokens (<all-tokens>)))
	    (list tokens (eq *token-generator* gen))))
      (list-tokens '(1)))
   ==> '((1 2 3) t))

  ;; Remember that push-token determines the next token that will
  ;; be returned by the token generator without changing the current
  ;; token at all, and that <token> calls next-token before returning.
  ((test-compile
      #'(lambda ()
	  (list (<token>)		;returns 1 but (token) is now 2
		(progn (push-token '3)
		       (<token>))	;returns 2 and (token) is now 3
		(<1-token>)))
      (list-tokens '(1 2)))
   ==> '(1 2 3))

  ((test-compile
      #'(lambda ()
	  (list (prog1 (<all-tokens>)	;(token) is now *end-token*
		       (push-token 'z))
		(progn (next-token)	;skip past old *end-token*
		       (<all-tokens>))))
      (list-tokens '(a b c)))
   ==> '((a b c) (z))))



(define-test-group recursive-compile-tests
  ((test-compile
      #'(lambda ()
	  (list (<token>)
		(test-compile #'<all-tokens> (list-tokens '(1 2)))
		(<1-token>)))
      (list-tokens '(a b)))
   ==> '(a (1 2) b))

  ((test-compile
      #'(lambda ()
	  (list (prog1 (token)		;a
		       (next-token)	;advance to b
		       (push-token 'c))	;insert c
		(test-compile
		   #'(lambda ()
		       (list (prog1 (token)		;1
			            (next-token)	;advance to 2
				    (push-token 'z))))	;insert z
		   (list-tokens '(1 2)))
		(<token>)		;the b
		(<token>)		;the c
		(<1-token>)))		;the d
      (list-tokens '(a b d)))
   ==> '(a (1) b c d))

  ((compile-tf
      #'(lambda ()
	  (list (<token>)
		(compile-tf #'<all-tokens> (list-tokens '(1 2)))
		(<1-token>)))
      (list-tokens '(a b)))
   ==> '(a (1 2) b)))


;;; Error recovery

(labels ((<a-b> ()
	   (list (must-be 'a) (must-be 'b)))
	 (<a-b+> ()
	   (one-or-more #'<a-b> :until *end-token*))
	 (<number> ()
	   (must-satisfy #'numberp "a number"))
	 (<number*> ()
	   (zero-or-more #'<number> :until *end-token*)))

  (define-test-group (error-recovery-tests
		      :with ((*syntax-error-reporter*
			      #'(lambda (&rest args) args))))
    ;; N.B. It may not be obvious that all of the indicated
    ;; results are correct, but they are.
    ((test-compile #'<a-b> (list-tokens '(a b)))     ==> '(a b))
    ((test-compile #'<a-b> (list-tokens '(a c)))     ==> '(a nil)   :errors 1)
    ((test-compile #'<a-b> (list-tokens '(c a b)))   ==> '(nil b)   :errors 1)
    ((test-compile #'<a-b> (list-tokens '(c d a b))) ==> '(nil b)   :errors 1)
    ((test-compile #'<a-b+> (list-tokens '(a b a b)))
     ==> '((a b) (a b)))
    ((test-compile #'<a-b+> (list-tokens '(a b c d a b)))
     ==> '((a b) (nil b))
     :errors 1)
    ((test-compile #'<number*> (list-tokens '()))
     ==> '())
    ((test-compile #'<number*> (list-tokens '(a)))
     ;; The first call to <number> reports an error and returns nil
     ;; with "a" still as the current token.  The second call to <number>
     ;; consumes all remaining tokens and returns nil.  But zero-or-more
     ;; eliminates the nils because they were returned when *recovering*
     ;; was still true.  So the final result is nil.
     ==> '()
     :errors 1)
    ((test-compile #'<number*> (list-tokens '(1 2 3)))
     ==> '(1 2 3))
    ((test-compile #'<number*> (list-tokens '(1 a 2 3)))
     ==> '(1 2 3)
     :errors 1)
    ((test-compile #'<number*> (list-tokens '(1 a 2 b c 3)))
     ==> '(1 2 3)
     :errors 2)))


;;; Name tokens.

(define-test-group name-token-tests
  ((string-token "a") ==> 'a)
  ((string-tokens "a") ==> '(a))
  ((string-token "a b") ==> 'a)
  ((string-tokens "a b") ==> '(a b))
  ((string-1-token "abc") ==> 'abc)
  ((string-1-token "abc_def") ==> 'abc_def)
  ((string-1-token "a1_b2_c3%") ==> 'a1_b2_c3%)
  )

;;; Number tokens.

(define-test-group number-token-tests
  ((string-1-token "1") ==> 1)
  ((string-1-token "12") ==> 12)
  ((string-1-token "1.2") ==> 1.2)
  ((string-1-token "1e2") ==> 1e2)
  ((string-1-token "1e+2") ==> 1e+2)
  ((string-1-token "1e-2") ==> 1e-2)
  ((string-1-token "123.456") ==> 123.456)
  ((string-1-token "0.001") ==> 0.001)
  ((string-1-token "12.34e05") ==> 12.34e5))

(define-test-group signed-number-token-tests
  ((string-1-token "-1") ==> -1)
  ((string-1-token "+12") ==> +12)
  ((string-1-token "-1.2e3") ==> -1.2e3))

(define-test-group (number-token-validity-tests
		    :with ((*suppress-messages* t)))
  ((string-1-token "1")   :errors 0)
  ((string-1-token "1.e2") :errors 1)
  ((let ((*digit-can-begin-name* nil))
     (string-1-token "1e"))
   :errors 1)
  ((let ((*digit-can-begin-name* t))
     (string-1-token "1e"))
   :errors 0))

;;; Operator tokens

(define-test-group operator-token-tests
  ((string-tokens "+") ==> '(+))
  ((string-tokens ">=") ==> '(>=))
  ((string-tokens "...") ==> '(|...|))
  ((string-tokens "--->") ==> '(--->))
  ((string-tokens "a + b") ==> '(a + b))
  ((string-tokens "a+b") ==> '(a + b))
  ((string-tokens "1 + 2") ==> '(1 + 2))
  ((string-tokens "1+2") ==> '(1 +2))		; + acts as a sign
  ((string-tokens "a++b") ==> '(a ++ b))
  ((string-tokens "a--->b") ==> '(a ---> b))
  ((string-tokens "a..b") ==> '(a |..| b))
  ((string-tokens "a.+.b") ==> '(a |.+.| b))
  )

;;; Dot-operators and numbers

(define-test-group dot-operator-token-tests
  ((string-tokens "..") ==> '(|..|))
  ((string-tokens "a..b") ==> '(a |..| b))
  ((string-tokens "1..2") ==> '(1 |..| 2))
  ((string-tokens "1.=.2") ==> '(1 |.=.| 2))
  ((string-tokens "-2..-1") ==> '(-2 |..-| 1))	   ; - as opchar, not sign
  ((string-tokens "1.2..3.4") ==> '(1.2 |..| 3.4))
  ((string-tokens "1e2..3e4") ==> '(1e2 |..| 3e4))
  ((string-tokens "2.71828..3.14") ==> '(2.71828 |..| 3.14))
  ((string-tokens "27.1828e-1..314e-2") ==> '(27.1828e-1 |..| 314e-2))
  ((string-tokens "10 minutes..20 hours") ==> '(10 minutes |..| 20 hours))
  ((let ((*digit-can-begin-name* t))
     (string-tokens "23rd..4th"))
   ==> '(23rd |..| 4th))
  ((let ((*digit-can-begin-name* nil)
	 (*suppress-messages* t))
     (string-tokens "23rd..4th"))
   :errors :some))

;;; Punctuation

(define-test-group punctuation-tests
  ((string-tokens ",,") ==> '(|,| |,|))
  ((string-tokens ",a,") ==> '(|,| a |,|))
  ((string-tokens " , a , ") ==> '(|,| a |,|))
  ((string-tokens "a,1,**,") ==> '(a |,| 1 |,| ** |,|))
  ((string-tokens "1:2") ==> '(1 |:| 2))
  #-kcl ;; KCL will treat the ":" as a package marker.
  ((string-tokens "tf:schema") ==> '(tf |:| schema))
  ((string-tokens "tf : schema") ==> '(tf |:| schema)))

(define-test-group semicolon-tests
  ((string-tokens ";") ==> '(|;|))
  ((string-tokens "abc;") ==> '(abc |;|))
  ((string-tokens "123;") ==> '(123 |;|))
  ((string-tokens "++;") ==> '(++ |;|))
  ((string-tokens ";a;2;*;") ==> '(|;| a |;| 2 |;| * |;|))
  ((string-tokens ";;; comment
                   abc") ==> '(abc))
  ((string-tokens ";;; comment") ==> '())
  ((string-tokens "abc;;; comment") ==> '(abc))
  ((string-tokens "123;;; comment") ==> '(123))
  ((string-tokens "==;;; comment") ==> '(==))
  ((string-tokens ",;;; comment") ==> '(|,|)))

;;; Strings

(define-test-group string-token-tests
  ((string-tokens "\"a string\"") ==> '("a string"))
  ; ((string-tokens "\"a \\\"string\\\"\"") ==> '("a \"string\""))
  ((string-tokens "abc \"a string\" 123") ==> '(abc "a string" 123))
  ((string-tokens "abc\"a string\"123") ==> '(abc "a string" 123))
  ((string-tokens "123\"a string\"abc") ==> '(123 "a string" abc))
  ((string-tokens "<<\"a string\">>") ==> '(<< "a string" >>))
  )


;;; <name>, <number>, <text_string>

(define-test-group (name-validity-tests
		    :with ((*suppress-messages* t)))
  ((compile-tf-string #'<name> "abc")     ==> 'abc     :errors 0)
  ((compile-tf-string #'<name> "%abc")    ==> 'nil     :errors 1)
  ((compile-tf-string #'<name> "a2%_b3%") ==> 'a2%_b3% :errors 0)
  ((compile-tf-string #'<name> "abc123")  ==> 'abc123  :errors 0)
  ((let ((*digit-can-begin-name* nil))
     (compile-tf-string #'<name> "123abc"))
   ==> 'nil :errors 1)
  ((let ((*digit-can-begin-name* t))
     (compile-tf-string #'<name> "123abc"))
   ==> '\123abc :errors 0)
  )

(define-test-group number-tests
  ((compile-tf-string #'<number> "10") ==> 10)
  ((compile-tf-string #'<number> "-10") ==> -10)
  ((compile-tf-string #'<number> "3.14") ==> 3.14)
  ((compile-tf-string #'<number> "0.271828e+1") ==> 0.271828e+1)
  ((compile-tf-string #'<number> "inf") ==> *infinity*)
  ((compile-tf-string #'<number> "infinity") ==> *infinity*)
  )

(define-test-group (text-string-tests
		    :with ((*suppress-messages* t)))
  ((compile-tf-string #'<text-string>
		      "\"a string\"")
   ==> "a string" :errors 0)
  ((compile-tf-string #'<text-string>
		      "\"a string = a string.\"")
   ==> "a string = a string." :errors 0)
  ((compile-tf-string #'<text-string>
		      "not a string in sight")
   ==> nil :errors 1)
  ;; If there's no close quote, we get an end of stream error
  ;; which we can't catch w/o the condition system.
  #+undef
  ((compile-tf-string #'<text-string>
		      "\"no close quote")
   ==> nil :errors 1))


;;; Patterns, values, varibles, and actors

;;; Note that a <pattern> must be {<name> <pattern_component>...}

(define-test-group ground-pattern-tests
  ((compile-tf-string #'<pattern> "{colour apples}")
   ==> '(colour apples))
  ((compile-tf-string #'<pattern> "{colour {camera_1 filter_3}}")
   ==> '(colour (camera_1 filter_3)))
  ((compile-tf-string #'<pattern> "{on a table}")
   ==> '(on a table))
  ((compile-tf-string #'<pattern> "{finish roofing and flashing}")
   ==> '(finish roofing and flashing))
  ((compile-tf-string #'<pattern> "{numbers 1 {2 3 {4} 5}}}")
   ==> '(numbers 1 (2 3 (4) 5))))

(define-test-group actor-token-tests
  ((actor-restriction-p (string-token "??"))
   ==> :true)
  ((actor-restriction-p (string-token "?name"))
   ==> :true)
  ((actor-restriction-p (string-token "?{type type_name}"))
   ==> :true)
  ((actor-restriction-p (string-token "?{or a b c}"))
   ==> :true)
  ((actor-restriction-p (string-token "?{has length 3}"))
   ==> :true)
  ((actor-restriction-p (string-token "?{property colour yellow}"))
   ==> :true)
  ((actor-restriction-p (string-token "?{hassupitem 2 {colour ??} yellow}"))
   ==> :true)
  ((string-1-token "??"))
  ((string-1-token "?name"))
  ((string-1-token "?{type type_name}"))
  ((string-1-token "?{or a b c}"))
  ((string-1-token "?{has length 3}"))
  ((string-1-token "?{property colour yellow}"))
  ((string-1-token "?{hassupitem 2 {colour ??} yellow}")))

(define-test-group (pattern-validity-tests
		    :with ((*suppress-messages* t)))
  ((compile-tf-string #'<pattern> "{on a b}")
   ==> '(on a b)
   :errors 0)
  ((compile-tf-string #'<pattern> "{on ?? ??}")
   ==> `(on ,actorsym ,actorsym)
   :errors 0)
  ((compile-tf-string #'<pattern> "{on ?a ?b}")
   :errors 0)
  ((compile-tf-string #'<pattern> "{?? ?? ??}")
   :errors 1)
  ((compile-tf-string #'<pattern> "{on ?? ?{not table}}")
   :errors 0)
  ((compile-tf-string #'<pattern> "{on ?? ?(not table)}")
   :errors 0)
  ((compile-tf-string #'<pattern> "{something missing")	 ;no "}"
   :errors 1)
  ((compile-tf-string #'<pattern> "{something wrong)")   ;")" instead of "}"
   :errors 1)
  ((compile-tf-string #'<pattern> "--->")
   :errors 1)
  ((compile-tf-string #'<pattern> "10")
   :errors 1))


;;; <pattern-assignment> ::= <pattern> [= <value>]

;;; <value>s

(define-test-group value-tests
  ;; Check that various <pattern_component>s can be <value>s.
  ((compile-tf-string #'<value> "red")
   ==> 'red)
  ((compile-tf-string #'<value> "3.14")
   ==> '3.14)
  ((compile-tf-string #'<value> "{on a b}")
   ==> '(on a b))
  ((actor-restriction-p (compile-tf-string #'<value> "??}"))
   ==> :true)
  ((actor-restriction-p (compile-tf-string #'<value> "?{not table}"))
   ==> :true))

;;; <pattern-assignment>s

(define-test-group pattern-assignment-tests
  (1 ==> 1))


;;; <restriction-assignment> ::= <variable-name> [= <variable-restriction>]

;;; <variable-name>s

(define-test-group variable-name-tests
  ((compile-tf-string #'<variable-name> "?x") ==> 'x)
  ((compile-tf-string #'<variable-name> "?block") ==> 'block))

;;; <variable-restriction>s

(define-test-group variable-restriction-tests
  ((compile-tf-string #'<variable-restriction> "undef")
   ==> *undef*)
  ((compile-tf-string #'<variable-restriction> "??")
   ==> *undef*)
  ((actor-restriction-p
    (compile-tf-string #'<variable-restriction> "?name"))
   ==> :true)
  ((actor-restriction-p
    (compile-tf-string #'<variable-restriction> "?{or a b c}"))
   ==> :true))

;;; <restriction-assignment>s
    
(define-test-group restriction-assignment-tests
  (1 ==> 1))


;;; Numeric expressions

(defun string-expr (s)
  (compile-tf-string #'<expr> s))

(defun string-value (s)
  (eval (string-expr s)))

(define-test-group numeric-expression-tests-1
  ;; N.B. + and - act as signs, not unary operators, when part of the
  ;; first token of an expression.  This is strange, but bearable.
  ((string-expr "1") ==> 1)
  ((string-expr "+1") ==> 1)		; + as sign
  ((string-expr "-1") ==> -1)		; - as sign
  ((string-expr "1+2") ==> '(+ 1 2))	; + as operator
  ((string-expr "1-2") ==> '(- 1 2))	; - as operator
  ((string-expr "1*2") ==> '(* 1 2))
  ((string-expr "1/2") ==> '(/ 1 2))
  ((string-expr "-1+2") ==> '(+ -1 2))	; - as sign
  ((string-expr "-1/2") ==> '(/ -1 2))	; - as sign
  ((string-expr "(1)") ==> 1)
  ((string-expr "((1))") ==> 1)
  ((string-expr "(1/2)") ==> '(/ 1 2))
  )

(define-test-group numeric-expression-tests-2
  ;; N.B. Division is CL division and may yield ratios.

  ;; Some simple expressions and parentheses.
  ((string-value "1") ==> 1)
  ((string-value "-1/2") ==> -1/2)
  ((string-value "-(1/2)") ==> -1/2)
  ((string-value "-((1)/(2))") ==> -1/2)
  ((string-value "-((-1)/(-2))") ==> -1/2)

  ((string-value "1+2+3") ==> 6)
  ((string-value "1*2*3") ==> 6)
  )

(define-test-group numeric-assoc-tests
  
  ;; Check that - is left associative
  ((string-value "1-2-3") ==> -4)
  ((string-value "(1-2)-3") ==> -4)
  ((string-value "1-(2-3)") ==> 2)

  ;; Check that / is left associative
  ;; Result differs by square of the last number.
  ((string-value "10/5/2") ==> 1)
  ((string-value "(10/5)/2") ==> 1)
  ((string-value "10/(5/2)") ==> 4)
  )

(define-test-group numeric-precedence-tests

  ;; Check alternating operators at the same precedence level.
  ((string-value "1+2-3+4") ==> (+ 1 2 -3 4))
  ((string-value "-1+2-3+4-5+6-7+8") ==> (+ -1 2 -3 4 -5 6 -7 8))

  ((string-value "10/5*2") ==> 4)
  ((string-value "10/(5*2)") ==> 1)

  ;; Check interaction between different precedence levels.
  ((string-value "1*2+3*4") ==> (+ 2 12))
  ((string-value "1+2*3+4") ==> (+ 1 6 4))
  ((string-value "(1+2)*(3+4)") ==> (* 3 7))
  ((string-value "-1 * -2 + -3 * -4") ==> (+ 2 12))
  ((string-value "-1 + -2 * -3 + -4") ==> (+ -1 6 -4))
  ((string-value "(-1 + -2) * (-3 + -4)") ==> (* -3 -7))
  )

;;; Symbolic expressions

(define-test-group symbolic-expression-tests
  ((string-expr "?a") ==> (variable-name->actor 'a))
  ((string-expr "?a+?b")
   ==> `(+ ,(variable-name->actor 'a)
	   ,(variable-name->actor 'b)))

  ((string-expr "inf") ==> *infinity*)
  ((string-expr "infinity") ==> *infinity*)
  ((string-expr "infinity+1") ==> `(+ ,*infinity* 1))
  )


;;; Sets

(define-test-group set-tests
  ((compile-tf-string #'<name-set> "(a b c)") ==> '(a b c))
  ((compile-tf-string #'<general-set> "({a} {b} {c})") ==> '((a) (b) (c))))

;;; Time specifications

(define-test-group time-spec-tests
  ((compile-tf-string #'<time-spec> "12")         ==> 12)
  ((compile-tf-string #'<time-spec> "12 seconds") ==> 12)
  ((compile-tf-string #'<time-spec> "12 minutes") ==> (* 12 60))
  ((compile-tf-string #'<time-spec> "12 hours")   ==> (* 12 60 60))
  ((compile-tf-string #'<time-spec> "12 days")    ==> (* 12 24 60 60))

  ((time-spec 2 3 5 7) ==> (+ (* 2 24 60 60) (* 3 60 60) (* 5 60) 7))
  
  ((compile-tf-string #'<time-spec> "2:30")
   ==> (+ (* 2 60 60) (* 30 60)))
  ((compile-tf-string #'<time-spec> "2:30:15")
   ==> (+ (* 2 60 60) (* 30 60) 15))
  ((compile-tf-string #'<time-spec> "6~2:30:15")
   ==> (+ (* 6 24 60 60) (* 2 60 60) (* 30 60) 15))
  ((compile-tf-string #'<time-spec> "6~2:30")
   ==> (+ (* 6 24 60 60) (* 2 60 60) (* 30 60)))
  ((compile-tf-string #'<time-spec> "2~3:5:7")
   ==> (time-spec 2 3 5 7))

  ((compile-tf-string #'<time-spec> "inf") ==> *infinity*)
  ((compile-tf-string #'<time-spec> "inf hours") ==> *infinity*))


;;; Time bounds specifications

(define-test-group time-bounds-spec-tests-1
  ((compile-tf-string #'<time-bounds-spec> "2")
   ==> (time-window 2 2))
  ((compile-tf-string #'<time-bounds-spec> "2 seconds")
   ==> (time-window 2 2))
  ((compile-tf-string #'<time-bounds-spec> "2 days")
   ==> (time-window (time-spec 2 0 0 0)
		    (time-spec 2 0 0 0)))
  ((compile-tf-string #'<time-bounds-spec> "15 minutes")
   ==> (time-window (time-spec 0 0 15 0)
		    (time-spec 0 0 15 0)))
  ((compile-tf-string #'<time-bounds-spec> "2~3:30")
   ==> (time-window (time-spec 2 3 30 0)
		    (time-spec 2 3 30 0)))
  ((compile-tf-string #'<time-bounds-spec> "5~9:20:10")
   ==> (time-window (time-spec 5 9 20 10)
		    (time-spec 5 9 20 10)))
  ((compile-tf-string #'<time-bounds-spec> "(2)")
   ==> (time-window 2 2))
  ((compile-tf-string #'<time-bounds-spec> "(2 seconds)")
   ==> (time-window 2 2))
  ((compile-tf-string #'<time-bounds-spec> "(2 days)")
   ==> (time-window (time-spec 2 0 0 0)
		    (time-spec 2 0 0 0)))
  ((compile-tf-string #'<time-bounds-spec> "((2))")
   ==> (time-window 2 2))
  ((compile-tf-string #'<time-bounds-spec> "(5~9:20:10)")
   ==> (time-window (time-spec 5 9 20 10)
		    (time-spec 5 9 20 10))))


(define-test-group time-bounds-spec-tests-2  
  ((compile-tf-string #'<time-bounds-spec> "2..3")
   ==> (time-window 2 3))
  ((compile-tf-string #'<time-bounds-spec> "2..inf")
   ==> (time-window 2 *infinity*))
  ((compile-tf-string #'<time-bounds-spec> "et=2..lt=3")
   ==> (time-window 2 3))
  ((compile-tf-string #'<time-bounds-spec> "2..lt=3")
   ==> (time-window 2 3))
  ((compile-tf-string #'<time-bounds-spec> "et=2..3")
   ==> (time-window 2 3))
  ((compile-tf-string #'<time-bounds-spec> "2 seconds..3 seconds")
   ==> (time-window 2 3))
  ((compile-tf-string #'<time-bounds-spec> "2 days..3 days")
   ==> (time-window (time-spec 2 0 0 0)
		    (time-spec 3 0 0 0)))
  ((compile-tf-string #'<time-bounds-spec> "et=2 seconds..lt=3 seconds")
   ==> (time-window 2 3))
  ((compile-tf-string #'<time-bounds-spec> "et=30 hours..lt=50 hours")
   ==> (time-window (time-spec 0 30 0 0)
		    (time-spec 0 50 0 0)))
  ((compile-tf-string #'<time-bounds-spec> "2~3:30..5~9:20:10")
   ==> (time-window (time-spec 2 3 30 0)
		    (time-spec 5 9 20 10)))
  ((compile-tf-string #'<time-bounds-spec> "et=2~3:30..lt=5~9:20:10")
   ==> (time-window (time-spec 2 3 30 0)
		    (time-spec 5 9 20 10))))

(define-test-group time-bounds-spec-tests-2-with-parens
  ((compile-tf-string #'<time-bounds-spec> "(2..3)")
   ==> (time-window 2 3))
  ((compile-tf-string #'<time-bounds-spec> "(et=2..lt=3)")
   ==> (time-window 2 3))
  ((compile-tf-string #'<time-bounds-spec> "(2 seconds..3 seconds)")
   ==> (time-window 2 3))
  ((compile-tf-string #'<time-bounds-spec> "(et=30 hours..lt=50 hours)")
   ==> (time-window (time-spec 0 30 0 0)
		    (time-spec 0 50 0 0)))
  ((compile-tf-string #'<time-bounds-spec> "(2~3:30..5~9:20:10)")
   ==> (time-window (time-spec 2 3 30 0)
		    (time-spec 5 9 20 10)))
  ((compile-tf-string #'<time-bounds-spec> "(et=2~3:30..lt=5~9:20:10)")
   ==> (time-window (time-spec 2 3 30 0)
		    (time-spec 5 9 20 10))))

  
(define-test-group time-bounds-and-spec-tests  
  ((compile-tf-string #'<time-bounds-spec> "2 and 3")
   ==> (time-window 2 3))
  ((compile-tf-string #'<time-bounds-spec> "(2 and 3)")
   ==> (time-window 2 3))
  ((compile-tf-string #'<time-bounds-spec> "et=2 and lt=3")
   ==> (time-window 2 3))
  ((compile-tf-string #'<time-bounds-spec> "2 seconds and 3 seconds")
   ==> (time-window 2 3))
  ((compile-tf-string #'<time-bounds-spec> "et=30 hours and lt=50 hours")
   ==> (time-window (time-spec 0 30 0 0)
		    (time-spec 0 50 0 0)))
  ((compile-tf-string #'<time-bounds-spec> "2~3:30 and 5~9:20:10")
   ==> (time-window (time-spec 2 3 30 0)
		    (time-spec 5 9 20 10)))
  ((compile-tf-string #'<time-bounds-spec> "et=2~3:30 and lt=5~9:20:10")
   ==> (time-window (time-spec 2 3 30 0)
		    (time-spec 5 9 20 10)))
  ((compile-tf-string #'<time-bounds-spec> "(et=2~3:30 and lt=5~9:20:10)")
   ==> (time-window (time-spec 2 3 30 0)
		    (time-spec 5 9 20 10))))


(define-test-group time-bounds-between-spec-tests-1
  ((compile-tf-string #'<time-bounds-spec> "between 2..3")
   ==> (time-window 2 3))
  ((compile-tf-string #'<time-bounds-spec> "between 2 and 3")
   ==> (time-window 2 3))
  ((compile-tf-string #'<time-bounds-spec> "between et = 2 .. lt = 3")
   ==> (time-window 2 3))
  ((compile-tf-string #'<time-bounds-spec> "between et = 2 and lt = 3")
   ==> (time-window 2 3))
  ((compile-tf-string #'<time-bounds-spec> "(between et = 2 and lt = 3)")
   ==> (time-window 2 3))
  ((compile-tf-string #'<time-bounds-spec> "between 2 seconds .. 3 seconds")
   ==> (time-window 2 3))
  ((compile-tf-string #'<time-bounds-spec> "between 2 seconds and 3 seconds")
   ==> (time-window 2 3))

  ((compile-tf-string #'<time-bounds-spec>
		      "between et=30 hours .. lt=50 hours")
   ==> (time-window (time-spec 0 30 0 0)
		    (time-spec 0 50 0 0)))
  ((compile-tf-string #'<time-bounds-spec>
		      "between et=30 hours and lt=50 hours")
   ==> (time-window (time-spec 0 30 0 0)
		    (time-spec 0 50 0 0)))
  ((compile-tf-string #'<time-bounds-spec>
		      "between 2~3:30 .. 5~9:20:10")
   ==> (time-window (time-spec 2 3 30 0)
		    (time-spec 5 9 20 10)))
  ((compile-tf-string #'<time-bounds-spec>
		      "between et=2~3:30 and lt=5~9:20:10")
   ==> (time-window (time-spec 2 3 30 0)
		    (time-spec 5 9 20 10)))
  ((compile-tf-string #'<time-bounds-spec>
		      "(between et=2~3:30 .. lt=5~9:20:10)")
   ==> (time-window (time-spec 2 3 30 0)
		    (time-spec 5 9 20 10))))

(define-test-group time-bounds-between-spec-tests-2
  ;; This is like the "and" tests, but with the "and" omitted and
  ;; "between" at the front.
  ((compile-tf-string #'<time-bounds-spec> "between 2 3")
   ==> (time-window 2 3))
  ((compile-tf-string #'<time-bounds-spec> "(between 2 3)")
   ==> (time-window 2 3))
  ((compile-tf-string #'<time-bounds-spec> "between et=2 lt=3")
   ==> (time-window 2 3))
  ((compile-tf-string #'<time-bounds-spec> "between 2 seconds 3 seconds")
   ==> (time-window 2 3))
  ((compile-tf-string #'<time-bounds-spec> "between et=30 hours lt=50 hours")
   ==> (time-window (time-spec 0 30 0 0)
		    (time-spec 0 50 0 0)))
  ((compile-tf-string #'<time-bounds-spec> "between 2~3:30 5~9:20:10")
   ==> (time-window (time-spec 2 3 30 0)
		    (time-spec 5 9 20 10)))
  ((compile-tf-string #'<time-bounds-spec> "between et=2~3:30 lt=5~9:20:10")
   ==> (time-window (time-spec 2 3 30 0)
		    (time-spec 5 9 20 10)))
  ((compile-tf-string #'<time-bounds-spec> "(between et=2~3:30 lt=5~9:20:10)")
   ==> (time-window (time-spec 2 3 30 0)
		    (time-spec 5 9 20 10))))


(define-test-group time-bounds-after-spec-tests
  ((compile-tf-string #'<time-bounds-spec> "after 10")
   ==> (time-window 10 *infinity*))
  ((compile-tf-string #'<time-bounds-spec> "after 15 minutes")
   ==> (time-window (time-spec 0 0 15 0) *infinity*))
  ((compile-tf-string #'<time-bounds-spec> "(after 5 hours)")
   ==> (time-window (time-spec 0 5 0 0) *infinity*))
  ((compile-tf-string #'<time-bounds-spec> "after 3~9:30")
   ==> (time-window (time-spec 3 9 30 0) *infinity*))
  ((compile-tf-string #'<time-bounds-spec> "> 10")
   ==> (time-window 10 *infinity*))
  ((compile-tf-string #'<time-bounds-spec> "> 15 minutes")
   ==> (time-window (time-spec 0 0 15 0) *infinity*))
  ((compile-tf-string #'<time-bounds-spec> "(> 5 hours)")
   ==> (time-window (time-spec 0 5 0 0) *infinity*))
  ((compile-tf-string #'<time-bounds-spec> "> 3~9:30")
   ==> (time-window (time-spec 3 9 30 0) *infinity*)))

(define-test-group time-bounds-before-spec-tests
  ((compile-tf-string #'<time-bounds-spec> "before 10")
   ==> (time-window 0 10))
  ((compile-tf-string #'<time-bounds-spec> "before 15 minutes")
   ==> (time-window 0 (time-spec 0 0 15 0)))
  ((compile-tf-string #'<time-bounds-spec> "(before 5 hours)")
   ==> (time-window 0 (time-spec 0 5 0 0)))
  ((compile-tf-string #'<time-bounds-spec> "before 3~9:30")
   ==> (time-window 0 (time-spec 3 9 30 0)))
  ((compile-tf-string #'<time-bounds-spec> "< 10")
   ==> (time-window 0 10))
  ((compile-tf-string #'<time-bounds-spec> "< 15 minutes")
   ==> (time-window 0 (time-spec 0 0 15 0)))
  ((compile-tf-string #'<time-bounds-spec> "(< 5 hours)")
   ==> (time-window 0 (time-spec 0 5 0 0)))
  ((compile-tf-string #'<time-bounds-spec> "< 3~9:30")
   ==> (time-window 0 (time-spec 3 9 30 0))))

(define-test-group time-bounds-occurs_at-spec-tests
  ((compile-tf-string #'<time-bounds-spec> "occurs_at 10")
   ==> (time-window 10 10))
  ((compile-tf-string #'<time-bounds-spec> "occurs_at 15 minutes")
   ==> (time-window (time-spec 0 0 15 0)
		    (time-spec 0 0 15 0)))
  ((compile-tf-string #'<time-bounds-spec> "(occurs_at 5 hours)")
   ==> (time-window (time-spec 0 5 0 0)
		    (time-spec 0 5 0 0)))
  ((compile-tf-string #'<time-bounds-spec> "occurs_at 3~9:30")
   ==> (time-window (time-spec 3 9 30 0)
		    (time-spec 3 9 30 0))))

(define-test-group time-bounds-prefernece-spec-tests
  ((compile-tf-string #'<time-bounds-spec> "2 with ideal = 3")
   ==> (make-time-window :min 2 :max 2 :ideal 3))
  ((compile-tf-string #'<time-bounds-spec> "2..5 with ideal = 3")
   ==> (make-time-window :min 2 :max 5 :ideal 3)))

;;; Resource specifications

(define-test-group resource-usage-spec-tests
  ;; /\/: Fill in result values.
  ((compile-tf-string #'<resource-usage-spec>
      "consumes {resource money} = 0..1000 dollars overall")
   ==> :any)
  ((compile-tf-string #'<resource-usage-spec>
      "{resource money} = 145 dollars")
   ==> :any)
  ((compile-tf-string #'<resource-usage-spec>
      "{resource bricks site_567} = 10000") 	;no units specified
   ==> :any)
  ((compile-tf-string #'<resource-usage-spec>
      "{resource fuel port_A tank_C} = 15000 gallons")
   ==> :any))


;;; Numeric bounds

(define-test-group min-max-spec-tests
  ((compile-tf-string #'<min-max-spec> "2..3")
   ==> (range 2 3))
  ((compile-tf-string #'<min-max-spec> "min 2..max 3")
   ==> (range 2 3))
  ((compile-tf-string #'<min-max-spec> "min 2")
   ==> (range 2 *infinity*))
  ((compile-tf-string #'<min-max-spec> ">=2")
   ==> (range 2 *infinity*))
  ((compile-tf-string #'<min-max-spec> "max 3")
   ==> (range 0 3))
  ((compile-tf-string #'<min-max-spec> "<=3")
   ==> (range 0 3))
  ((compile-tf-string #'<min-max-spec> "2")
   ==> (range 2 2))
  ((compile-tf-string #'<min-max-spec> "no")
   ==> (range 0 0))
  ((compile-tf-string #'<min-max-spec> "none")
   ==> (range 0 0))
  ((compile-tf-string #'<min-max-spec> "some")
   ==> (range 0 *infinity*))
  ((compile-tf-string #'<min-max-spec> "(2..3)")
   ==> (range 2 3))
  ((compile-tf-string #'<min-max-spec> "(-3 .. -2)")
   ==> (range -3 -2)))

	
;;; Node ends

(define-test-group (node-number-tests
		    :with ((*suppress-messages* t)))
  ((compile-tf-string #'<node-number> "1") ==> 1 :errors 0)
  ((compile-tf-string #'<node-number> "a")       :errors 1)
  ((compile-tf-string #'<node-number> "1.2")     :errors 1))

(define-test-group node-end-tests
  ((compile-tf-string (part-apply #'<node-end> 'begin_of)
		      "begin_of 2")
   ==> '(2 :begin))
  ((compile-tf-string (part-apply #'<node-end> 'end_of)
		      "end_of 2")
   ==> '(2 :end))
  ((compile-tf-string (part-apply #'<node-end> 'end_of)
		      "begin_of 2")
   ==> '(2 :begin))
  ((compile-tf-string (part-apply #'<node-end> 'begin_of)
		      "end_of 2")
   ==> '(2 :end))
  ((compile-tf-string (part-apply #'<node-end> 'begin_of)
		      "2")
   ==> '(2 :begin))
  ((compile-tf-string (part-apply #'<node-end> 'end_of)
		      "2")
   ==> '(2 :end))
  ((compile-tf-string (part-apply #'<node-end> 'link_from_node_end)
		      "2")
   ==> '(2 :end))
  ((compile-tf-string (part-apply #'<node-end> 'link_to_node_end)
		      "2")
   ==> '(2 :begin))
  ((compile-tf-string (part-apply #'<node-end> 'condition_node_end)
		      "2")
   ==> '(2 :begin))
  ((compile-tf-string (part-apply #'<node-end> 'effect_node_end)
		      "2")
   ==> '(2 :end)))


;;; tf_info forms

(define-test-group (tf_info-form-tests
		    :with ((*progress-reports* nil)))
  ((compile-tf-string #'<tf-form> "
      tf_info title   \"tf_info test\";
              author  \"the test writer\";
              date    \"8 May 1992\";
              history \"unmodified\";
              description
                \"A test of the tf_info form\";
      end_tf_info;")
   ==> '(tf_info (title "tf_info test")
	         (author "the test writer")
	         (date "8 May 1992")
	         (history "unmodified")
	         (description "A test of the tf_info form"))))


;;; defaults forms

(eval-when (eval compile load)
  ;; Instead of requiring that "parser" be loaded first.  /\/
  (proclaim '(special *user-defaults* *default-defaults*)))

(define-test-group (defaults-form-tests
		    :with ((*progress-reports* nil)
			   (*user-defaults* nil)))
  ;; We rebind *user-defaults* so that any changes are undone after
  ;; the tests and so that we are isolated from any changes made
  ;; before the tests were run.

  ;; Test get-default-value
  ((get-default-value 'link_from_node_end)
   ==> 'end_of)

  ;; Test parsing of all options and expect something equivalent
  ;; to the standard defaults.
  ((compile-tf-string #'<tf-form>
    "
    defaults value = true,
             variable_restriction = undef,
             condition_node_end = begin_of,
             condition_contributor_node_end = end_of,
             achieve_after_point = end_of start,
             effect_node_end = end_of,
             resource_usage_node_end = begin_of,
             time_window_node_end = begin_of,
             link_from_node_end = end_of,
             link_to_node_end = begin_of,
             resource_overall = (0..infinity),      ;;; i.e. some
             resource_at_node_end = (0..0);         ;;; i,e. none
    ")
   ==> (cons 'defaults
	     *default-defaults*))

  ;; The first entry in the user defaults should now be the same as
  ;; the standard defaults
  ((car *user-defaults*) ==> *default-defaults*)

  ;; Now try changing a default
  ((let ((*user-defaults* nil))
     (compile-tf-string #'<tf-form>
	"defaults link_from_node_end = begin_of;")
     (get-default-value 'link_from_node_end))
   ==> 'begin_of))


;;; plan_viewer forms

;;; world_viewer forms

;;; prefer_plans_with forms

(define-test-group (prefer_plans_with-form-tests
		    :with ((*progress-reports* nil)))
  ;; Test the example from the TF Manual.
  ((compile-tf-string #'<tf-form> "
      prefer_plans_with 1 latest_finish_of_plan,
                        2 {resource fuel Port_1},
                        3 {resource money};
      ") ==> :any)) ;/\/: fill in

;;; prefer_schemas forms

;;; resource_units forms

(define-test-group (resource_units-form-tests
		    :with ((*progress-reports* nil)))
  ;; Test the example from the TF Manual.
  ((compile-tf-string #'<tf-form> "
      resource_units person/people = count,
                     gallons = count;
      ") ==> :any)) ; /\/: fill in

;;; resource_types forms

(define-test-group (resource_types-form-tests
		    :with ((*progress-reports* nil)))
  ;; Test the example from the TF Manual, but only the parts that
  ;; are supported.  I.e., only the consumable_strictly resource.
  ((compile-tf-string #'<tf-form> "
      resource_types consumable_strictly {resource money} = dollars,
                     consumable_strictly
                     ;;; should be consumable_producable_by_agent
                       {resource fuel ?{type port} ?{type tank}}
                         = gallons;
      ") ==> :any)) ; /\/: fill in

;;; resource_conversions forms -- not yet defined


;;; domain_rules forms

(define-test-group (domain_rules-form-tests
		    :with ((*progress-reports* nil)))

    ((compile-tf-string #'<tf-form>
      "domain_rules {cleartop table} = true => {empty table} = true;")
     ==> `(domain_rules
	    (=> (and ,(pv-pair '(cleartop table) 'true))
	             ,(pv-pair '(empty table) 'true))))
    
  ;; Test the example from the TF Manual.
  ((compile-tf-string #'<tf-form> "
      domain_rules forall ?a=?(type block), ?b=?(type block),
                          {on ?a ?b}=true => {cleartop ?b}=false;")
   ==> :any)) ; /\/: fill in

;;; compute_condition forms

(define-test-group (compute_condition-form-tests
		    :with ((*progress-reports* nil)))

    ((compile-tf-string #'<tf-form> "
        compute_condition
           {fn_ask ?? ?{or undef ?{type list}}}       = ??,
           {fn_cond ?{or true false} ?? ??}           = ??,
           {fn_or ?{or true false} ?{or true false}}  = ?{or true false},
           {fn_and ?{or true false} ?{or true false}} = ?{or true false},
           {fn_eq ?? ??}                              = ?{or true false},
           {fn_neq ?? ??}                             = ?{or true false},
           {fn_leq ?{type number} ?{type number}}     = ?{or true false},
           {fn_geq ?{type number} ?{type number}}     = ?{or true false};
        ") ==> :any)) ; /\/: fill in

;;; Language forms

;;; N.B. In "language lisp" code, { ... } works only inside actors.

(define-test-group lisp-from-strings-tests
  ;; Note that there is no implicit whitespace between the strings.
  ((read-lisp-from-strings '())                  ==> '())
  ((read-lisp-from-strings '("a b c"))           ==> '(a b c))
  ((read-lisp-from-strings '("a" "b" "c"))       ==> '(abc))
  ((read-lisp-from-strings '("(op" "en)"))       ==> '((open)))
  ((read-lisp-from-strings '("(on" " a " " b)")) ==> '((on a b))))

(define-test-group lisp-from-lines-tests
  ;; There is implicit whitespace (newlines) between the strings.
  ((convert-language-text 'lisp '())               ==> '())
  ((convert-language-text 'lisp '("a b c"))        ==> '(a b c))
  ((convert-language-text 'lisp '("a" "b" "c"))    ==> '(a b c))
  ((convert-language-text 'lisp '("(op" "en)"))    ==> '((op en)))
  ((convert-language-text 'lisp '("(on" "a" "b)")) ==> '((on a b)))
  ((convert-language-text 'lisp '("?{or" "a b}"))
   ==> `(,(make-function-actor 'or '(a b)))))

(define-test-group (language-form-tests
		    :with ((*progress-reports* nil)))

  ((compile-tf-string #'<tf-form> "
language lisp;
end_language;
")
   ==> '(language lisp ()))

  ((compile-tf-string #'<tf-form> "
language lisp;
  (defun square (x) (* x x))
  (defun on-a-b ()
    '?{or a b})
end_language;
")
   ==> `(language lisp
	 ((defun square (x) (* x x))
	  (defun on-a-b ()
	    ',(make-function-actor 'or '(a b))))))
  )


;;; Always forms

(define-test-group (always-form-tests
		    :with ((*progress-reports* nil)))

  ((compile-tf-string #'<tf-form>
		      "always {clear block_a};")
   ==> (cons 'always
	     (list (always-fact '(clear block_a) 'true))))

  ((compile-tf-string #'<tf-form>
		      "always {clear a}, {colour a} = red;")
   ==> (cons 'always
	     (list (always-fact '(clear a) 'true)
		   (always-fact '(colour a) 'red))))
  )


;;; Types forms

(define-test-group (types-form-tests
		    :with ((*progress-reports* nil)))
    
  ((compile-tf-string #'<tf-form>
		      "types colour = (red green blue);")
   ==> (cons 'types
	     (list (type-def 'colour '(red green blue)))))

  ((compile-tf-string #'<tf-form>
		      "types colour = (red green blue),
			     object = (a b c);")
   ==> (cons 'types
	     (list (type-def 'colour '(red green blue))
		   (type-def 'object '(a b c)))))
  )

(define-test-group (basic-type-expression-tests
		    :with ((*suppress-messages* t)))
  ((t-eval-string "(a b c)")                         ==> '(a b c))
  ((t-eval-string "(1 .. 5)")                        ==> '(1 2 3 4 5))
  ((t-eval-string "(a 2 c)")                         :errors 1)
  ((t-eval-string "?{or (a b c) (b d c)}")           ==> '(a b c d))
  ((t-eval-string "?{or (a b c) ?{not (b d c)}}")    :errors 1)
  ((t-eval-string "?{and (a b c) (b d c)}")          ==> '(b c))
  ((t-eval-string "?{and (a b c) ?{not (b d c)}}")   ==> '(a))
  ((t-eval-string "?{and ?{not (b d c)} (a b c)}")   ==> '(a))
  ((t-eval-string "?{and (a b c d e) ?{not (c)} (b c d)}") ==> '(b d))
  ((t-eval-string "?{type block}"
		  (list (type-def 'colour '(red green blue))
			(type-def 'block '(a b c))))
   ==> '(a b c))
  ((t-eval-string "?{or ?{type block} ?{type colour}}"
		  (list (type-def 'colour '(red green blue))
			(type-def 'block '(a b c))))
   ==> '(a b c red green blue))
  )

(define-test-group (type-expression-definition-tests
		    :with ((*defs* nil)))
  ((let ((f "t1 = (a b c),
             t2 = ?{or ?{type t1} ?{type t3}},
             t3 = ?{and ?{type t4} ?{not ?{type t5}}},
             t4 = (a b c d e f g h),
             t5 = (a   c   e   g  );"))
     (multiple-value-bind (types errs) (compile-tf-string #'<types-form> f)
       (assert (or (zerop errs)
		   (list-beginning 'types types)))
       (setq *defs* (cdr types))
       (values types errs)))
   :errors 0)
  ((lookup-tf-type 't1 *defs*)   ==> '(a b c))
  ((lookup-tf-type 't4 *defs*)   ==> '(a b c d e f g h))
  ((let ((*error-count* 0))
     (setq *defs* (expand-type-expressions *defs*))
     (values *defs* *error-count*))
   :errors 0)
  ((lookup-tf-type 't1 *defs*)   ==> '(a b c))
  ((lookup-tf-type 't2 *defs*)   ==> '(a b c d   f   h))
  ((lookup-tf-type 't3 *defs*)   ==> '(  b   d   f   h))
  ((lookup-tf-type 't4 *defs*)   ==> '(a b c d e f g h))
  ((lookup-tf-type 't5 *defs*)   ==> '(a   c   e   g  )))

(define-test-group (type-expression-cycle-tests
		    :with ((*suppress-messages* t)
			   (*defs* nil)))
  ((let ((f "t1 = ?{type t2},
             t2 = ?{or (a b c) ?{type t1}};"))
     (multiple-value-bind (types errs) (compile-tf-string #'<types-form> f)
       (assert (or (zerop errs)
		   (list-beginning 'types types)))
       (setq *defs* (cdr types))
       (values types errs)))
   :errors 0)
  ((let ((*error-count* 0))
     (setq *defs* (expand-type-expressions *defs*))
     (values *defs* *error-count*))
   :errors 1))

(defun t-eval-string (s &optional defs)
  (multiple-value-bind (expr errors) (compile-tf-string #'<type-expr> s)
    (if (and errors (> errors 0))
	(values expr errors)
      (t-eval expr defs '()))))

(defun lookup-tf-type (name defs)
  (let ((d (find name defs :key #'type-def-name)))
    (if d
	(type-def-set d)
      (error "No type named ~S in ~S." name defs))))


;;; Schemas

;;; We have to test many different things about schemas.

;;; Schema bracketing

;;; This is a test of our use of *blocking-tokens*.  If we change
;;; that mechanism, or how we use it, then these tests should change
;;; as well.

(define-test-group (schema-bracketing-tests
		    :with ((*suppress-messages* t)))
  ((map-1st-value
      #'(lambda ()
	  (compile-tf-string
	     (nt-sequence #'<tf-form> #'<1-token>)
	     "schema test; end_schema; a"))
      #'last)
   ==> '(a)
   :errors 0)
  
  ((map-1st-value
      #'(lambda ()
	  (compile-tf-string
	     (nt-sequence #'<tf-form> #'<1-token>)
	     "schema test; end_meta_schema; a"))
      #'last)
   ==> '(a)
   :errors :some)
  
  ((map-1st-value
      #'(lambda ()
	  (compile-tf-string
	     (nt-sequence #'<tf-form> #'<1-token>)
	     "schema test;
                vars ?x =    ;;; missing <variable_restriction>
              end_schema;
              a"))
      #'last)
   ==> '(a)
   :errors :some)

  ;; It's the semicolon that lets us see the "end_schema"
  ;; as the end of the schema.
  ((map-1st-value
      #'(lambda ()
	  (compile-tf-string
	     (nt-sequence #'<tf-form> #'<1-token>)
	     "schema test;
                vars ?x = undef, ?y = undef;
                expands {test ?x ?y;           ;;; missing } but present ;
              end_schema;
              a"))
      #'last)
   ==> '(a)
   :errors :some)
  )


;;; instance_of clauses -- parses, but not yet supported

#+:undef
(define-test-group (instance_of-clause-tests
		    :with ((*progress-reports* nil)
			   (*suppress-messages* t)))
  ((compile-tf-string #'<schema-clause>
		      "instance_of test_meta_schema;")
   ==> '(instance_of . test_meta_schema)
   :errors 1))				;because not supported yet

;;; info clauses

(define-test-group (info-clause-tests
		    :with ((*progress-reports* nil)))
  ((compile-tf-string #'<schema-clause>
		      "info title \"test schema\",
                            author \"the test writer\";")
   ==> '(info (title "test schema")
	      (author "the test writer"))))

;;; vars clauses

(define-test-group (vars-clause-tests
		    :with ((*progress-reports* nil)))
  ((compile-tf-string #'<schema-clause>
		      "vars ?x = undef, ?y = undef;")
   ==> `(vars ,(var-def 'x *undef*)
	      ,(var-def 'y *undef*))))

;;; expands clauses

(define-test-group (expands-clause-tests
		    :with ((*progress-reports* nil)))
  ((compile-tf-string #'<schema-clause>
		      "expands {puton ?x ?y};")
   ==> (cons 'expands
	     (string-pattern "{puton ?x ?y}"))))


;;; only_use_for_effects clauses

(define-test-group (only_use_for_effects-clause-tests
		    :with ((*progress-reports* nil)))

  ((get-default-value 'effect_node_end) ==> 'end_of)
    
  ((compile-tf-string #'<schema-clause>
      "only_use_for_effects
         {installed services contractor B};")
   ==> (cons 'only_use_for_effects
	     (list
	      (effect (string-pattern "{installed services contractor B}")
		      (get-default-value 'value)))))

  ((compile-tf-string #'<schema-clause>
      "only_use_for_effects
         {brickwork done} = true;")
   ==> (cons 'only_use_for_effects
	     (list
	      (effect (string-pattern "{brickwork done}")
		      (get-default-value 'value)))))

  ((compile-tf-string #'<schema-clause>
      "only_use_for_effects
         {on ?x ?y}    = true,                                   
         {cleartop ?y} = false,
         {on ?x ?z}    = false,
         {cleartop ?z} = true;")
   ==> (cons 'only_use_for_effects
	     (list (effect (string-pattern "{on ?x ?y}") 'true)
		   (effect (string-pattern "{cleartop ?y}") 'false)
		   (effect (string-pattern "{on ?x ?z}") 'false)
		   (effect (string-pattern "{cleartop ?z}") 'true)))))

;;; only_use_for_resources clauses

;;; --- Not yet fully implemented.

(define-test-group (only_use_for_resources-clause-tests
		    :with ((*progress-reports* nil)))
  ((compile-tf-string #'<schema-clause>
      "only_use_for_resources
         consumes {resource money} = 0..1000 dollars overall;")
   ==> :any)) ; /\/: fill in


;;; local_vars clauses

(define-test-group (local_vars-clause-tests
		    :with ((*progress-reports* nil)))
  ((compile-tf-string #'<schema-clause>
		      "local_vars ?x = undef, ?y = undef;")
   ==> `(local_vars
	  ,(var-def 'x *undef*)
	  ,(var-def 'y *undef*))))

;;; vars_relations clauses

(define-test-group (vars_relations-clause-tests
		    :with ((*progress-reports* nil)))
  ((compile-tf-string #'<schema-clause>
      "vars_relations ?x = ?y, ?y /= ?z;")
   ==> (cons 'vars_relations
	     (list (var-relation (var->actor 'x) '= (var->actor 'y))
		   (var-relation (var->actor 'y) '/= (var->actor 'z))))))

(defun var->actor (name)
  (list actorsym :*act (list 'given name)))


;;; nodes clauses

(define-test-group (nodes-clause-tests
		    :with ((*progress-reports* nil)
			   (*schema-plist* '(:type task))))
  ((compile-tf-string #'<schema-clause>
      "nodes 1 start, 2 finish, 3 action {build house};")
   ==> (cons 'nodes
	     (list (node 1 'start)
		   (node 2 'finish)
		   (node 3 'action (string-pattern "{build house}"))))))

#+:undef
(define-test-group (iterated-node-spec-tests
		    :with ((*suppress-messages* t)))
  ;; Test example from the TF Manual.
  ((compile-tf-string #'<node-spec>
      "3 iterate action {fly_to ?way_point}
           for ?way_point over ({100 50} {200 60} {150 40})")
   ==> :any				; /\/: fill in
   :errors 1))				; because not yet supported

;;; orderings clauses

(define-test-group (orderings-clause-tests
		    :with ((*progress-reports* nil)))

  ((get-default-value 'link_from_node_end) ==> 'end_of)
  ((get-default-value 'link_to_node_end)   ==> 'begin_of)

  ((compile-tf-string #'<schema-clause>
      "orderings 1 ---> 3, 3 ---> 2;")
   ==> (cons 'orderings
	     (list (ordering 1 :end 3 :begin)
		   (ordering 3 :end 2 :begin))))

  ((compile-tf-string #'<schema-clause>
      "orderings 3 --- 0..100 ---> 4;")
   ==> (cons 'orderings
	     (list (ordering 3 :end 4 :begin (time-window 0 100))))))


;;; conditions clauses

;;; --- Not ready yet.
;;; The parser should handle everything (modulo recent changes
;;; in syntax), but there are many cases to test and the emitted
;;; structures have not yet been fully defined.

(define-test-group (conditions-clause-tests
		    :with ((*progress-reports* nil)))

  ((get-default-value 'condition_node_end) ==> 'begin_of)
  
  ;; Test examples from the TF Manual.
  ((compile-tf-string #'<schema-clause> "
      conditions achieve {on a b} at 2,
                 achieve {on b c} at 2;")
   ==> :any) ; /\/: fill in

  ;; /\/: Needs syntax change to make "at <condition-point>" optional.
  #+undef
  ((compile-tf-string #'<schema-clause> "
      conditions  only_use_for_query {on ?x ?z},
                  achieve {cleartop ?y},
                  achieve {cleartop ?x};")
   ==> :any)) ; /\/: fill in


;;; effects clauses

(define-test-group (effects-clause-tests
		    :with ((*progress-reports* nil)))

  ((get-default-value 'effect_node_end) ==> 'end_of)
    
  ((compile-tf-string #'<schema-clause> "
      effects {on c a} at 1,
              {on a table} at 1,
              {on b table} at 1,
              {cleartop c} at 1,
              {cleartop b} at 1;")
   ==> (cons 'effects
	     (list (effect (string-pattern "{on c a}") 'true
			   (node-end 1 :end))
		   (effect (string-pattern "{on a table}") 'true
			   (node-end 1 :end))
		   (effect (string-pattern "{on b table}") 'true
			   (node-end 1 :end))
		   (effect (string-pattern "{cleartop c}") 'true
			   (node-end 1 :end))
		   (effect (string-pattern "{cleartop b}") 'true
			   (node-end 1 :end))))))

;;; resources clauses

;;; --- Not ready yet.


;;; time_windows clauses

(define-test-group (time_windows-clause-tests
		    :with ((*progress-reports* nil)))

  ((get-default-value 'time_window_node_end) ==> 'begin_of)

  ((compile-tf-string #'<schema-clause> "
      time_windows duration 2 = between 3 days and 4 days;")
   ==> (cons 'time_windows
	     (list (construct-time-constraint
		     (node-end 2 :begin)
		     (node-end 2 :end)
		     (time-window (time-spec 3 0 0 0)
				  (time-spec 4 0 0 0))))))

  ((compile-tf-string #'<schema-clause> "
      time_windows duration self = between 3 days and 4 days;")
   ==> (cons 'time_windows
	     (list (construct-time-constraint
		     (node-end :self :begin)
		     (node-end :self :end)
		     (time-window (time-spec 3 0 0 0)
				  (time-spec 4 0 0 0))))))

  ((compile-tf-string #'<schema-clause> "
      time_windows delay_between 2 and 3 = 10 hours .. inf;")
   ==> (list 'time_windows
	     (construct-time-constraint
	       (node-end 2 :end)
	       (node-end 3 :begin)
	       (time-window (time-spec 0 10 0 0)
			    *infinity*))))

  ((compile-tf-string #'<schema-clause> "
      time_windows between 1~11:30:00 and 1~14:30:00 at 2,
                   between 1~12:00:00 and 1~14:00:00 at 3;")
   ==> (cons 'time_windows
	     (list (construct-time-constraint
		     :abst0
		     (node-end 2 :begin)
		     (time-window (time-spec 1 11 30 00)
				  (time-spec 1 14 30 00)))
		   (construct-time-constraint
		     :abst0
		     (node-end 3 :begin)
		     (time-window (time-spec 1 12 00 00)
				  (time-spec 1 14 00 00))))))

  ((compile-tf-string #'<schema-clause> "
      time_windows 2 hours .. 3 hours at self;")
   ==> (cons 'time_windows
	     (list (construct-time-constraint
		     :abst0
		     (node-end :self :begin)
		     (time-window (time-spec 0 2 0 0)
				  (time-spec 0 3 0 0))))))

  ((compile-tf-string #'<schema-clause> "
      time_windows 20 minutes .. 2 hours;")
   ==> (cons 'time_windows
	     (list (construct-time-constraint
		     :abst0
		     (node-end :self :begin)
		     (time-window (time-spec 0 0 20 0)
				  (time-spec 0 2  0 0))))))
  )


;;; initially forms

;;; initial_resources forms

;;; initial_time forms

;;; task schemas


;;; Domain and schema structure tests

;;; All of the tests so far have essentially been tests of the parser.
;;; Such tests are less limited than they may seem, because many of
;;; the structures in the parse tree are already what they will be in
;;; the final result.  Nonetheless, it's now time to test the whole
;;; compiler.

(define-test-group (types-redefinition-tests
		    :with ((*progress-reports* nil)
			   (domain-0 nil)))
  ((let ((domain
	  (compile-from-tf-string
	     "types colour = (red green blue),
		    object = (a b c);")))
     (setq domain-0 domain)
     (domain-object-types domain))
   ==> (list (type-def 'colour '(red green blue))
	     (type-def 'object '(a b c))))

  ;; /\/: Depends on the order produced by editing-merge.
  ((let ((domain
	  (compile-from-tf-string
	     "types colour = (cyan magenta);"
	     domain-0)))
     (domain-object-types domain))
   ==> (list (type-def 'object '(a b c))
	     (type-def 'colour '(cyan magenta)))))

(define-test-group (schema-redefinition-tests
		    :with ((*progress-reports* nil)
			   (domain-0 nil)))
  ())


(define-test-group (orderings-and-windows-merge-tests
		    :with ((*progress-reports* nil)
			   (domain-0 nil)))

  ;; Provide a time window in the orderings clause and check that
  ;; it is added to those in the time_windows clause.
  ((let ((domain
	  (compile-from-tf-string
	     "schema test_1;
                expands {do something};
                nodes 1 action {step one},
                      2 action {step two},
                      3 action {step three};
                orderings 1 --- 7 days .. 8 days ---> 2,
                          2 ---> 3;
                time_windows delay_between 2 and 3 = 4 days .. 5 days;
              end_schema;")))
     (schema-time-windows (first (domain-schemas domain))))
   ==> (list ;; From orderings
	     (construct-time-constraint
	       (node-end 1 :end)
	       (node-end 2 :begin)
	       (time-window (time-spec 7 0 0 0)
			    (time-spec 8 0 0 0)))
	     ;; From time_windows
	     (construct-time-constraint
	       (node-end 2 :end)
	       (node-end 3 :begin)
	       (time-window (time-spec 4 0 0 0)
			    (time-spec 5 0 0 0)))))

  ;; Provide an ordering in the time_windows clause and check that
  ;; it is added to those in the orderings clause.
  ((let ((domain
	  (compile-from-tf-string
	     "schema test_1;
                expands {do something};
                nodes 1 action {step one},
                      2 action {step two},
                      3 action {step three};
                orderings 1 --- 7 days .. 8 days ---> 2;
                time_windows delay_between 2 and 3 = 4 days .. 5 days;
              end_schema;")))
     (schema-orderings (first (domain-schemas domain))))
   ==> (list ;; From orderings
	     (ordering 1 :end 2 :begin
		       (time-window (time-spec 7 0 0 0)
				    (time-spec 8 0 0 0)))
	     ;; From time_windows
	     (ordering 2 :end 3 :begin nil)))

  ;; Provide an ordering in the time_windows clause and check that
  ;; it is added to those in the orderings clause.  But also have
  ;; some time_windows that should not be added.
  ((let ((domain
	  (compile-from-tf-string
	     "schema test_1;
                expands {do something};
                nodes 1 action {step one},
                      2 action {step two},
                      3 action {step three};
                orderings 1 --- 7 days .. 8 days ---> 2;
                time_windows delay_between 2 and 3 = 4 days .. 5 days,
                             duration 2 = 10 days,
                             before 100 days at end_of 3;
              end_schema;")))
     (schema-orderings (first (domain-schemas domain))))
   ==> (list ;; From orderings
	     (ordering 1 :end 2 :begin
		       (time-window (time-spec 7 0 0 0)
				    (time-spec 8 0 0 0)))
	     ;; From time_windows
	     (ordering 2 :end 3 :begin nil)))

  )


;;; Some functions to call and look at the result.
;;; Should be replaced by proper tests when there's time.

(defun test-compile-1 ()
  (compile-from-tf-string
    "always {on a b},
            {colour a} = red;
     types colour = (red green blue),
           objects = (a b c);
     schema test_1;
       vars ?c = ?{type colour}, ?d = undef;
       expands {nothing};
     end_schema;
    "))

(defun test-compile-2 ()
  (compile-from-tf-string
    "
task stack_ABC;
  nodes 1 start,
        2 finish;
  orderings 1 ---> 2;
  conditions achieve {on a b} at 2,
             achieve {on b c} at 2;
  effects {on c a} at 1,
          {on a table} at 1,
          {on b table} at 1,
          {cleartop c} at 1,
          {cleartop b} at 1;
end_task;

schema puton;
  vars ?x = undef,  ?y = undef,  ?z = undef;
  expands {puton ?x ?y};                 ;;; the actual action name
  only_use_for_effects
              {on ?x ?y}    = true,                                   
              {cleartop ?y} = false,     ;;; satisfy conditions in plan
              {on ?x ?z}    = false,
              {cleartop ?z} = true;
  conditions  only_use_for_query {on ?x ?z},    ;;; used to bind one or
                                                ;;; more free variables
              achieve {cleartop ?y},
              achieve {cleartop ?x};     ;;; conditions have value true
end_schema;
"))

(defun test-compile-3 ()
  ;; Check that node ends are reasonable.
  (compile-from-tf-string
    "schema test_1;
       expands {action a};
       effects {a done};
       conditions achieve {b done};
     end_schema;
     schema test_2;
       expands {action a};
       effects {a done} at self;
       conditions achieve {b done} at self;
     end_schema;
     schema test_3;
       expands {action a};
       nodes 1 action {do something};
       effects {a done} at 1;
       conditions achieve {b done} at 1;
     end_schema;"))

;;; End
