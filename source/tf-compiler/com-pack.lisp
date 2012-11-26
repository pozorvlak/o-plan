;;;; File: com-pack.lisp
;;; Contains: Package definition for the TF compiler implementation
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: March 1992
;;; Updated: Sun Sep  5 20:25:26 1999 by Jeff Dalton
;;; Copyright: (c) 1992, 1993, 1994 AIAI, University of Edinburgh

;;; N.B. must :use the package that defines the symbols in the
;;; TF language.

(in-package :user)

(defpackage #:oplan-tf-compiler
  (:use #+(or :ansi-cl :cltl2) #:common-lisp
	#-(or :ansi-cl :cltl2) #:lisp
		 #:tf-language
		 #:oplan-graph-util
		 #:oplan-domain-services
		 #:oplan-schema-defs
		 #:oplan-cm-defs
		 #:oplan-parser-kit
		 #:oplan-developerlib
		 #:oplan-ipc		;for defmessage
		 #:oplan-util)	
  (:import-from #:oplan-obase
                #:actorsym
		#:actorp
		#:question-reader
		#:given
		#:given-p
		#:actfn
		#:actargs
		#:make-given-actor
		#:make-function-actor
		#:actor-rename)
  (:shadowing-import-from #:oplan	;/\/ shouldn't be necessary
			  #:nodes)
  (:export #:compile-tf-stream
	   #:compile-from-tf-file
	   #:compile-from-tf-string
	   #:run-as-tfc
	   #:tfc

	   #:compile-tf-string		;args are: nonterminal, string

	   #:define-constraint-parser
	   #:register-constraint-parser
	   #:eval-in-tf-compiler

	   ;; For parsing
	   #:<pattern>
	   #:<pattern-component>
	   #:<value>
	   #:<node>
	   #:<node-end>
	   #:var-pattern-p
	   #:namep
	   )

  )

;;; End
