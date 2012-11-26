;;;; File: parser-kit-pack.lsp
;;; Contains: Package definition for a recursive-descent parser kit
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: 27 March 1993
;;; Updated: Tue Mar 16 04:12:24 1999 by Jeff Dalton
;;; Copyright: (c) 1992, 1993, AIAI, University of Edinburgh

;;; Parser-Kit -- an interface to the TF compiler parsing framework.

(in-package :oplan)

(defpackage #:oplan-parser-kit

  (:use #+(or :ansi-cl :cltl2) :common-lisp
        #-(or :ansi-cl :cltl2) :lisp)

  (:use #:oplan-util)

  (:export
   
    ;; Scanner interface
    ;; /\/ Maybe don't export *token*?
    #:token #:next-token #:*token* #:*end-token*
    #:with-token-generator #:*token-generator*
    #:push-token

    ;; Compiler-interface
    #:*syntax-error-reporter*
    #:*recovering*
    #:*error-count*
    #:*warning-count*
    #:test-compile
    #:list-tokens
    
    ;; Parsing procedures
    #:syntax-error
    #:token-mismatch-error
    #:token-set-error
    #:must-be      #:must-satisfy     #:must-be-member
    #:recover-at   #:recover-when
    #:skip-to      #:skip-until
    #:token-is     #:token-satisfies  #:token-case
    #:one-or-more  #:zero-or-more     #:*blocking-tokens*)

  )


