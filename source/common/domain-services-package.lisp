;;;; File: domain-services-package.lisp
;;; Contains: Interface to domain services
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: November 1996
;;; Updated: Thu Nov 28 22:44:39 1996 by Jeff Dalton
;;; Copyright: (c) 1996, AIAI, University of Edinburgh

(in-package :oplan)

(defpackage #:oplan-domain-services

  (:export

     #:*domain*

     #:mapping				;for now, at least /\/

     #:action-level
     #:effect-level

     )

  )

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
