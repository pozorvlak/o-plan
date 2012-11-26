;;;; File: interface-package.lisp
;;; Contains: Package defining the programmatic interface
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: October 1994
;;; Updated: Sat Jan 25 04:25:40 1997 by Jeff Dalton
;;; Copyright: (c) 1994, AIAI, University of Edinburgh

;;; The oplan-interface package defines the names in the programmatic
;;; interface to O-Plan.  Most of the actual definitions are in the files
;;; subr-mode.lisp and program-interface.lisp.

;;; In a sense, this package defines the exports of the oplan package;
;;; but rather than have the exports in the definition of the oplan
;;; package, we put them here and have the oplan package use this one.
;;; This is because the oplan package doesn't have a neat definition
;;; in one place.

(in-package :oplan)

(defpackage #:oplan-interface

  (:nicknames #:opi)

  ; (:import-from #:oplan-util #:exit-lisp)

  (:import-from #:oplan #:exit-oplan)

  (:import-from #:oplan-pseudo-process #:pprocess-main-loop)

  (:export

     ;; Planning and replanning
     #:plan-for
     #:replan

     #:generate-plans
     #:generate-plans-as-options

     #:plan-by-levels

     ;; Authority
     #:set-authority

     ;; Plan statistics
     #:request-plan-statistics-list

     ;; Plan views
     #:request-plan-view-list
     #:request-plan-view
     #:request-psgraph

     ;; World views
     #:request-world-view-list
     #:request-world-view

     ;; Other requests for plan information
     #:request-action-level-alist

     ;; Time bounds
     #:request-time-bounds

     ;; Component output
     #:redirect-component-output
     #:set-component-output-stream
     #:set-component-debug-level
     #:output-off			;defined in manual-interface.lisp

     ;; Sending and receiving
     #:ask-oplan
     #:send-to-oplan
     #:receive-from-oplan
     #:receive-else-error
     #:receive-case			;/\/ not defined yet

     ;; Timeouts
     #:timeout				;a condition type
     #:timeout-not-handled		;an error condition type
     #:set-timeout
     #:cancel-timeout
     #:with-timeout			;a macro
     #:call-with-timeout		;function equiv to & used by the macro

     ;; Etc
     #:pprocess-main-loop
     #:exit-oplan
     #:add-exit-action			;defined in subr-mode.lisp
     #:go-faster			;defined in manual-interface.lisp
     )

  )

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
