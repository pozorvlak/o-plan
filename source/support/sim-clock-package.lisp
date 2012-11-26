;;;; File: sim-clock-package.lsp
;;; Contains: Package definition for simulated-time clocks.
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: October 1994
;;; Updated: Tue Mar 23 01:46:05 1999 by Jeff Dalton
;;; Copyright: (c) 1994, AIAI, University of Edinburgh

(in-package :oplan)

(defpackage :oplan-sim-clock

  (:nicknames :clock)

  (:use #+:cltl2 :common-lisp
        #-:cltl2 :lisp)
  (:use :oplan-util)
  (:use :oplan-time-util)
  (:use :oplan-pseudo-process)

  (:export

     ;; The clock
     #:*clock*				;binding provided by support/components

     ;; The clock struct
     #:clock
     #:clock-p
     #:make-clock
     #:clock-running-p
     #:clock-base-simulated-time
     #:clock-base-real-time
     #:clock-simulated-seconds-per-second
     #:clock-granularity
     ; #:clock-cached-time
     ; #:clock-cached-time-string

     #:define-a-clock

     #:the-clock-is-running

     #:start-clock
     #:set-clock-granularity
     #:stop-clock
     #:init-clock

     ;; Conversions
     #:simulated->real-seconds
     #:real->simulated-seconds
     #:simulated->universal-time
     #:simulated-time-string
     
     ;; Getting the simulated time
     #:get-simulated-time
     #:current-simulated-time

     ;; Setting the simulated time
     #:set-simulated-time

     ;; Re-exports from pseudo-precess
     #:get-primitive-real-time 
     #:primitive-time-units-per-second
     #:universal->primitive-real-time
     #:primitive-real->universal-time

     )

  )

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
