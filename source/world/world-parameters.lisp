;;;; File: world-parameters.lisp
;;; Contains: WorldSim parameters that are global variables
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: 15 May 1995
;;; Updated: Tue Mar 23 02:07:22 1999 by Jeff Dalton
;;; Copyright: (c) 1995, AIAI, University of Edinburgh

(in-package :oplan-world)

(defparameter *default-world-definition* "micro-exec")

(defparameter *show-hidden-events* nil
  "True to show scheduled events even if their hidden-p slot is true.")

;;; The Exogenous Event Manager is off by default.  It is automatically
;;; turned off if an EEM-script is loaded should it already be on.

(defparameter *EEM-on* nil) 

;;; Where an interval is 10 minutes (600 seconds).
(defparameter *EEM-probability-of-occurance-in-an-interval* 0.40)

(defparameter *exogenous-event-distribution*
  '(PROB-DIST
    (1.00 no-event)))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
