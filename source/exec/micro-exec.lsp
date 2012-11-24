;;;; File: micro-exec.lsp
;;; Contains: Simple, small, internal execution agent
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: 13 February 1995
;;; Updated: Tue Mar 23 03:41:49 1999 by Jeff Dalton
;;; Copyright: (c) 1995, AIAI, University of Edinburgh

;;; /\/: Doesn't work with a separate world yet.

;;; Contents:
;;;  * Introductory remarks.
;;;  * Package definition.
;;;  * Window streams.
;;;  * Startup and init.
;;;  * Event handler.
;;;  * Message definitions.

;;; This file provides a minimal execution agent, "Micro-Exec", that
;;; runs as a process in the planner.  It can use the built-in world
;;; process or an external one.

;;; The Micro-Exec and a World Simulator are started automatically if
;;; the config file contains a :config entry for the name :micro-exec.
;;; Unlike with the external Execs, this happens when the planner starts
;;; up rather than waiting for the first attempt to execute a plan.

;;; Note that socket names (:{left,right}{in,out}) are relative to the
;;; planner.  The micro-Exec therefore reads :rightout and writes :rightin.
;;; The internal World would normally want to do the same, as would the
;;; connector p-process that handles I/O with the external World -- so we
;;; have to arrange for them to do something else.  Our convention is to
;;; have sockets called :world-in and :world-out that the World reads and
;;; writes respectively.  This is handled for the internal world by using
;;; an appropriate .config file.  "oplan-combined-small.config" is an
;;; example.  The external world is handled by redefining/advising the
;;; function start-separate-world.


;;;; The micro-exec package

(in-package :oplan-micro-exec)

(use-package
  '(:oplan-util
    :oplan-time-util
    :oplan-developerlib
    :oplan-pseudo-process
    :oplan-sim-clock
    :oplan-components
    :oplan-ipc
    :oplan-xwindowio
    :oplan-initsystem
    :oplan-nodes))

(import 'micro-exec-startup :oplan)	;/\/ for call-component-startup


;;;; Window streams

(defvar *exec-io* nil)			;from (x-get-stream :execio)


;;;; Startup and init

(defun micro-exec-startup ()
  (is-init)

  ;; Make breaks, etc. go to our i/o window.
  (x-redirect-errors :execio)
  (setq *exec-io* (x-get-stream :execio))

  ;; Don't forget the event handler.
  (ipc-set-event-handler 'micro-exec-event-handler))


(defun micro-exec-init ()
  (clear-screen *exec-io*)
  )


;;;; Event handler

(defun micro-exec-event-handler (self)
  (let ((*print-case* :downcase))	;/\/: for our output
    (while (next-event-p self)
      (let ((event (next-event self)))
	(format *exec-io* "~&~%Event: ~S~%" (message-contents event))
	(ipc-handle-message self event)))))


;;;; Messages

;;; Message handlers take the place of knowledge sources.

;;; Messages from the Planner

(defmessage (:micro-exec :init) ()
  (micro-exec-init)
  (ipc-send :world :init))

(defmessage (:micro-exec :always) (pv-pairs)
  (ipc-send :world :always pv-pairs))

(defmessage (:micro-exec :begin-execution) (sim-secs-per-sec)
  (let ((real-now (get-universal-time))
	(sim-now 0)
	(sim-scale sim-secs-per-sec))
    (ipc-send :world :start-clock real-now sim-now sim-scale)))

(defmessage (:micro-exec :execute)
            (node-end-tag type pattern earliest-time latest-time
			  effects conditions)
  ;; Split up the node-end-tag so the World doesn't need to know
  ;; what it looks like.
  (ipc-send :world
    :execute (etag-node node-end-tag)
             (etag-end node-end-tag)
	     type
             pattern
	     earliest-time
	     latest-time
	     effects
	     conditions))

;;; Messages from the World.

(defmessage (:micro-exec :success) (when node end pattern)
  (report-to-planner
    :execution-success
    when
    (etag node end)
    pattern))

(defmessage (:micro-exec :failure)
            (when reason node end pattern failed-effects)
  (report-to-planner
    :execution-failure
    when
    reason
    (etag node end)
    pattern
    failed-effects))

(defmessage (:micro-exec :world-event) (when action effects)
  (report-to-planner
    :unexpected-world-event
    when
    action				;a pattern
    effects))				;a list of (p v) pairs

(defun report-to-planner (keyword &rest args)
  (ipc-write-to-oplan (cons keyword args)))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
