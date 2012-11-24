;;;; File: world-toplevel.lsp
;;; Contains: The world simulation top-level
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: Tue Aug 03 1993
;;; Updated: Tue Mar 23 02:09:46 1999 by Jeff Dalton
;;; Copyright: (c) 1992, 1993, AIAI, University of Edinburgh

;;; See world-services.lsp for a description of the World process
;;; and how it works.

;;; This file defines the World process top-level for the version of
;;; the World that runs within the O-Plan Exec agent.  It also defines
;;; some global variables.

(in-package :oplan-world)

(import 'world-startup :oplan)		;/\/ for call-component-startup


;;;; Global variables with no better place to go

(defvar *world-command-readtable* (copy-readtable nil)
  "The readtable for reading world commands.  Normally the
   standard Common Lisp readtable.")


;;;; Window streams

;;; The World has three main windows plus one for displaying the current
;;; simulated time.  The history window is used to display a trace of
;;; what is happening in the world; the agenda window shows the events
;;; that have been scheduled and have yet to occur; and the interaction
;;; window is used to receive commands from, and send messages to, the
;;; user.

(defvar *history-window* nil)	;from (x-get-stream :world-history)
(defvar *agenda-window* nil)	;from (x-get-stream :world-agenda)
(defvar *interact* nil)		;from (x-get-stream :world-interact)


;;;; Initialization

;;; World-startup is called to intitialize the world process when
;;; the agent that contains the world process first starts up.
;;; Once world-startup returns, it is not called again.

;;; One "world-init" file is loaded, if it exists.  We look first in
;;; the current directory, then in the user's home directory.

(defun world-startup ()
  (load-init-file "world-init")
  (is-init)

  (setq *history-window* (x-get-stream :world-history))
  (setq *agenda-window* (x-get-stream :world-agenda))
  (setq *interact* (x-get-stream :world-interact))
  (define-a-clock)
  (ensure-world-clock-window)
  
  ;; Make breaks, etc. go to a World-related window, indeed
  ;; to the one the user will use to send input to the World.
  (x-redirect-errors :world-interact)

  ;; Arrange to be run if there's user input.
  (ipc-register-listen-stream (x-get-stream :world-interact))

  ;; Load any world definition
  (load-world-definition)

  ;; Don't forget the event handler.
  (ipc-set-event-handler 'world-event-handler))


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
