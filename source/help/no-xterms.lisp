;;;; File: no-xterms.lsp
;;; Contains: Patch so O-Plan doesn't need xterm
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: 9 March 1994
;;; Updated: Mon Dec 16 20:56:28 1996 by Jeff Dalton
;;; Copyright: (c) 1994, AIAI, University of Edinburgh

;;; This is a patch to O-Plan version 3.x.  It changes O-Plan
;;; so that it no longer uses xterm.

;;; N.B. The no-x-programs patch should probably be loaded as well.
;;; Then O-Plan can run without any separate Unix processes.

;;; This patch (no-xterms) stops O-Plan from starting the run-lights,
;;; causes clear-screen to print a line of dashes, and changes ta-monitor
;;; (the routine that reads selections from the TA menu) to do nothing
;;; unless it sees :ta before the (numeric) selection.

;;; In effect, you have to type something like ":ta 1" to select from
;;; the TA menu.  However, you also have to do this at the right time.
;;; Usually, only the TA and REPL processes want to read anything, and
;;; the TA gets in first.  So typically typing ":ta" before a number
;;; selects from the TA menu and typing something else (such as ":e")
;;; before an expression gets the expression evaluated.  That is, the
;;; TA then ignores it, and so the REPL reads it.  Of course, this
;;; doesn't apply after a prompt such as "menu> " or at other times
;;; when something other than ta-monitor or the REPL is trying to
;;; read.

;;; N.B. Remember that the REPL's "form> " prompt doesn't mean the
;;; REPL has called any input function.  It means only that *if*
;;; input is available, it will read an expression.

;;; Similar remarks apply to the TA's "Please choose a number:- "
;;; prompt.

(in-package :oplan)

(defun oplan-x:x-open-and-register-io-win (tag &optional (args ""))
  t)

(defun oplan-x:x-get-stream (tag)
  *terminal-io*)

(defun oplan-util:terminate-xterms-if-necessary ()
  nil)

(defun oplan-x::clear-xterm (&optional (s *standard-output*))
  (format t "~&--------------------------------------------------~%")
  nil)

(defun run-lights-if-wanted ()
  nil)

(advice+ 'oplan-task-assigner::ta-monitor 'no-xterms
  #'(lambda (previous)
      #'(lambda ()
	  (when (listen)
	    (when (eq (read) :ta)
	      (funcall previous))))))

;;; End
