;;;; File: no-x-programs.lsp
;;; Contains: Patch so O-paln2 no longer needs xmenu or xuim.
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: 17 February 1994
;;; Updated: Mon Dec 16 20:54:33 1996 by Jeff Dalton
;;; Copyright: (c) 1994, AIAI, University of Edinburgh

;;; This is a patch to O-Plan version 3.x.  It changes O-Plan
;;; so that it no longer uses xmenu or xuim.

;;; Operations formerly done via the control panel now must be done
;;; by hand.  This is a pain.  You'll have to look at the sources
;;; to see how to do it.  Fortunately, it's almost never required.

;;; Menus formerly presented via xmenu will appear in a different
;;; form.  It doesn't look very nice, but it should work.

;;; N.B. Menus appear in the Lisp interaction window (usually
;;; the window you used to start O-Plan).  A menu will be displayed
;;; like this (without the leading ";;;"s):

;;; Menu: Domains to choose from:
;;;    "bbb"
;;;    "blocks-1"
;;;    "blocks-2"
;;;    "eusat"
;;;    "house-1-inc-1"
;;;    "house-1"
;;;    "house-2"
;;;    "house-3"
;;;    "house-4"
;;;    "pacifica"
;;;    "space-platform"
;;;    "spanner"
;;;    ----------
;;;    "Change directory=:cd"
;;;    "Enter filename=:enter"
;;;    ----------
;;;    "QUIT=:quit"
;;;
;;; menu>

;;; Type your selection after the "menu> " prompt.  Each indented line
;;; other than the "----------" lines is a possible selection.  Decide
;;; which selection you want.  If the text between quotes does not
;;; contain an equal sign ("="), type *exactly* the text between the
;;; quotes.  Do not type the quotes.  If it contains an =, type the
;;; text between the "=" and the closing quote.  Note that there's
;;; often a ":" after the "=".  Don't forget to type the ":".

;;; >>> Try not to make a mistake when typing the selection.  <<<
;;; >>> A Lisp error may occur if you get it wrong.           <<<


(in-package :oplan)


;;; 1. Eliminate the control panel.

(defun set-up-control-panel ()
  ;; Normally in interface-manager/control-panel.lsp
  nil)


;;; 2. Replace xmenu.

(defun oplan-util:menu-request (xmenu-args &key (read-function #'read))
  ;; Normally in support/lucid-util.lsp
  (let ((io *terminal-io*)
	(args xmenu-args))
    (loop
      (cond ((null args) (return))
	    ((string= (car args) "-heading")
	     (pop args)
	     (princ "Menu: ")
	     (princ (pop args))
	     (terpri))
	    ((string= (car args) "-line")
	     (pop args)
	     (princ "   ----------" io)
	     (terpri))
	    (t
	     (let ((m (pop args)))
	       (princ "   ")
	       (prin1 m)
	       (terpri)))))
    (terpri)
    (princ "menu> " io)
    (funcall read-function io)))

;;; End

