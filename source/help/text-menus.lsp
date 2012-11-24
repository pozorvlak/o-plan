;;;; File: text-menus.lsp
;;; Contains: A textual version of util:menu-request
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: 06 Oct 94
;;; Updated: Mon Dec 16 15:07:28 1996 by Jeff Dalton
;;; Copyright: (c) 1994, AIAI, University of Edinburgh

;;; This is a patch to O-Plan version 3.x.  It changes O-Plan
;;; so that it no longer uses xmenu.  Instead, menus are presented
;;; a itemized text.

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
;;; which selection you want. 

(in-package :oplan-util)

(defun menu-request (xmenu-args &key (read-function #'read))
  ;; Normally in support/LISP-util.lsp where LISP={lucid,kcl,...}.
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

