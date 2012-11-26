;;;; File: kcl-clos.lsp
;;; Contains: Defines a CLOS (subset) for KCL/GCL.
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: October 1996
;;; Updated: Fri Feb  1 23:24:37 2008 by Jeff Dalton
;;; Copyright: (c) 1996, AIAI, University of Edinburgh

(in-package :lisp)

(defparameter *micro-clos-source-directory*
  #-:linux "../public-mc/"
  #+:linux "../public-mc-linux/")

(load (concatenate 'string *micro-clos-source-directory* "mc.cl"))

(let ((*readtable* (copy-readtable nil)) ;protect our readtable /\/
      (micro-clos::*source-directory* *micro-clos-source-directory*))

  (micro-clos:load-mc))

(setq micro-clos::*display-notes* nil)

(shadow 'si::find-class :si)
(shadow 'si::class-of :si)
(shadow 'si::class-precedence-list :si)
(shadow 'si::standard-object :si)
(shadow 'si::standard-class :si)
(shadow 'si::standard-generic-function :si)
(shadow 'si::standard-method :si)
(shadow 'si::built-in-class :si)
(shadow 'si::structure-class :si)

(import micro-clos::*externals* :lisp)

; (export micro-clos::*externals* :lisp)

(dolist (s micro-clos::*externals*)
  (format t "~&Exporting ~S~%" s)
  (export s :lisp))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
