;;;; File: kcl-clos.lsp
;;; Contains: Defines a CLOS (subset) for KCL/GCL.
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: October 1996
;;; Updated: Mon Jan 24 15:44:38 2000 by Jeff Dalton
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

(import micro-clos::*externals* :lisp)

(export micro-clos::*externals* :lisp)

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
