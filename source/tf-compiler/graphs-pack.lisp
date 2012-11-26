;;;; File: graphs-pack.lisp
;;; Contains: Package for some graph algorithms
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: October 1997
;;; Updated: Wed Oct 15 02:51:11 1997 by Jeff Dalton
;;; Copyright: (c) 1997, AIAI, University of Edinburgh

;;; This is another backwards package (see interface-package.lisp)
;;; in that the file that defines the contents calls use-package
;;; rather than in-package.  That makes the package definition
;;; simpler, because it doesn't need to get the package-use and
;;; imports right, but still defines the exports.

(in-package :oplan)

(defpackage #:oplan-graph-util

  (:export

     #:strongly-connected-components
     #:build-component-graph
     #:element-to-set-map
     #:transpose-graph
     #:tclosure
     #:copy-eq-hash-table
     #:tsort*
     #:dfs-finish-order
     #:find-longest-path-lengths*
     #:has-cycle-p
     
     )

  )

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
