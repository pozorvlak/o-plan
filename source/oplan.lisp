;;;; File: oplan.lisp
;;; Contains: Initial setup for compiling and building O-Plan
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: February 1993
;;; Updated: Mon Jan 24 22:44:55 2000 by Jeff Dalton
;;; Copyright: (c) 1993 -- 1997 AIAI, University of Edinburgh
;;; This material may be reproduced by or for the U.S. Government pursuant
;;; to the copyright license under the clause at DFARS 252.227-7032
;;; (June 1975) -- Rights in Technical Data and Computer Software (Foreign).

;;; Load this file to prepare for compiling or loading O-Plan.
;;; It loads defsystem and, if needed, defpackage, and takes some
;;; other steps to prepare Lisp for what's to come.  When loading,
;;; it assumes that the current directory is "source" in an O-Plan
;;; file tree.

;;; O-Plan is normally compiled and built by using the Makefile, but
;;; it can also be done by hand.

;;; To build an executable image:
;;;   0. Run a Lisp.
;;;   1. (load "oplan.lisp").
;;;   2. Maybe:
;;;       2.1. (compile-system 'oplan), or
;;;            (compile-system 'everything)
;;;       2.2. Exit Lisp, rerun, and reload "oplan.lisp".
;;;   3. (load-system 'oplan)
;;;   4. (oplan:save-oplan <filename>).

;;; For more on the sequence of commands for various cases, consult
;;; the Makefile.

;;; Note that O-Plan makes a number of changes to Lisp, by defining
;;; packages, setting global variables, etc, that may interfere with
;;; other code if you try to load it and O-Plan into the same Lisp.
;;; However, most of these changes are confined to packages that have
;;; names beginning with "OPLAN-".

;;; IMPORTANT: changes to this file (oplan.lisp) and to the files
;;; it loads (such as support/lisp-prep.lisp) will not be noticed
;;; by defsystem and hence will not result in recompilation.
;;; So you'll have to work out the required recompilations (if any)
;;; yourself.  This has not been a significant problem in practice.

#+ansi-cl (in-package :common-lisp-user)
#-ansi-cl (in-package :user)
(rename-package "COMMON-LISP-USER" "COMMON-LISP-USER" '("CL-USER" "USER"))

(defvar user::*oplan-version* "3.3")

(push :oplan *features*)

(load "oplan-release-date.lisp")		;defines *oplan-release-date*


;;; Lisp-implementation-specific prep

(load "support/lisp-prep.lisp")


;;; General prep

(setq *print-length* nil
      *print-level* nil
      *print-case* :downcase
      *print-pretty* t)

(unless (fboundp (find-symbol "DEFPACKAGE"))
  (load "support/defpackage.lsp"))

(unless (find-package "SIMPLE-DEFSYSTEM")
  (load "support/defsys.lsp"))

(shadowing-import '(simple-defsystem:defsystem
		    simple-defsystem:find-system
                    simple-defsystem:compile-system
                    simple-defsystem:load-system
		    simple-defsystem:clean-system
		    simple-defsystem:print-system-tree))


;;; Initial OPLAN Package setup

(defpackage "OPLAN"
  (:nicknames "OPLAN-KNOWLEDGE-PLATFORM"
              "OPLAN-KNOWLEDGE-SOURCE")
  (:use #+(or :ansi-cl :clcl2) "COMMON-LISP"
	#-(or :ansi-cl :clcl2) "LISP"
        "SIMPLE-DEFSYSTEM"))


;;; System definitions

(load "system-definitions.lsp")


;;; End
