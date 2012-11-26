;;;; File: test-systems.lisp
;;; Contains: O-Plan system definitions for test systems
;;; Author: Jeff Dalton
;;; Created: Sat Mar  5 13:22:55 1994 by Jeff Dalton
;;; Updated: Sun Dec  1 20:22:13 1996 by Jeff Dalton
;;; Copyright: (c) 1994, AIAI, University of Edinburgh

;;; Dependencies on "the rest of O-Plan" are not listed.

;;; Tests aren't normally compiled, so we'll usually be loading
;;; interpreted code.

;;; The test-support system is defined with the main O-Plan system
;;; definitions, because it's often compiled with them.  Tests are
;;; typically not compiled.

;;; Test systems must :require test-support, but they can assume
;;; this gives them the support system as well.

(in-package :oplan)

(defun load-test-modules ()
  (load-system 'test-support :recursive nil :ignore-depends t)
  (load-system 'all-tests :recursive t :ignore-depends t)
  (use-package :oplan-test-support)
  t)

(defsystem (all-tests
	    :required-systems
	      ;; We like them in the following order, but all that really
	      ;; matters is that the 1st one come 1st.
	      (t-frame-tests
	       context-tests
	       tpn-tests
	       or-tree-tests
	       pseudo-process-tests
	       all-tf-compiler-tests
	       graph-tests))
  ;; This system should directly or indirectly require all test systems.
  )

(defsystem (t-frame-tests
	    :directory "support"
	    :required-systems (test-support))
  (t-frame-tests))

(defsystem (context-tests
	    :directory "support"
	    :required-systems (test-support))
  (context-tests))

(defsystem (tpn-tests
	    :directory "database-manager"
	    :required-systems (test-support
			       shared-datastructures))
  (tpn-test-support)
  (tpn-tests))

(defsystem (or-tree-tests
	    :directory "database-manager"
	    :required-systems (test-support))
  (or-tree-tests))

(defsystem (pseudo-process-tests
	    :directory "support"
	    :required-systems (test-support))
  (pseudo-process-tests))

(defsystem (all-tf-compiler-tests
	    :required-systems (tf-compiler-tests
			       tf-parser-tests))
  ;; This system should directly or indirectly require all
  ;; TF compiler test systems.
  )

(defsystem (tf-compiler-tests
	    :directory "tf-compiler"
	    :required-systems (test-support
			       tf-compiler))
  ;; Definitions needed to all TF compiler tests.
  (compiler-tests))

(defsystem (tf-parser-tests
	    :directory "tf-compiler"
	    :required-systems (tf-compiler-tests))
  (parser-tests))

(defsystem (graph-tests
	    :directory "tf-compiler"
	    :required-systems (test-support))
  (graph-tests))

;;; End
