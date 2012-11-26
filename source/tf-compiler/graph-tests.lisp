;;;; File: graph-tests.lisp
;;; Contains: Tests for the TF compiler's graph algorithms.
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: June 1994
;;; Updated: Fri Nov 22 01:21:53 1996 by Jeff Dalton
;;; Copyright: (c) 1994, AIAI, University of Edinburgh

(in-package :oplan-tf-compiler)

(use-package :oplan-test-support)

(define-test-module :graphs)

(in-test-module :graphs)

;;; /\/: We don't want the TF compiler's test wrapper which is one
;;; reason not to have :tf-compiler as a parent module.  Actually,
;;; :tf-parser and :graphs could both be children of :tf-compiler,
;;; with only :tf-parser using the special test wrapper.


(defun same-set-p (set1 set2 &key (test #'eql))
  (null (set-exclusive-or set1 set2 :test test)))

(defun same-set-of-sets-p (sos1 sos2 &key (test #'eql))
  (same-set-p sos1 sos2
    :test #'(lambda (set1 set2)
	      (same-set-p set1 set2 :test test))))


(define-test-group set-comparison-tests

  ((same-set-p '(a b c) '(b c a)) ==> :true)

  ((same-set-of-sets-p
    '((a b c) (p q) (i j k l))
    '((q p) (b c a) (k i j l)))   ==> :true)

  )


(define-test-group transpose-graph-tests

  ((hash-table-alist
    (transpose-graph '(a b c d e)
      #'(lambda (v)
	  (ecase v
	    (a '(b c))
	    (b '(c d))
	    (c '(b d e))
	    (d '(e))
	    (e '(a))))))
   ==> '((a . (e))
	 (b . (a c))
	 (c . (a b))
	 (d . (b c))
	 (e . (c d)))
   :test #'(lambda (have expected)
	     (same-set-p have expected
	       :test #'(lambda (h e)
			 (and (eq (alist-key h) (alist-key e))
			      (set-eql (alist-value h) (alist-value e)))))))

  )


(define-test-group transitive-closure-tests

  ;; This example drove a bug in an early version of tclosure when
  ;; run in Lucid 4.1.
  ((hash-table-alist
    (tclosure
      (alist->hash-table '((e i) (a b c) (b d e f) (d e d) (i j) (k)))))
   ==> '((e i j)
	 (a b c f e d i j)
	 (b f e d i j)
	 (d e d i j)
	 (i j)
	 (k))
   :test #'same-set-of-sets-p)

  ;; To get exactly the same test as in Lucid 4.1, we have to control
  ;; the order of the keys returned by hash-table-keys.
  ((let ((h (alist->hash-table '((e i) (a b c) (b d e f) (d e d) (i j) (k)))))
     (letf (((symbol-function 'hash-table-keys)
	     #'(lambda (ht)
		 (assert (eq ht h))
		 '(i k b d a e))))
       (hash-table-alist (tclosure h))))
   ==> '((e i j)
	 (a b c f e d i j)
	 (b f e d i j)
	 (d e d i j)
	 (i j)
	 (k))
   :test #'same-set-of-sets-p)

  )


(define-test-group scc-tests		;strongly connected components

  ((strongly-connected-components
    '(a)
    #'(lambda (v)
	(case v
	  (a  '(b c d))
	  (b  '(b1 b2))
	  (b1 '(b))
	  (b2 '(b1))
	  (c  '(d e f))
	  (d  '(e))
	  (f  '(c)))))
   ==> '((a) (b b1 b2) (c f) (d) (e))
   :test #'same-set-of-sets-p)

  )

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
