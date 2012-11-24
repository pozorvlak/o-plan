;;;; File: tf-pack-checker.lsp
;;; Contains: Procedure for checking the TF-LANGUAGE package
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: March 1992
;;; Updated: Tue Jul  9 00:52:29 1996 by Jeff Dalton
;;; Copyright: (c) 1992, 1993, 1994 AIAI, University of Edinburgh

(in-package :oplan-tf-compiler)

;;; The following procedure is used to enumerate symbols that probably
;;; ought to be in the "TF" package.  It was used when constructing the
;;; package and can also be used to check it.

(defparameter *parser-source* "parser.lsp") ;see the defsystem

(defun potential-tf-symbols ()
  (let ((syms '()))
    (map-potential-tf-symbols #'(lambda (s) (push s syms)))
    (sort (remove-if #'keywordp
		     (remove-duplicates syms))
	  #'lcase-string<)))

(defun lcase-string< (s1 s2)
  ;; Make sure non-letters appear in the right order w.r.t. letters.
  (string< (string-downcase s1) (string-downcase s2)))

(defun map-potential-tf-symbols (fn)
  (declare (special *parser-source*))	;see near the defsystem
  (labels ((map-quoted-symbols (list)
	     (cond ((atom list))
		   ((eq (car list) 'quote)
		    (map-symbols (cdr list)))
		   (;(eq (car list) 'tf-compiler::token-case)
		    (and (symbolp (car list))
			 (string= (symbol-name (car list)) "TOKEN-CASE"))
		    (map-symbols
		     (mapcar #'(lambda (keys)
				 (if (member keys '(t otherwise)) nil keys))
			     (mapcar #'car (cdr list))))
		    (map-quoted-symbols (mapcar #'cdr (cdr list))))
		   ((member (car list) '(case ecase))
		    (map-quoted-symbols (cadr list))
		    (map-symbols
		     (mapcar #'(lambda (keys)
				 (if (member keys '(t otherwise)) nil keys))
			     (mapcar #'car (cddr list))))
		    (map-quoted-symbols (mapcar #'cdr (cddr list))))
		   (t (map-quoted-symbols (car list))
		      (map-quoted-symbols (cdr list)))))
	   (map-symbols (tree)
	     (cond ((and (symbolp tree) (not (null tree)))
		    (funcall fn tree))
		   ((consp tree)
		    (map-symbols (car tree))
		    (map-symbols (cdr tree))))))
    (with-open-file (tf-parser *parser-source* :direction :input)
      (let ((*package* (find-package "TF-COMPILER"))
	    (form nil))
	(loop (setq form (read tf-parser nil :eof))
	      (when (eq form :eof) (return))
	      (map-quoted-symbols form))))))

;;; Check-tf-package returns two values:
;;;   1. A list of the potential TF symbols that are not external
;;;      symbols of the TF package.
;;;   2. A list if external TF symbols that are not in the list of
;;;      potential TF symbols.
;;;
;;; Expected elements of 1 are: given, tf-syntax-error-reporter.

(defun check-tf-package ()
  (let ((missing-symbols (potential-tf-symbols))
	(extra-symbols '()))
    (do-external-symbols (sym (find-package "TF-LANGUAGE"))
      (if (member sym missing-symbols :test #'eq)
	  (setq missing-symbols (delete sym missing-symbols :count 1))
	  (setq extra-symbols (cons sym extra-symbols))))
    (values missing-symbols
	    (sort extra-symbols #'lcase-string<))))

;;; End
