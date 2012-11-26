;;;; File: qa-all.lisp
;;; Contains: qa-all and associated routines
;;; Author: Jeff Dalton
;;; Created: November 1992
;;; Updated: Mon Dec  9 03:35:04 1996 by Jeff Dalton
;;; Copyright: (c) 1992, AIAI, University of Edinburgh

(in-package :oplan-qa)

(export '(qa-all qa-all-at-node *qa-all-report-always-facts*))

(import 'oplan::some-work-to-do-p)	;/\/

(defparameter *qa-all-report-always-facts* t)

;;;; QA-ALL
;;;
;;; There are two entry points.  QA-ALL-AT-NODE takes a single
;;; argument, a node-end.  QA-ALL takes three arguments: pattern,
;;; value, and node-end.  In both cases, the result is a list of
;;; (pattern value) sublists.  (QA-ALL-AT-NODE n) is equivalent to
;;; (QA-ALL ?? ?? n).
;;; 
;;; The basic strategy is:
;;;  1. Get all TOME entries that are valid in the current context to
;;;     see what p=v might be true.  (N.B. this does not mean v=true.)
;;;  2. Ask QA about each possibility to see if it _is_ true.
;;;
;;; One shortcut is taken: always facts are accepted immediately,
;;; without asking QA.
;;;
;;; Note that TOME entries look like this:
;;;
;;;   (TOME <patt> <tome-at-node-end>) = <val>
;;;
;;;   <tome-at-node-end> = :ALWAYS
;;;                      | (<node> <end>)
;;;                      | (<node> <end> :ALWAYS . <bindings>)
;;;

(defun qa-all-at-node (n)
  (qa-all '??
	  '??
	  n))

(defun qa-all (p v n)
  (let ((tome-shortlist (cdr (getashortlist `(TOME ,p ??))))
	(always '())			;list of (p v) pairs
	(candidates '()))		;list of (p v) pairs
    (dolist (item tome-shortlist)
      ;; exclude unless in current context
      (unless (eq (oplan-obase:value item) :undef)
	(let* ((a-item (oplan-psv:psv-actorise-pattern item))
	       (a-p (tgm-tome-pattern a-item))
	       (a-v (oplan-psv:psv-actorise-pattern (oplan-obase:value item))))
	  (cond ((not (match-p a-p p)))	;skip
		((not (match-p a-v v)))	;skip
		((eq (tgm-tome-node-end a-item) :always)
		 (push (list a-p a-v) always)) ;assume unique, so not pushnew
		(t
		 (pushnew (list a-p a-v) candidates :test #'equal))))))
    (let ((successes
	   (remove-if-not
	      #'(lambda (p-v)
		  (qa-w/o-work (first p-v)
			       (second p-v)
			       n))
	      candidates)))
      (if *qa-all-report-always-facts*
	  ;; Due to vars, some always facts may also be asserted elsewhere.
	  (delete-duplicates (nconc successes always) :test #'equal)
	;; Keep any re-asserted always facts.
	successes))))

(defun match-p (t1 t2)
  (let ((act::*bindings* nil))
    (oplan-obase:obmatch t1 t2)))

(defun old-qa-all (p v n)
  (let* ((tome-items (gen->list (tome-generator p v)))
	 (p-v-pairs
	  (mapcar #'(lambda (tome-item)
		      (list (tgm-tome-pattern
			     (oplan-psv:psv-actorise-pattern tome-item))
			    (oplan-psv:psv-actorise-pattern
			     (oplan-obase:value tome-item))))
		  tome-items))
	 (unique-p-v-pairs
	  (remove-duplicates p-v-pairs :test #'equal)))
    (remove-if-not
       #'(lambda (p-v)
	   (qa-w/o-work (first p-v)
			(second p-v)
			n))
       unique-p-v-pairs)))

(defun qa-w/o-work (p v n) ; -> true or false
  (let ((answer
	 (oplan-qa:qa p v n
	   (oplan-qa:qa-make-work-flag-structure :by-bindings nil nil))))
    (not (or (null answer)
	     (some-work-to-do-p answer)))))


;;;; Utilities

;;; /\/: TOME-GENERATOR should be in tgm.lisp.  In that file, VALUE,
;;; GENERATOR, and TRY-NEXT can be used w/o the package qualification.

(defun tome-generator (&optional (p '??) (v '??))
  (let ((gen (oplan-obase:generator `(TOME ,p ??) v)))
    #'(lambda ()
	(let ((item (oplan-obase:try-next gen)))
	  (loop
	    (cond ((null item)
		   ;; Gen has finished.
		   (return nil))
		  ((eq (oplan-obase:value item) :undef)
		   ;; Filter entries that have no value in the current
		   ;; context.
		   (setq item (oplan-obase:try-next gen)))
		  (t
		   (return item))))))))

;;; /\/: gen->list should be in obextra.lisp.

(defun gen->list (g)
  (do ((item (oplan-obase:try-next g)
	     (oplan-obase:try-next g))
       (result '()
	       (cons item result)))
      ((null item)
       (nreverse result))))

;;; End
