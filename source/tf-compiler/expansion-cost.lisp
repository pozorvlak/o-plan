;;;; File: expansion-cost.lsp
;;; Contains: Further TF compiler domain analysis
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: July 1994
;;; Updated: Mon Jun  7 00:46:24 1999 by Jeff Dalton
;;; Copyright: (c) 1994, AIAI, University of Edinburgh

(in-package :oplan-tf-compiler)

(define-mapping schema min-expansion)
(define-mapping schema successor-sets)

(defun construct-min-expansion-table (domain)
  (construct-schema-successor-set-table domain)
  (dolist (s (domain-schemas domain))
    (unless (schema->min-expansion s)
      (min-full-expand s #'schema->successor-sets))))

(defun show-schema-min-expansions ()
  (dolist (s (domain-schemas *domain*))
    (format t "~S -> ~S~%"
	    (schema-name s)
	    (schema->min-expansion s))))

;;; (min-full-expand s successor-set-fn) --
;;;
;;; Suppose s is a schema.  Each node in the expansion of s leads to
;;; a set of schemas that can be used to expand the node.  Call each
;;; set a "successor set".  The min # of nodes in a full expansion of
;;; s is the # of nodes in the immediate expansion of s (= the # of s-sets)
;;; plus the sum over the s-sets of the min # of nodes in a full expansion
;;; of the best choice from each s-set.

(defparameter *pseudo-infinity* 10000000000)

(defun min-full-expand (s ssfn)
  (let ((marks (make-hash-table :test #'eq)))
    (macrolet ((mark (s) `(gethash ,s marks))
	       (size (s) `(schema->min-expansion ,s)))
      (labels
	  ((find-size (at)
	     (ecase (mark at)
	       ((:start)
		(warn "Cycle at ~S" at)
		*pseudo-infinity*)
	       ((:finish)
		(size at))
	       ((nil)
		(setf (mark at) :start)
		(let ((s-sets (funcall ssfn at)))
		  (setf (size at)
			(if (null s-sets)
			    0
			    (+ (length s-sets)
			       (reduce #'+ (mapcar #'s-set-size s-sets)))))
		  (setf (mark at) :finish)
		  (size at)))))
	   (s-set-size (s-set)
	     (if (null s-set)
		 0
	       (reduce #'min (mapcar #'find-size s-set)))))
	;; So do it...
	(find-size s)))))

(defun construct-schema-successor-set-table (domain)
  (setf (mapping schema successor-sets)
	(compose-maps
	  (domain-schemas domain)
	  #'(lambda (s)
	      (mapcar #'expand-name->schemas
		      (all-schema-node-action-names s))))))

(defun all-schema-node-action-names (s)
  ;; Like get-schema-node-action-names but retains duplicates.
  (loop for n in (schema-nodes s)
	when (eq (node-type n) 'action)
	collect (car (node-pattern n))))

;;; End

