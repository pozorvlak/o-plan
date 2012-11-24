language lisp;

;;; Agenda priority function for SOCAP demo.

(in-package :oplan)

; /\/: Only the DM has the context for looking up node structs,
; and the code below runs in the AM.  Otherwise, we could just
; use the length of (n-numbers node) instead of counting "-"s.

(defun node-tag->level (node-tag)
  ;; Sample tag: node-3-9-8
  (count #\- (symbol-name node-tag)))

(defun-for-domain assign-agenda-priority (ag)
  (let ((old-priority (get-agenda-priority ag)))
    (setf (ag-priority ag)
	  (new-priority ag old-priority))))

(defun new-priority (ag old-priority)
  (cond ;; Expand of level <= 3
        ((and (eq (first (ag-body ag)) ':expand)
	      (<= (node-tag->level (second (ag-body ag)))
		  2))
	 53)
	;; Unsupervised for level <= 3
	((and (eq (first (ag-body ag)) ':condition)
	      (let* ((cond (second (ag-body ag)))
		     (at-node-end (con-at-node-end cond)))
		(and (eq (con-type cond) 'unsupervised)
		     (<= (node-tag->level
			  (etag-node at-node-end))
			 3))))
	 52)
	((eq (first (ag-body ag)) ':bind)
	 51)
        (t
         old-priority)))

end_language;
