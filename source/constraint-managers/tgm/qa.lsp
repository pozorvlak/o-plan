;;;; File: qa.lsp
;;;; Contains: The QA code.
;;;; Author: Richard Kirby (rbk) and Jeff Dalton
;;;; Created: Tue Jun 26 11:54:43 1990
;;;; Updated: Thu Jul 10 19:19:55 1997 by Jeff Dalton
;;;; Copyright: (c) 1992, AIAI, University of Edinburgh

(in-package :oplan-qa)

(use-package :oplan-util)
(use-package :oplan-developerlib)
(use-package :oplan-obase)
(use-package :oplan-tgm)
(use-package :oplan-nodes)
(use-package :oplan-gop)
(use-package :oplan-psv)
(use-package :oplan-or-trees)

(import 'act::get-var-body)

(export '(qa qa-all qa-make-work-flag-structure qa-initialise))
(export '(qa-add-to-not-link-table))

(deftype work-flag-value ()
  '(member :BY-BINDINGS :LINK-WITH-BINDINGS :EXPAND))

(defvar *qa-p*)
(defvar *qa-v*)


;;; Mark vectors.

;;; /\/: "Vector" for historical reasons.

(defvar *qa-mark-vectors*
  (make-array (* 2 *estimated-max-nodes*) ;one entry per node-end
	      :initial-element nil))

(defstruct (qa-mark-vector (:conc-name mv-))
  (a-mark 0 :type fixnum)		;for the qa-mark algorithm
  (b-mark 0 :type fixnum)		;          "
  (c-mark 0 :type fixnum)		;          "
  (v-mark nil)				;visited by qa-mark[-parallel]-shadows
  (shadow-before-list nil)
  (shadow-after-list nil))

(defun-inline get-qa-mark-vector (node-end)
  (or (svref *qa-mark-vectors* (ne-index node-end))
      (setf (svref *qa-mark-vectors* (ne-index node-end))
	    (make-qa-mark-vector))))

(defun clear-qa-mark-vectors ()
  (fill *qa-mark-vectors* nil))


;;; Mark values

(proclaim '(fixnum *qa-before-mark* *qa-special-mark*
	           *qa-special-with-vars-mark* *qa-after-mark*))

(defvar *qa-before-mark* -3)
(defvar *qa-special-mark* -2)
(defvar *qa-special-with-vars-mark* -1)
(defvar *qa-after-mark* 0)

(defun get-new-qa-marks ()
  (when (> (node-end-count) (length *qa-mark-vectors*))
    (setq *qa-mark-vectors*
	  (make-array (round (* 3/2 (node-end-count)))
		      :initial-element nil)))
  (when (>= *qa-after-mark* (- most-positive-fixnum 4))
    (cerror "Continue anyway."
	    "Need to wrap QA marks.")
    (clear-qa-mark-info))
  (incf *qa-before-mark* 4)
  (incf *qa-special-mark* 4)
  (incf *qa-special-with-vars-mark* 4)
  (incf *qa-after-mark* 4)
  (check-type *qa-after-mark* fixnum))

(defun clear-qa-mark-info ()
  (clear-qa-mark-vectors)
  (setq *qa-before-mark* -3)
  (setq *qa-special-mark* -2)
  (setq *qa-special-with-vars-mark* -1)
  (setq *qa-after-mark* 0))


;; This is called to mark out all node-ends that are before a definite node-end
;; that is before the focus (ie last incoming node-ends with no bindings
;; required.

(defun qa-mark-a-definite-befores (node-end)
  "Marks the a and b marks of all node-ends before the node-end
   with the before mark."
  (declare (optimize (speed 3) (safety 0)))
  (dev-note :qa :debug "In QA-MARK-A-DEFINITE-BEFORES")
  (labels ((walk (ne)
	     (let ((mv (get-qa-mark-vector ne)))
	       (dev-note :qa :debug "Looking at ~W - ~W~" ne mv)
	       (unless (= (mv-b-mark mv)
			  *qa-before-mark*)
		 (setf (mv-a-mark mv) *qa-before-mark*)
		 (setf (mv-b-mark mv) *qa-before-mark*)
		 (dolist (p (ne-pre-ends ne))
		   (walk p))))))
    (assert (= (mv-a-mark (get-qa-mark-vector node-end)) *qa-before-mark*))
    (dolist (ne (ne-pre-ends node-end))
      (walk ne))
    (dev-note :qa :debug "Exiting QA-MARK-A-DEFINITE-BEFORES")))

;; This is called to add node-end to the shadows list of each special node-end
;; that is shadowed by this node-end.

(defun qa-mark-shadows (node-end)
  (declare (optimize (speed 3) (safety 0)))
  (dev-note :qa :debug "In QA-MARK-SHADOWS")
  (let ((node-end-marks (get-qa-mark-vector node-end))
	(visit-mark (list 'qa-mark-shadows-mark)))
    (labels
	((walk (ne)
	   (let ((mv (get-qa-mark-vector ne)))
	     (unless (eq (mv-v-mark mv) visit-mark)
	       (setf (mv-v-mark mv) visit-mark)
	       (dev-note :qa :debug "Looking at ~W - ~W" ne mv)
	       (unless (= (mv-b-mark mv) *qa-before-mark*)
	         ;; Not shadowed by a definite.
		 (let ((c-mark (mv-c-mark mv)))
		   (when (or (= c-mark *qa-special-mark*)
			     (= c-mark *qa-special-with-vars-mark*))
		     (push (ne-tag ne) (mv-shadow-before-list node-end-marks)))
		   (if (= c-mark *qa-special-mark*)
		       (progn
			 (setf (mv-a-mark mv) *qa-before-mark*)
			 (qa-mark-a-definite-befores ne))
		     (dolist (p (ne-pre-ends ne))
		       (walk p)))))))))
      (assert (= (mv-a-mark node-end-marks) *qa-before-mark*))
      (dolist (ne (ne-pre-ends node-end))
	(walk ne))
      (dev-note :qa :debug "Exiting QA-MARK-SHADOWS"))))

;; This is called to add node-end to the shadows list of each special node-end
;; that is shadowed by this node-end both forwards and backwards.

(defun qa-mark-parallel-shadows (node-end)
  (declare (optimize (speed 3) (safety 0)))
  (dev-note :qa :debug "In QA-MARK-PARALLEL-SHADOWS")
  (let ((node-end-marks (get-qa-mark-vector node-end))
	(visit-mark (list 'backwards-visit-mark)))
    ;; Go backwards.
    (label walk-before ((prenode-ends (ne-pre-ends node-end)))
      (dolist (prenode-end prenode-ends)
	(let ((prenode-end-marks (get-qa-mark-vector prenode-end)))
	  (unless (eq visit-mark (mv-v-mark prenode-end-marks))
	    (let ((c-mark (mv-c-mark prenode-end-marks)))
	      (dev-note :qa :debug "Looking at ~W - ~W"
			prenode-end prenode-end-marks)
	      (if (or (= c-mark *qa-special-mark*)
		      (= c-mark *qa-special-with-vars-mark*))
		  (push (ne-tag prenode-end)
			(mv-shadow-before-list node-end-marks)))
	      (setf (mv-v-mark prenode-end-marks) visit-mark)
	      (walk-before (ne-pre-ends prenode-end)))))))
    ;; Go forwards.
    (setq visit-mark (list 'forwards-visit-mark))
    (label walk-after ((postnode-ends (ne-post-ends node-end)))
      (dolist (postnode-end postnode-ends)
	(let ((postnode-end-marks (get-qa-mark-vector postnode-end)))
	  (unless (eq visit-mark (mv-v-mark postnode-end-marks))
	    (let ((c-mark (mv-c-mark postnode-end-marks)))
	      (dev-note :qa :debug "Looking at ~W - ~W"
			postnode-end postnode-end-marks)
	      (if (or (= c-mark *qa-special-mark*)
		      (= c-mark *qa-special-with-vars-mark*))
		  (push (ne-tag postnode-end)
			(mv-shadow-after-list node-end-marks)))
	      (setf (mv-v-mark postnode-end-marks) visit-mark)
	      (walk-after (ne-post-ends postnode-end)))))))
    (dev-note :qa :debug "Exiting QA-MARK-PARALLEL-SHADOWS")))

(defun qa-mark (focus special-list)
  (declare (optimize (speed 3) (safety 0)))
  (dev-note :qa :debug "In QA-MARK")
  (let ((focus-s (get-node-end focus)))
    (get-new-qa-marks)
    ;; Mark the C mark of each special node-end with either *qa-special-mark*
    ;; or *qa-special-with-vars-mark* depending on whether bindings are
    ;; required or not.
    (dolist (special special-list)
      (let ((node-end (tgm-tome-node-end (tgm-tome-entry special))))
	(unless (eql node-end :ALWAYS)
	  (setf (mv-c-mark (get-qa-mark-vector (get-node-end node-end)))
		(if (tgm-tome-bindings special)
		    *qa-special-with-vars-mark*
		    *qa-special-mark*)))))
    ;; Sort out before marking.
    (label walk-before ((prenode-ends (ne-pre-ends focus-s)))
      (dolist (consider prenode-ends)
	(let ((marks (get-qa-mark-vector consider)))
	  (dev-note :qa :debug "Looking at ~W - ~W" consider marks)
	  (unless (= (mv-a-mark marks) *qa-before-mark*)
	    (setf (mv-a-mark marks) *qa-before-mark*)
	    (if (= (mv-c-mark marks) *qa-special-mark*)
		(qa-mark-a-definite-befores consider)
	      (progn
		(if (= (mv-c-mark marks)
		       *qa-special-with-vars-mark*)
		    (qa-mark-shadows consider))
		(walk-before (ne-pre-ends consider))))))))
    ;; Sort out after marking.
    (label walk-after ((postnode-ends (ne-post-ends focus-s)))
      (dolist (consider postnode-ends)
	(let ((marks (get-qa-mark-vector consider)))
	  (dev-note :qa :debug "Looking at ~W - ~W" consider marks)
	  (unless (= (mv-a-mark marks) *qa-after-mark*)
	    (setf (mv-a-mark marks) *qa-after-mark*)
	    (walk-after (ne-post-ends consider))))))
    ;; The parallel node-ends have their a marks < *qa-before-mark*. Now for
    ;; each parallel special we need to build up the before and after lists of
    ;; shadowed node-ends.
    (dolist (special special-list)
      (let ((node-end (tgm-tome-node-end (tgm-tome-entry special)))
	    marks)
	(unless (eql node-end :ALWAYS)
	  (setq node-end (get-node-end node-end))
	  (setq marks (get-qa-mark-vector node-end))
	  (if (< (mv-a-mark marks) *qa-before-mark*)
	      (qa-mark-parallel-shadows node-end)))))
    (dev-note :qa :debug "Exiting QA-MARK")))

;; This routine splits up the special-plus and special-minus lists into the
;; twelve different types of contributors and deletors, and records the
;; necessary links to make.
(defun qa-build-contributors-and-deletors (focus special-plus special-minus)
  (let (always-plus always-plus-with-bindings
		    before-plus before-plus-with-bindings
		    parallel-plus parallel-plus-with-bindings
		    always-minus always-minus-with-bindings
		    before-minus before-minus-with-bindings
		    
		    parallel-minus parallel-minus-with-bindings)
    (dolist (special special-plus)
      (let ((node-end (tgm-tome-node-end (tgm-tome-entry special)))
	    marks)
	(if (eql node-end :ALWAYS)
	    (if (tgm-tome-bindings special)
		(push special always-plus-with-bindings)
		(push special always-plus))
	    ;; Ignore effects at the condition node end.
	    (unless (equal node-end focus)
	      (setq marks (get-qa-mark-vector (get-node-end node-end)))
	      (unless (= (mv-b-mark marks) *qa-before-mark*)
		(setf (tgm-contributor-rules-out-list special)
		      (mv-shadow-before-list marks))
		(cond ((= (mv-a-mark marks) *qa-before-mark*)
		       (if (= (mv-c-mark marks) *qa-special-mark*)
			   (push special before-plus)
			   (push special before-plus-with-bindings)))
		      ((< (mv-a-mark marks) *qa-before-mark*)
		       ;; Indicate that to make this parallel node-end a
		       ;; contributor, need to link it before the focus.
		       (setf (tgm-contributor-link special) t)
		       (if (= (mv-c-mark marks) *qa-special-mark*)
			   (push special parallel-plus)
			   (push special parallel-plus-with-bindings)))))))))
    
    (dolist (special special-minus)
      (let ((node-end (tgm-tome-node-end (tgm-tome-entry special)))
	    marks)
	(if (eql node-end :ALWAYS)
	    (if (tgm-tome-bindings special)
		(push special always-minus-with-bindings)
		(push special always-minus))
	    ;; Ignore effects at the condition node end.
	    (unless (equal node-end focus)
	      (setq marks (get-qa-mark-vector (get-node-end node-end)))
	      (unless (= (mv-b-mark marks) *qa-before-mark*)
		(setf (tgm-deletor-before-rules-out-list special)
		      (mv-shadow-before-list marks))
		(setf (tgm-deletor-after-rules-out-list special)
		      (mv-shadow-after-list marks))
		(cond ((= (mv-a-mark marks) *qa-before-mark*)
		       ;; Indicate that to rule out this definite before
		       ;; deletor node-end, need to link before a contributor.
		       (setf (tgm-deletor-link-before special) t)
		       (if (= (mv-c-mark marks) *qa-special-mark*)
			   (push special before-minus)
			   (push special before-minus-with-bindings)))
		      ((< (mv-a-mark marks) *qa-before-mark*)
		       ;; Indicate that to rule out this parallel deletor
		       ;; node-end, need to link before a contributor or after
		       ;; the focus.
		       (setf (tgm-deletor-link-before special) t)
		       (setf (tgm-deletor-link-after special) t)
		       (if (= (mv-c-mark marks) *qa-special-mark*)
			   (push special parallel-minus)
			   (push special parallel-minus-with-bindings)))))))))
    ;; Reset.
    (dolist (special special-plus)
      (let ((node-end (tgm-tome-node-end (tgm-tome-entry special)))
	    marks)
	(unless (eql node-end :ALWAYS)
	  (setq marks (get-qa-mark-vector (get-node-end node-end)))
	  (setf (mv-shadow-before-list marks) nil)
	  (setf (mv-shadow-after-list marks) nil))))
    (dolist (special special-minus)
      (let ((node-end (tgm-tome-node-end (tgm-tome-entry special)))
	    marks)
	(unless (eql node-end :ALWAYS)
	  (setq marks (get-qa-mark-vector (get-node-end node-end)))
	  (setf (mv-shadow-before-list marks) nil)
	  (setf (mv-shadow-after-list marks) nil))))
    
    (values always-plus always-plus-with-bindings
	    before-plus before-plus-with-bindings
	    parallel-plus parallel-plus-with-bindings
	    always-minus always-minus-with-bindings
	    before-minus before-minus-with-bindings
	    parallel-minus parallel-minus-with-bindings)))

(defun qa-make-true (focus special-plus special-minus work-flag)
  (declare (type work-flag-value work-flag)
	   (ignore work-flag))
  ;; Check for a trivial case.
  (if (null special-plus) (return-from qa-make-true nil))
  
  ;; Work out the various relationships and shadowing.
  (qa-mark focus (append special-plus special-minus))
  ;; Build up the eight lists.
  (multiple-value-bind (always-plus always-plus-with-bindings
				    before-plus before-plus-with-bindings
				    parallel-plus parallel-plus-with-bindings
				    always-minus always-minus-with-bindings
				    before-minus before-minus-with-bindings
				    parallel-minus
				    parallel-minus-with-bindings)
      (qa-build-contributors-and-deletors focus special-plus special-minus)
    ;; Check for trivial cases.
    (if always-plus (return-from qa-make-true (cons always-plus nil)))
    (if always-minus (return-from qa-make-true nil))
    (unless (or always-plus always-plus-with-bindings
		before-plus before-plus-with-bindings
		parallel-plus parallel-plus-with-bindings)
      (return-from qa-make-true nil))
    (if (and before-plus (null special-minus))
	(return-from qa-make-true (cons before-plus nil)))
    (if (and before-plus (null before-minus) (null before-minus-with-bindings)
	     (null parallel-minus) (null parallel-minus-with-bindings))
	(return-from qa-make-true (cons before-plus nil)))
    
    (dev-note :qa :detail "Before plus - ~W~%   Before plus with bindings - ~
~W~%   Parallel plus - ~W~%   Parallel plus with bindings - ~W"
			    before-plus before-plus-with-bindings
			    parallel-plus parallel-plus-with-bindings)
    (dev-note :qa :detail "Before minus - ~W~%   Before minus with bindings ~
- ~W~%   Parallel minus - ~W~%   Parallel minus with bindings - ~W"
			    before-minus before-minus-with-bindings
			    parallel-minus parallel-minus-with-bindings)
    ;; Now return the two lists.
    (cons (nconc always-plus before-plus always-plus-with-bindings
		 before-plus-with-bindings
		 parallel-plus parallel-plus-with-bindings)
	  (nconc before-minus always-minus-with-bindings
		 before-minus-with-bindings
		 parallel-minus parallel-minus-with-bindings))))

;;; QA not-link code

(defvar *qa-not-link-table* (make-hash-table :test #'equal))

(defun qa-not-link-entry (key)
  (oplan-ctxt:deref-in-context (gethash key *qa-not-link-table*)))

(defsetf qa-not-link-entry (key) (new-value)
  `(progn (oplan-ctxt:update-in-context
	   #'(lambda (k v)
	       (setf (gethash k *qa-not-link-table*) v))
	   #'(lambda (k) (gethash k *qa-not-link-table*))
	   ,key
	   ,new-value)
    ,new-value))

;; Returns t if okay, else nil.
(defun qa-add-to-not-link-table (from to)
  (unless (oplan-gop:gop-precedes from to)
    ;;    (break "qa-add-to-not-link-table")
    (setf (qa-not-link-entry (cons from to)) t)))

#+:undef
(defun qa-not-link-p (from to)
  (let ((result (qa-not-link-entry (cons from to))))
    ;;    (break "qa-not-link-p: result = ~S" result)
    (eql result t)))

(defun qa-not-link-p (from to)
  (declare (ignore from to))
  nil)

;;; New definition of build-or-tree-for-deletors-given-contributor to
;;; take into account the checking for not-links. Have to include the
;;; rest since this is a lexical closure.

;;; Lexically scope these variables across the following three (?)
;;; functions. These are used to work out the satisfaction method used
;;; for each contributor.

;;; /\/: No longer a lexical closure.  The AKCL compiler doesn't normally
;;; compile functions that are defined inside a LET.  [jwd 1 feb 93]

; (let (!need-bindings-p! !need-linkings-p!)

(defvar !need-bindings-p! nil)
(defvar !need-linkings-p! nil)
  
  (defun work-out-satisfaction-method ()
    (if !need-linkings-p!
	(if !need-bindings-p!
	    :LINK-WITH-BINDINGS
	    :LINK-NO-BINDINGS)
	(if !need-bindings-p!
	    :BY-BINDINGS
	    :ALREADY-SATISFIED)))
  
  (defun make-link-action (from to)
    (list :LINK from to))
  
  (defun make-bind-action (tuple)
    (list :BIND (var-name tuple) (var-deref-value tuple)))
  
  (defun make-restrict-action (tuple)
    (list :RESTRICT (var-name tuple) (var-deref-value tuple)))
  
  (defun make-update-action (contributor)
    (list :UPDATE (list (cons contributor (work-out-satisfaction-method)))))
  
  ;; Returns either a contributor or-branch structure, or nil if more work is
  ;; needed to make this a contributor than allowed for by the work-flag.
  ;;
  (defun make-or-branch-for-contributor (contributor condition-node-end
						       work-flag)
    (declare (type work-flag-value work-flag))
    (let ((contributor-node-end
	   (tgm-tome-node-end (tgm-tome-entry contributor)))
	  satisfaction-method actions)
      (setq !need-bindings-p! (tgm-tome-bindings contributor))
      (setq !need-linkings-p! (tgm-contributor-link contributor))
      (setq satisfaction-method (work-out-satisfaction-method))
      (when (<= (tgm-get-satisfaction-method-level satisfaction-method)
		(tgm-get-satisfaction-method-level work-flag))
	;; When the work required to make this a contributor is compatible with
	;; the work allowed by the condition type.
	(mapc #'(lambda (x)
		  (unless (eql (var-value x) :undef)
		    (push (make-bind-action x) actions)))
	      (tgm-tome-bindings contributor))
	(if !need-linkings-p!
	    ;; If not-link then abort this contributor.
	    (if (qa-not-link-p contributor-node-end condition-node-end)
		(return-from make-or-branch-for-contributor nil)
		(push
		 (make-link-action contributor-node-end condition-node-end)
		 actions)))
	(make-or-branch :actions actions))))
  
  (defun pick-next-deletor (contributor-node-end deletors)
    ;; Simple at the moment.
    (declare (ignore contributor-node-end))
    (values (car deletors) (cdr deletors)))

  (defun make-and (or-branch or-tree)
    (assert (null (or-branch-subtree or-branch)))
    (setf (or-branch-subtree or-branch) or-tree)
    or-branch)

  ;; Returns either an or-tree for suggestions of how to remove all the
  ;; deletors given the contributor, or nil if any of the deletors require
  ;; more work than allowed for by the work-flag.
  ;;
  (defun build-or-tree-for-deletors-given-contributor
      (contributor condition-node-end deletors work-flag)
    (declare (type work-flag-value work-flag))
    (let ((contributor-node-end (tgm-tome-node-end
				 (tgm-tome-entry contributor))))
      (multiple-value-bind
	    (deletor deletors)
	  (pick-next-deletor contributor-node-end deletors)
	(let ((deletor-node-end (tgm-tome-node-end (tgm-tome-entry deletor)))
	      (deletor-precedes-list
	       (tgm-deletor-before-rules-out-list deletor))
	      (deletor-follows-list
	       (tgm-deletor-after-rules-out-list deletor))
	      next-deletors
	      sub-result
	      result)

	  ;; For a deletor, go through the various ways of removing it from the
	  ;; range, and AND it with the ways for removing the other deletors.
	  (unless (eql work-flag :BY-BINDINGS)

	    ;; 1: Consider linking after the condition (if this option is
	    ;; available, ie this deletor is in parallel with the condition).
	    (when (and (tgm-deletor-link-after deletor)
		       (not (equal condition-node-end deletor-node-end))
		       (not (qa-not-link-p condition-node-end
					   deletor-node-end))
		       (gop-link-looks-ok-p condition-node-end
					    deletor-node-end))
	      (setq !need-linkings-p! t)
	      (setq next-deletors
		    (remove-if #'(lambda (x)
				   ;; Check if no need to consider this
				   ;; deletor.
				   (member (tgm-tome-node-end
					    (tgm-tome-entry x))
					   deletor-follows-list :test #'equal))
			       deletors))
	      (if next-deletors
		  (progn
		    (setq sub-result
			  (build-or-tree-for-deletors-given-contributor
			   contributor condition-node-end
			   next-deletors work-flag))
		    (if sub-result
			(push (make-and
			        (make-or-branch
				  :actions
				    (list
				     (make-link-action condition-node-end
						       deletor-node-end)))
				sub-result)
			      result)
		      ;; No way to remove at least one of the other deletors.
		      (return-from
		        build-or-tree-for-deletors-given-contributor
			 nil)))
		;; At the bottom of the OR tree, so add the :UPDATE action
		;; for this contributor.
		(push
		  (make-and
		    (make-or-branch
		      :actions
			(list (make-link-action condition-node-end
						deletor-node-end)
			      #+:undef
			      (make-update-action contributor-node-end)))
		    nil)
		  result)))
	    
	    ;; 2: Consider linking before the contributor (if this option is
	    ;; available ie this deletor doesn't follow the contributor).
	    (unless (or (member contributor-node-end deletor-precedes-list
				:test #'equal)
			(equal deletor-node-end contributor-node-end)
			(qa-not-link-p deletor-node-end contributor-node-end)
			(not (gop-link-looks-ok-p deletor-node-end
						  contributor-node-end)))
	      (setq !need-linkings-p! t)
	      (setq next-deletors
		    (remove-if #'(lambda (x)
				   ;; Check if no need to consider this
				   ;; deletor.
				   (member (tgm-tome-node-end
					    (tgm-tome-entry x))
					   deletor-precedes-list
					   :test #'equal))
			       deletors))
	      (if next-deletors
		  (progn
		    (setq sub-result
			  (build-or-tree-for-deletors-given-contributor
			   contributor condition-node-end
			   next-deletors work-flag))
		    (if sub-result
			(push (make-and
			        (make-or-branch
				  :actions
				    (list
				     (make-link-action deletor-node-end
						       contributor-node-end)))
				sub-result)
			      result)
		      ;; No way to remove at least one of the other deletors.
		      (return-from
		        build-or-tree-for-deletors-given-contributor
			 nil)))
		;; At the bottom of the OR tree, so add the :UPDATE action
		;; for this contributor.
		(push
		  (make-and
		    (make-or-branch
		      :actions
		        (list
			 (make-link-action deletor-node-end
					   contributor-node-end)
			 #+:undef
			 (make-update-action contributor-node-end)))
		    nil)
		  result))))
	  
	  (if (and (eql work-flag :BY-BINDINGS)
		   (null (tgm-tome-bindings deletor)))
	      ;; The work-flag does not allow the linking out of a deletor, but
	      ;; there are no bindings to remove this deletor, and so we have
	      ;; to fail for this contributor.
	      (return-from build-or-tree-for-deletors-given-contributor nil)

	      ;; 3: Consider restricting any bindings first, unless needed for
	      ;; making contributors, or the deletor is at the contributor in
	      ;; which case we can ignore.
	      (let ((contributor-bindings (tgm-tome-bindings contributor)))
		(dolist (restrict (tgm-tome-bindings deletor))
		  (unless (or (eql (var-value restrict) :undef)
			      (not (or (pattern-contains-var-p
					 *qa-p* (var-name restrict))
				       (pattern-contains-psv-p
					 (tgm-tome-pattern
					   (tgm-tome-entry deletor))
					 (var-name restrict))))
			      (and
			       (member (var-name restrict) contributor-bindings
				       :key #'var-name)
			       ;; If this is true, then it means that the
			       ;; restriction would rule out a binding required
			       ;; for the contributor, so we can not restrict
			       ;; this binding.
			       (oplan-obase:obmatch3
				(act:var-really-deref-value restrict)
				(act:var-really-deref-value
				 (find (var-name restrict)
				       contributor-bindings
				       :key #'var-name))
				(copy-tree contributor-bindings))))
		    (setq !need-bindings-p! t)
		    (setq next-deletors
			  (remove-if
			    #'(lambda (d)
				(restriction-handles-deleter-p restrict d))
			    deletors))
		    (if next-deletors
			(progn
			  (setq sub-result
				(build-or-tree-for-deletors-given-contributor
				 contributor condition-node-end
				 next-deletors work-flag))
			  (if sub-result
			      (push (make-and
				      (make-or-branch
				        :actions
					  (list
					   (make-restrict-action restrict)))
				      sub-result)
				    result)
			    ;; No way to remove at least one of the other
			    ;; deletors.
			    (return-from
			      build-or-tree-for-deletors-given-contributor
			       nil)))
		      ;; At the bottom of the OR tree, so add the :UPDATE
		      ;; action for this contributor.
		      (push
		        (make-and
			  (make-or-branch
			    :actions
			      (list (make-restrict-action restrict)
				    #+:undef
				    (make-update-action
				     contributor-node-end)))
			  nil)
			result))))))
	  (new-or-tree result)))))

(defun restriction-handles-deleter-p (restrict d)
  (let ((var (var-name restrict))
	(val (var-deref-value restrict)))
    (dolist (b (tgm-tome-bindings d) nil)
      (when (and (eq (var-name b) var)
		 (eql (var-deref-value b) val))
	(dev-warn "~S not ~S kills ~S = ~S."
	      var
	      val
	      (tgm-tome-entry d)
	      (tgm-tome-val d))
	(return t)))))

(defun same-restriction-p (r1 r2)
  ;; r1 and r2 are "tuples" from a binding a-list
  (and (eq (var-name r1) (var-name r2))
       (eql (var-deref-value r1) (var-deref-value r2))))

  (defun build-and-or-tree (qa-result condition-node-end work-flag)
    (declare (type work-flag-value work-flag))
    (let ((contributors (car qa-result))
	  (deletors (cdr qa-result))
	  next-deletors
	  con-result
	  del-result)
     (new-or-tree
      (mapcan
        #'(lambda (contributor)
	    (setq !need-bindings-p! nil !need-linkings-p! nil)
	    (let ((contributor-shadows-list
		   (tgm-contributor-rules-out-list contributor))
		  (contributor-node-end
		   (tgm-tome-node-end (tgm-tome-entry contributor))))
	      (setq con-result (make-or-branch-for-contributor
				contributor condition-node-end work-flag))
	      (when con-result
		(setq next-deletors
		      (remove-if
		        #'(lambda (x)
			    ;; Check if no need to consider this deletor.
			    (or (member (tgm-tome-node-end (tgm-tome-entry x))
					contributor-shadows-list
					:test #'equal)
				(contributor-binds-out-deleter-p
				  contributor x)
				(contributor-equals-deleter-p
				  contributor x)))
			deletors))
		(if next-deletors
		    (progn
		      (setq del-result
			    (build-or-tree-for-deletors-given-contributor
			     contributor condition-node-end
			     next-deletors work-flag))
		      (when del-result
			;; /\/: Copied up from below.  Always want
			;; the update at the top of the tree.
			(push (make-update-action contributor-node-end)
			      (or-branch-actions con-result))
			(list (make-and con-result del-result))))
		  (progn
		    ;; There are no deletors to consider so add the
		    ;; update to the contributors' actions.
		    (push (make-update-action contributor-node-end)
			  (or-branch-actions con-result))
		    (list (make-and con-result nil)))))))
	contributors))))

(defun contributor-equals-deleter-p (contributor deleter)
  (and (equal (tgm-tome-entry contributor)
	      (tgm-tome-entry deleter))
       (equal (tgm-tome-val contributor)
	      (tgm-tome-val deleter))))

(defun contributor-binds-out-deleter-p (contributor deleter)
  (let ((c-binds (tgm-tome-bindings contributor))
	(d-binds (tgm-tome-bindings deleter)))
    ;; Consider only variables that appear in the condition pattern,
    ;; not the value.
    (setq c-binds
	  (remove-if-not
	    #'(lambda (c-bind)
		(pattern-contains-var-p *qa-p* (var-name c-bind)))
	    c-binds))
    (dolist (c-bind c-binds)
      (let ((c-var (var-name c-bind)))
	;; See if the deleter has a view of the same variable.
	(dolist (d-bind d-binds)
	  (let ((d-var (var-name d-bind)))
	    (when (eq d-var c-var)
	      ;; Yes, it does.  So compare them.
	      (let ((c-val (var-deref-value c-bind))
		    (d-val (var-deref-value d-bind)))
		(cond ((c-val-could-be-d-val-p c-val d-val c-binds)
		       ;; This c-bind does nothing -- try the next
		       (return))
		      (t
		       ;; The c-bind automatically rules out the deleter
		       (dev-warn "~S = ~S kills ~S = ~S for ~S = ~S."
			     c-var
			     c-val
			     d-var
			     d-val
			     (tgm-tome-entry deleter)
			     (tgm-tome-val deleter))
		       (return-from contributor-binds-out-deleter-p
				    t)))))))))
    ;; If we get here, the deleter has survived
    nil))

(defun pattern-contains-var-p (pat target-name)
  (label scan ((p pat))
    (cond ((oplan-obase::given-p p)
	   (let ((var-name (car (actargs p))))
	     (eq var-name target-name)))
	  ((consp p)
	   (or (scan (car p))
	       (scan (cdr p))))
	  (t
	   nil))))

(defun pattern-contains-psv-p (pat target-name)
  (and (psv-p target-name)
       (label scan ((p pat))
	 (cond ((eq p target-name)
		t)
	       ((consp p)
		(or (scan (car p))
		    (scan (cdr p))))
	       (t
		nil)))))

(defun c-val-could-be-d-val-p (c-val d-val *bindings*)
  (cond ((eql c-val d-val)
	 t)
	((eq c-val :undef)
	 t)
	((eq d-val :undef)
	 t)
	((consp c-val)
	 (dev-warn "c-val = ~S" c-val)
	 t)				;/\/ for now
	((consp d-val)
	 (dev-warn "d-val = ~S" d-val)
	 t)				;/\/ for now
	((psv-p c-val)
	 (if (psv-p d-val)
	     (psv-can-match-psv-p c-val d-val)
	   (psv-can-have-value-p c-val d-val)))
	((psv-p d-val)
	 (psv-can-have-value-p d-val c-val))
	(t
	 nil)))

;  ) ; End of lexical closure.

(defun qa-make-work-flag-structure (flag contributors deletors
					 &optional contributors-to-ignore)
  (list* flag contributors deletors contributors-to-ignore))

(defun qa-explode-work-flag-structure (work-flag)
  (values (car work-flag) (cadr work-flag)
	  (caddr work-flag) (cdddr work-flag)))

(defun qa (p v n work-flag)
  (dev-note :qa :detail "QA called for ~W = ~W @ ~W" p v n)
  (let ((*qa-p* p)
	(*qa-v* v)
	qa-raw-result)
    (multiple-value-bind
	  (flag given-contributors given-deletors contributors-to-ignore)
	(qa-explode-work-flag-structure work-flag)
      (multiple-value-bind
	    (contributors deletors)
	  (tgm-find-contributors-and-deletors p v
					      (null given-contributors)
					      (null given-deletors))
	(when given-contributors
	  ;; We have been passed a set of contributors from the value of the
	  ;; GOST entry, so convert into contributor record format.
	  ;; IE we have been called from tgm-add-effect. This will not be
	  ;; needed when tgm-add-effect is rewritten to not need QA.
	  (setq contributors
		(mapcar #'(lambda (x)
			    (tgm-make-contributor-record
			     (tgm-make-tome-entry p (car x))
			     v
			     nil))
			given-contributors)))
	(if contributors-to-ignore
	    (setq contributors
		  (remove-if #'(lambda (x)
				 (when (member x contributors-to-ignore
					       :test #'equal)
				   (dev-note :qa :detail "Throwing away ~
possible contributor:~%~W~%because we have been told to forget it."
							x)
				   t))
			     contributors
			     :key #'(lambda (x)
				      (tgm-tome-node-end
				       (tgm-tome-entry x))))))
	(if given-deletors (setq deletors given-deletors))
	(setq qa-raw-result (qa-make-true n contributors deletors flag))
	(dev-note :qa :debug "QA raw result =~%~W" qa-raw-result)
	(when (consp qa-raw-result)
	  (setq qa-raw-result (build-and-or-tree qa-raw-result n flag))
	  (when given-contributors
	    ;; Now we need to replace the satisfaction tactics of the
	    ;; contributors to reflect the GOST entries.
	    (dolist (contributor (or-tree-branches qa-raw-result))
	      (mapc #'(lambda (x) (setf (cdr x)
					(cdr (assoc (car x) given-contributors
						    :test #'equal))))
		    ;; /\/: Don't mess with it if there were deleters
		    ;; that had to be handled.  I don't know if this is
		    ;; right, but it kept the GOSTs the same as before
		    ;; the change that put the :update action always at
		    ;; the top of the or-tree.  [jd 02 feb 96]
		    (unless (or-branch-subtree contributor)
		      (second (find :UPDATE (or-branch-actions
					     contributor)
				    :key #'car)))))))
	
	(dev-note :qa :detail "QA result = ~%~W" qa-raw-result)
	qa-raw-result))))

(define-initializer :dm qa-initialise ()
  (clear-qa-mark-info)
  (clrhash *qa-not-link-table*))

;;;; ---------------------------- Change History ----------------------------
;;;; (Initials) (Date of change)           (Comment)
