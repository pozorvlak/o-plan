;;;; File: KS-OR.lisp
;;; Contains: Handles AND/OR interaction tree.
;;; Author: Richard Kirby and Jeff Dalton
;;; Created: Wed Nov 27 15:56:27 1991
;;; Updated: Sun Jun 20 01:06:13 1999 by Jeff Dalton
;;; Copyright: (c) 1992, 1993, 1994, AIAI, University of Edinburgh

(in-package :oplan-knowledge-source)

(use-package :oplan-QA)

#| This handles the AND/OR linkage/binding tree returned from the TGM when
adding effects or conditions. Basically it selects a choice at the top-level,
posts KS-LINKs and KS-BINDs for the linkages and/or bindings required, a KS-OR
to handle the next level (if there is one), and an alternative to handle any
other choices.

/\/: No it doesn't.  It directly asks the DM to bind, restrict, link, etc.
rather than posting KS-LINKs and KS-BINDs.  KS-LINK doesn't even exist.  [JD]
|#

(defparameter *trim-in-ks-or* nil)

(defun KS-OR (ag)
  (STAGE-MANAGER
   nil					;no state vars
   (body cond satisfy-p or-tree contribs
    chosen-branch branches initial-n-branches)
   ag
   
   (setq body (cdr (ag-body ag)))
   (setq cond (first body))
   (setq contribs (second body))
   (setq satisfy-p (third body))
   (setq or-tree (cdddr body))
   ;; /\/ Experiment:
   #+:undef
   (when (and or-tree *trim-in-ks-or*)
     (setq or-tree (db-call 'trim-or-tree or-tree)))
   (when (null or-tree)
     (post-agenda '(:poison-state :empty-or-tree))
     (return-from KS-OR nil))
   ;; /\/
   (setq branches (or-tree-branches or-tree))
   ;; /\/ Was: (setq initial-n-branches (length branches))
   (setq initial-n-branches (ag-branch-1 ag))

   (assert (&= initial-n-branches (ag-branch-1 ag)))
   ;; /\/ Was: (assert (= initial-n-branches (or-tree-branch-1 or-tree)))

   (when (and (not (eq satisfy-p :not-again))
	      (null contribs)
	      (not (eq (con-type cond) 'achievable))
	      (not (eq (con-type cond) 'only_use_for_query)))
     (return-from KS-OR
       (simple-ks-or ag cond satisfy-p or-tree)))

   (loop

      (when (null branches)
	(return))

      ;; A list of alternatives, so take one and post the rest back.
      (multiple-value-bind
	    (choice rest) (pick-from-next-level branches)
	(when (and rest (not (eql satisfy-p :NOT-AGAIN)))
	  (with-ag-copy (ag ag)
	    (setf (ag-body ag)
		  `(:OR ,cond
		        ,contribs
		        ,satisfy-p	;was :NOT-AGAIN
		        .,(new-or-tree rest)))
	    (setf (ag-branch-1 ag) (length rest))
	    (post-alternatives ag)))
	(setq chosen-branch choice))
   
      (whats-going-on "Doing the following:")
   
      (if (null satisfy-p)
	  (if (or (and (eql (con-type cond) 'achievable)
		       (every #'(lambda (x)
				  (eql (cdr x) :already-satisfied))
			      contribs))
		  ;; This OR-tree posted to resolve an interaction with an
		  ;; achievable which was already-satisfied. So what we do here
		  ;; is set up an alternative where the opposite bindings and
		  ;; restrictions are made and a request to reachieve the
		  ;; achievable any old how.
		  #|(member (con-type cond) '(only_use_for_query))|#
		  ;; This OR-tree posted to resolve an interaction with a
		  ;; reestablishable type of condition (ie only_use_for_query).
		  ;; So what we do here is to set up an alternative where we
		  ;; try to resatisfy the condition with the opposite bindings
		  ;; and restrictions.
		  )
	      (set-up-alternative-with-opposite cond chosen-branch))
	  (unless (eql satisfy-p :NOT-AGAIN)
	    (when (member (con-type cond) '(only_use_for_query))
	      ;; /\/: The chosen-branch will put in contribs for the cond,
	      ;; and in the alt we'll have discarded the current or-tree
	      ;; in favor of a :reachieve one.  We have to call now-have-cond
	      ;; here so the cond won't still seem to be outstanding in the
	      ;; alt.
	      (db-call 'now-have-cond cond) ; /\/
	      (set-up-alternative-with-opposite cond chosen-branch))))

      (unless (process-or-rec cond chosen-branch satisfy-p)
	(return-from KS-OR nil))		;will poison, so quit now.

      ;; We now consider the next level of the or-tree.  The right thing
      ;; to do is probably to post back to the AM so that this OR can be
      ;; compared to other agenda entries that might be better choices.
      ;; But to decrease the difference from past behavior, we send the
      ;; OR back to the AM only if the number of branches is greater than
      ;; it was initially.  This ensures that if this OR was picked because
      ;; its branch-1 was 1, it doesn't go on to less constrained parts
      ;; of the or-tree.  /\/

      ;; /\/: Now posts back only of the initial branch-1 was 1.
      ;; This decreases the number of times KS-OR has to run, and
      ;; we don't (yet?) make meaningful decisions about other
      ;; branch-1 values anyway.

      (setq or-tree (or-branch-subtree chosen-branch))
      (assert (implies or-tree
		       (= (or-tree-branch-1 or-tree)
			  (length (or-tree-branches or-tree)))))
      (cond ((null or-tree)
	     (return))
	    ((and (= initial-n-branches 1)
		  (> (or-tree-branch-1 or-tree) initial-n-branches))
	     ;; Send the OR back to the AM.
	     ;; /\/: Should the ID change?
	     (dev-debug :minimal "Reposting or, now = ~W" or-tree)
	     (post-or cond contribs satisfy-p or-tree)
	     (return))
	    (t
	     ;; Otherwise, go 'round the loop to process the next
	     ;; level of the tree.
	     (setq branches (or-tree-branches or-tree)))))))


(defun simple-ks-or (ag cond satisfy-p or-tree)
  (assert or-tree)
  (when *trim-in-ks-or*
    (setq or-tree (db-call 'trim-or-tree or-tree))
    (when (null or-tree)
      (post-agenda '(:poison-state :empty-or-tree))
      (return-from simple-ks-or nil)))
  (let ((contribs nil)
	(branches (or-tree-branches or-tree))
	(initial-n-branches (or-tree-branch-1 or-tree))
	(chosen-branch nil)
	(trail '()))
    (loop
      (when (null branches)
	(return))
      ;; Pick a branch and post the rest (if any) in an alt.
      (setq chosen-branch (first branches))
      (when (rest branches)
	(with-ag-copy (ag ag)
	  (setf (ag-body ag)
		`(:OR ,cond
		      ,contribs
		      ,satisfy-p
		      .,(new-or-tree (rest branches))))
	  (setf (ag-branch-1 ag) (length (rest branches)))
	  (let ((alt-id (am-request :alt-agenda ag)))
	    (push (cons alt-id (rest branches))
		  trail))))
      ;; See if the chosen branch works.
      (cond ((process-or-rec cond chosen-branch satisfy-p)
	     ;; Looks ok.  So let's go to the subtree
	     (setq or-tree (or-branch-subtree chosen-branch))
	     (cond ((null or-tree)
		    (return))
		   (t
		    ;; Otherwise, go 'round the loop to process the next
		    ;; level of the tree.
		    (setq branches (or-tree-branches or-tree)))))
	    ;; The chosen branch didn't work.
	    ((null trail)
	     ;; Nothing we can do about it.
	     (return))
	    (t
	     ;; Locally backtrack.  Remember that posting the alt
	     ;; pushed a context.
	     (let* ((trail-rec (pop trail))
		    (trail-alt (am-request :get-alt (car trail-rec)))
		    (trail-branches (cdr trail-rec)))
	       (db-request :pop-context)
	       (assert (= (db-request :get-context) (alt-context trail-alt)))
	       (am-request :delete-alt (alt-id trail-alt))
	       (setq branches trail-branches))))
      ;; Now we have a new list of branches
      (when (and (= initial-n-branches 1)
		 (> (length branches) initial-n-branches))
	;; Send the OR back to the AM.
	;; /\/: Should the ID change?
	(let ((new-tree (new-or-tree branches)))
	  (dev-debug :minimal "Reposting or, now = ~W" new-tree)
	  (post-or cond contribs satisfy-p new-tree)
	  (return))))))


;; Keep simple at mo.
(defun pick-from-next-level (res-list)
  (values (car res-list) (cdr res-list)))


(defun process-or-rec (cond or-rec update-p)
  
  (dolist (action (or-branch-actions or-rec)
	   t)				;success
    (ecase (car action)
      (:BIND
       (whats-going-on "Binding ~S to ~S" (second action) (third action))
       (when (null (set-binding (second action) (third action)))
	 (post-agenda `(:POISON-STATE :COULD-NOT-BIND
			,(second action) ,(third action)))
	 (return nil)))
      (:RESTRICT
       (whats-going-on "Making ~S not equal to ~S"
		       (second action) (third action))
       (when (null (add-restriction-to-psv
		     (second action) :NOT (third action)))
	 (post-agenda `(:POISON-STATE :COULD-NOT-RESTRICT
			,(second action) ,(third action)))
	 (return nil)))
      (:LINK
       (whats-going-on "Linking ~S to ~S" (second action) (third action))
       (when (null (db-request :ADD-LINK (second action) (third action)))
	 (post-agenda `(:POISON-STATE :COULD-NOT-ADD-LINK :FROM
			,(second action) :TO ,(third action)))
	 (return nil)))
      (:UPDATE
       (when update-p
	 (whats-going-on "Updating the contributors for~%~S" cond)
	 (db-request :UPDATE-CONTRIBUTORS cond (second action))))
      (:REACHIEVE
       (whats-going-on "Posting to reachieve ~%~S" cond)
       ;; Remove GOST entry for the condition.
       (let ((old-contribs
	      (db-request :UPDATE-CONTRIBUTORS cond :undef)))
	 ;; Strip off the satisfaction method keyword.
	 (setq old-contribs
	       (mapcar #'car old-contribs))
	 (if (eql (cadr (second action)) 'achievable)
	     (post-achieve `(:ACHIEVE ,(cdr (second action)))
		 :trigger t
		 :stage   0
		 :info    (list nil nil nil old-contribs))
	     (post-condition `(:CONDITION ,(cdr (second action)))
		 :trigger t
		 :stage   0
		 :info    old-contribs)))))))


(defun set-up-alternative-with-opposite (cond or-rec)
  ;; As long as the set of actions in the or-tree do not contain any linkings
  ;; set up an alternative with an or-tree which gives the not of the bindings
  ;; and restrictions required and an action to reachieve the cond.
  (unless (null (or-branch-actions or-rec))
    (let (opposite-actions)
      (dolist (action (or-branch-actions or-rec))
	(case (car action)
	  (:BIND
	   (push (list :RESTRICT (second action) (third action))
		 opposite-actions))
	  (:RESTRICT
	   (push (list :BIND (second action) (third action))
		 opposite-actions))
	  (:LINK
	   (push (list :NOT-LINK (second action) (third action))
		 opposite-actions)
	   #|(MONITOR :KP
		    (:DETAILED "LINKings not handled so ignore at moment."))|#)
	  (:UPDATE)
	  (:REACHIEVE)))
      (when opposite-actions
	(unless (every #'(lambda (x) (eql (car x) :NOT-LINK))
		       opposite-actions)
	  (post-alternatives
	    (assign-or-level
	     (make-agenda-entry
		:body `(:OR ,cond nil :NOT-AGAIN
			.,(new-or-tree
			   (list (oplan-qa::make-or-branch
				   :actions (list
					     (list :REACHIEVE
						   (cons 'GOST cond)))))))
		:trigger (make-wait-for-effect-trigger cond nil)
		:branch-1 1
		))
	    #|opposite-actions|#))))))


#+:undef
(defun some-bindings-to-do-p (or-branch)
  "Returns t if there are some bindings, or restrictions to be made."
  (some #'(lambda (x) (or (eql (car x) :BIND) (eql (car x) :RESTRICT)))
	(oplan-qa::or-branch-actions or-branch)))


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;

