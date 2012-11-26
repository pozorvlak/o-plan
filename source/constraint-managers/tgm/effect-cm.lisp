;;;; File: effect-cm.lsp
;;; Contains: The CM interface to the TGM for effects
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: November 1996
;;; Updated: Sat Jun 19 23:51:28 1999 by Jeff Dalton
;;; Copyright: (c) 1996, AIAI, University of Edinburgh

(in-package :oplan)


;;;; DM message handlers

#+:undef
(defmessage (:DM :CHECK-TOME) (args)
  (list (apply #'tgm-add-effect args)))

#+:undef
(defmessage (:DM :ADD-TOME) (args)
  (list (apply #'tgm-commit-to-adding-effect args)))


;;;; The CM

(define-constraint-manager effect-cm (standard-cm))

(register-constraint-manager effect-cm 
  :constraint-types '(effect))


;;;; Initialization

(defmethod cm-init-constraints ((self effect-cm))
  ;; The TGM will do the work when it's initialized.
  nil)


;;;; Adding constraints

(defmethod cm-add-constraints ((self effect-cm) effects)
  (ca-agenda-wrapper #'add-effect-constraints effects))

(defun add-effect-constraints (effects)
  ;;; ADDING EFFECTS
  (let (interact)
    ;; For each effect, ask the TGM to add it, and if okay continue, else if
    ;; there are some interactions with GOST entries then post the interactions
    ;; as KS-OR records, and if the condition types are appropriate, post the
    ;; reachievement of the conditions as alternatives. If we cannot add an
    ;; effect then POISON PLAN.
    (dolist (effect effects t)
      (whats-going-on "Trying to add effect~%")
      (when (expected-cond-p (effect-pattern effect))
	(cerror "Continue anyway" "Effect when outstanding conds: ~S." effect))
      (setq interact (try-adding-effect effect))
      (if (handle-adding-effect-interaction effect interact)
	  (whats-going-on "Okay to add effect.")
	;; Can't add this effect so POISON PLAN STATE.
	(progn
	  (whats-going-on "Couldn't add effect.")
	  (unless *cm-poison*				;modularity /\/
	    ;; If someting hasn't already poisoned.
	    (ca-post-agenda `(:POISON-STATE :COULD-NOT-ADD-EFFECT ,effect
			      :WHEN :EXPANDING))) 	;had: ,node /\/
	  (return nil))))))

;; Goes through all the interactions in interact-list ignoring the :ALWAYS and
;; :ALWAYS-CONFLICT, and posting KS-ORs for any :INTERACTs. After that it will
;; commit to adding the effect, unless the effect matches an :ALWAYS or
;; :ALWAYS-CONFLICT.
(defun handle-adding-effect-interaction (effect interact-list)
  (let ((add-effect-p t))
    (if (consp interact-list)
	(dolist (interaction interact-list)
	  (case (car interaction)
	    (:INTERACT
	     (let ((qa-result (tgm-interact-qa-result interaction))
		   (cond (cdar (tgm-interact-cond interaction)))
		   (contribs (cadr (tgm-interact-cond interaction))))
	       (if qa-result
		   (ca-post-agenda `(:OR ,cond ,contribs ,nil ,qa-result))
		 ;; No way to resolve interaction, so post to reachieve
		 ;; the cond, unless this is a condition type that rules
		 ;; out being reachieved, or has already been satisfied by
		 ;; the maximum method allowed.
		 (when (or (member (con-type cond)
				   '(supervised unsupervised only_use_if))
			   (not (post-to-reachieve-cond-else-poison
				 cond contribs)))
		   (abort-add-effect)
		   (return-from handle-adding-effect-interaction nil)))))
	    (:ALWAYS
	     (let ((bindings (tgm-always-bindings interaction)))
	       (unless bindings (setq add-effect-p nil))))
	    (:ALWAYS-CONFLICT
	     (let ((bindings (tgm-always-bindings interaction)))
	       (unless bindings
		 (dev-debug :detail "~W conflicts with ~W"
			    effect (tgm-always-effect interaction))
		 (setq add-effect-p nil))))))
      (setq add-effect-p interact-list))
    (if add-effect-p
	(really-add-effect effect)
      (abort-add-effect))
    interact-list))

(defun try-adding-effect (effect)
  (apply #'tgm-add-effect effect))

(defun really-add-effect (effect)
  (apply #'tgm-commit-to-adding-effect effect))

(defun abort-add-effect ()
  (tgm-abort-adding-cond-or-effect))

(defun post-to-reachieve-cond-else-poison (cond contributors)
  ;; Posts the reachievement of the cond as something else to do,
  ;; as long as methods are still available, otherwise POISON-PLAN-STATE
  (let ((max-satisfaction-method 0))
    ;; Find out the most work that has been done to satisfy this condition.
    (mapc #'(lambda (contributor)
	      (setq max-satisfaction-method
		    (max max-satisfaction-method
			 (tgm-get-satisfaction-method-level
			  (cdr contributor)))))
	  contributors)
    (cond ((= (tgm-get-satisfaction-method-level
	       (tgm-condition-type-to-allowable-satisfaction-methods
		(con-type cond)))
	      max-satisfaction-method)
	   ;; Already done the maximum amount of work, so poison plan state.
	   ;; unless this is just an alternative.
	   (ca-post-agenda `(:POISON-STATE :COULD-NOT-RESATISFY-THE-CONDITION 
					   ,cond))
	   nil)
	  ((eq (con-type cond) 'achievable)
	   (ca-post-agenda `(:ACHIEVE ,cond)
			   :trigger (make-wait-for-effect-trigger cond nil))
	   t)
	  (t
	   (ca-post-agenda `(:CONDITION ,cond)
	      :trigger (make-wait-for-effect-trigger cond nil))
	   t))))


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
