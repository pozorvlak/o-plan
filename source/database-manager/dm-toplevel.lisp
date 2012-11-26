;;;; File: dm-toplevel.lisp
;;; Contains: Top-level processing for the DM.
;;; Author: Richard Kirby (rbk) and Jeff Dalton
;;; Created: Mon Apr 16 11:29:20 1990
;;; Updated: Thu Jun  3 03:07:33 1999 by Jeff Dalton
;;; Copyright: (c) 1992, AIAI, University of Edinburgh

(in-package :oplan)

;;;; Startup and event handling

(defun dm-startup ()
  
  ;; This is the initial function for the DM process.  After startup,
  ;; it is not called again.  Instead, dm-event-handler is called
  ;; whenever a message arrives.
  
  (is-init)
  ;; Make breaks, etc. go to the debug window.
  (x-redirect-errors :dmout)
  ;; Register with IM.
  (clear-screen *debug-io*)
  
  (ipc-register-with-im)

  (ipc-set-event-handler 'dm-event-handler))

(defun dm-event-handler (self)
  (let ((c (message-contents (next-event self))))
    (db-answer (car c) (cdr c))))

(defun db-answer (ident args)
  (apply (ipc-get-handler :DM ident) args))

(defun db-handle-request (ident args)
  (let ((dm *pprocess*))
    ;; Take care of any messages the DM has already received.
    (while (next-event-p dm)
      (dm-event-handler dm))
    ;; Answer the new request.
    (apply (ipc-get-handler :DM ident) args)))


;;;; Messages

(defmessage (:DM :FUNCALL) (fn &rest args)
  (apply fn args))

(defmessage (:DM :APPLY) (fn &rest args)
  ;; last elt of args is a list of args as in apply.
  (apply #'apply fn args))

(defmessage (:DM :OBMATCH) (t1 t2 bindings)
  (ipc-with-own-copy (binds bindings (copy-tree binds))
    (obmatch3 t1 t2 binds)))

(defmessage (:DM :GET-EXPAND-SCHEMAS) (pat)
  (find-schemas-matching-expansion-pattern pat))

(defmessage (:DM :NUMBER-OF-EXPAND-SCHEMAS) (pat)
  (length (find-schemas-matching-expansion-pattern pat)))

(defmessage (:DM :GET-EFFECT-SCHEMAS) (effect)
  (find-schemas-for-using-for-effect effect))

(defmessage (:DM :NUMBER-OF-EFFECT-SCHEMAS) (effect)
  (length (find-schemas-for-using-for-effect effect)))

(defmessage (:DM :EXISTS-NAMED-SCHEMA) (name)
  (if (find-schema-with-name name)
      t
    nil))

(defmessage (:DM :GET-NAMED-SCHEMA) (name)
  (find-schema-with-name name))

(defmessage (:DM :EXPAND) (node schema-name subnodes orderings)
  (ads-expand-node node schema-name subnodes orderings))

(defmessage (:DM :ADD-LINK) (from to)
  (add-a-link from to))

(defmessage (:DM :ADD-NODE)
            (&key type pattern reason parent pre-list post-list)
  ;; This is used only by ks-achieve.
  (add-a-node type pattern reason parent pre-list post-list))

(defmessage (:DM :ADD-INITIAL-NODE) (schema-node)
  (add-an-initial-node schema-node))

(defmessage (:DM :GET-PLAN-NODES) (&rest args)
  (apply #'print-plan-nodes args))

(defmessage (:DM :GET-ACTION-LEVELS) ()
  (hash-table-alist (mapping action-name level)))

(defmessage (:DM :GET-RUT) ()		;resource usage table
  (cm-constraint-descriptions
    (find-cm 'resource)))

(defmessage (:DM :DOMAIN) (filename)
  (init-new-domain filename))

(defmessage (:DM :PUSH-CONTEXT) ()	;return context number? /\/
  (push-context)
  :OK)

(defmessage (:DM :POP-CONTEXT) ()	;return context number? /\/
  (pop-context)
  :OK)

(defmessage (:DM :GET-CONTEXT) ()
  (context-number *context*))

(defmessage (:DM :SET-CONTEXT) (context-number)
  (setq *context* (find-context context-number))
  (whats-going-on "NEW DATABASE STATE~%==================")
  (print-database)
  :OK)

(defmessage (:DM :NEW-CONTEXT) (parent-number)
  (context-number (new-context (find-context parent-number))))

(defvar *context-was-poisoned* nil)	;context-layered

(defmessage (:DM :POISON-CONTEXT) (agenda-id reason)
  (when (ctxt-symbol-value '*context-was-poisoned*)
    (error "Context already poisoned before ~S ~S." agenda-id reason))
  (setf (ctxt-symbol-value '*context-was-poisoned*) 
	(list agenda-id (or reason t))))

(defmessage (:DM :CONTEXT-WAS-POISONED) ()
  (ctxt-symbol-value '*context-was-poisoned*))

(defmessage (:DM :GET-VAR-VALUE) (psv)
  (psv-get-value psv))

(defmessage (:DM :GET-VAR-BINDINGS) (psv) ;call it :GET-VAR-POSSIBLES? /\/
  (psv-get-possibles psv))

(defmessage (:DM :SET-VAR-BINDING) (psv value)
  (psv-begin-var-transaction)
  (let ((answer (psv-set-value psv value)))
    (if answer				;was (eq answer t)
	(psv-commit-var-transaction)
      (psv-abort-var-transaction))
    answer))

(defmessage (:DM :ADD-VAR-RESTRICTION) (var type value)
  (psv-begin-var-transaction)
  (let ((answer (psv-add-restriction var type value)))
    (if answer				;was (eql answer t)
	(psv-commit-var-transaction)
      (psv-abort-var-transaction))
    answer))

(defmessage (:DM :SET-VAR-POSSIBLES) (psv values)
  (psv-begin-var-transaction)
  (let ((answer (psv-set-possibles psv values)))
    (if answer
	(psv-commit-var-transaction)
      (psv-abort-var-transaction))
    answer))

(defmessage (:DM :INSTANTIATE-SCHEMA) (schema bindings node-tag)
  (db-instantiate-schema schema bindings node-tag))

(defmessage (:DM :CHECK-ONLY-USE-IFS) (cond bindings)
  (tgm-check-condition-within-bindings cond bindings))

(defmessage (:DM :PRINT-DATABASE) ()
  (whats-going-on "DATABASE STATE~%==============")
  (print-database)
  nil)

(defmessage (:DM :CHECK-FOR-EFFECT) (effect bindings &rest optionals)
  (apply #'check-for-effect effect bindings optionals))

(defmessage (:DM :ADD-TIME-WINDOW) (from to min max)
  (add-time-window from to min max))

(defmessage (:DM :EXCHANGE-AGENDA-ENTRY) (old-ae new-ae)
  ;; This combines :PROCESSED-AGENDA-ENTRY and :ADD-AGENDA-ENTRY
  ;; for when we pick an alternative.
  ;; /\/: Placed here to avoid changing the time to lookup other
  ;; entries in the original case statement.  Eventually we should
  ;; put all cases in a more reasonable order...
  (atm-remove-triggered-agenda-entry old-ae)
  (atm-add-untriggered-agenda-entry new-ae)
  :OK)

(defmessage (:DM :CHECK-PLAN) (&rest args)
  (apply #'check-plan args))

(defmessage (:DM :EVAL-PLAN) ()
  (eval-plan))

(defmessage (:DM :GET-PSVS) ()
  (psv-get-psv-descriptions))

(defmessage (:DM :GET-PSVS-FOR-USER) ()
  (psv-get-psv-descriptions-for-user))
		 
(defmessage (:DM :REQUEST-UNTRIGGERED-AGENDA) ()
  (atm-get-untriggered-agenda))

(defvar *dm-base-context* 'not-a-dm-base-context)

(defmessage (:DM :INIT) ()
  ;; Set the DM to the NULL state, ie wipe everything!
  (clear-screen *debug-io*)
  ;; Discard contexts from last time, if any.
  ;; /\/: Should be supported more directly by the context code.
  (when (oplan-ctxt::context-p *dm-base-context*)
    (assert (context-parent *dm-base-context*))	;not a root
    (deletef *dm-base-context*
	     (context-children (context-parent *dm-base-context*))))
  ;; Set context to the top, and then push to a new one.
  (setq *context* *global-context*)
  (setq *dm-base-context* (push-context))
  (initialize-variable-group :dm)
  (call-initializers :dm)
  :ok)

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
