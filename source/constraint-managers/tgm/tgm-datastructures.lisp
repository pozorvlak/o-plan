;;;; File: tgm-datastructures.lsp
;;;; Contains: TGM datastructures and associated functions.
;;;; Author: Richard Kirby (rbk)
;;;; Created: Thu Jun 18 11:26:11 1992
;;;; Updated: Sat Dec  7 02:11:21 1996 by Jeff Dalton
;;;; Copyright: (c) 1992, AIAI, University of Edinburgh

(in-package :oplan-tgm)

(use-package :oplan-util)
(use-package :oplan-tf)
(use-package :oplan-or-trees)

(export '(tome
	  gost
	  tgm-tome-pattern
	  tgm-tome-node-end
	  tgm-make-tome-entry
	  tgm-gost-condition-type
	  tgm-gost-satisfaction-method
	  tgm-gost-pattern
	  tgm-gost-value
	  tgm-gost-node-end
	  tgm-make-gost-entry
	  tgm-make-gost-contributors
	  tgm-tome-entry
	  tgm-contributor-deletor-node-end
	  tgm-tome-val
	  tgm-tome-bindings
	  tgm-contributor-link
	  tgm-contributor-rules-out-list
	  tgm-deletor-link-before
	  tgm-deletor-before-rules-out-list
	  tgm-deletor-link-after
	  tgm-deletor-after-rules-out-list
	  tgm-make-contributor-record
	  tgm-find-contributors-and-deletors
	  tgm-get-satisfaction-method-level
	  tgm-condition-type-to-allowable-satisfaction-methods
	  tgm-always-bindings
	  tgm-always-effect
	  tgm-interact-qa-result
	  tgm-interact-cond
	  tgm-satisfy-qa-result
	  tgm-positive-result-p))

#|
TOME entries look like this:
(TOME <patt> <tome-at-node-end>) = <val>

<tome-at-node-end> = :ALWAYS | (<node> <end>) |
                                  (<node> <end> :ALWAYS . <bindings>)

GOST entries look like this:
(GOST <cond-type> <patt> <val> <at-node-end>) =
    ( ... (<contributor> . <satisfaction method>) ... )

<satisfaction method> = :NONE | :ALWAYS | :ALREADY-SATISFIED |
                        :ALWAYS-WITH-BINDINGS | :BY-BINDINGS |
                        :LINK-NO-BINDINGS | :LINK-WITH-BINDINGS | :EXPAND

Contributors passed to (and returned from) QA look like this:
(<TOME entry> <TOME val> <bindings> <link> . <rules-out-list>)

Deletors passed to (and returned from) QA look like this:
(<TOME entry> <TOME val> <bindings> <link before> <rules-out-list>
                                    <link after> . <rules-out-list>)

Return value of tgm-add-effect and tgm-add-condition has the following form:
( ... <option> ... )

<option> = (:ALWAYS NIL :EFFECT . <effect>)
           (:ALWAYS <bindings> :EFFECT . <effect>)
           (:ALWAYS-CONFLICT NIL :EFFECT . <effect>)
           (:ALWAYS-CONFLICT <bindings> :EFFECT . <effect>)
           Only from tgm-add-effect
           (:INTERACT <qa result> :COND . <cond>)
           Only from tgm-add-condition
           (:SATISFY <qa result>)
           If no problems.
           (T)

<effect> = ((TOME <patt> <at-node-end>) <val>)
<qa result> = T
              NIL
              OR linkage tree
<cond> = ((GOST <cond-type> <patt> <val> <at-node-end>) <contrib-nodes>)
|#

;;;; TOME field accessor functions.
(defun tgm-tome-pattern (x)
  (second x))

(defun tgm-tome-node-end (x)
  (third x))

(defun tgm-tome-node-end-bindings-required (x)
  (cdddr (tgm-tome-node-end x)))

;;;; TOME entry constructor.
(defun tgm-make-tome-entry (pattern node-end)
  (list 'TOME pattern node-end))

(defun tgm-make-tome-node-end-entry (node-end always-p bindings)
  ;; If always-p then bindings must be set to the bindings required to make
  ;; this TOME entry match with an ALWAYS fact.
  (if always-p
      (append node-end (cons :ALWAYS bindings))
      node-end))

;;;; GOST field accessor functions.
(defun tgm-gost-condition-type (x)
  (second x))

(defun tgm-gost-pattern (x)
  (third x))

(defun tgm-gost-value (x)
  (fourth x))
  
(defun tgm-gost-node-end (x)
  (fifth x))

;;;; GOST entry constructor.
(defun tgm-make-gost-entry (condition-type p v node-end
			    &optional contribs after-point)
  (declare (ignore contribs after-point))
  ;; Safely throws away the contributors for a supervised condition.
  (list 'GOST condition-type p v node-end))

(defun tgm-make-gost-contributors (satisfaction-method contributors)
  (mapcar #'(lambda (x)
	      (cons x satisfaction-method))
	  contributors))

;;;; Contributor/Deletor accessor functions.
(defun tgm-tome-entry (x)
  (first x))

(defsetf tgm-tome-entry (x) (new-val) `(setf (first ,x) ,new-val))

(defun tgm-contributor-deletor-node-end (x)
  (tgm-tome-node-end (tgm-tome-entry x)))

(defun tgm-tome-val (x)
  (second x))

(defsetf tgm-tome-val (x) (new-val) `(setf (second ,x) ,new-val))

(defun tgm-tome-bindings (x)
  (third x))

(defsetf tgm-tome-bindings (x) (new-val) `(setf (third ,x) ,new-val))

(defun tgm-contributor-link (x)
  (fourth x))

(defsetf tgm-contributor-link (x) (new-val) `(setf (fourth ,x) ,new-val))

(defun tgm-contributor-rules-out-list (x)
  (cddddr x))

(defsetf tgm-contributor-rules-out-list (x) (new-val)
  `(setf (cddddr ,x) ,new-val))

(defun tgm-deletor-link-before (x)
  (fourth x))

(defsetf tgm-deletor-link-before (x) (new-val) `(setf (fourth ,x) ,new-val))
  
(defun tgm-deletor-before-rules-out-list (x)
  (fifth x))

(defsetf tgm-deletor-before-rules-out-list (x) (new-val)
  `(setf (fifth ,x) ,new-val))

(defun tgm-deletor-link-after (x)
  (sixth x))

(defsetf tgm-deletor-link-after (x) (new-val) `(setf (sixth ,x) ,new-val))

(defun tgm-deletor-after-rules-out-list (x)
  (cdddr (cdddr x)))

(defsetf tgm-deletor-after-rules-out-list (x) (new-val)
  `(setf (cdddr (cdddr ,x)) ,new-val))

;;;; Contributor/deletor constructors.
(defun tgm-make-contributor-record (tome-entry tome-value bindings)
  (list* tome-entry tome-value bindings nil nil))

(defun tgm-make-deletor-record (tome-entry tome-value bindings)
  (list* tome-entry tome-value bindings nil nil nil nil))

;;;; TGM result structures.
(defun tgm-make-always-result (bindings &rest effect)
  `(:ALWAYS ,bindings :EFFECT ,@effect))

(defun tgm-make-always-conflict-result (bindings &rest effect)
  `(:ALWAYS-CONFLICT ,bindings :EFFECT ,@effect))

(defun tgm-make-interact-result (qa-result &rest cond)
  `(:INTERACT ,qa-result :COND ,@cond))

(defun tgm-make-satisfy-result (qa-result)
  `(:SATISFY ,qa-result))

(defun tgm-always-bindings (interact)
  (second interact))

(defun tgm-always-effect (interact)
  (cdddr interact))

(defun tgm-interact-qa-result (interact)
  (second interact))

(defun tgm-interact-cond (interact)
  (cdddr interact))

(defun tgm-satisfy-qa-result (interact)
  (second interact))

(defun tgm-positive-result-p (tgm-result)
  "True if the TGM result structure indicates that it is okay to add the
   condition or effect, regardless of whether there may be work involved."
  (or (eq tgm-result t)
      (and (eq (car tgm-result) :SATISFY)
	   (progn
	     (check-type (cadr tgm-result) (or null or-tree))
	     (not (null (cadr tgm-result)))))))

;;;; Gost satisfaction methods table.
(eval-when (eval compile load)
  (defconstant satisfaction-methods '(:NONE :ALWAYS :ALREADY-SATISFIED
				      :ALWAYS-WITH-BINDINGS :BY-BINDINGS
				      :LINK-NO-BINDINGS :LINK-WITH-BINDINGS
				      :EXPAND)))

(eval-when (load eval compile)
  (do ((count 0 (1+ count))
       (x satisfaction-methods (cdr x)))
      ((null x))
    (setf (get (car x) 'satisfaction-method-level) count)))

(defun tgm-get-satisfaction-method-level (x)
  (get x 'satisfaction-method-level))

(defun tgm-condition-type-to-allowable-satisfaction-methods
    (condition-type &optional (old-satisfaction-method :NONE))
  "Returns the next highest applicable satisfaction method dependent on the
   condition type and the previous method used (if given and appropriate)."
  (declare (ignore old-satisfaction-method))
  (case condition-type
    ((only_use_if only_use_for_query)
     :LINK-WITH-BINDINGS)
    (supervised
     :LINK-WITH-BINDINGS)
    (unsupervised
     :LINK-WITH-BINDINGS)
    (oplan::achievable			;shouldn't need package /\/
     :EXPAND)
    (otherwise
     (break "~A is an unrecognised condition type" condition-type))))

;;;; Pretty printing routines.
(defun tgm-pretty-print-tome-entry (stream tome)
  (xp-format stream "TOME ~W @ ~W" (tgm-tome-pattern tome)
	      (tgm-tome-node-end tome)))

(eval-when (load eval)
  (set-pretty-printer '(cons (member tome))
			     #'tgm-pretty-print-tome-entry))

;;;; ---------------------------- Change History ----------------------------
;;;; (Initials) (Date of change)           (Comment)
