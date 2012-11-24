;;;; File: tgm.lsp
;;; Contains: TOME and GOST management routines.
;;; Author: Richard Kirby (rbk) and Jeff Dalton
;;; Created: Fri Jun  5 16:22:12 1992
;;; Updated: Tue Jun  8 02:35:26 1999 by Jeff Dalton
;;; Copyright: (c) 1992 - 1999, AIAI, University of Edinburgh


(in-package :oplan-tgm)

(use-package :oplan-developerlib)
(use-package :oplan-nodes)
(use-package :oplan-gop)
(use-package :oplan-qa)
(use-package :oplan-obase)
(use-package :oplan-psv)

(export '(tgm-add-effect tgm-add-condition
	  tgm-commit-to-adding-effect tgm-commit-to-adding-condition
	  tgm-abort-adding-cond-or-effect
	  tgm-update-contributors
	  tgm-check-condition-within-bindings
	  tgm-walk-tome
	  tgm-walk-gost
	  tgm-teleology-at-node-end tgm-print
	  tgm-make-a-list-of-the-gost
	  tgm-make-a-list-of-the-tome
	  tgm-init
	  tome gost))

(defvar *tgm-open-transaction-p* nil
  "t if called tgm-add-effect/condition, which stops another call to
   tgm-add-effect/condition until tgm-commit or tgm-abort is called.")

(define-initializer :dm tgm-init ()
  (setq *tgm-open-transaction-p* nil))

(defun eval-value (v)
  "The value associated with a GOST entry may be an O-Base variable or a
   function to be computed. If so then the value should be evaluated as
   TRUE for the sake of interaction detection."
  ;; (or (fn-rec-p v) (itemp v) user::TRUE)
  (declare (ignore v))
  'user::TRUE)

(defun tgm-node-end-within-range-p (query focus contributors)
  "Returns t if query falls within the range of contributors to focus, or if
   no contributors, in parallel with or before the focus."
  (and (not (gop-follows query focus))
       (not (some #'(lambda (contributor)
		      (or (eql (car contributor) :ALWAYS)
			  (gop-precedes query (car contributor))))
		  contributors))))

;; Returns two values, a list of contributors and a list of deletors.
;; contributors are those TOME entries where p and v match, and deletors are
;; those TOME entries where p match but v doesn't.
;;
;; (/\/: You wish!)
;;
;; p and v are the pattern and value to search HBase for TOME entries with
;; matching patterns. Any PSVs that they may have contained should have been
;; converted to given actors using psv-actorise-pattern.
;; The optional argument is used to control the looking up of contributors
;; and/or deletors. Will find ALWAYS effects as well.
;;
(defun tgm-find-contributors-and-deletors (p v &optional
					     (look-for-contributors-p t)
					     (look-for-deletors-p t))
  (let ((act::*bindings* act::*bindings*)
	(initial-bindings act::*bindings*)
	p-tome tome-list tome-entry a-tome-entry tome-value a-tome-value
	contributors deletors)
    (setq p-tome (tgm-make-tome-entry p actorsym))
    (setq tome-list (cdr (get-a-tome-list p-tome)))
    (loop
      (if (null tome-list) (return))
      ;; This inner loop looks through tome-list calling obmatch until a match
      ;; is found and then the loop is exited, with tome-entry equal to nil if
      ;; no more, otherwise the next matching entry; a-tome-entry is the
      ;; psv-actorised version of tome-entry; tome-list is left pointing to
      ;; just after this matching entry; *bindings* will have the required
      ;; bindings to match the patterns.
      (loop
        ;; Reset bindings.
        (setq act::*bindings* (copy-tree initial-bindings))
	(setq tome-entry (pop tome-list))
	(setq tome-value (value tome-entry))
	(if (null tome-entry) (return))
	(unless (eql tome-value :undef) ; Only use TOME's in current context.
	  (setq a-tome-entry (psv-actorise-pattern tome-entry))
	  (if (obmatch a-tome-entry p-tome)
	      (return))))
      ;; Now check the value to see whether the matching TOME entry
      ;; is a potential contributor or deletor.
      (when tome-entry
	(setq a-tome-value (psv-actorise-pattern tome-value))
	(let* ((p-bindings (copy-tree act::*bindings*))
	       (match-p (obmatch a-tome-value v))
	       (p+v-bindings act::*bindings*))
	  (when (and match-p look-for-contributors-p)
	    (push (tgm-make-contributor-record
		    (item-inst tome-entry)
		    (item-inst tome-value)
		    p+v-bindings)
		  contributors))
	  (when (and look-for-deletors-p
		     (or (not match-p)
			 (and match-p
			      (not (equal p+v-bindings p-bindings)))))
	    (push (tgm-make-deletor-record
		    (item-inst tome-entry)
		    (item-inst tome-value)
		    p+v-bindings)
		  deletors)))))
    (values contributors
	    deletors)
    #+:undef
    (values (delete-duplicates contributors
			       :test #'equal
			       :key #'tgm-contributor-deletor-node-end)
	    (delete-duplicates deletors
			       :test #'equal
			       :key #'tgm-contributor-deletor-node-end))))

(defun possible-to-reachieve-p (cond contribs)
  "Returns t if able to try to reachieve this condition"
  (let ((max-satisfaction-method 0))
    ;; Find out the most work that has been done to satisfy this condition.
    (mapc #'(lambda (contributor)
	      (setq max-satisfaction-method
		    (max max-satisfaction-method
			 (tgm-get-satisfaction-method-level
			  (cdr contributor)))))
	  contribs)
    ;;(if (zerop max-satisfaction-method)
    ;;(break "possible-to-reachieve-p: No satisfaction"))
    (not (= (tgm-get-satisfaction-method-level
	     (tgm-condition-type-to-allowable-satisfaction-methods
	      (tgm-gost-condition-type cond)))
	    max-satisfaction-method))))

(defun tgm-add-effect (patt val at-node-end)
  "Checks to see if the effect patt=val @ at-node-end can be added to the TOME,
   and returns a structure which either says yes, no, or yes if do some work.
   Arguments:
     patt - A pattern.
     val - The pattern's value.
     at-node-end - The node-end that the effect is asserted.
   Results:
     See comments above."
  
  (when *tgm-open-transaction-p*
    (break "Transaction already opened, will return :OUTSTANDING")
    (dev-note :tgm :debug "Transaction already open.")
    (return-from tgm-add-effect :OUTSTANDING))

  (setq *tgm-open-transaction-p* t)
  
  ;; Look through the GOST for any conditions that have matching patterns but
  ;; not matching values with the effect being added. 
  (let ((a-p (psv-actorise-pattern patt))
	(a-v (psv-actorise-pattern val))
	always-tome-entry tome-entry tome-list a-tome-entry tome-value
	a-tome-value qa-result
	not-a-v a-p-gost gost-list gost-entry a-gost-entry
	gost-contribs result act::*bindings* p-tome-entry)
    
    (setq always-tome-entry (tgm-make-tome-entry a-p :ALWAYS))
    (setq tome-list (cdr (get-a-tome-list always-tome-entry)))
    ;; Look for :ALWAYS facts.
    (loop
      (if (null tome-list) (return))
      ;; This inner loop looks through tome-list calling obmatch until a match
      ;; is found, and then the loop is exited, with tome-entry equal to nil if
      ;; no more, otherwise the next matching entry, a-tome-entry is the
      ;; psv-actorised version of tome-entry, tome-list is left pointing to
      ;; just after this matching entry, and *bindings* will have the required
      ;; bindings to match the patterns.
      (loop
        (setq act::*bindings* nil)
	(setq tome-entry (pop tome-list))
	(setq tome-value (value tome-entry))
	(if (null tome-entry) (return))
	(unless (eql tome-value :undef) ; Only use TOME's in current context.
	  (setq a-tome-entry (psv-actorise-pattern tome-entry))
	  (if (obmatch a-tome-entry always-tome-entry) (return))))
      ;; Now check the values. If *bindings* is nil then we know that
      ;; the effect matches an ALWAYS fact exactly and so we do not
      ;; add (if the values are different then issue a warning just to
      ;; indicate that the TF may be wrong). If bindings are required
      ;; then record the bindings for storing with the effect.
      (when tome-entry
	(setq a-tome-value (psv-actorise-pattern tome-value))
	(if (obmatch a-tome-value a-v)
	    (if act::*bindings*
		(push (tgm-make-always-result act::*bindings*
					      (item-inst tome-entry)
					      (item-inst tome-value))
		      result)
		(return-from tgm-add-effect
		  (list (tgm-make-always-result nil (item-inst tome-entry)
						(item-inst tome-value)))))
	    (if act::*bindings*
		(push (tgm-make-always-conflict-result act::*bindings*
						       (item-inst tome-entry)
						       (item-inst tome-value))
		      result)
		(return-from tgm-add-effect
		  (list
		   (tgm-make-always-conflict-result nil (item-inst tome-entry)
						    (item-inst
						     tome-value))))))))
    
    ;; Now check for possibly clobbered conditions.
    (setq p-tome-entry (tgm-make-tome-entry patt at-node-end))
    (setq not-a-v (make-not-value-actor a-v))
    (setq a-p-gost (tgm-make-gost-entry actorsym a-p not-a-v actorsym))
    ;; Get a shortlist of GOST entries.
    (setq gost-list (cdr (get-a-gost-list a-p-gost)))
    (loop
      (if (null gost-list) (return result))
      ;; This inner loop looks through gost-list calling obmatch until
      ;; a match is found and then the loop is exited, with gost-entry
      ;; equal to nil if no more, otherwise the next matching entry,
      ;; a-gost-entry is the psv-actorised version of gost-entry,
      ;; gost-list is left pointing to just after this matching entry,
      ;; and *bindings* will have the required bindings to match the
      ;; patterns.
      (loop
        ;; Reset bindings.
        (setq act::*bindings* nil)
	(setq gost-entry (pop gost-list))
	(setq gost-contribs (value gost-entry))
	(if (null gost-entry) (return))
	;; Only consider GOST entries in this context.
	(unless (eql gost-contribs :undef)
	  (setq a-gost-entry (psv-actorise-pattern gost-entry))
	  (if (obmatch a-gost-entry a-p-gost)
	      (return))))
      (when gost-entry
	;; A possibly clobbered condition, so check if the effect falls within
	;; the range.
	(let ((gost-node-end (tgm-gost-node-end a-gost-entry)))
	  (when (tgm-node-end-within-range-p
		 at-node-end gost-node-end gost-contribs)
	    ;; Ask QA for ways out.
	    (setq qa-result
		  (qa (tgm-gost-pattern a-gost-entry)
		      (tgm-gost-value a-gost-entry)
		      (tgm-gost-node-end a-gost-entry)
		      (qa-make-work-flag-structure
		       (tgm-condition-type-to-allowable-satisfaction-methods
			(tgm-gost-condition-type a-gost-entry))
		       gost-contribs
		       (list
			(tgm-make-deletor-record p-tome-entry val
						 act::*bindings*)))))
	    ;; For some conditions with some satisfaction methods, it is
	    ;; appropriate to consider reachieving the condition as an option.
	    (let ((cond (item-inst gost-entry)))
	      (unless (eql qa-result t)
		(push (tgm-make-interact-result
		       qa-result cond gost-contribs)
		      result)))))))
    (or (nreverse result) t)))

(defun tgm-add-condition
    (type patt val at-node-end &optional contrib-list after-point)
  "Add a condition, which involves using QA to make sure that it can be done.
   Arguments:
     type - The type of condition (eg user::supervised)
     patt - The condition.
     val - The value of the condition.
     at-node-end - Where the condition is required.
     contrib-list - Any given contributors of the condition.
     after-point - only for achieve...after
   Results:
     See comments above tgm-add-effect."

  (declare (ignore after-point))
  
  (when *tgm-open-transaction-p*
    (break "Transaction already opened, will return :OUTSTANDING")
    (dev-note :tgm :debug "Transaction already open.")
    (return-from tgm-add-condition :OUTSTANDING))

  (setq *tgm-open-transaction-p* t)
  
  (let ((a-p (psv-actorise-pattern patt))
	(a-v (psv-actorise-pattern val))
	qa-result)
    (if (and contrib-list (not (eql (car contrib-list) :NOT)))
	(setq contrib-list
	      (mapcar #'(lambda (x) (cons x :NONE))
		      contrib-list)))
    (setq qa-result
	  (qa a-p a-v at-node-end
	      (qa-make-work-flag-structure
	       (tgm-condition-type-to-allowable-satisfaction-methods type)
	       (if (eql (car contrib-list) :NOT) nil contrib-list)
	       nil
	       (if (eql (car contrib-list) :NOT) (cdr contrib-list)))))
    (tgm-make-satisfy-result qa-result)))

(defun tgm-commit-to-adding-effect (patt val at-node-end)
  "Actually adds the TOME entry.
   Arguments:
     patt - the pattern that describes the effect.
     val - the value of the pattern.
     at-node-end - Where the effect is asserted.
   Results:
     NONE.
   Side Effects:
     Creates a TOME entry."
  (if *tgm-open-transaction-p*
      (let ((tome-entry (tgm-make-tome-entry patt at-node-end))
	    (act::*bindings* nil))
	(dev-note :tgm :trace "Adding ~W = ~W" tome-entry val)
	(setvalue tome-entry val)
	(setq *tgm-open-transaction-p* nil))
      (progn
	(break "Transaction not opened, will return :OUTSTANDING")
	(dev-note :tgm :debug "Transaction not open"))))

(defun tgm-commit-to-adding-condition (type patt val at-node-end
					    &optional contrib-list
					              after-point)
  "Actually adds the GOST entry.
   Arguments:
     type - The condition type.
     method - The satisfaction method used.
     patt - the condition pattern.
     val - The required value of the pattern.
     at-node-end - Where the pattern needs to have the value.
     contrib-list - A list of nodes that contribute to the satisfaction
                    of this condition.
     after-point - used only by achieve...after
   Results:
     NONE.
   Side Effects:
     Creates a GOST entry."
  (declare (ignore after-point))
  (if *tgm-open-transaction-p*
      (let ((gost-entry (tgm-make-gost-entry type patt val at-node-end))
	    (contribs (tgm-make-gost-contributors :NONE contrib-list))
	    act::*bindings*)
	(dev-note :tgm :trace "Adding ~W = ~W" gost-entry contribs)
	(setvalue gost-entry contribs)
	(setq *tgm-open-transaction-p* nil))
      (progn
	(break "Transaction not opened, will return :OUTSTANDING.")
	(dev-note :tgm :debug "Transaction not open"))))

(defun tgm-abort-adding-cond-or-effect ()
  (if *tgm-open-transaction-p*
      (progn
	(dev-note :tgm :trace "Aborting TGM transaction")
	(setq *tgm-open-transaction-p* nil))
      (progn
	(break "Transaction not opened")
	(dev-note :tgm :debug "Transaction not open"))))

(defun tgm-update-contributors (cond contributors)
  (let* ((gost-entry (apply #'tgm-make-gost-entry cond))
	 (old-contribs (value gost-entry)))
    (if (and old-contribs (not (eql old-contribs :undef))
	     (not (eql contributors :undef))
	     (null (intersection contributors old-contribs
				 :test #'equal :key #'car)))
	(dev-note :tgm :detail "Some old contributors"))
    (dev-note :tgm :detail "Updating contributors for ~W~%to ~W"
	      gost-entry contributors)
    (setvalue gost-entry contributors)
    old-contribs))

(defun tgm-check-condition-within-bindings (cond bindings)
  (let ((act::*bindings* bindings)
	interact)
    ;;    (setq cond (instance cond))
    ;;    (setq act::*bindings* nil)
    (setq interact (apply #'tgm-add-condition cond))
    (tgm-abort-adding-cond-or-effect)
    interact))


;;; Walk-tome and -ghost

;;; /\/: Should be possible to make the -walk- functions more efficient
;;; by not using generators, or else more general by allowing a pattern
;;; as input.

;;; /\/: If we have a pattern for the whole item, e.g., (TOME ?? ??),
;;; and value, then we may be able to do more efficient lookup when
;;; some parts of the pattern aren't ??.  The current walk-{tome,gost}
;;; functions always look at the whole TOME or GOST item-by-item.

;;; /\/: Define a walk-items for O-Base?

;;; Note that item-inst is called on the TOME and GOST entries.

;;; (tgm-walk-tome fn) --
;;; Applies fn to each TOME entry that is in the current context.
;;; Fn takes two arguments: the TOME entry, and the TOME value.

(defun tgm-walk-tome (fn)
  (let ((tome-entries (generator '(TOME ?? ??) '??)))
    (do ((tome-entry (try-next tome-entries) (try-next tome-entries)))
	((null tome-entry))
      (let ((tome-value (value tome-entry)))
	(unless (eq tome-value :undef)
	  (funcall fn (item-inst tome-entry) tome-value))))))

;;; (tgm-walk-gost fn) --
;;; Applies fn to each GOST entry that is in the current context.
;;; Fn takes two arguments: the GOST entry, and the GOST value.

(defun tgm-walk-gost (fn)
  (let ((gost-entries (generator '(GOST ?? ?? ?? ??) '??)))
    (do ((gost-entry (try-next gost-entries) (try-next gost-entries)))
	((null gost-entry))
      (let ((gost-value (value gost-entry)))
	(unless (eql gost-value :undef)
	  (funcall fn (item-inst gost-entry) gost-value))))))


;;; Some functions for looking around

(defun tgm-teleology-at-node-end (n &optional (stream *debug-io*))
  (let (act::*bindings*)
    (let ((g (generator '(GOST ?? ?? ?? ??) '??))
	  clist)
      (format stream "Statements that activity ~A achieves for others.~%" n)
      (do ((one (try-next g) (try-next g)))
	  ((null one))
	(setq clist (value one))
	(if (member n clist :test #'equal)
	    (progn
	      (format stream "~1,8T~A" (item-inst one))
	      (if (eql (length clist) 1)
		  (format stream " from only ")
		  (format stream " from "))
	      (format stream "~A~%" clist))))
      (terpri stream)
      (terpri stream)
      
      (format stream
	      "Statements that must hold for activity ~A to be executed.~%" n)
      (setq g (generator `(GOST ?? ?? ?? ?? ,n) '??))
      (do ((one (try-next g) (try-next g)))
	  ((null one))
	(setq clist (value one))
	(format stream "~1,8T~A" (item-inst one))
	(if (eql (length clist) 1)
	    (format stream " from only ")
	    (format stream " from "))
	(format stream "~A~%" clist)))))

(defun tgm-print (&optional (stream *debug-io*))
  (let (act::*bindings*)
    (let ((g (generator '?? '??)))
      (format stream "TOME & GOST Printer.~%~%")
      (do ((i (try-next g) (try-next g)))
	  ((null i))
	(format stream "~A = ~A~%" (item-inst i) (value i)))
      (terpri stream)
      (terpri stream))))

(defun tgm-make-a-list-of-the-tome (&optional (filter-out-undefs t)
					      (replace-psvs t))
  (let (act::*bindings*)
    (let ((result nil)
	  (g (generator '(TOME ?? ??) '??)))
      (do ((item (try-next g) (try-next g)))
	  ((null item) result)
	(if (or (null filter-out-undefs)
		(null (eql (value item) :undef)))
	    (push (list (if replace-psvs
			    (psv-replace-psvs-in-pattern item)
			    (item-inst item))
			(if replace-psvs
			    (psv-replace-psvs-in-pattern (value item))
			    (value item)))
		  result))))))

(defun tgm-make-a-list-of-the-gost (&optional (filter-out-undefs t)
					      (replace-psvs t))
  (let (act::*bindings*)
    (let ((result nil)
	  (g (generator '(GOST ?? ?? ?? ??) '??)))
      (do ((item (try-next g) (try-next g)))
	  ((null item) result)
	(if (or (null filter-out-undefs)
		(null (eql (value item) :undef)))
	    (push (list (if replace-psvs
			    (psv-replace-psvs-in-pattern item)
			    (item-inst item))
			(if replace-psvs
			    (psv-replace-psvs-in-pattern (value item))
			    (value item)))
		  result))))))

;;; Specialized versions of getashortlist.

(defun get-a-tome-list (tome-pat)
  (getashortlist tome-pat))

(defun get-a-gost-list (gost-pat)
  (getashortlist gost-pat))

;;;; ---------------------------- Change History ----------------------------
;;;; (Initials) (Date of change)           (Comment)
