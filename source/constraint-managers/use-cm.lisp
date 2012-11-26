;;;; File: use-cm.lsp
;;; Contains: An example plug-in constraint-manager
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: March 1996
;;; Updated: Mon Sep 29 00:57:36 1997 by Jeff Dalton
;;; Copyright: (c) 1996, AIAI, University of Edinburgh

(in-package :oplan-use-cm)

;;; Somewhat simple version.  Moderately cleaver about some things.
;;; Handles PSVs.  But intervals are still always from begin to end
;;; of a single node.  Clarity is often preferred to efficiency.

(import 'oplan::use)	;which we define as a CM / constraint type

(use-package :oplan-cm-defs)
(use-package :oplan-util)
(use-package :oplan-dev)
(use-package :oplan-ctxt)
(use-package :oplan-schema-defs)
(use-package :oplan-cm-defs)
(use-package :oplan-nodes)
(use-package :oplan-psv)
(use-package :oplan-obase)
(use-package :oplan-gop)
; (use-package :oplan-tpn)	 ; = :datastrcutures = :ads
(use-package :oplan-or-trees)
(use-package :oplan-tf-compiler) ;maybe tf-language/oplan-tf too
(use-package :oplan-parser-kit)


(defparameter *bind-when-linking* t)


;;;; Use intervals

;;; The min-duration is determined once, when the use-interval is created.
;;; It would be better to keep it up-to-date, and then the slot would have
;;; to be context-layered.  /\/

(defstruct (use-interval (:conc-name ui-))
  resource		;a name
  from			;a node-end tag
  to			;a node-end tag
  (min-duration	0))	;lower bound on duration

(defvar *use-intervals* nil)

(defun all-use-intervals ()
  (ctxt-symbol-value '*use-intervals*))

(defun add-use-interval (ui)
  (push ui (ctxt-symbol-value '*use-intervals*)))

(defun use-constraint->use-interval (uc)
  (check-type (constraint-pattern uc) symbol) 	  ;includes PSVs /\/
  (let ((from (constraint-from uc))
	(to (constraint-to uc)))
    (assert (eq (etag-node from) (etag-node to))) ;must be across <node>
    (make-use-interval
      :resource (constraint-pattern uc)
      :from from
      :to   to
      :min-duration (min-distance-between from to))))


;;;; Parser

(define-constraint-parser (use use) ()
  (let* ((resource (<pattern-component>))
	 (node     (token-case
		     ((tf:across) (<node>))
		     (t           :self))))
    (unless (or (var-pattern-p resource)
		(namep resource))
      (syntax-error "Illegal resource in \"use\" constraint: \"~S\"."
		    resource))
    (make-constraint
      :type 'use
      :pattern resource
      :from (node-end node :begin)
      :to   (node-end node :end))))


;;;; The CM

(define-constraint-manager use-cm (simple-cm))

(register-constraint-manager use-cm
  :constraint-types '(use))


;;;; Initialization

(defmethod cm-init-constraints ((self use-cm))
  (setq *use-intervals* nil)
  nil)


;;;; Sanity check

(defmethod cm-check-constraints ((self use-cm))
  (do ((intervals (all-use-intervals) (cdr intervals)))
      ((null intervals))
    (let ((u (car intervals)))
      (dolist (i (cdr intervals))
	(when (potential-conflict-p u i)
	  (check-error "Use conflict: ~S vs ~S" u i))))))


;;;; Adding constraints

(defmethod cm-add-constraint ((self use-cm) constraint)
  (try-to-add-use-interval
   (use-constraint->use-interval constraint)))

(defun try-to-add-use-interval (u) ; -> t, nil, or an or-tree
  (when (looks-possible-to-add-use-interval-p u)
    (let* ((conflicts (find-possible-conflicts u))
	   (answer (if conflicts
		       (resolve-conflicts u conflicts nil)
		     t)))
      (when answer
	(add-use-interval u))
      answer)))

;;; Check whether there might be room for the interval, given time
;;; limits.

(defun looks-possible-to-add-use-interval-p (u)
  (let* ((related (cons u (find-intervals-possibly-using (ui-resource u))))
	 (resources (possible-resources related))
	 (total-min-time (loop for i in related sum (ui-min-duration i)))
	 (min-time-required (ceiling total-min-time (length resources)))
	 (max-time-available
	  (let ((e (find-best related 
		     #'(lambda (a b) (< (ui-est a) (ui-est b)))))
		(l (find-best related
		     #'(lambda (a b) (&> (ui-lft a) (ui-lft b))))))
	    (&- (ui-lft l)
		(ui-est e)))))
    (or (&>= max-time-available min-time-required)
	(progn
	  (dev-warn "Can't fit ~S in ~S for ~S.~%resources: ~S"
		    min-time-required max-time-available u resources)
	  nil))))

(defun find-intervals-possibly-using (r)
  (let ((result '()))
    (dolist (i (all-use-intervals) (nreverse result))
      (when (might-be-same-resource-p r (ui-resource i))
	(push i result)))))

(defun possible-resources (intervals)
  (let ((result '()))
    (dolist (i intervals result)
      (setq result (union result (ui-possible-resources i))))))

(defun ui-possible-resources (u)
  (let ((r (deref-if-psv (ui-resource u))))
    (if (psv-p r)
	(let ((possibles (psv-get-possibles r))) ;shouldn't be :already-bound
	  (check-type possibles list)
	  possibles)
      (list r))))

;;; Find potentially conflicting use-intervals

(defun find-possible-conflicts (u)
  (let ((conflicts '()))
    (dolist (i (all-use-intervals) (nreverse conflicts))
      (when (potential-conflict-p u i)
	(push i conflicts)))))

(defun potential-conflict-p (u i)
  (and (might-use-same-resource-p u i)
       (might-overlap-p u i)))

(defun might-use-same-resource-p (u i)
  (might-be-same-resource-p
    (deref-if-psv (ui-resource u)) 	;deref not needed /\/
    (deref-if-psv (ui-resource i))))	;deref not needed /\/

(defun might-be-same-resource-p (ur ir)
  (let ((*bindings* nil))		;just in case /\/
    (or (eq ur ir)
	(cond ((psv-p ur)
	       (if (psv-p ir)
		   (or (same-psv-p ur ir) ;needed?  helpful?  /\/
		       (oplan-qa::psv-can-match-psv-p ur ir))
		 (oplan-qa::psv-can-have-value-p ur ir)))
	      ((psv-p ir)
	       (oplan-qa::psv-can-have-value-p ir ur))
	      (t
	       nil)))))

(defun might-overlap-p (u i)
  (not (or (already-ordered-p u i)
	   (already-ordered-p i u))))


;;; Figure out how to resolve conflicts

(defvar *opt-path* nil)

(defun resolve-conflicts (u conflicts opt) ; -> an or-tree or nil
  (let* ((*opt-path* (cons-if opt *opt-path*))
	 (c1 (car conflicts))
	 (ur (deref-if-psv (ui-resource u)))
	 (cr (deref-if-psv (ui-resource c1)))
	 (var nil)
	 (val nil)			;may be a 2nd var
	 (link-options '()))
    ;; Find a variable and value.
    (cond ((psv-p ur)
	   (setq var ur val cr))
	  ((psv-p cr)
	   (setq var cr val ur)))
    ;; See what links might be useful.
    (when (order-looks-ok-p u c1)
      (push `(:link-before ,u ,c1) link-options))
    (when (order-looks-ok-p c1 u)
      (push `(:link-after ,c1 ,u) link-options))
    ;; Build the link tree.  Here we have var = val, because that's
    ;; the only case in which we need to link.
    (let ((link-tree
	   (and link-options
		(build-link-tree u conflicts var val link-options))))
      ;; Build the restrict branch.
      (let ((restrict-branch
	     (and var
		  (psv-restriction-looks-ok-p var :not val)
		  (build-restrict-branch u conflicts var val))))
	(if (null restrict-branch)
	    ;; Can only link (if that).
	    link-tree
	  ;; Can restrict var and maybe link too.
	  (new-or-tree
	    (list-if
	      ;; Branch-1 -- restrict var, don't need links.
	      restrict-branch
	      ;; Branch-2 -- bind var and use links
	      (and link-tree
		   (make-or-branch
		     :actions (when *bind-when-linking*
				`((:bind ,var ,val)))
		     :subtree link-tree)))))))))

(defun build-link-tree (u conflicts var val link-options)
  (new-or-tree
    (if (and var *bind-when-linking*)
	(let* ((opt `(:bind ,var ,val))
	       (*opt-path* (cons opt *opt-path*)))
	  (and (bind-looks-ok-p u var val)
	       (build-link-branches
	         u (filter-conflicts (cdr conflicts) opt) link-options)))
      (build-link-branches u (cdr conflicts) link-options))))

(defun build-link-branches (u conflicts link-options)
  (let ((branches '()))
    (dolist (opt link-options (nreverse branches))
      (let ((remaining (filter-conflicts conflicts opt))
	    (link (apply #'link-to-order (cdr opt))))
	(if remaining
	    (let ((subtree (resolve-conflicts u remaining opt)))
	      (when subtree
		(push (make-or-branch
		        :actions (list link)
		        :subtree subtree)
		      branches)))
	  (push (make-or-branch
		  :actions (list link))
		branches))))))

(defun build-restrict-branch (u conflicts var val)
  (let* ((opt `(:restrict ,var ,val))
	 (remaining (filter-conflicts (cdr conflicts) opt)))
    (if remaining
	(let ((subtree (resolve-conflicts u remaining opt)))
	  (and subtree
	       (make-or-branch
	         :actions (list opt)
		 :subtree subtree)))
      (make-or-branch
        :actions (list opt)))))


;;; Conflict filters

(defun filter-conflicts (conflicts option)
  (remove-if (make-conflict-filter option) conflicts))

(defun make-conflict-filter (option)
  ;; The filter returns true to eliminate a conflict.
  (apply (ecase (car option)
	   (:link-before  #'make-link-before-filter)
	   (:link-after   #'make-link-after-filter)
	   (:restrict     #'make-restrict-filter)
	   (:bind         #'make-bind-filter))
	 (cdr option)))

(defun make-link-before-filter (u c1)
  (declare (ignore u))
  ;; The new interval, u, will be linked before c1.
  ;; So eliminate conflicts that are already after c1.
  #'(lambda (c)
      (already-ordered-p c1 c)))

(defun make-link-after-filter (c1 u)
  (declare (ignore u))
  ;; The new interval, u, will be linked after c1.
  ;; So eliminate conflicts that are already before c1.
  #'(lambda (c)
      (already-ordered-p c c1)))

(defun make-restrict-filter (var val)
  ;; Var = val eliminates c1.
  ;; Eliminate other conflicts that involve the same var.
  (declare (ignore val))
  #'(lambda (c)
      (let ((r (ui-resource c)))
	(and (psv-p r)
	     (same-psv-p var r)))))

(defun make-bind-filter (var val)
  ;; Eliminate any conflict that can't have val as its resource.
  (declare (ignore var))
  #'(lambda (c)
      (not (might-be-same-resource-p val (ui-resource c)))))


;;;; Utilities

(defun already-ordered-p (before after)
  ;; Are the intervals already forced to be in the order before -> after?
  (gop-precedes (ui-to before) (ui-from after)))

(defun order-looks-ok-p (before after)
  (or (gop-link-looks-ok-p (ui-to before) (ui-from after))
      (progn
	(dev-warn "Rejecting link ~S --> ~S." before after)
	nil)))

(defun link-to-order (before after)
  (list :link (ui-to before) (ui-from after)))

(defun deref-if-psv (x)
  (if (and (psv-p x) (psv-has-value-p x))
      (psv-get-value x)
    x))

(defun same-psv-p (u v)
  (or (eq u v)
      (eq (oplan-psv::psv-body u)
	  (oplan-psv::psv-body v))))

(defun cons-if (item list)
  (if item (cons item list) list))

(defun list-if (&rest items)
  (remove nil items))

(defun min-distance-between (from-etag to-etag)
  ;; The returned value can be too low, but not too high.
  (let ((from-tp (ne-time-point (get-node-end from-etag)))
	(to-tp (ne-time-point (get-node-end to-etag)))
	(delta 0))
    (dolist (constraint (tpoint-post-con from-tp)
			delta)
      (when (and (eq (tcon-pre-point constraint) from-tp)
		 (eq (tcon-post-point constraint) to-tp))
	(setq delta (max delta (tcon-min constraint)))))))

(defun psv-restriction-looks-ok-p (var type val)
  (assert (eq type :not))
  (assert (psv-p var))
  (cond ((and (psv-p val) (same-psv-p var val))
	 (dev-warn "Can't have ~S /= ~S." var val)
	 nil)
	(t t)))

#+:undef
(defun psv-restriction-looks-ok-p (var type val)
  (assert (eq type :not))
  (assert (psv-p var))
  (push-context)
  (prog1 (or (psv-add-restriction var type val)
	     (progn
	       (dev-warn "Can't have ~S /= ~S." var val)
	       nil))
    (pop-context)))

;;; In the end, a PSV will have a single value.

(defun bind-looks-ok-p (u var val)
  (assert (psv-p var))
  ; (assert (might-be-same-resource-p var val))
  (assert (equal (car *opt-path*) `(:bind ,var ,val)))
  ; (assert (not (member u (all-use-intervals))))
  (or (sequence-looks-ok-p u var)
      (progn
	(dev-warn "Seq loses for var ~S." var)
	nil)))

(defun sequence-looks-ok-p (u var)
  (let* ((intervals-using-var
	  (find-intervals-using-var var (cons u (all-use-intervals))))
	 (sorted
	  (sort-by-lft-then-est intervals-using-var))
	 (sequence-est 0))
    (do ((intervals sorted (cdr intervals)))
	((null intervals) t)
      (let* ((i (car intervals))
	     (e (ui-est i)))
	;; If i and all intervals sorted after i have an est later
	;; than sequence-est, we can increase sequence-est.
	(when (and (> e sequence-est)
		   (loop for other-i in (cdr intervals)
			 never (< (ui-est other-i) e)))
	  (setq sequence-est e))
	(incf sequence-est (ui-min-duration i))
        ;; All the intervals seen so far must fit in a sequence
	;; before (ui-lft i): because of the sort above, we know
	;; that none of those intervals has a later lft.
	(let ((l (ui-lft i)))
	  (when (infp l)
	    (return t))
	  (when (> sequence-est l)
	    (return nil)))))))

(defun find-intervals-using-var (var candidate-intervals)
  (let ((vars (find-equivalent-vars var)))
    (remove-if-not #'(lambda (i)
		       (and (psv-p (ui-resource i))
			    (member (ui-resource i) vars)))
		   candidate-intervals)))

(defun find-equivalent-vars (var)
  (assert (psv-p var))
  (let ((vars (oplan-psv::psv-body-tags (oplan-psv::psv-body var))))
    (dolist (opt *opt-path* vars)
      (when (eq (first opt) :bind)
	(let ((opt-var (second opt))
	      (opt-val (third opt)))
	  (assert (psv-p opt-var))
	  (if (eq opt-var var)
	      (when (psv-p opt-val)
		(pushnew opt-val vars))
	    (if (eq opt-val var)
		(pushnew opt-var vars))))))))

(defun ui-est (u)
  (tpoint-min (ne-time-point (get-node-end (ui-from u)))))

(defun ui-lft (u)
  (tpoint-max (ne-time-point (get-node-end (ui-to u)))))
    
(defun sort-by-lft-then-est (intervals)
  ;; Sorts by lft and w/in a given lft by est
  (stable-sort (copy-list intervals)
	       #'(lambda (a b)
		   (or (&< (ui-lft a) (ui-lft b))
		       (and (&= (ui-lft a) (ui-lft b))
			    (&< (ui-est a) (ui-est b)))))))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;

