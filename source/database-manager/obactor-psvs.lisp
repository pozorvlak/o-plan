;;;; File: obactor-psvs.lisp
;;; Contains: PSV extensions to OBACTOR package.
;;; Author: Richard Kirby (rbk)
;;; Created: Thu Jul 11 17:22:23 1991
;;; Updated: Sun Mar 21 05:56:22 1999 by Jeff Dalton
;;; Copyright: (c) 1992, AIAI, University of Edinburgh

(in-package :oplan-obase)

(use-package :oplan-util)
(use-package :oplan-developerlib)
(use-package :oplan-psv)

;;; *bindings* is an alist of the following form:
;;;
;;;   (...(var value not-sames restrictions)...)
;;;
;;; If value is :undef then the restrictions are used for matching
;;; purposes.  If value is (var <varname>) then this var has been
;;; unified with <varname> and that var's value/restrictions should be
;;; used.  If value is an object then that is used.

(defun get-tuple (x &optional (bindings *bindings*))
  (assoc x bindings))

(defstruct (var (:type list)
		(:constructor make-var)
		(:constructor make-tuple (name value not-sames restrictions)))
  name
  value
  not-sames
  restrictions
  (possible-values :unknown))

(defun unified-p (x) (and (consp x) (eq (car x) '*var)))

(defun unified-var (x) (cdr x))

;; Handles unified variables.
;; NOTE: unify-vars guarentees that for unified variables we only have one
;; level of indirection to go through to get the unified bodies.
(defun get-var-body (var)
  (let ((tuple (get-tuple var)))
    (unless tuple
      ;; No entry for var on binding list at present, so make an entry from
      ;; the PSV body.
      (assert (psv-p var))
      (setq tuple (psv-create-binding-tuple var))
      (push tuple *bindings*))
    (let ((value (var-value tuple)))
      (if (unified-p value)
	  ;; Get body of unified variable.
	  (let ((next-tuple (get-var-body-1 (cdr value))))
	    (assert (not (unified-p (var-value next-tuple))))
	    next-tuple)
	tuple))))

(defun get-var-body-1 (var)
  ;; Does one step of get-var-body, i.e. without dereferencing
  ;; when the variable has been unified.
  (let ((tuple (get-tuple var)))
    (unless tuple
      ;; No entry for var on binding list at present, so make an entry from
      ;; the PSV body.
      (setq tuple (psv-create-binding-tuple var))
      (push tuple *bindings*))
    tuple))

(defun var-deref-value (tuple)
  (let ((value (var-value tuple)))
    (if (unified-p value)
	(cdr value)
	value)))

(defun var-really-deref-value (tuple &optional (bindings *bindings*))
  ;; /\/: Why the optional arg?
  (declare (ignore bindings))
  (let ((value (var-value tuple)))
    (if (unified-p value)
	(var-really-deref-value (get-tuple (cdr value)))
	value)))

;;;; Functions for compute conditions

;;; /\/: No separate functions should be needed.  But the existing
;;; functions all have various problems.  Some are in the TF compiler
;;; (hence in the wrong module), others have bugs or fail to do
;;; exactly what we need.

;;; "compute p = v" is used first as a schema filter.  At that point,
;;; neither p nor v contain PSVs.  Later (KS-COMPUTE), they may contain
;;; PSVs.  In that case, psv-actorise-pattern should be called before
;;; calling the routines here.

(defun fully-instantiable-p (pat &optional *bindings*)
  (label fip ((p pat))
    (if (atom p)
	t
      (if (given-p p)
	  (var-has-value-p (car (actargs p)))
	(and (fip (car p))
	     (fip (cdr p)))))))

(defun fully-instantiate (pat &optional *bindings*)
  (label fi ((p pat))
    (if (atom p)
	p
      (if (given-p p)
	  (let ((value (get-var-value (car (actargs p)))))
	    (assert (not (eq value :undef)))
	    (assert (not (unified-p value)))
	    value)
	(recons p (fi (car p))
		  (fi (cdr p)))))))

(defun fully-instantiated-p (pat)
  (if (psv-p pat)
      (psv-has-value-p pat)		;was nil [JD, 21 Mar 99]
    (if (atom pat)
	t
      (if (given-p pat)
	  nil
	(and (fully-instantiated-p (car pat))
	     (fully-instantiated-p (cdr pat)))))))

(defun unbound-pattern-vars (pat &optional *bindings*)
  (let ((names '()))
    (label scan ((p pat))
      (cond ((given-p p)
	     (let* ((name (car (actargs p)))
		    (value (get-var-value name)))
	       (when (eq value :undef)
		 (nconcf-new names name :test #'eq))))
	    ((consp p)
	     (scan (car p))
	     (scan (cdr p)))))
    names))

(defun var-has-value-p (var)
  (not (eq :undef (get-var-value var))))

(defun get-var-value (var)
  ;; Var is the name of a variable.
  (let* ((var-tuple (get-var-body-1 var))
	 (value (var-value var-tuple)))
    (cond ((unified-p value)
	   (get-var-value (unified-var value)))
	  ((eq value :undef)
	   :undef)
	  ((psv-p value)		;should this happen?  /\/
	   (psv-get-value value))
	  (t value))))


;;;; Queries

;;; c-val is often a contributor value and d-val a deleter value.  See QA.

(defun psv-can-match-psv-p (c-val d-val)
  (or (eq c-val d-val)
      (let ((c-v (get-var-body c-val))
	    (d-v (get-var-body d-val)))
	(and (not (member (var-name d-v) (var-not-sames c-v)))
	     (if (not (eq (var-value c-v) :undef))
		 ;; c-v has a value
		 (if (not (eq (var-value d-v) :undef))
		     ;; c-v and d-v both have values
		     (obmatch (var-value d-v) (var-value c-v))
		   ;; c-v has a value, but d-v doesn't
		   (obmatch (var-restrictions d-v) (var-value c-v)))
	       ;; c-v has no value
	       (if (not (eql (var-value d-v) :undef))
		   ;; c-v has no value but d-v has a value
		   (obmatch (var-value d-v) (var-restrictions c-v))
		 ;; neither c-v nor d-v has a value.
		 ;; at this point, v-val and d-val really have to
		 ;; be PSVs, because we look at the possibles cache.
		 (not (disjoint-sets-p
		        (psv-possible-values c-val)
			(psv-possible-values d-val)))))))))

(defun psv-possible-values (psv)
  (let ((psv-s (oplan-psv::psv-body psv)))
    (assert psv-s)
    (oplan-psv::psv-body-possibles-cache psv-s)))

(defun psv-can-have-value-p (psv val)
  (let ((v (get-var-body psv)))
    (if (eq (var-value v) :undef)
	(obmatch (var-restrictions v) val)
      (obmatch (var-value v) val))))


;;;; Unification

;; Modifies tuple1 to include tuple2's not-sames and restrictions, and modifies
;; value field of tuple2 to point to tuple1.
(defun amalgamate-tuples (tuple1 tuple2)
  (let ((not-sames1 (var-not-sames tuple1))
	(not-sames2 (var-not-sames tuple2)))
    ;; Foreach not-sames of tuple1 add the var of tuple2 to their not-sames.
    (dolist (x (set-difference not-sames1 not-sames2))
      (push (var-name tuple2) (var-not-sames (get-var-body x))))
    ;; Now visa-versa
    (dolist (x (set-difference not-sames2 not-sames1))
      (push (var-name tuple1) (var-not-sames (get-var-body x))))
    ;; Set tuple1's not-sames list.
    (setf (var-not-sames tuple1) (union (var-not-sames tuple1)
					(var-not-sames tuple2)))
    ;; Now add the restrictions together, and place in tuple1. Do simply for
    ;; now.
    (if (var-restrictions tuple2)
	(setf (var-restrictions tuple1)
	      (list '?? :*ACT `(actand ,(var-restrictions tuple1)
				,(var-restrictions tuple2)))))
    ;; If tuple1 doesn't have a value but tuple2 does, then set tuple1 to the
    ;; value.
    (if (eq (var-value tuple1) :undef)
	(if (not (eq (var-value tuple2) :undef))
	    (setf (var-value tuple1) (var-value tuple2))))
    ;; Finally point tuple2 at tuple1.
    (setf (var-value tuple2) (cons '*var (var-name tuple1)))))

;; Makes two variables the same - handles already unified vars and guarentees
;; that only one variable has the amalgamated bodies, the rest just point to
;; this one var.
(defun unify-vars (var1 var2)
  (let* ((tuple1 (get-tuple var1))
	 (value1 (var-value tuple1))
	 (tuple2 (get-tuple var2))
	 (value2 (var-value tuple2)))
    (if (unified-p value1)
	(let ((unified-body1 (get-tuple (cdr value1))))
	  (if (unified-p value2)
	      (let ((unified-body2 (get-tuple (cdr value2))))
		;; Just check not unified to the same var already, in which
		;; case nothing to do.
		(unless (eq (var-name unified-body1)
			    (var-name unified-body2))
		  ;; So, both var1 and var2 are already unified, to
		  ;; different variables. So What we do is pick var1's
		  ;; unified var as the main one, make var2 point to this
		  ;; along with unified-body2 and then search the alist
		  ;; for any others that were unified to unified-body2,
		  ;; and make them point to unified-body1. Got that?!?
		  (amalgamate-tuples unified-body1 unified-body2)
		  (dolist (x *bindings*)
		    (let ((value-x (var-value x)))
		      (if (and (unified-p value-x)
			       (eq (cdr value-x)
				   (var-name unified-body2)))
			  (setf (cdr value-x)
				(var-name unified-body1)))))))
	      (unless (eq (var-name unified-body1) (var-name tuple2))
		;; Point var2 to the unified var of var1
		(amalgamate-tuples unified-body1 tuple2)
		;; Now point any that pointed to var2 to unified var of
		;; var1.
		(dolist (x *bindings*)
		  (let ((value-x (var-value x)))
		    (if (and (unified-p value-x)
			     (eq (cdr value-x)
				 (var-name tuple2)))
			(setf (cdr value-x)
			      (var-name unified-body1))))))))
	(if (unified-p value2)
	    (let ((unified-body2 (get-tuple (cdr value2))))
	      (unless (eq (var-name unified-body2) (var-name tuple1))
		;; Point var1 to the unified var of var2
		(amalgamate-tuples unified-body2 tuple1)
		(dolist (x *bindings*)
		  (let ((value-x (var-value x)))
		    (if (and (unified-p value-x)
			     (eq (cdr value-x)
				 (var-name tuple1)))
			(setf (cdr value-x)
			      (var-name unified-body2)))))))
	    (unless (eq (var-name tuple1) (var-name tuple2))
	      ;; Unify var1 and var2
	      (amalgamate-tuples tuple1 tuple2)
	      (dolist (x *bindings*)
		(let ((value-x (var-value x)))
		  (if (and (unified-p value-x)
			   (eq (cdr value-x)
			       (var-name tuple2)))
		      (setf (cdr value-x)
			    (var-name tuple1))))))))))

;; T if arg is a given actor.
(defun given-p (x)
  (and (actorp x) (eq (actfn x) 'given)))

;; Set value of the tuple to be obj, and add a ?{not obj} restriction to all
;; the not sames of this var.
(defun set-var-value (tuple obj)
  (setf (var-value tuple) obj)
  (dolist (not-same (var-not-sames tuple))
    (let ((not-sames-tuple (get-var-body not-same)))
      (setf (var-restrictions not-sames-tuple)
	    (make-and-actor (make-not-value-actor obj)
			    (var-restrictions not-sames-tuple))))))

#|
Algorithm for given:

Passed in two args, the first being the object to match against, the second
being the variable alist name.

01) Get var body (ie if var is unified then get the body of the other var)
02) If the object is not a variable then goto 11
03) Get object name
04) If object name on not-sames of var body then fail match
05) Get object body (ie if object is unifed then get the body of the other var)
06) If object body does not have a value then goto 09
07) If var has value, and the values match then unify var with object else fail
08) If var restrictions match with object value then unify var with object
    else fail
09) If var has value, and the value matches with the object restrictions then
    unify var with object else fail
10) Unify var with object
11) If var has value, if value matches object then ok else fail
12) If var restrictions match object and the object does not match any of the
    not sames variables which have vales, then set var to have value = object
    else fail
|#

;; Redefinition of given.
(defun given (obj var)
  ;; If obj is actually a var then we need to use either its value or if it
  ;; has been unified with another variable, then that variable's value
  ;; (or restriction list)
  (let ((var-tuple (get-var-body var)))
    (if (eq obj actorsym)
	;; Used when instantiating a pattern (see instance).
	(instance-var var)
	;; Normal role of given actor. Used from the matcher.
	(if (given-p obj)
	    ;; Matching against another variable (ie unifying).
	    (let ((obj-tuple (get-var-body (car (actargs obj)))))
	      (if (member (var-name obj-tuple) (var-not-sames var-tuple))
		  nil
		  (if (not (eq (var-value obj-tuple) :undef))
		      ;; Obj has a value
		      (if (not (eq (var-value var-tuple) :undef))
			  ;; Var has a value too
			  (if (obmatch (var-value var-tuple)
				       (var-value obj-tuple))
			      (progn
				(unify-vars (var-name var-tuple)
					    (var-name obj-tuple))
				t))
			  ;; Var doesn't have a value
			  (if (obmatch (var-restrictions var-tuple)
				       (var-value obj-tuple))
			      (progn
				(unify-vars (var-name var-tuple)
					    (var-name obj-tuple))
				t)))
		      ;; Obj doesn't have a value
		      (if (not (eq (var-value var-tuple) :undef))
			  ;; But var does have a value
			  (if (obmatch (var-value var-tuple)
				       (var-restrictions obj-tuple))
			      (progn
				(unify-vars (var-name var-tuple)
					    (var-name obj-tuple))
				t))
			  ;; Neither has a value
			  (if (tuples-might-match-p var-tuple obj-tuple)
			      (progn
				(unify-vars (var-name var-tuple)
					    (var-name obj-tuple))
				t))))))
	    ;; Matching against an object.
	    (if (not (eq (var-value var-tuple) :undef))
		(obmatch (var-value var-tuple) obj)
		(if (obmatch (var-restrictions var-tuple) obj)
		    (and
		     ;; Check that if any of this vars not-sames have values,
		     ;; that their values do not match this value.
		     (every #'(lambda (not-same)
				(let ((not-same-value
				       (var-value (get-var-body not-same))))
				  (or (eq not-same-value :undef)
				      (null (obmatch not-same-value obj)))))
			    (var-not-sames var-tuple))
		     (progn
		       (set-var-value var-tuple obj)
		       t))))))))

;;; In tuples-might-match-p, we are matching two variables, neither has
;;; a value, and we want to detect cases where we can tell for sure that
;;; they cannot match.  In such cases, we return false; otherwise we
;;; return true.  (We're assuming that false positives (where we say
;;; they might match, but, really, they can't) are ok, but that false
;;; negatives are not.)

;;; Amazingly enough, for most of its life O-Plan did not perform
;;; any check of this sort at all -- it just assumed (when matching)
;;; that any two variables without values could match. [jd 13 Sep 98]

(defun-inline find-var-possible-values (v) ; -> list or :unknown
  ;; First look at the var's list of possible values.
  ;; At present, this exists only for tuples made from PSVs.
  (let ((p (var-possible-values v)))
    (if (not (eq p :unknown))
	p
      ;; If it's :unknown, and it's possible to determine the variable's
      ;; type, get the type's list of possible values.
      (let ((type (psv-get-type-from-restrictions (var-restrictions v))))
	(if type
	    (psv-get-possibles-for-type type)
	  :unknown)))))

(defun tuples-might-match-p (v1 v2)
  (assert (eq (var-value v1) :undef))
  (assert (eq (var-value v2) :undef))
  ;; Look at the lists of possible values, if they're available.
  (let ((p1 (find-var-possible-values v1))
	(p2 (find-var-possible-values v2)))
    ;; If the possible-values lists are disjoint, the vars cannot match.
    (if (and (not (eq p1 :unknown))
	     (not (eq p2 :unknown))
	     (disjoint-sets-p p1 p2))
	(progn
	  #+:undef
	  (format t "~&Vars ~S and ~S are disjoint:~%~S: ~S~%~S: ~S~%"
		  (var-name v1) (var-name v2)
		  (var-name v1) p1
		  (var-name v2) p2)
	  nil)
      ;; Otherwise, we'll assume they can match.
      t)))

;; Instance-var is called when we're instantiating a pattern
;; (see the OBase function instance).
;;
;; At this point (see db-instantiate-schema), any schema vars
;; that are becoming PSVs will have a symbol PSV-i as their value.
;; But this happens only if their value had been :undef, not if,
;; say, they'd been unified with another var.
;;
;; This also means the value of a schema var can't now be :undef, 
;; because :undef would have been replaced by some PSV-i.

(defun instance-var (var)
  ;; Var is the name of a variable.
  (if (psv-p var)
      var
    (let* ((var-tuple (get-var-body-1 var))
	   (value (var-value var-tuple)))
      (cond ((unified-p value)
	     (instance-var (unified-var value)))
	    ((eq value :undef)
	     (error "Attempt to instantiate undefined var ~S." var))
	    ((psv-p value)
	     value)
	    (t value)))))

;; Older method, now unused.  The code remains for emergency backup /\/.
;;
;; If var has a value return it, otherwise return the current
;; restrictions.  Used when instantiating a pattern.
;;
#+:undef
(defun old-instance-var (var)
  (let* ((var-tuple (get-var-body var))
	 (value (var-value var-tuple)))
    (if (eq value :undef) (var-restrictions var-tuple) value)))

;; An actor for checking if a variable is bound. Returns t if this is not a
;; variable.
(defun bound (x)
  (cond ((or (oplan-psv::psv-p x)
	     (assoc x *bindings*))
	 ;; Its a schema var or PSV, so check if it has a value
	 (not (eq (var-value (get-var-body x)) :undef)))
	((actorp x)
	 (break "In bound - an actor"))
	((listp x)
	 (every #'bound x))
	(t t)))



;; x is a symbol representing a variable. This returns a given actor for this
;; symbol for use by the matcher.
;; /\/: Now in obase.lisp
#+:undef
(defun make-given-actor (x)
  (list actorsym :*act (list 'given x)))

(defun make-not-value-actor (value)
  (list actorsym :*act (list 'non value)))

(defun make-and-actor (&rest clauses)
  (list actorsym :*act (list* 'actand clauses)))

;; Adds var2 to the not-sames of var1 and vice-versa, taking care of
;; unified variables.
(defun add-to-not-sames (var1 var2)
  (let ((var1-tuple (get-var-body var1))
	(var2-tuple (get-var-body var2)))
    (push var2 (var-not-sames var1-tuple))
    (push var1 (var-not-sames var2-tuple))))

;;;; Pretty printing functions.

(defun actor-print-fn (s obj)
  (if (eq (actfn obj) 'given)
      (xp-format s "?~W" (car (actargs obj)))
      (xp-format s "~@<?{~;~W~{ ~W~}~;}~:>" (actfn obj) (actargs obj))))

(eval-when (load eval)
  #+kcl
  (set-pretty-printer '(xp::cons (xp::member ??)) #'actor-print-fn)
  #-kcl
  (set-pretty-printer '(cons (member ??)) #'actor-print-fn))

;;;; ---------------------------- Change History ----------------------------
;;;; (Initials) (Date of change)           (Comment)
