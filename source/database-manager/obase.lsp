;;;; File: obase.lsp
;;; Contains: Low-level database support
;;; Author: Jeff Dalton
;;; Created: Mon Jan 24, 1994 by Jeff Dalton
;;; Updated: Sun Mar 21 04:30:15 1999 by Jeff Dalton
;;; Copyright: (c) 1992, 1993, 1994, 1995, 1996, AIAI, University of Edinburgh

;;; This is in intention a completely new system, only slightly
;;; related to past versions of OBase.  However, this version is
;;; preliminary and designed to fit into O-Plan 2.1+ rather than 2.2.
;;; Consequently, it contains a number of attempts at backwards
;;; compatibility.

(in-package :oplan-obase :nicknames '(:act))
(use-package :oplan-util)
(use-package :oplan-ctxt)
(use-package :oplan-tf)

;; Base exports
(export '(item itemp value setvalue
          obmatch obmatch3 safe-obmatch3 getashortlist
          ?? item-inst))

;; New exports
(export '(init-obase clear-obase))
(export '(has satisfies))
(export '(*bindings*))

;; Extras
(export '(generator try-next))

;; Actor exports
(export '(actorp actorsym make-instantiable instant instance obvars
	  non actand actor given))

(export '(act-satisfies))

(export '(actfn actargs))

(export '(make-given-actor make-function-actor actor-rename))

;; Obactor-psvs exports
(export '(get-tuple var-name var-value var-not-sames var-restrictions))
(export '(unified-p unified-var))
(export '(var-deref-value var-really-deref-value))
(export '(make-var make-tuple add-to-not-sames))
(export '(make-not-value-actor make-and-actor make-given-actor bound))
(export '(fully-instantiable-p
	  fully-instantiate
	  fully-instantiated-p
	  pattern-vars))
(export '(evaluate-compute-condition))
(export '(psv-can-match-psv-p
	  psv-can-have-value-p))

;; A global variable to hold variable bindings throughout a match.
(defvar *bindings* nil)

(defconstant actorsym '??)


;;;; DB items

(defstruct (dbitem (:predicate itemp)
		   (:print-function print-dbitem))
  identifier
  %value)			;context-layered

(define-context-accessor dbitem-value dbitem-%value)

(defun print-dbitem (item stream depth)
  (if (and (numberp *print-level*) (> depth *print-level*))
      (format stream "#")
    (let ((id (dbitem-identifier item))
	  (val (dbitem-value item)))	;value in current context
      (if (eq val :undef)
	  (format stream "#<dbitem ~S>" id)
	  (format stream "#<dbitem ~S -> ~S>" id val)))))


;;;; Shortlists / DB indexing

(defvar *all-indexed-names* nil)

(defvar *all-table-names* nil)

(defun get-a-shortlist (table-name pattern) ; -> ilist
  ; (assert (member table-name *all-table-names*))
  (assert (indexable-pattern-p pattern))
  (get (car pattern) table-name))

(defun get-table-items (table-name)
  (get table-name 'all-items))

(defun indexable-pattern-p (p)
  (and (consp p) (symbolp (car p)) (not (eq (car p) actorsym))))

(defun add-obase-item (table-name pattern id &optional (value :undef))
  (assert (indexable-pattern-p pattern))
  (let ((shortlist (get (car pattern) table-name)))
    #+:paranoid
    (assert (not (member id (cdr shortlist)
			 :test #'equal
			 :key #'dbitem-identifier)))
    (let ((new-item (make-dbitem :identifier id :%value value)))
      ;; Note that table exists.
      (pushnew table-name *all-table-names* :test #'eq)
      ;; Record the new item as a table entry.
      (setf (get table-name 'all-items)
	    (cons-ilist new-item (get table-name 'all-items)))
      ;; Index new item.
      (setf (get (car pattern) table-name)
	    (cons-ilist new-item shortlist))
      ;; Remember that it's indexed.
      (pushnew (car pattern) *all-indexed-names* :test #'eq)
      ;; And return it.
      new-item)))

(define-initializer :dm clear-obase ()
  (ensure-obase-2.1-compatibility)
  ;; Clear indexing
  (dolist (name *all-indexed-names*)
    (dolist (table *all-table-names*)
      (when (get name table)
	(setf (get name table) nil))))
  ;; Clear tables
  (dolist (table *all-table-names*)
    (setf (get table 'all-items) nil))
  ;; Reset variables.
  (setq *all-indexed-names* nil)
  (setq *all-table-names* nil)
  nil)


;;;; ilists

;;; An ilist is a list which is either null or (integer item1 item2 ....) 
;;; where integer > 0 is the number of items in the ilist.  An ilist can
;;; be constructed in 3 ways:
;;;   - (cons-ilist new-item ilist) returns the ilist that results from
;;;        adding new-item to ilist,
;;;   - (make-ilist list) makes an ilist from a list of items,
;;;   - (make-ilist1 item) creates an ilist containing the single item.

(defun cons-ilist (new-item ilist)
  (if (null ilist)
      (make-ilist1 new-item)
    (cons (1+ (car ilist))
	  (cons new-item (cdr ilist)))))

(defun make-ilist (list)
  (and list (cons (length list) list)))

(defun make-ilist1 (item)
  (cons 1 (list item)))


;;;; 2.1 compatibility

;;; TOME entries look like this:
;;; (TOME <patt> <tome-at-node-end>) = <val>
;;;
;;; <tome-at-node-end> = :ALWAYS | (<node> <end>) |
;;;                                   (<node> <end> :ALWAYS . <bindings>)

;;; GOST entries look like this:
;;; (GOST <cond-type> <patt> <val> <at-node-end>) =
;;;     ( ... (<contributor> . <satisfaction method>) ... )

(defvar *tome* nil)
(defvar *gost* nil)

(defun ensure-obase-2.1-compatibility ()
  (unless *tome*
    (setq *tome* (find-symbol "TOME" "OPLAN-TGM"))
    (setq *gost* (find-symbol "GOST" "OPLAN-TGM"))
    (assert *tome*)
    (assert *gost*)))

(defun getashortlist (x)
  (%getashortlist x))

(defun %getashortlist (x)
  (assert (consp x))
  (let* ((table-name (car x))
	 (pattern
	  (cond ((eq table-name *tome*) (cadr x))
		((eq table-name *gost*) (caddr x))
		(t (error "Unknown table ~S for ~S." table-name x)))))
    (if (eq pattern actorsym)
	(get-table-items table-name)
      (get-a-shortlist table-name pattern))))

(defun %add-item (x)
  (assert (consp x))
  (let* ((table-name (car x))
	 (pattern
	  (cond ((eq table-name *tome*) (cadr x))
		((eq table-name *gost*) (caddr x))
		(t (error "Unknown table ~S for ~S." table-name x)))))
    (add-obase-item table-name pattern x)))


;;;; Functions to screen ilists

;;; (get-imatch-in ilist id) returns the first item in ilist whose
;;; identifier successfully matches id.  If no match is found, nil
;;; is returned.

(defun get-imatch-in (ilist id)
  (dolist (item (cdr ilist))
    (when (obmatch item id)
      (return item))))


;;;; Item lookup / installation

;;; getaitem returns an existing item matching id.  If no existing item
;;; matches, no new item is created and nil is returned.

(defun getaitem (id)
  (cond ((itemp id) id)
	((consp id) (get-imatch-in (%getashortlist id) id))
	(t
	 (error "Can't getaitem ~S." id))))

;;; item finds or constructs a database item for x.

(defun item (x)
  (cond ((get-imatch-in (%getashortlist x) x))
	((consp x)
	 (%add-item x))
	(t
	 (error "Can't item ~S." x))))

(defun value (identifier)
  (cond ((itemp identifier) (dbitem-value identifier))  ; accessed in context
	((consp identifier) (value (getaitem identifier)))
	((null identifier)  :undef)	;/\/ keep tgm happy...
	(t
	 (error "Can't do value of ~S." identifier))))

;;; modified BAT 26Jan87 to always do (item identifier) first

(defun setvalue (identifier new-value)
  (let ((item (item identifier)))
    (assert (itemp item))
    (setf (dbitem-value item) new-value))) ; updated in context

(defsetf value setvalue)		;??? [jwd] /\/


;;;; The obase pattern matcher

;;; We take advantage of the fact that dbitem ids no longer contain
;;; other dbitems to move all itemp checks out of the main matcher,
;;; retaining them only in a separate "outer level" function.

(defun obmatch (t1 t2)
  (and (%obmatch (item-name t1) (item-name t2))
       (or *bindings* t)))

(defun %obmatch (t1 t2)
  (declare (optimize (speed 3)))
  (cond ((or (eq t1 actorsym) (eq t2 actorsym))
	 t)
	((and (actorp t1) (not (actorp t2)))
	 (applyact t1 t2))
	((actorp t2)
	 (cond ((not (actorp t1)) (applyact t2 t1))
	       ;; so t1 AND t2 are actors
	       ((instant t1) (applyact t1 t2))
	       ((instant t2) (applyact t2 t1))
	       ;; both actors and neither instantiable
	       (t (error "Cannot match 2 non-instatiable actors: ~S and ~S."
			 t1 t2))))
	((symbolp t1)
	 (eq t1 t2))
	((consp t1)
	 (and (consp t2)
	      (%obmatch (car t1) (car t2))
	      (%obmatch (cdr t1) (cdr t2))))
	(t (equal t1 t2))))

(defun item-name (item)
  (if (itemp item)
      (dbitem-identifier item)
    item))


(defun obmatch3 (t1 t2 bindings)
  ;; Used in QA -- /\/ but is this really what was intended?
  ;; /\/ Now also used in the sanity checker and in generators.
  (let ((*bindings* bindings))
    (obmatch t1 t2)))

(defun safe-obmatch3 (t1 t2 bindings)
  (obmatch3 t1 t2 (copy-tree bindings)))


;;;; DB items -> lisp objects

;;; This is now trivial...

(defun item-inst (x)			;/\/ same as item-name
  (if (itemp x)
      (dbitem-identifier x)
    x))


;;;; Generators

(defun generator (id v)
  (let ((possible (%getashortlist id)))
    (when (consp possible)
      (pop possible))			;turn ilist into ordinary list
    #'(lambda ()
	(loop
	  (when (null possible)
	    (return nil))
	  (let ((p (pop possible)))
	    (when (and (obmatch3 (dbitem-identifier p) id nil)
		       (obmatch3 (value p) v nil))
	      (return p)))))))

(defun try-next (gen)
  (funcall gen))


;;;; Actor functions

;;; The format of a valid actor is (?? :*act (<fn> . <args>))

(defun actorp (x)
  (and (consp x)
       (eq (car x) actorsym)
       (consp (cdr x))
       (eq (cadr x) :*act)
       (or (and (consp (caddr x))		;(<fn> . <args>)
		(symbolp (car (caddr x))))	;<fn>
	   (error "invalid actor: ~S." x))))

;;; Actor name -> function name

(defmacro actor-rename (name) `(get ,name 'actor-rename ,name))

;;; Actor constructors

(defun-inline make-given-actor (var-name)
  `(,actorsym :*act (given ,var-name)))

(defun-inline make-function-actor (fn args)
  `(,actorsym :*act (,(actor-rename fn) . ,args)))

;;; Actor accessors

(defun actfn (actor)
  (car (caddr actor)))

(defun actargs (actor)
  (cdr (caddr actor)))

(defun applyact (actor thing)
  (cond ((actargs actor)
	 (apply (actfn actor) 
		thing
		(actargs actor)))	;a flat list
	(t
	 (funcall (actfn actor) thing))))


;;;; Actors

(setf (actor-rename 'not) 'non)
(setf (actor-rename 'and) 'actand)
(setf (actor-rename 'or)  'actor)

(defun non (object pattern)
  (not (obmatch object pattern)))

(defun actand (object &rest things)
  (every #'(lambda (x)
	     (obmatch object x))
	 things))

(defun actor (object &rest things)
  (some #'(lambda (x)
	    (obmatch object x))
	things))

(defun has (object fn &rest arg-result-list)
  (let ((result (car (last arg-result-list)))
	(args (butlast arg-result-list)))
    (obmatch (apply fn (instance object) (mapcar #'instance args))
	     result)))

;;; ?{satisfies fn arg...} is like ?{has fn arg... true} except that
;;; the "true" is omitted and any Lisp true value will do.  This is
;;; a win, because Lisp doesn't normally return "true" and we can't
;;; always rely on it returning t either.

(setf (actor-rename 'satisfies) 'act-satisfies)

(defun act-satisfies (object fn &rest args)
  (apply fn (instance object) (mapcar #'instance args)))


;;; Instantiable actors

(defun make-instantiable (x)
  (setf (get x 'instantiable-actor) t))

(defun instant (x)
  ;; is x an instantiable actor  (?? :*act (given ...))
  (get (actfn x) 'instantiable-actor))

(defun instact (x)
  (cond ((instant x) (applyact x actorsym))
        (t x)))

(defun instance (x &optional (bindings nil bindings-passed-in-p))
  (when bindings-passed-in-p (setq *bindings* bindings))
  (cond ((and (actorp x) (instant x))
	 (instact x))
        ((listp x)			; also does non-instantiable actors
	 (mapcar 'instance x))  
        (t x)))

(eval-when (load eval)
  (make-instantiable 'given))


;;;; ?-notation reader.

;;; Pattern match actors have the following forms
;;;   ??              matches anything
;;;   ?name           matches anything first time and binds the variable;
;;;                     after that acts as a restriction on binding;
;;;   ?(fn arg1 ...)  matches anything where (fn ITEM arg1 ... ) evals to t.
;;;
;;; All other actor forms are illegal; they will be replaced by ??.

;;; Error "?? not a defined function" will show up if an actor is
;;; given at an actor level with no list quote.  Use '?(fn ... ) if
;;; needed.  /\/: There used to be a check for embedded quotes for
;;; this case, with a warning if any were found; but since quoted
;;; actors are uncommon the way TF is used these days, and since
;;; embedded quotes aren't necessarily wrong, I removed the check.
;;; -- jd, 28 Jan 94.

(defun question-reader (stream char)
  (declare (ignore char))
  (let ((next (peek-char nil stream t nil t)))
    (cond ((eql next #\?)
	   ;; We have ?? -- so discard the 2nd ? and return actorsym
	   (read-char stream t nil t)
	   actorsym)
	  (t
	   (let ((body (read stream t nil t)))
	     (cond ((and (symbolp body) (not (null body)))
		    ;; It's ?name, i.e. a variable.
		    (make-given-actor body))
		   ((and (consp body)
			 (not (eq (car body) actorsym)))
		    ;; It's ?{fn ...}, but we sometimes have to
		    ;; rename fn to get the name that's actually
		    ;; defined as a function.  [See above.]
		    (make-function-actor (car body) (cdr body)))
		   (t (cerror "Pretend it's ??."
			      "Illegal matcher form: ?~S" body)
		      actorsym)))))))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
;;;  jwd  05 Jan 99	Changed eql to eq in %obmatch in the comparisons
;;;			with actorsym.
;;;
