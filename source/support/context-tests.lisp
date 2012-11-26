;;;; File: context-tests.lisp
;;; Contains: Tests of the context mechanism
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: November 1996
;;; Updated: Fri Jun  4 00:12:25 1999 by Jeff Dalton
;;; Copyright: (c) 1996, AIAI, University of Edinburgh

(in-package :oplan-ctxt)

(use-package :oplan-test-support)


(define-test-module :context-tests)

(in-test-module :context-tests)


;;; Note that a series of ASSERTs lets us do an AND and know which
;;; test failed.


(define-context-accessor cell-value car)

(defun find-context-if (pred)
  (walk-contexts
    #'(lambda (c)
	(when (funcall pred c)
	  (return-from find-context-if c)))))

(defun walk-contexts (fn)
  (label walk ((contexts *root-contexts*))
    (dolist (c contexts)
      (funcall fn c)
      (walk (context-children c)))))


(define-test-group initial-context-tests
  ((context-p *context*)                     ==> :true)
  ((context-p *global-context*)              ==> :true)
  ((consp *root-contexts*)                   ==> :true) ;must be at least one
  ((every #'context-p *root-contexts*)       ==> :true)
  ((every #'(lambda (c)
	      (null (context-parent c)))
	  *root-contexts*)                   ==> :true)
  ((member *global-context* *root-contexts*) ==> :true)
  ((typep *context-count* 'fixnum)           ==> :true)

  ;; Tests below can have side-effects that affect later tests.

  ;; Make a new root context
  ((context-p (setq *context* (new-context nil))) ==> :true)
  ((null (context-parent *context*))      ==> :true)
  ((member *context* *root-contexts*)     ==> :true)

  ;; Make a new nonroot context.
  ((context-p (setq *context* (new-context *global-context*)))
   ==> :true)
  ((eq (context-parent *context*) *global-context*)
   ==> :true)
  ((not (member *context* *root-contexts*))
   ==> :true)
  ((> (context-number *context*)
      (context-number (context-parent *context*)))
   ==> :true)

  ;; Try a non-context value
  ((let ((a (list 'a)))
     (cell-value a))
   ==> 'a)

  ;; Set a value
  ((let ((a (list 'a)))
     (assert (eq (setf (cell-value a) 'b) 'b))
     (cell-value a))
   ==> 'b)

  ;; Set a value, test various things
  ((let ((a (list 'a)))
     (setf (cell-value a) 'b)
     (assert (eq (cell-value a) 'b))
     (assert (cval-p (car a)))
     ;; Return value from parent context.
     (let ((*context* (context-parent *context*)))
       (cell-value a)))
   ==> 'a)

  ;; Check find-context-if
  ((let ((n (context-number *context*)))
     (eq (find-context-if #'(lambda (c) (= (context-number c) n)))
	 *context*))
   ==> :true)

  ;; Check context numbers of all existing contexts.
  ((find-context-if
     #'(lambda (c)
	 (assert (context-p c))
	 (assert (typep (context-number c) 'fixnum))
	 (and (context-parent c)
	      (<= (context-number c) (context-number (context-parent c))))))
   ==> nil)

  ;; Check find-context
  ((find-context-if
     #'(lambda (c)
	 (not (eq c (find-context (context-number c))))))
   ==> nil))


(define-test-group (basic-context-push/pop-tests
		     :with ((should-exist '())))
  ;; Push context
  ((let* ((parent *context*)
	  (child1 (push-context)))
     (assert (context-p child1))
     (assert (eq child1 *context*))
     (assert (eq (context-parent child1) parent))
     (assert (equal (context-children parent) (list child1)))
     (assert (> (context-number child1) (context-number parent)))

     ;; Pop context
     (let ((should-be-parent (pop-context)))
       (assert (eq should-be-parent *context*))
       (assert (eq should-be-parent parent))

       ;; Push again
       (let ((child2 (push-context)))
	 (assert (context-p child2))
	 (assert (eq child2 *context*))
	 (assert (eq (context-parent child2) parent))
	 (assert (set-eql (context-children parent) (list child1 child2)))
	 (assert (> (context-number child2) (context-number parent)))

	 ;; Pop again
	 (let ((sb-parent (pop-context)))
	   (assert (eq sb-parent *context*))
	   (assert (eq sb-parent parent)))

	 ;; Start with a value that's not context-layered.
	 (let ((cell (list 'global)))
	   (assert (eq (cell-value cell) 'global))

	   ;; Set a value in parent
	   (setf (cell-value cell) 'parent)
	   (assert (eq (cell-value cell) 'parent))

	   ;; Global value should be unchanged
	   (let ((*context* *global-context*))
	     (assert (eq (cell-value cell) 'global)))

	   ;; Parent value should be the same as before checking
	   ;; the global value.
	   (assert (eq (cell-value cell) 'parent))

	   ;; Set a different value in child1, but let child2
	   ;; inherit from the parent
	   (let ((*context* child1))
	     (setf (cell-value cell) 'child1))

	   (flet ((cv-in (c) (let ((*context* c)) (cell-value cell))))

	     ;; Check the values
	     (assert
	       (equal (mapcar #'cv-in (list parent child1 child2))
		      '(parent child1 parent)))

	     ;; Check them again
	     (assert
	       (equal (mapcar #'cv-in
			      (list parent child1 child2 child1 child2))
		      '(parent child1 parent child1 parent)))

	     ;; Grandchildren
	     (let ((gc1 (let ((*context* child1)) (push-context)))
		   (gc2 (let ((*context* child2)) (push-context))))

	       (assert (eq *context* parent))	;still

	       ;; Give them values of their own
	       (let ((*context* gc1))
		 (setf (cell-value cell) 'gc1))
	       (let ((*context* gc2))
		 (setf (cell-value cell) 'gc2))

	       ;; Remember the contexts we created
	       (setq should-exist
		     (list parent child1 child2 gc1 gc2))

	       ;; Check the values one more time by returning them.
	       (mapcar #'cv-in 
		       (list gc1 gc2 parent child1 child2 gc2 gc1 gc1))))))))
   ==> '(gc1 gc2 parent child1 parent gc2 gc1 gc1))

  ;; Check for the contexts we expect to exist.
   
  ((let ((checklist should-exist))
     (walk-contexts
       #'(lambda (c)
	   (removef c should-exist)))
     should-exist)
   ==> nil)

  ;; Check context numbers
  ((find-context-if
     #'(lambda (c)
	 (assert (context-p c))
	 (assert (typep (context-number c) 'fixnum))
	 (and (context-parent c)
	      (<= (context-number c) (context-number (context-parent c))))))
   ==> nil)

  ;; Check find-context
  ((find-context-if
     #'(lambda (c)
	 (not (eq c (find-context (context-number c))))))
   ==> nil))


(define-test-group (ctxt-gethash-tests
		     :with ((*context* (new-context nil))
			    (ht (make-hash-table))))
  ;; There used to be a bug so that ctxt-gethash did not regard
  ;; the value as "not there" for a key once that key had a value
  ;; in any context.  In some cases, the value appeared to be :undef,
  ;; in others nil, but ctxt-gethash never returned nil as its 2nd value,
  ;; nor would it use the default value supplied as the 3rd argument.
  ((ctxt-gethash 'a ht)                ==> 'nil)
  ((ctxt-gethash 'a ht 1)              ==> 1)
  ((progn
     (push-context)
     (list
       (ctxt-gethash 'a ht 1)
       (setf (ctxt-gethash 'a ht) 2)
       (ctxt-gethash 'a ht 1)))        ==> '(1 2 2))
  ;; N.B. now in pushed context
  ((progn
     (pop-context)
     (ctxt-gethash 'a ht 1))           ==> 1)
  ;; Back in our initial context
  ((ctxt-gethash 'a ht)                ==> 'nil)
  ((ctxt-gethash 'a ht 1)              ==> 1)
  ;; Push again
  ((progn
     (push-context)
     (ctxt-gethash 'a ht 1))           ==> 1)
  ((ctxt-gethash 'a ht)                ==> nil))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;

