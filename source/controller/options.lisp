;;;; File: options.lisp
;;; Contains: The Controller side of the implementation of options
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: November 1996
;;; Updated: Fri Oct  9 03:45:32 1998 by Jeff Dalton
;;; Copyright: (c) 1996, AIAI, University of Edinburgh

(in-package :oplan)

;;; Options are similar to externally accessible contexts.

;;; /\/: Should options remember authority?  If they do, then when
;;; *option* is changed, the message to the TA should include authroity
;;; settings.


(defvar *root-option* nil)		;initial current option

(defvar *option* nil)			;current option


;;; Name to option mapping

(defvar *name-to-option-table* (make-hash-table :test #'eq))

(defmacro %find-option (name)
  `(gethash ,name *name-to-option-table*))

(defun find-option (name)
  (or (%find-option name)
      (harmless-error "There is no option named ~S." name)))


;;; Initialization

;;; Note that the root option must be constructed by hand, beause we
;;; don't want some of what new-option does, such as pushing contexts.

(define-initializer :am init-options ()
  (clrhash *name-to-option-table*)
  (setq *root-option* (make-option :name 'root-option))
  (setf (%find-option 'root-option) *root-option*)
  (assert (root-option-p *root-option*))
  (setq *option* *root-option*))


;;; Basic operations

(defun new-option (&key (name (generate-option-name)))
  (check-type *option* option)
  (when (%find-option name)
    (harmless-error "There is already an option named ~S." name))
  (let* ((c0 (db-request :get-context))
	 (c1 (db-request :new-context c0))
	 (c2 (db-request :new-context c1))
	 (opt (make-option :name name
			   :parent *option*
			   :twinning-context c1
			   :base-context c2
			   :current-context c2
			   :alternatives '())))
    ;; Make sure we haven't (yet) changed the current context.
    (assert (eql (db-request :get-context) c0))
    ;; Now push a context so we're not still in an ancestor of the
    ;; new option's contexts.
    (db-request :push-context)
    ;; Record the new option.
    (setf (%find-option name) opt)
    (nconcf (option-children *option*) (list opt))
    opt))

(defun twin-option (&key (opt *option*) (name nil name-p))
  (when (symbolp opt)
    (setq opt (find-option opt)))
  (check-type opt option)
  (when (root-option-p opt)
    (harmless-error "Can't make a twin of the root option."))
  (unless name-p
    (setq name (generate-option-name (option-parent opt))))
  (when (%find-option name)
    (harmless-error "There is already an option named ~S." name))
  (let* ((c2 (db-request :new-context (option-twinning-context opt)))
	 (parent (option-parent opt))
	 (twin (make-option
		 :name name
		 :parent parent
		 :twinning-context (option-twinning-context opt)
		 :base-context c2
		 :current-context c2
		 :alternatives '())))
    (setf (%find-option name) twin)
    (nconcf (option-children parent) (list twin))
    twin))

(defun set-current-option (opt)
  (check-type *option* option)
  (check-type opt option)
  (assert (not *alt-transaction-open-p*))
  ;; Save state in the old current option.
  (setf (option-alternatives *option*) *alternatives*
	(option-current-context *option*) (db-request :get-context))
  ;; Install the new current option.
  (db-request :set-context (option-current-context opt))
  (setf *alternatives* (option-alternatives opt))
  (when (null *alternatives*)
    (setq *alternatives* (list *no-more-alternatives-alt*)))
  (assert *alternatives*)
  (setq *option* opt))

(defun clear-option (opt)
  ;; Similar to twinning the option, but w/o creating a new option.
  (check-type opt option)
  (let ((c2 (db-request :new-context (option-twinning-context opt))))
    ; (delete-context (option-base-context opt))
    (setf (option-base-context opt) c2
	  (option-current-context opt) c2
	  (option-alternatives opt) '())
    opt))

(defun delete-option (opt)
  ;; /\/: Tricky.  What if it's the current option?
  (declare (ignore opt))
  (harmless-error "Delete-option has not yet been implemented."))

(defun root-option-p (option)
  ;; The assertions will detect options that look like root options
  ;; but are not eq to *root-option*.  (There shouldn't be any.)
  (check-type option option)
  (cond ((eq option *root-option*)
	 (assert (null (option-parent option)))
	 t)
	(t
	 (assert (option-p (option-parent option)))
	 nil)))

(defun generate-option-name (&optional (parent-option *option*))
  (concat-name
    (if (root-option-p parent-option) '#:option (option-name parent-option))
    "-"					;"."? /\/
    (int->string (1+ (length (option-children parent-option))))))


;;; Adding constraits to options

(defun can-add-to-option-p (opt)
  ;; /\/: Can't really call find-context, 'cause we're not the DM.
  (and (not (root-option-p opt))
       ;; No further restrictions nowadays.
       #+:undef
       (let ((children 
	      (context-children (find-context (option-base-context opt)))))
	 (or (null children)
	     ;; /\/: set-current-option may create an alternative, which
	     ;; pushes a context.  So the option's base-context will
	     ;; have 1 child.
	     (and (length=1 children)
		  (null (context-children (first children))))))))


;;; Internal messages (w/in O-Plan)

(defmessage (:am :make-option-1) ()
  (assert (length=1 *alternatives*))	;only the :no-more-alternatives
  (assert (eq :expand-task (ag-type *ae-being-processed*)))
  (let* ((opt (new-option :name 'oplan::option-1))
	 (c (db-request :get-context)))
    (db-request :set-context (option-twinning-context opt))
    (db-request :processed-agenda-entry (ag-id *ae-being-processed*))
    (db-request :set-context c)
    (set-current-option opt)
    :ok))

(defmessage (:am :can-add-to-option-p) ()
  (can-add-to-option-p *option*))

(defmessage (:am :add-to-option) (additions)
  (check-type *option* option)
  ;; Record the additions in the current option.
  (setf (option-added-constraints *option*)
	(append (option-added-constraints *option*) additions))
  ;; Record the additions in all existing alternatives
  (atm-add-to-alternatives additions)
  :ok)

(defmessage (:am :get-option) (name)
  (%find-option name))


;;; Messages from the TA

(defmessage (:agent :make-option) (&rest args &key name)
  (declare (ignore name))
  (ipc-send-out :option
    (option-name
      (set-current-option
        (apply #'new-option args)))))

(defmessage (:agent :get-option) ()
  (check-type *option* option)
  (ipc-send-out :option (option-name *option*)))

(defmessage (:agent :set-option) (name)
  (set-current-option (find-option name))
  (ipc-send-out :option name))

(defmessage (:agent :push-option) ()
  ;; Like :make-option but w/o args, so name is generated.
  (funcall (ipc-get-handler :agent :make-option)))

(defmessage (:agent :pop-option) ()
  (check-type *option* option)
  (when (root-option-p *option*)
    (harmless-error "Can't pop beyond the root option."))
  (set-current-option (option-parent *option*))
  (ipc-send-out :option
    (option-name *option*)))

(defmessage (:agent :twin-option) (&rest args &key name)
  ;; Twins the current option.  A name for the twin may be specified.
  (declare (ignore name))
  (ipc-send-out :option
    (option-name
      (set-current-option
        (apply #'twin-option args)))))

(defmessage (:agent :clear-option) ()
  (clear-option *option*)
  (ipc-send-out :option			; /\/: Is a reply really needed?
    (option-name *option*)))

(defmessage (:agent :get-option-tree) ()
  (ipc-send-out :option
    (make-tree *root-option*
	       #'option-children
	       :map #'option-name)))

(defun make-tree (root children-fn &key ((:map map-fn) #'identity))
  (label grow ((at root))
    (cons (funcall map-fn at)
	  (mapcar #'grow (funcall children-fn at)))))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
