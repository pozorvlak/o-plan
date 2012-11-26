;;;; File: struct-macros.lisp
;;; Contains: defstruct extensions
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: October 1996, though the contents are older
;;; Updated: Mon Oct 28 20:11:09 1996 by Jeff Dalton
;;; Copyright: (c) 1996, AIAI, University of Edinburgh

(in-package :oplan-util)


;;;; Defining struct extensions (which contain extra slots)
;;;
;;; Example:
;;;
;;; (define-struct-extension (ne-exec-aux :accessor-prefix ne-
;;;                                       :extension-prefix nex-
;;;                                       :base-accessor ne-exec-slots)
;;;   ...
;;;   (effects '())
;;;   ...)
;;;
;;; This defines a struct called ne-exec-aux.  For that struct, the
;;; :conc-name is nex-.  However, names with that prefix will hardly
;;; ever be used.  Instead, for each slot we'll define accessors with
;;; a different prefix, ne-.
;;;
;;; The idea is that we already have a struct where accessors have
;;; ne- names, and we want to define some extra slots for that struct.
;;; Suppose ne is an instance of that struct.  The definition above
;;; will define (ne-effects ne) == (nex-effects (ne-exec-slots ne)).
;;;
;;; Ne-exect-slots is a function that goes from an instance of the
;;; base struct to an instance of the extension struct.  So it's
;;; an accessor on instances of the base struct, hence the name
;;; :base-accessor.  /\/: A better name can no doubt be found.
;;;

(defmacro define-struct-extension
            ((name &key accessor-prefix extension-prefix base-accessor)
	     &rest extra-slots)
  (unless (and accessor-prefix extension-prefix base-accessor)
    (error "Struct extension ~S is not fully specified." name))
  `(progn
     (defstruct (,name (:conc-name ,extension-prefix))
       ,@extra-slots)
     ,@(mapcan
	  #'(lambda (accessor-name)
	      (let ((accessor
		     (concat-name accessor-prefix accessor-name))
		    (extn-accessor
		     (concat-name extension-prefix accessor-name)))
		(list
		  `(defun ,accessor (obj)
		     (,extn-accessor (,base-accessor obj)))
		  `(defsetf ,accessor (obj) (new-value)
		     `(setf (,',extn-accessor (,',base-accessor ,obj))
		            ,new-value)))))
	  (mapcar #'car-if-consp extra-slots))))



;;;; Defstruct-export

;;; Defstruct-export is like defstruct but exports the type name, the
;;; constructors, the predicate (if any), the copier, all slot names
;;; (for use in :include options), and and all accessor names.

;;; At present there's no way to selectively export some names but
;;; not others.  Ways to do that would include a :private slot option
;;; that would prevent the slot and accessor names from being exported,
;;; and a :private struct option that would include a list of names
;;; to keep private.

;;; We also have import-struct which works like this:

(defmacro import-struct (struct-name &optional (package nil))
  ;; N.B. The args are evaluated, just as in IMPORT.
  `(import (get ,struct-name 'struct-exports)
           ,@(if package (list package) nil)))

;;; SLot-names are not normally imported, because they tend
;;; to use up common names (e.g. position, cost, x) and create
;;; name conflicts.

(defmacro import-slot-names (struct-name &optional (package nil))
  `(import (get ,struct-name 'struct-slot-names)
           ,@(if package (list package) nil)))

(defmacro defstruct-export (struct-spec &rest slot-specs &aux doc-string)

  ;; Minimal syntax check.
  (unless (or (symbolp struct-spec) (symbolp (car-if-consp struct-spec)))
    (error "Invalid export struct spec: ~S." struct-spec))

  ;; Separate doc string from slot specs if it exists.
  (when (stringp (car slot-specs))
    (setq doc-string (car slot-specs)
	  slot-specs (cdr slot-specs)))

  ;; Get various names and options
  (let* ((name (car-if-consp struct-spec))
	 (slot-names (mapcar #'car-if-consp slot-specs))
	 (options (if (consp struct-spec) (cdr struct-spec) nil))
	 (opts (make-defstruct-option-plist name options))
	 (include-name
	  (getf opts :include))
	 (conc-name
	  (if (find :conc-name options :key #'car-if-consp)
	      (getf opts :conc-name)	;may be nil
	    (concat-name name "-")))
	 (constructors
	  (cons (concat-name "MAKE-" name) (getf opts :constructors)))
	 (predicate
	  (or (getf opts :predicate)
	      (and (or (not (getf opts :type)) (getf opts :named))
		   (concat-name name "-P"))))
	 (copier
	  (or (getf opts :copier)
	      (concat-name "COPY-" name))))

    ;; Get inherited slots, if any.  The inherited slots are a
    ;; list of slot names.
    (let* ((inherited-slots
	    (if include-name
		(get include-name 'struct-slot-names)
	      '()))
	   (total-slots
	    (append slot-names inherited-slots))
	   (total-accessors
	    (if conc-name
		(mapcar #'(lambda (slot-name)
			    (concat-name conc-name slot-name))
			total-slots)
	      total-slots))
	   (exports
	    `(,name
	      ,@constructors
	      ,copier
	      ,@(if predicate (list predicate) nil)
	      ;; ,@slot-names
	      ,@(when conc-name
		  total-accessors))))

      ;; Produce the macro expansion
      `(progn
	;; Record this struct's slots for any struct that :includes it,
	;; and record the exports so import-struct can work.
	(eval-when (eval compile load)
	   (setf (get ',name 'struct-slot-names) ',total-slots
		 (get ',name 'struct-exports) ',exports))
	(export ',exports)
	(defstruct ,struct-spec
	  ,@(if doc-string (list doc-string) nil)
	  ,@slot-specs)))))

;;; Make-defstruct-option-plist converts the options to an easily used
;;; canonical form.

(defun make-defstruct-option-plist (name options)
  (let ((plist nil))
    (dolist (opt options plist)
      (let ((opt-name (car-if-consp opt)))
	(case opt-name
	  (:constructor
	   (setf (getf plist :constructors)
		 (nconc (getf plist :constructors) (list (cadr opt)))))
	  (:named
	   (setf (getf plist :named) t))
	  (t
	   (if (getf plist opt-name)
	       (error "Two ~S options for export struct ~S" opt-name name)
	     (setf (getf plist opt-name)
		   (cadr opt)))))))))


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
