;;;; File: domain.lsp
;;; Contains: The DM's interface to the TF compiler etc.
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: November 1996
;;; Updated: Thu Oct  8 12:30:48 1998 by Jeff Dalton
;;; Copyright: (c) 1996, AIAI, University of Edinburgh

;;; /\/: Perhaps some of this code should be in the TF compiler.

(in-package :oplan)

;;;; New domain / TF compilation

(defvar *loaded-TFs* nil)

(define-initializer :dm clear-domain-information ()
  (setq *domain* nil)
  (setq *loaded-TFs* nil)
  (advice- :all :domain-specific))

;;; Function redefinition macro for use in included Lisp code.
;;; Note that the user can also use advice+ directly.

;;; We have to make sure the function is defined before advice+ is
;;; called, so that defun-for-domain can be used on names that are
;;; not otherwise defined.

(defmacro defun-for-domain (name lambda-list &body body)
  (let ((global-name (concat-name name "-DOMAIN-SPECIFIC")))
  `(progn
     (defun ,global-name ,lambda-list ,@body)
     (unless (fboundp ',name)
       (defun ,name (&rest args)
	 (error "Non-domain-specific ~S called;~%args: ~S." ',name args)))
     (advice+ ',name :domain-specific
       #'(lambda (previous)
	   (declare (ignore previous))
	   #',global-name)))))

;;; Load in a domain TF descripter file.

;;; The argument will normally be a pathname.  However, we also allow it
;;; to be a namestring.  The _name_ (as opposed to the directory, type,
;;; etc) field of the pathname is significant.  It gives us the domain
;;; name and is used to determine whether the domain has already been
;;; loaded.

;;; N.B. If you compile and load a .tf file successfully, then you can't
;;; reload it after a change unless you go through an :init first.

;;; The result of init-new-domain is either nil (if something went wrong)
;;; or a list containing the domain name and the names of all available
;;; task schemas.

;;; /\/: Actually, it's only the names of all tasks in the .tf file
;;; just processed.  See below.

(defun init-new-domain (filename) ; -> nil or (domain-name task-name...)
  (let* ((path (pathname filename))
	 (ident (pathname-name path)))

    ;; See if we've already loaded this domain.  Return nil if so.
    (when (member ident *loaded-TFs* :test #'string-equal)
      (dev-debug :non-fatal-error "TF for domain ~A already loaded." ident)
      (return-from init-new-domain nil))

    ;; Call the TF compiler.
    (let ((defaulted-path (defaulted-tf-pathname path))
	  (*package* (find-package :oplan))
	  (tf-errors 0))
      (multiple-value-setq (*domain* tf-errors)
	(compile-domain defaulted-path))

      ;; See if any errors.  Return nil if there were.
      (when (> tf-errors 0)
	(dev-debug :fatal-error "Error in loading TF file ~A." filename)
	(return-from init-new-domain nil))

      ;; So we've just read a valid TF file.
      (dev-debug :information "Found TF file ~A" filename)

      ;; Evaluate any "language lisp" code.
      (when (domain-included-code *domain*)
	(mapc #'eval (domain-included-code *domain*)))

      ;; Process always facts.
      (dolist (effect (domain-always *domain*))
	(tgm-add-effect (car effect) (cadr effect) :always)
	(tgm-commit-to-adding-effect
	   (car effect) (cadr effect) :always))

      ;; Process types
      (dolist (type-spec (domain-object-types *domain*))
	(psv-add-type (car type-spec) (cadr type-spec)))

      ;; Let constraint managers see the domain.
      (walk-registered-constraint-managers
        #'(lambda (cm) (cm-new-domain cm *domain*)))

      ;; And we're done.
      (push ident *loaded-TFs*)
      (cons ident
	    (list-available-tasks)))))

(defun defaulted-tf-pathname (path)
  (let ((path-with-type (merge-pathnames path (make-pathname :type "tf"))))
    (if (find #\/ (namestring path))
	path-with-type
      (merge-pathnames
        path-with-type
	(concat-string (get-parameter :oplan-tf-dir) "/")))))

;;; "Memoized" interface to the TF compiler.

(defvar *previous-truename* nil)
(defvar *previous-write-date* nil)
(defvar *previous-domain* nil)

(defun compile-domain (pathname)
  (let* ((truename (truename pathname))
	 (write-date (file-write-date truename)))
    (cond ((and (equal *previous-truename* truename)
		(equal *previous-write-date* write-date))
	   ;; Previous result is still valid.
	   ;; We can assume 0 errors because of what we do below.
	   (dev-debug :minimal "Reusing ~S domain." (pathname-name pathname))
	   (values *previous-domain* 0))
	  (t
	   ;; Need to compile a new domain, so call the compiler.
	   (dev-debug :minimal "Compiling ~S." pathname)
	   (multiple-value-bind (domain errors)
	       (oplan-tf-compiler:compile-from-tf-file pathname)
	     ;; If no errors, remember the result.
	     (when (= errors 0)
	       (setq *previous-truename* truename
		     *previous-write-date* write-date
		     *previous-domain* domain))
	     (values domain errors))))))

;;; /\/: We want the list of tasks in the order of their definition
;;; in the TF file.  However, the total schema list is not built up
;;; in *domain*.  Instead, we get a new domain struct each time we
;;; compile a TF file; and so *domain* will hold only the schemas
;;; from the most recent TF compilation.  Now, we could look in the
;;; schema-name-table, but then the order wouldn't be preserved.
;;; What we do now is let the TA keep track of the total list.

;;; Maybe this can all be fixed once we get rid of the convert-new-tf-
;;; compiler-output-to-my-old-style funciton.  /\/

(defun list-available-tasks () ; -> list of task names
  (mapcar #'string-downcase
	  (mapcar #'schema-name
		  (remove-if-not #'task-schema-p 
				 (domain-schemas *domain*)))))


;;;; Domain services

(defun find-schemas-matching-expansion-pattern (pattern)
  ;; /\/: Try this:
  (setq pattern (psv-actorise-pattern pattern))
  ;; /\/
  (let ((possibles (gethash (first pattern) (mapping expand-name schemas)))
	result matchup)
    (dolist (v possibles)
      (when (setq matchup
		  (match-up pattern
			    (schema-expands v)
			    (create-binding-tuples-from-schema v)))
	;; Because t is not a valid binding list.
	(if (eql matchup t) (setq matchup nil))
	(setq result (cons (cons v matchup) result))))
    (unless result
      (dev-debug :warning "NO SCHEMA TO EXPAND ~S" pattern))
    (nreverse result)))

(defun find-schemas-for-using-for-effect (effect)
  (let* ((pattern (first effect))
	 (value (second effect))
	 (possibles (gethash (first pattern)
			     (mapping effect-name effect+schema_s)))
	 result matchup)
    ;; /\/: Try this:
    (setq pattern (psv-actorise-pattern pattern))
    (setq value (psv-actorise-pattern value))
    ;; /\/
    (dolist (v possibles)
      (let ((oufe (car v))
	    (schema (cdr v)))
	(if (setq matchup
		  (match-up pattern (effect-pattern oufe)
			    (create-binding-tuples-from-schema schema)))
	    (when (setq matchup
			(match-up value (effect-value oufe) matchup))
	      ;; Because t is not a valid binding list.
	      (if (eql matchup t) (setq matchup nil))
	      (setq result (cons (cons schema matchup) result))))))
    (unless result
      (warn #|break|# "NO SCHEMA FOR EFFECT ~S" effect))
    (nreverse result)))

(defun find-schema-with-name (name)
  (gethash name (mapping schema-name schema)))

(defun get-possible-effects-from-expanding (pattern)
  ;; The table maps the first fixed-word of a pattern for expansion to a list
  ;; of all the possible effects that could be added from this expansion.
  (gethash (car pattern)
	   (mapping expand-name possible-effect-names)))

(defun get-possible-effects-from-achieving (pattern)
  ;; The table maps the first fixed-word of a pattern for satisfying an
  ;; achieve, to a list of all the possible effects that could be added
  ;; from adding to the plan network.
  (gethash (car pattern)
	   (mapping effect-name possible-effect-names)))


;;;; Draw-schema

(defun draw-schema (&optional (name (ask-user-for-schema-name)))
  (let ((schema
	 (or (etypecase name
	       (symbol (find-schema-with-name name))
	       (string (find-schema-with-name (intern (string-upcase name)))))
	     (error "No schema named ~S." name)))
	(outfile
	 (generate-unique-filename
	   (namestring (temp-filename "schema-graph"))
	   ".ps")))

    ;; Draw the graph
    (with-open-file (*standard-output* outfile :direction :output)
       (draw-schema-psgraph schema))

    ;; Now print / view the graph.
    (let ((viewer
	   (or (get-parameter :ps-viewer)
	       "lpr")))
      ;; Run the viewer (which might just be "lpr") in the background,
      ;; together with a command to delete outfile.  That way we can
      ;; go on to other things while the viewer runs.
      (system
       (concat-string
	 "(" viewer " " outfile "; /bin/rm " outfile ")&")))))

(defun ask-user-for-schema-name ()
  (let* ((schemas (domain-schemas *domain*))
	 (expandable-schemas
	  (remove-if #'null schemas :key #'schema-nodes)))
    (big-menu-request
      `("-heading" "Expandable Schemas"
	,@(mapcar #'string-downcase
		  (mapcar #'schema-name
			  expandable-schemas)))
      :read-function #'read-line)))

;;; Draw-schema-psgraph writes a PostScript graph to *standard-output*
;;; using (format t ...) calls.

(defun draw-schema-psgraph (schema)
  (let ((node-successor-table (build-schema-node-successor-table schema))
	(*print-case* :upcase))
    ;; Now we can draw the graph.
    (psgraph:psgraph
      ;; Root
      (etag :self :begin)
      ;; Childf
      #'(lambda (etag)
	  (gethash etag node-successor-table))
      ;; Infof
      #'(lambda (etag)
	  (if (eq (etag-node etag) :self)
	      (list :self (etag-end etag))
	    (ecase (etag-end etag)
	      (:begin
	       (let ((node (find (etag-node etag) (schema-nodes schema)
				 :key #'node-number)))
		 (ecase (node-type node)
		   ((action)
		    (list (format nil "~A BEGIN ACTION" (node-number node))
			  (string-upcase
			    (oplan-plan-world-viewer::pattern->string
			      (node-pattern node)))))
		   ((start finish dummy)
		    (list (format nil "~A ~A"
				  (node-number node)
				  (node-type node))
			  (etag-end etag))))))
	      (:end
	       (list (etag-node etag) (etag-end etag))))))
      ;; Trivia
      t					;force to one page
      nil				;not insert
      #'equal				;test
      nil				;don't remove redundant links
      ;; Title
      (string-capitalize (schema-name schema)))))

(defun build-schema-node-successor-table (schema)
  (let* ((nodes (schema-nodes schema))
	 (orderings (schema-orderings schema))
	 (node-successor-table (make-hash-table :test #'equal)))

    ;; The node-successor-table maps an etag to a list of the etags for
    ;; the node-ends that are linked directly after it.

    ;; Start by taking the links that are explicitly in the orderings.
    (dolist (ord orderings)
      (let ((from (etag (ordering-node-number1 ord) (ordering-node-end1 ord)))
	    (to (etag (ordering-node-number2 ord) (ordering-node-end2 ord))))
	(nconcf1 (gethash from node-successor-table) to)))

    ;; Any :begin end without a predecessor is linked after begin_of self.
    (dolist (n nodes)
      (when (notany #'(lambda (ord)
			(and (eql (ordering-node-number2 ord) (node-number n))
			     (eq (ordering-node-end2 ord) :begin)))
		    orderings)
	(nconcf1 (gethash (etag :self :begin) node-successor-table)
		 (etag (node-number n) :begin))))

    ;; Any :begin without a successor is linked before the :end
    ;; of the same node.  An :end without a successor is linked
    ;; before end_of self.
    (dolist (n nodes)
      (let ((b (etag (node-number n) :begin))
	    (e (etag (node-number n) :end)))
	(ensuref (gethash b node-successor-table) (list e))
	(ensuref (gethash e node-successor-table) (list (etag :self :end)))))

    ;; And we're done.
    node-successor-table))

;;; End


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
