;;;; File: defsys.lisp
;;; Contains: Simple defsystem
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: March 1992
;;; Updated: Tue Feb  5 16:44:06 2008 by Jeff Dalton
;;; Copyright: (c) 1991, 1992, 1993, 1994, 1995, 1995, 1996 AIAI,
;;;            University of Edinburgh

(in-package :user)			;only initially

;;;; Operations on systems

;;; (COMPILE-SYSTEM system-name &KEY ...)
;;; (LOAD-SYSTEM system-name &KEY ...)
;;; (CLEAN-SYSTEM system-name &KEY ...)

;;; (SET-SYSTEM-BASE-DIRECTORY system-name directory-name &KEY ...)

;;; The operation is performed for the named system and for all
;;; systems it directly or indirectly :REQUIRES.  Required systems
;;; are processed before systems that require them.  "Clean" means
;;; to delete object files.

;;; System names should be symbols and are looked up using EQ.
;;; However, the top-level operations on systems (e.g. COMPILE-SYSTEM)
;;; will try STRING= if they find no EQ match, so that it matters less
;;; which package is current.  An error is signalled if more than one
;;; system name matches.  FIND-SYSTEM also follows these rules.

;;; Recompilation or reloading occur for a file F in system S if F,
;;; or a file that F requires, has changed since the last time F was
;;; compiled or loaded (respectively).  "A file that F requires"
;;; includes the files listed after :REQUIRES in the specification
;;; of F (in the definition of S) plus all files in all :REQUIRED-
;;; SYSTEMS of S.  However, a file that F requires counts only if
;;; it contains relevant definition types and is either in S or else
;;; is in an S-required system that provides relevant definition types.
;;; The definition types are given by the :DEFINES slot of the file
;;; or system.  This is discussed further below.

;;; Keyword parameters, in terms of the opposites of the defaults:

;;;   :TEST T           -- report actions without performing them.
;;;   :RECURSIVE NIL    -- don't perform the operation on required systems.
;;;   :IGNORE-DEPENDS T -- ignore required files and systems when
;;;                        determining whether to recompile or reload.
;;;   :SILENT T         -- don't print progress notes

;;; :RECURSIVE NIL does not mean that _no_ operation is performed.
;;; For instance (COMPILE-SYSTEM 'S :RECURSIVE NIL) will load S's
;;; required systems, so that S can be compiled, but won't compile them.

;;; COMPILE-SYSTEM and LOAD-SYSTEM also support :RECURSIVE :IF-NOT-LOADED.
;;; In that case, a system is compiled or loaded only if it has not
;;; already been loaded by LOAD-SYSTEM (i.e. if its SYSTEM-LOAD-DATE
;;; is not NIL).  This is used primarily for loading systems into a
;;; running Lisp.  :RECURSIVE T would recurse into already-loaded
;;; systems, which might not be desired.

;;; The :REQUIRES relation between systems forms a directed acyclic graph.
;;; A textual description of the graph can be obtained by calling

;;;   (PRINT-SYSTEM-TREE root-system-name)


;;;; System definitions

;;; DEFSYSTEM can be used to define systems, modules, and other logical
;;; groupings of files.  Each such grouping is defined by a separate,
;;; top-level call to DEFSYSTEM, and DEFSYSTEM makes no distinction
;;; between different grouping types.

;;; Defsystem syntax: (DEFSYSTEM (system-name system-option*) file-spec*)

;;; System options:

;;;   :REQUIRED-SYSTEMS (system-name*)
;;;   :DIRECTORY string
;;;   :DEFINES (definition-type*)
;;;   :DO-AFTER-LOAD form
;;;   :OPTIONAL true-or-false

;;; The :REQUIRED-systems can be seen as services used or as subsystems.

;;; The names of files in a system will (if they're not already absolute)
;;; be taken relative to the systems's :DIRECTORY, which will in turn
;;; (if not absolute) be taken relative to *SYSTEM-BASE-DIRECTORY*.
;;; Both the :DIRECTORY and *SYSTEM-BASE-DIRECTORY* can be NIL,
;;; and the final result need not be an absolute name.  (An "absolute"
;;; file name is one that starts with "/".)

;;; When the file names in a system are not absolute, a further prefix
;;; may be specified later on by calling SET-SYSTEM-BASE-DIRECTORY.
;;; This allows relative (non-absolute) names to be used in the
;;; system definitions and while an application is being built,
;;; and then for the installed location of the application to be
;;; prefixed later on for systems that may be loaded at run-time.

;;; An :OPTIONAL system is treated as empty (as having no files and no
;;; required systems) if its directory does not exist.

;;; The :DEFINES property of a system shadows the :DEFINES properties
;;; of the files in the system, so far as requiring systems are concerned.
;;; For more on this, see the discussion of file :DEFINES below.

;;; File spec syntax: (name file-option*)

;;; File options:

;;;   :REQUIRES (file-name*)
;;;   :DEFINES (definition-type*)       ; defaults to (:EVERYTHING)
;;;   :COMPILER function-name           ; defaults to COMPILE-FILE
;;;   :LOADER function-name             ; defaults to LOAD
;;;   :DO-AFTER-LOAD form

;;; The :COMPILER can be nil for files that should not be compiled,
;;; and the :LOADER can be nil for files that should not be loaded
;;; (maybe they're loaded as-needed later on).

;;; File :SOURCE-NAME and :OBJECT-NAME can be specified expolicitly, but
;;; it's better to use a consistent naming system so that it's not necessary.

;;; N.B.:

;;; System names should be symbols; file names should be symbols or strings.
;;; Strings are safer, because a symbol might conflict with a package export.

;;; Defsystem assumes a Unix filename syntax and that most names are lower
;;; case.  When symbols are used a file names, they're converted to lower
;;; case strings (see name-for-defsys), rather than keeping whatever case(s)
;;; they had internally.  Moreover, file names are often manipulated directly
;;; as strings.  In principle, this is less portable than using pathnames;
;;; but in practice it often wins because CL implementations still disagree
;;; about some pathname operations (eg, whether merge-pathnames does
;;; a relative merge).

;;; A file should be in at most one system.

;;; It is possible to indicate that a file contains (only) certain
;;; types of definitions by providing a list of definition types for
;;; its :DEFINES slot.  Only definitions that make a difference to
;;; other files need to be listed.  (So, e.g., defvars for variables
;;; used only within the file that defines them don't matter.)

;;; A system's :DEFINES slot indicates the definition types that are
;;; provided to (make a difference to) requiring systems.

;;; At present, the only case that makes any difference to how
;;; defsystem behaves is when a :DEFINES list contains only the
;;; single keyword :FUNCTIONS.  In this case it's assumed that when
;;; the file or system changes, files that :REQUIRE it do not have
;;; to be recompiled.

;;; For checking purposes only, there is a list of acceptable definition
;;; types, *known-definition-types*.  Feel free to add to this list.
;;; Eventually it may become clear what types are actually useful, and
;;; then defsystem can go further in taking them into account.


(defpackage :simple-defsystem
  (:use #+(or :ansi-cl :cltl2) :common-lisp
	#-(or :ansi-cl :cltl2) :lisp)
  (:shadow #:defsystem
	   #:compile-system	#:load-system		#:clean-system
	   #:find-system	#:system-files		#:system-load-date
	   #:print-system-tree
	   #:set-system-base-directory
	   #:file-name
	   #:*system-base-directory*
	   #:*all-systems*	#:*loaded-systems*
	   #:*source-type*	#:*object-type*)
  (:export #:defsystem
	   #:compile-system	#:load-system		#:clean-system
	   #:find-system	#:system-files		#:system-load-date
	   #:print-system-tree
	   #:set-system-base-directory
	   #:file-name
	   #:*system-base-directory*
	   #:*all-systems*	#:*loaded-systems*
	   #:*source-type*	#:*object-type*))

(in-package :simple-defsystem)


;;; Parameters

;;; The *SYSTEM-BASE-DIRECTORY* is prefixed to system :DIRECTORY names.
;;; It should be nil or a string.  Nil is more or less equivalent to
;;; "." and to "./".

(defparameter *system-base-directory* nil)


;;; Implementation-dependent defintions

(defvar *source-type* "lsp")		;could be part of system definitions

(defvar *object-type*
  (implementation-object-type))

(defun implementation-object-type ()
  (pathname-type
    (compile-file-pathname
      (merge-pathnames "TEST.LSP"
                       (user-homedir-pathname)))))

#+kcl
(defmacro with-compilation-unit (options &body forms)
  (declare (ignore options))
  `(progn . ,forms))

#+Lucid
(defmacro with-compilation-unit (options &body forms)
  `(lcl:with-compiler-options ,options
     (lcl:with-deferred-warnings . ,forms)))


;;; System definitions

;;; For a system S, we'll use Req(S) for the list of all systems directly
;;; required by S and Req*(S) for the list of all systems directly or
;;; indirectly required.  So Req* is the transitive closure of Req.

;;; When a top-level operation (such as load-system or compile-system)
;;; is performed on S, the following slots are given the correct values
;;; for S and Req*(S) by set-up-system:

;;;  * All-required-systems contains the structs of all systems required
;;;    directly or indirectly by this one, ordered so that a system always
;;;    appears before any systems that require it.  The order in which
;;;    the the top-level operation processes systems is that of
;;;       (system-all-required-systems S).

;;;  * The system-modification-date is the file-source-date of the most
;;;    recently modified file in the system, ie, the date of the most
;;;    recent edit of any file in the system.

;;;  * The system-redefiniton-date is like the system-modification-date
;;;    but considers only changes that imply that files that depend
;;;    on this system should be recompiled.

;;;  * System-dependent-files-p indicates whether any systems that
;;;    require this one (whether directly or indirectly) contain any
;;;    files.  If not, then this system needn't be loaded when we're
;;;    compiling.  [We could do better by seeing whether any files
;;;    need to be compiled, rather than seeing if there are no files,
;;;    but we don't (yet?) do so.]

(defvar *all-systems* '())

(defvar *loaded-systems* '())

(defstruct (mark (:conc-name nil))	;for topological sort (tsort)
  mark)

(defstruct (system (:include mark) (:print-function print-system))
  name				;as given to defsystem
  files				;file structs
  required-systems		;names, and only direct requirements
  all-required-systems		;structs, direct and indirect
  dependent-files-p		;true or false
  do-after-load			;a single form
  directory			;a lower-case, Unix-syntax string
  defines
  load-date
  compile-date
  modification-date
  redefinition-date)

(defun print-system (sys stream depth)
  (declare (ignore depth))
  (format stream "#<system ~S>" (system-name sys)))


;;; Flexible system lookup

(defun find-system (name &key (if-not-found nil))
  (or (find-exact-system name)
      (let ((possibles
	     (remove name *all-systems*
		     :key #'system-name
		     :test-not #'string=)))
	(cond ((null possibles)
	       (if (eq if-not-found :error)
		   (error "System ~S is not defined." name)
		 nil))
	      ((null (cdr possibles))
	       (car possibles))
	      ((eq if-not-found :error)
	       (error "System name ~S is ambiguous among ~S."
		      name possibles))
	      (t nil)))))

;;; Exact (EQ) system lookup

(defun find-exact-system (name)
  #+kcl					;member is faster in KCL
  (car (member name *all-systems* :key #'system-name :test #'eq))
  #-kcl
  (find name *all-systems* :key #'system-name :test #'eq))

;;; Exact lookup and error if not found

(defun find-system-else-error (name)
  (or (find-exact-system name)
      (error "System ~S is not defined." name)))


(defmacro defsystem ((system &key required-systems
			          defines
			          do-after-load
			          directory
				  optional)
		     &rest file-specs)
  "(DEFSYSTEM (system-name option*) file-spec*)."
  `(def-system ',system
     ',file-specs
     :required-systems ',required-systems
     :defines ',defines
     :do-after-load ',do-after-load
     :directory ',directory
     :optional ,optional))		;n.b. evaluated

(defun def-system (name file-specs
		   &key required-systems defines do-after-load
		        directory optional)
  ;; Note that we do not call find-system for the systems we require,
  ;; because they may not be defined yet.
  (let ((sys (find-exact-system name)))
    ;; Ensure that the system exists if it doesn't already.
    (when (null sys)
      (setq sys (make-system :name name))
      (setq *all-systems* (nconc *all-systems* (list sys))))
    ;; Install parameter values.
    (setf (system-modification-date sys)
	  'not-a-system-modification-date)
    (setf (system-required-systems sys)
	  required-systems)
    (setf (system-defines sys)
	  defines)
    (setf (system-do-after-load sys)
	  do-after-load)
    (when (or directory *system-base-directory*)
      (setf (system-directory sys)
	    (directory-for-defsys directory)))
    ;; Optional systems are emptied if their directory doesn't exist.
    (when (and optional (not (directory-exists-p (system-directory sys))))
      (setf (system-required-systems sys) '()
	    file-specs '()))
    ;; N.B. Must process files after setting directory.
    (setf (system-files sys)
	  (process-system-files sys file-specs))
    sys))

(defun name-for-defsys (name)		;puts name into std form
  (if (stringp name) name (string-downcase name)))

(defun directory-for-defsys (directory)
  (setq directory (if directory (name-for-defsys directory) ""))
  (cond (*system-base-directory*
	 (check-type *system-base-directory* string)
	 (relative-merge directory *system-base-directory*))
	(t
	 directory)))


;;; Printing the system tree

;;; A required system is listed under the system that requires it,
;;; but indented a few spaces the the right.  This process is recursive.
;;; "Joins" are systems required by > 1 other system among the systems
;;; being described.  The recursive listing and indenting process stops
;;; at a join.  Instead of listing the subsystems of a join each time
;;; it occurs, the join's name is printed in angle brackets and it is
;;; given its own top-level description.

(defun print-system-tree (root-name)
  (let* ((root (find-system root-name :if-not-found :error))
	 (systems (cons root (reverse (find-all-required-systems root))))
	 (joins nil)
	 (*print-case* :downcase))
    (labels ((directly-required-p (server client)
	       (member (system-name server)
		       (system-required-systems client)
		       :test #'string=))
	     (directly-requiring-systems (server)
	       (remove server systems
		       :test-not #'directly-required-p))
	     (join-p (sys)
	       (> (length (directly-requiring-systems sys)) 1))
	     (walk (sys depth show-children-p)
	       (format t "~&~vT~:[~S~;<~S>~]~%"
		       (1+ (* 3 depth))
		       (and (> depth 0) (member sys joins))
		       (system-name sys))
	       (when show-children-p
		 (dolist (child (find-required-systems sys))
		   (walk child
			 (1+ depth)
			 (not (member child joins)))))
	       (when (= depth 0)
		 (format t "~%"))))
      (setq joins (remove-if-not #'join-p systems))
      (walk root 0 t)
      (dolist (j joins)
	(walk j 0 t))
      (values))))


;;; Generating file structs

(defparameter *known-definition-types*
  '(:functions :macros :variables :classes :structures :types :conditions
    :methods :messages :objects :package :exports :everything))

(defstruct (file (:include mark) (:print-function print-file))
  name
  source-name
  object-name
  required-files
  (defines '(:everything))		;list of :functions, :macros, etc.
  source-date				;when last edited
  (compiler 'compile-file)
  (loader 'load)
  load-date
  loaded-version			;source or object name
  do-after-load
  system)

(defun print-file (file stream depth)
  (declare (ignore depth))
  (format stream "#<file ~S in system ~S>"
	  (file-name file)
	  (system-name (file-system file))))

(defun process-system-files (sys file-specs) ; -> list of file structs
  (let ((structs
	 (mapcar #'(lambda (spec) (apply #'process-file-spec spec))
		 file-specs)))
    ;; Post-processing
    (dolist (file structs)
      ;; The lists of required files must be changed to refer to the structs.
      (setf (file-required-files file)
	    (mapcar
	       #'(lambda (r)
		   (let ((rn (name-for-defsys r)))
		     (or (find rn structs :key #'file-name :test #'string=)
			 (error "File ~S requires ~S, but ~S is not ~
                                 a file in system ~S."
				(file-name file) rn rn (system-name sys)))))
	       (file-required-files file)))
      ;; The source and object names have to include the directory
      (when (system-directory sys)
	(setf (file-source-name file)
	      (relative-merge (file-source-name file)
			      (system-directory sys)))
	(setf (file-object-name file)
	      (relative-merge (file-object-name file)
			      (system-directory sys))))
      ;; The file should point back to the system
      (setf (file-system file) sys))
    structs))

(defun relative-merge (name directory)
  (let ((n (namestring name))
	(d (namestring directory)))
    (if (absolute-name-p n)
	n				;already absolute
      (concatenate 'string
	d
	(if (eql (last-char d) #\/) "" "/") ;put / between d and n if needed
	n))))

(defun absolute-name-p (string)
  (eql (first-char string) #\/))

(defun first-char (string)
  (declare (type string string))
  (if (> (length string) 0)
      (char string 0)
    nil))

(defun last-char (string)
  (declare (type string string))
  (let ((len (length string)))
    (if (> len 0)
	(char string (1- len))
      nil)))


(defun process-file-spec (name &key (source-name nil)
			            (object-name nil)
			            (requires '())
			            (defines '(:everything))
			            (compiler 'compile-file)
				    (loader 'load)
				    (do-after-load nil))
  ;; N.B. The keyword args to this function determine what keywords
  ;; can be specified for files in system definitions.
  (let ((unknown-types (set-difference defines *known-definition-types*)))
    (when unknown-types
      (cerror "Accept the definition types for this file only."
	      "File ~S had unknown definition types ~S."
	      (name-for-defsys name) unknown-types)))
  (let ((name (name-for-defsys name)))
    (make-file
      :name name
      :source-name
        (or source-name
	    (merge-pathnames (make-pathname :type *source-type*) name))
      :object-name
        (or object-name
	    (merge-pathnames (make-pathname :type *object-type*) name))
      :required-files requires
      :defines        defines
      :compiler       compiler
      :loader         loader
      :do-after-load  do-after-load)))


;;; Operations on systems

;;; A top-level operation, such as compile-system, that will look at
;;; file-source-dates should start by calling set-up-system.  This will
;;; make sure that all relevant file structs contain the current source
;;; (write) date and that all relevant systems have correct values in
;;; their all-required-systems, dependent-files-p, modification-date,
;;; and redefinition-date slots.

;;; Re Pass 2 in set-up-system:
;;; In (reverse req), a system always appears before the systems it it
;;; requires; and so an indirectly required system always appears
;;; after one that is more directly required.  This lets us propagate
;;; dependent-files-p from a system r by visiting only the systems
;;; (rr) that it directly requires.  We could have used the longer
;;; all-required-systems lists instead, like this:
;;;
;;;    (dolist (r req)
;;;      (when (system-files r)
;;;        (dolist (rr (system-all-required-systems r))
;;;          (setf (system-dependent-files-p rr) t))))
;;;
;;; That way we could process the systems in any order, but would have
;;; to do more work.  The main advantage would be that we wouldn't
;;; have to call find-system-else-error (which at present searches
;;; *all-systems*).

(defvar *defsystem-test* nil)
(defvar *ignore-depends* nil)
(defvar *silent* nil)

(defun note (format-string &rest format-args)
  (unless *silent* (apply #'format t format-string format-args)))

(defun set-up-system (sys)
  (set-all-required-systems sys)
  (set-current-source-dates sys)
  (let ((req (system-all-required-systems sys)))
    (dolist (r req)				;Pass 1
      (setf (system-dependent-files-p r) nil)
      (set-all-required-systems r)
      (set-current-source-dates r))
    (dolist (r (cons sys (reverse req)))	;Pass 2
      (when (or (system-files r) (system-dependent-files-p r))
	(dolist (rr (system-required-systems r))
	  (setf (system-dependent-files-p (find-system-else-error rr))
		t)))))
  sys)

(defun set-all-required-systems (sys)
  (setf (system-all-required-systems sys)
        (find-all-required-systems sys)))

(defun set-current-source-dates (sys)
  (let ((mod-date 0)
	(redef-date 0))
    (dolist (file (system-files sys))
      (let ((source-date (get-file-source-date file)))
	(setf (file-source-date file) source-date)
	(setq mod-date (max mod-date source-date))
	(when (file-affects-compilation-p file)
	  (setq redef-date (max redef-date source-date)))))
    (setf (system-modification-date sys)
	  mod-date)
    (setf (system-redefinition-date sys)
	  redef-date)))

;;; Compile-system

(defun compile-system (name &key ((:test *defsystem-test*) *defsystem-test*)
			         ((:ignore-depends *ignore-depends*) nil)
				 ((:silent *silent*) nil)
			         (recursive t))
  (let ((sys (find-system name :if-not-found :error))
	(*load-verbose* nil))
    (set-up-system sys)
    (let ((req (system-all-required-systems sys)))
      (with-compilation-unit ()
        (mapc (if recursive #'compile/load-1-system #'load-1-system)
	      (if (eq recursive :if-not-loaded)
		  (remove-if #'system-load-date req)
		req)))
      (compile-1-system sys))))

(defun compile/load-1-system (system)
  (compile-1-system system)
  (when (system-dependent-files-p system)
    (load-1-system system)))		;for systems that require this one

(defun compile-1-system (system)
  (note "~&;;; Compiling system ~S.~%" (system-name system))
  (map-system-files system #'defsystem-compile-file)
  (setf (system-compile-date system) (get-universal-time))
  system)

(defun defsystem-compile-file (file)
  (when (file-needs-compilation-p file)
    (note "~&;;; Compiling ~S.~%" (file-source-name file))
    (unless *defsystem-test*
      (mapc #'defsystem-load-file (find-all-required-files file))
      (unless 
	  (funcall (file-compiler file)
		   (file-source-name file)
		   ;; /\/: Confuses Lucid 4.0 because merge-pathnames does
		   ;; a directory-relative merge!
		   ; :output-file (file-object-name file)
		   )
	(error "Failed to compile ~S." (file-source-name file)))
      ;; Load right after compile.
      ;; /\/: Maybe load only if (file-affects-compilation-p file).
      (when (file-loader file)
	(note "~&;;; Loading ~S.~%" (file-object-name file))
	(funcall (file-loader file) (file-object-name file))
	(eval (file-do-after-load file))
	(setf (file-loaded-version file) (file-object-name file)
	      (file-load-date file) (get-universal-time))))))


;;; Load-system

(defun load-system (name &key ((:test *defsystem-test*) *defsystem-test*)
			      ((:ignore-depends *ignore-depends*) nil)
			      ((:silent *silent*) nil)
			      (recursive t))
  (let ((sys (find-system name :if-not-found :error))
	(*load-verbose* nil))
    (set-up-system sys)
    (let ((req (system-all-required-systems sys)))
      (when recursive
	(mapc #'load-1-system
	      (if (eq recursive :if-not-loaded)
		  (remove-if #'system-load-date req)
		req))))
    (load-1-system sys)))

(defun load-1-system (system)
  (note "~&;;; Loading system ~S.~%" (system-name system))
  (map-system-files system #'defsystem-load-file)
  (unless *defsystem-test*
    (setf (system-load-date system) (get-universal-time))
    (eval (system-do-after-load system))
    (unless (member system *loaded-systems*)
	    (setq *loaded-systems* (nconc *loaded-systems* (list system)))))
  system)

(defun defsystem-load-file (file)
  (when (file-needs-loading-p file)
    (let ((load-version (file-load-version file)))
      (note "~&;;; Loading ~S.~%" load-version)
      (unless *defsystem-test*
	(funcall (file-loader file) load-version)
	(eval (file-do-after-load file))
	(setf (file-loaded-version file) load-version
	      (file-load-date file) (get-universal-time))))))


;;; Clean-system

;;; /\/: Does not treat :recursive :if-not-loaded specially.

(defun clean-system (name &key ((:test *defsystem-test*) *defsystem-test*)
			       ((:silent *silent*) nil)
			       (recursive t))
  (let* ((sys (find-system name :if-not-found :error))
	 (req (find-all-required-systems sys)))
    (when recursive
      (mapc #'clean-1-system req))
    (clean-1-system sys)))

(defun clean-1-system (system)
  (note "~&;;; Cleaning system ~S.~%" (system-name system))
  (map-system-files system #'defsystem-delete-file)
  system)

(defun defsystem-delete-file (file)
  (let ((object (file-object-name file)))
    (when (probe-file object)
      (note "~&;;; Deleting ~S.~%" (namestring object))
      (unless *defsystem-test*
	(delete-file object)))))


;;; Installing a base directory

(defun set-system-base-directory
       (system-name directory-name 
	&key ((:test *defsystem-test*) *defsystem-test*)
             ((:silent *silent*) nil)
	     (recursive t))
  (let* ((sys (find-system system-name :if-not-found :error))
	 (req (find-all-required-systems sys))
	 (basedir (name-for-defsys directory-name)))
    (when recursive
      (dolist (s (if (eq recursive :if-not-loaded)
		     (remove-if #'system-load-date req)
		   req))
        (set-1-system-base-directory s basedir)))
    (set-1-system-base-directory sys basedir)))

(defun set-1-system-base-directory (system basedir)
  (let ((sysdir (or (system-directory system) "")))
    (if (absolute-name-p sysdir)
	(note "~&;;; ~S already has an absolute directory ~S.~%"
	      system sysdir)
      (let ((d (relative-merge sysdir basedir)))
	(note "~&;;; Setting directory of ~S to ~S.~%" system d)
	(unless (directory-exists-p d)
	  (warn "Directory ~S does not exist." d))
	(unless *defsystem-test*
	  (setf (system-directory system) d))
	;; Files already have the old system-directry in their source
	;; and object names.
	(dolist (file (system-files system))
          (let ((source (relative-merge (file-source-name file) basedir))
		(object (relative-merge (file-object-name file) basedir)))
	    ;; No special messages for already-absolute file names /\/
	    (note "~&;;;   ~S~%;;;   ~S.~%" source object)
	    (unless *defsystem-test*
	      (setf (file-source-name file) source
		    (file-object-name file) object))))))))

(defun directory-exists-p (d)
  #+gcl (si:stat d)
  #-gcl (probe-file d))


;;; Topological sort and related routines.

(defun map-system-files (system fn)
  (mapc fn (required-file-order (system-files system)))
  nil)

(defun required-file-order (files)
  (tsort files #'file-required-files))

(defun find-all-required-files (file)
  (required-file-order (file-required-files file)))

(defun find-all-required-systems (sys) ; -> system structs
  (tsort (find-required-systems sys)
	 #'find-required-systems))

(defun find-required-systems (sys) ; -> system structs [not recursive]
  (mapcar #'(lambda (required-name)
	      (or (find-exact-system required-name)
		  (error "System ~S, required by ~S, is not defined."
			 required-name (system-name sys))))
	  (system-required-systems sys)))

;;; Topological sort, returning descendants before ancestors.

;;; Each call to tsort constructs unique marks, so that clearing
;;; old marks is not necessary.

(defun tsort (roots children-fn)
  (let ((start-mark (list :start))
	(finish-mark (list :finish))
	(result '()))
    (labels ((walk (items)
	       (dolist (at items)
		 (cond ((eq (mark at) start-mark)
			(error "Cycle involving ~S." at))
		       ((eq (mark at) finish-mark)
			;; already processed
			)
		       (t
			(setf (mark at) start-mark)
			(walk (funcall children-fn at))
			(push at result)
			(setf (mark at) finish-mark))))))
      (walk roots)
      (nreverse result))))


;;; Functions for examining file states, etc.

(defun most-recent-version (file)
  (if (source-newer-p file)
      (file-source-name file)
      (file-object-name file)))

(defun file-compilation-date (file)
  (if (not (probe-file (file-object-name file)))
      0
    (file-object-date file)))

(defun get-file-source-date (file)	;stored in file's source-date slot
  (or (file-write-date (file-source-name file))
      (error "No source ~S." (file-source-name file))))

(defun file-object-date (file)
  (file-write-date (file-object-name file)))

(defun source-newer-p (file)
  (or (not (probe-file (file-object-name file)))
      (> (file-source-date file) (file-object-date file))))

(defun file-affects-compilation-p (file)
  (not (equal (file-defines file) '(:functions))))

(defun system-affects-compilation-p (system)
  (not (equal (system-defines system) '(:functions))))

(defun file-needs-compilation-p (file)
  (and (file-compiler file)
       (or (source-newer-p file)
	   (and (not *ignore-depends*)
		(let ((compilation-date (file-compilation-date file)))
		  (or (some #'(lambda (reqd-sys)
				(and (system-affects-compilation-p reqd-sys)
				     (> (system-redefinition-date reqd-sys)
					compilation-date)))
			    (system-all-required-systems (file-system file)))
		      (some #'(lambda (required)
				(and (file-affects-compilation-p required)
				     (> (file-source-date required)
					compilation-date)))
			    (find-all-required-files file))))))))

(defun file-needs-loading-p (file)
  ;; /\/ Do we need to reload if a required file has merely
  ;; been reloaded?  If so, we can get away with checking only
  ;; the load-dates of the required files (and systems).
  ;; /\/ May also want to load if object file is newer than load-date.
  (and (file-loader file)
       (or (null (file-load-date file))
	   ;; The file has already been loaded -- do we need to reload it?
	   (> (file-source-date file) (file-load-date file))
	   (and (not *ignore-depends*)
		(some #'(lambda (required-sys)
			  (> (system-redefinition-date required-sys)
			     (file-load-date file)))
		      (system-all-required-systems (file-system file))))
	   (and (not *ignore-depends*)
		(some #'(lambda (required)
			  (and (file-affects-compilation-p required)
			       (> (file-source-date required)
				  (file-load-date file))))
		      (find-all-required-files file))))))

(defun file-load-version (file)
  ;; If a required file or system has changed, we should load
  ;; the source (perhaps a macro has been redefined)
  (cond (*ignore-depends*
	 (most-recent-version file))
	((file-needs-compilation-p file)
	 (file-source-name file))
	(t
	 (most-recent-version file))))

;;; End
