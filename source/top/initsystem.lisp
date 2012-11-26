;;;; File: initsystem.lisp
;;; Contains: Window and some parameter configuration and initialization
;;; Authors: Jeff Dalton and Richard Kirby
;;; Created: Mon Jan 22 12:17:30 1990
;;; Updated: Sat Feb 12 02:30:45 2000 by Jeff Dalton
;;; Copyright: (c) 1992, 1993, AIAI, University of Edinburgh

(in-package :oplan-initsystem :nicknames '(:is))

(use-package :oplan-util)
(use-package :oplan-developerlib)
(use-package :oplan-xwindowio)
(use-package :oplan-ipc)
(use-package :oplan-parser-kit)
(use-package :simple-defsystem)

(export '(is-init-parameter-defaults))
(export '(is-init-env-parameters))
(export '(is-connect-p is-set-connect))
(export '(is-http-p))
(export '(is-server-p))
(export '(is-subr-p))

(export '(is-init is-get-config-entry))
(export '(is-get-window-args is-get-window-width))
(export '(is-set-config-file is-configure))

(export '(is-patch-loader))
(export '(is-load-system))


;;;; Parameter initialization.

;;; /\/: May want a way to convert from strings to other types,
;;; with checking, as is done for query-args in the Web demos.

(defparameter *parameter-defaults*
  ;;  Parameter-name   Default-value
  '( (:menu-geometry   "+100+150")
     (:interactive     t         )
     (:windows         t         )
     (:patches         t         )
     (:run-lights      t         )
     (:ps-printer      "lpr"     )
     (:ps-viewer       "ghostview -landscape")
     (:new-psgraph     t         ) ))

(defparameter *parameter-env-map*
  ;;  Paramater-name   Env-variable   Status
  '( (:oplan-dir       "OPLANDIR"     :required)
     (:oplan-tmp-dir   "OPLANTMPDIR"  :required)
     (:oplan-tf-dir    "OPLANTFDIR"   :optional) ))

(defun is-init-parameter-defaults ()
  (dolist (d *parameter-defaults*)
    (apply #'set-parameter d)))

(defun is-init-env-parameters ()
  ;; Initialize from the environment
  (dolist (m *parameter-env-map*)
    (apply #'init-env-parameter m))
  ;; Apply defaults
  (default-oplan-directory :oplan-tf-dir "demonstrations/tf")
  nil)

(defun init-env-parameter (p env-var status)
  (set-parameter p
    (ecase status
      (:required (require-env env-var))
      (:optional (getenv env-var)))))

(defun require-env (name)
  (or (getenv name)
      (ask-user-for-env name)))

(defun ask-user-for-env (name)
  (format t "Environment variable ~A is not defined.~%" name)
  (format t "Please enter a value: ")
  (read-line t))

(defun default-oplan-directory (parameter relative-default)
  ;; The relative-default is relative to :oplan-dir
  (set-parameter parameter
    (or (get-parameter parameter)
	(concat-string (get-parameter :oplan-dir) "/" relative-default))))


;;; Routines related to command-line arguments

(defun is-set-connect (value)
  (set-parameter :connect value))

(defun is-connect-p ()
  (get-parameter :connect))

(defun is-http-p ()
  (get-parameter :http))

(defun is-server-p ()
  (get-parameter :server))

(defun is-subr-p ()
  (get-parameter :subr))

(defun is-set-config-file (filename)
  (set-parameter :config filename))



;;;; .config file processing

;;; The user can specify the .config file by using the "-config"
;;; command-line argument.  If so, the specified filename is saved
;;; as the value of the :config parameter.  If not, this parameter
;;; will be nil, which means to use the default .config file.
;;; Note that using :config <name> on the command line would also
;;; work.

;;; A user-specified .config file name is first tried as-is and then
;;; relative to $OPLANDIR/lib.  If the file is not found in either case,
;;; the default .config file is used instead.

;;; The user-specified name needn't end in ".config".  If some other
;;; file type is present, it will be used; otherwise ".config" will
;;; be added via merge-pathnames.  With this mechanism, it is not
;;; possible to refer to a file with no type in its name.

(defparameter *planner-config-file* "oplan-planner-default.config")

(defparameter *planner-exec-config-file* "oplan-planner-exec-default.config")

(defvar *config-records* nil)

(defun is-configure ()
  (let* ((std-dir (concat-string (get-parameter :oplan-dir) "/lib/"))
	 (default (if (get-parameter :allow-exec)
		      *planner-exec-config-file*
		      *planner-config-file*)))
    ;; Process the main config file.
    (setq *config-records* nil)
    (process-config-file
      (or (and (get-parameter :config)
	       (find-config-file (get-parameter :config) std-dir))
	  (concat-string std-dir default)))))

(defun find-config-file (filename std-dir)
  (let ((config-file
	 (merge-pathnames filename
			  (make-pathname :type "config"))))
    ;; Try filename in the current directory first, then in the standard
    ;; directory.
    (or (probe-file config-file)
	(probe-file (concat-string std-dir (namestring config-file)))
	(error "Can't find config file ~S." filename))))

(defun process-config-file (filename)
  (with-open-file (str filename :direction :input)
    (multiple-value-bind (records error-count)
	(read-config-records str)
      (if (> error-count 0)
	  (error "~D errors in config file ~S." error-count filename)
	(setq *config-records*
	      (append *config-records* records))))))

(defun read-config-records (instream)
  (labels
      ((<config-file> ()
	 (one-or-more #'<config-entry> :until :eof))
       (<config-entry> ()
	 (must-be :config)
	 (cons (<name> "a component name")
	       (one-or-more #'<config-line> :until :end)))
       (<config-line> ()
	 (token-case
	   ((:window-args) (<window-args-line>))
	   ((:window)      (<window-line>))
	   (t (syntax-error "Invalid config entry starting with ~S."
			    (token)))))
       (<window-args-line> ()
         (list :window-args
	       (must-satisfy #'keywordp "a window tag")
	       (must-satisfy #'listp    "a list of window arguments")))
       (<window-line> ()
	 (list :window
	       (must-satisfy #'keywordp "a window tag")
	       (must-satisfy #'stringp  "a window title")
	       (must-satisfy #'numberp  "an x position")
	       (must-satisfy #'numberp  "a y position")
	       (<width>) ; was (must-satisfy #'numberp  "a width")
	       (must-satisfy #'numberp  "a height")
	       (must-satisfy #'stringp  "a string of xterm args")))
       (<name> (description)
	 (must-satisfy #'keywordp description))
       (<width> ()
	;; We allow the width to be a product, to avoid use of "#."
	(or (token-satisfies #'numberp)
	    (let ((w (must-satisfy
		       #'(lambda (w) (match '(* $ $) w))
		      "a width")))
	      (apply #'* (cdr w))))))
    (let ((*end-token* :eof))
      (test-compile
        #'<config-file>
	#'(lambda ()
	    (read instream nil *end-token*))))))


;;;; Init routines

(defun is-init ()
  ;; Process the config file entry for the current process.
  (let* ((who (ipc-whoami))
	 (entry (assoc who *config-records*)))
    (if entry
	(dolist (line (cdr entry))
	  (ecase (car line)
	    (:window-args nil)		;handled by the process
	    (:window (is-setup-window (cdr line)))))
      (dev-error who "is-init failed."))))

(defun is-get-config-entry (who)
  ;; Return the config entry for a proccess, or nil if there is none.
  (cdr (assoc who *config-records*)))

(defun is-get-window-args (window-tag)
  ;; Get the list of arguments from a :window-args line.
  (let ((window-line (find-config-line :window-args window-tag)))
    (if window-line
	(caddr window-line)
      nil)))

(defun is-get-window-width (window-tag)
  ;; Get the width of a :WINDOW window.
  (let ((window-line (find-config-line :window window-tag)))
    (if window-line
	(nth 5 window-line)
      (error "Can't find window ~S." window-tag))))

(defun find-config-line (type tag)
  ;; N.B. Assumes unique tag for a given type.  Two processes can't
  ;; have windows with the same tag, for instance.
  (some #'(lambda (config-record)
	    (find-if #'(lambda (line)
			 (and (eq (car line) type)
			      (eq (cadr line) tag)))
		     (cdr config-record)))
	*config-records*))


;;; is-setup-window
;;;
;;; Sets up a window for IO, taking the required info from the config file.
;;;
;;; The structure of the config file entry is
;;;   :WINDOW <tag> <title> <xpos> <ypos> <width> <height> <string of extras>
;;;
;;; Arguments:
;;;   win - A list describing the window and containing the same sequence
;;;         of items as the entry in the file.
;;;
;;; Results: t or nil.
;;;
;;; Side Effects: A window should now be open with a stream set up."

(defun is-setup-window (win)
  (multiple-value-bind (tag title xpos ypos width height extras)
                       (values-list win)
    ;; /\/ This allows the geometry entries to be nil, but the parser
    ;; in read-config-records (above) doesn't.
    (let ((string
	   (if (and xpos ypos width height)
	       (format nil " -title ~S -geometry ~Dx~D+~D+~D ~A"
		              title width height xpos ypos extras)
	       (format nil " -title ~S ~A" title extras))))
      (x-open-and-register-io-win tag string))))


;;; Define is-patch-loader to load .o files if they exist and are
;;; newer than the source.

(defun is-patch-loader (&optional (subdir-in-source "."))
  "Checks if any patches to load in."
  (when (get-parameter :patches)
    (let ((files
	   (find-all-files
	     (concat-string (get-parameter :oplan-dir)
			    "/source/" subdir-in-source "/patch/")
	     "lsp"))
	  #+lucid
	  (lcl:*redefinition-action*
	   (if (get-parameter :interactive) 	;??? /\/
	       :warn
	     nil))
	  (*load-verbose* (get-parameter :interactive))
	  (loaded nil))
      (dolist (file files)
	(load-most-recent file)
	(push file loaded))
      (set-parameter :loaded-patches
	(append (get-parameter :loaded-patches) (nreverse loaded)))
      (and files t))))


;;; is-load-system is called for the -load-system command-line argument.

(defun is-load-system (namestring)
  (let ((name (intern (string-upcase namestring) (find-package '#:oplan))))
    (load-system name :silent t :recursive :if-not-loaded)))

;;; ---------------------------- Change History ----------------------------
;;; (Initials) (Date of change)           (Comment)
