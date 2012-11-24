;;;; File: world-loader.lsp
;;; Contains: World definition loader
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: May 1995
;;; Updated: Tue Mar 23 02:07:03 1999 by Jeff Dalton
;;; Copyright: (c) 1995, AIAI, University of Edinburgh

(in-package :oplan-world)

;;;; Loading world definitions
;;;
;;; Relevant parameters:
;;;
;;;  :world-dir -- the WorldSim root directory.
;;;  :oplan-dir -- the O-Plan root directory.
;;;  :world     -- a directory containing a world definition to load.
;;;
;;; Note that the separate world assigns the value of :world-dir to
;;; :oplan-dir if :oplan-dir isn't already defined.
;;;
;;; The default directory for world definitions is the "lib/worlds"
;;; subdirectory of :world-dir.  If no :world-dir has been specified,
;;; the "lib/worlds" subdirectory of :oplan-dir is used instead.
;;; If neither :world-dir nor :oplan-dir is defined, an error is signalled.
;;;
;;; /\/: Maybe :world-dir should be the directory of world definitions.
;;; /\/: Maybe the root dir should be :world-root or :world-home.
;;;
;;; A world definition is a directory containing the files that define
;;; the world.  The definitions directory (e.g. lib/worlds in :world-dir)
;;; is therefore a directory of directories.  Each world directory W
;;; should contain a Lisp source file named W-world.lsp.  There may
;;; also be a correcponding object file.  The world is loaded by loading
;;; the source or object, whichever is newer.  During this load, the
;;; (Unix) working directory will be the directory W.  This should make
;;; it easier for W-world to load a world definition that contains several
;;; files.  (OTOH, W-world may be the entire definition itself.)
;;;
;;; If no :world has been specified, we could ask the user what
;;; world in the definitions directory to load.  At present, we
;;; just use *default-world-definition*.  This makes :world like
;;; :config and not like a TF file.
;;;
;;; :world contains a "/", it is used as-is; otherwise it is taken
;;; relative to the defintions directory.
;;; 

(defun load-world-definition ()
  (let ((path (world-definition-file)))
    (with-working-directory (namestring (pathname->directory path))
      (load path))))

(defun world-definition-file ()
  ;; Returns the true name of the file as a pathname.
  (let* ((name (or (get-parameter :world) *default-world-definition*))
	 (path (if (find #\/ name)
		   name
		 (concat-string (world-definitions-dir) "/" name)))
	 ;; Path should refer to a directory that contains a world definition.
	 (world-name
	  (pathname-name (parse-namestring (string-right-trim "/" path))))
	 (world-path
	  (concat-string path "/" world-name "-world")))
    (truename
     (find-most-recent world-path))))

(defun world-definitions-dir ()
  (concat-string
    (or (get-parameter :world-dir)
	(get-parameter :oplan-dir)
	(error "Can't determine the world-defintions directory."))
    "/lib/worlds"))

(defun pathname->directory (pathname) ; -> pathname
  (make-pathname
    :host (pathname-host pathname)
    :device (pathname-device pathname)
    :directory (pathname-directory pathname)))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
