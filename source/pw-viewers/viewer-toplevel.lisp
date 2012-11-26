;;;; File: viewer-toplevel.lisp
;;; Contains: Plan / World viewer toplevel
;;; Authors: Jeff Dalton
;;; Created: Wed Mar  4 17:00:03 1992
;;; Updated: Wed Jun  2 20:37:06 1999 by Jeff Dalton
;;; Copyright: (c) 1992, 1993, 1994 AIAI, University of Edinburgh

;;; Plan/World-Viewer
;;;
;;; This module is given a plan or world description and produces output
;;; in various forms: text to screen, text to file, AutoCAD, etc.
;;;
;;; The viewer is used by (1) getting a view by calling pw-get-plan-for-
;;; viewing or pw-get-world-for-viewing, and (2) calling pw-handle-view.
;;; The args for pw-handle-view look like this:
;;;
;;;    :PLAN (node-description*) viewer-arg...
;;; or
;;;    :WORLD node-id ((pattern value)*) viewer-arg...
;;;
;;; The viewer-args are used to answer questions about how the plan or
;;; world view should be handled (e.g. whether it should be written to
;;; a file and, if so, which file).  They should be a p-list of the form
;;; ( {keyword value}... ).  One keyword should be :mode to specify the
;;; display mode.  The other keywords will depend on the mode.
;;;
;;; The viewer-args are optional, but if any appear they indicate that
;;; the viewer is under program control.  All questions will be answered
;;; from the args and an error will be signalled if a requested keyword
;;; is not present (though there are default values in some cases --
;;; see the viewer-arg procedure).  Moreover, the viewer will not create
;;; a window or procuce any window output unless specifically told to
;;; put something on the screen.
;;;
;;; If no viewer-args appear, then all questions will be answered by
;;; asking the user (e.g. by putting up a menu) and the viewer process
;;; will automatically create a window when it starts running, unless
;;; it's been told to use an existing window by the caller binding
;;; *pw-viewing-window*
;;;

(in-package :oplan-plan-world-viewer)

(use-package :oplan-tf)

(use-package :oplan-x)
(use-package :oplan-pseudo-process)		;/\/ too low-level?
(use-package :oplan-util)
(use-package :oplan-time-util)
(use-package :oplan-ipc)
(use-package :oplan-initsystem)

(import '(oplan::node-1 oplan::unknown))

(defvar *pw-viewing-window* :pw-window)	;name of window viewer should use

(defvar *window* nil)			;current request's screen-I/O stream

(defvar *viewer-io-package*		;package to read and write in
  (find-package :oplan))

(defvar *plan-display-mode* nil)
(defvar *world-display-mode* nil)
(defvar *max-plan-level* nil)		;max levels of expansion to show

(defvar *nodes* nil)			;to aid debugging, etc.

;;; View handler

;;; The caller should bind *pw-viewing-window* to a window name (as in
;;; x-get-stream) if there's a window we're supposed to use.

;;; /\/: Perhaps instead there should be a var whose value is a function
;;; that will create the window.  That would give the caller more control
;;; over how the window was set up.

(defun pw-handle-view (type &rest whatever)
  (let ((*errors-are-harmless* t)
	(*window* 'not-a-window-yet))
    (catch 'viewer-exit
      (apply (ecase type
	       (:plan  #'view-plan)
	       (:world #'view-world))
	     whatever))))

(defun exit-viewer ()
  (throw 'viewer-exit nil))

(defun exit-view ()
  (throw 'view-exit nil))		;see below

;;;
;;; World view
;;;

(defun view-world (node-id p-v-pairs &rest viewer-args)
  (common-viewer-setup viewer-args)
  (display-banner "World View at end_of node-~A" node-id)
  (unwind-protect
      (output-viewer-world p-v-pairs)
    (common-viewer-finish)
    (display-divider)))

;;; Viewer world-view

(defun output-viewer-world (p-v-pairs)
  (loop
    (setq *world-display-mode* (get-world-display-mode))
    (catch 'view-exit
      (ecase *world-display-mode*
	(:screen  (output-text-world p-v-pairs))
	(:file    (output-file-world p-v-pairs))
	(:quit    (return))))
    (force-output *window*)
    (unless (interactivep)
      (return))))

(defun get-world-display-mode ()
  (or (viewer-arg :mode) (ask-world-display-mode)))

(defun ask-world-display-mode ()
  (menu-request
    '("-heading" "World display mode"
      "-line"
      "Text to screen=:screen"
      "Text to file=:file"
      "-line"
      "QUIT=:quit")))

;;;
;;; Plan View
;;;

(defun view-plan (nodes &rest viewer-args)
  (common-viewer-setup viewer-args)
  (setq *nodes* nodes)		;to aid debugging
  (fix-up-nodes nodes)		;via side effects
  (display-banner "Plan View")
  (unwind-protect
      (output-viewer-plan nodes)
    (common-viewer-finish)
    (display-divider)))

;;; Viewer plan view

(defun output-viewer-plan (nodes)
  (loop
    (setq *plan-display-mode* (get-plan-display-mode))
    (catch 'view-exit
      (ecase *plan-display-mode*
	(:screen    (output-text-plan nodes))
	(:file      (output-file-plan nodes))
	(:psgraph   (output-psgraph-plan nodes))
	(:narrative (output-narrative-plan nodes))
	(:xml       (output-xml-plan nodes))
	(:quit      (return))))
    (force-output *window*)
    (unless (interactivep)
      (return))))

(defun get-plan-display-mode ()
  (or (viewer-arg :mode) (ask-plan-display-mode)))

(defun ask-plan-display-mode ()
  (menu-request
    `("-heading" "Plan display mode"
      "-line"
      "Text to screen=:screen"
      "Text to file=:file"
      ,@(when (or (get-parameter :ps-printer) (get-parameter :ps-viewer))
	      '("PostScript graph=:psgraph"))
      ,@(when (ensure-xml-printer) '("XML=:xml"))
      "Narrative=:narrative"
      "-line"
      "QUIT=:quit")))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
;;;  jd    March 93	Extensive modification and cleanup for version 2.0.
;;;
;;;  jd    June 93      Receive planner output as lists rather than strings.
;;;
;;;  jd    October 93   Split AutoCAD drawer into a spearate file.
;;;
;;;  jd    April 94	PSGraph drawer that draws nodes as one box when
;;;			the only link from the begin_of the node is to the
;;;			end_of the same node and vice versa.
;;;
;;;  jd    Sept 94	Reorganized into separate files for text, psgraph
;;;                     etc; and into -toplevel and -services.
;;;
;;;  jd    October 94	Added viewer-args so that the viewer could be
;;;			subject to programmatic control.
;;;
;;;  jd    October 94   Changed "Print graph" plan-view option to
;;;			"PostScript graph".
;;;
;;;  jd    September 96 Removed AutoCAD and KRSL viewers
;;;
;;;  jd    October 96   Converted to run as a procedure rather than as
;;;			as a separate process.
;;;
;;;  jd    June 99	Now pays attention to the :ps-printer and
;;;			:ps-viewer parameters.
;;;

