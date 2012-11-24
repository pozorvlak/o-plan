;;;; File: psgraph-viewer.lsp
;;; Contains: Plan / World viewer
;;; Authors: Jeff Dalton
;;; Created: Wed Mar  4 17:00:03 1992
;;; Updated: Wed Jun  2 20:35:22 1999 by Jeff Dalton
;;; Copyright: (c) 1992, 1993, 1994 AIAI, University of Edinburgh

(in-package :oplan-plan-world-viewer)

;;; Print / Hardcopy psgraph plan view

;;; We prepare a PostScript file and then print it, optionally reduced
;;; to fit on one page.  It is assumed that the value of the :ps-printer
;;; parameter (default "lpr") will print a file on a PostScript printer.
;;; It is also possible to use a viewer such as Gostview to view the file,
;;; rather than printing it.  Such a viewer can be declared by setting
;;; the :ps-viewer parameter.  For instance, to use "gv", put the
;;; following in your oplan-init file:
;;;
;;;    (set-parameter :ps-viewer "gv -landscape")
;;;

;;; Handles viewer-args:
;;;
;;;  :mode :psgraph
;;;  :format <psgraph format>
;;;  :levels <:ALL or number>
;;;  :output-file <file name>
;;;  :title <string>
;;;

;;; Parameters:
;;;
;;;  :ps-viewer
;;;  :new-psgraph
;;;  :psgraph-all-links
;;;  :psgraph-all-nodes
;;;

;;; Functions that can be given :domain-specific advice:
;;;
;;;  (psgraph-hidden-actions) -> list of action-name initial segments
;;;  (psgraph-filter-hidden-actions eg)
;;;  (psgraph-hidden-p en)

(export '(psgraph-hidden-actions
	  psgraph-filter-hidden-actions
	  psgraph-hidden-p))

(proclaim '(notinline psgraph-hidden-actions
		      psgraph-filter-hidden-actions
		      psgraph-hidden-p))

(defparameter *one-page-formats*
  '(:hardcopy-one-page :viewer-one-page :file-one-page))

(defun output-psgraph-plan (all-nodes)
  (let ((format (get-psgraph-format)))
    (when (eq format :quit)
      (return-from output-psgraph-plan))
    (let ((nodes (slice-to-level all-nodes))
	  (outfile (graph-file-name format))
	  (shrink-p (member format *one-page-formats*))
	  (title (get-psgraph-title)))
      ;; Draw the graph
      (with-open-file (*standard-output* outfile :direction :output)
	(draw-plan-psgraph nodes shrink-p title))
      (report-action "Wrote graph.")
      ;; If to a file, we're done.
      (when (eq format :file-one-page)
	(return-from output-psgraph-plan))
      ;; Otherwise, print / view the graph.
      (let ((viewer
	     (case format
	       ((:viewer-one-page)
		(get-parameter :ps-viewer))
	       (otherwise
		(get-parmeter :ps-printer)))))
	;; Run the viewer (which might just be "lpr") in the background,
	;; together with a command to delete outfile.  That way we can
	;; go on to other things while the viewer runs.
	(system
	 (concat-string
	  "(" viewer " " outfile "; /bin/rm " outfile ")&"))))))

(defun draw-plan-psgraph (nodes shrink-p title)
  (let ((*print-case* :upcase))		;more readable when printed
    (if (get-parameter :new-psgraph)
	(new-draw-plan-psgraph nodes shrink-p title)
        (old-draw-plan-psgraph nodes shrink-p title))))

(defun old-draw-plan-psgraph (nodes shrink-p title)
  (psgraph:psgraph
    '(begin_of node-1)			;root
    #'(lambda (ne-tag)			;childf
	(ne-tag-successors ne-tag nodes))
    #'(lambda (ne-tag)			;infof
	(ne-tag-info ne-tag nodes))
    shrink-p				;force to one page?
    nil					;not insert
    #'equal
    (not (get-parameter :psgraph-all-links)) ;remove redundant links
    title))

(defun get-psgraph-format ()
  (or (viewer-arg :format) (ask-psgraph-format)))

(defun ask-psgraph-format ()
  ;; N.B. We say "hardcopy" explicitly so we can also have options
  ;; such as one to write a file w/o printing or to select one of
  ;; several viewers.
  (menu-request
    `("-heading" "Output format"
      "-line"
      ,@(if (get-parameter :ps-printer)
	    '("Hardcopy full size=:hardcopy-full-size"
	      "Hardcopy one page=:hardcopy-one-page")
	  '())
      ,@(if (get-parameter :ps-viewer)
	    '("Viewer one page=:viewer-one-page")
	  '())
      "File one page=:file-one-page"
      "-line"
      "QUIT=:quit")))

(defun ne-tag-successors (ne-tag nodes)
  (let ((node (find-node (ne-tag-node ne-tag) nodes)))
    (ecase (ne-tag-end ne-tag)
      (begin_of (nd-begin-suc node))
      (end_of   (nd-end-suc node)))))

(defun ne-tag-info (ne-tag nodes)
  (ecase (ne-tag-end ne-tag)
    ((begin_of)
     (let ((node (find-node (ne-tag-node ne-tag) nodes)))
       (list (format nil "~A ~A" (nd-node-name node) (nd-node-type node))
	     (if (member (nd-node-type node) '(dummy start finish))
		 :begin
	       ;; Upcase pattern
	       (format nil "~:@(~A~)" (nd-pattern node))))))
    ((end_of)
     (list (ne-tag-node ne-tag)
	   :end))))

(defun get-psgraph-title ()
  (or (viewer-arg :title :default-value nil)
      (make-psgraph-title)))

(defun make-psgraph-title ()
  ;; /\/: Violates modularity by grabbing values from the TA.
  ;; /\/: Won't work in -connect mode (no TA).
  (concat-string
    oplan-task-assigner::*domain*
    ":"
    oplan-task-assigner::*task*
    "   "
    (apply #'format nil "~:(~A ~A ~A~)"
	   (decode-date-short (get-universal-time)))))

(defun graph-file-name (format)
  (case format
    (:file-one-page
     (get-graph-file-name
      "File for PostScript graph"
      (default-psgraph-filename)))
    (t
     (generate-unique-filename
       (namestring (temp-filename "graph"))
       ".ps"))))

(defun get-graph-file-name (message default)
  ;; /\/: No way to say "forget it".
  (let ((name (ask-user :output-file "~A [~A]" message default)))
    (if (string= name "")
	default
      name)))

(defun default-psgraph-filename ()
  (default-viewer-filename "-graph" "ps"))

(defun default-viewer-filename (suffix type)
  ;; /\/: Violates modularity by grabbing values from the TA.
  ;; /\/: Won't work in -connect mode (no TA).
  (concat-string
    oplan-task-assigner::*domain*
    ":"
    oplan-task-assigner::*task*
    suffix
    "."
    type))


;;;; New version of draw-plan-psgraph

;;; Parameters:

;;;  :psgraph-all-links  --  true to keep redundant lines
;;;  :psgraph-all-nodes  --  true to keep dummy nodes

;;; Functions that can be given :domain-specific advice:
;;;
;;;  (psgraph-hidden-actions) -> list of action-name initial segments
;;;  (psgraph-filter-hidden-actions eg)
;;;  (psgraph-hidden-p en)

;;; This version can delete dummy nodes and do its own redundant-link
;;; removal (rather than asking psgraph to remove them).  It also draws
;;; a node as a single box (rather than as one box for each end) if
;;; unit-node-p is true and describes the ends of dummy nodes as:
;;;     <  --  a begin_of
;;;     <  --  an end_of
;;;     <> --  dummy drawn as a unit

;;; "Hidden actions" are converted to dummy nodes and can then be 
;;; deleted.  The conversion happens only if :psgraph-all-nodes is
;;; false.

;;; Unit-node-p is true of a node N if there's only one link from
;;; begin_of N and only one link to end_of N and that one link directly
;;; links the two ends.  (It might link them indirectly if redundant
;;; links have been removed, because the direct link might have counted
;;; as redundant.)  Doing our own redundant-link removal lets us discover
;;; mode unit nodes -- otherwise, we could just let psgraph do it.

;;; Actually, unit-node-p returns true for the *begin* end of the node,
;;; i.e. for the enode whose en-end is begin_of.  So the begin-end represents
;;; the whole (PLAN-)node.


(defvar *psgraph-all-nodes* nil)

(defvar *egraph* nil)

(defun new-draw-plan-psgraph (nodes shrink-p title)
  (let ((*psgraph-all-nodes* (get-parameter :psgraph-all-nodes))
	(*egraph* (simplify-for-psgraph (make-end-graph-from-nodes nodes))))
    (psgraph:psgraph
      (eg-start-node *egraph*)		;root
      #'psgraph-successors		;childf
      #'psgraph-info			;infof
      shrink-p				;force to one page?
      nil				;not insert
      #'eq
      nil				;don't remove redundant links
      title)))

(defun simplify-for-psgraph (eg)
  (unless (get-parameter :psgraph-all-links)
    (eg-delete-extra-links eg))
  (unless (get-parameter :psgraph-all-nodes)
    (psgraph-filter-hidden-actions eg)
    (when (and (eg-delete-dummies eg)
	       (not (get-parameter :psgraph-all-links)))
      ;; We've deleted some dummies, and we're allowed to delete links.
      ;; So maybe with fewer dummies there are more links we can delete,
      ;; and maybe that will let us delete more dummies.
      (loop
        (unless (eg-delete-extra-links eg)
	  (return))			;no links deleted, so we're done
	(unless (eg-delete-dummies eg)
	  (return)))))			;no dummies deleted, so we're done
  eg)

#+:undef
(defun simplify-for-psgraph (eg)
  (unless (get-parameter :psgraph-all-nodes)
    (eg-delete-dummies eg))
  (unless (get-parameter :psgraph-all-links)
    (eg-delete-extra-links eg))
  eg)

(defun psgraph-hidden-actions ()
  '())

(defun psgraph-filter-hidden-actions (eg)
  (do-egraph-nodes (en eg)
    (when (psgraph-hidden-p en)
      (setf (en-type en) 'dummy))))

(defun psgraph-hidden-p (en)
  ;; Actions in (psgraph-hidden-actions) are hidden.
  ;; Initial segments count as well as exact matches.
  (let ((pat (en-pattern en)))
    (dolist (name (psgraph-hidden-actions) nil)
      (when (eql (search (symbol-name name)
                         pat
                         :test #'char-equal)
                 1)
        (return t)))))

;;; Successors (= children) for the graph-drawer

(defun psgraph-successors (en) ; -> list of enodes
  (if (unit-node-p en)
      ;; Remember that en is a begin_of end.  So we're taking the
      ;; successors of the end_of end.
      (en-suc-enodes (en-other-end-enode en))
    ;; Ordinary, non-unit, node-end.
    (en-suc-enodes en)))

(defun en-suc-enodes (en) ; -> list of enodes
  (mapcar #'(lambda (i) (egref *egraph* i))
	  (en-suc en)))

(defun en-other-end-enode (en)
  (egref *egraph* (en-other-end en)))

(defun unit-node-p (en)
  ;; True if only one link from begin_of node and one to end_of node
  ;; and the one link directly links the two ends of the node.
  (and (eq (en-end en) 'begin_of)
       (length=1 (en-suc en))
       (length=1 (en-pre (en-other-end-enode en)))
       (fix= (car (en-suc en)) (en-other-end en))))

;;; "Info" for the graph-drawer

(defun psgraph-info (en)
  (when (en-gone-p en)
    (warn "Deleted enode ~D survives" (en-index en)))
  (cond ((unit-node-p en)
	 (assert (eq (en-end en) 'begin_of))
	 (case (en-type en)
	   ((start finish)
	    (list (en-name en)
		  (en-type en)))
	   ((dummy)
	    (list (if *psgraph-all-nodes*
		      (format nil "<~A>" (en-number en))
		    "<>")))
	   (t
	    (list (format nil "~A ~A" (en-name en) (en-type en))
		  ;; Upcase pattern
		  (format nil "~:@(~A~)" (en-pattern en))))))
	((eq (en-end en) 'begin_of)
	 (case (en-type en)
	   ((start finish)
	    (list (format nil "~A ~A" (en-name en) (en-type en))
		  :begin))
	   ((dummy)
	    (list (if *psgraph-all-nodes*
		      (format nil "<~A" (en-number en))
		    "<")))
	   (t
	    (list (format nil "~A BEGIN ~A" (en-name en) (en-type en))
		  ;; Upcase pattern
		  (format nil "~:@(~A~)" (en-pattern en))))))
	((eq (en-end en) 'end_of)
	 (case (en-type en)
	   ((dummy)
	    (list (if *psgraph-all-nodes*
		      (format nil "~A>" (en-number en))
		    ">")))
	   (t
	    (list (en-name en)
		  :end))))
	(t
	 (error "Messed up enode ~D" (en-index en)))))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
