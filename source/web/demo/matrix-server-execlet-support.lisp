;;;; File: matrix-server-execlet-support.lsp
;;; Contains: Support code for COA matrix Web demos
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: October 1999
;;; Updated: Sun Nov 21 21:44:28 1999 by Jeff Dalton
;;; Copyright: (c) 1999, AIAI, University of Edinburgh

(in-package :oplan)

(in-local-defun-class matrix-server-exec-mixin *demo*)


;;;; The exec-plan execlet page

;;; We write this page when an :exec-plan request specifies "Applet"
;;; as the interface.

(defun-local write-coa-exec-plan-execlet-page (coa)
  (html-standard-page (:title (title-for-user-and-coa coa "Plan Execution")
		       :centered-title-header t)
    (html-line "<hr>")
    (html-block `("applet"
		  ("codebase" ,(web-demo-url "execlet/"))
		  ("code" "oplan/execlet/Execlet.class")
		  ; ("archive" "execlet.jar")
		  ("width" "700") ("height" "240"))
      ())
    (html-line "<hr>")))


;;;; Answering Execlet requests for plans

;;; The requests are:

;;;   * Expansion "tree".
;;;   * Execution plan (what the HTML exec stuff uses as
;;;     its complete description of the plan -- it includes
;;;     descriptions of effects and conditions)
;;;   * Current exec status (a status value for each node-end)
;;;   * Current world state (pattern-value pairs)
;;;   * Always facts (pattern-value pairs)

;;; Expansion tree

;;; Sends a tree as (branch ...) where a branch is (tag pattern subtree).

(add-http-uri-interpretation "/exec-query/tree/"
  'http-exec-query-tree)

(defun-local http-exec-query-tree (path r stream)
  (check-exec-query-allowed)
  (send-exec-query-response path r stream
    (get-coa-expansion-tree (demo-executing-coa *demo*))))

(defun-local get-coa-expansion-tree (coa)
  (let* ((evals (coa-raw-evaluations coa))
	 (levels (lookup :plan-levels evals))
	 (tree (getf levels :expansion-tree)))
    (assert evals)
    (assert levels)
    (assert tree)
    tree))

;;; Execution plan

;;; Sends (stage ...) where stage is (step ...) and step is
;;; (tag type pattern est lst status).

(add-http-uri-interpretation "/exec-query/exec-plan/" 
  'http-exec-query-exec-plan)

(defun-local http-exec-query-exec-plan (path r stream)
  (check-exec-query-allowed)
  (send-exec-query-response path r stream
    (mapcar #'(lambda (stage)
		(mapcar #'exec-step-for-execlet
			stage))
	    (coa-exec-plan (demo-executing-coa *demo*)))))

(defun exec-step-for-execlet (step)
  (list (exec-step-tag step)
	(exec-step-type step)
	(exec-step-pattern step)
	(exec-step-earliest-time step)
	(exec-step-latest-time step)
	(exec-step-status step)))

;;; Execution status

;;; Sends ((tag status) ...).

(add-http-uri-interpretation "/exec-query/exec-status/"
   'http-exec-query-exec-status)

(defun-local http-exec-query-exec-status (path r stream)
  (check-exec-query-allowed)
  (let ((result (make-tconc)))
    (dolist (stage (coa-exec-plan (demo-executing-coa *demo*)))
      (dolist (step stage)
	(tconc result (list (exec-step-tag step) (exec-step-status step)))))
    (send-exec-query-response path r stream
      (tconc-contents result))))

;;; World state

;;; Sends ((pattern value) ...).

(add-http-uri-interpretation "/exec-query/world-state/"
  'http-exec-query-world-state)

(defun-local http-exec-query-world-state (path r stream)
  (check-exec-query-allowed)
  (with-coa-exec-state ((demo-executing-coa *demo*))
    (send-exec-query-response path r stream
      (nice-pv-pairs-from-hash-table
        *world-state*))))

;;; Always facts

;;; Sends ((pattern value) ...).

(add-http-uri-interpretation "/exec-query/always/"
  'http-exec-query-always)

(defun-local http-exec-query-always (path r stream)
  (check-exec-query-allowed)
  (with-coa-exec-state ((demo-executing-coa *demo*))
    (send-exec-query-response path r stream
      (nice-pv-pairs-from-hash-table
        *world-always-table*))))


;;; Response utilities

(defun check-exec-query-allowed ()	;does this make the right test?  /\/
  (unless (and (demo-executing-coa *demo*)
	       (coa-exec-control (demo-executing-coa *demo*)))
    (bogus-matrix-request-error)))

(defun send-exec-query-response (path r stream response-data)
  (unless (string= path "")
    (http-request-error r 'http-request-forbidden))
  (send-http-response-headers r stream
    :content-type "text/plain"
    :no-cache t)
  (print-readably response-data stream)
  (terpri stream)
  (finish-output stream))

(defun nice-pv-pairs-from-hash-table (ht)
  (canonical-description-order
    (loop for p being each hash-key of ht
	  using (hash-value v)
	  collect (pv-pair p v))))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
