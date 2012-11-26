;;;; File: any-tf-matrix-support.lsp
;;; Contains: Support code for generic matrix demo
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: April 1999
;;; Updated: Tue Feb 22 04:25:59 2000 by Jeff Dalton
;;; Copyright: (c) 1997, 1998, 1999, AIAI, University of Edinburgh

(in-package :oplan)


;;;; The demo class

(defclass any-tf-matrix-demo (matrix-server-exec-mixin matrix-server-demo)
  ((task-names :accessor demo-task-names)))

(in-local-defun-class any-tf-matrix-demo *demo*)


;;;; Startup

;;; This happens (only) when we get the first request from a user.
;;; So only the first user to join can set the TF file.

;;; Note that we want the form that's used to start the demo to be
;;; able to specify a TF file that has a "/" in the name, like this:
;;;
;;;   <option value="group/suo-thread-2">SUO Thread 2
;;;
;;; Note that it's not an absolute pathname, because it doesn't
;;; start with "/"; and we're not planning to support absolute
;;; pathnames, only relative ones, whether or not they contain a "/".
;;;
;;; We need a domain name to give to O-Plan and a URL that points
;;; to the TF file.  We'd like the domain name to be taken relative
;;; to :oplan-tf-dir, but O-Plan treats names that contain "/" as-is,
;;; not relative to the TF directory.  Our current directory when
;;; running is the O-Plan "source" directory, so the TF compiler
;;; will look there if the name contains a "/".  We therefore
;;; have to add the right prefix ourselves.  This is done by putting
;;; "standard-tf" on the front, ".tf" on the back, and then calling
;;; web-demo-filename to get the domain name and web-demo-url to
;;; get the URL.  /\/

(defun-for any-tf-matrix-demo initialize-matrix-demo ()

  ;; Get the name of the TF file and provide an opportunity to set
  ;; any other parameters.
  (install-demo-parameters)

  (web-note "~&Set demo TF file: ~S~%" (demo-tf-file *demo*))
  (web-note "~&Set demo TF URL: ~S~%~%" (demo-tf-url *demo*))

  ;; Point to the right TF directory
  ; (set-parameter :oplan-tf-dir (web-demo-filename "standard-tf"))

  ;; Initialize O-Plan
  (send-to-oplan :init)
  (receive-else-error '(:init-ok))

  ;; Set the domain and get the list of available tasks
  (send-to-oplan :domain (demo-tf-file *demo*))
  (receive-else-error '(:domain ($tf-file-name . $task-names))
    #'(lambda (file-name task-names)
	(declare (ignore file-name))
	(setf (demo-task-names *demo*) task-names)))

  ;; Plan for an empty task
  (send-to-oplan :set-task :empty)
  (assert (receive-plan-status))

  ;; Check that we have the expected option: option-1.
  (send-to-oplan :get-option)
  (receive-else-error '(:option option-1)))

(defun-for any-tf-matrix-demo install-demo-parameters ()
  (setq *query-arg-table* *server-cgi-query-args*)
  (convert-query-args '((:tf-file (:text) "TF file name")))
  (let* ((tf-file (string-downcase (query-arg :tf-file)))
	 (relative-name (concat-string "standard-tf/" tf-file ".tf")))
    (setf (demo-tf-file *demo*)
	  (web-demo-filename relative-name))
    (setf (demo-tf-url *demo*)
	  (web-demo-url relative-name))))


;;;; Parameters


;;;; Matrix page

;;; COA description table

(defun-for any-tf-matrix-demo write-coa-description-table ()
  (when (some #'coa-defined-p *coas*)
    (html-tag-line "h2" "COA tasks")
    (html-aiai-table ()
      ;; Header row
      (html-item "tr"
        (html-item "th" "COA")
	(html-item "th" "Task"))
      ;; Description rows
      (do-visible-coas (coa)
        (when (coa-defined-p coa)
          (html-item "tr"
	    ;; COA number
	    (html-item "td align=center"
              (html-format "~A" (coa-visible-number coa)))
	    ;; Task
	    (html-item "td"
	      (html-format "~A"
		(lookup :task (coa-definition-parameters coa))))))))))


;;;; COA definition page

(defparameter *coa-definition-parameters*
  '((:n    (:int 0)    "COA number")
    (:task (:text)     "task name")))

;;; COA definition table

(defun-for any-tf-matrix-demo write-coa-definition-table (coa)
  (declare (ignore coa))
  (let ((available-tasks (demo-task-names *demo*)))
    (html-tag-line "h2" "Select a task")
    (html-block "table"
      (html-item "tr"
        (html-item "td"
          (html-select-from-list
            :name :task
	    :size 1
	    :options
	      (mark-selected (first available-tasks)
		available-tasks)))))))


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;

