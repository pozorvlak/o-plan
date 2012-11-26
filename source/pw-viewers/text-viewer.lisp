;;;; File: text-viewer.lsp
;;; Contains: Text and File Plan / World viewer
;;; Authors: Jeff Dalton and Brian Drabble
;;; Created: Wed Mar  4 17:00:03 1992
;;; Updated: Tue Jul  9 01:18:53 1996 by Jeff Dalton
;;; Copyright: (c) 1992, 1993, 1994 AIAI, University of Edinburgh

(in-package :oplan-plan-world-viewer)

;;; Text/screen world-view

;;; Handles viewer-args:
;;;
;;;  :MODE :SCREEN
;;;  :MODE :FILE
;;;  :OUTPUT-FILE <file name>
;;;

(defun output-text-world (p-v-pairs)
  (print-text-world p-v-pairs)
  (format *window* "~%"))

(defun print-text-world (p-v-pairs &optional (out *window*))
  (let ((*print-case* :downcase)
	(*print-length* nil)
	(*print-level* nil)
	(*package* *viewer-io-package*))
    (format out "~:{  ~S = ~S~%~}" p-v-pairs)))

;;; File world view

(defun output-file-world (p-v-pairs)
  (let* ((name (ask-user :output-file "File for world view"))
	 (temp-name (temp-filename name)))
    (unless (string= name "")
      (with-open-file (out temp-name :direction :output)
	(format out "~&world~%")
	(print-text-world p-v-pairs out)
	(format out "end_world~%~%"))
      (report-action "Wrote ~A." temp-name))))


;;; Text plan view

(defun output-text-plan (nodes &optional (out *window*))
  (let ((*print-case* :downcase)
	(*print-length* nil)
	(*print-level* nil)
	(*package* *viewer-io-package*))
    (dolist (n nodes)
      (format out "Node Number: ~S ~%" (nd-node-name n))
      (format out "    Begin_end Predecessors : ~S ~%"
	                 (flatten-pairs (nd-begin-pre n)))
      (format out "    Begin_end Successors   : ~S ~%"
	                 (flatten-pairs (nd-begin-suc n)))
      (format out "    End_end Predecessors   : ~S ~%"
	                 (flatten-pairs (nd-end-pre n)))
      (format out "    End_end Successors     : ~S ~%"
	                 (flatten-pairs (nd-end-suc n)))
      (format out "    Earliest start time    : ~A ~%"
	                 (plan-time-string (nd-est n)))
      (format out "    Latest start time      : ~A ~%"
	                 (plan-time-string (nd-lst n)))
      (format out "    Earliest finish time   : ~A ~%"
	                 (plan-time-string (nd-eft n)))
      (format out "    Latest finish time     : ~A ~%"
	                 (plan-time-string (nd-lft n)))
      (format out "    Minimum duration       : ~A ~%"
	                 (plan-time-string (nd-min-duration n)))
      (format out "    Maximum duration       : ~A ~%"
	                 (plan-time-string (nd-max-duration n)))
      (format out "    Node_type              : ~S ~%"
	                 (nd-node-type n))
      (format out "    Node_pattern           : ~S ~%"
	                 (nd-pattern n))
    )
    (format out "~%")
  )
)


;;; File plan view

(defun output-file-plan (nodes)
  (let* ((name (ask-user :output "File for plan view"))
	 (temp-name (temp-filename name)))
    (unless (string= name "")
      (with-open-file (out temp-name :direction :output)
	(write-file-plan nodes out))
      (report-action "Wrote ~A." temp-name))))

(defun write-file-plan (nodes &optional (out *window*))
  (let ((*print-case* :downcase)
        (*print-length* nil)
        (*print-level* nil)
        (*package* *viewer-io-package*))
    (format out "~&plan~%~%")
    (dolist (n nodes)
      (format out "  node~%")
      (format out "    ~S ~%" (nd-node-name n))
      (format out "    ~S ~%" (flatten-pairs (nd-begin-pre n)))
      (format out "    ~S ~%" (flatten-pairs (nd-begin-suc n)))
      (format out "    ~S ~%" (flatten-pairs (nd-end-pre n)))
      (format out "    ~S ~%" (flatten-pairs (nd-end-suc n)))
      (format out "    ~S ~%" (nd-time-info n))
      (format out "    ~S ~%" (nd-node-type n))
      (format out "    ~S ~%" (nd-pattern n))
      (format out "  end_node~%~%")
    )
    (format out "end_plan~%~%")
  )
)

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
