;;;; File: volume-groups-support.lisp
;;; Contains: Support code for the volume groups Web demo
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: May 1997
;;; Updated: Sun Sep 28 19:41:52 1997 by Jeff Dalton
;;; Copyright: (c) 1997, AIAI, University of Edinburgh

(in-package :oplan)

(defparameter *volume-groups-parameters*
  '((:n-logical-volumes   (:int 0 4)  "the number of logical volumes")
    (:n-physical-volumes  (:int 1 4)  "the number of physical volumes")))

(defun volume-groups-cgi ()
  (with-web-environment "volume-groups"
    (parse-query-args)
    (convert-query-args *volume-groups-parameters*)
    (set-parameter :oplan-tf-dir
      (web-demo-filename "standard-tf"))
    (html-line "<HR>")
    (html-print-oplan-greeting)
    (if (prog1 ; (html-time (plan-vgremove))
	         (plan-vgremove)
	  (html-line "<HR>"))
	(report-vg-success)
      (report-vg-failure))))

(defun plan-vgremove ()
  (let ((n-logical   (query-arg :n-logical-volumes))
	(n-physical  (query-arg :n-physical-volumes))
	(domain-name "volume-groups")
	(task-name   "task_web_demo_base"))

    ;; Start with the standard sequence of messages for establishing
    ;; the domain.
    (send-to-oplan :init)
    (receive-else-error '(:init-ok))
    (send-to-oplan :domain domain-name)
    (receive-else-error '(:domain ($tf-file-name . $task-names)))

    ;; Allow the planner to expand the task, but block any further
    ;; planning.
    (set-authority :level 0)

    ;; Now set the task.  The planner will say it is waiting for
    ;; authority.
    (send-to-oplan :set-task task-name)
    (receive-else-error '(:waiting (:authority :triggers)))

    ;; Set up initial conditions
    (send-to-oplan :add-to-task :all-of
      `(:initially
	(logical_contents vg0)
	,(loop for i from 1 to n-logical
	       collect (concat-name 'lv (int->string i))))
      `(:initially
	(physical_contents vg0)
	,(loop for i from 1 to n-physical
	       collect (concat-name 'pv (int->string i)))))

    ;; Now get a plan.
    (set-authority :level :inf)
    (pprocess-main-loop)
    (receive-plan-status)))


;;;; Success

(defun report-vg-success ()
  (html-report-plan-statistics)
  (report-vg-script)
  (html-paragraph
    (html-anchor (web-demo-url "standard-tf/volume-groups.tf")
		 "The TF file"))
  (html-line "<BR>")
  (web-mail-comment-link)
  (web-note-success))


;;;; Plan -> shell script

(defun report-vg-script ()
  (html-paragraph
    (html-line "Script:"))
  (html-block "PRE"
    (dolist (line (db-call 'vg-script-lines))
      (html-line "~A"
		 (html-encode-pre-string line)))))

(defun vg-script-lines ()
  (assert (eq *pprocess* (find-pprocess :dm)))
  (cons "#!/bin/sh"
	(loop for ne in (tsort-node-ends)
	      when (ne-unix-command-p ne)
	      collect (ne-unix-command-string ne))))

(defun tsort-node-ends ()
  (tsort (earliest-node-end) #'ne-post-ends))

(defun ne-unix-command-p (ne)
  (let ((n (ne-node ne)))
    (and (eq (n-type n) 'action)
	 (eq (etag-end (ne-tag ne)) :begin)
	 (member (first (n-pattern n)) '(type comment)))))

(defun ne-unix-command-string (ne)
  (let* ((n (ne-node ne))
	 (p (psv-actorise-pattern (n-pattern n))))
    (ecase (first p)
      (type
       (format nil "~{~A ~}"
	       (mapcar #'cvt-unix-command-elt (second p))))
      (comment
       (format nil "# ~{~A ~}"
	       (second p))))))

(defun cvt-unix-command-elt (e)
  (case e
    (rm_r                "rm -r")
    (usr_sbin_lvremove_f "/usr/sbin/lvremove -f")
    (usr_sbin_vgreduce   "/usr/sbin/vgreduce")
    (usr_sbin_vgremove   "/usr/sbin/vgremove")
    (usr_umount          "/usr/umount")
    (t
     e)))


;;;; Failure

(defun report-vg-failure ()

  (html-paragraph
    (html-line "No plan is possible."))

  (html-report-plan-statistics)

  (html-paragraph
    (html-line "Referece links:")
    (html-block "UL"
      ;; The TF file
      (html-item "LI"
	(html-anchor (web-demo-url "standard-tf/volume-groups.tf")
		     "The TF file"))))

  (web-mail-comment-link)
  (web-note-failure "no plan was possible"))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
