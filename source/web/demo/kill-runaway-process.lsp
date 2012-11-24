;;;; File: kill-runaway-process.lsp
;;; Contains: CGI-invoked code to kill a runaway O-Plan
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: September 1998
;;; Updated: Thu Sep 10 18:23:35 1998 by Jeff Dalton
;;; Copyright: (c) 1998, AIAI, University of Edinburgh

(in-package :oplan)

;;; This is invoked by "kill-oplan.cgi".  Edit that file to change
;;; the pid to kill.

(defun kill-runaway-process ()
  (with-web-environment "kill-runaway-process"
    (let* ((enemy-pid (string->int (get-parameter :enemy-pid)))
	   (exists-p (process-still-exists-p enemy-pid)))
      (kill-note "Process ~S exists? ~S" enemy-pid exists-p)
      (when exists-p
        (kill-note "Killing -INT pid ~D" enemy-pid)
        (system (format nil "kill -INT ~A" enemy-pid))
        (sleep 3))
      (when (process-still-exists-p enemy-pid)
	(kill-note "Still exists, so killing -KILL pid ~D" enemy-pid)
	(system (format nil "kill -KILL ~A" enemy-pid))
	(sleep 3))
      (if (process-still-exists-p enemy-pid)
	  (kill-note "Failed.")
	(kill-note "It's gone.")))))

(defun process-still-exists-p (pid)
  (let ((status-lines
	 (stream->lines
	  (fork:process-receive "ps" "-p" (int->string pid)))))
    (web-note "~&Status of pid ~D:~%~%~{~S~%~}~%" pid status-lines)
    (> (length status-lines) 1)))

(defun kill-note (format-string &rest format-args)
  (web-note "~&~?~%~%" format-string format-args)
  (html-paragraph
    (apply #'html-line format-string format-args)))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
