;;;; File: control-panel.lisp
;;; Contains: The Lisp interface to the control panel (xuim)
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: March 1993
;;; Updated: Thu Nov 14 02:58:12 1996 by Jeff Dalton
;;; Copyright: (c) 1992, 1993, AIAI, University of Edinburgh

;;; The Lisp interface to the control panel (xuim)

(in-package :oplan)

(defvar *control-io* nil)

(defun set-up-control-panel ()
  (when (and (get-parameter :interactive)
	     (get-parameter :windows))
    (define-cp-message-handlers)
    (let ((*debug-io* *debug-io*)
	  (*trace-output* (make-synonym-stream '*debug-io*))
	  (*error-output* (make-synonym-stream '*debug-io*)))
      (new-pprocess :xuim
         :specials '(*debug-io* *trace-output* *error-output*)
         :status :run
         :run-function 'xuim-startup))))

(defun xuim-startup (self)
  (setq *control-io* (run-xuim))
  (push *control-io* (pprocess-input-streams self))
  (setq *debug-io* (pprocess-symbol-value (find-pprocess :im) '*debug-io*))
  ;; Equiv of (ipc-register-with-im) is sent by xuim when it starts.
  (ipc-set-event-handler 'xuim-event-handler))

(defun run-xuim ()
  (apply #'ipc-run-program
     (concat-string (get-parameter :oplan-dir) "/bin/xuim")
     (or (is-get-window-args :xuim)
	 '("-title" "O-Plan Control Panel (CP)"
	   "-name" "O-Plan CP"
	   "-geometry" "+459+16"))))

;;; The event handler:
;;;  * Takes messages from the IM and outputs them to *control-io*.
;;;  * Takes input from *control-io* and sends it to the IM as :CONTROL
;;;    messages.

(defun xuim-event-handler (self)
  (when (next-event-p self)
    (handle-message-to-xuim (message-contents (next-event self))))
  (when (listen *control-io*)
    (let ((msg (read *control-io*)))
      #+kcl
        (pprocess::really-clear-whitespace *control-io*)
      (handle-message-from-xuim msg))))

;;; Define "message forwarding" handlers for all messages we can
;;; accept.  We need this because of the way ipc-ask works.  It calls
;;; handlers directly.

(defun define-cp-message-handlers ()
  (dolist (message-id (ipc-list-all-handled-messages :COMPONENT-DEFAULT))
    (assert (not (ipc-exists-handler :xuim message-id)))
    (setf (ipc-get-handler :xuim message-id)
	  (let ((id message-id))	;dolist assignment pitfall
	    #'(lambda (&rest args)
		(handle-message-to-xuim (cons id args)))))))

;;; /\/: Both Lucid and KCL signal an error when we try to close
;;; *control-io*, presumably because xuim has terminated and (hence?)
;;; (somehow) caused at least half of the two-way stream to become
;;; closed.  In Lucid, you can normally close a stream twice, so
;;; presumably LCL isn't expecting this.

(defun handle-message-to-xuim (contents)
  (let* ((ident (car contents))
	 (args (cdr contents))
	 (*print-case* :upcase))
    (assert (keywordp ident))
    ;; /\/: *print-length*, etc?
    (prin1 `(:IM ,ident ,args) *control-io*)
    (force-output *control-io*)
    (case ident
      (:DIE-DIE-DIE
       ;; We have to exit too.
       ; (close *control-io*)
       (terminate-pprocess)))))

(defun handle-message-from-xuim (msg)
  (check-type msg list)
  (destructuring-bind (sender ident args) msg
    (assert (eq sender :xuim))
    (assert (eq (pprocess-name *pprocess*) :xuim))
    (case ident
      (:DYING
       (apply #'ipc-send-control ident args)
       ;; We have to exit too
       ; (close *control-io*)
       (terminate-pprocess))
      (:OK
       ;; ignore
       )
      (t
       (apply #'ipc-send-control ident args)))))

;;; End
