;;;; File: kp_toplevel.lsp
;;; Contains: Top-level processing for the KP.
;;; Author: Richard Kirby (rbk) and Jeff Dalton
;;; Created: Tue Apr 17 15:28:10 1990
;;; Updated: Sun May 30 22:47:07 1999 by Jeff Dalton
;;; Copyright: (c) 1992, AIAI, University of Edinburgh

(in-package :oplan-knowledge-platform)

(use-package :oplan-nodes)
(use-package :oplan-tgm)
(use-package :oplan-qa)

(defvar *kp-agenda-entry-being-processed* nil) 	;in case it's useful

(defun kp-startup ()
  ;; This is the initial function for a KP process.  After startup,
  ;; it is not called again.  Instead, kp-event-handler is called
  ;; whenever a message arrives.

  (is-init)

  ;; Make breaks, etc. go to the debug window.
  (x-redirect-errors :kpout)
  
  (clear-screen *debug-io*)

  ;; N.B. A KP has its own value for the current context.
  ;; Set context to the top, and then push to a new one.
  (setq *context* *global-context*)
  (push-context)

  ;; Register with IM.
  (ipc-register-with-im)

  (ipc-send :AM :KPREADY (ipc-whoami))

  (ipc-set-event-handler 'kp-event-handler))


(defun kp-event-handler (self)
  (ipc-handle-message self (next-event self)))

(defmessage (:kp :init) ()
  (kp-init)
  (ipc-send :AM :KPREADY (ipc-whoami))
  :ok)

(defun kp-init ()
  ;; Called by KS-INIT for whatever KP runs KS-INIT.  Else called
  ;; by the :INIT message handler.
  (clear-screen *debug-io*)
  (call-initializers :KP)
  (initialize-variable-group :KP)
  :ok)

(defmessage (:kp :agenda) (entry)
  (unwind-protect (catch :ks-exit (kp-process-agenda entry))
    ;; Make sure we send a :KPREADY message if we throw back to
    ;; the scheduler after an error.
    (ipc-send :AM :KPREADY (ipc-whoami))))

(defun kp-process-agenda (entry)
  ;; Looks at the entry to determine the KS required.

  (setq entry (ipc-with-own-copy (ae entry (copy-agenda-entry ae))))
  
  (let ((ks (ensure-ks (car (ag-body entry)))))

    (dev-level-case
      (:trace 
       (dev-debug "ae-~W/~W,~A: ~W" 
		  (ag-id entry) (ag-stage entry) (ag-level entry)
		  (ag-body entry)))
      #+:undef
      (:minimal
       (let ((*print-length* 4)
	     (*print-level* 3))
	 (dev-debug "ae-~W/~W: ~W" 
		    (ag-id entry) (ag-stage entry) (ag-body entry)))))


    (whats-going-on "~%**************************************************~%~
                     Processing:~%~ae-S - ~S"
		    (ag-id entry) (ag-body entry))

    (let ((*kp-agenda-entry-being-processed* entry))

      (funcall ks entry))))


;;;; KS name -> function name

(defun ensure-ks (ks-name)
  (check-type ks-name keyword)
  (let ((fn-name 
	 (intern (concatenate 'string "KS-" (symbol-name ks-name))
		 (find-package :oplan-knowledge-source))))
    (if (fboundp fn-name)
	fn-name
      (invalid-command "There is no ~S knowledge source." ks-name))))


;;;; Modes

;;; PSV binding mode

;;; Either :auto or :ask - if :auto then KS-BIND automatically picks a
;;; variable binding, otherwise the user is presented with the options
;;; and asked to select a binding.")

(definit :kp *psv-binding-mode*
  (if (get-parameter :test-ta-questions) :ask :auto))

;;; Schema selection mode

(definit :kp *schema-selection-mode*
  (if (get-parameter :test-ta-questions) :ask :auto))

;;; Poison-handler mode

(definit :kp *poison-handler-mode* :auto)

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;

