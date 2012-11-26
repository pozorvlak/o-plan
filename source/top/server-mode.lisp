;;;; File: server-mode.lsp
;;; Contains: Server mode code
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: 16 May 1997
;;; Updated: Sat Feb 12 02:39:56 2000 by Jeff Dalton
;;; Copyright: (c) 1997, 2000, AIAI, University of Edinburgh

(in-package :oplan)

;;; In server mode, O-Plan creates an internet domain socket and waits
;;; for connections.  Each connection then gets an O-Plan running in
;;; connect mode.

;;; Set-up-server-mode is called when a "-server" command line argument
;;; is processed.  It makes the :server parameter true, and that causes
;;; run-as-server to be called after top-level initialization is complete.

;;; The :windows parameter is set to nil.

;;; That makes "-server" provide O-Plans that are basically as if
;;; run using "-no -windows -connect".

(defun set-up-server-mode ()
  (set-parameter :windows nil)
  (set-parameter :server t))


;;; Run-as-server makes O-Plan act as a server.

(defparameter *default-server-port* 5040)

(defvar *server-port* nil)

(defvar *server-socket* nil)		;for debugging

(defun run-as-server ()
  (kill-unsafe-readtable)		;it may pay to be a bit paranoid
  ;; The usual sequence of socket operarions.
  (let ((s (create-socket)))
    (setq *server-socket* s)
    (bind-socket s (server-port))
    (listen-socket s 1)
    ;; Loop accepting connections and forking O-Plans.
    (let ((child-pids '()))
      (loop
        ;; Wait for a connection but not too long.
        (when (or (null child-pids)
		  (fork::call-real-select (list s) 10)) ;wait max 10 seconds
	  ;; Either a connection's ready or, because there are no
	  ;; child-pids, we can wait for one.
	  (push (accept-server-mode-connection s)
		child-pids))
	;; See if any children have exited
	(setq child-pids
	      (loop for pid in child-pids
		    for (p v) = (fork:waitpid-no-hang pid)
		    unless (eql p pid)
		    collect pid))))))

(defun accept-server-mode-connection (s) ; -> pid
  (let ((io (socket-connection-stream s)))
    ;; Fork
    (let ((pid #+kcl (fork:fork) #-kcl (error "no fork function")))
      (cond ((null pid)
	     ;; Child
	     ;; Act like connect mode
	     (set-up-connect-mode)
	     (setq *agent-io* io)
	     (run)
	     (exit-oplan))
	    (t
	     ;; Parent
	     (mapc #'close (fork:unpack-stream io))
	     (close io)
	     pid)))))

(defun server-port ()
  (ensuref *server-port*
    (let ((p (get-parameter :port)))
      (cond ((stringp p) (string->int p))
	    ((numberp p) p)
	    ((parameter-set-p :port)
	     (error "Invalid port: ~S."))
	    (*default-server-port*)
	    (t
	     (error "No port specified; use \"-port n\"."))))))

(defun kill-unsafe-readtable ()
  (let* ((safe-readtable oplan-util::*safe-readtable*)
	 (wrapper
	  #'(lambda (previous)
	      #'(lambda (&optional from to)
		  (funcall previous
			   (or from safe-readtable)
			   to)))))
    (let ((*error-output* (make-null-output-stream)))
      (advice-replace 'copy-readtable 'kill-unsafe-readtable
		      wrapper))
    (setq *readtable* safe-readtable)))


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
