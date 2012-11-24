;;; A select for KCL
;;; Copyright (c) 1998, 1999 by Jeff Dalton

(in-package :fork)

;;; The recommended macro...

(eval-when (eval compile)
  (set-macro-character #\% 
    #'(lambda (s c) (declare (ignore c)) (values (read-line s)))))

;;; Our very own error routine.  We try to print the appropriate Unix
;;; error, if one exists.

;;; /\/: The conditionalization of sys_errlist is to deal with NetBSD 0.9a

#|
#+(and bsd sun) ; SunOS?
(Clines
%	extern char *sys_errlist[];
)

(Clines

; %#if defined(_ANSI_SOURCE) && defined(_POSIX_SOURCE)
; %	extern char *sys_errlist[];
; %#endif
%	extern int errno, sys_nerr;
)|#

(Clines

%	extern int errno;

%	static Zerror(default_msg)
%	char *default_msg;
%	{
%	    char *msg;
%	    msg=strerror(errno);
%	    if (msg == NULL)
%	        FEerror(default_msg, 0);
%	    else
%	        FEerror(msg, 0);
%	}

)

;;; /\/: Handles only input streams.

;;; N.B. Because of synonym streams, can't assume a stream always has
;;; the same fd.

(defun real-select (streams &optional timeout) ; -> ready streams
  (cond ((null streams)
	 (error "No streams for select."))
	;; See if there's anything already in the buffers.
	;; /\/: But LISTEN does more.
	((remove-if-not #'listen streams))
	;; If we're supposed to poll, we're done.
	((eql timeout 0)
	 nil)
	;; Call select.
	(t
	 (call-real-select streams timeout))))

;;; The list of "streams" given to call-real-select can contain
;;; fds (ints) as well as proper Lisp streams.

(defun call-real-select (streams timeout)
  (let ((fds (mapcar #'(lambda (s)
			 (if (numberp s)
			     s
			   (stream-fd (deref-stream s :input))))
		     streams)))
    (clear-fds)
    (dolist (fd fds)
      (set-fd fd))
    (let ((n-ready (call-select (or timeout -1))))
      (if (= n-ready 0)
	  '()
	(mapcan #'(lambda (stream fd)
		    (if (/= 0 (fd-set-p fd)) (list stream) nil))
		streams
		fds)))))

(defun real-select-p (streams &optional timeout) ; -> T/F
  (cond ((null streams)
	 (error "No streams for select-p."))
	;; See if there's anything already in the buffers.
	;; /\/: But LISTEN does more.
	((dolist (s streams nil)
	   (when (listen s) (return t))))
	;; If we're supposed to poll, we're done.
	((eql timeout 0)
	 nil)
	;; Call select.
	(t
	 (clear-fds)
	 (dolist (s streams)
	   (set-fd (stream-fd (deref-stream s :input))))
	 (/= 0 (call-select (or timeout -1))))))


(Clines

%#	include <sys/types.h>
%#	include <sys/time.h>
%#	include <unistd.h>

%	static fd_set fds;

%	static void clear_fds ()
%	{
%	    FD_ZERO(&fds);
%	}

%	static void set_fd (fd)
%	{
%	    FD_SET(fd, &fds);
%	}

%	static int fd_set_p (fd)
%	int fd;
%	{
%	    return FD_ISSET(fd, &fds);
%	}

%	static int call_select (timeout)
%	int timeout;			/* seconds */
%	{
%	    int err;
%	    if (timeout >= 0)
%	    {
%	        struct timeval tv;
%	        tv.tv_sec = timeout;
%	        tv.tv_usec = 0;
%	        err = select(FD_SETSIZE, &fds, (fd_set *)0, (fd_set *)0, &tv);
%	    }
%	    else
%		err = select(FD_SETSIZE, &fds, (fd_set *)0, (fd_set *)0,
%		             (struct timeval *)0);
%	    if (err == -1)
%		Zerror("Select failed");
%	    return err;
%	}

)

(defentry clear-fds () (void clear_fds))
(defentry set-fd (int) (void set_fd))
(defentry fd-set-p (int) (int fd_set_p))
(defentry call-select (int) (int call_select))


;;; Now define select-input and select-input-p

(unless (find-package :pprocess)
  (cerror "Make the pprocess package."
	  "There's no pprocess package, so I can't define select-input.")
  (make-package :pprocess))

(setf (symbol-function 'pprocess::select-input)
      #'real-select)

(setf (symbol-function 'pprocess::select-input-p)
      #'real-select-p)

;;; End
