;;;; File: kcl-sockets-non-ansi.lsp
;;; Contains: KCL socket code for losing C compilers
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: Mon 8 Sep 1997 from kcl-sockets.lsp
;;; Updated: Tue Jul 14 05:11:29 1998 by Jeff Dalton
;;; Copyright: (c) 1994, 1997, AIAI, University of Edinburgh

(in-package :oplan-util)


(export '(create-socket			; -> soc_fd
	  bind-socket			; soc_fd, int_port  -> T
	  get-socket-port		; soc_fd -> int_port
	  bind-socket-to-a-free-port	; soc_fd -> int_port
	  listen-socket			; soc_fd, int_count -> T
	  accept-socket-connection	; soc_fd -> soc_fd
	  socket-connection-stream	; soc_fd -> stream
	  get-socket-peer-address	; soc_fd -> int_addr
	  get-socket-peer-name		; soc_fd -> string
	  get-host-by-address		; soc_fd -> string
	  fdclose			; fd -> int_status
	  ignore-sigpipe
	  note-sigpipes
	  sigpipe-p
	  clear-sigpipe-note
	  ))
	  

;;; The recommended macro...

(eval-when (eval compile)
  (set-macro-character #\% 
    #'(lambda (s c) (declare (ignore c)) (values (read-line s)))))



;;; Our very own error routine, based on the one in xfork.lsp.
;;; We try to print the appropriate Unix error, if one exists.

#+(and bsd sun) ; SunOS?
(Clines
%	extern char *sys_errlist[];
)

(Clines

%   extern int errno, sys_nerr;

%   unix_error(default_msg)
%   char *default_msg;
%   {
%	if (errno >= sys_nerr)
%	    FEerror(default_msg, 0);
%	else
%	    FEerror(sys_errlist[errno], 0);
%   }

)


;;; Socket includes

(Clines

%#  include <sys/types.h>
%#  include <sys/socket.h>
%#  include <netinet/in.h>
%#  include <netdb.h>			/* for gethostbyaddr */

)


;;; create-socket

;;; Only INET SOCK_STREAM sockets for now.  /\/

(Clines

%   int create_socket()
%   {
%	int s;
%	s = socket(AF_INET, SOCK_STREAM, 0);
%	if (s < 0)
%	    unix_error("Create_socket failed.");
%	else
%	    return(s);
%   }

)

(defentry create-socket () (int create_socket))


;;; bind-socket

;;; Only INET sockets for now.  /\/

;;; To bind and to a system-assigned port and get back the port number,
;;; call bind-socket-to-a-free-port.

(Clines

%   object bind_socket(s, addr)
%   int s, addr;
%   {
%	struct sockaddr_in a;
%	bzero(&a, sizeof(a));
%	a.sin_family = AF_INET;
%	a.sin_port = htons(addr);
%	if (bind(s, &a, sizeof(a)) != 0)
%	   unix_error("Bind-socket failed.");
%	else
%	   return(Ct);
%   }

)

(defentry bind-socket (int int) (object bind_socket))


;;; get-socket-port

;;; A port will be assigned for us if we bind a socket to zero.
;;; get-socket-port can then be used to see which port was assigned.

(Clines

%   int get_socket_port(s)
%   int s;
%   {
%	struct sockaddr_in a;
%	int len = sizeof(a);
%	if (getsockname(s, (struct sockaddr *)&a, &len) != 0)
%	    unix_error("Get-socket-port failed.");
%	else
%	    return ntohs(a.sin_port);
%   }

)

(defentry get-socket-port (int) (int get_socket_port))


;;; bind-socket-to-a-free-port

;;; Binds and returns the port number assigned by the system.

(defun bind-socket-to-a-free-port (s)
  (bind-socket s 0)
  (get-socket-port s))


;;; listen-socket

(Clines

%   object listen_socket(s, backlog)
%   int s, backlog;
%   {
%	check_arg(2);
%	if (listen(s, backlog) != 0)
%	    unix_error("Listen_socket failed.");
%	else
%	    return(Ct);
%   }

)

(defentry listen-socket (int int) (object listen_socket))


;;; accept-socket-connection

;;; It will usually make sense to call socket-connection-stream instead.

(Clines

%   int accept_socket_connection(s)
%   int s;
%   {
%	int fd;
%	fd = accept(s, 0, 0);
%	if (fd < 0)
%	    unix_error("Accept_socket_connection failed.");
%	else
%	    return(fd);
%   }

)

(defentry accept-socket-connection (int) (int accept_socket_connection))


;;; Socket-connection-stream

(defun socket-connection-stream (s)
  (let ((fd (accept-socket-connection s)))
    (make-two-way-stream
      (fork:fdopen fd :input)
      (fork:fdopen fd :output))))


;;; get-socket-peer-address

;;; /\/: Assumes the conversion from an "int" specified in a defentry
;;; to a Lisp number is able to handle 32-bit ints.  (We're assuming
;;; the size of the s_addr field -- usually a u_long -- is 32 bits.)

;;; /\/: We return an int, because we want the value only for
;;; comparison: to check that connections keep coming from the
;;; same place.

;;; /\/: Not clear whether ntohs really applies.

(Clines

%   int get_socket_peer_address(s)
%   int s;
%   {
%	struct sockaddr_in a;
%	int len = sizeof(a);
%	if (getpeername(s, (struct sockaddr *)&a, &len) != 0)
%	    unix_error("Get-socket-peer-address failed.");
%	else
%	    return ntohl(a.sin_addr.s_addr);
%   }

)

(defentry get-socket-peer-address (int) (int get_socket_peer_address))


;;; get-socket-peer-name

(Clines

%   char *get_socket_peer_name(s)
%   int s;
%   {
%	struct sockaddr_in a;
%	int len = sizeof(a);
%	
%	if (getpeername(s, (struct sockaddr *)&a, &len) != 0)
%	    unix_error("getpeername failed.");
%	else
%	{
%	    struct hostent *h;
%	    h = gethostbyaddr((char *)&a.sin_addr,
%			      sizeof(a.sin_addr),
%                             AF_INET);
%	    if (h)
%	        return h->h_name;
%	    else
%		/* FEerror(hstrerror(h_errno), 0); */
%		unix_error("gethostbyaddr failed");
%	}
%   }

)

(defentry get-socket-peer-name (int) (string get_socket_peer_name))


;;; get-host-by-address

;;; Mostly for testing that numbers make sense, and the code like
;;; this works in error cases.

;;; Can be called on values returned by get-socket-peer-address,
;;; hence the call to htonl.

(Clines

%   int get_host_by_address(addr)
%   int addr;
%   {
%	struct in_addr a;
%       struct hostent *h;
%	a.s_addr = htonl((u_long)addr);
%	h = gethostbyaddr((char *)&a, sizeof(a), AF_INET);
%	if (h)
%	    return h->h_name;
%	else
%	    /* FEerror(hstrerror(h_errno), 0); */
%	    unix_error("gethostbyaddr failed");
%   }

)

(defentry get-host-by-address (int) (string get_host_by_address))



;;; fdclose -- close by numbers

(defentry fdclose (int) (int close))


;;; ignore-sigpipe

(Clines

%#  include <signal.h>

%   extern void gcl_signal();

%   ignore_sigpipe()
%   {
%	gcl_signal(SIGPIPE, SIG_IGN);
%   }

)

(defentry ignore-sigpipe () (void ignore_sigpipe))


;;; sigpipe noting

(Clines

%   object received_sigpipe_p = Cnil;

%   void sigpipe_handler()
%   {
%	received_sigpipe_p = Ct;
%	gcl_signal(SIGPIPE, sigpipe_handler);  /* needed? /\/ */
%   }

%   void note_sigpipes()
%   {
%	gcl_signal(SIGPIPE, sigpipe_handler);
%   }

%   object sigpipe_p()
%   {
%	return received_sigpipe_p;
%   }

%   void clear_sigpipe_note()
%   {
%	received_sigpipe_p = Cnil;
%   }

)

(defentry note-sigpipes () (void note_sigpipes))
(defentry sigpipe-p () (object sigpipe_p))
(defentry clear-sigpipe-note () (void clear_sigpipe_note))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
