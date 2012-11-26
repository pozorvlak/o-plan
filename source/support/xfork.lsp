;;; Process, fork, exec, etc. for KCL

;;; Jeff Dalton, AIAI, University of Edinburgh

;;; Documentation can be found in the comments near each definition.
;;; Sort of.  Actually, you may have to read the code, the Unix manual, ...
;;; The interface isn't always very C-like...

(in-package "FORK")

(export '(fork exit-child getpid wait))
(export '(waitpid-no-hang wait-reason))
(export '(kill))
(export '(pipe dup dup2 fdopen execlp))
(export '(process process-send process-receive))
(export '(stream-type stream-fd stream-ferror 
	  stream-synonym unpack-stream deref-stream))
(export '(input-stream output-stream io-stream probe-stream
	  synonym-stream broadcast-stream concatenated-stream
	  two-way-stream echo-stream string-input-stream
	  string-output-stream))


;;; The recommended macro...

(eval-when (eval compile)
  (set-macro-character #\% 
    #'(lambda (s c) (declare (ignore c)) (values (read-line s)))))

(defun c-string (x)
  "Turns a Lisp string into a null terminated vector of characters, i.e.,
   a C string.  This is useful when calling procedures written in C."
  (concatenate 'string (string x) (list (code-char 0))))

#+:was-gcl-2
(Clines
%#define Creturn return
)


#+:linux
(Clines
%#  include <sys/types.h>
%#  include <unistd.h>
)

;;; Our very own error routine.  We try to print the appropriate Unix
;;; error, if one exists.

;;; /\/: The conditionalization of sys_errlist is to deal with NetBSD 0.9a
;;; /\/: But it's all commented out for FreeBSD 2.0

#+(and bsd sun) ; SunOS?
(Clines
%	extern char *sys_errlist[];
)

#+:undef
(Clines
; %#if defined(_ANSI_SOURCE) && defined(_POSIX_SOURCE)
; %	extern char *sys_errlist[];
; %#endif
%	extern int errno, sys_nerr;
)

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


;;; (FORK) returns nil to the child and a fixnum pid to the parent.

(Clines

%	static object zfork()
%	{
%	    int pid;
%	    check_arg(0);
%	    pid = fork();
%	    if (pid == -1)
%		Zerror("Fork failed.");
%	    else if (pid == 0)
%		return (Cnil);
%	    else
%		return (make_fixnum(pid));
%	}

)

(defentry fork () (object zfork))


;;; (EXIT-CHILD n) calls _exit(n).  This is supposedly better than
;;; calling exit.

(Clines

%	static void z_exit(n)
%	object n;
%	{
%	    check_arg(1);
%	    _exit(fix(n));
%	}

)

(defentry exit-child (object) (void z_exit))


;;; (GETPID) returns the process id of the current process.

(defentry getpid () (int getpid))



;;; (WAIT) returns (pid . status) when the next child exits.

(Clines

%	static object zwait()
%	{
%	    int status = -1, pid;
%	    object result;
%	    extern int errno;

%	    check_arg(0);
%	    pid = wait(&status);

%	    result = make_cons(Cnil, Cnil);
%	    vs_push(result);
%	    result->c.c_car = make_fixnum(pid);
%	    result->c.c_cdr = make_fixnum(pid != -1 ? status : errno);
%	    return(vs_pop);
%	}

)

(defentry wait () (object zwait))


;;; (WAITPID-NO-HANG pid) returns (pid . status) or (0 . ?) if the
;;; status is not yet available.  Errors of various sorts, including
;;; signals to the calling process, result in (-1 . errno).  To deal
;;; with the signal case, we should check whether errno = EINTR.

(Clines

%#	include <sys/wait.h>

%	static object zwaitpid_no_hang(zpid)
%	object zpid;
%	{
%	    int status = -1, pid;
%	    object result;
%	    extern int errno;
%
%	    check_arg(1);

%	    pid = waitpid(fix(zpid), &status, WNOHANG);

%	    result = make_cons(Cnil, Cnil);
%	    vs_push(result);
%	    result->c.c_car = make_fixnum(pid);
%	    result->c.c_cdr = make_fixnum(pid != -1 ? status : errno);
%	    return(vs_pop);
%	}

)

(defentry waitpid-no-hang (object) (object zwaitpid_no_hang))


;;; (KILL pid sig) returns t if successful, returns nil if the process
;;; did not exist, and otherwise signals an error.  Thus (kill pid 0)
;;; can be used to check whether a process exists.

(Clines

%#	include <signal.h>
%#	include <errno.h>

%	static object zkill(pid, sig)
%	int pid;
%	int sig;
%	{
%	    extern int errno;
%
%	    if (kill((pid_t)pid, sig) == 0)
%		return Ct;
%	    else if (errno == ESRCH)
%		return Cnil;
%	    else
%		Zerror("Kill failed.");
%	}

)

(defentry kill (int int) (object zkill))


;;; (PIPE) returns file descriptor integers (read . write)

(Clines

%	static object zpipe()
%	{
%	    int fildes[2];
%	    object result;

%	    check_arg(0);
%	    if (pipe(fildes) == -1)
%	        Zerror("Pipe failed.");

%	    result = make_cons(Cnil, Cnil);
%	    vs_push(result);
%	    result->c.c_car = make_fixnum(fildes[0]); /* from */
%	    result->c.c_cdr = make_fixnum(fildes[1]); /* to   */
%	    return(vs_pop);
%	}

)

(defentry pipe () (object zpipe))


;;; (dup fildes) -- returns a new file descriptor (integer) that "duplicates"
;;; the given one.  The new fd will be the lowest free fd for the process.

(Clines

%	static object zdup(fildes)
%	object fildes;
%	{
%	    int result;
%	    check_arg(1);
%	    result = dup(fix(fildes));
%	    if (result == -1) Zerror("Dup failed.");
%	    return(make_fixnum(result));
%	}

)

(defentry dup (object) (object zdup))


;;; (DUP2 fildes1 fildes2) -- makes fildes2 be the same file as fildes1

(Clines

%	static object zdup2 (fildes1, fildes2)
%	object fildes1, fildes2;
%	{
%	    int fd1, fd2;

%	    check_arg(2);
%	    fd1 = fix(fildes1);
%	    fd2 = fix(fildes2);

%	    if (dup2(fd1, fd2) == -1)
%	       Zerror("Dup2 failed.");

%	    return(Cnil);
%	}

)

(defentry dup2 (object object) (object zdup2))


;;; Stream object declarations

#+akcl
(Clines "typedef object Sobject;")

#-akcl
(Clines

%	/* from h/object.h because cmpinclude.h LOSES */

%	struct stream {
%		short   t, m;
%	        FILE    *sm_fp;         /*  file pointer  */
%	        object  sm_object0;     /*  some object  */
%	        object  sm_object1;     /*  some object */
%	        int     sm_int0;        /*  some int  */
%	        int     sm_int1;        /*  some int  */
%	        short   sm_mode;        /*  stream mode  */
%                                       /*  of enum smmode  */
%		};

%	typedef union {struct stream sm;} *Sobject;

)

#-:gcl-2
(Clines

%	/* LOSE AGAIN */

%	enum smmode {smm_input, smm_output, smm_io, smm_probe,
%		     smm_synonym, smm_broadcast, smm_concatenated,
%		     smm_two_way, smm_echo, smm_string_input,
%		     smm_string_output};

)


;;; (FDOPEN fildes type) "opens" a Unix file descriptor as a CL stream.
;;; The type must be :input or :output.

#-:gcl-2
(Clines
%	extern object Kinput;
%	extern object Koutput;
%	extern object Sstring_char;
)

#+:gcl-2
(Clines
%#define Kinput sKinput
%#define Koutput sKoutput
%#define Sstring_char sLstring_char
)

#+(and :gcl FreeBSD (not :gcl-2))	;NO_SETBUF -- see FreeBSD.h
(Clines
%#define setup_stream_buffer(x)
)

#-:gcl
(Clines

%	/* Ifdopen does the work and is also called from PROCESS */

%	static object Ifdopen(fd, Ztype)
%	int fd;
%	object Ztype;
%	{
%	    Sobject result;		 /* s.b. object result */
%	    FILE *f;

%	    if (Ztype == Kinput)
%	        f = fdopen(fd, "r");
%	    else if (Ztype == Koutput)
%		f = fdopen(fd, "w");
%           else
%	        FEerror("Fdopen: type must be :INPUT or :OUTPUT.", 0);

%	    if (f == (FILE *)NULL)
%	        Zerror("Fdopen failed.");

%	    result = (Sobject)alloc_object(t_stream);
%	    vs_push((object) result);
%	    result->sm.sm_fp = f;
%	    result->sm.sm_mode 
%               = (short)((Ztype == Kinput) ? smm_input : smm_output);

%	    result->sm.sm_object0 = Sstring_char;	/* elt type */
%	    result->sm.sm_object1 = make_fixnum(fd);	/* ? OBJNULL ? */
%	    result->sm.sm_int0 = 0;
%	    result->sm.sm_int1 = 0;
%	    setbuf(result->sm.sm_fp, alloc_contblock(BUFSIZ));
%	    return(vs_pop);
%	}

)

#+:gcl
(Clines

%	/* Ifdopen does the work and is also called from PROCESS */

%	static object Ifdopen(fd, Ztype)
%	int fd;
%	object Ztype;
%	{
%	    Sobject result;		 /* s.b. object result */
%	    FILE *f;

%	    if (Ztype == Kinput)
%	        f = fdopen(fd, "r");
%	    else if (Ztype == Koutput)
%		f = fdopen(fd, "w");
%           else
%	        FEerror("Fdopen: type must be :INPUT or :OUTPUT.", 0);

%	    if (f == (FILE *)NULL)
%	        Zerror("Fdopen failed.");

%	    result = (Sobject)alloc_object(t_stream);
%	    vs_push((object) result);
%	    result->sm.sm_fp = f;
%	    result->sm.sm_mode 
%               = (short)((Ztype == Kinput) ? smm_input : smm_output);

%	    result->sm.sm_object0 = Sstring_char;	/* elt type */
%	    result->sm.sm_object1 = make_fixnum(fd);	/* ? OBJNULL ? */
%	    result->sm.sm_int0 = 0;
%	    result->sm.sm_int1 = 0;
%	    setup_stream_buffer(result);
%	    return(vs_pop);
%	}

)

(Clines

%	static object zfdopen(Zfildes, Ztype)
%	object Zfildes, Ztype;
%	{
%	    check_arg(2);
%	    return(Ifdopen(fix(Zfildes), Ztype));
%	}

)

(defentry fdopen (object object) (object zfdopen))


;;; String operations

;;; Here is some code that could be used to convert Lisp strings to
;;; C strings.  It's not used because we can do the conversion in
;;; Lisp by appending a null character to the Lisp string.

#|
(Clines

%	static void Get_C_String(Lisp_string, C_string, max)
%	object Lisp_string;
%	char * C_string;
%	int max;
%	{
%	    int i;
%	    Lisp_string = coerce_to_string(Lisp_string);

%	    if (Lisp_string->st.st_fillp >= max)
%	    {
%		vs_push(Lisp_string);
%		FEerror("String too long: ~S", 1, Lisp_string);
%	    }

%	    for (i = 0; i < Lisp_string->st.st_fillp; i++)
%           {
%		C_string[i] = Lisp_string->st.st_self[i];
%           }
%	    C_string[i] = '\0';
%	}

)
|#


;;; (EXECLP filename &REST argv-strings) is like the Unix execlp, although
;;; it does an execvp internally.  The final 0 argument will be supplied
;;; automatically, as will the first argument, which is derived from the
;;; filename.  Failure signals an error, success (of course) doesn't return.

;;; The extra layer of interface is needed to deal with the variable
;;; number of arguments.  We also use it to convert the arguments to
;;; C-style strings, etc.

;(proclaim '(notinline c-string internal-execvp))

(defun execlp (filename &rest argv-strings)
  (let ((pn (pathname filename)))
    (internal-execvp
      (c-string (namestring pn))
      (coerce (cons (c-string (file-namestring pn))
		    (mapcar #'c-string argv-strings))
	      'simple-vector))))

;;; The C part expects Zargs to be a vector of C-strings and Zname to be
;;; a C-string.  NO CHECKING is done.  KCL supplies the malloc and free.

(Clines

%	extern char *malloc();
%	extern void free();

%	static object zexecvp (Zname, Zargv)
%	object Zname, Zargv;
%	{
%	    char **argv;
%	    int nargs, i;

%	    nargs = Zargv->v.v_fillp;
%	    argv = (char **)malloc((nargs + 1) * sizeof(char *));

;%	    printf("nargs == %d\n", nargs);
%	    for(i = 0; i < nargs; i++)
%	    {
;%	        printf("arg[%d] == %s\n", i, Zargv->v.v_self[i]->st.st_self);
%		argv[i] = Zargv->v.v_self[i]->st.st_self;
%	    }
%	    argv[nargs] = (char *)0;
;%	    fflush(stdout);

%	    execvp(Zname->st.st_self, argv);
%	    free(argv);
%	    Zerror("Exec failed.");
%	}

)

(defentry internal-execvp (object object) (object zexecvp))


;;; Random STREAM functions

;;; Since some of our functions operate in terms of Unix file descriptors
;;; (i.e., integers), we need some way to extract the fd from a CL stream.
;;; It would be nice if we could just say ok, we'll work only on simple
;;; input or output streams; no two-way streams, synonyms, etc. could be
;;; dereferenced.  Unfortunately, e.g., *standard-input* is a synonym
;;; for the two-way stream *terminal-io* (which, I think, violates the
;;; CL spec, but what can we do?), and so we have to be able to take
;;; complex streams apart for even this simple case.

;;; So, given that we need to handle complex streams, we then have to decide
;;; what functionality to provide at the Lisp level.  We could put everything
;;; into a single function that extracted an fd, but it seemed better to
;;; provide somthing more generally useful:

;;; STREAM-FD      returns the fd of a simple input or output stream.
;;; STREAM-FERROR  looks at ferror of the FILE * of a simple input or
;;;                output stream, returning t if there was an error, else nil.
;;; STREAM-TYPE    returns a symbol representing the stream type ("mode")
;;; STREAM-SYNONYM returns the name of the special variable that
;;;                points to the synonym of a synonym stream.
;;; UNPACK-STREAM  returns a list of the streams inside a two-way, echo,
;;;                or broadcast stream.
;;; DEREF-STREAM   returns a simple input- output- or io-stream when
;;;                given a stream and a direction :INPUT or :OUTPUT.


;;; STREAM-FD stream

(Clines

%	extern void check_type_stream();

%	static int stream_fd(Zstream)
%	object Zstream;
%	{
%	    Sobject str;

%	    check_type_stream(&Zstream);	/* make sure it is one */
%	    str = (Sobject)Zstream;		/* s.b. unnecessary */

%	    switch (str->sm.sm_mode)
%	    {
%	    case smm_input:
%	    case smm_output:
%	    case smm_io:
%		return((int)fileno(str->sm.sm_fp));
%	    default:
%		FEerror("~S is not a simple input, output or io stream.",
%		        1, Zstream);
%	    }
%	}

)

(defentry stream-fd (object) (int stream_fd))


;;; STREAM-FERROR stream

(Clines

%	static object stream_ferror(Zstream)
%	object Zstream;
%	{
%	    Sobject str;

%	    check_type_stream(&Zstream);	/* make sure it is one */
%	    str = (Sobject)Zstream;		/* s.b. unnecessary */

%	    switch (str->sm.sm_mode)
%	    {
%	    case smm_input:
%	    case smm_output:
%	    case smm_io:
%		return((ferror(str->sm.sm_fp) == 0) ? Cnil : Ct);
%	    default:
%		FEerror("~S is not a simple input, output or io stream.",
%		        1, Zstream);
%	    }
%	}

)

(defentry stream-ferror (object) (object stream_ferror))


;;; STREAM-TYPE stream

(defCfun "static object stream_type(object Zstream)" 0
%	Sobject str;				/* sigh */
%	object result;

%	check_type_stream(&Zstream);
%	str = (Sobject)Zstream;

%	switch(str->sm.sm_mode)
%	{
%	case smm_input:
	    ('input-stream "result")
%	    break;
%	case smm_output:
	    ('output-stream "result")
%	    break;
%	case smm_io:
	    ('io-stream "result")
%	    break;
%	case smm_probe:
	    ('probe-stream "result")
%	    break;
%	case smm_synonym:
	    ('synonym-stream "result")
%	    break;
%	case smm_broadcast:
	    ('broadcast-stream "result")
%	    break;
%	case smm_concatenated:
	    ('concatenated-stream "result")
%	    break;
%	case smm_two_way:
	    ('two-way-stream "result")
%	    break;
%	case smm_echo:
	    ('echo-stream "result")
%	    break;
%	case smm_string_input:
	    ('string-input-stream "result")
%	    break;
%	case smm_string_output:
	    ('string-output-stream "result")
%	    break;
%	default:
%	    FEerror("Internal error: ~S claimed to be a stream.", 1, Zstream);
%	}

%	/* conditional to avoid unreached code warning after Creturn */
%	if (result != Cnil) Creturn(result);
)

(defentry stream-type (object) (object stream_type))

;;; STREAM-SYNONYM synonym-stream

(defCfun "static object stream_synonym(object Zstream)" 0
%   Sobject str;
%   object result;

%   check_type_stream(&Zstream);
%   str = (Sobject)Zstream;

%   if ((enum smmode)str->sm.sm_mode == smm_synonym)
%   {
%	result = str->sm.sm_object0;
%	if (type_of(result) == t_symbol)
%	    Creturn(result);
%	FEerror("~S doesn't contain a variable.", 1, Zstream);
%   }
%   else
%   {
%	FEerror("~S is not a synonym stream", 1, Zstream);
%   }
%
)

(defentry stream-synonym (object) (object stream_synonym))


;;; UNPACK-STREAM stream

(Clines

%   static object unpack_stream(Zstream)
%   object Zstream;
%   {
%	Sobject str;
%	object result;

%	check_type_stream(&Zstream);
%	str = (Sobject)Zstream;
%	switch(str->sm.sm_mode)
%	{
%	case smm_broadcast:
%	    return(str->sm.sm_object0);		/* list of streams */
%	case smm_two_way:
%	case smm_echo:
%	    vs_push(result = make_cons(str->sm.sm_object0, Cnil));
%	    result->c.c_cdr = make_cons(str->sm.sm_object1, Cnil);
%	    return(vs_pop);
%	default:
%	    return(Cnil);
%	}
%   }

)

(defentry unpack-stream (object) (object unpack_stream))

;;; DEREF-STREAM stream direction
;;;
;;; The idea is that a stream can be turned into an fd that can be used with
;;; the Unix-level calls by (STREAM-FD (DEREF-STREAM stream direction)).
;;; An error will be signalled for streams that cannot be meaningfully
;;; dereferenced to a Unix fd.

;;; /\/ Could add ECHO-STREAMs.

(defun deref-stream (arg-stream direction)
  (labels
      ((deref (s)
	 (ecase direction
	   (:INPUT
	    (case (stream-type s)
	      (INPUT-STREAM s)
	      (IO-STREAM s)
	      (SYNONYM-STREAM (deref (symbol-value (stream-synonym s))))
	      (TWO-WAY-STREAM (deref (car (unpack-stream s))))
	      (t (error "~S can't be dereferenced to an input stream."
			arg-stream))))
	   (:OUTPUT
	    (case (stream-type s)
	      (OUTPUT-STREAM s)
	      (IO-STREAM s)
	      (SYNONYM-STREAM (deref (symbol-value (stream-synonym s))))
	      (TWO-WAY-STREAM (deref (cadr (unpack-stream s))))
	      (t (error "S~ can't be dereferenced to an output stream."
			arg-stream)))))))
    (deref arg-stream)))

;;; (PROCESS filename &key args input output error) returns a list:
;;;      child pid,
;;;      input stream for reading child or nil.
;;;      output stream for writing child or nil.
;;;      input stream for reading child's error output or nil.
;;;
;;; This is a packaged vfork-exec that tries to handle most of the things
;;; that people will want to do.  In the other cases, they can cons their
;;; own from FORK, PIPE, EXECLP, etc.  But then they have to pay the cost
;;; of fork instead of vfork.
;;;
;;; The returned list contains streams only if they were created by
;;; PROCESS.  The arguments are interpreted as follows:
;;;    NAME   -- name of the file to exec
;;;    ARGS   -- A list of argv strings (see also break-args)
;;;    INPUT  -- An input stream or :STREAM to create one
;;;    OUTPUT -- An output stream or :STREAM to create one
;;;    ERROR  -- An output stream or :STREAM to create one
;;; The input and output streams can also be specified by giving a Unix fd
;;; (a fixnum), and T can be used instead of :STREAM.  The default values
;;; for input, output, and error are *standard-input*, *standard-output*,
;;; and *error-output* respectively.
;;;
;;; The correspondence between the arguments and results may be confusing
;;; because the arguments are described relative to the child and the results
;;; relative to the parent.  So, specifying :INPUT :STREAM creates an input
;;; stream for the child and returns the other end as an output stream in
;;; the parent.

(defun process (filename
		&key args
		     (input *standard-input*)
		     (output *standard-output*)
		     (error *error-output*))
  (flet ((check-stream-arg (arg direction)
	   (cond ((typep arg 'fixnum) arg)
		 ((eq arg :STREAM) nil)
		 ((eq arg T) nil)
		 ((streamp arg) (stream-fd (deref-stream arg direction)))
		 (t (error "Illegal ~A stream for PROCESS: ~S"
			   direction arg)))))
    (let ((pn (pathname filename)))
      (internal-process
        (c-string (namestring pn))
	(coerce (cons (c-string (file-namestring pn))
		      (mapcar #'c-string args))
		'simple-vector)
	(check-stream-arg input :INPUT)
	(check-stream-arg output :OUTPUT)
	(check-stream-arg error :OUTPUT)))))


;;; The C code expects:
;;;    Zname :: a C string
;;;    Zargv :: a simple vector of C strings
;;;    Zin   :: an input stream fd (int) or nil to create one
;;;    Zout  :: an output stream fd (int) or nil to create one
;;;    Zerr  :: an output stream fd (int) or nil to create one
;;;
;;; Note that we must be very careful to close any files we've opened
;;; if an error occurs.  This means that we must also be careful about
;;; calling things that may signal an error.

#-:gcl-2
(Clines
%#  ifdef sparc
%#  include <vfork.h>           /* needed on SPARC */
%#  endif
)

(Clines

%#  include <sys/param.h>	/* for NOFILE */
%#  include <fcntl.h>

%   static object zprocess(Zname, Zargv, Zin, Zout, Zerr)
%   object Zname, Zargv, Zin, Zout, Zerr;
%   {
%	char *name, **argv;
%	int nargs;
%	int child_pid;
%	int read_child, write_child, read_child_error;	/* in parent */
%	int child_input, child_output, child_error;	/* in child */
%	int read_exec_error, write_exec_error;		/* parent <- child */
%	int fd[2];
%	int i;
%	object result;

%	/* Convert name and argv */

%	name = Zname->st.st_self;
%	nargs = Zargv->v.v_fillp;
%	argv = (char **)malloc((nargs + 1) * sizeof(char *));
%	for(i = 0; i < nargs; i++)
%	    argv[i] = Zargv->v.v_self[i]->st.st_self;
%	argv[nargs] = (char *)0;

%	/* Set up child input.  If we make a pipe, we will
%	 * later return a new stream in the parent.
%	 */
%	if (Zin == Cnil)
%	{
%	    if (pipe(fd) == -1) Zerror("Pipe failed.");
%	    child_input = fd[0];
%	    write_child = fd[1];
%	}
%	else child_input = fix(Zin);

%	/* Set up child output.  If we make a pipe, we will
%	 * later return a new stream in the parent.
%	 */
%	if (Zout == Cnil)
%	{
%	    if (pipe(fd) == -1) Zerror("Pipe failed.");
%	    read_child = fd[0];
%	    child_output = fd[1];
%	}
%	else child_output = fix(Zout);

%	/* Set up child error output.  If we make a pipe, we will
%	 * later return a new stream in the parent.
%	 */
%	if (Zerr == Cnil)
%	{
%	    if(pipe(fd) == -1) Zerror("Pipe failed.");
%	    read_child_error = fd[0];
%	    child_error = fd[1];
%	}
%	else child_error = fix(Zerr);

%	/* Finally, we open a pipe that will be closed on exec -- if the
%	 * exec succeeds.  If the exec fails, the child will write an error
%	 * message which the parent can read.  This trick from R. Tobin.
%	 */
%	if (pipe(fd) == -1) Zerror("Exec error pipe failed.");
%	read_exec_error = fd[0];
%	write_exec_error = fd[1];
%	fcntl(write_exec_error, F_SETFD, 1);	/* set close on exec bit */

%#ifdef debug
%	printf("child_input = %d, child_output = %d\n",
%	       child_input, child_output);
%	if (Zin == Cnil)
%	   printf("write_child = %d\n", write_child);
%	if (Zout == Cnil)
%	   printf("read_child = %d\n", read_child);
%	if (Zerr == Cnil)
%	   printf("read_child_error = %d\n", read_child_error);
%	if (child_error != 2)
%	   printf("child_error = %d\n", child_error);
%	printf("read_exec_error = %d, write = %d\n",
%	       read_exec_error, write_exec_error);
%	fflush(stdout);
%#endif

%	/* Fork */
%	child_pid = vfork();
%	if (child_pid == 0)
%	{
%	    /* Child.  Remember that we are somewhat limited in what
%	     * we can do since we don't want to mess up the parent's
%	     * address space.  No mallocs, etc.
%	     */

%	    /* Put files in place if needed */
%	    if (child_input  != 0)
%           {
%		dup2(child_input, 0);
%		close(child_input);
%	    }
%	    if (child_output != 1)
%	    {
%		dup2(child_output, 1);
%		close(child_output);
%	    }
%	    if (child_error  != 2)
%	    {
%		dup2(child_error, 2);
%		if (child_error != 1)		/* KCL makes it 1 */
%		    close(child_error);
%	    }

%#ifdef undef
%	    /* Close random files.  Note that this closes the parent
%	     * ends of any pipes we might have made. 
%	     */
%	    for (i = 4; i < NOFILE; i++)
%		if (i != write_exec_error) close(i);
%#endif

%	    /* Close parent ends if we made any */
%	    /* N.B. This replaces the too-general close above. */
%	    if (Zin == Cnil) close(write_child);
%	    if (Zout == Cnil) close(read_child);
%	    if (Zerr == Cnil) close(read_child_error);

%	    /* Do the exec */
%	    execvp(name, argv);

%	    /* If we get here, the exec failed and we send a message */
%	    dup2(write_exec_error, 2);
%	    perror("Child exec");
%	    _exit(-1);

%	}

%	/* Parent */

%	/* release argv strings -- no exec here, so we don't need them */
%	free(argv);

%	/* Close the child ends if we made any */
%	if (Zin == Cnil) close(child_input);
%	if (Zout == Cnil) close(child_output);
%	if (Zerr == Cnil) close(child_error);

%	/* See if the exec failed.  If we close write_exec_error and it
%	 * was also closed on exec, we will be unable to read from it.
%	 */
%	close(write_exec_error);
%	{
%	    char msg[257];
%	    int nchars;
%	    nchars = read(read_exec_error, msg, 256);
%	    close(read_exec_error);
%	    if (nchars != 0)
%	    {
%		if (nchars < 0) Zerror("Trouble reading exec error.");
%		msg[nchars] = '\0';
%		FEerror(msg, 0);
%	    }
%	}

%	/* The final result is a list of 4 elements.  The first is
%	 * the child pid, followed, if we made the corresponding pipe,
%	 * by streams for reading and writing the child, respectively,
%	 * and a stream for reading the child's error output.  When we
%	 * did not make the pipe, the corresponding list entry is nil. 
%	 */
%	result = make_cons(Cnil, Cnil);
%	vs_push(result);

%	result->c.c_car = make_fixnum(child_pid);

%	/* make stream to read child output */
%	result = result->c.c_cdr = make_cons(Cnil, Cnil);
%	if (Zout == Cnil) 
%	   result->c.c_car = Ifdopen(read_child, Kinput);

%	/* make stream to write child input */
%	result = result->c.c_cdr = make_cons(Cnil, Cnil);
%	if (Zin == Cnil)
%	   result->c.c_car = Ifdopen(write_child, Koutput);

%	/* make stream to read child's error output */
%	result = result->c.c_cdr = make_cons(Cnil, Cnil);
%	if (Zerr == Cnil)
%	   result->c.c_car = Ifdopen(read_child_error, Koutput);

%	return(vs_pop);

%   }

)

;;; Note that the DEFENTRY interface requires that the C function
;;; return a single value.  We return a list so that could then be
;;; given to VALUES-LIST.

(defentry internal-process (object object object object object)
  (object zprocess))


;;; PROCESS-SEND -- fork a process and return a stream for sending it
;;; input.

(defun process-send (command &rest argv-strings)
  (third (process command :args argv-strings :input :stream)))

;;; PROCESS-RECEIVE -- fork a process and return a stream for reading
;;; its output.

(defun process-receive (command &rest argv-strings)
  (second (process command :args argv-strings :output :stream)))

;;; End
