;;;; File: cgi-util.lsp
;;; Contains: Support for Web / CGI demos
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: October 1994
;;; Updated: Thu May 27 01:38:11 1999 by Jeff Dalton
;;; Copyright: (c) 1994 - 1997, AIAI, University of Edinburgh

(in-package :oplan)

;;; Contents:
;;;  * TF output support
;;;  * Demo utilities
;;;  * Lock files
;;;  * String and other (low-level) utilities


;;;; TF output

(defvar *tf-out* nil)

(defmacro with-tf-output-file (name &body forms)
  `(with-open-file (*tf-out* ,name :direction :output :if-exists :error)
     ,@forms))

(defun tf-format (format-string &rest format-args)
  (apply #'format *tf-out* format-string format-args))

(defun tf-line (format-string &rest format-args)
  (format *tf-out* "~&~?~%" format-string format-args))


;;; Outputting comma-separtated items, usually TF

(defun output-comma-separated (prefix strings &optional final-suffix)
  (when strings
    (output // prefix (car strings))
    (cond ((null (cdr strings))
	   (when final-suffix
	     (output final-suffix)))
	  (t
	   (output ",")
	   (output-comma-separated prefix (cdr strings) final-suffix)))))


;;;; Demo utilities

;;; An assortment of potentially useful items.

;;; web-note-ta-messages uses the pprocess single-stepper to write a
;;; trace to the web-notes file.  /\/: It's actually messages to and
;;; from the :user process that are traced, because the pprocess
;;; created for subr mode is called :user instead of :ta.

(defun web-note-ta-messages ()
  (declare (special *web-notes*))
  (setq *step-io* *web-notes*
	*step-print-length* nil
	*step-print-level* 4)
  (step-pprocesses :trace)
  (watch-pprocess :trace :user))	;It's not :TA in subr mode /\/

;;; KP-1 trace to a file.  Assumes the output level starts at :nothing
;;; (by not recording what the value actually is) and that the output
;;; level wanted for the trace is :trace.

(defun start-kp-trace ()
  ;; Redirects output to a trace file and arranges for the file to
  ;; be closed when O-Plan (running in subr mode) exits.
  (redirect-component-output :kp (web-tmp-filename "kp-trace" "txt"))
  (set-component-debug-level :kp :trace))

(defun stop-kp-trace ()
  ;; Use after start-kp-trace.  Turns off output but leaves the
  ;; trace stream open.
  (finish-output (pprocess-symbol-value (find-pprocess :kp) '*debug-io*))
  (set-component-debug-level :kp :nothing))

(defmacro with-kp-trace (&body body)
  ;; N.B. *not* equiv to start-kp-trace + stop-kp-trace, because it
  ;; handles closing the stream itself rather than making you post
  ;; an exit-action to do it.
  (let ((stream (gensym "STREAM")))
    `(with-open-file (,stream (web-tmp-filename "kp-trace" "txt")
			      :direction :output
		              :if-exists :error)
       (unwind-protect
	   (progn (set-component-output-stream :kp ,stream)
		  (set-component-debug-level :kp :trace)
		  ,@body)
	 (set-component-debug-level :kp :nothing)))))

;;; /\/: Defining condition-types with defstruct-style slot-specs.
;;; /\/: Should be in util-macros.lsp, or someplace similarly low-level.

#+kcl
(defmacro defcond (name (parent-type &rest include-slots) &rest new-slots)
  `(oplan-util::define-condition-struct
       (,name (:include ,parent-type ,@include-slots))
     ,@new-slots))


;;;; Lock files

(define-condition lockout (simple-error) ())

(defmacro with-lock-file (filename &rest forms)
  `(call-with-lock-file ,filename #'(lambda () ,@forms)))

(defun call-with-lock-file (filename thunk)
  (let ((stream (open filename :direction :output :if-exists nil)))
    (if (null stream)
	(signal-error 'lockout
		      :format-string "Can't open lock ~S."
		      :format-arguments (list filename))
      (unwind-protect
	  (progn (close stream)
		 (funcall thunk))
	(delete-file filename)))))


;;;; String and other (low-level) utilities

;;; /\/: Most of this should go in support/util-functions.lsp.

;;; Print-length determines how many characters are needed to print
;;; various simple objects (symbols, strings, integers) w/o escape chars.

(defun print-length (x)
  (cond ((stringp x) (length x))
	((symbolp x) (length (symbol-name x)))
	((integerp x)
	 (cond ((>= x 0) (int-print-length x))
	       (t        (1+ (int-print-length x)))))
	(t
	 (error "Can't determine the print-length of ~S." x))))

(defun int-print-length (i &optional (base 10))
  (do ((c 1 (1+ c))
       (q (abs i) (floor q base)))
      ((< q base)
       c)))


;;; Hash-tables

(defun copy-hash-table (ht &key (test #'eql))
  ;; The test is used when creating the result table.
  (alist->hash-table (hash-table-alist ht) :test test))


;;; Sequence functions

;;; /\/: Move replace-subseq to util-function.lsp?

;;; /\/: Will type-of always return something concatenate will like?

(defun replace-subseq (new old seq)
  (let ((pos (search old seq)))
    (if (null pos)
        seq
      (concatenate (type-of seq)
	  (subseq seq 0 pos)
          new
          (replace-subseq new
			  old
			  (subseq seq (+ pos (length old))))))))

(defun sequence-begins (init-seg seq)
  (let ((m (mismatch init-seg seq)))
    (or (not m)
	(eql m (length init-seg)))))

(defun sequence-after (init-seg seq)
  (subseq seq (length init-seg)))

;;; *char=
;;;
;;; /\/: *char= is needed at least in AKCL.  With *char= we get
;;; if(!((V1)==(((V2))->ust.ust_self[V6]))) ...; otherwise we get
;;; if(!(char_code(code_char(V1))==char_code(code_char(((V2))->
;;; ust.ust_self[fix(V7)]))))... .  Why we have fix(V7) is not clear.
;;; [This is from (disassemble 'break-string-at).]

(defmacro *char= (c1 c2)
  `(char= (the character ,c1) (the character ,c2)))

;;; (Break-string-at char string) takes a string containing fields
;;; separated by char and returns a list of the fields, in order, as
;;; strings.

(defun break-string-at (char string)
  (declare (character char) (simple-string string))
  (let ((start 0)
	(parts '()))
    (declare (fixnum start))
    (fix-dotimes (i (length string))
      (when (*char= char (schar string i))
	(push (subseq string start i) parts)
	(setq start (fix1+ i))))
    (nreverse (cons (subseq string start) parts))))

;;; (Break-string-at-first char string) takes a string containing fields
;;; separated by char and returns two values: the substring before the
;;; first occurrence of the char, and the substring after.  Neither
;;; substring contains the char.  If the char does not appear in the
;;; string at all, the values are the string and "".

(defun break-string-at-first (char string)
  (declare (character char) (simple-string string))
  (fix-dotimes (i (length string) (values string ""))
    (when (*char= char (schar string i))
      (return
	(values (subseq string 0 i)
		(subseq string (fix1+ i)))))))

;;; (break-string-at-last char string) is like a break-string-at-first
;;; that ignores all instances of char except the last.  So if the char
;;; does not appear at all, the values are string and "".

(defun break-string-at-last (char string)
  (declare (character char) (simple-string string))
  (let ((j (length string)))
    (declare (fixnum j))
    (fix-dotimes (i j (values string ""))
      (setq j (fix1- j))
      (when (*char= char (schar string j))
        (return
	  (values (subseq string 0 j)
		  (subseq string (fix1+ j))))))))

(defun concat-strings-with-separator (sep-string strings)
  (if (null strings)
      ""
    (big-string-concat
      (cons (car strings)
	    (flatten-one-level
	      (mapcar #'(lambda (s) (list sep-string s))
		      (cdr strings)))))))

(defun encode-string (s char-map)
  ;; Converts chars into char sequences as specified by the alist char-map.
  ;; Assumes that all transformations make the text longer.
  (let ((outlen 0))
    ;; See how long the output string needs to be.
    (dotimes (i (length s))
      (let ((e (lookup (schar s i) char-map)))
	(incf outlen (if e (length e) 1))))
    ;; If the output would be the same length as the input, no
    ;; encoding is needed.
    (when (= outlen (length s))
      (return-from encode-string s))
    ;; Encode away...
    (let ((output (make-string outlen))
	  (outpos 0))
      (dotimes (i (length s))
	(let* ((c (schar s i))
	       (e (lookup c char-map)))
	  (cond (e (dotimes (ei (length e))
		     (setf (schar output outpos) (schar e ei))
		     (incf outpos)))
		(t (setf (schar output outpos) c)
		   (incf outpos)))))
      (assert (= outpos (length output)))
      output)))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
