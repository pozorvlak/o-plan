;;;; File: util-functions.lsp
;;; Contains: Useful functions that don't belong anywhere else
;;; Author: Jeff Dalton
;;; Created: February 1993
;;; Updated: Thu Feb 24 03:50:48 2000 by Jeff Dalton
;;; Copyright: (c) 1993, 1994, 1995, 1996 AIAI, University of Edinburgh


(in-package :oplan-util)


;;; In general, global/special variables are not exported from the
;;; UTIL package; instead, they are manipulated via (exported) functions.


;;;; Parameter database

(defvar *parameter-table* (make-hash-table))

(defun get-parameter (name)
  (gethash name *parameter-table*))

(defun set-parameter (name value)
  (setf (gethash name *parameter-table*) value))

(defsetf get-parameter set-parameter)

(defun parameter-set-p (name)
  ;; Has the parameter ever been set?
  (multiple-value-bind (value found-p) (gethash name *parameter-table*)
    (values found-p value)))

(defun parameter-alist ()
  (sort (hash-table-alist *parameter-table*)
	#'string-lessp
	:key #'car))


;;;; Environment variable enquiries

;;; (getenv string) -> string or nil is defined in an implementation-
;;; dependent manner.  See kcl-util.lsp, allegro-util.lsp, etc.

(defun getenv-else (default string)
  (or (getenv string)
      default))

(defun getenv-else-error (string)
  (or (getenv string)
      (error "No value for environment variable ~S." string)))


;;;; Mode operations

(defun flip-mode (variable)
  (setf (symbol-value variable)
	(ecase (symbol-value variable)
	  (:auto :ask)
	  (:ask :auto))))

(defun opposite-mode (mode)
  (ecase mode
    (:auto :ask)
    (:ask :auto)))


;;;; Reference objects

(defstruct (reference (:conc-name ref-)
		      (:constructor ref-to (value)))
  (value 'not-a-valid-ref-value))


;;;; Concatenation and other string operations

(defun concat-name (s1 s2 &rest more)
  (intern (apply #'concatenate 'string
		 (string s1) (string s2) (mapcar #'string more))))

(defun concat-string (s1 s2 &rest more)
  (apply #'concatenate 'string
	 (string s1) (string s2) (mapcar #'string more)))

(defun big-string-concat (strings)
  (let ((len 0))
    (dolist (s strings) (incf len (length s)))
    (let ((big-string (make-string len))
	  (i 0))
      (dolist (s strings big-string)
	(replace big-string s :start1 i)
	(incf i (length s))))))

(defun string->keyword (s)
  (intern (string-upcase s) (find-package :keyword)))

(defun string->int (s)
  (handler-case (values (parse-integer s))
    (error (c)
      (error "Can't convert ~S to an int because: ~A" s c))))

(defun int->string (i)
  (write-to-string i))


;;; Break-args.

;;; A function that runs a Unix process may expect a list of argv
;;; arguments, but you may have them all together in a string.

;;; (Break-args string) takes a string containing space-separated
;;; command-line arguments and returns a list of the arguments, each
;;; as a separate string.  It can handle args that begin and end
;;; with " but cannot handle escape chars.

;;; /\/: This is a bit perverse in Allegro, because we have to concat
;;; the args into a single string in order to run a program.  So if we
;;; break up a string, we'll eventually put it back together again.

(defun break-args (string)
  (let ((p 0)
	(args '())
	(chars '()))
    (flet ((current () (schar string p))
	   (next () (incf p))
	   (done () (>= p (length string))))
      (loop
        ;; Scan off leading spaces
        (loop (when (done) (return))
	      (if (char= (current) #\space)
		  (next)
		(return)))
	;; Scan next arg
	(when (done)
	  (return (nreverse args)))
	(setq chars '())
	(cond ((char= (current) #\")
	       ;; Scan quoted string
	       (next)
	       (loop (cond ((or (done) (char= (current) #\"))
			    (push (coerce (nreverse chars) 'string)
				  args)
			    (next)
			    (return))
			   (t (push (current) chars)
			      (next)))))
	      (t
	       ;; Scan to next space
	       (loop (cond ((or (done) (char= (current) #\space))
			    (push (coerce (nreverse chars) 'string)
				  args)
			    (next)
			    (return))
			   (t (push (current) chars)
			      (next))))))))))


;;;; List and sequence operations

;;; Car- and cdr-if-consp are useful for processing such things as
;;; defstruct slot specs, where the value might be either a name or
;;; a list beginning with a name.

(defun car-if-consp (x)
  (if (consp x) (car x) x))

(defun cdr-if-consp (x)
  (if (consp x) (cdr x) x))


;;; Length=1 is used to test whether a list contains only one element.
;;; It should be faster than actually calling length, and it may remove
;;; the temptation to just say (null (cdr x)) w/o being sure x is a cons
;;; (or when relying on an earlier test in a COND that might change).
;;; /\/: A different name, such as singleton-p, might be better.

(defun-inline length=1 (x)
  (and (consp x) (null (cdr x))))

(defun-inline length>1 (x)
  (and (consp x) (consp (cdr x))))


;;; List-beginning is a useful combined test copied from some JAR code.

(defun-inline list-beginning (item obj)
  (and (consp obj)
       (eq (car obj) item)))


;;; List-proper-prefix-p

(defun list-proper-prefix-p (prefix items)
  ;; Prefix must be an initial segment of items that's also shorter
  ;; than items.
  (do ((pref prefix (cdr pref))
       (tail items (cdr tail)))
      (nil)
    (cond ((null pref)
	   (return (not (null tail))))
	  ((null tail)
	   (return nil))
	  ((not (eql (car tail) (car pref)))
	   (return nil)))))


;;; Last-element does what last doesn't, namely return the last element.

(defun-inline last-element (lis)
  (car (last lis)))

;;; Last-elt returns the last element of a sequence.

(defun-inline last-elt (seq)
  (if (listp seq)
      (last-element seq)
    (let ((len (length seq)))
      (declare (fixnum len))
      (if (= len 0)
	  nil
	(elt seq (the fixnum (1- len)))))))


;;; Replace-sublist replaces non-overlapping occurrences of one sublist
;;; by another.
;;;
;;; (replace-sublist '(the big cat) '(the cat) '(the cat chased the rat))
;;;   ==> (the big cat chased the rat)
;;;
;;; (replace-sublist '(c d) '(a b a) '(a b a b a b a))
;;;   ==> (c d b c d)
;;;
;;; [Have to watch out for infinite loops when old is null.]

(defun replace-sublist (new old lis)
  (when (not (consp old))
    (error "Cannot replace a null sublist."))
  (let ((result '()))
    (loop
      (cond ((null lis)
	     (return (nreverse result)))
	    ((eq (car lis) (car old))
	     (do ((l-tail (cdr lis) (cdr l-tail))
		  (o-tail (cdr old) (cdr o-tail)))
		 (nil)
	       (cond ((null o-tail)
		      ;; Have a match
		      (setq result (revappend new result)
			    lis l-tail)
		      (return))
		     ((null l-tail)
		      ;; No more matches are possible
		      (setq result (revappend lis result)
			    lis nil)
		      (return))
		     ((eq (car l-tail) (car o-tail))
		      ;; Still matching
		      )
		     (t
		      ;; Not a match
		      (push (pop lis) result)
		      (return)))))
	    (t
	     (push (pop lis) result))))))

#+:undef
(defun replace-sublist (new old lis)
  (let ((pos (search old lis)))
    (if (null pos)
	lis
	(append (subseq lis 0 pos)
		new
		(replace-sublist new
				 old
				 (subseq lis (+ pos (length old))))))))

;;; Take and drop operations

(defun-inline take (n seq)
  (subseq seq 0 n))

(defun-inline drop (n seq)
  (subseq seq n))

(defun take-while (pred seq)
  (let ((pos (position-if-not pred seq)))
    (if (null pos)
        seq
        (subseq seq 0 pos))))

(defun drop-while (pred seq)
  (let ((pos (position-if-not pred seq)))
    (if (null pos)
        (subseq seq 0 0)		;the null sequence of the same type
        (subseq seq pos))))

(defun take-until (pred seq)
  (let ((pos (position-if pred seq)))
    (if (null pos)
        seq
        (subseq seq 0 pos))))

(defun drop-until (pred seq)
  (let ((pos (position-if pred seq)))
    (if (null pos)
        (subseq seq 0 0)		;the null sequence of the same type
        (subseq seq pos))))


;;; Set-eql dertermines whether to two sets are equal when the elements
;;; can be compared by eql.

(defun set-eql (set1 set2)
  (null (set-exclusive-or set1 set2)))

;;; (Disjoint-sets-p set1 set2) is equiv to (null (intersection set1 set2))

(defun disjoint-sets-p (set1 set2)
  (dolist (e set1 t)
    (when (member e set2)
      (return nil))))

;;; Set-difference doesn't guarantee the order of the result.
;;; This does.  It retains the order in the first argument.

(defun stable-set-difference (set items-to-remove
			       &key (key #'identity) (test #'eql))
  ;; Like set-difference but preserves the order in the 1st arg
  ;; and does not support :test-not.
  (declare (inline remove-if member))
  (remove-if #'(lambda (item)
		 (member (funcall key item) items-to-remove
			 :key key :test test))
	     set))

;;; For union, Allegro seems to push onto set2 while GCL retains
;;; the order from set1.  Both seem to preserve set2.

;;; /\/: It's arguably more stable to preserve set1.  For instance,
;;; if you use reduce some something else that repeatedly calls
;;; stable-union to build a result, it would be nice to preserve
;;; the order in the set as build so far, when it's the 1st are
;;; to stable-union.  But it's difficult to change this right
;;; now [June 99], so for now we'll just define an internal
;;; more-stable-union.  Later, rename more-stable-union to stable-union
;;; and the current stable-union to standard-union.

(defun standard-union (set1 set2 &rest whatever)
  ;; /\/: Let people start using it now, rectification of names later.
  (apply #'stable-union set1 set2 whatever))

(defun stable-union (set1 set2 &key (test #'eql) (key #'identity))
  (append (stable-set-difference set1 set2 :test test :key key)
	  set2))

(defun more-stable-union (set1 set2 &key (test #'eql) (key #'identity))
  (append set1
	  (stable-set-difference set2 set1 :test test :key key)))

(defun standard-intersection (set1 set2 &key (test #'eql) (key #'identity))
  ;; It looks like Lisps tend to do something like this.
  (declare (inline member))
  (let ((result '()))
    (dolist (e set1 result)
      (when (member (funcall key e) set2 :test test :key key)
	(push e result)))))

(defun stable-intersection (set1 set2 &key (test #'eql) (key #'identity))
  ;; This one is more stable, because it preserves the order in set1,
  ;; but less like the ones in the Lisps we use.
  (declare (inline remove-if not member))
  (remove-if #'(lambda (e)
		 (not (member (funcall key e) set2 :test test :key key)))
	     set1))

;;; Interleave

;;; /\/: If interleave stopped at the end of the shortest list,
;;; and didn't care about the call-arguments-limit, it could use
;;; (apply #'mapcan #'list lists).

(defun interleave (&rest lists)
  ;; Acts as if shorter lists were extended with NILs.
  (if (every #'null lists)
      '()
    (append (mapcar #'car lists)
	    (apply #'interleave (mapcar #'cdr lists)))))


;;; Transposition

(defun transpose-list (list-of-lists)
  ;; Turns a list of rows into a list of columns and vice versa.
  ;; E.g. ((a b c) (1 2 3)) => ((a 1) (b 2) (c 3))
  (apply #'mapcar #'list list-of-lists))


;;; Turn a list of lists into a list

(defun flatten-one-level (list)
  (mapcan #'copy-list list))		;one of many ways to do it


;;; For-adjacent-elements applies a function to all pairs of adjacent
;;; elements in a list, taking the pairs from left to right.  For example,
;;; in (a b c d e), the pairs would be (a b) (b c) (c d) (d e), in that
;;; order.

(defun for-adjacent-elements (fn lis)
  (if (null lis)
      nil
    (do ((tail lis (cdr tail)))
	((null (cdr tail)))
      (funcall fn (car tail) (cadr tail)))))


;;; Walk-tree.  Treats NILs as empty trees rather than leaves.

(defun walk-tree (fn tree)
  (cond ((null tree)
	 nil)
	((atom tree)
	 (funcall fn tree))
	(t
	 (walk-tree fn (car tree))
	 (walk-tree fn (cdr tree)))))

(defun remap-tree (fn tree)
  (cond ((null tree)
	 nil)
	((atom tree)
	 (funcall fn tree))
	(t
	 (recons tree
		 (remap-tree fn (car tree))
		 (remap-tree fn (cdr tree))))))


;;; Utilities for lists of fixnums

(defmacro fix-member (i lis)
  `(do ((tail ,lis (cdr tail))
	(i ,i))
       ((null tail) nil)
     (declare (fixnum i))
     (when (fix= i (car tail))
       (return tail))))

(defun fix-delete (item lis)
  (declare (fixnum item))
  (if (null lis)
      lis
    (let ((root lis))
      ;; Scan off initial elements = to item.
      (while (and root (fix= (car root) item))
	(setq root (cdr root)))
      ;; Delete item from rest of list.
      (when root
	(let ((tail root))
	  (loop (when (null (cdr tail))
		  (return))
		(if (fix= item (cadr tail))
		    (setf (cdr tail) (cddr tail))
		    (setq tail (cdr tail))))))
      root)))



;;; (REMOVE-1-EQ item list) = (REMOVE item list :test #'EQ :COUNT 1)
;;; (DELETE-1-EQ item list) = (DELETE item list :test #'EQ :COUNT 1)

(defun remove-1-eq (item list)
  ;; N.B. We expect that the item will occur, so we don't try to avoid
  ;; copying in the case where it doesn't.
  (cond ((null list)
	 nil)
	((eq item (car list))
	 (cdr list))
	(t (let ((head (list (car list))))
	     (do ((ptr head (cdr ptr))
		  (tail (cdr list) (cdr tail)))
		 (nil)
	       (cond ((null tail)
		      (return head))
		     ((eq item (car tail))
		      (setf (cdr ptr) (cdr tail))
		      (return head))
		     (t (setf (cdr ptr) (list (car tail))))))))))

(defun delete-1-eq (item list)
  (cond ((null list)
	 nil)
	((eq item (car list))
	 (cdr list))
	(t (do ((tail list (cdr tail)))
	       ((null (cdr tail))
		list)
	     (when (eq item (cadr tail))
	       (setf (cdr tail) (cddr tail))
	       (return list))))))


;;; Tconcs

;;; A tconc structure is a cons containing a headed list (whose car is
;;; the symbol TCONC) and a pointer to the last cons of the headed list
;;; (it's headed to ensure it contains at least one cons).  The last-cons
;;; pointer makes it possible to add new elements to the end efficiently.
;;; A tconc can also be passed around so that elements can be added in
;;; different functions.

;;; /\/: A better, though less traditional, name might be chosen instead
;;; of tconc.  However, names that involve "collect" might clash with the
;;; series iteration macros.

;;; /\/: Tconcs are very like queues, which we also have.

(defun make-tconc ()
  (let ((contents (list 'tconc)))
    (cons contents contents)))

(defun-inline tconc (tc-struct new-elt)
  (setf (cdr tc-struct)
	(setf (rest (cdr tc-struct)) (list new-elt)))
  tc-struct)

(defun lconc (tc-struct lis)
  (setf (cdr tc-struct)
	(last (setf (rest (cdr tc-struct)) lis)))
  tc-struct)

(defun-inline tconc-contents (tc-struct)
  (rest (car tc-struct)))


;;; How to avoid consing...

(defun recons (old-cons new-car new-cdr)
  (if (and (eq new-car (car old-cons))
	   (eq new-cdr (cdr old-cons)))
      old-cons
    (cons new-car new-cdr)))

(defun remapcar (f l)
  (if (atom l)
      l
    (recons l
	    (funcall f (car l))
	    (remapcar f (cdr l)))))


;;; List-lessp performs a lexicographic comparison of two lists,
;;; given a < for elements.

(defun list-lessp (element-lessp lis1 lis2)
  (labels ((compare (one two)
	     (cond ((null one) (not (null two)))
		   ((null two) nil)
		   ((funcall element-lessp (first one) (first two)) t)
		   ((funcall element-lessp (first two) (first one)) nil)
		   (t (compare (rest one) (rest two))))))
    (compare lis1 lis2)))

;;; Equivalence-classes returns an a-list with entries of the form
;;; (key . items) where the items in an entry all have the same key
;;; value.

(defun equivalence-classes (set &key (key #'identity) (test #'eql))
  (setq set (reverse set))		;make the final order nicer
  (let ((alist '()))
    (dolist (e set)
      (let* ((k (funcall key e))
	     (entry (assoc k alist :test test)))
	(if entry
	    (push e (cdr entry))
	    (push (cons k (list e)) alist))))
    alist))

;;; (Group-by-numeric-key items key-fn) assumes the keys are non-negative
;;; integers and that they occupy a fairly small range.  It constructs a
;;; hash table that maps a key value to a list of all items such that
;;; (funcall key-fn item) gives that key value.  The hash-table is then
;;; used to construct a list in which the n-th element is the list of
;;; all items with key-value n.

(defun group-by-numeric-key (items key-fn)
  (let ((key->items (make-hash-table))
	(max-key 0))
    (dolist (item (reverse items))	;to preserve order when using push
      (let ((key (funcall key-fn item)))
	(push item (gethash key key->items))
	(setq max-key (max key max-key))))
    (let ((result (make-list (1+ max-key))))
      (do ((tail result (cdr tail))
	   (i 0 (1+ i)))
	  ((null tail) result)
	(setf (car tail) (gethash i key->items))))))
      

;;; (Rotate-list L N) rotates the list L N places to the left.
;;; If N < 0, it's in effect rotated to the right.

(defun rotate-list (list count)
  (if (< count 0)
      (rotate-list list (+ (length list) count))
    (let ((tail (nthcdr count list)))
      (if (null tail)
	  (rotate-list list (mod count (length list)))
	(append tail (ldiff list tail))))))

;;; (Chunk-list L N) returns a list of the elements of L grouped into
;;; "chunks" of length N.  For instance:
;;;
;;;    (chunk-list '(a b c d e) 2) ==> ((a b) (c d) (e))
;;;

(defun chunk-list (list chunk-length)
  (assert (> chunk-length 0))
  (let ((tail (nthcdr chunk-length list)))
    (if (null tail)
	(list list)
      (cons (ldiff list tail)
	    (chunk-list tail chunk-length)))))

;;; (Find-best list lessp) finds the first element e of the list
;;; such that for all later elements e' (not (funcall lessp e' e)).

(defun find-best (list lessp)
  (let ((best (car list)))
    (dolist (item (cdr list))
      (when (funcall lessp item best)
	(setq best item)))
    best))

;;; Max-value calls a function on all elements of a list and returns
;;; that maximum value returned by the function.

(defun max-value (fn items)
  (loop for i in items maximize (funcall fn i)))


;;; A comparison that allows renaming.

(defun equal-with-1-1-renames (old new &key renamable-p) ; -> T/F, rename-alist
  ;; If A is renamed to B, nothing else, not even B itself,
  ;; can be renamed to B.  Moreover, if A lines up with B at
  ;; some point, comparing as in equal, it must always line
  ;; up with B.
  (let ((new-names (make-hash-table :test #'eq))
	(old-names (make-hash-table :test #'eq))
	(no-entry (list nil)))
    (labels ((compare (old new)
	       (cond ((consp old)
		      (and (consp new)
			   (compare (car old) (car new))
			   (compare (cdr old) (cdr new))))
		     ((consp new)
		      nil)
		     ((and (funcall renamable-p old) (funcall renamable-p new))
		      (compare-renamables old new))
		     (t
		      (eql old new))))
	     (compare-renamables (old new)
	       ;; old and new are renamable.  If old has already been
	       ;; renamed, its new name must eq new; otherwise old is
	       ;; renamed to new provided that nothing else has been
	       ;; renamed to new.  Note it counts as a rename if
	       ;; something is renamed to itself.  This ensures that
	       ;; if A is renamed to B, B is renamed to something
	       ;; other than B.
	       (let ((new-name (gethash old new-names no-entry)))
		 (if (not (eq new-name no-entry))
		     ;; Old has been renamed.
		     (eq new-name new)
		   ;; Old has not been renamed.
		   (let ((old-name (gethash new old-names no-entry)))
		     (cond ((eq old-name no-entry)
			    ;; Nothing has been renamed to new,
			    ;; so old can be.
			    (setf (gethash old new-names) new)
			    (setf (gethash new old-names) old)
			    t)
			   (t
			    ;; Something has been renamed to new,
			    ;; presumably something other than old.
			    (assert (not (eq old-name old)))
			    nil)))))))
      (if (compare old new)
	  (values t
		  (filtered-hash-table-alist
		    new-names
		    ;; Filter out renames to self
		    #'(lambda (k v) (if (eq k v) nil (cons k v)))))
	  (values nil
		  nil)))))



;;;; Lexicographic ordering for lists

;;; Canonical-description-order is used to sort various descriptions
;;; produced as output from the planner.  This has two goals: to make
;;; it easier to find things in long descriptions and to ensure that
;;; we get the same order in all Common Lisps.  (A different order
;;; might result if, say, maphash was used to produce the items in
;;; a description.)

(defun canonical-description-order (items)
  (sort (copy-list items) #'object-lessp))

;;; Object-lessp is a somewhat arbitrary comparison that can be
;;; applied to a variety of objects.

;;; Atoms are < conses.
;;; Numbers are < other kinds of atoms.
;;; Strings and symbols are < other kinds of atoms except numbers.
;;; Symbols and strings are compared as if by string<.
;;; Other kinds of atoms are all = to each other, hence not <.

(defun object-lessp (a b)
  (cond ((atom a)
	 (if (atom b)
	     (atom-lessp a b)		; atom vs atom
	     t))			; atom < cons
	((atom b)
	 nil)				; cons not < atom
	(t
	 ;; a < b if car[a] < car[b]
	 ;;       or car[a] = car[b] and cdr[a] < cdr[b]
	 (ecase (compare-objects (car a) (car b))
	   (:less    t)
	   (:equal   (object-lessp (cdr a) (cdr b)))
	   (:greater nil)))))

(defun atom-lessp (a b)
  ;; Numbers are < other kinds of atoms.
  ;; Strings and symbols are < other kinds of atoms except numbers.
  ;; Symbols are strings are compared by string<.
  ;; Other kinds of atoms are all =, hence not <.
  (cond ((numberp a)
	 (cond ((numberp b) (< a b))
	       (t t)))
	((or (symbolp a) (stringp a))
	 (cond ((or (symbolp b) (stringp b)) (string< a b))
	       ((numberp b) nil)
	       (t t)))
	(t nil)))

;;; Compare arbitrary objects
;;;
;;; (compare-objects a b) determines whether a is <, =, or > b.
;;; The reason we want to, in effect, test a < b and a = b at the
;;; same time is that when, for example, recursively comparing
;;; two conses for <,
;;;    a < b if car[a] < car[b]
;;;          or car[a] = car[b] and cdr[a] < cdr[b]
;;; Determining < and = at once is presumably faster than doing
;;; two separate compares.
;;;
;;; Atoms are < conses.

(defun compare-objects (a b) ; -> :less, :equal, or :greater
  (cond ((atom a)
	 (cond ((atom b)
		(compare-atoms a b))	; atom vs atom
	       (t
		:less)))		; atom vs non-atom
	((atom b)
	 :greater)			; cons vs atom
	(t
	 ;; cons vs cons
	 (case (compare-objects (car a) (car b))
	   (:less    :less)
	   (:equal   (compare-objects (cdr a) (cdr b)))
	   (:greater :greater)))))

;;; Compare atoms

;;; Numbers are < other kinds of atoms.
;;; Strings and symbols are < other kinds of atoms except numbers.
;;; Symbols are converted to strings and compared as if by string<.
;;; Other kinds of atoms are all = to each other, hence not <.

;;; N.B. In KCL at least, (eq (string 'a) (string 'a)) ==> nil, but
;;; (eq (symbol-name 'a) (symbol-name 'a)) ==> t.  So we don't want
;;; to call string.

;;; In KCL, char= is faster than char< or char> (or at least does more
;;; inline), presumably because the official Common Lisp character order
;;; might be different from the natural machine order and > and < depend
;;; on the official order while = doesn't.

(defun compare-atoms (a b) ; -> :less, :equal, or :greater
  (cond ((numberp a)
	 (cond ((numberp b)
		(cond ((< a b) :less)	; number vs number
		      ((= a b) :equal)
		      (t       :greater)))
	       (t :less)))		; number vs non-number
	((numberp b)
	 :greater)			; non-number vs number
	((symbolp a)
	 (cond ((symbolp b)
		;; symbol vs symbol
		(compare-strings (symbol-name a) (symbol-name b)))
	       ((stringp b)
		;; symbol vs string
		(compare-strings (symbol-name a) b))
	       (t
		;; symbol vs random atom
		:less)))
	((stringp a)
	 (cond ((symbolp b)
		;; string vs symbol
		(compare-strings a (symbol-name b)))
	       ((stringp b)
		;; string vs string
		(compare-strings a b))
	       (t
		;; string vs random atom
		:less)))
	(t
	 ;; random atom vs random atom
	 :equal)))

(defun compare-strings (a b)
  (declare (string a b))
  (let* ((len-a (length a))
	 (len-b (length b))
	 (m (fix-min2 len-a len-b)))
    (declare (fixnum len-a len-b m))
    (or (fix-dotimes (i m)
          (let ((ca (char a i))
		(cb (char b i)))
	    (cond ((char= ca cb))
		  ((char< ca cb) (return :less))
		  (t             (return :greater)))))
	(cond ((fix< len-a len-b) :less)
	      ((fix= len-a len-b) :equal)
	      (t                  :greater)))))



;;; Topological sort (returning ancestors before descendants).

(defun tsort (root children-fn &key (hash-test #'eq)) ; -> list(node)
  (let ((marks (make-hash-table :test hash-test))
	(result '()))
    (macrolet ((mark (node) `(gethash ,node marks)))
      (labels
	  ((walk (at)
	     (ecase (mark at)
	       ((:start)
		(error "Cycle involving ~S." at))
	       ((:finish)
		;; already processed
		)
	       ((nil)
		(setf (mark at) :start)
		(dolist (child (funcall children-fn at))
		  (walk child))
		(push at result)
		(setf (mark at) :finish)))))
	(walk root)
	result))))


;;; DAG Longest-path-lengths algorithm

;;; Finds distances from the start vertex by finding the number of
;;; steps in the longest path along successor links from the start to
;;; each vertex, where the length of each link is 1.  The results are
;;; returned in a hash table that maps vertices to distances.
;;; Vertices must be represented by objects that are EQL-uniqie.

;;; This is O(v+e) where v is the number of vertices and e the number
;;; of edges (links to successors).

(defun find-longest-path-lengths (start get-successors) ; -> hash-table
  (let ((distance-table (make-hash-table))
	(vertex-sequence (tsort start get-successors)))
    (setf (gethash start distance-table) 0)
    (dolist (v vertex-sequence)
      (let ((dist-to-v (gethash v distance-table)))
	(assert (numberp dist-to-v))
	(dolist (child (funcall get-successors v))
	  (setf (gethash child distance-table)
		(max (gethash child distance-table 0)
		     (1+ dist-to-v))))))
    distance-table))


;;;; Function tools

(defun partial1 (fn arg1)		;partial application
  #'(lambda (&rest more-args)
      (apply fn arg1 more-args)))

(defun compose2 (f g)
  #'(lambda (x) (funcall f (funcall g x))))

(defun wrap-1-arg-memoizer (fn &key (test #'eql)) ; -> function
  (let ((memo-table (make-hash-table :test test)))
    #'(lambda (arg)
	(multiple-value-bind (value found-p)
	    (gethash arg memo-table)
	  (if found-p
	      value
	      (setf (gethash arg memo-table) (funcall fn arg)))))))


;;;; Hash table tools

(defun alist->hash-table (a-list &key (test #'eql))
  (let ((ht (make-hash-table :test test)))
    (dolist (entry a-list)
      (let ((key (car entry))
	    (val (cdr entry)))
	(setf (gethash key ht) val)))
    ht))

(defun hash-table-alist (hash-table)
  (let ((entries '()))
    (maphash #'(lambda (key value)
		 (push (cons key value) entries))
	     hash-table)
    entries))

(defun filtered-hash-table-alist (hash-table filter)
  ;; The filter returns nil or a cons.  Only the conses are recorded
  ;; in the result.  The cons can be the original key and value from
  ;; the hash table or some function of the key and value.
  (let ((entries '()))
    (maphash #'(lambda (key value)
		 (let ((filter-value (funcall filter key value)))
		   (when filter-value
		     (push filter-value entries))))
	     hash-table)
    entries))

(defun hash-table-keys (hash-table)
  (let ((keys '()))
    (maphash #'(lambda (key value)
		 (declare (ignore value))
		 (push key keys))
	     hash-table)
    keys))
  
(defun hash-table-values (hash-table)
  (let ((vals '()))
    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (push value vals))
	     hash-table)
    vals))

(defun hash-table->function (ht &optional default)
  #'(lambda (x)
      (gethash x ht default)))


;;;; A-list tools

(defun-inline alist-key (entry) (car entry))

(defun-inline alist-value (entry) (cdr entry))

(defsetf alist-value (entry) (new-value)
  `(setf (cdr ,entry) ,new-value))

(defun alist->function (a-list)
  #'(lambda (key) (cdr (assoc key a-list))))

;;; Lookup doesn't signal an error if no entry is found so that we
;;; can use it in conditionals.

(defun lookup (key a-list)
  (cdr (assoc key a-list)))		;assumes (cadr nil) ==> nil


;;;; P-list tools

(defun walk-plist (fn plist)
  (do ((p plist (cddr p)))
      ((null p))
    (when (consp p)			;in case prop's value is missing
      (funcall fn (car p) (cadr p)))))


;;;; I/O and file tools

;;; Find-all-files returns pathames of all files in a given directory
;;; that have a given type.

;;; The two versions below are not completely equivalent.  The first
;;; uses the CL directory function and hence returns truenames.

;;; The second runs "/bin/ls" to get the file names (w/ their types)
;;; and then constructs pathnames that include the directory, file name,
;;; and file type, but which may not be true names and may not start
;;; at "/" (root).  The whole point of the second version is to avoid
;;; getting truenames, because that can be extremely slow (eg, with
;;; GCL 1.0 on FreeBSD 2.0).

;;; /\/: At present, we always use the 2nd version so that it can
;;; be thoroughly tested.

#+:gcl-2
(defun find-all-files (directory type)
  ;; Some CLs understand "*" in names as :wild, but other CLs,
  ;; such as Allegro, do not.  So this should be more portable.
  (directory
   (merge-pathnames directory
		    (make-pathname :name :wild :type type))))

#-:gcl-2
(defun find-all-files (directory type)
  (check-type directory string)
  (check-type type string)
  (unless (or (string= directory "")
	      (eql (last-elt directory) #\/ ))
    (error "Directory name must be empty or end in \"/\"."))
  (unless (probe-file directory)
    (error "Directory ~S does not exist." directory))
  (let* ((files
	  (with-unix-process-io (io "/bin/ls" directory)
	    (stream->lines io)))
	 (paths
	  (mapcar #'pathname files)))
    ;; The paths are used only to check the type.  It may be better
    ;; to do that as a string operation.  /\/
    (loop for f in files as p in paths
	  when (and (pathname-type p)
		    (string= (pathname-type p) type))
	  collect (pathname (concat-string directory f)))))


;;; Init-file loader: tries the name as-is, then in the user's home directory.

(defun load-init-file (name &optional package-name)
  ;; The name is something like "oplan-init".
  (let ((home-init (merge-pathnames name (user-homedir-pathname)))
	(*package* (if package-name (find-package package-name) *package*)))
    (cond ((probe-file name)
	   (load name))
	  ((probe-file home-init)
	   (load home-init)))))


;;; Load-most-recent

;;; A version of load that loads source or object, depending on which
;;; has been most recently modified.  Note that we get the source and
;;; object types from defsystem.  (See the shadowing-import above.)

(defun load-most-recent (pathname &rest load-args)
  (if (pathname-type pathname)
      (apply #'load pathname load-args)
    (apply #'load (apply #'find-most-recent pathname load-args)
	          load-args)))

(defun find-most-recent (pathname &key (if-does-not-exist :error)
				  &allow-other-keys)
  (unless (member if-does-not-exist '(:error nil))
    (error "Invalid :if-does-not-exist value for find-most-recent: ~S."
	   if-does-not-exist))
  (let* ((source (merge-pathnames (make-pathname :type *source-type*)
				  pathname))
	 (object (merge-pathnames (make-pathname :type *object-type*)
				  pathname))
	 (source-p (probe-file source))
	 (object-p (probe-file object)))
    (cond ((not source-p)
	   (if object-p
	       object
	     ;; Can't find source or object
	     (if (null if-does-not-exist)
		 nil
	       (error "Cannot find source or object for ~S." pathname))))
	  ((not object-p)
	   source)
	  ((> (file-write-date object) (file-write-date source))
	   object)
	  (t
	   source))))


;;; Print-readably is used when we want to make reasonably sure that the
;;; output can be read by the Lisp reader, e.g. when communicating with a
;;; separate Lisp process.

(defun-inline print-readably
     (object &optional (output-stream *standard-output*))
  ;; /\/: In theory, we should use with-standard-io-syntax in ANSI CLs,
  ;; bit it binds too many variabes that we might want to vary.
  (let (#+(or :cltl2 :ansi-cl) (*print-readably* t)
	#+(or :cltl2 :ansi-cl) (*read-eval* nil))
      (write object
	 :stream output-stream
	 :escape t :pretty nil :length nil :level nil)))


;;; Read-safely is used to read input from external sources such
;;; as separate Lisp processes.  It (a) protects us from "#." and
;;; "#," and (b) gives a useful error message when #<...>" is used.

;;; We define "#<" to collect everything up to the ">" before signalling
;;; an error.  Otherwise, we get the error as soon as the "<" is seen,
;;; making it tricky to find out what follows it.  (This is so in at
;;; least GCL 1.1 and Lucid CL 4.1.)

;;; #. and #, are defined to signal an error.

(defvar *safe-readtable* (copy-readtable *readtable*))

(defun-inline read-safely (&optional (input-stream *standard-input*)
				     (eof-error-p t)
				     (eof-value nil)
				     (recursive-p nil))
  (let ((*readtable* *safe-readtable*))
    (read input-stream eof-error-p eof-value recursive-p)))

(defun read-the-unreadable (stream subchar arg)
  (assert (char= subchar #\<))
  (let ((chars '()))
    (loop
      (let ((char (read-char stream t nil t)))
	(when (char= char #\>)
	  (cerror "Return nil."
		  "Read #<~A>, arg=~S." (coerce (nreverse chars) 'string)
		                        arg)
	  (return nil))
	(push char chars)))))

(defun unsafe-macro-char (stream subchar arg)
  (declare (ignore stream arg))
  (error "Attempt to use an unsafe read macro: \"#~A\"." subchar))

(let ((*readtable* *safe-readtable*))
  (set-dispatch-macro-character #\# #\< #'read-the-unreadable)
  (set-dispatch-macro-character #\# #\. #'unsafe-macro-char)
  (set-dispatch-macro-character #\# #\, #'unsafe-macro-char))


;;; Handy file and stream readers

(defun file->list (filename)
  (with-open-file (in filename :direction :input)
    (let ((result '()))
      (loop
        (let ((form (read in nil :eof)))
	  (when (eq form :eof)
	    (return))
	  (push form result)))
      (nreverse result))))

(defun stream->list (stream)
  (with-open-stream (in stream)
    (let ((result '()))
      (loop
        (let ((form (read in nil stream)))
	  ;; Since the stream can't be read from itself, it indicates
	  ;; and end-of-stream.
	  (when (eq form stream)
	    (return))
	  (push form result)))
      (nreverse result))))

(defun stream->lines (stream)
  (with-open-stream (in stream)
    (loop for line = (read-line in nil :eof)
	  until (eq line :eof)
	  collect line)))

(defun string->list (string)
  (stream->list (make-string-input-stream string)))


;;; Null streams: like Unix /dev/null -- swallow all outout and
;;; give eof for any input request.

(defun make-null-input-stream ()
  (make-concatenated-stream))

(defun make-null-output-stream ()
  (make-broadcast-stream))

(defun make-null-io-stream ()
  (make-two-way-stream (make-null-input-stream) (make-null-output-stream)))


;;; Temp-filename returns a pathname defaulted to OPLANTMPDIR.

(defun temp-filename (name)
  (merge-pathnames name
		   (concat-string (get-parameter :oplan-tmp-dir) "/")))


;;; Generate-unique-filename lets us find a name that no one else
;;; is (yet) using.  The prefix will typically contain the directory
;;; and the first part of the file name; the suffix will typically
;;; contain "." followed by the file type.

(defun generate-unique-filename (prefix suffix)
  (check-type prefix string)
  (check-type suffix string)
  (let ((random-state (make-random-state t)))
    (loop
      (let ((tmpname
	     (concat-string
	        prefix
		(prin1-to-string (random 10000 random-state))
		suffix)))
	(unless (probe-file tmpname)
	  (return tmpname))))))


;;; Query utilities

(defun ask-for-line (stream default question &rest format-args)
  (if default
      (format stream "~? [~A] " question format-args default)
      (format stream "~? " question format-args))
  (let ((line (read-line stream)))
    (if (string= line "")
	default
      line)))

(defun ask-if (&rest args)
  (find (char (apply #'ask-for-line args)
	      0)
	"yY"))


;;; How-to-continue message

(defun hit-return-to-continue (stream)
  (format stream "~%Hit <Return> key to continue")
  (read-line stream))


;;; Menu-request for menus that may be too long to fit all on
;;; the screen at once.  This is pretty primitive really.

(defparameter *max-small-menu* 32)

(defparameter *min-menu-chunk* 4)

(defun big-menu-request (xmenu-args
			 &key (read-function #'read)
			      (max-lines *max-small-menu*)
			      (min-chunk *min-menu-chunk*))
  (let* ((heading (cadr xmenu-args))
	 (lines (cddr xmenu-args))
	 (chunks (chunk-menu-list lines max-lines min-chunk)))
    (unless (equal (car xmenu-args) "-heading")
      (error "Big-menu-request needs -heading in ~S." xmenu-args))
    (when (= (length chunks) 1)
      (return-from big-menu-request
	(menu-request xmenu-args :read-function read-function)))
    (loop
      (let ((selection (menu-request
			`("-heading" ,heading
			  "> Forward=:forward-1-chunk"
			  "< Back=:back-1-chunk"
			  "-line"
			  ,@(first chunks))
			:read-function #'read-line)))
	(cond ((equal selection ":forward-1-chunk")
	       (setq chunks (rotate-list chunks 1)))
	      ((equal selection ":back-1-chunk")
	       (setq chunks (rotate-list chunks -1)))
	      (t
	       ;; A real selection
	       (return
		 (funcall read-function
			  (make-string-input-stream selection)))))))))

;;; Chunk-menu-list attempts to redistribute items if the final
;;; chunk would be too small.

;;; /\/: Really should distinguish special items on the end ("change dir",
;;; "QUIT", etc), and make sure they don't end up in a chunk on their
;;; own.  Or perhaps put them at the end of every chunk.  Maybe "forward"
;;; and "backward" should go there too.

(defun chunk-menu-list (items chunk-length min-chunk)
  (assert (>= chunk-length min-chunk))
  (let ((number-items (length items)))
    (if (>= chunk-length number-items)
	(list items)
      (multiple-value-bind (number-chunks remainder)
	  (floor number-items chunk-length)
	(declare (ignore number-chunks))
	(cond ((or (= 0 remainder) (>= remainder min-chunk))
	       (chunk-list items chunk-length))
	      (t
	       (chunk-menu-list items (1+ chunk-length) min-chunk)))))))
    
;;; End
