;;;; File: simple-tpn-support.lisp
;;; Contains: Support code for the simple TPN Web demo
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: 15 January 1995
;;; Updated: Sun Jan  3 19:28:36 1999 by Jeff Dalton
;;; Copyright: (c) 1995, AIAI, University of Edinburgh

(in-package :oplan)

(setq *print-case* :downcase)

;;; The demonstration is "simple" because it uses the TPN directly and
;;; at a low level: it uses time-points and constraints rather than working
;;; with higher-level concepts such as actions.

;;; Contents:
;;;  * Top level
;;;  * Input parser
;;;     - Imports
;;;     - Result structs
;;;     - Readtable
;;;     - Parsing framework
;;;     - Parsing point descriptions
;;;     - Parsing constraint descriptions
;;;     - Other nonterminals
;;;     - Expanding constraint descriptions
;;;  * Net construction
;;;  * Net inspection
;;;  * Net graphing
;;;  * Table construction
;;;     - Determining field widths
;;;     - Initial and final point values
;;;     - Points in context
;;;     - Table utilities
;;;


;;;; Top level

(defparameter *simple-tpn-parameters*
  '((:pt-min      (:int  0)  "default min value for points")
    (:pt-max      (:int* 0)  "default max value for points")
    (:points      (:text  )  "time points")
    (:con-min     (:int  0)  "default min value for constraints")
    (:con-max     (:int* 0)  "default max value for constraints")
    (:constraints (:text  )  "constraints")

    ;; Output requests
    (:show-parsing         (:checkbox)
     "results of parsing the point and constraint arguments")
    (:expanded-constraints (:checkbox)
     "expanded constraints")
    (:tpn-graph            (:checkbox)
     "postscript TPN graph showing constraints as links")
    (:constraint-graph     (:checkbox)
     "postscript TPN graph showing constraints as nodes")
    (:point-table          (:checkbox)
     "table of initial and final point values")
    (:points-in-context    (:checkbox)
     "table of points in context")))


(defun simple-tpn ()
  (with-web-environment "simple-tpn"
    (html-line "<HR>")
    (html-line "<PRE>")			; ----- begin PRE ----->
    (print-oplan-greeting *html-out*)
    (html-line "")
    (parse-query-args)
    (convert-query-args *simple-tpn-parameters*)
    (trim-arg-text :points)
    (trim-arg-text :constraints)

    ;; Parse the point and constraint descriptions, and expand the
    ;; constraints.
    (let* ((points (parse-tpn-points (query-arg :points)))
	   (c-parse (parse-tpn-constraints (query-arg :constraints)))
	   (constraints (expand-constraints c-parse)))

      ;; Show the arguments.
      (show-tpn-arguments points c-parse)

      ;; Show what we got from parsing.
      (when (query-arg :show-parsing)
	(show-tpn-argument-parses points c-parse))

      ;; Show expanded constraints
      (when (query-arg :expanded-constraints)
	(html-format "~%<B>Expanded constraints:</B>~%")
	(print-constraint-table *html-out* constraints))

      ;; Build a TPN.
      (multiple-value-bind (success-p culprit)
	  (construct-tpn points c-parse constraints)
	(if success-p
	    (html-format "~%<B>Success!</B>~%")
	    (html-format
	       "~%<B>Failure:</B> cannot add constraint ~S.~%"
	       culprit))

	;; Drop the constraint that failed and all constraints after it.
	(when culprit
	  (let ((losers (member culprit constraints)))
	    (html-format "~%<B>Dropped constraints:</B>~%")
	    (print-constraint-table *html-out* losers)
	    (setq constraints
		  (ldiff constraints losers))))
	(assert (equal constraints (constraint-list)))
	
	(html-line "</PRE>")		; <----- end PRE -----
	(html-line "<HR>")

	;; Graphs and tables
	(produce-tpn-graphs)
	(produce-tpn-tables)

	;; Closing
	(html-anchor (web-demo-url "simple-tpn-info.html")
	  "Explain the output formats")
	(html-line "<BR>")
	(web-mail-comment-link)
	(if success-p
	    (web-note-success)
	  (web-note-failure (princ-to-string culprit)))))))

(defun trim-arg-text (kwd)
  (setf (query-arg kwd)
	(string-trim '(#\Newline #\Return)
		     (query-arg kwd))))

(defun show-tpn-arguments (points c-parse)

  ;; Defaults
  (html-line "<B>Defaults:</B>")
  (html-line "Points: ~A to ~A."
	     (query-arg :pt-min) (query-arg :pt-max))
  (html-line "Constraints: ~A to ~A."
	     (query-arg :con-min) (query-arg :con-max))

  ;; Points
  (if points
      (html-format "~%<B>Point input:</B>~%~A~&"
	(html-encode-pre-string (query-arg :points)))
    (html-format "~%No explicit points.~%"))

  ;; Constraints
  (if c-parse
      (html-format "~%<B>Constraint input:</B>~%~A~&"
	(html-encode-pre-string (query-arg :constraints)))
    (html-format "~%No constraints.~%")))

(defun show-tpn-argument-parses (points c-parse)

  ;; Points
  (when points
    (html-format "~%<B>Points as parsed:</B>~{~%  ~S~}~%" points))

  ;; Constraint parse trees
  (when c-parse
    (let ((*print-pretty* t))
      (html-xp-format
         "~%<B>Constraints as parsed:</B>~{~%  ~W~}~%" c-parse))))


;;;; Input parser

;;; /\/: Might be better to pay attention to line boundaries,
;;; perhaps by using read-line to take things line-by-line.

(use-package :oplan-parser-kit)

(import
 '(oplan-tf-compiler::define-punctuation-char
   oplan-tf-compiler::define-illegal-char
   oplan-tf-compiler::make-tf-readtable
   oplan-tf-compiler::*line-number*
   oplan-tf-compiler::<time-bounds-pair> ;/\/ use this? or <time-bounds-spec>?
   oplan-tf-compiler::<integer>
   oplan-tf-compiler::<name>
;  oplan-tf-compiler::namep
   oplan-tf-compiler::inf
   oplan-tf-compiler::infinity))


;;; Parser result structs

;;; /\/: You can't change the description formats just by changing these
;;; structs.  There are DESTRUCTURING-BINDs, APPLYs, and LISTs that also
;;; "know" the layout.

(defstruct (pd (:type list)		;point description
	       (:constructor pd (name min max)))
  name					;symbol
  min					;int
  max)					;int or :inf

(defstruct (cd (:type list)		;constraint description
	       (:constructor cd (from to min max)))
  from					;symbol
  to					;symbol
  min					;int
  max)					;int or :inf


;;; Readtable

(defun make-tpn-readtable ()
  ;; Start with a TF readtable, then modify it.
  (let ((*readtable* (make-tf-readtable)))
    (map nil #'define-punctuation-char "<>")
    (define-illegal-char #\?)
    *readtable*))

(defparameter *tpn-readtable* (make-tpn-readtable))


;;; Parsing framework

(defun parse-input (description nt source-string)
  (let ((*syntax-error-reporter*
	 #'(lambda (message &rest format-args)
	     (format t "~&Error in ~A, line ~D: ~?~%"
		     description *line-number* message format-args)))
	(*readtable* *tpn-readtable*)
	(*line-number* 1)
	(in (make-string-input-stream source-string)))
    (multiple-value-bind (result error-count)
	(test-compile
	  nt
	  #'(lambda ()
	      (read in nil *end-token*)))
      (if (> error-count 0)
	  (error "~D errors in ~A." error-count description)
	result))))


;;; Parsing point descriptions

(defun parse-tpn-points (string)
  (parse-input "points" #'<points> string))

(defun <points> ()
  (zero-or-more #'(lambda () (prog1 (<point>) (<optional-comma>)))
		:until *end-token*))

(defun <point> ()
  (let* ((name (<name>))
	 (l (<optional-integer>))
	 (u (when l (<optional-upper-bound>))))
    (cond ((null l)
	   ;; Have neither l nor u.
	   (setq l (query-arg :pt-min)
		 u (query-arg :pt-max)))
	  ((null u)
	   ;; Have l but not u.
	   (setq u l)))
    (list name l u)))


;;; Parsing constraint descriptions

#| Example:

zero 100 end,
a b, b c,
c end,
<p, [<q1 12 q2>, r], <s>>,
[i,j,k]

(:sequence zero 100 100 end)
(:sequence a 10 20 b)
(:sequence b 10 20 c)
(:sequence c 10 20 end)
(:sequence p
    10 20 (:parallel (:sequence q1 12 12 q2) r)
    10 20 (:sequence s))
(:parallel i j k)

|#

(defun parse-tpn-constraints (string)
  (parse-input "constraints" #'<constraints> string))

(defun <constraints> ()
  (zero-or-more #'(lambda () (prog1 (<constraint>) (<optional-comma>)))
		:until *end-token*))

(defun <constraint> ()
  (token-case
    ((|<|) (<sequence-constraint>))
    ((|[|) (<parallel-constraint>))
    (  t   (<simple-constraint>))))

(defun <sequence-constraint> ()
  (cons ':sequence (<seqcon-body>)))

(defun <seqcon-body> ()
  (let ((pt (<constraint-element>)))
    (cond ((numberp (token))
	   ;; Have <constraint-element> <int> ...
	   (let* ((l (<integer>))
		  (u (<optional-upper-bound>)))
	     (list* pt
		    l
		    (or u l)
		    (<seqcon-body>))))
	  ((token-is '|,|)
	   ;; Have <constraint-element>, ...
	   (list* pt
		  (query-arg :con-min)
		  (query-arg :con-max)
		  (<seqcon-body>)))
	  ((token-is '|>|)
	   ;; End of "< ... >" block.
	   (list pt))
	  (t
	   (syntax-error "Unexpected \"~A\" in sequence constraint."
			 (token))))))

(defun <parallel-constraint> ()
  (cons ':parallel
	(one-or-more #'<constraint-element> :separator '|,| :until '|]|)))

(defun <constraint-element> ()
  (token-case
    ((|<|) (<sequence-constraint>))
    ((|[|) (<parallel-constraint>))
    (  t   (<name>))))

(defun <simple-constraint> ()
  (let* ((from (<name>))
	 (l (<optional-integer>))
	 (u (when l (<optional-upper-bound>)))
	 (to (<name>)))
    (cond ((null l)
	   ;; Have neither l nor u.
	   (setq l (query-arg :con-min)
		 u (query-arg :con-max)))
	  ((null u)
	   ;; Have l but not u.
	   (setq u l)))
    (list ':sequence from l u to)))


;;; Other nonterminals

(defun <optional-integer> ()
  (when (numberp (token)) (<integer>)))

(defun <optional-upper-bound> ()
  (let ((bound
	 (token-satisfies
	   #'(lambda (token)
	       (or (integerp token) (eq token 'inf) (eq token '*))))))
    (case bound
      ((inf *) :inf)
      (t bound))))

(defun <optional-comma> ()
  (token-is '|,|))


;;; Expanding constraint descriptions

(defvar *constraints*)

(defun expand-constraints (constraints)
  (let ((*constraints* '()))
    (mapc #'expand-c-elt constraints)
    (nreverse *constraints*)))

(defun expand-c-elt (c-elt)
  (cond ((list-beginning :sequence c-elt)
	 (expand-sequence (cdr c-elt)))
	((list-beginning :parallel c-elt)
	 (mapc #'expand-c-elt (cdr c-elt)))
	(t
	 (assert (symbolp c-elt)))))

(defun expand-sequence (body)
  (let ((at (car body)))
    (expand-c-elt at)
    (when (cdr body)
      (destructuring-bind (l u next &rest tail) (cdr body)
	(dolist (from (constraint-maximals at))
	  (dolist (to (constraint-minimals next))
	    (emit-constraint from to l u)))
	(expand-sequence (cons next tail))))))

(defun emit-constraint (from to l u)
  (push (list from to l u) *constraints*))

(defun constraint-minimals (c-elt)
  (cond ((list-beginning :sequence c-elt)
	 (constraint-minimals (cadr c-elt)))
	((list-beginning :parallel c-elt)
	 (mapcan #'constraint-minimals (cdr c-elt)))
	(t
	 (list c-elt))))

(defun constraint-maximals (c-elt)
  (cond ((list-beginning :sequence c-elt)
	 (constraint-maximals (last-element (cdr c-elt))))
	((list-beginning :parallel c-elt)
	 (mapcan #'constraint-maximals (cdr c-elt)))
	(t
	 (list c-elt))))


;;;; Net construction

;;; This can be confusing if you're not already familiar with the TPN.

;;; The :points and :constraints query-args use names to refer to
;;; time-points.  These names are not part of the TPN, and the TPN
;;; routines know nothing about them.  However, the TPN also uses
;;; (generated) point names which, to avoid confusion, are always
;;; called "tags".  The routine that adds a new point to the TPN,
;;; tpn-add-time-point, returns such a tag.  The TPN also generates
;;; tags for constraints.

;;; The code in this file maintains mappings both ways between names
;;; and tags, and the TPN maintains mappings both ways between tags
;;; and point structs.  By using both sets of mappings, we can provide
;;; the procedures name->point and point->name (see below).

(defvar *all-point-names* '())

(defun all-point-names ()
  (reverse *all-point-names*))

(defvar *initial-point-values* '())

(defun initial-point-values ()
  (reverse *initial-point-values*))

(defvar *constraint-list* '())

(defun constraint-list ()
  (reverse *constraint-list*))

(defvar *name-to-point-tag-table* (make-hash-table :test #'eq))

(defvar *point-tag-to-name-table* (make-hash-table :test #'eq))

(defmacro name->point-tag (name)
  `(gethash ,name *name-to-point-tag-table*))

(defmacro point-tag->name (point-tag)
  `(gethash ,point-tag *point-tag-to-name-table*))

(defun construct-tpn (points c-parse constraints)
  ;; Start from nothing
  (reset-tpn)
  ;; Add explicitly specified points
  (dolist (p points)
    (apply #'load-time-point p))
  ;; Add time points from constraints
  (let ((min (query-arg :pt-min))
	(max (query-arg :pt-max)))
    (walk-tree
      #'(lambda (name)
	  (when (and (symbolp name)
		     (not (keywordp name))
		     (not (name->point-tag name)))
	    (load-time-point name min max)))
      c-parse))
  (assert (equal (list-point-values) (initial-point-values)))
  ;; Add constraints
  (dolist (c constraints)
    (destructuring-bind (from to min max) c
      (let ((ctag (load-constraint from to min max)))
	(if ctag
	    (push c *constraint-list*)
	  (return-from construct-tpn	;constraint can't be added
	    (values nil c))))))		; so fail
  ;; Success
  (values t nil))

(defun load-time-point (name min max)
  (when (name->point-tag name)
    (error "Time point \"~A\" is specified twice." name))
  (push name *all-point-names*)
  (push (list name min max) *initial-point-values*)
  (let ((ptag (tpn-add-time-point min (proper-tpn-max max))))
    (setf (name->point-tag name) ptag)
    (setf (point-tag->name ptag) name)
    ptag))

(defun load-constraint (from to min max)
  (prog1
      (tpn-add-time-constraint
	(name->point-tag from)
	(name->point-tag to)
	min
	(proper-tpn-max max))
    #+:undef
    (format t "~&   ~A ~A..~A ~A ~20t~3@A cycle~:P~%"
	    from min max to *tpn-iterations*)))

(defun proper-tpn-max (max)
  ;; /\/: We use :inf, the TPNM uses *infinity*
  ;; N.B. We need to use a keyword because of the way we get
  ;; time points from constrants.  If we used infinity instead
  ;; of :inf, infinity would appear as a point.
  (cond ((eq max :inf) *infinity*)
	((integerp max) max)
	(t (error "Illegam max: ~S." max))))

(defun reset-tpn ()
  (tpn-clear-tpn))


;;;; Net inspection

(defun name->point (name)
  (tpn-get-tpoint (name->point-tag name)))

(defun point->name (tp-s)
  (point-tag->name (tpoint-tag tp-s)))

(defun list-point-values ()
  (mapcar #'(lambda (name)
	      (let ((tp-s (name->point name)))
		(assert (eq name (point->name tp-s)))
		(list name
		      (tpoint-min tp-s)
		      (proper-max (tpoint-max tp-s)))))
	  (all-point-names)))

(defun name->point-min (name)
  (tpoint-min (name->point name)))

(defun name->point-max (name)
  (proper-max (tpoint-max (name->point name))))

(defun proper-max (max)
  ;; /\/: We want :inf, the TPN uses *infinity*.
  (if (eq max *infinity*) ':inf max))

(defun tpn-minimal-point-names ()
  (remove-if #'(lambda (name)
		 (tpoint-pre-con (name->point name)))
	     (all-point-names)))


;;;; Net graphing

;;; Constraints can be shown either an plain links or as intermediate
;;; nodes.  The constraint nodes can be distinguished from time-point
;;; nodes, because they show only min and max values with no node name.

;;; Psgraph requires a single root node from which all others can be
;;; reached; so if there isn't already a unique minimal node (ie, node
;;; without predecessors), we pretend that there's one called :root
;;; and that it has all the minimal nodes as successors.

(defun produce-tpn-graphs ()

  (when (query-arg :tpn-graph)
    (html-paragraph
     (psgraph-tpn
       (web-tmp-filename "tpn-graph" "ps")
       :show-constraints nil)
     (html-tmp-anchor "tpn-graph" "ps" "PostScript graph")
     (html-line "showing constraints as links")))

  (when (query-arg :constraint-graph)
    (html-paragraph
     (psgraph-tpn
       (web-tmp-filename "constraint-graph" "ps")
       :show-constraints t)
     (html-tmp-anchor "constraint-graph" "ps" "PostScript graph")
     (html-line "showing constraints as intermediate nodes"))))

(defun psgraph-tpn (filename &key (show-constraints nil))
  (let ((minimals (tpn-minimal-point-names)))
    (with-open-file (*standard-output* filename :direction :output)
      (psgraph:psgraph
        (if (length=1 minimals)		;root node, can reach all others
	    (first minimals)
	    :root)
	(if show-constraints		;childf
	    #'tpn-full-psgraph-successors
	    #'tpn-point-psgraph-successors)
	#'tpn-psgraph-info		;infof
	t				;shrink to one page
	nil				;not insert
	#'eq				;test
	nil				;don't remove redundant links
	nil))))				;no title (was "TPN")

(defun tpn-full-psgraph-successors (node)
  (cond ((eq node :root)
	 (tpn-point-psgraph-successors node))
	((symbolp node)
	 ;; The node is a point name
	 (tpoint-post-con (name->point node)))
	(t
	 ;; The node is a constraint struct
	 ;; /\/: Should check the type
	 (list (point->name (tcon-post-point node))))))
  

(defun tpn-point-psgraph-successors (name)
  (if (eq name :root)
      (tpn-minimal-point-names)
    (mapcar #'(lambda (c)
		(point->name (tcon-post-point c)))
	    (tpoint-post-con (name->point name)))))

(defun tpn-psgraph-info (node)
  (cond ((symbolp node) (tpn-point-info node))
	(t (tpn-constraint-info node))))	;/\/ should check type

(defun tpn-point-info (name)
  (if (eq name :root)
      (list "ROOT")
    (let* ((tp-s (name->point name))
	   (min (tpoint-min tp-s))
	   (max (tpoint-max tp-s)))
      (assert (eq name (point-tag->name (tpoint-tag tp-s))))
      (list (string-upcase name)
	    (if (eql min max)
		(format nil "~A" min)
		(format nil "~A ~A" min (proper-max max)))))))

(defun tpn-constraint-info (c-s)
  (let ((min (tcon-min c-s))
	(max (tcon-max c-s)))
    (list
      (if (eql min max)
	  (format nil "~A" min)
	  (format nil "~A ~A" min (proper-max max))))))


;;;; Tables

(defvar *tpn-name-width*)	;width of "name" field
(defvar *tpn-min-width*)	;width of "min" field, not smallest width
(defvar *tpn-max-width*)	;width of "max" field, not largest width

(defun produce-tpn-tables ()
  ;; Get points and constraints
  (let ((initial-points (initial-point-values))
	(constraints (constraint-list)))

    ;; Determine field widths
    (multiple-value-bind (*tpn-name-width* *tpn-min-width* *tpn-max-width*)
	(get-point-table-field-widths initial-points constraints)

      (when (query-arg :point-table)
	(html-tag-line "H3" "Initial and final point values")
	(html-block "PRE"
	  (print-point-table *html-out* initial-points)))

      (when (query-arg :points-in-context)
	(html-tag-line "H3" "Points in context")
	(html-block "PRE"
	  (print-points-in-context *html-out*
				   initial-points constraints))))))


;;; Determining field widths

(defun get-point-table-field-widths (points constraints)
  (let ((name-width
	 (if (null points)
	     5
	   (loop for p in points
		 maximize (length (symbol-name (pd-name p))))))
	(largest-min 0)
	(largest-max 0))
    (dolist (c constraints)
      (setq largest-min (max largest-min (cd-min c))
	    largest-max (max largest-max (max-for-width (cd-max c)))))
    (dolist (p points)
      (setq largest-min (max largest-min (pd-min p))
	    largest-max (max largest-max (max-for-width (pd-max p)))))
    (values
      name-width
      (int-print-length largest-min)
      (int-print-length largest-max))))

(defun max-for-width (m)
  (if (numberp m) m 999))		;width of 999 = 3 = width of "inf"


;;; Table of initial and final point values

;;; There are two tables, printed side-by-side: one table in alphabetical
;;; order and one in order of increasing final min time.

(defun print-point-table (stream initial-points)
  (let ((alpha (make-alphabetic-point-table initial-points))
	(min-t (make-min-time-point-table initial-points)))
    (print-tables-side-by-side
       stream
       '("Alphabetical order" "Increasing final min time")
       alpha
       min-t)))

(defun make-alphabetic-point-table (initial-points)
  (mapcar #'(lambda (p)
	      (format nil "<B>~v@A</B>  ~v@A..~v@A  ~v@A..~v@A"
		      *tpn-name-width* (pd-name p)
		      *tpn-min-width*  (pd-min p)
		      *tpn-max-width*  (pd-max p)
		      *tpn-min-width*  (name->point-min (pd-name p))
		      *tpn-max-width*  (name->point-max (pd-name p))))
	  (sort (copy-list initial-points) #'string-lessp :key #'pd-name)))

(defun make-min-time-point-table (initial-points)
  (mapcar #'(lambda (p)
	      (format nil "<B>~v@A</B>  ~v@A..~v@A"
		      *tpn-name-width* (pd-name p)
		      *tpn-min-width*  (name->point-min (pd-name p))
		      *tpn-max-width*  (name->point-max (pd-name p))))
	  (sort (copy-list initial-points)
		#'(lambda (p1 p2)
		    (let ((m1 (name->point-min (pd-name p1)))
			  (m2 (name->point-min (pd-name p2))))
		      (or (< m1 m2)
			  (and (= m1 m2)
			       (string-lessp (pd-name p1) (pd-name p2)))))))))


;;; Table of points in context

;;; This is similar to a KWIC (Key Words In Context) index.
;;;
;;; It's easiest to explain with an example.  Suppose the constraints
;;; are given by <a,b,[c1,c2,c3],d,[e1,e2],f> with default point range
;;; 0..100 and default constraint range 10..10.  In this table, each
;;; point is shown between its pre and post constraints.  The part of
;;; the table for b looks like this:
;;;
;;;  a= 0.. 50 10.. 10  b=10.. 60 10.. 10 c1=20.. 70
;;;                     b=10.. 60 10.. 10 c2=20.. 70
;;;                     b=10.. 60 10.. 10 c3=20.. 70
;;;
;;; Points are shown as "name=min..max".  "b=10.. 60" shows the
;;; current range for b.  All the "b=10.. 60"s will be in bold,
;;; because in this part of the table it's b that's "in context".
;;;
;;; "a= 0.. 50 10.. 10  b=10.. 60" shows a constraint from a to b.
;;; A's range is 0..50, and the constraint's is 10..10.
;;;
;;; "b=10.. 60 10.. 10 c1=20.. 70" shows a constraint from b to
;;; c1.  C1's range is 20..70, and the constraint's is again 10..10.
;;;
;;; There's no particular relationship between the a -> b constraint
;;; and the b -> c1 constraint.  They're shown on the same line just
;;; to help make the table more compact.
;;;
;;; The aim is to make it fairly easy to see that b's range satisfies
;;; all of the constraints that involve b.  The rest of the table does
;;; the same for the other points.

(defun print-points-in-context (stream initial-points constraints)
  ;; Display points
  (dolist (p (sort (copy-list initial-points) #'string-lessp :key #'pd-name))
    (multiple-value-bind (pre-cons post-cons)
	(get-constraints-for-point constraints (pd-name p))
      (let* ((p-name (pd-name p))
	     (pds (point-descr-string p-name)))
	; (format stream "~&<B>~:(~A~)</B>, initially ~A..~A"
	;         p-name (pd-min p) (pd-max p))
	(if (not (or pre-cons post-cons))
	    (print-point-between-constraints stream p-name pds nil nil)
	  (do ((pre-list  pre-cons  (cdr pre-list))
	       (post-list post-cons (cdr post-list)))
	      ((and (null pre-list) (null post-list)))
	    (print-point-between-constraints
	      stream
	      p-name pds
	      (car pre-list) (car post-list))))
	(format stream "~&~%")))))

(defun get-constraints-for-point (constraints point-name)
  (let ((pre '())
	(post '()))
    (dolist (c constraints)
      (when (eq (cd-from c) point-name)
	(push c post))
      (when (eq (cd-to c) point-name)
	(push c pre)))
    (values
      (sort pre  #'string-lessp :key #'cd-from)
      (sort post #'string-lessp :key #'cd-to))))
  
(defun print-point-between-constraints (stream p-name pds pre post)
  (let ((point-width (point-descr-width))
	(range-width (con-bounds-descr-width)))
    (cond ((and (null pre) (null post))
	   ;; No pre or post constraint
	   (format stream "~&~vT <B>~A</B>"
		   (+ 3 point-width
		      1 range-width)
		   pds))
	  ((null pre)
	   ;; No pre constraint
	   (assert (eq (cd-from post) p-name))
	   (format stream "~&~vT <B>~A</B> ~A ~A"
		   (+ 3 point-width
		      1 range-width)
		   pds
		   (con-bounds-descr-string post)
		   (point-descr-string (cd-to post))))
	  ((null post)
	   ;; No post constraint
	   (assert (eq (cd-to pre) p-name))
	   (format stream "~&   ~A ~A <B>~A</B>"
		   (point-descr-string (cd-from pre))
		   (con-bounds-descr-string pre)
		   pds))
	  (t
	   ;; Both a pre and a post constraint
	   (assert (eq (cd-from post) p-name))
	   (assert (eq (cd-to pre) p-name))
	   (format stream
		   "~&   ~A ~A <B>~A</B> ~A ~A"
		   (point-descr-string (cd-from pre))
		   (con-bounds-descr-string pre)
		   pds
		   (con-bounds-descr-string post)
		   (point-descr-string (cd-to post)))))))

(defun point-descr-string (name)
  (format nil "~v@A=~v@A..~v@A"
	  *tpn-name-width*
	  name
	  *tpn-min-width*
	  (name->point-min name)
	  *tpn-max-width*
	  (name->point-max name)))

(defun point-descr-width ()
  ;; A point description is: name=min..max
  (+ *tpn-name-width* 1 *tpn-min-width* 2 *tpn-max-width*))

(defun con-bounds-descr-string (c)
  (format nil "~v@A..~v@A"
	  *tpn-min-width*
	  (cd-min c)
	  *tpn-max-width*
	  (cd-max c)))

(defun con-bounds-descr-width ()
  ;; A bounds description is: min..max
  (+ *tpn-min-width* 2 *tpn-max-width*))


;;;; Table utilities

;;; Print-tables-side-by-side is a moderately elegant solution to the problem
;;; of printing several tables side-by-side across the page.

(defun print-tables-side-by-side (stream titles &rest tables)
  ;; Each table is a list of strings.  The lists (and the strings)
  ;; can be of different lengths.
  (let ((widths
	 (mapcar #'(lambda (title table)
		     (max (html-print-length title)
			  (or (max-value #'html-print-length table) 0)))
		 titles
		 tables)))
    ;; Print line of titles
    (format stream "   ~{<B>~vA</B>~^    ~}~%~%"
	    (interleave widths titles))
    ;; Print tables
    (label repeat ((line-lists tables))
      (when (notevery #'null line-lists)
	(format stream "   ~{~vA~^    ~}~%"
		(mapcan #'(lambda (width lines)
			    (list width (or (car lines) "")))
			widths
			line-lists))
	(repeat (mapcar #'cdr line-lists))))))

;;; (print-table-from-lists stream prefixes lists)
;;;
;;; Each list represents a row in the table.  All of the lists should be
;;; the same length.  The prefixes are strings, one per column.  The n-th
;;; prefix is printed before entries in the n-th column.  The 1st prefix
;;; therefore serves to indent the table from the left margin, and the
;;; remaining prefixes separate the columns.
;;;
;;; A max width is determined for each column and then used for every
;;; item in the column, with items padded on the left with spaces.
;;; 

(defun print-table-from-lists (stream prefixes lists)
  (let* ((columns (transpose-list lists))
	 (col-widths (mapcar (partial1 #'max-value #'print-length) columns)))
    (dolist (row lists)
      (format stream "~&~{~A~v@A~}~%"
	      (interleave prefixes col-widths row)))))

(defun print-constraint-table (stream constraints)
  (print-table-from-lists
     stream
     '("  " " " ".." " ")
     (mapcar #'(lambda (c) (list (cd-from c) (cd-min c) (cd-max c) (cd-to c)))
	     constraints)))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
