;;; -*- Mode: Lisp; Package: PSGRAPH -*-
;;;
;;; ****************************************************************
;;; PostScript DAG Grapher *****************************************
;;; ****************************************************************
;;; Written by Joseph Bates, CMU CSD, March 1988. jbates+@cs.cmu.edu
;;;
;;; The PSGrapher is a set of Lisp routines that can be called to produce
;;; PostScript commands that display a directed acyclic graph.
;;;
;;; Modifications History:
;;;
;;; JULY 90   mkant   Fixed shrink mode so that it scales x and y
;;;                   axes uniformly (e.g., aspect ratio = 1).
;;; MAY 90    Chiles  Made exported specials have stars.
;;; APR 90    Skef    Now lives in PSGRAPH package (instead of USER,
;;;                   or *package*...) with user-tweakable stuff exported.
;;;                   Node equivalence function can now be specified as
;;;                   EQ, EQUAL, or EQUALP.
;;; DEC 88    Bates   Modified to include optional insert parameter to psgraph.
;;; MAR 88    Bates   File created.
;;;
;;;
;;; Modification history at AIAI, University of Edinburgh:
;;;
;;; Jul 92    RBK     Extraneous link removal option added.  Title option
;;;                   added.  Changes of tests to various hash tables for
;;;                   efficiency.
;;; May 93    JDalton Bit-vector Warshall's algorithm to compute the
;;;                   ancestor/descendent relation.
;;; Oct 94    JDalton When printing 1-page, force landscape mode
;;;                   "unless inserting into a document", as RBK says.
;;;                   Richard Tobin figured out that what was needed
;;;                   was: "%%Orientation: Landscape".
;;; Jan 95    JDalton Fixed vertical centering when shrinking to 1 page
;;;                   landscape.  Now puts the mean y val in the middle
;;;                   of the page.  Before, it put 0 in mid-page.
;;; Feb 99    JDalton Converted to defpackage
;;; 

(defpackage :psgraph
  (:export #:*max-psnodes* #:*extra-x-spacing* #:*extra-y-spacing* 
	   #:*fontname* #:*fontsize* #:*second-fontname* #:*second-fontsize*
	   #:*title-fontname* #:*title-fontsize* 
	   #:*boxgray* #:*boxkind* #:*boxradius* #:*boxedge* #:*chunksize*
	   #:*edgewidth* #:*edgegray* #:*edgecap*
	   #:*textgray* #:*pageheight* #:*pagewidth*
	   #:psgraph
	   #:bit-relation		;[jd]
	   #:bit-transitive-closure))	;[jd]

(cl:in-package :psgraph)

(defstruct psnode
  name
  info
  children
  parents
  appears-in-top-sort
  children-appear-in-top-sort
  yvalue
  height
  )

(defvar *max-psnodes* 5000)
(defvar *extra-x-spacing* 20)
(defvar *extra-y-spacing* 40)
(defvar *fontname* "Helvetica")
(defvar *fontsize* 6)
(defvar *second-fontname* "Helvetica")
(defvar *second-fontsize* 6)
(defvar *title-fontname* "Helvetica")
(defvar *title-fontsize* 10)
(defvar *boxgray* ".2")      ;; 0 is black, 1 is white
(defvar *boxkind* "stroke")  ;; stroke for outline, fill for solid
(defvar *edgewidth* ".5")    ;; .5 is thin lines, 1 is fairly thick
(defvar *edgegray* ".2")     ;; dark, but not solid black, lines
(defvar *edgecap* "1")       ;; round the line caps
(defvar *textgray* "0")      ;; solid black text
(defvar *pageheight* 720)
(defvar *pagewidth* (+ (* 7 72) 36))
(defvar *boxradius* (floor *fontsize* 2))
(defvar *boxedge* (floor *fontsize* 4))
(defvar *chunksize* 400)

(defvar num-psnodes)
(defvar narray)
(defvar psnode-index)
(defvar child-function)
(defvar info-function)
(defvar top-sort)
(defvar maximum-y)
(defvar minimum-y)
(defvar rows)
(defvar y-offset)
(defvar psnodes-at-y)
; The ancestor-cache is no longer used [jd]
;   (defvar ancestor-cache)
; instead we have:
(defvar descendent-table)

(defun psgraph (root childf infof
		&optional (shrink nil) (insert nil) (test #'equal)
                          (remove-extraneous-links-p t) (title nil))
  ;; RBK 20/7/92 Added eql to list of acceptable test functions.
  (unless (member test
		  (list 'eq 'eql 'equal 'equalp #'eq #'eql #'equal #'equalp))
    (error "Test must be a function suitable to hand to Make-Hash-Table."))
  (when insert (setf shrink t))
  (setq narray (make-array *max-psnodes*))
  (setq num-psnodes 0)
  (setq psnode-index (make-hash-table :test test
				      :size 500
				      :rehash-size 2.0))
  (setq child-function childf)
  (setq info-function infof)
  ;; RBK 20/7/92 ancestor-cache hash table used the test passed in, but this
  ;; will not work for eq since a new list is used for looking up in the cache
  ;; each time.
  ; (setq ancestor-cache (make-hash-table :test #'equal
  ;                                       :size 1000
  ;                                       :rehash-size 2.0))
  
  ;;; walk the graph computing node info
  (walk-graph root)
  (dotimes (index num-psnodes)
    (setf (psnode-parents (aref narray index))
	  (nreverse (psnode-parents (aref narray index))))
    (setf (psnode-children (aref narray index))
	  (nreverse (psnode-children (aref narray index)))))

  ;; Compute descendent as the transitive closure of child. [jd]
  (compute-descendent-table)

  ;; RBK 20/7/92 Remove extraneous links if option given.
  (if remove-extraneous-links-p
      (prune-extraneous-links))
  
  ;;; topological sort the graph
  (setq top-sort nil)
  (top-sort-node 0)
  (setq top-sort (nreverse top-sort))
  
  ;;; declare this as a PostScript file
  (format t "%!PS-Adobe-1.0~%")

  ;;; Tell the viewer to use landscape orientation.  This works at least
  ;;; with Ghostview.  Richard Tobin determined what to write.  [J.Dalton]
  (when (and shrink (not insert))
    (format t "%%Orientation: Landscape~%"))
  
  ;;; this is required for the Apple LaserWriter (it likes to know the page
  ;;; ordering so that it can print pages upside down).  It is best not to
  ;;; confuse the LaserWriter when inserting things into a other documents.
  (when (not insert)
    (format t "%%Page: ? ?~%"))
  
  ;;; define global functions
  (dolist (line '(
		  
		  "/max {2 copy lt {exch} if pop} def"
		  
		  "/min {2 copy gt {exch} if pop} def"
		  
		  "/inch {72 mul} def"
		  
		  "/drawbox"
		  " {/height exch def"
		  "  /width exch def"
		  "  /y exch def"
		  "  /x exch def"
		  "  gsave newpath"
		  "  x y boxradius add moveto"
		  "  x y height add x width add y height add boxradius arcto"
		  "      pop pop pop pop"
		  "  x width add y height add x width add y boxradius arcto"
		  "      pop pop pop pop"
		  "  x width add y x y boxradius arcto pop pop pop pop"
		  "  x y x y height add boxradius arcto pop pop pop pop"
		  "  boxgray setgray boxkind grestore"
		  " } def"
		  
		  "/printposter"
		  " {/rows exch def"
		  "  /columns exch def"
		  "  /bigpictureproc exch def"
		  "  newpath"
		  "    leftmargin botmargin moveto"
		  "    0 pageheight rlineto"
		  "    pagewidth 0 rlineto"
		  "    0 pageheight neg rlineto"
		  "  closepath clip"
		  "  leftmargin botmargin translate"
		  "  0 1 rows 1 sub"
		  "   {/rowcount exch def"
		  "    0 1 columns 1 sub"
		  "     {/colcount exch def"
		  "      gsave"
		  "       pagewidth colcount mul neg"
		  "       pageheight rowcount mul neg"
		  "       translate"
		  "       bigpictureproc"
		  "       gsave showpage grestore"
		  "      grestore"
		  "     } for"
		  "   } for"
		  " } def"
		  
		  )) (format t "~A~%" line))
  
  ;;; declare arrays
  (format t "/xarray ~D array def~%" num-psnodes)
  (format t "/widtharray ~D array def~%" num-psnodes)
  
  ;;; define global settings
  (format t "/leftmargin 36 def~%")
  (format t "/botmargin 36 def~%")
  (format t "/pagewidth ~D def~%" *pagewidth*)
  (format t "/pageheight ~D def~%" *pageheight*)
  (format t "/boxradius ~D def ~%" *boxradius*)
  (format t "/boxedge ~D def ~%" *boxedge*)
  (format t "/boxgray ~A def ~%" *boxgray*)
  (format t "/boxkind {~A} def~%" *boxkind*)
  
  ;;; compute width and height of each node
  (format t "/~A findfont ~D scalefont setfont~%" *fontname* *fontsize*)
  (dotimes (index num-psnodes)
    (format t "widtharray ~D (~A) stringwidth pop put~%"
	    index
	    (car (psnode-info (aref narray index)))))
  (format t "/~A findfont ~D scalefont setfont~%"
	  *second-fontname* *second-fontsize*)
  (dotimes (index num-psnodes)
    (format t "widtharray ~D get~%" index)
    (dolist (info (cdr (psnode-info (aref narray index))))
      (format t "(~A) stringwidth pop max~%" info))
    (format t "~D add widtharray exch ~D exch put~%"
 	    (* 2 *boxedge*)
	    index))
  (dotimes (index num-psnodes)
    (setf (psnode-height (aref narray index))
	  (+ (* 2 *boxedge*)
	     *extra-y-spacing*
	     *fontsize*
	     (* (- (length (psnode-info (aref narray index))) 1)
		*second-fontsize*))))
  
  ;;; compute x location of each node
  (format t "xarray 0 0 put~%")
  (dolist (index (cdr top-sort))
    (let ((parents (psnode-parents (aref narray index))))
      (format t "xarray ~D get widtharray ~D get add~%"
	      (car parents) (car parents))
      (dolist (parent (cdr parents))
	(format t "xarray ~D get widtharray ~D get add max~%"
		parent parent))
      (format t "~D add xarray exch ~D exch put~%" *extra-x-spacing* index)))
  
  ;;; compute maximum x used
  (format t "/maximum-x 0~%")
  (dotimes (index num-psnodes)
    (format t "xarray ~D get widtharray ~D get add max~%"
	    index index))
  (format t "def~%")
  
  ;;; compute y location of each node and maximum and minimum y used
  (setq maximum-y 0)
  (setq minimum-y 0)
  ;; RBK 20/7/92 Changed test to eql since this is a hash table keyed on
  ;; integers.
  (setq psnodes-at-y (make-hash-table :test #'eql
				      :size (* num-psnodes *fontsize*)
				      :rehash-size 2.0))
  (let ((currenty 0))
    (dolist (index (reverse top-sort))
      (let (desired-y)
	(cond ((null (psnode-children (aref narray index)))
	       (setf desired-y currenty)
	       (setf currenty (+ currenty (psnode-height
					   (aref narray index)))))
	      (t
	       (let ((children (psnode-children (aref narray index)))
		     (ysum 0))
		 (dolist (child children)
		   (setf ysum (+ ysum (psnode-yvalue (aref narray child)))))
		 (setq desired-y (floor ysum (length children))))))
	
	 ;;; We may not be able to put the node at the desired y.
	 ;;; If there is another node that overlaps in the y direction
	 ;;; and that node is neither a parent* nor child* (hence its x
	 ;;; location may overlap this one) then we have to choose
         ;;; another y value -- we choose the nearest (up or down)
	 ;;; location so that there is no possible overlap in y or x.
	(let ((height (psnode-height (aref narray index)))
	      ;; RBK 20/7/92 Changed related to an array of num-psnodes in size
	      (related (make-array num-psnodes))
	      collision
	      upward-bot
	      upward-top
	      downward-bot
	      downward-top)
	  (setq upward-bot desired-y)
	  (setq upward-top upward-bot)
	  (setq downward-top (- (+ desired-y height) 1))
	  (setq downward-bot downward-top)
	  (loop
	      ;;; check upward-top for collision
	   (setq collision nil)
	   (dolist (n (gethash upward-top psnodes-at-y))
	     (let ((r (aref related n)))
	       (when (null r)
		 (setf r (related-classes n index))
		 (setf (aref related n) r))
	       (when (eql r 'no)
		 (setq collision t)
		 (return))))
	   
	     ;;; if no collision and big enough space then we win
	   (incf upward-top)
	   (when (and (not collision) (= (- upward-top upward-bot) height))
	     (setf desired-y upward-bot)
	     (return))
	   
	   (when collision
	     (setf upward-bot upward-top))
	   
	     ;;; check downward-bot for collision
	   (setq collision nil)
	   (dolist (n (gethash downward-bot psnodes-at-y))
	     (let ((r (aref related n)))
	       (when (null r)
		 (setf r (related-classes n index))
		 (setf (aref related n) r))
	       (when (eql r 'no)
		 (setq collision t)
		 (return))))
	   
	      ;;; if no collision and big enough space then we win
	   (decf downward-bot)
	   (when (and (not collision) (= (- downward-top downward-bot) height))
	     (setf desired-y (+ 1 downward-bot))
	     (return))
	   
	   (when collision
	     (setf downward-top downward-bot))
	   )
	   ;;; add our name to psnodes-at-y table
	  (dotimes (i height)
	    (push index (gethash (+ i desired-y) psnodes-at-y)))
	  
	  (setf (psnode-yvalue (aref narray index)) desired-y)
	  )
	
	(setf minimum-y (min minimum-y (psnode-yvalue (aref narray index))))
	(setf maximum-y
	      (max maximum-y (+ (psnode-yvalue (aref narray index))
				(psnode-height (aref narray index))))))))
  
  ;;; compute y-offset to center graph vertically
  (setq rows (ceiling (- maximum-y minimum-y) *pageheight*))
  (setq y-offset (- (floor (- (* rows *pageheight*) (- maximum-y minimum-y)) 2)
		    minimum-y))
  (when shrink (setq y-offset 0))
  
  ;;; create dictionary big enough to hold all the
  ;;; procedures defined below and make it a current dictionary
  (format t "~D dict begin~%"
	  (+ (* 4 (+ num-psnodes (ceiling num-psnodes *chunksize*))) 50))
  
  ;;; define procedures to display the background box for each node
  (dotimes (index num-psnodes)
    (format t "/box~D {~%" index)
    (format t "xarray ~D get ~D widtharray ~D get ~D drawbox~%"
	    index
	    (+ y-offset 
	       (psnode-yvalue (aref narray index))
	       (floor *extra-y-spacing* 2))
	    index
	    (- (psnode-height (aref narray index)) *extra-y-spacing*))
    (format t "} def~%"))
  
  ;;; define procedures to display the text info for each node
  (dotimes (index num-psnodes)
    (let ((yvalue (+ y-offset
		     (floor *extra-y-spacing* 2)
		     (floor *fontsize* 5)
		     *boxedge*
		     (psnode-yvalue (aref narray index))
		     (* *second-fontsize*
			(- (length (psnode-info (aref narray index))) 1)))))
      (format t "/text~D {xarray ~D get boxedge add ~D moveto (~A) show} def~%"
	      index
	      index
	      yvalue
	      (car (psnode-info (aref narray index))))
      (when (not (null (cdr (psnode-info (aref narray index)))))
	(format t "/secondtext~D {~%" index)
	(dolist (info (cdr (psnode-info (aref narray index))))
	  (setq yvalue (- yvalue *second-fontsize*))
	  (format t "xarray ~D get boxedge add ~D moveto (~A) show~%"
		  index
		  yvalue
		  info))
	(format t "} def~%"))))
  
  ;;; define procedures to display the edges leading into each node
  (dotimes (index num-psnodes)
    (format t "/edge~D {newpath~%" index)
    (dolist (parent (psnode-parents (aref narray index)))
      (format t "xarray ~D get widtharray ~D get add ~D moveto~%"
	      parent
	      parent
	      (+ (psnode-yvalue (aref narray parent))
		 (floor (psnode-height (aref narray parent)) 2)
		 y-offset))
      (format t "xarray ~D get ~D lineto~%"
	      index
	      (+ (psnode-yvalue (aref narray index))
		 (floor (psnode-height (aref narray index)) 2)
		 y-offset)))
    (format t "stroke } def~%"))
  
  ;;; Define procedures to display chunks of boxes, text, and edges.
  ;;; We limit each chunk to at most *chunksize* calls, to avoid overflowing
  ;;; the Postscript operand stack.
  (dotimes (index num-psnodes)
    (when (eql (mod index *chunksize*) 0)
      (format t "/boxchunk~D {~%" (floor index *chunksize*)))
    (format t "box~D~%" index)
    (when (or (eql (mod index *chunksize*) (- *chunksize* 1))
	      (eql index (- num-psnodes 1)))
      (format t "} def~%")))
  (dotimes (index num-psnodes)
    (when (eql (mod index *chunksize*) 0)
      (format t "/textchunk~D {~%" (floor index *chunksize*)))
    (format t "text~D~%" index)
    (when (or (eql (mod index *chunksize*) (- *chunksize* 1))
	      (eql index (- num-psnodes 1)))
      (format t "} def~%")))
  (dotimes (index num-psnodes)
    (when (eql (mod index *chunksize*) 0)
      (format t "/secondtextchunk~D {~%" (floor index *chunksize*)))
    (when (not (null (cdr (psnode-info (aref narray index)))))
      (format t "secondtext~D~%" index))
    (when (or (eql (mod index *chunksize*) (- *chunksize* 1))
	      (eql index (- num-psnodes 1)))
      (format t "} def~%")))
  (dotimes (index num-psnodes)
    (when (eql (mod index *chunksize*) 0)
      (format t "/edgechunk~D {~%" (floor index *chunksize*)))
    (format t "edge~D~%" index)
    (when (or (eql (mod index *chunksize*) (- *chunksize* 1))
	      (eql index (- num-psnodes 1)))
      (format t "} def~%")))
  
  ;;; Define procedure to display entire graph.
  ;;; First do the boxes, then the edges, then the text.
  (format t "/drawgraph { gsave~%")
  (dotimes (i (ceiling num-psnodes *chunksize*))
    (format t "boxchunk~D~%" i))
  (format t "~A setlinewidth~%" *edgewidth*)
  (format t "~A setlinecap~%" *edgecap*)
  (format t "~A setgray~%" *edgegray*)
  (dotimes (i (ceiling num-psnodes *chunksize*))
    (format t "edgechunk~D~%" i))
  (format t "~A setgray~%" *textgray*)
  (format t "/~A findfont ~D scalefont setfont~%" *fontname* *fontsize*)
  (dotimes (i (ceiling num-psnodes *chunksize*))
    (format t "textchunk~D~%" i))
  (format t "/~A findfont ~D scalefont setfont~%"
	  *second-fontname* *second-fontsize*)
  (dotimes (i (ceiling num-psnodes *chunksize*))
    (format t "secondtextchunk~D~%" i))
  (format t "grestore } def~%")
  
  ;;; show the virtual page in as many actual pages as needed
  (when title
    (format t "(~A) stringwidth~%" title)
    (format t "/title-height exch def~%")
    (format t "/title-width exch def~%"))
  ;; Stuff out the maximum and minimum y used.
  (format t "/maximum-y ~D def~%" maximum-y)
  (format t "/minimum-y ~D def~%" minimum-y)
  ;; RBK 20/7/92 Changes to shrink output to produce a landscape plan, unless
  ;; inserting into a document.
  (cond (shrink
	 ;; shrink the output to fit on one page
	 (if insert
	     (progn
	       (format t "leftmargin minimum-y neg botmargin add translate~%")
	       (when title
		 (format t "gsave~%")
		 (format t "/~A findfont ~D scalefont setfont~%"
			 *title-fontname* *title-fontsize*)
		 (format t "pagewidth title-width sub 2 div ~
                            minimum-y title-height sub moveto~%")
		 (format t "(~A) show~%" title)
		 (format t "grestore~%"))
	       (format t "pagewidth dup maximum-x max div ~
                          pageheight dup maximum-y minimum-y sub max div ~
                          min dup scale~%")
	       (format t "drawgraph end~%"))
	     (progn
	       ;; Fix so that it scales x and y to the same ratio. 
	       (format t "/scalefactor pageheight dup maximum-x max div ~
                          pagewidth dup maximum-y minimum-y sub max div ~
                          min def~%")
	       ;; Now rotate 90 degrees to landscape the graph, translate onto
	       ;; the centre of the page and scale to fit.
	       (format t "90 rotate~%")
	       (when title
		 (format t "gsave~%")
		 (format t "/~A findfont ~D scalefont setfont~%"
			 *title-fontname* *title-fontsize*)
		 (format t "pageheight title-width sub 2 div ~
                            leftmargin title-height add neg moveto~%")
		 (format t "(~A) show~%" title)
		 (format t "grestore~%"))
	       (format t "/scaled-mean-y maximum-y minimum-y add 2 div ~
                           scalefactor mul def~%~
                          botmargin pagewidth 2 div scaled-mean-y add ~
                          leftmargin add neg translate~%")
	       #+:undef
	       (format t "botmargin pagewidth 2 div leftmargin add neg ~
                          translate~%")
	       (format t "scalefactor dup scale~%")
	       (format t "drawgraph showpage end~%"))))
	(t
	 (format t "{drawgraph}~%")
	 (when title
	   (format t "gsave~%")
	   (format t "/~A findfont ~D scalefont setfont~%"
		   *title-fontname* *title-fontsize*)
	   (format t "pagewidth title-width sub 2 div~%")
	   (format t "botmargin moveto~%")
	   (format t "(~A) show~%" title)
	   (format t "grestore~%"))
	 (format t "maximum-x pagewidth div ceiling ~D printposter end~%"
		 rows)))
  
  )


(defun walk-graph (root)
  (when (eql num-psnodes *max-psnodes*)
    (error "More than ~D nodes in graph.  Graphing aborted."))
  (let ((root-index num-psnodes)
	(child-names (apply child-function (list root))))
    (incf num-psnodes)
    (setf (gethash root psnode-index) root-index)
    (setf (aref narray root-index) (make-psnode))
    (setf (psnode-name (aref narray root-index)) root)
    (setf (psnode-info (aref narray root-index))
	  (apply info-function (list root)))
    (setf (psnode-children (aref narray root-index)) nil)
    (setf (psnode-parents (aref narray root-index)) nil)
    (setf (psnode-appears-in-top-sort (aref narray root-index)) nil)
    (setf (psnode-children-appear-in-top-sort (aref narray root-index)) nil)
    (dolist (child child-names)
      (let ((child-index (gethash child psnode-index)))
	(cond (child-index
	       (push child-index
		     (psnode-children (aref narray root-index)))
	       (push root-index
		     (psnode-parents (aref narray child-index))))
	      (t
	       (let ((child-index (walk-graph child)))
		 (push child-index
		       (psnode-children (aref narray root-index)))
		 (push root-index
		       (psnode-parents (aref narray child-index))))))))
    root-index)
  )

;;; J. Dalton, 31 May 1995: Changed the topological sort to detect
;;; circularities.

(defmacro top-sort-mark (index)
  `(psnode-appears-in-top-sort (aref narray ,index)))

(defun top-sort-node (index)
  (ecase (top-sort-mark index)

    ((nil)				; Never been here before.

     (setf (top-sort-mark index) :start)

     ;; Make sure the parents are processed
     (dolist (parent (psnode-parents (aref narray index)))
       (top-sort-parent parent))
    
     ;; add this node to top-sort
     (push index top-sort)
     (setf (top-sort-mark index) :finish))

    ((:start)				; Circularity
     (error "psgraph circularity at ~A =~%~S" index (aref narray index))
     )

    ((:finish)				; Done it already
     ))

  ;; Visit children
  (when (not (psnode-children-appear-in-top-sort (aref narray index)))
    (dolist (child (psnode-children (aref narray index)))
      (top-sort-node child))
    (setf (psnode-children-appear-in-top-sort (aref narray index)) t))

  )

(defun top-sort-parent (index)
  (ecase (top-sort-mark index)
    ((nil)

     ;; make sure the parents are processed
     (setf (top-sort-mark index) :start)
     (dolist (parent (psnode-parents (aref narray index)))
       (top-sort-parent parent))

     ;; add this node to top-sort
     (push index top-sort)
     (setf (top-sort-mark index) :finish))

    ((:start)
     (error "psgraph circularity at ~A =~%~S" index (aref narray index)))

    ((:finish)))

  )

(defun related-classes (x y)
  (cond ((ancestor x y) 'yes)
	((ancestor y x) 'yes)
	(t 'no))
  )

(defun ancestor (x y)
  (descendent x y))

;;; The old version on ancestor is still interesting... but not used [jd]
#|
(defun ancestor (x y)
  (let ((cached-value (gethash (list x y) ancestor-cache)))
    (cond (cached-value (car cached-value))
	  (t
	   (setq cached-value
		 (cond ((equal x y) t)
		       (t
			(some #'(lambda (child) (ancestor child y))
			      (psnode-children (aref narray x))))))
	   (setf (gethash (list x y) ancestor-cache) (list cached-value))
	   cached-value)))
  )
|#

;;;; RBK 20/7/92 Remove extraneous links.

;;; JD 15/2/93 Added visited check.  Now much faster in some cases.
;;; JD 21/5/93 Converted to bit-vector Warshall's algorithm.

;;; Remove links between parant and child when there is another, longer
;;; path from the parent to the child.

;;; Although we delete some parent<->child links, we do this only when
;;; there's another, less direct, path between parent and child; so the
;;; ancestor and descendent sets should not be altered.

(defun prune-extraneous-links ()
  ;; Step through narray, and for each child, see if there is another
  ;; route to the child.  If there is, delete the parent/child links
  ;; for that child.
  (dotimes (n num-psnodes)
    (let* ((node (aref narray n))
	   (children (copy-list (psnode-children node))))
      (dolist (child children)
	(when (another-path-to-p n child)
	  (setf (psnode-children node)
		(delete child (psnode-children node)))
	  (setf (psnode-parents (aref narray child))
		(delete n (psnode-parents (aref narray child)))))))))

(defun another-path-to-p (parent this-child)
  ;; Is a path along the psnode-children links from parent to this-child
  ;; other than the direct link from parent to this-child.  (If there is,
  ;; prune-extraneous-links will delete the direct link.)
  (some #'(lambda (other-child)
	    (and (not (eql other-child this-child))
		 (descendent other-child this-child)))
	(psnode-children (aref narray parent))))

#| ; obsolete version
(defun another-path-to-p (from to)
  (let ((consider-list (remove to (psnode-children (aref narray from))))
	(visited '())
	consider)
    (loop
      (when (null consider-list) (return nil))
      (setq consider (pop consider-list))
      (when (eql consider to) (return t))
      (unless (member consider visited)
	(setq consider-list
	      (append (psnode-children (aref narray consider))
		      consider-list))
	(push consider visited)))))
|#

;;; The old version on descendent is still interesting... but not used [jd]
#|
(defun descendent (from to)
  ;; Is "to" a descendent of "from"?
  ;; This is essentially identical to the ancestor routine above,
  ;; but it's used only for the extraneous-links check.  Note that
  ;; a node is considered a descendent of itself.
  (let ((from->to (list from to)))
    (car
      (or (gethash from->to descendent-cache)
	  (setf (gethash from->to descendent-cache)
		(list
		  (or (equal from to)
		      (some #'(lambda (child)
				(descendent child to))
			    (psnode-children (aref narray from)))))))))
  )
|#

;;;; Compute descendent as the transitive closure of child. [jd]

;;; /\/: The descendent and ancestor functions are the same.  They
;;; even take args in the same order.  This is surely wrong!

(defun descendent (from to)
  ;; Is "to" a descendent of "from"?
  ;; A node is considered a descendent of itself.
  (= 1 (sbit (svref descendent-table from) to)))

(defun compute-descendent-table ()
  (setq descendent-table (make-descendent-table)))

(defun make-descendent-table ()
  (bit-transitive-closure
    (bit-relation
      #'(lambda (i) (cons i (psnode-children (aref narray i))))
      num-psnodes)))

;;; (Bit-relation f n) returns an n-element vector of n-element
;;; bit-vectors to represent a relation R between small integers
;;; >= 0 and < n.  We start with the first line of equivalence
;;; below and establish the second line by constructing the
;;; required bit-vectors.
;;;
;;;   (R a b) iff (member b (funcall f a))
;;;           iff (= 1 (sbit (svref R a) b))
;;;

(defun bit-relation (f n)
  (let ((R (make-array n)))
    (check-type R simple-vector)
    (dotimes (i n)
      (let ((bits (make-array n :element-type 'bit :initial-element 0)))
	(check-type bits simple-bit-vector)
	(setf (svref R i) bits)
	(dolist (j (funcall f i))
	  (setf (sbit bits j) 1))))
    R))

;;; Bit-transitive-closure: a bit-vector version of Warshall's algorithm.

;;; A relation R between small, positive integers >= 0 and < n is
;;; represented by an n x n square matrix, stored as a simple vector
;;; of simple bit-vectors, such that
;;;    (R a b) iff (= 1 (sbit (svref R a) b)).
;;; We compute the transitive closure of R via destructive
;;; modification of the bit-vectors.  The basic algorithm is:
;;;
;;;   For i from 0 to n-1
;;;     For j from 0 to n-1 such that j/=i
;;;       If (R j i), bit-ior (R i) into (R j)
;;;

(defun bit-transitive-closure (R)
  (declare (type simple-vector R))
  (assert (typep (length R) 'fixnum))
  (let ((n (length R)))
    (declare (fixnum n))
    (dotimes (i n)
      (declare (fixnum i))
      (let ((R-i (svref R i)))
	(declare (type simple-bit-vector R-i))
	(assert (= (length R-i) n))
	;; Expand i to (R i) whenever i is in (R j).
	(dotimes (j n)
	  (declare (fixnum j))
	    (when (not (= i j))
	      (let ((R-j (svref R j)))
		(declare (type simple-vector R-j))
		;; If (R j i), or (R i) into (R j)
		(when (= 1 (sbit R-j i))
		  (bit-ior R-j R-i t))))))))
  R)

;;; End
