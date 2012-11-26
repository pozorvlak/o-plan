;;;; File: graphs.lisp
;;; Contains: Graph algorithms
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: Thu Sep 30 15:49:01 1993 by Jeff Dalton
;;; Updated: Wed Oct 15 02:45:42 1997 by Jeff Dalton
;;; Copyright: (c) 1993, 1994 AIAI, University of Edinburgh

;;; Some graphgorithms used by the TF compiler, chiefly for level analysis.

(in-package :oplan-tf-compiler)

;;; This file defines the routines exported from the oplan-graph-util
;;; package -- see graphs-pack.lisp.


;;;; Strongly connected components

;;; Any vertex can be given as a root, so it's ok to use all vertices
;;; rather than only the ones that lack predecessors.

;;; Uses a tconc, rather than push, when gathering the elements
;;; so that the elements of a cycle come out in an order that lets
;;; you follow links by reading L->R, which seems more natural than R->L.

(defun strongly-connected-components (roots get-successors)
  ;; Returns a list of vertex sets, where each set is a list.
  (let* ((vertices (dfs-finish-order roots get-successors))
	 (predecessor-table (transpose-graph vertices get-successors))
	 (marks (make-hash-table :test #'eq))
	 (components '()))
    (macrolet ((predecessors (v) `(gethash ,v predecessor-table))
	       (mark (v) `(gethash ,v marks)))
      (flet ((reachable-from (v)
	       ;; Reachable and not yet marked, that is.
	       (let ((reached (make-tconc)))
		 (label walk ((at v))
		    (ecase (mark at)
		      ((:begin :end))
		      ((nil)
		       (setf (mark at) :begin)
		       (mapc #'walk (predecessors at))
		       (tconc reached at)
		       (setf (mark at) :end))))
		 (tconc-contents reached))))
	;; Collect components using the transposed graph.
	;; The vertices must be taken in dfs-finish-order.
	(dolist (v vertices)
	  (let ((component (reachable-from v)))
	    (when component
	      (push component components))))
	(nreverse components)))))


;;; Build-component-graph, given a directed graph G represented by a
;;; list of root vertices and a successor function, constructs a graph
;;; of the strongly connected components of G.  Three values are
;;; returned:
;;;
;;;   * A list of the SCCs of G.  Each component is a list of vertices.
;;;     Each vertex is in exactly one component.
;;;   * An EQ hash-table mapping each component to a list of its successor
;;;     components.  A successor appears only once in the list, and the
;;;     mapping is acyclic.
;;;   * An EQ hash-table mapping each vertex to the component that
;;;     contains it.
;;;
;;; The successors of a component C are those components C' such that
;;; some member of C has a successor (in terms of the original graph, G)
;;; in C'.
;;;
;;; Note that components are used as EQ-comparable table indexes
;;; even though they are lists.  Copying should therefore be avoided.

(defun build-component-graph (roots get-successors)
  (let* ((components (strongly-connected-components roots get-successors))
	 (vertex-to-component-table (element-to-set-map components))
	 (component-successor-table (make-hash-table :test #'eq))
	 (marks (make-hash-table :test #'eq)))
    (macrolet ((component (v)
		 `(gethash ,v vertex-to-component-table))
	       (mark (component)
		 `(gethash ,component marks))
	       (clear-marks ()
		 `(clrhash marks)))
      (dolist (component components)
	;; /\/: Could use each component as a (unique) mark, rather
	;; than clearing marks.  The marking is just to avoid duplicates.
	(clear-marks)
	(let ((component-successors '())) ;a list of components
	  (setf (mark component) :self)
	  (dolist (v component)
	    (dolist (s (funcall get-successors v))
	      (let ((s-c (component s)))
		(unless (mark s-c)
		  (push s-c component-successors)
		  (setf (mark s-c) :seen)))))
	  (setf (gethash component component-successor-table)
		(nreverse component-successors))))
      ;; The components _could_ be recovered from the table ...
      (values
        components
	component-successor-table
	vertex-to-component-table))))

;;; Given a list of sets, element-to-set-map returns an EQ hash table
;;; that maps each set element to the set that contains it.

(defun element-to-set-map (sets) ; -> hash-table
  (let ((table (make-hash-table :test #'eq)))
    (dolist (set sets)
      (dolist (e set)
	(setf (gethash e table) set)))
    table))


;;; Transpose graph

;;; Returns a table in which vertex-to-successor edges go in the
;;; opposite direction from those in the original graph.

(defun transpose-graph (vertices get-successors) ; -> hash-table
  (let ((table (make-hash-table :test #'eq)))
    ;; The table is a transposed successor map
    (dolist (v vertices)
      (dolist (s (funcall get-successors v))
	;; We have v -> s and want s -> v
	(push v (gethash s table))))
    table))


;;; Transitive closure of R where
;;;
;;;   (R a b) iff (member b (gethash a ht))

(defun tclosure (ht)
  ;; Modifies a copy of ht
  (let ((dom (hash-table-keys ht))
	(ht (copy-eq-hash-table ht)))
    (dolist (i dom)
      (let ((R-i (gethash i ht)))
	(dolist (j dom)
	  (when (not (eq i j))
	    (let ((R-j (gethash j ht)))
	      (when (member i R-j :test #'eq)
		(setf (gethash j ht)
		      (union R-j R-i :test #'eq))))))))
    ht))


;;; Copying an eq hash-table

(defun copy-eq-hash-table (ht)
  (let ((result (make-hash-table :test #'eq)))
    (maphash #'(lambda (k v)
		 (setf (gethash k result) v))
	     ht)
    result))


;;;; Topological sort and related algorithms

;;; The *-functions (e.g. tsort*) are just like the *-less functions
;;; of similar names in the OPLAN-UTIL package but take a list of
;;; start nodes ("roots") rather than a single node.

;;; Topological sort (returning ancestors before descendants).

(defun tsort* (roots children-fn &key (hash-test #'eq)) ; -> list(node)
  (let ((marks (make-hash-table :test hash-test))
	(result '()))
    (macrolet ((mark (node) `(gethash ,node marks)))
      (labels ((walk (at)
		 (ecase (mark at)
		   ((:start)   (error "Cycle involving ~S." at))
		   ((:finish)) ; already processed
		   ((nil)
		    (setf (mark at) :start)
		    (dolist (child (funcall children-fn at))
		      (walk child))
		    (push at result)
		    (setf (mark at) :finish)))))
	(mapc #'walk roots)
	result))))


;;; Vertices in order of decreasing depth-first search finish times.

(defun dfs-finish-order (roots get-successors)
  ;; This is like tree topological sort except that we don't mind loops.
  ;; Consequently, :start and :finish marks are treated alike.
  (let ((marks (make-hash-table :test #'eq))
	(result '()))
    (macrolet ((mark (vertex) `(gethash ,vertex marks)))
      (labels ((walk (at)
		 (unless (mark at)
		   (setf (mark at) :start)
		   (dolist (child (funcall get-successors at))
		     (walk child))
		   (push at result)
		   (setf (mark at) :finish))))
	(mapc #'walk roots)
	result))))


;;; DAG Longest-path-lengths algorithm

;;; Finds distances from the root vertices by finding the number of
;;; steps in the longest path along successor links from a roots without
;;; predecessors to each vertex, where the length of each link is 1.
;;; (The term "root" implies no predecessors, but the topological sort
;;; should straighten things out if a root happens to have some.)

;;; The results are returned in a hash table that maps vertices to
;;; distances.  Vertices must be represented by objects that are
;;; EQL-uniqie.

;;; This is O(v+e) where v is the number of vertices and e the number
;;; of edges (links to successors).

(defun find-longest-path-lengths* (roots get-successors) ; -> hash-table
  (let ((distance-table (make-hash-table :test #'eq))
	(vertex-sequence (tsort* roots get-successors)))
    (dolist (r roots)
      (setf (gethash r distance-table) 0))
    (dolist (v vertex-sequence)
      (let ((dist-to-v (gethash v distance-table)))
	(assert (numberp dist-to-v))
	(dolist (child (funcall get-successors v))
	  (setf (gethash child distance-table)
		(max (gethash child distance-table 0)
		     (1+ dist-to-v))))))
    distance-table))


;;; Has-cycle-p determines whether a directed graph contains a
;;; cycle (hence is not acyclic).

;;; The nodes might just be the "root" (minimal) nodes rather than
;;; all nodes.  (Minimal in that they lack predecessors.)

(defun has-cycle-p (nodes successor-fn &key (hash-test #'eq))
  (let ((marks (make-hash-table :test hash-test)))
    (macrolet ((mark (node) `(gethash ,node marks)))
      (labels
	  ((check (at)
	     (ecase (mark at)
	       ((:start)		; found a cycle
	        t)
	       ((:finish)		; already seen, and if we'd found
		nil)			; a cycle we wouldn't still be here.
	       ((nil)
		(setf (mark at) :start)
		(or (some #'check (funcall successor-fn at))
		    (progn
		      (setf (mark at) :finish)
		      nil))))))
	(some #'check nodes)))))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
