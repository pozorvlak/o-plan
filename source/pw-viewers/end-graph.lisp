;;;; File: end-graph.lsp
;;; Contains: Operations on graphs of node-ends
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: November 1994
;;; Updated: Tue Jul  9 01:19:01 1996 by Jeff Dalton
;;; Copyright: (c) 1994, AIAI, University of Edinburgh

(in-package :oplan-plan-world-viewer)

;;; Viewers have normally worked with a list of node descriptions, but
;;; it's often easier to work with node ends.  So here we provide a way
;;; to convert a list of node-descriptions to a graph of node ends; 
;;; and then we provide some useful operations on such "end-graphs",
;;; operations such as deleting dummy nodes and deleting redundant
;;; links.

;;; Naming conventions:
;;;   egraph, eg -- a [node-]end graph, ie an end-graph struct.
;;;   enode, en -- a node in such a graph, ie an eg-node struct.

;;; /\/: Maybe adopt eg-* as a naming convention for graph ops?
;;; For instance, eg-make-graph-from-nodes, eg-lookup-i.

;;; /\/: Maybe should drop the use of indices and make successor
;;; lists contain structs directly.  enodes could also contain
;;; a pointer back the the egraph that contains them.  A :print-
;;; function would be required, of course.  Actually, indices
;;; might still be needed for transitive closure.

;;; /\/: Use pre- and post- rather than pre- and suc- ?


;;;; [Node-]End graphs

(defstruct (end-graph (:conc-name eg-))
  start			;index of start node (the only minimal node)
  nodes			;vector of eg-node structs
  suc*			;bit-vector transitive-closure of the en-suc relation
  plist)		;anything else we think of

(defstruct (eg-node (:conc-name en-))	;eg-node or end-node?
  name			;e.g. node-1
  end			;begin_of or end_of
  type			;node type, e.g. action, dummy
  pattern		;a string, e.g. "(puton a b)"
  index			;index in the eg-nodes vector of the-graph
  other-end		;index of the other end of the same node
  pre			;indices of predecessor ends
  suc			;indices of successor ends
  min-time		;earliest time what the end represents can occur
  max-time		;latest time what the end represents can occur
  (gone-p nil)		;true if node was deleted
  mark			;for marking during graph traversal
  (plist nil)		;plists are cool
  )

(defun en-number (en)
  (subseq (symbol-name (en-name en)) 5)) ;e.g "NODE-3-2" --> "3-2"

(defun-inline egref (eg i)
  (svref (eg-nodes eg) (the fixnum i)))

(defun-inline eg-index->node (eg i)
  (egref eg i))

(defun eg-start-node (eg)
  (egref eg (eg-start eg)))

(defmacro do-egraph-nodes ((en-var eg-form) &body body)
  `(do-vector-elements (,en-var (eg-nodes ,eg-form))
     ,@body))


;;;; Constructing a [node-]end-graph from a list of node-descriptions.

(defun make-end-graph-from-nodes (nodes)
  (let* ((index (make-node-index nodes))
	 (eg-nodes (make-array (* 2 (hash-table-count index)))))
    (loop for n in nodes
	  as i from 0 by 2
	  do (setf (svref eg-nodes i) (make-begin-eg-node index i n)
		   (svref eg-nodes (1+ i)) (make-end-eg-node index i n)))
    (check-end-graph
      nodes
      index
      (make-end-graph
        :start 0		;we know begin_of node-1 will be 1st
        :nodes eg-nodes))))

;;; Make-node-index returns an EQ hash-table that maps a node-name
;;; to a cons (i . nd) where nd is a node-description struct (see
;;; viewer-services.lsp) and i is the index of nd in the list of nodes.

(defun make-node-index (nodes) ; -> hash-table
  (let ((table (make-hash-table :test #'eq)))
    (loop for n in nodes
	  as i from 0
	  do (setf (gethash (nd-node-name n) table)
		   (cons i n)))
    table))

(defun lookup-nd (index node-name)
  (cdr (gethash node-name index)))

(defun lookup-i (index node-name)
  (car (gethash node-name index)))

(defun lookup-eg-i (index node-name end)
  ;; This is the index of the corresponding eg-node in the eg-nodes
  ;; vector.  Remember that there are 2 eg-nodes per node.
  (let ((i (* 2 (lookup-i index node-name))))
    (ecase end
      (begin_of i)
      (end_of (1+ i)))))

;;; Make-begin-eg-node and make-end-eg-node make eg-nodes from
;;; node-description structs, using information about the begin_of
;;; and end_of the node, respectively.  In both cases, i is the
;;; index of the begin ed-node.  Index is a hash-table built by
;;; make-node-index.

(defun make-begin-eg-node (index i n)
  (assert (= i (* 2 (lookup-i index (nd-node-name n)))))
  (make-eg-node
    :name (nd-node-name n)
    :end 'begin_of
    :type (nd-node-type n)
    :pattern (nd-pattern n)
    :index i
    :other-end (1+ i)
    :pre (make-eg-i-list index (nd-begin-pre n))
    :suc (make-eg-i-list index (nd-begin-suc n))
    :min-time (nd-est n)
    :max-time (nd-lst n)))

(defun make-end-eg-node (index i n)
  (assert (= i (* 2 (lookup-i index (nd-node-name n)))))
  (make-eg-node
    :name (nd-node-name n)
    :end 'end_of
    :type (nd-node-type n)
    :pattern (nd-pattern n)
    :index (1+ i)
    :other-end i
    :pre (make-eg-i-list index (nd-end-pre n))
    :suc (make-eg-i-list index (nd-end-suc n))
    :min-time (nd-eft n)
    :max-time (nd-lft n)))

;;; Make-eg-i-list takes a list ((end node-name)...) and returns
;;; a list of the corresponding eg-node vector indices.

(defun make-eg-i-list (index end-tag-list)
  (mapcar #'(lambda (end-tag)
	      (lookup-eg-i index (ne-tag-node end-tag) (ne-tag-end end-tag)))
	  end-tag-list))

;;; Check-end-graph makes some consistency checks

(defun check-end-graph (nodes index eg) ; -> eg
  (declare (ignore nodes))
  (let ((eg-nodes (eg-nodes eg)))
    ;; Check start node
    (let ((en0 (svref eg-nodes (eg-start eg))))
      (assert (fix= (en-index en0) 0))
      (assert (eq (en-name en0) 'node-1))
      (assert (eq (en-end  en0) 'begin_of))
      (assert (eq (en-type en0) 'start))
      (assert (null (en-pre en0))))
    ;; Check nodes
    (fix-dotimes (i (length eg-nodes))
      (let ((en (svref eg-nodes i)))
	(assert (fix= i (en-index en)))
	(assert (eq (en-name en) (en-name (svref eg-nodes (en-other-end en)))))
	(assert (fix= i (lookup-eg-i index (en-name en) (en-end en))))))
    ;; Return the graph
    eg))


;;;; Adding and removing links

;;; From and to are eg-nodes, not indices.

(defun en-linked-p (from to)
  (fix-member (en-index to) (en-suc from)))

(defun en-add-link (from to)
  (unless (en-linked-p from to)
    (push (en-index from) (en-pre to))
    (push (en-index to)   (en-suc from))))

(defun en-delete-link (from to)
  (setf (en-suc from) (fix-delete (en-index to)   (en-suc from)))
  (setf (en-pre to)   (fix-delete (en-index from) (en-pre to))))


;;;; Removing nodes

;;; When deleting a node, we add equivalent links between its
;;; predecessors and successors.  Note that we do that first
;;; because deleting links is destructive.

;;; We assume that nodes are never predecessors or successors
;;; of themselves (and indeed that the graph is acyclic).

(defun eg-delete-enode (egraph enode)
  ;; Link every predecessor of enode before every successor of enode.
  (dolist (pre (en-pre enode))
    (let ((p (egref egraph pre)))
      (dolist (suc (en-suc enode))
	(let ((s (egref egraph suc)))
	  (en-add-link p s)))))
  ;; Now snip enode's own links to predecessors and successors.
  (dolist (pre (en-pre enode))
    (en-delete-link (egref egraph pre) enode))
  (dolist (suc (en-suc enode))
    (en-delete-link enode (egref egraph suc)))
  ;; Make enode as deleted.
  (setf (en-gone-p enode) t))


;;;; Transitive closure of the enode successor (en-suc) relation

;;; See support/psgraph.lsp for an explanation of the bit- routines.

(defun eg-ensure-suc* (eg)
  (unless (eg-suc* eg)
    (setf (eg-suc* eg) (eg-make-path-table eg))))

(defun eg-path-p (eg from to)
  ;; Determines whether there is a path from "from" to "to".
  (= 1 (sbit (svref (eg-suc* eg) (en-index from)) (en-index to))))

(defun eg-make-path-table (eg)
  ;; N.B. there's a path from a node to itself.
  (let ((enodes (eg-nodes eg)))
    (psgraph:bit-transitive-closure
      (psgraph:bit-relation
        #'(lambda (i) (cons i (en-suc (svref enodes i))))
	(length enodes)))))


;;;; Redundant link deletion

;;; Remove links between an enode and an immediate successor when there
;;; is another, longer, path between them.

(defun eg-delete-extra-links (eg)
  (eg-ensure-suc* eg)
  (let ((change-p nil))
    (do-egraph-nodes (en eg)
      (when (eg-delete-extra-links-from-enode eg en)
	(setq change-p t)))
    change-p))

(defun eg-delete-extra-links-from-enode (eg en)
  ;; For each direct successor ds of en, see if there is another route
  ;; to ds from en.  If there is, delete the link from en to ds.
  (let ((change-p nil))
    (dolist (ds-i (copy-list (en-suc en)))
      (let ((ds (egref eg ds-i)))
	(when (another-path-p eg en ds)
	  (en-delete-link en ds)
	  (setq change-p t))))
    change-p))

(defun another-path-p (eg en ds)
  (loop for other-s-i in (en-suc en)
	for other-s = (egref eg other-s-i)
	thereis (and (not (eq other-s ds))
		     (eg-path-p eg other-s ds))))


;;;; Dummy node deletion

;;; /\/: A simple version that deletes all dummies.  A Dummy that
;;; is both a split and a join ought to be kept, because the graph
;;; requires fewer links that way.  ...  Ok, we'll try it:

(defun eg-delete-dummies (eg)
  (let ((change-p nil))
    (do-egraph-nodes (en eg)
      (when (and (eq (en-type en) 'dummy)
		 (not (and (eg-split-p eg en)
			   (eg-join-p eg en))))
	(eg-delete-enode eg en)
	(setq change-p t)))
    change-p))

(defun eg-split-p (eg en)
  (declare (ignore eg))
  (length>1 (en-suc en)))

(defun eg-join-p (eg en)
  (declare (ignore eg))
  (length>1 (en-pre en)))

#+:undef
(defun eg-split-p (eg en)
  (> (loop for suc in (en-suc en)
	   count (not (eq 'dummy (en-type (egref eg suc)))))
     1))

#+:undef
(defun eg-join-p (eg en)
  (> (loop for pre in (en-pre en)
	   count (not (eq 'dummy (en-type (egref eg pre)))))
     1))  


;;;; Other operations

(defun en-pair-p (en1 en2)
  (= (en-index en1) (en-other-end en2)))


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
