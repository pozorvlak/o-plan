;;;; File: gopher.lsp
;;; Contains: Graphical Operations Processor Handler of Entity Relations.
;;; Author: Jeff Dalton and Richard Kirby (rbk)
;;; Created: Fri Apr 20 10:35:06 1990
;;; Updated: Tue Jun 15 03:02:48 1999 by Jeff Dalton
;;; Copyright: (c) 1992, 1994, 1995, 1996 AIAI, University of Edinburgh

(in-package :oplan-gop :nicknames '(:oplan-gopher))

(use-package :oplan-nodes)

(use-package :oplan-util)

;;; Many of these functions were not used, and some the unused ones
;;; were wrong.  (Be very careful when comparing values from the TPN.)
;;; Since their presence in this file was somewhat misleading, the unused
;;; functions have been deleted and their names commented out of the export
;;; list.  [JWD 27 Mar 93]

(export '(gop-precedes
	  gop-is-linked-before-p
	  ; gop-finishes-before
	  gop-follows
	  gop-is-linked-after-p
	  ; gop-finishes-after
	  ; gop-parallel
	  ; gop-precedes-list
	  ; gop-finishes-before-list
	  ; gop-follows-list
	  ; gop-finishes-after-list
	  ; gop-parallel-list
	  ; gop-last-incoming
	  ; gop-early
	  ; gop-late
	  gop-is-linked-between-p
	  gop-link-looks-ok-p
	  ))

;;;; The relationship functions.

;;; These take two node ends, and return t or nil depending on whether
;;; the relationship holds or not.

;;; /\/: Of course (gop-follows a b) could be defined as (gop-precedes b a)

(defun gop-precedes (query-tag focus-tag)
  "Does the latest time of query come before the earliest time of focus?"
  (let ((query (get-node-end query-tag))
	(focus (get-node-end focus-tag)))
    (or (&< (tpoint-max (ne-time-point query))
	    (tpoint-min (ne-time-point focus)))
	(and ;(not (eq query focus))
	     (fix< (ne-link-distance query) (ne-link-distance focus))
	     (gop-is-linked-before-p query focus)))))


(defun gop-follows (query-tag focus-tag)
  "Does the latest time of focus come before the earliest time of query?"
  (let ((query (get-node-end query-tag))
	(focus (get-node-end focus-tag)))
    (or (&< (tpoint-max (ne-time-point focus))
	    (tpoint-min (ne-time-point query)))
	(and ;(not (eq query focus))
	     (fix> (ne-link-distance query) (ne-link-distance focus))
	     (gop-is-linked-after-p query focus)))))


;;;; Net traversal functions

;;; GOP marks are used for preventing excessive work.

(defvar *gop-marks* (make-array 500 :element-type 'fixnum :initial-element 0))
(defvar *gop-mark* 0) ; a fixnum

(defun get-new-gop-mark ()
  (incf *gop-mark*)
  (when (or (fix= *gop-mark* most-positive-fixnum)
	    (> (node-end-count) (length *gop-marks*)))
    ;; Create a new mark vector if need to clear old marks or need
    ;; a larger array.
    (setq *gop-marks*
	  (make-array (round (* 3/2 (node-end-count)))
		      :element-type 'fixnum :initial-element 0)))
  (when (fix= *gop-mark* most-positive-fixnum)
    (cerror "Start over at 1" "Cor, you've done a lot.")
    (setq *gop-mark* 1))
  *gop-mark*)

;;; /\/: Avoids return-from in the local function, because it loses in AKCL.

(defun gop-is-linked-before-p (query focus)
  ;; N.B. returns false if query and focus are the same.
  (declare (optimize (speed 3) (safety 0)))
  (get-new-gop-mark)
  (let ((query-dist (ne-link-distance query)))
    (declare (type fixnum query-dist))
    (label walk ((consider focus))
      (let ((visited *gop-mark*)
	    (marks *gop-marks*))
	(declare (type fixnum visited)
		 (type (simple-array fixnum (*)) marks))
	(macrolet ((mark (ne) `(aref marks (ne-index ,ne))))
	  (dolist (c (ne-pre-ends consider) nil)
	    (cond ((eq c query)
		   (return t))
		  ((not (= visited (mark c)))
		   (setf (mark c) visited)
		   (when (fix> (ne-link-distance c) query-dist)
		     (when (walk c)
		       (return t)))))))))))

(defun gop-is-linked-after-p (query focus)
  ;; N.B. returns false if query and focus are the same.
  (declare (optimize (speed 3) (safety 0)))
  (get-new-gop-mark)
  (let ((query-dist (ne-link-distance query)))
    (declare (type fixnum query-dist))
    (label walk ((consider focus))
      (let ((visited *gop-mark*)
	    (marks *gop-marks*))
	(declare (type fixnum visited)
		 (type (simple-array fixnum (*)) marks))
	(macrolet ((mark (ne) `(aref marks (ne-index ,ne))))
	  (dolist (c (ne-post-ends consider) nil)
	    (cond ((eq c query)
		   (return t))
		  ((not (= visited (mark c)))
		   (setf (mark c) visited)
		   (when (fix< (ne-link-distance c) query-dist)
		     (when (walk c)
		       (return t)))))))))))


(defun gop-is-linked-between-p (a b c)
  ;; Returns true iff b is linked between a and c.
  ;; a, b, and c are ne structs.
  (and (gop-is-linked-after-p b a)	;b's after a, and
       (gop-is-linked-after-p c b)))	;c's after b.


(defun gop-link-looks-ok-p (from-etag to-etag)
  ;; A quick test for when we don't want to take time to mark, etc.
  (tpn-link-looks-ok-p
    (ne-time-point (get-node-end from-etag))
    (ne-time-point (get-node-end to-etag))))


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
