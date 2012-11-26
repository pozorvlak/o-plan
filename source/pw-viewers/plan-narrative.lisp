;;;; File: plan-narrative.lsp
;;; Contains: A way to view a plan as a narrative
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: November 1994
;;; Updated: Wed Oct  8 20:29:31 1997 by Jeff Dalton
;;; Copyright: (c) 1994, AIAI, University of Edinburgh

(in-package :oplan-plan-world-viewer)

;;; Handles viewer-args:
;;;
;;;  :MODE :NARRATIVE
;;;  :LEVELS <:ALL or number>
;;;  :OUTPUT-FILE <file name or :SCREEN>
;;;
;;; /\/: How _should_ we handle screen vs file?
;;;

;;; /\/: Better output
;;;
;;; Determine:
;;;   scale -- how long does the plan take overall?
;;;   pace  -- how much time passes between events?
;;;
;;; Both are :days, :hours, :minutes, or :seconds.
;;;
;;; The output format can be varied accordingly.
;;;
;;; Should pace be min or mean, or should we maybe measure something
;;; else completely?  Min may be most useful.  For instance, if everything's
;;; always on a minute boundary, there's no need to report the seconds.
;;;
;;; What if several events happen at the same time?  Print the time
;;; only once?  Maybe for each day print "Day n~%".
;;;
;;; Should also check whether there are interesting time windows
;;; at all.


;;; The "plan narrative" is meant to be a textual description of the
;;; plan that's relatively easy to follow.  Instead of describing all
;;; the details of nodes and links, it's more like an execution trace.


(defun output-narrative-plan (all-nodes)
  (let* ((nodes (slice-to-level all-nodes))
	 (where (get-narrative-destination)))
    (case where
      (:quit)
      (:screen
       (write-narrative nodes *window*))
      (t
       (with-open-file (stream where :direction :output)
	 (write-narrative nodes stream))))))

(defun get-narrative-destination ()
  (let ((dest (ask-user :output-file "File for plan narrative")))
    (cond ((string= dest "")
	   :quit)
	  ((string= dest ":screen")
	   :screen)
	  (t
	   dest))))

(defvar *narrative-output* nil)

(defun write-narrative (nodes *narrative-output*)
  (n-line "Plan narrative")
  (n-line "")
  (n-line "No special text-generation has been defined for this domain.")
  (n-line "")
  (n-line "The beginning and end of an action are listed separately,")
  (n-line "and labelled \"Begin\" and \"End\" respectively, only when")
  (n-line "they must be separated in time.  Otherwise, the action is")
  (n-line "described in one line and labelled \"Action\".")
  (n-line "")
  ;; /\/: Fairly simple version
  (let ((plan-depth (get-plan-depth nodes)))
    (when (> plan-depth 1)
      (n-line "The first level of the plan")
      (n-line "")
      (write-narrative-for-nodes
        (extract-plan-to-level nodes 1))
      (n-line ""))
    (when (> plan-depth 2)
      (n-line "The first two levels of the plan")
      (n-line "")
      (write-narrative-for-nodes
        (extract-plan-to-level nodes 2))
      (n-line ""))
    (when (> plan-depth 1)
      (n-line "The full plan")
      (n-line ""))
    (write-narrative-for-nodes nodes)))

(defun write-narrative-for-nodes (nodes)
  (let ((graph (make-end-graph-from-nodes nodes)))
    (annotate-graph-for-narrative graph)
    (if nil ; will be: (= 0 (getf (eg-plist graph) :min-end-time))
	(write-narrative-with-deltas graph)
      (write-narrative-with-times graph))))

(defvar *last-reported-narrative-time* -1)

(defun write-narrative-with-times (graph)
  (setq *last-reported-narrative-time* -1)
  (dolist (en (getf (eg-plist graph) :narrative-sequence))
    (report-enode-for-narrative en)))

(defun report-enode-for-narrative (en)
  (case (en-type en)
    ((dummy))
    ((start)
     (when (eq (en-end en) 'begin_of)
       (n-line "Plan execution starts.")))
    ((finish)
     (when (eq (en-end en) 'end_of)
       (n-line "Plan execution finishes.")))
    (t
     (case (getf (en-plist en) 'combined)
       ((begin_of)
	(n-line "~11@A ~@(~A ~A.~)"
		(narrative-time-string (en-min-time en))
		"Action"
		(en-pattern en)))
       ((end_of))
       (t
	(n-line "~11@A ~@(~A ~A.~)"
		(narrative-time-string (en-min-time en))
		(ecase (en-end en)
		  (begin_of " Begin")
		  (end_of   "   End"))
		(en-pattern en)))))))

(defun narrative-time-string (seconds)
  (if (= seconds *last-reported-narrative-time*)
      ""
    (progn
      (setq *last-reported-narrative-time* seconds)
      (seconds->minimal-time-string seconds))))


(defun write-narrative-with-deltas (graph)
  (n-line "Since all time windows are 0..inf, ...")
  (n-line "")
  (let ((d nil))			;last reported delta
    (dolist (en (getf (eg-plist graph) :narrative-sequence))
      (case (en-type en)
	((dummy))
	((start)
	 (when (eq (en-end en) 'begin_of)
	   (n-line "Plan execution starts.")))
	((finish)
	 (when (eq (en-end en) 'end_of)
	   (n-line "Plan execution finishes.")))
	(t
	 (let ((delta (getf (en-plist en) :distance-from-start)))
	   (case (getf (en-plist en) 'combined)
	     ((begin_of)
	      (if (eql delta d)
		  (n-line "~4A ~@(Action ~A.~)" ""    (en-pattern en))
		  (n-line "~4D ~@(Action ~A.~)" delta (en-pattern en)))
	      (setq d delta))
	     ((end_of))
	     (t
	      (n-line "~4@A ~@(~A ~A.~)"
		      (if (eql delta d) "" delta)
		      (ecase (en-end en)
			(begin_of " Begin")
			(end_of   "   End"))
		      (en-pattern en))
	      (setq d delta)))))))))


;;; Annotate-graph-for-narrative looks at the graph and figures out
;;; some things.  It sets the following properties in (eg-plist graph):
;;;   :narrative-sequence
;;;      The result of sorting and peephole-optimizing.
;;;   :min-end-time
;;;      Min-time of the last enode in the narrative-sequence.

(defun annotate-graph-for-narrative (graph)
  (let* ((narrative-sequence
	  (peephole-optimize-for-narrative
	   (tsort-for-narrative graph)))
	 (last-enode
	  (last-element narrative-sequence))
	 (min-end-time
	  (en-min-time last-enode)))
    (assert (eq (en-name last-enode) 'oplan::node-2))
    (setf (getf (eg-plist graph) :narrative-sequence)
	  narrative-sequence)
    (setf (getf (eg-plist graph) :min-end-time)
	  min-end-time)
    (when (= 0 min-end-time)
      (assign-narrative-distances graph))
    graph))


;;; Peephole optimization

;;; If the two ends of an action happen at the same time and are
;;; adjacent in the list, we mark them as combined.

(defun peephole-optimize-for-narrative (enodes)
  (loop for (en . rest) on enodes
	when rest
	do (maybe-combine-for-narrative en (first rest)))
  enodes)

(defun maybe-combine-for-narrative (e f)
  (when (and (eq (en-end e) 'begin_of)
	     (en-pair-p e f)
	     (= (en-min-time e) (en-min-time f)))
    (setf (getf (en-plist e) 'combined) 'begin_of
	  (getf (en-plist f) 'combined) 'end_of)))


;;; Sorting the enodes

;;; We want a topological sort with each enode appearing before its
;;; successors in the result, but we also want to take time windows
;;; (not just explicit links) into account.  So we start by putting
;;; the enodes in reverse order by min-time (so that those with
;;; later/larger min-times are earlier in the list).  The topological
;;; sort then visits enodes in this same reverse order.

;;; The topological sort visits an enode E by visiting all its (linked)
;;; successors then pushing E onto the front of the result.  Starting with
;;; the late enodes gets them into the result before early enodes (unless 
;;; links disagree, which they'd better not).  Since we're building the
;;; result using push, the early enodes (which go in last) appear first
;;; in the final result, before all enodes either linked after them or
;;; with greater min-times, as desired.  Stable-sort helps keep results
;;; the same in all Common Lisps.  Reverse-copying the enodes before
;;; sorting helps us get something more like the original, before-we-did-
;;; anything order when the tsort is done.  (If there are no links,
;;; this tsort is very like reverse.)

(defun tsort-for-narrative (eg) ; -> list of enodes
  (tsort-enodes
    eg
    (stable-sort (reverse-copy-vector (eg-nodes eg))
		 #'>
		 :key #'en-min-time)))

(defun tsort-enodes (eg nodes-in-order)
  (let ((start-mark (list :start))	;new, unique object
	(finish-mark (list :finish))	;new, unique object
	(result '()))
    (labels ((visit (at)
	       (cond ((eq (en-mark at) finish-mark)
		      ;; already visited
		      )
		     ((eq (en-mark at) start-mark)
		      (error "Cycle involving ~S." at))
		     (t
		      (setf (en-mark at) start-mark)
		      (dolist (s-i (en-suc at))
			(let ((s (egref eg s-i)))
			  (assert (>= (en-min-time s) (en-min-time at)))
			  (visit s)))
		      (push at result)
		      (setf (en-mark at) finish-mark)))))
      (do-vector-elements (en nodes-in-order)
	(visit en))
      result)))


;;; Distance (number of links) to each enode

;;; Sets the :distance-from-start property in (en-plist enode).

(defun assign-narrative-distances (graph)
  (let ((narrative-sequence (getf (eg-plist graph) :narrative-sequence)))
    (macrolet ((dist (en) `(getf (en-plist ,en) :distance-from-start 0)))
      (dolist (en narrative-sequence)
	(let ((d (if (eq (en-type en) 'dummy) (dist en) (1+ (dist en)))))
	  (dolist (suc (en-suc en))
	    (let ((s (egref graph suc)))
	      (setf (dist s) (max (dist s) d))))))))
  graph)


;;; Useful functions (/\/ eventually move to the util package)

(defun reverse-copy-vector (v)
  (let* ((l-v (length v))
	 (r (make-array l-v)))
    (loop for v-i from (1- l-v) downto 0
	  as r-i from 0
	  do (setf (svref r r-i) (svref v v-i)))
    r))

(defun last-vector-element (v)
  (declare (type vector v))
  (aref v (1- (length v))))


;;; Output utilities

(defun n-format (format-string &rest format-args)
  (apply #'format *narrative-output* format-string format-args))

(defun n-line (format-string &rest format-args)
  (n-format "~&~?~%" format-string format-args))


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
