;;;; File: tpn.lisp
;;; Contains: Time Point Network datastructures and related functions.
;;; Authors: Jeff Dalton <J.Dalton@ed.ac.uk> and Richard Kirby (rbk)
;;; Created: September 1992
;;; Updated: Wed Jan  6 04:22:26 1999 by Jeff Dalton
;;; Copyright: (c) 1993, 1994, AIAI, University of Edinburgh

(in-package :oplan-nodes)

(use-package :oplan-developerlib)
(use-package :oplan-ctxt)

;;; Prefixes for external names: tpn-, tpoint-, tcon-.

(export '(;; Time points
	  tpoint-tag tpoint-min tpoint-max tpoint-pre-con tpoint-post-con

	  tpn-get-tpoint
	  tpn-add-time-point
	  tpn-make-tpoint-at-zero
	  *tpn-tpoint-at-zero*

	  ;; Constraints
	  tcon-tag tcon-node-tag tcon-pre-point tcon-post-point tcon-min
	  tcon-max

	  tpn-get-tcon
	  tpn-add-time-constraint

	  tpn-link-looks-ok-p

	  ;; Etc.
	  tpn-clear-tpn
	  *tpn-iterations*))


;;; Size estimates:

;;; There will be two time points per node (one per node-end), plus
;;; any other time points that exist.

;;; There will be at least 3 constraints per (typical) node N: 1 from
;;; the begin_of N to the end_of N, 1 from some other node-end to the
;;; begin_of N, and 1 from the end_of N to some node-end after it:
;;;
;;;     ... ---> begin_of N ---> end_of N ---> ...
;;;
;;; However, the constraint from the end_of one node is presumably the
;;; same as the constraint to the begin_of some other node, so we
;;; don't want to count it twice.  This gives us an estimate of
;;; min 2 x (number of nodes).


;;; The valid TPN operations are:
;;;  - Clear the TPN.
;;;  - Add a point.
;;;  - Add a constraint.

;;; In addition, point and constraint structs can be retrieved
;;; and examined.

;;; N.B. Other operations, such as changing the min or max of a point,
;;; are not yet supported and will not work correctly.  Routines for
;;; _deleting_ points or constraints from the net should not be called
;;; from outside the TPN manager.  It is not safe to delete a point or
;;; constraint without undoing all of its effects on the points remaining
;;; in the TPN.

;;; A tpn-info struct is used to hold information about the TPN that
;;; is not held elsewhere.  It can be (and is) context-layered.

(defstruct (tpn-info (:conc-name tpn-))
  (i-number-of-points 0))

(define-context-accessor tpn-number-of-points tpn-i-number-of-points)

(defvar *time-net* (make-tpn-info))

(defmacro tpn-N-points ()
  '(tpn-number-of-points *time-net*))

;;; For some applications, it is useful to have a time point that is fixed
;;; at time zero.  One can be created by calling tpn-make-tpoint-at-zero
;;; and is then available as the value of *tpn-tpoint-at-zero*.  Note that
;;; tpn-clear-tpn does not automatically create such a point, but it does
;;; reset the value of *tpn-tpoint-at-zero* to something that is not a
;;; time point (because the old zero point probably contained references
;;; to points and constraints that have ceased to be valid -- indeed it
;;; may be that no storage is freed until this happens, because the
;;; zero point may directly or indirectly refer to all other points).

(defvar *tpn-tpoint-at-zero* 'not-a-tpoint)

;;; (tpn-clear-tpn)
;;;
;;; tpn-clear-tpn resets the tpn, removing all points and constraints.
;;;
;;; Its main purpose is to reset the number of points to zero, because
;;; it's used as a limit on the number of iterations by the propagation
;;; algorithm.  If we ever need a list of all current points (eg, to
;;; recalculate the whole net), that would have to be maintained and
;;; reset as well.
;;;
;;; When context layering is actively used, it may not be necesssary
;;; to call tpn-clear-tpn, because the equivalent of starting over from
;;; scratch is to start a new context from the root, and the values in
;;; the root context should be the right ones (ie, no points exist).

(defun tpn-clear-tpn ()
  ;; /\/: Making a new one means we can't switch back to the old values
  ;; by switching contexts, but the same applies to clearing hash tables,
  ;; so what the heck.
  (setq *tpn-tpoint-at-zero* 'again-not-a-tpoint)
  (setq *time-net* (make-tpn-info)))


;;;; Time Point data structure, and associated functions.

;;; /\/: For many years, the time point and constraint structs contained
;;; tags (symbols) that could be used to refer indirectly to the structs.
;;; Since the mapping from tags to structs did not change (except after
;;; the TPN was reset), it was seldom necessary to use these tags; and
;;; in later versions of O-Plan (probably since 2.1), the tags were not
;;; used in any interesting way.  They were therefore eliminated, to
;;; save some space and time, in January 1999.  However, the functions
;;; that go from tags to structs, and back, have been retained for
;;; compatibility and to make it easier to restore the tags if that
;;; should be necessary.  But those functions are now identity functions.

(defstruct (tpn-time-point (:conc-name tpoint-)
			   (:print-function print-hashed))

  "A point in time. The fields are:
    tpoint-tag - identifier of this point. [eliminated]
    tpoint-min - Minimum time.
    tpoint-max - Maximum time.
    tpoint-pre-con - Pre-constraints list.
    tpoint-post-con - Post-constraints list.
    mark - indicates change."

  ; tag
  i-min			; +-- internal accessors for context-layering
  i-max			; |
  i-pre-con		; |
  i-post-con		; +--
  (mark -1 :type fixnum))

#+akcl
(eval-when (compile)
  (si::freeze-defstruct 'tpn-time-point))

(defun tpoint-tag (tp) tp)

(define-context-accessor tpoint-min tpoint-i-min)
(define-context-accessor tpoint-max tpoint-i-max)
(define-context-accessor tpoint-pre-con tpoint-i-pre-con)
(define-context-accessor tpoint-post-con tpoint-i-post-con)

(defun tpn-get-tpoint (tp-tag)
  tp-tag)

(defun tpn-add-time-point (min max) ; -> tpn-time-point
  (assert (and (numberp min) (or (%infp max) (<= min max))))
  (let ((tp (make-tpn-time-point)))
    (setf (tpoint-min tp) min)
    (setf (tpoint-max tp) max)
    (incf (tpn-N-points))		;for the iteration limit
    tp))

(defun tpn-make-tpoint-at-zero ()
  (setq *tpn-tpoint-at-zero*
	(tpn-add-time-point 0 0)))


;;;; Time Constraint data structure, and associated functions.

(defstruct (tpn-time-constraint (:conc-name tcon-))

  "A time constraint.
    tcon-tag - the identifier of this structure. [eliminated]
    tcon-pre-point - tpn-time-point structure at start of the constraint.
    tcon-post-point - tpn-time-point structure at end of the constraint.
    tcon-min - min value of this constraint.
    tcon-max - max value of this constraint."

  ; tag
  pre-point
  post-point
  i-min
  i-max)

#+akcl
(eval-when (compile)
  (si::freeze-defstruct 'tpn-time-constraint))

(defun tcon-tag (tc) tc)

(define-context-accessor tcon-min tcon-i-min)
(define-context-accessor tcon-max tcon-i-max)

(defun tpn-get-tcon (c-tag)
  c-tag)

;;; tpn-add-time-constraint

(defun tpn-add-time-constraint (pre post min max)

  "Creates a tpn-time-constraint structure and adds this constraint to
   the pre and post constraint lists of the pre and post time points.
   Lastly, a call to tpn-propagate-constraints-after-adding-constraint
   is made to update the TPN.

   Arguments:
     pre - tpn-time-point structure at start.
     post - tpn-time-point structure at end.
     min - min value of constraint.
     max - max value of constraint.
   Returns:
     the constraint, or nil if the constraint leads to an inconsistency
     in the Time Point Network."

  (assert (and (numberp min) (or (%infp max) (<= min max))))

  (let ((c (make-tpn-time-constraint :pre-point pre :post-point post)))
    (setf (tcon-min c) min)
    (setf (tcon-max c) max)

    ;; Also update the time-point constraints lists.
    (push c (tpoint-post-con pre))
    (push c (tpoint-pre-con post))
    ;; Now propagate any changes.
    (if (tpn-propagate-constraints-after-adding-constraint c)
	c
      (progn
	;; Bad constraint, so undo the addition and return nil.
	(tpn-delete-time-constraint c)
	nil))))

(defun tpn-delete-time-constraint (c)
  ;; Removes the constraint from its pre- and post-points.
  (deletef c (tpoint-post-con (tcon-pre-point c)))
  (deletef c (tpoint-pre-con (tcon-post-point c))))


;;; TPN-propagate-constraints-after-adding-constraint

;;; This is called when a constraint is added.

#+:undef
(defun tpn-propagate-constraints-after-adding-constraint (constraint)
  "Returns: t or nil."
  (update-time-windows
    (list (tcon-pre-point constraint)
	  (tcon-post-point constraint))))

(defun tpn-propagate-constraints-after-adding-constraint (constraint)
  "Returns: t or nil."
  (let ((pre (tcon-pre-point constraint))
	(post (tcon-post-point constraint))
	(cmin (tcon-min constraint))
	(cmax (tcon-max constraint)))
    ;; If nothing will change, we can just return true now.
    (implies
      (or (let ((pre-min (tpoint-min pre))
		(post-min (tpoint-min post)))
	    (or (> (+ pre-min cmin) post-min)	      ;post min pushed up?
		(and (not (%infp cmax))
		     (> (- post-min cmax) pre-min))   ;pre min pulled up?
		(let ((pre-max (tpoint-max pre))
		      (post-max (tpoint-max post)))
		  (or (&< (&- post-max cmin) pre-max) ;pre max pushed down?
		      (&< (&+ pre-max cmax) post-max) ;post max pulled down?
		      )))))
      (update-time-windows (list pre post)))))


;;; TPN-propagate-constraints-after-constraining-pt

;;; This is called after constraining a point.

;;; /\/: We need a procedure that will undo the change to the point
;;; and check that it is a valid change (ie, increase a min or decrease
;;; a max).  So this routine is fairly useless in itself.

(defun tpn-propagate-constraints-after-constraining-pt (tp)
  (update-time-windows (list tp)))



;;; (Update-time-windows initial-points) -> true (success) or false (failure)

;;; The initial points are all the points that have been changed to
;;; increase their min or decrease their max, plus the end points of
;;; all the constraints that have been added, since the last call to
;;; update-time-windows.  Update-time-windows propagates any changes
;;; to point min and max values required by the changed points and
;;; added constraints.  If the net is still consistent, a true value
;;; is returned; otherwise, update-time-windows undoes all the changes
;;; it made and returns false.

;;; Note that it is up to the *caller* of update-time-windows to
;;; undo any changes made between calls to update-time-windows.

;;; Update-time-windows uses the algorithm of AIAI-TR-6 modified for
;;; a net of points instead of a net of nodes.  The start, end, and
;;; duration of a node are here represented respectively by two points
;;; and a constraint between them; but that is at the ADS level and
;;; has no direct impact on the algorithm.

;;; The algorithm is essentially a longest path algorithm for a graph
;;; that is related to the time-point net as follows:

;;; There is a virtual point-0 that is before all other points.
;;; The min-limit of a point is an arc of that length from point-0,
;;; and the max-limit is an arc of negated length back to point-0.

;;; A constraint, c, between two points is a forward arc of length
;;; (tcon-min c) from (tcon-pre-point c) to (tcon-post-point c), and a
;;; backward arc of length (- (tcon-max c)) from (tcon-post-point c)
;;; to (tcon-pre-point c).

;;; The calculation of min values for points is done by finding longest
;;; paths from the virtual point-0 which has [min,max] = [0,0].  The length
;;; of the longest path to a point becomes the min of that point.  This
;;; length is determined by an iterative algorithm that increments the
;;; recorded distance to a point whenever it is less that the distance
;;; to a predecessor plus the length of the arc from the predecessor.
;;; The algorithm terminates when no more increments are necessary or
;;; when it has been determined that no solution is possible.  The details
;;; are explained in AIAI-TR-6.

;;; In effect, the forward arcs (of positive length) from pred to succ
;;; "push up" the distance to succ if it would otherwise be less than
;;; the minimum allowed between the points.

;;; The backward arcs of negative length from pred to succ "pull up"
;;; the succ point if the distance from pred to succ would otherwise
;;; exceed the maximum.  (Note that in this case the succ point of
;;; the constraint arc is earlier in time than the pred.)

;;; Note that distance values are changed only if they will increase.

;;; The calculation of max values is similar.  See below.

;;; /\/: We could be faster by eliminating the tag -> struct mapping
;;;      at least within the TPN.  (E.g., record the point structs
;;;      instead of the tags as the pre- and post-points of a constraint.

;;; /\/: The special variables *change-history* and *changed-points*
;;;      can be eliminated by moving the call of undo-tpn-changes into
;;;      find-min-values and by making constrain-min and constrain-max
;;;      into local procedures.  The local procedures could be declared
;;;      inline.

(defvar *tpn-mark* 0)
(defvar *tpn-base-mark* 0)

(defvar *change-history*)
(defvar *changed-points*)

(defvar *tpn-iterations* 0)		;in case anyone wants to know

(defun update-time-windows (active-points)
  (let ((*change-history* '()))
    (if (find-min-values active-points)
	(if (find-max-values active-points)
	    ;; Successfully updated the net
	    t
	  ;; This error should never happen.
	  (error "Can't find max values."))
      ;; The net has inconsistent constraints and is not invalid.
      ;; We have to undo all our changes and put it back in its
      ;; earlier, valid state.
      (progn
	(undo-tpn-changes *change-history*)
	nil))))

(defun find-min-values (initial-active-points) ; -> success?
  (let ((active-points initial-active-points) 	;check this iteration
	(*changed-points* '())			;check next iteration
	(N (tpn-N-points))			;number of points
	(iterations 0))				;number of iterations completed
    (setq *tpn-base-mark* *tpn-mark*)
    ;; Repeatedly process constraint arcs from the active points to
    ;; get a new list of active points.  The min of a point is the
    ;; length of the longest path (so far) from an imaginary point-0
    ;; at 0.  The max values of constraints and points are interpreted
    ;; as backward arcs of negated length.
    (loop (unless (and active-points (<= iterations N))	;allow N+1 iterations
	    (return))
	  ;; Each iteration needs a new change-mark.
	  (new-tpn-mark)
	  ;; Process all arcs by visiting all points, collecting
	  ;; a list of *changed-points* as we go.  The changed points
	  ;; will become the active points for the next iteration.
	  (dolist (tp-s active-points)
	    ;; Process arc from tp back to the imaginary point-0.
	    ;; This would make the distance to point-0 be > 0 when  [< 0? /\/]
	    ;; the min of the point is > the max.  So in that case
	    ;; we exit, indicating failure.
	    (unless (or (%infp (tpoint-max tp-s))
			(>= (tpoint-max tp-s) (tpoint-min tp-s)))
	      (return-from find-min-values nil))
	    ;; Process forward arcs to successors.  This may push up
	    ;; the min of some successors.
	    (dolist (constraint-s (tpoint-post-con tp-s))
	      (assert (eq tp-s (tcon-pre-point constraint-s)))
	      (constrain-min tp-s
			     (tcon-post-point constraint-s)
			     (tcon-min constraint-s)))
	    ;; Process backward arcs to predecessors, negating length.
	    ;; This may pull up the min of some predecessors.
	    (dolist (constraint-s (tpoint-pre-con tp-s))
	      (assert (eq tp-s (tcon-post-point constraint-s)))
	      (unless (%infp (tcon-max constraint-s))
		(constrain-min tp-s
			       (tcon-pre-point constraint-s)
			       (- (tcon-max constraint-s))))))
	  (shiftf active-points *changed-points* '())
	  (incf iterations))
    ;(format t "~&N nodes = ~S, iterations ~S ~S.~%"
    ;        (tpn-N-points) iterations (null active-points))
    (setq *tpn-iterations* iterations)
    ;; Converged if no points changed on the last iteration.
    (null active-points)))

;;; Processing arcs of length len from pred to succ

(defun constrain-min (pre-tp-s post-tp-s len)
  ;; If the distance to the successor is less than the distance
  ;; to the predecessor plus the arc length, make it be the
  ;; distance to the predecessor plus the arc length.
  (let ((dist-sum (+ (tpoint-min pre-tp-s) len)))
    (when (> dist-sum (tpoint-min post-tp-s))
      (unless (fix> (tpoint-mark post-tp-s) *tpn-base-mark*)
	(push (cons post-tp-s (tpoint-min post-tp-s))
	      *change-history*))
      (setf (tpoint-min post-tp-s) dist-sum)
      (unless (fix= (tpoint-mark post-tp-s) *tpn-mark*)
	(setf (tpoint-mark post-tp-s) *tpn-mark*)
	(push post-tp-s *changed-points*)))))

;;; Undo changes

;;; The change history is a list of (point-struct . old-min-value) pairs.

(defun undo-tpn-changes (change-history) ; -> always failure [nil]
  (dolist (change change-history)
    (let ((tp-s (car change))
	  (old-min (cdr change)))
      (setf (tpoint-min tp-s) old-min))))

;;; Marking

;;; When finding min values, we need to be able to
;;;  (a) tell whether a tpoint has already been recorded on the
;;;      *changed-points* list for the current iteration, and
;;;  (b) tell whether a tpoint has already been recorded on the
;;;      *change-history* list for this call to the TPN.

;;; This is done by seeing whether the tpoint-mark of the tpoint
;;; is equal to *tpn-mark*, or > *tpn-base-mark*, respectively.

;;; When finding max values, we need to do only (a).

(defun new-tpn-mark ()
  (incf *tpn-mark*)
  (unless (typep *tpn-mark* 'fixnum)
    (error "Out of tpn marks."))
  *tpn-mark*)


;;; Finding max values

;;; As indicated in AIAI-TR-6, having found earliest times, it follows
;;; that latest times also exist and yield non-empty time windows when
;;; coupled with earliest times.  Consequently, we no longer have to
;;; worry about the existence of positive cycles.  The iterations are
;;; counted only as a consistency check.  The check that corresponds
;;; to "pulling up" point-0 [ie, for the min of a point becoming greater
;;; than the max] is not performed at all.

;;; Indeed, we know the net is valid (consistent) because there is at
;;; least one value for each point that satisfies all the constraints
;;; (including the min and max limits on points), namely the min value
;;; for that point.  This suggests that if all we care about is validity
;;; we can skip the step of determining max values.

;;; In AIAI-TR-6, it is suggested that max values can be found by
;;; determining the lengths of the longest paths from all points to
;;; a terminal point N that represents the finish of the plan's
;;; last activity.  The max of a point, P, would then be set to
;;; the completion deadline of the plan minus the length of the
;;; longest path from P to N.

;;; In the code below, we do not explicitly calculate path lengths,
;;; and we don't need to know which point, if any, represents the plan
;;; finish.  Instead of calculating distances, we keep track of each
;;; max value and iteratively adjust it towards the right value,
;;; letting arcs push or pull it down in a manner analogous to the
;;; pushing and pulling up in the calculation of min values.

;;; Note that infinite upper bounds on points can become finite
;;; in order to satisfy constraints.  Infinite upper bounds on
;;; constraints never change and are treated as if there were no
;;; max constraint.

(defun find-max-values (initial-active-points) ; -> success?
  (let ((active-points initial-active-points)
	(*changed-points* '())
	(N (tpn-N-points))
	(iterations 0))
    (loop (unless active-points
	    (return))
	  (assert (<= iterations N))
	  (new-tpn-mark)
	  (dolist (tp-s active-points)
	    ;; Process arcs forward from predecessors.  This may push down
	    ;; the max of a predecessor to be the min distance before the
	    ;; max of tp.
	    (dolist (constraint-s (tpoint-pre-con tp-s))
	      (assert (eq tp-s (tcon-post-point constraint-s)))
	      (constrain-max (tcon-pre-point constraint-s)
			     tp-s
			     (tcon-min constraint-s)))
	    ;; Process backward arcs from successors, negating length.
	    ;; This may pull down the max of some successors.
	    (dolist (constraint-s (tpoint-post-con tp-s))
	      (assert (eq tp-s (tcon-pre-point constraint-s)))
	      (unless (%infp (tcon-max constraint-s))
		(constrain-max (tcon-post-point constraint-s)
			       tp-s
			       (- (tcon-max constraint-s))))))
	  (shiftf active-points *changed-points* '())
	  (incf iterations))
    t))

(defun constrain-max (pre-tp-s post-tp-s len)
  ;; If the value at the predecessor is greater than the value at
  ;; the successor minus the arc length, make it be the value at the
  ;; successor minus the arc length.
  (unless (%infp (tpoint-max post-tp-s))
    (let ((new-low (- (tpoint-max post-tp-s) len)))
      (when (or (%infp (tpoint-max pre-tp-s))
		(< new-low (tpoint-max pre-tp-s)))
	(setf (tpoint-max pre-tp-s) new-low)
	(unless (fix= (tpoint-mark pre-tp-s) *tpn-mark*)
	  (setf (tpoint-mark pre-tp-s) *tpn-mark*)
	  (push pre-tp-s *changed-points*))))))


;;; See if a link looks like it will work.

(defun tpn-link-looks-ok-p (from-tp to-tp)
  (if (&< (tpoint-min from-tp) (tpoint-min to-tp))
      ;; The min of tp-to could stay the same.
      t
    ;; The min of tp-to would increase.
    (let ((to-tp-new-min (tpoint-min from-tp)))
      ;; For now, just make a simple check without propagating
      ;; the effects of the increase.  (Then we can see if a
      ;; fancier version works any better.)
      (&<= to-tp-new-min (tpoint-max to-tp)))))


;;; Init routine called the DM initialized.

(define-initializer :dm tpn-init-tpn ()
  (tpn-clear-tpn)
  (tpn-make-tpoint-at-zero))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;  jwd   02 Jan 99	Eliminated tags from the representation of points
;;;			and constraints.  Instead, the struct instances are
;;;			always used directly.  This eliminates the space
;;;			and time costs that tags incur.
;;;  jwd   03 Jan 99	Added change-marks in order to eliminate calls to
;;;			pushnew for the *changed-points* and *change-history*
;;;			lists.
;;;  jwd   04 Jan 99    Added a pretest to tpn-propagate-constraints-after-
;;;			adding-constraint to avoid going into the full
;;;			algorithm when no point values will change.
;;;  jwd   04 Jan 99	Changed infinitep calls to use %infp instead,
;;;			to avoid function calls.
;;;
