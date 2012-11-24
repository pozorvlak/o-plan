;;; File:    tpn-tests
;;; Purpose: Tests of the time point network manager
;;; Author:  Jeff Dalton <J.Dalton@ed.ac.uk>
;;;          AI Applications Institute, University of Edinburgh
;;; Updated: Tue Jul  9 01:04:24 1996 by Jeff Dalton

;;; This file contains a number of test cases for the TPN manager.

; (in-package :oplan-tpn)
(in-package :oplan-nodes)

(use-package :oplan-test-support)

(define-test-module :tpn-tests)

(in-test-module :tpn-tests)


;;; The tests:

(define-test-group initial-tpn-tests
  ;; Empty net
  ((test-net '() '())
   ==> '())
  ;; Nonempty net
  ((test-net '((a 0 10) (b 2 3)) '())
   ==> '((a 0 10) (b 2 3)))
  ;; New net with different points
  ((test-net '((r 5 6) (s 7 8)) '())
   ==> '((r 5 6) (s 7 8)))
  ;; Extend current net
  ((test-net '((a 0 100) (b 20 30)) '() :reset nil)
   ==> '((r 5 6) (s 7 8)
	 (a 0 100) (b 20 30)))
  ;; Net with infinities
  ((test-net `((a 0 ,(infinity)) (b 10 ,(infinity)))
	     '())
   ==> `((a 0 ,(infinity)) (b 10 ,(infinity))))
  )

(define-test-group basic-effect-tests
  ;; Most of these tests start with the result of the previous test;
  ;; then we combine them all at the end.  We avoid :RESET NIL so that
  ;; later tests can work even if earlier ones failed.
  ;; Start with a constraint with no effect.
  ((test-net '((a 0 10) (b 5 15)
	       (z 0 0) (end 20 20))
	     `((a b 0 ,(infinity))))
   ==> '((a 0 10) (b 5 15)
	 (z 0 0) (end 20 20)))
  ;; Have a constraint min push up the min of A.
  ((test-net '((a 0 10) (b 5 15)
	       (z 0 0) (end 20 20))
	     `((z a 2 ,(infinity))))
   ==> '((a 2 10) (b 5 15)
	 (z 0 0) (end 20 20)))
  ;; Have a constraint max pull up the min of A.
  ((test-net '((a 2 10) (b 5 15)
	       (z 0 0) (end 20 20))
	     '((a end 0 17)))
   ==> '((a 3 10) (b 5 15)
	 (z 0 0) (end 20 20)))
  ;; Have a constraint min push down the max of B.
  ((test-net '((a 3 10) (b 5 15)
	       (z 0 0) (end 20 20))
	     `((b end 6 ,(infinity))))
   ==> '((a 3 10) (b 5 14)
	 (z 0 0) (end 20 20)))
  ;; Have a constraint max pull down the max of B.
  ((test-net '((a 3 10) (b 5 14)
	       (z 0 0) (end 20 20))
	     '((z b 0 11)))
   ==> '((a 3 10) (b 5 11)
	 (z 0 0) (end 20 20)))
  ;; Try them all in one test.
  ((test-net '((a 0 10) (b 5 15) (z 0 0) (end 20 20))
	     `((z a   2 ,(infinity))
	       (a end 0 17)
	       (b end 6 ,(infinity))
	       (z b   0 11)))
   ==> '((a 3 10) (b 5 11) (z 0 0) (end 20 20)))
  )

(define-test-group basic-failure-tests
  ;; Constraint is too wide for point bounds
  ((test-net '((a 0 10)
	       (b 20 30))
	     '((a b 31 100)))		;30 would be ok
   ==> '((a b 31 100)))
  ;; Constraint is too narrow for point bounds
  ((test-net '((a 0 10)
	       (b 20 30))
	     '((a b 0 5)))
   ==> '((a b 0 5)))
  ;; Constraint direction conflicts with point bounds
  ((test-net '((a 0 10)
	       (b 20 30))
	     `((b a 0 ,(infinity))))
   ==> `((b a 0 ,(infinity))))
  ;; Conflicting min and max distance in separate constraints.
  ;; Note that this counts as a positive-length cycle in the sense
  ;; of AIAI-TR-6.
  ((test-net `((a 0 ,(infinity))
	       (b 0 ,(infinity)))
	     `((a b 10 ,(infinity))
	       (a b 0 5)))
   ==> '((a b 0 5)))
  ;; Conflicting exact distances in two constraints
  ((test-net '((a 0 10) (b 0 20))
	     '((a b 0 0)
	       (a b 10 10)))
   ==> '((a b 10 10)))
  )

(define-test-group equality-constraint-tests
  ;; A should become as restricted as B.
  ((test-net '((a 0 10) (b 5 10))
	     '((a b 0 0)))
   ==> '((a 5 10) (b 5 10)))
  ;; Both should get the intersection
  ((test-net '((a 0 10) (b 5 15))
	     '((a b 0 0)))
   ==> '((a 5 10) (b 5 10)))
  ;; Set up some points constrained only to be equal.
  ;; Use a linear sequence of constraints.
  ;; Include a zero point for later.
  ((test-net `((a 0 ,(infinity)) (b 0 ,(infinity))
	       (c 0 ,(infinity)) (d 0 ,(infinity))
	       (e 0 ,(infinity))
	       (z 0 0))
	     `((a b 0 0) (b c 0 0) (c d 0 0) (d e 0 0)))
   ==> `((a 0 ,(infinity)) (b 0 ,(infinity))
	 (c 0 ,(infinity)) (d 0 ,(infinity))
	 (e 0 ,(infinity))
	 (z 0 0)))
  ;; Now constrain the middle point to be between 3 and 5.
  ((test-net '() '((z c 3 5)) :reset nil)
   ==> `((a 3 5) (b 3 5)
	 (c 3 5) (d 3 5)
	 (e 3 5)
	 (z 0 0)))
  ;; Constrain the last point be be between 1 and 4.
  ((test-net '() '((z e 1 4)) :reset nil)
   ==> `((a 3 4) (b 3 4)
	 (c 3 4) (d 3 4)
	 (e 3 4)
	 (z 0 0)))
  ;; Constrain the first point to be at most 3.
  ((test-net '() '((z a 0 3)) :reset nil)
   ==> `((a 3 3) (b 3 3)
	 (c 3 3) (d 3 3)
	 (e 3 3)
	 (z 0 0)))
  )

(define-test-group basic-cycle-tests
  ;; Conflicting direction on points w/o other constraints.
  ;; This one is ok, because the points can have the same value.
  ((test-net `((a 0 ,(infinity))
	       (b 0 ,(infinity)))
	     `((a b 0 ,(infinity))
	       (b a 0 ,(infinity))))
   ==> `((a 0 ,(infinity))
	 (b 0 ,(infinity))))
  ;; But this one isn't.
  ((test-net `((a 0 ,(infinity))
	       (b 0 ,(infinity)))
	     `((a b 1 ,(infinity))
	       (b a 0 ,(infinity))))
   ==> `((b a 0 ,(infinity))))
  ;; Nor this one.
  ((test-net `((a 0 ,(infinity))
	       (b 0 ,(infinity)))
	     `((a b 0 ,(infinity))
	       (b a 1 ,(infinity))))
   ==> `((b a 1 ,(infinity))))
  ;; A longer cycle
  ((test-net `((a 0 ,(infinity))
	       (b 0 ,(infinity))
	       (c 0 ,(infinity)))
	     `((a b 0 ,(infinity))
	       (b c 1 ,(infinity))
	       (c a 0 ,(infinity))))
   ==> `((c a 0 ,(infinity))))
  )

(define-test-group max-limited-cycle-tests
  ;; This is the same as the basic-cycle-tests except that the points
  ;; are confined to [0,10].  Implementations without the iteration
  ;; limit might therefore eventually conclude something.
  ;;
  ;; This one is ok, because the points can have the same value.
  ((test-net `((a 0 10)
	       (b 0 10))
	     `((a b 0 ,(infinity))
	       (b a 0 ,(infinity))))
   ==> `((a 0 10)
	 (b 0 10)))
  ;; But this one isn't.
  ((test-net `((a 0 10)
	       (b 0 10))
	     `((a b 1 ,(infinity))
	       (b a 0 ,(infinity))))
   ==> `((b a 0 ,(infinity))))
  ;; Nor this one.
  ((test-net `((a 0 10)
	       (b 0 10))
	     `((a b 0 ,(infinity))
	       (b a 1 ,(infinity))))
   ==> `((b a 1 ,(infinity))))
  ;; A longer cycle
  ((test-net `((a 0 10)
	       (b 0 10)
	       (c 0 10))
	     `((a b 0 ,(infinity))
	       (b c 1 ,(infinity))
	       (c a 0 ,(infinity))))
   ==> `((c a 0 ,(infinity))))
  )


;;; Tim Duncan's tests for the Tosca-92 modifications.

;;; The problem numbers are from Tim's file, which has a completely
;;; different format and doesn't include tests of the results; they
;;; do _not_ correspond to the test numbers used by DEFINE-TEST-GROUP.

(define-test-group timd-tpn-tests

  ;; PROBLEM-1: This shows that constraints don't do propagation backwards
  ;; properly.  The result should be that tp1=(10,26), and tp2=(20,36), but
  ;; we miss out on tp1-max.  SOLUTION: Modified tpn-add-time-constraint to
  ;; propagate constraints from the timepoints involved.
  ((test-net '((tp1 10 500)
	       (tp2 0 36))
	     '((tp1 tp2 10 10)))
   ==> '((tp1 10 26) (tp2 20 36)))

  ;; PROBLEM-2: Values do not appear to be propagated over an equality
  ;; constraint, that is a constraint which links two timepoints by a zero-zero
  ;; distance.  Adding the constraint should reduce the domains of both
  ;; timepoints to 10..36.  SOLUTION: (actually this works)
  ((test-net '((tp1 10 500)
	       (tp2 0 36))
	     '((tp1 tp2 0 0)))
   ==> '((tp1 10 36)
	 (tp2 10 36)))

  ;; PROBLEM-3: As with test2 but now has to propagate over an intermediate
  ;; timepoint.  SOLUTION: this works too ...
  ((test-net '((tp1 10 500)
	       (tp2 0 800)
	       (tp3 0 36))
	     '(;; make TP1 and TP2 equivalent
	       (tp1 tp2 0 0)
	       ;; constrain TP2 and TP3 to be equal
	       (tp2 tp3 0 0)))
   ==> '((tp1 10 36)
	 (tp2 10 36)
	 (tp3 10 36)))

  ;; test3b
  ((test-net '((tp1 10 500)
	       (tp2 0 800)
	       (tp3 0 36)
	       (tp0 0 0))
	     '(;; make TP1 and TP2 equivalent
	       (tp1 tp2 0 0)
	       ;; constrain TP2 and TP3 to be equal
	       (tp2 tp3 0 0)
	       ;; now restrict TP1's time window and see effect
	       (tp0 tp1 20 999)))	; :POSITIONAL (it says)
   ==> '((tp1 20 36)
	 (tp2 20 36)
	 (tp3 20 36)
	 (tp0 0 0)))

  ;; PROBLEM-4: Model fixing the setup in a MOP.  This appears to work alright.
  ((test-net '((start 10 500)
	       (proc  10 500)
	       (end 10 500))
	     '(;; Set duration of processing and setup.
	       (proc end 20 20)
	       (start proc 0 2)
	       ;; Fix setup with a constraint.
	       (start proc 2 2)))
   ==> `((start 10			;start min stays at 10
	        ,(- 500 20 2))		;start max = 2 before proc max
	 (proc ,(+ 10 2)		;proc min = 2 after start min
	       ,(- 500 20))		;proc max = 20 before end max
	 (end ,(+ 10 2 20)		;end min = 20 after proc min
	      500)))			;end max stays at 500

  ;; PROBLEM-5: Propagation ... two MOPS with precedence link.  The main point
  ;; to look for is that the start tp of mop-2 should be same as the proc tp.
  ;; This problem has been fixed by TPN propagation fixes.
  ((test-net '((time-zero-tp 0 0)
	       (start1 10 500)
	       (start2 10 500)
	       (proc1 10 500)
	       (proc2 10 500)
	       (end1 10 500)
	       (end2 10 500))
	     '(;; MOP-1
	       (proc1 end1 20 20)
	       (start1 proc1 0 2)
	       ;; MOP-2 is after MOP-1
	       (proc2 end2 20 20)
	       (start2 proc2 0 2)
	       ;; Constrain precedence
	       (end1 proc2 0 999)
	       ;; Now make MOP-1 later
	       (time-zero-tp start1 25 999))) ; :POSITIONAL (it says)
   ==> ;; The result for start2 is perhaps the hardest to see.
       ;; Nothing pushes it up from the value it is given at the
       ;; start.  Instead it is pulled up to be at most 2 before
       ;; the min of proc2.  
       '((time-zero-tp 0 0)		;unchanged at [0,0]
	 (start1 25			;set by constraint w.r.t. 0
	         460)			;can be as late as proc1 max
	 (start2 43			;can be as early as 2 before proc2 min
	         480)			;can be as late as proc2 max
	 (proc1 25			;can be as early as start1 min
	        460)			;duration 20 means 20 before end1 max
	 (proc2 45			;must have room for MOP-1 proc duration
					;and be >= end1 min
	        480)			;duration 20 means 20 before end2 max
	 (end1 45			;duration 20 means 20 after proc1 min
	       480)			;must have room for MOP-2 proc duration
	 (end2 65			;duration 20 means 20 after proc2 min
	       500)))			;unchanged

  ;; PROBLEM-6: Check that the changes returned as second value of
  ;; tpn-add-time-constraint are working reasonably.

  ;; BUT the O-Plan TPN doesn't return this 2nd value.

  ;; PROBLEM-7: try to reduce the latest-end of a MOP (This is an Arthur
  ;; generated problem).
  ((test-net '((time-zero-tp 0 0)
	       (start 20 500)
	       (proc 20 500)
	       (end 20 38))
	     '(;; MOP-1
	       (proc end 4 4)
	       (start proc 0 0)
	       ;; Now reduce latest-end
	       (time-zero-tp end 24 30)))
   ==> '((time-zero-tp 0 0)		;no change
	 (start 20 26)			;limits exactly end's
	 (proc 20 26)			;limits exactly 4 before end's
	 (end 24 30)))			;set by constraint w.r.t. [0,0].
  
  )

;;; End
