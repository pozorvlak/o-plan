;;;; File: sim-clock.lisp
;;; Contains: Simulated time clocks
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: October 1994
;;; Updated: Tue Mar 23 01:45:56 1999 by Jeff Dalton
;;; Copyright: (c) 1994, AIAI, University of Edinburgh

(in-package :oplan-sim-clock)

;;;; Simulated time

;;; [This section describes "scaled real time".  An alternative is
;;; "purely simulated time", which will be described below if it's
;;; ever implemented.]

;;; World time in the simulated world, henceforth known "simulated
;;; time", is in seconds and is related to real time seconds by a
;;; scale factor that indicates how many seconds of simulated time
;;; pass for each second of real time.  The scale factor should be
;;; an integer >= 1.  This is not essential, but to properly support
;;; scale factors < 1, we should switch to a floating point
;;; representation of simulated time.

;;; Our actual real-time clock is O-Plan's primitive-real-time, which
;;; normally ticks in a unit smaller than seconds.  This complicates
;;; the handling of simulated time somewhat but usually lets us avoid
;;; using universal times, which tend to be bignums.

;;; /\/: Unfortunately, primitive-real-time is usually internal-real-time
;;; which is even bigger bignums in Common Lisps such as Lucid 4.1.

;;; The conversion is initialized relative to a given universal time
;;; rather than to the time when start-clock is called.  This allows
;;; one agent (usually the Exec) to establish the relationship between
;;; real and simulated time and tell another agent (such as the World)
;;; what it is, rather than have the 2nd agent's version of simulated
;;; time be slow relative to the 1st's because of the time that elapsed
;;; before it was able to process the request.  So if the Exec wants
;;; to say "it's now simulated time t -- start your clock", it has to
;;; say when "now" is, for otherwise the Exec's now (when it sends the
;;; :start-clock message) and the World's now (when it receives the
;;; message) will be different.

;;; The conversion from universal time to primitive real time, used
;;; when initializing simulated time, is provided by the p-process
;;; mechanism (in the single-process version of O-Plan).

;;; N.B. If you use a scale-factor that is large for the frequency
;;; of events (e.g. 1 second = 1 day with things happening every
;;; few minutes), it may be difficult for simulated time to keep
;;; up.

;;; Each pprocess that wants a clock must provide a binding for *clock*.
;;; If more than one pprocess may have a clock, *clock* should be one of
;;; pprocess-sepecials of each pprocess that might have one.  A pprocess
;;; calls define-a-clock to give *clock* a value.

;;; However, code that uses clocks should not refer to *clock* directly
;;; (except when creating the binding).

(defvar *clock* 'not-a-clock)

(defstruct clock
  (running-p nil)
  (base-simulated-time 0)
  (base-universal-time 0)
  (base-real-time 0)
  (simulated-seconds-per-second 1)
  (granularity (default-clock-granularity))	;experimental /\/
  (current-simulated-time 0)
  ;; Slots for simulated-time-string:
  (cached-time -1)				;should not = any time
  (cached-time-string "no time"))

(defun define-a-clock (&rest initargs)
  (setq *clock* (apply #'make-clock initargs)))

(defun default-clock-granularity ()
  (let ((g (get-parameter :clock-granularity)))
    (if g (string->int g) nil)))


;;; Starting and stopping the clock

(defun start-clock (universal-time simulated-time scale-factor)
  ;; Initialize simulated time so that at time = universal-time
  ;; in the real world, time = simulated-time in the simulated world.
  ;; This initial simulated-time is typically 0.
  (setf (clock-base-simulated-time *clock*)
	  simulated-time
	(clock-base-universal-time *clock*)
	  universal-time
	(clock-base-real-time *clock*)
	  (universal->primitive-real-time universal-time)
	(clock-simulated-seconds-per-second *clock*)
	  scale-factor
	(clock-running-p *clock*)
	  t)
  (get-simulated-time))

(defun set-clock-granularity (g)
  (setf (clock-granularity *clock*) g))

(defun stop-clock ()			;suspend- ? /\/
  (setf (clock-running-p *clock*) nil))

(defun-inline the-clock-is-running ()
  (clock-running-p *clock*))

(defun init-clock ()
  (stop-clock)
  (setf (clock-base-simulated-time *clock*) 0
	(clock-base-universal-time *clock*) 0
	(clock-base-real-time *clock*) 0
	(clock-simulated-seconds-per-second *clock*) 1
	(clock-granularity *clock*) (default-clock-granularity)
	(clock-current-simulated-time *clock*) 0
	;; leave "cached-" values alone
	)
  0)


;;; Conversions

(defun simulated->real-seconds (simulated-seconds)
  (floor simulated-seconds (clock-simulated-seconds-per-second *clock*)))

(defun real->simulated-seconds (real-seconds)
  (* real-seconds (clock-simulated-seconds-per-second *clock*)))

(defun simulated->universal-time (simulated-time)
  ;; /\/: Ceiling when setting wakeup times?  We don't want to
  ;; wake up before it's time, because of rounding down, and then
  ;; have to spin our wheels, repeatedly sleeping until the same
  ;; universal time, waiting for enough time to pass.  Or maybe
  ;; we'd rather not sleep too long.  So for now it's floor.
  ;; N.B. We can't sleep for < 1 second.
  (let ((sim-delta (- simulated-time (clock-base-simulated-time *clock*))))
    (+ (clock-base-universal-time *clock*)
       ;; /\/: Could call simulated->real-seconds here.
       (floor sim-delta
	      (clock-simulated-seconds-per-second *clock*)))))

;;; Simulated-time-string returns a time-string that corresponds to
;;; a simulated time (ie, to some number of seconds).  The most recent
;;; result is cached and reused if the same conversion is requested.

(defun simulated-time-string (sim-time)
  (if (= sim-time (clock-cached-time *clock*))
      (clock-cached-time-string *clock*)
    (setf (clock-cached-time *clock*)
	    sim-time
	  (clock-cached-time-string *clock*)
	    (seconds->minimal-time-string sim-time))))


;;; Asking the time

;;; Get-simulated-time does a system call, recomputes the current simulated
;;; time, and stores the result in the *clock*'s current-simulated-time slot.
;;; Set-simulated-time sets the *clock*'s current-simulated-time directly.
;;; Current-simulated-time just returns the value of the current-simulated-
;;; time slot.

;;; Note that the conversion from primitive-real units to simulated
;;; seconds could be:
;;;
;;;   real-delta = (floor primitive-delta primitive-time-units-per-second)
;;;   sim-delta = (* real-delta simulated-seconds-per-second)
;;;
;;; which would give a more granular result.

;;; /\/: In the actual conversion, below, we used to use round instead
;;; of floor.  It may not be clear which is better.

(defun get-simulated-time ()
  (let* ((clock *clock*)
	 (base-simulated-time (clock-base-simulated-time clock))
	 (base-real-time (clock-base-real-time clock))
	 (simulated-seconds-per-second
	  (clock-simulated-seconds-per-second clock))
	 (granularity (clock-granularity clock)))
    (let* ((primitive-delta
	    (- (get-primitive-real-time) base-real-time))
	   (sim-delta
	    (floor (* simulated-seconds-per-second primitive-delta)
		   primitive-time-units-per-second))
	   (sim-time
	    (+ base-simulated-time sim-delta)))
      ;; Record the time
      (setf (clock-current-simulated-time clock)
	    (if granularity
		(quantize-down sim-time granularity)
	      sim-time)))))

(defun-inline current-simulated-time ()
  (clock-current-simulated-time *clock*))


;;; Setting the time

;;; In some cases, it may be desirable to "pretend" the simulated time
;;; is some particular time.

;;; Note that set-simulated-time does not change the mapping from real
;;; to simulated time.  The next call to get-simulated-time will compute
;;; the same time as if set-simulated-time had not been called.

(defun set-simulated-time (sim-time)
  (setf (clock-current-simulated-time *clock*) sim-time))


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
