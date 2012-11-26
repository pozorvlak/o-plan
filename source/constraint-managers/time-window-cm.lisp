;;;; File: time-window-cm.lisp
;;; Contains: The constraint manager for time windows
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: November 1996
;;; Updated: Tue Dec  3 00:41:57 1996 by Jeff Dalton
;;; Copyright: (c) 1996, AIAI, University of Edinburgh

(in-package :oplan)

;;; The CM

(define-constraint-manager time-window-cm (variable-free-cm simple-cm))

(register-constraint-manager time-window-cm
  :constraint-types '(time-window))


;;; Methods

(defmethod cm-init-constraints ((self time-window-cm))
  ;; Done by the DM.
  nil)

(defmethod cm-add-constraint ((self time-window-cm) time-window)
  (and (add-time-window
	 (time-constraint-start time-window)
	 (time-constraint-end time-window)
	 (time-constraint-min time-window)
	 (time-constraint-max time-window))
       t))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
