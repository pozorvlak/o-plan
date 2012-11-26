;;;; File: island-rescue-support.lsp
;;; Contains: Support code for the island-rescue Web demo
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: October 1994
;;; Updated: Thu Jun  3 18:07:48 1999 by Jeff Dalton
;;; Copyright: (c) 1994, AIAI, University of Edinburgh

(in-package :oplan)

(defparameter *island-rescue-parameters*
  '((:evac-to     (:name Delta Hawaii) "evac destination")
    (:max-days    (:int 0)             "deadline days")
    (:max-hrs     (:int 0)             "deadline hours")
    (:n-abyss     (:int 0 500)         "number to evacuate from Abyss")
    (:n-barnacle  (:int 0 500)         "number to evacuate from Barnacle")
    (:n-calypso   (:int 0 500)         "number to evacuate from Calypso")
    (:n-gts       (:int 1 5)           "number of GTs")
    (:gt-capacity (:int 10 100)        "GT capacity")

    (:psgraph     (:checkbox)          "output PostScript graph?")
    (:narrative   (:checkbox)          "output plan narrative?")
    (:kp-trace    (:checkbox)          "output KP trace?")
    (:world-1     (:checkbox)          "output initial world state?")
    (:world-2     (:checkbox)          "output final world state?")

    (:timeout     (:optional (:int 1 1000))
                  "time limit on finding a plan")
    (:feasibility (:checkbox)
		  "feasibility filter")
    (:use-constraints
                  (:checkbox)
		  "\"use\" constraints")
    ))

(defparameter *default-island-rescue-planning-time-limit* 30) ;seconds

(set-parameter :new-psgraph t)
; (set-parameter :psgraph-all-links t)
; (set-parameter :psgraph-all-nodes t)

(defun island-rescue ()
  (with-web-environment "island-rescue"
    (parse-query-args)
    (convert-query-args *island-rescue-parameters*)
    (island-rescue-feasibility-filter) ;may exit the demo
    (output-rescue-task)
    (html-line "<HR>")
    (html-print-oplan-greeting)
    (let ((have-a-plan-p nil))
      (html-time (setq have-a-plan-p (plan-island-rescue)))
      (html-line "<HR>")
      (if have-a-plan-p
	  (report-island-rescue-success)
	(report-island-rescue-failure)))))

(defun plan-island-rescue ()
  (with-timeout (island-rescue-planning-time-limit)
    (handler-case
        (if (want-output :kp-trace)
	    (with-kp-trace (plan-island-rescue-1))
	  (plan-island-rescue-1))
      (timeout ()
	(error "Planning took more than the time limit of ~S seconds"
	       (island-rescue-planning-time-limit))))))

(defun plan-island-rescue-1 ()
  (plan-for (web-tmp-filename "task" "tf") "task_pacifica_evacuation"))

(defun island-rescue-planning-time-limit ()
  (or (query-arg :timeout) *default-island-rescue-planning-time-limit*))

(defun want-output (name)
  (query-arg name))

(defun island-rescue-tf-file ()
  (if (query-arg :use-constraints)
      "island-rescue-use-example"
    "island-rescue"))

;;;; Feasibility Filter

(defparameter *min-drive-time* 3)	;hours -- must match the TF file

(defparameter *min-fly-time* 2)		;hours -- must match the TF file

(defparameter *trip-limit* 9)

(defun island-rescue-feasibility-filter ()
  (let* ((total-trips 
	  (reduce #'+ (mapcar #'n-trips-required
			      '(:n-abyss :n-barnacle :n-calypso))))
	 (min-sequential-trips
	  (ceiling total-trips (query-arg :n-gts)))
	 (min-total-drive-time
	  (* min-sequential-trips *min-drive-time*))
	 (return-flight-time
	  (ecase (query-arg :evac-to)
	    (delta 0)
	    (hawaii *min-fly-time*)))
	 (deadline-in-hours
	  (+ (* 24 (query-arg :max-days)) (query-arg :max-hrs)))
	 (total-time-needed
	  (+ min-total-drive-time return-flight-time))
	 (hours-to-spare
	  (max 0 (- deadline-in-hours total-time-needed))))

    ;; Assume that the time needed to fly the B707 out is not a factor:
    (assert (<= *min-fly-time* min-total-drive-time))

    (flet ((reasoning ()
	     (output
	       // "Given the current populations, GT numbers and GT capacity,"
	       // "a total of " total-trips " GT trips are required,"
	       // "with at most " (query-arg :n-gts) " in parallel.")
	     (when (> min-sequential-trips 1)
	       (output
		 // "There will therefore be at least one sequence of "
		 // min-sequential-trips " trips."))
	     (output
	       // "Each trip requires at least " *min-drive-time* " hours."
	       // "If as many as possible occur in parallel, it will take at"
	       // "least " min-total-drive-time " hours to complete them all.")
	     (when (> return-flight-time 0)
	       (output
		 // "In addition, it will take at least " return-flight-time
		 // " hours to fly the evacuees to Hawaii."))
	     (output
	      // "The deadline is " deadline-in-hours
	         (if (> deadline-in-hours 1) " hours." " hour.")
	      //))

	   (explain-difficulty ()
	     (when (> total-trips *trip-limit*)
	       (output
		 "The maximum number of trips to attempt is"    //
		 *trip-limit* ", but you have " total-trips "." //))
	     (when (< hours-to-spare *min-drive-time*)
	       (output
		"You should leave at least " *min-drive-time* " hours" //
		"to spare, but you have " hours-to-spare "."           //)))

	   (explain-potential-difficulty ()
	     (unless (> total-trips *trip-limit*)
	       (output
		 "The maximum number of trips to attempt is"     //
		 *trip-limit* " (you now have " total-trips ")." //))
	     (unless (< hours-to-spare *min-drive-time*)
	       (output
		 "You should leave at least " *min-drive-time* " hours" //
		 "to spare (you now have " hours-to-spare ")."          //)))

	   (try-to-exit ()
	     (when (query-arg :feasibility)
	       (web-mail-comment-link)
	       (exit-web-demo))
	     (html-paragraph
	       "However, the filter is not allowed to intervene.")))

      ;; Determine whether we should try to find a plan.
      (cond ((> total-time-needed deadline-in-hours)
	     ;; No plan is possible.
	     (html-paragraph
	       (output (:stream *html-out*)
	         "The feasibility filter for this domain has determined" //
	         "that no plan is possible.  The reasons are as follows:" //
		 (:include
	           (html-block "BLOCKQUOTE"
		     (reasoning)))))
	     (web-note-failure "no plan is possible")
	     (try-to-exit))
	    ((or (> total-trips *trip-limit*)
		 (< hours-to-spare *min-drive-time*))
	     ;; planning is too difficult
	     (html-paragraph
	       (output (:stream *html-out*)
	         "The feasibility filter for this domain has determined" //
	         "that it will be difficult to find a plan in the time" //
		 "available for running the planner.  The reasons are" //
		 "as follows:" //
		 (:include
	           (html-block "BLOCKQUOTE"
		     (explain-difficulty)))
		 "Additional factors to consider when altering parameters:" //
		 (:include
		   (html-block "BLOCKQUOTE"
		     (html-paragraph
		       (reasoning))
		     (html-paragraph
		       (explain-potential-difficulty))))))
	     (web-note-failure "planning is too difficult")
	     (try-to-exit))))))

(defun n-trips-required (n-city-arg)
  (ceiling (query-arg n-city-arg) (query-arg :gt-capacity)))
    

;;;; Links to results

;;; Success results

(defun report-island-rescue-success ()
  (report-island-rescue-results)
  (html-anchor (web-demo-url "output-formats.html")
    "Explain outputs")
  (html-line "<BR>")
  (web-mail-comment-link)
  (web-note-success))

(defun report-island-rescue-results ()
  (html-paragraph
    (html-line "There's a plan that evacuates all ~A people to ~A in ~A."
      (reduce #'+ (mapcar #'query-arg '(:n-abyss :n-barnacle :n-calypso)))
      (string-capitalize
        (query-arg :evac-to))
      (seconds->description
        (first (request-time-bounds '(node-2 :begin)))))
    (html-line "This is the ~:R plan produced for this demo."
	       (incf-file-counter)))

  (html-report-plan-statistics)

  (html-paragraph
    (html-line "Results:")
    (html-block "UL"

      ;; Results that are always produced.

      (html-item "LI"
	(html-tmp-anchor "task" "tf" "TF task definition"))

      (html-item "LI"
        (html-anchor (web-demo-url
		      (concat-string (island-rescue-tf-file) ".tf"))
		     "Support TF"))

      ;; Results produced only when requested

      (html-standard-result-links
        "Island-resuce"
	"Pacifica Evacuation"))))


;;; Failure results

(defun report-island-rescue-failure ()

  (html-paragraph
    (html-line "No plan is possible within the given constraints."))

  (html-report-plan-statistics)

  (html-paragraph
    (html-line "Reference links:")
    (html-block "UL"

      ;; Results that are always produced.

      (html-item "LI"
	(html-tmp-anchor "task" "tf" "TF task definition"))

      (html-item "LI"
        (html-anchor (web-demo-url
		      (concat-string (island-rescue-tf-file) ".tf"))
		     "Support TF"))

      ;; Results produced only when requested

      (when (want-output :kp-trace)
	(html-item "LI"
	  (html-tmp-anchor "kp-trace" "txt" "KP trace output")))))

  (web-mail-comment-link)
  (web-note-failure "no plan was possible"))


;;;; TF output

(defparameter *evac-alist*
  '(("Abyss" . :n-abyss) ("Barnacle" . :n-barnacle) ("Calypso" . :n-calypso)))

(defun output-rescue-task ()
  (with-tf-output-file (web-tmp-filename "task" "tf")
    (output (:stream *tf-out*)
      // ";;; Rescue task"
      //
      // "types ground_transport = " (generate-gt-names) ";"
      //
      // "always {gt_capacity " (query-arg :gt-capacity) "};"
      //
      // "initially" (:include (output-rescue-initially))
      //
      // "task Pacifica_evacuation;"
      // "  nodes sequential"
      // "          1 start,"
      // "          2 finish"
      // "        end_sequential;"
      // "  conditions" (:include (output-rescue-conditions))
      // "  time_windows 0.." (deadline-time-spec) " at 2;"
      // "end_task;"
      //
      // "include \"" (web-demo-filename (island-rescue-tf-file)) "\";"
      //)))

;;; Conditions

(defun output-rescue-conditions ()
  (output-comma-separated "     " (make-rescue-conditions) ";"))

(defun make-rescue-conditions ()
  (append (make-evac-conditions) (make-destination-conditions)))

(defun make-evac-conditions ()
  (loop for (city . n-arg) in *evac-alist*
	for n = (query-arg n-arg)
	collect
	  (format nil "achieve ~A at 2" (evac-status-string city 0 n))))

(defun make-destination-conditions ()
  (list
    (format nil "achieve {safe_at ~:(~A~)} at 2" (query-arg :evac-to))))

;;; "Initially" effects

(defun output-rescue-initially ()
  (output-comma-separated "     " (make-rescue-initially) ";"))

(defun make-rescue-initially ()
  (append
    (make-evac-effects)
    (make-gt-init-effects)))

(defun make-evac-effects ()
  (loop for (city . n-arg) in *evac-alist*
	for n = (query-arg n-arg)
	collect
	  (evac-status-string city n 0)))

(defun make-gt-init-effects ()
  (loop for gt in (generate-gt-names)
	collect
	  (format nil "{in_use_for ~A} = available" gt)))

(defun generate-gt-names (&optional (n (query-arg :n-gts)))
  (loop for i from 1 to n
	collect (concat-name "GT" (int->string i))))

;;; Time limit

(defun deadline-time-spec ()
  (format nil "~D~~~2,'0D:00" (query-arg :max-days) (query-arg :max-hrs)))

;;; Evac_status strings

(defun evac-status-string (city n-remaining n-evacuated)
  (format nil
	  "{evac_status ~:(~8A~)} = {~3@A ~3@A}"
	  city n-remaining n-evacuated))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
