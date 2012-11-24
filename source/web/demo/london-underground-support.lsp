;;;; File: london-underground-support.lsp
;;; Contains: Support code for the london underground Web demo
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: November 1994
;;; Updated: Tue Jul  9 17:08:03 1996 by Jeff Dalton
;;; Copyright: (c) 1994, AIAI, University of Edinburgh

(in-package :oplan)

(defparameter *stations*
  '(aldwych baker_street bank bond_street cannon_street charing_cross
    earls_court elephant_and_castle embankment euston gatwick
    gloucester_road green_park heathrow holborn kennington
    kings_cross leicester_square liverpool_street marylebone
    mile_end monument moorgate neasden notting_hill_gate
    oxford_circus paddington piccadilly_circus south_kensington
    stratford tottenham_court_road tower_hill victoria
    warren_street waterloo))

(defparameter *lines*
  '(bakerloo central circle district district_west docklands_light_railway
    escalator_between_bank_and_monument gatwick_express jubilee
    metropolitan northern_city northern_west piccadilly
    piccadilly_aldwych victoria waterloo_and_city))

(defparameter *london-underground-parameters*
  `((:from        (:name ,@*stations*)   "starting point")
    (:to          (:name ,@*stations*)   "destination")
    (:postscrpt   (:optional (:name On)) "PostScript")
    (:narrative   (:optional (:name On)) "Narrative")
    (:kp-trace    (:optional (:name On)) "KP trace")
    (:feasibility (:optional (:name On)) "feasibility filter")
    (:timeout     (:optional (:int 1 1000))
                  "time limit on finding a plan")))

(defparameter *default-planning-time-limit* 30) ;seconds

(set-parameter :new-psgraph t)
; (set-parameter :psgraph-all-links t)
; (set-parameter :psgraph-all-nodes t)

(defvar *have-a-plan-p* nil)

(defun london-underground ()
  (with-web-environment "london-underground"
    (parse-query-args)
    (convert-query-args *london-underground-parameters*)
    (output-route-task)
    (html-time (setq *have-a-plan-p* (plan-route)))
    (if *have-a-plan-p*
	(report-route-success)
      (report-route-failure))))

(defun plan-route ()
  (terpri)				;in case there was earlier output
  (with-timeout (planning-time-limit)
    (handler-case
        (if (query-arg :kp-trace)
	    (with-kp-trace (plan-route-1))
	  (plan-route-1))
      (timeout ()
	(error "Planning took more than the time limit of ~S seconds"
	       (planning-time-limit))))))

(defun plan-route-1 ()
  (plan-for (web-tmp-filename "task" "tf") "task_find_route"))

(defun planning-time-limit ()
  (or (query-arg :timeout) *default-planning-time-limit*))


;;;; Links to results

(defun report-route-success ()

  (html-report-plan-statistics)

  (html-paragraph
    (html-line "Results:")
    (html-block "UL"

      ;; Results that are always produced.

      (html-item "LI"
	(html-tmp-anchor "task" "tf" "TF task definition"))

      (html-item "LI"
        (html-anchor (web-demo-url "london-underground.tf")
		     "Support TF"))

      ;; Results produced only when requested

      ;; /\/: Should be able to use html-standard-result-links,
      ;; but can't because the names of the outputs are symbols
      ;; while our parameters are keywords and, moreover, keywords
      ;; with print-names that don't match the symbols.

      (when (query-arg :PostScript)
	(request-psgraph :file-one-page
	   :title "From A to B"
	   :output-file (web-tmp-filename "graph" "ps"))
	(html-item "LI"
	  (html-tmp-anchor "graph" "ps" "PostScript graph")
	  (html-line "of the plan")))

      (when (query-arg :Narrative)
	(request-plan-view
	   :mode :narrative
	   :levels :all
	   :output-file (web-tmp-filename "narrative" "txt"))
	(html-item "LI"
	  (html-tmp-anchor "narrative" "txt" "Plan narrative")))

      (when (query-arg :KP-trace)
	(html-item "LI"
	  (html-tmp-anchor "kp-trace" "txt" "KP trace output")))))

  (web-mail-comment-link))


(defun report-route-failure ()
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
        (html-anchor (web-demo-url "london-underground.tf")
		     "Support TF"))

      ;; Results produced only when requested

      (when (query-arg :KP-trace)
	(html-item "LI"
	  (html-tmp-anchor "kp-trace" "txt" "KP trace output")))))

  (web-mail-comment-link))


;;;; TF output

;;; A route task should be defined something like this:

;;; initially {used_line ??} = false;

;;; task find_route;
;;;   nodes sequential
;;;           1 start,
;;;           2 finish
;;;         end_sequential;
;;;   conditions achieve {at} = gatwick at 2;
;;;   effects {at} = bank at 1;
;;; end_task;

(defun output-route-task ()
  (let ((from (string-downcase (query-arg :from)))
	(to   (string-downcase (query-arg :to))))
    (with-tf-output-file (web-tmp-filename "task" "tf")
      (output (:stream *tf-out*)
        // ";;; Route task"
        //
        // "initially {visited_station ??} = false,"
	// "          {used_line ??} = false;"
        //
        // "task find_route;"
        // "  nodes 1 start,"
        // "        2 finish,"
        // "        3 action {from " from "};"
        // "  orderings 1 ---> 3, 3 ---> 2;"
        // "  effects {goal} = " to " at 1;"
        // "end_task;"
        //
        // "include \"london-underground\";"
        //))))

;;; Output-line-schema was used during the constrcution of
;;; london-underground.tf.

#+:undef
(defun output-line-schema (line)
  (output
    // "schema " line ";"
    // "  vars ?from = ?{type " line "},"
    // "       ?to =   ?{type " line "};"
    // "  expands {" line " to ?to};"
    // "  only_use_for_effects {at} = ?to;"
    // "  conditions"
    // "     only_use_if {used_line " line "} = false,"
    // "     achieve {at} = ?from;"
    // "  effects {used_line " line "} = true;"
    // "end_schema;"
    //))


;;;; Database

(defmacro station (station line)
  `(record-station ',station ',line))

(defun record-station (station line)
  (assert (member station *stations*))
  (assert (member line *lines*))
  (push line (get station 'lines))
  (push station (get line 'stations)))

(defun line-stations (line)
  (get line 'stations))

(defun station-lines (station)
  (get station 'lines))

(station aldwych piccadilly_aldwych)
(station baker_street bakerloo)
(station baker_street circle)
(station baker_street jubilee)
(station baker_street metropolitan)
(station bank central)
(station bank escalator_between_bank_and_monument)
(station bank northern_city)
(station bank waterloo_and_city)
(station bond_street central)
(station bond_street jubilee)
(station cannon_street circle)
(station cannon_street district)
(station charing_cross bakerloo)
(station charing_cross jubilee)
(station charing_cross northern_west)
(station earls_court district)
(station earls_court district_west)
(station earls_court piccadilly)
(station elephant_and_castle bakerloo)
(station elephant_and_castle northern_city)
(station embankment bakerloo)
(station embankment circle)
(station embankment district)
(station embankment northern_west)
(station euston northern_city)
(station euston northern_west)
(station euston victoria)
(station gatwick gatwick_express)
(station gloucester_road circle)
(station gloucester_road district)
(station gloucester_road piccadilly)
(station green_park jubilee)
(station green_park piccadilly)
(station green_park victoria)
(station heathrow piccadilly)
(station holborn central)
(station holborn piccadilly)
(station holborn piccadilly_aldwych)
(station kennington northern_city)
(station kennington northern_west)
(station kings_cross circle)
(station kings_cross metropolitan)
(station kings_cross northern_city)
(station kings_cross piccadilly)
(station kings_cross victoria)
(station leicester_square northern_west)
(station leicester_square piccadilly)
(station liverpool_street central)
(station liverpool_street circle)
(station liverpool_street metropolitan)
(station marylebone bakerloo)
(station mile_end central)
(station mile_end district)
(station mile_end metropolitan)
(station monument circle)
(station monument district)
(station monument escalator_between_bank_and_monument)
(station moorgate circle)
(station moorgate metropolitan)
(station moorgate northern_city)
(station neasden jubilee)
(station notting_hill_gate central)
(station notting_hill_gate circle)
(station notting_hill_gate district_west)
(station oxford_circus bakerloo)
(station oxford_circus central)
(station oxford_circus victoria)
(station paddington bakerloo)
(station paddington circle)
(station paddington district_west)
(station paddington metropolitan)
(station piccadilly_circus bakerloo)
(station piccadilly_circus piccadilly)
(station south_kensington circle)
(station south_kensington district)
(station south_kensington piccadilly)
(station stratford central)
(station stratford docklands_light_railway)
(station tottenham_court_road central)
(station tottenham_court_road northern_west)
(station tower_hill circle)
(station tower_hill district)
(station tower_hill docklands_light_railway)
(station victoria circle)
(station victoria district)
(station victoria gatwick_express)
(station victoria victoria)
(station warren_street northern_west)
(station warren_street victoria)
(station waterloo bakerloo)
(station waterloo northern_west)
(station waterloo waterloo_and_city)

(defun find-hard-trips ()
  (dolist (from *stations*)
    (dolist (to *stations*)
      (unless (or (eq from to)
		  (intersection
		    (station-lines from)
		    (station-lines to))
		  (intersecting-lines-p from to))
	(format t "~&From ~A to ~A~%" from to)))))

(defun intersecting-lines-p (from to)
  (dolist (from-line (station-lines from))
    (dolist (to-line (station-lines to))
      (when (intersection (line-stations from-line)
			  (line-stations to-line))
	(return-from intersecting-lines-p t)))))
  

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
