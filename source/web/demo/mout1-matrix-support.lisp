;;;; File: mout1-matrix-support.lsp
;;; Contains: Support code for mout1 matrix demo
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: September 1999
;;; Updated: Mon Oct 11 00:01:36 1999 by Jeff Dalton
;;; Copyright: (c) 1999, AIAI, University of Edinburgh

(in-package :oplan)


;;;; The demo class

(defclass mout1-demo (any-tf-matrix-demo)
  ())

(in-local-defun-class mout1-demo *demo*)


;;;; COA definition

;;; /\/: There should be a mixin that provides one-from-each-column
;;; menus.


;;; Query-arg types

(define-query-arg-name-type :mout1-platoon
  (platoon_1 platoon_2 platoon_4))

(define-query-arg-name-type :mout1-building
  (building_11 building_12 building_21 building_22 building_31
   building_32 building_33 building_34 building_35 building_41
   building_42 building_43 building_44 building_45 building_46))

(define-query-arg-name-type :mout1-toehold
  (building_11 building_21 building_22 building_31 building_34 
   building_35 building_42 building_46))

(define-query-arg-name-type :mout1-target
  (building_32 building_33 building_41))


;;; COA definition parameters

(defun generate-platoon-objective-parameters (i)
  ;; Args are platoon name and a corresponding "mission" number
  ;; to use when constructing query-arg names.
  `((,(coa-arg-key i "UNIT")    (:mout1-platoon)  "platoon")
    (,(coa-arg-key i "ACTION")  (:text)           "task")
    (,(coa-arg-key i "TOEHOLD") (:mout1-toehold)  "toehold")
    (,(coa-arg-key i "TARGET")  (:mout1-target)   "target")))

(defparameter *coa-definition-parameters*
  (append '((:n    (:int 0)    "COA number"))
	  (loop for p in (query-arg-name-type-values :mout1-platoon)
		and i from 1
		append (generate-platoon-objective-parameters i))))


;;; COA definition page

;;; /\/: This is just the one we'd inherit, but we'll want to change it.

(defun-local write-coa-definition-page (&optional (coa *coa*))
  (when (coa-defined-p coa)
    (bogus-matrix-request-error))
  (let ((id (coa-id coa))
	(n (coa-number coa)))
    (set-user-focus-page *user* (title-for-coa coa "Definition")
      (coa-path-action-url :coa-def coa))
    (html-standard-page 
        (:title (title-for-user-and-coa coa "Definition")
	 :centered-title-header t)
      (write-coa-definition-page-buttons)
      (html-form "post" (path-action-url :def-coa)
        (html-line "<input type=\"hidden\" name=\"n\" value=\"~D\">" id)
	(write-coa-definition-table coa)
	(write-situation-definition-table coa)
	(html-line "<p>")
	(html-matrix-form-buttons (format nil "Define COA ~A" n)))
      (html-line "<hr>")
      (write-coa-description-table)
      (write-situation-description-table))))


;;; COA definition table

(defparameter *mout1-objective-actions*
  '("rescue hostages"))

(defparameter *mout1-coa-1-objective-defaults*
  ;; This is predefined task_coa_1
  '((platoon_1 :target building_33 :toehold building_34)
    (platoon_2 :target building_32 :toehold building_21)
    (platoon_4 :target building_41 :toehold building_46)))

(defparameter *mout1-coa-3-objective-defaults*
  ;; This is predefined task_coa_3
  '((platoon_1 :target building_33 :toehold building_34)
    (platoon_2 :target building_32 :toehold building_31)
    (platoon_4 :target building_41 :toehold building_42)))

(defvar *mout1-objective-defaults* *mout1-coa-1-objective-defaults*)

(defun-local write-coa-definition-table (coa)
  (declare (ignore coa))
  (coa-definition-rolling-default)
  (html-tag-line "h2" "Objectives")
  (html-block "table"
    (html-item "tr"
      (html-item "th" "Unit")
      (html-item "th" "Task")
      (html-item "th" "Toehold")
      (html-item "th" "Target"))
    (loop for unit in (query-arg-name-type-values :mout1-platoon)
	  and i from 1
	  do
      (html-item "tr"
	(write-objective-definition-row unit i)))))

(defun coa-definition-rolling-default ()
  (let ((mrd *most-recently-defined-coa*))
    (when (and mrd (coa-defined-p mrd))
      (setq *mout1-objective-defaults*
	    (make-mout1-objective-defaults-from-coa mrd)))))

(defun make-mout1-objective-defaults-from-coa (coa)
  (loop for i from 1 while (coa-arg-exists coa i :action)
	collect
	  (list (coa-arg coa i :unit)
		:target (coa-arg coa i :target)
		:toehold (coa-arg coa i :toehold))))

(defun-local write-objective-definition-row (unit i)
  ;; Unit
  (html-item "td"
    (html-simple-list-select
      (coa-arg-name i "unit")
      (list unit)))			;don't let the user pick
  ;; Task
  (html-item "td"
    (html-simple-list-select
      (coa-arg-name i "action")
      *mout1-objective-actions*))
  ;; Toehold
  (html-item "td"
    (html-select-from-list
      :name (coa-arg-name i "toehold")
      :size 1
      :options
        (mark-selected (mout1-objective-default unit :toehold)
	  (query-arg-name-type-values :mout1-toehold))))
  ;; Target
  (html-item "td"
    (html-select-from-list
      :name (coa-arg-name i "target")
      :size 1
      :options
        (mark-selected (mout1-objective-default unit :target)
	  (query-arg-name-type-values :mout1-target)))))

(defun mout1-objective-default (unit arg-type)
  (let ((plist (lookup unit *mout1-objective-defaults*)))
    (getf plist arg-type)))

(defun html-simple-list-select (arg-name options)
  ;; Defaults to :size 1 and to 1st of options marked as selected.
  (html-select-from-list
    :name arg-name
    :size 1
    :options (mark-selected (first options) options)))


;;; COA description table

(defun-local write-coa-description-table ()
  ())


;;; Situation definition

;;; /\/: We don't have one yet.


;;;; COA definition computed values

;;; define-coa is the main routine.  Here we provide some routines that
;;; compute certain coa slot values.

;;; Task actions

;;; This one is easy!

(defun-local get-coa-task-actions (coa)
  (loop for i from 1 while (coa-arg-exists coa i :action)
	collect
	  (list (string->name
		  (replace-subseq "_" " " (coa-arg coa i :action)))
		(coa-arg coa i :unit)
		(coa-arg coa i :target)
		(coa-arg coa i :toehold))))


;;; Initial effects

;;; What a mess!

(defun-local get-coa-initial-effects (coa)
  `(
    ;; Hazards
    ((obstacles |??| |??|) ())		;no obstacles
    ((snipers   |??| |??|) ())		;no snipers

    ;; The incredibly annoying friendly_occupancy facts
    ,@(generate-initial-friendly-occupancy-facts coa)

    ;; Some standard stuff
    ((historic_interest building_33) valuable)
    ((location_of platoon_1)    landing_zone)
    ((location_of platoon_2)    landing_zone)
    ((location_of platoon_4)    landing_zone)
    ((status_afss platoon_1)    available)
    ((status_tacair platoon_1)  available)))

(defun-local generate-initial-friendly-occupancy-facts (coa)
  (loop for b in (query-arg-name-type-values :mout1-building)
	collect
	  (if (plan-to-occupy-building-p coa b)
	      `((friendly_occupancy ,b) planned)
	      `((friendly_occupancy ,b) not_planned))))

(defun-local plan-to-occupy-building-p (coa building)
  (loop for i from 1 while (coa-arg-exists coa i :action)
	thereis (or (eq building (coa-arg coa i :toehold))
		    (eq building (coa-arg coa i :target)))))


;;;; Add map and scenario links to some button bars

;;; /\/: There ought to be a general way to add map and scenario links.

(defun mout1-map-url ()
  (web-demo-url "image/mout1-map.html"))

(defun mout1-scenario-url ()
  (web-demo-url "mout1-scenario")
  #+:undef
  "http://www.aiai.ed.ac.uk/~johnl/mout/")


;;; The TA's martrix page

(defun-local write-matrix-page-buttons ()
  (html-matrix-button-bar
    `(("Restart"            ,(server-restart-url))
      ("Map"                ,(mout1-map-url))
      ("Scenario"           ,(mout1-scenario-url))
      ("TF file"            ,(demo-tf-url *demo*))
      ("Server status"      ,(path-action-url :server-status))
      ("Select COAs"        ,(path-action-url :coa-select))
      ("Select evaluations" ,(path-action-url :eval-select))
      ("Exit"               ,(path-action-url :exit))
      )))

;;; The COA definition page

(defun-local write-coa-definition-page-buttons ()
  (html-button-bar
    `(("Map"        ,(mout1-map-url))
      ("Scenario"   ,(mout1-scenario-url))
      ("TF file"    ,(demo-tf-url *demo*))
      ("Simple process editor"
       ,(web-demo-url "matrix-cpe/gpdt3-ref.cgi"))
      ("&lt;I-N-OVA&gt; process editor screen"
       "http://www.dai.ed.ac.uk/students/stevep/cpe/coa.gif"))))


;;; COA selection

(defun-local write-coa-selection-page-buttons ()
  (html-button-bar
    `(  ("Map"                     ,(mout1-map-url))
        ("Scenario"                ,(mout1-scenario-url))
        ("Server status"           ,(path-action-url :server-status))
      )))

;;;; The Planner's matrix page

(defun-local write-planning-page-buttons ()
  (html-matrix-button-bar
    `(  ("Map"                     ,(mout1-map-url))
        ("Scenario"                ,(mout1-scenario-url))
        ("TF file"                 ,(demo-tf-url  *demo*))
        ("Server status"           ,(path-action-url :server-status))
        ("Select evaluations"      ,(path-action-url :eval-select))
        ("Logout"                  ,(path-action-url :exit))
      )))

;;;; Question-answering

(defun-local write-question-page-buttons (coa)
  (html-button-bar
    `(("Map"                     ,(mout1-map-url))
      ("Scenario"                ,(mout1-scenario-url))
      ("TF file"                 ,(demo-tf-url *demo*))
      ("Server status"           ,(path-action-url :server-status))
      ("Change planner settings" ,(coa-path-action-url :plan-settings coa))
      )))

;;;; Returning plans to the TA

(defun-local write-return-selection-page-buttons ()
  (html-button-bar
    `(  ("Help"                    ,(mout1-map-url))
        ("Map"                     ,(mout1-scenario-url))
        ("Scenario"                ,(web-demo-url "gpdt3-scenario.html"))
        ("Server status"           ,(path-action-url :server-status))
      )))

;;; Exec control page

(defun-local write-exec-control-page-buttons (coa)
  (html-button-bar
    `(("Map"                     ,(mout1-map-url))
      ("Scenario"                ,(mout1-scenario-url))
      ("TF file"                 ,(demo-tf-url *demo*))
      ("Server status"           ,(path-action-url :server-status))
      ("Plan views"              ,(coa-path-action-url :view-plan coa))
      ("Current state"           ,(coa-path-action-url :exec-world-state coa))
      ("Quit execution"          ,(path-action-url :quit-exec)))))


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
