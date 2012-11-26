;;;; File: gpdt3-matrix-support.lisp
;;; Contains: Support code for a "go places and do things" Web demo
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: April 1998
;;; Updated: Wed Jun 23 22:45:55 1999 by Jeff Dalton
;;; Copyright: (c) 1997, 1998, AIAI, University of Edinburgh

(in-package :oplan)


;;;; The demo class

(defclass gpdt3-demo (matrix-server-demo)
  ((tf-file :initform "gpdt3.tf")
   (tf-url  :initform (web-demo-url "gpdt3.tf"))))

(in-local-defun-class gpdt3-demo *demo*)


;;;; Parameters

(defparameter *max-objectives* 5)


;;; Query-arg types

(define-query-arg-name-type :city (|| abyss barnacle calypso delta exodus))

(define-query-arg-name-type :weather (clear rain storm))

(define-query-arg-name-type :road-status (open landslide flooding))

(define-query-arg-type :action (argd argval)
  (funcall (get :text 'query-arg-converter) argd argval))


;;; There are no task parameters

(defparameter *task-parameters* nil)


;;; Situation-definition parameters

(defparameter *situation-definition-parameters*
  '((:weather                (:weather)     "weather")
    (:time-limit             (:int 0)       "time limit (in hours)")
    (:road-delta-abyss       (:road-status) "road status")
    (:road-abyss-barnacle    (:road-status) "road status")
    (:road-barnacle-calypso  (:road-status) "road status")
    (:road-calypso-delta     (:road-status) "road status")
    (:road-abyss-exodus      (:road-status) "road status")))

(defun enumerate-situation-parameter-values (argd)
  (let ((type-name (q-arg-type-name argd)))
    (if (query-arg-name-type-p type-name)
	(query-arg-name-type-values type-name)
      ;; Anything not a name type is a special case
      (ecase (q-arg-name argd)
	(:time-limit
	 (loop for i from 3 to 48 by 3 collect i))))))


;;; COA-definition parameters

;;; /\/: The *max-objectives* should not be built-in.

(defun gen-coa-objective-parameters (n)
  `((,(coa-arg-key n "ACTION") (:text)       "objective action")
    (,(coa-arg-key n "CITY")   (:city)       "objective city")))

(defparameter *coa-objective-parameters*
  (append '((:n    (:int 0)    "COA number"))
	  (loop for i from 1 to *max-objectives*
		append (gen-coa-objective-parameters i))))

(defparameter *coa-definition-parameters*
  (append *coa-objective-parameters*
	  *situation-definition-parameters*))

(defparameter *coa-addition-parameters*
  (mapcar #'(lambda (argd)
	      (if (eq (q-arg-name argd) :n)
		  argd
		(make-q-arg 
		  :name (q-arg-name argd)
		  :type `(:optional ,(q-arg-type argd))
		  :description (q-arg-description argd))))
	  *coa-objective-parameters*))

(defun properly-defined-objective-p (coa objective-number)
  (let ((action (coa-arg coa objective-number :action))
	(city (coa-arg coa objective-number :city)))
    (not (or (null action)
	     (null city)
	     (equal action  "")
	     (eq city '||)))))


;;;; Initialization

; (load "random/eval-once.lisp")

(defun-for gpdt3-demo initialize-matrix-demo ()
  ; (eval-once gpdt3-ta-message-logging (web-note-ta-messages))
  (setq *task-query-args* (default-situation-args))
  (set-parameter :oplan-tf-dir (web-demo-filename ""))
  ;; Make a note of messages to and from the TA
  ; (web-note-ta-messages)
  ;; Plan for an empty task.
  (assert (plan-for (demo-tf-file *demo*) :empty))
  ;; Check that we have the expected option: option-1.
  (send-to-oplan :get-option)
  (receive-else-error '(:option option-1)))

(defun default-situation-args ()
  (alist->hash-table
    '((:weather                . clear)
      (:time-limit             . 24   )
      (:road-delta-abyss       . open )
      (:road-abyss-barnacle    . open )
      (:road-barnacle-calypso  . open )
      (:road-calypso-delta     . open )
      (:road-abyss-exodus      . open ))
    :test #'eq))


;;;; Matrix page items

;;; Buttons

(defun-for gpdt3-demo write-matrix-page-buttons ()
  (html-matrix-button-bar
    `(("Restart"            ,(server-restart-url))
      ("Help"               ,(web-demo-url "gpdt3-help.html"))
      ("Map"                ,(web-demo-url "image/pacifica-map.html"))
      ("Scenario"           ,(web-demo-url "gpdt3-scenario.html"))
      ("TF file"            ,(demo-tf-url  *demo*))
      ("Server status"      ,(path-action-url :server-status))
      ("Select COAs"        ,(path-action-url :coa-select))
      ("Select evaluations" ,(path-action-url :eval-select))
      ("Exit"               ,(path-action-url :exit))
      )))


;;;; COA description table

(defparameter *min-objective-columns* 5)

(defun-for gpdt3-demo write-coa-description-table ()
  (let ((max-objectives
	 (max (find-max-coa-objectives) *min-objective-columns*)))
    (when (some #'coa-defined-p *coas*)
      (html-tag-line "h2" "COA objectives")
      (html-aiai-table ("width=100%")
        ;; Header row
        (html-item "tr"
          (html-item "th" "COA")
	  (loop for i from 1 to max-objectives do
	     (html-item "th"
	       (html-line "Objective ~D" i))))
	;; COA description rows
	(do-visible-coas (coa)
	  (when (coa-defined-p coa)
	    (write-coa-description-row coa max-objectives t))))
      ;; Explain the gray backgrounds, if any
      (when (some #'coa-actions-to-add *coas*)
	(html-small-right-explanation
	  "Shaded objectives are not yet in a plan")))))

(defun write-coa-description-row (coa max-objectives show-coa-p)
  (html-item "tr"
    (when show-coa-p
      (html-item "td align=center"
	(html-format "~A" (coa-visible-number coa))))
    (let ((old (coa-added-actions coa))
	  (new (coa-actions-to-add coa)))
      (loop for i from 1 to max-objectives do
	(cond (old
	       (let ((act (pop old)))
		 (html-item "td align=left"
		   (html-line (action-description-string act)))))
	      (new
	       (let ((act (pop new)))
		 ;; Give it a [was: very] light grey background.
		 ;; Used to use #eeeeee.  Silver = #c0c0c0.
		 ;; Austin's "metal gray" is "#cccccc".
		 (html-item `("td align=left" ("bgcolor" ,*aiai-metal-gray*))
		   (html-line (action-description-string act)))))
	      (t
	       (html-empty-td)))))))

(defun action-description-string (action)
  ;; The action is a list of symbols, which have upper case names
  ;; internally.
  (concat-strings-with-separator " "
    (mapcar #'(lambda (sym)
		(replace-subseq " " "_"
		  (string-downcase sym :start 1)))
	    action)))

(defun find-max-coa-objectives ()
  (max-value
    #'(lambda (coa)
	(+ (length (coa-added-actions coa))
	   (length (coa-actions-to-add coa))))
    *coas*))


;;;; Situation table


(defun-for gpdt3-demo
    write-situation-description-table (&key (include-default t))
  (unless (or include-default (some #'coa-defined-p *coas*))
    (return-from write-situation-description-table))
  (html-tag-line "h2" "COA initial situations")
  (html-aiai-table ("width=100%")
    ;; A row containing the query arg names as headings
    (html-item "tr"
      (html-item "th" "COA")
      (dolist (argd *situation-definition-parameters*)
	(html-item "th align=center"
          (html-format "~A" (keyword->text (q-arg-name argd))))))
    ;; If requested, a row giving the default values.
    (when include-default
      (html-item "td align=center"
        (html-anchor (path-action-url :situation-def) "Default"))
      (dolist (argd *situation-definition-parameters*)
	(html-item "td align=center"
	  (html-format "~A" (task-arg (q-arg-name argd))))))
    ;; One row per COA to give the parameter values.
    (do-visible-coas (coa)
      (when (coa-defined-p coa)
	(html-item "tr"
	  (html-item "td align=center"
	    (html-format "~A" (coa-visible-number coa)))
	  (dolist (argd *situation-definition-parameters*)
	    (html-item "td align=center"
	      (html-format "~A"
		(situation-arg coa (q-arg-name argd)))))))))
  (when include-default
    (html-small-right-explanation
      "The default is used as a base for any new COA.")))


;;;; Changing the default initial situation

(defun write-situation-definition-page ()
  (html-standard-page 
      (:title (title-for-user "Define Default Initial Situation")
       :centered-title-header t)
    (html-form "post" (path-action-url :def-situation)
      (write-situation-definition-table nil)
      (html-line "<p>")
      (html-matrix-form-buttons "Define situation" :return-to :focus))))

(defun set-situation ()
  (parse-query-args)
  (convert-query-args *situation-definition-parameters*)
  (setq *task-query-args* (copy-hash-table *query-arg-table* :test #'eq)))


;;;; COA definition page

(defun-for gpdt3-demo write-coa-definition-page-buttons ()
  (html-button-bar
    `(("Simple process editor"
       ,(web-demo-url "matrix-cpe/gpdt3-ref.cgi"))
      ("&lt;I-N-OVA&gt; process editor screen"
       "http://www.dai.ed.ac.uk/students/stevep/cpe/coa.gif"))))

;;; COA definition table

;;; /\/: At present, we always present a table in which 5 objectives can
;;; be defined, and there is no way to add more.

(defparameter *gpdt-objective-cities*
  '("Abyss" "Barnacle" "Calypso" "Exodus"))

(defparameter *gpdt-objective-actions*
  '("evacuate injured"
    "evacuate population"
    "evacuate with medical team"
    "send medical supplies"
    "send emergency food"
    "send medical team"
    "repair gas leak"
    "defuse terrorist bomb"
    "build emergency housing"
    "repair power station turbine"
    "provide immediate assistance"
    "shut down power station"))

(defun-for gpdt3-demo write-coa-definition-table
     (coa &key (n-objectives *max-objectives*)
	       (allow-defaults t)
	  &aux (mrd-coa *most-recently-defined-coa*))
  ;; Default COA objectives from the most recently defined COA.
  (when (and mrd-coa (coa-defined-p mrd-coa))
    (setf (coa-parameters coa)
	  (coa-definition-parameters mrd-coa)))
  ;; Now write the table.
  (html-tag-line "h2" "Objectives")
  (html-block "table"
    (loop for i from 1 to n-objectives do
      (html-item "tr"
        (html-item "td"
	  (cond ((not allow-defaults)
		 (html-gpdt-objective-inputs i))
		((and mrd-coa (properly-defined-objective-p coa i))
		 ;; Default to the most recent defintion, if there is one.
		 (html-gpdt-objective-inputs i
		   (coa-arg coa i :action)
		   (string-capitalize (coa-arg coa i :city))))
		((= i 1)
		 ;; Else, a nonempty default for 1st objective,
		 (html-gpdt-objective-inputs 1
		   (first *gpdt-objective-actions*)
		   (first *gpdt-objective-cities*)))
		(t	
		 ;; and an empty default for the others.
		 (html-gpdt-objective-inputs i))))))))

(defun html-gpdt-objective-inputs (objective-number &optional action city)
  ;; Number
  (html-tag "b" "~D" objective-number)
  ;; Action
  (html-select-from-list
    :name (coa-arg-name objective-number "action")
    :size 1
    :options
      (mark-selected action
	`("" ,@*gpdt-objective-actions*)))
  ;; City
  (html-select-from-list
    :name (coa-arg-name objective-number "city")
    :size 1
    :options
      (mark-selected city
	`("" ,@*gpdt-objective-cities*))))


;;; Situation definition table

;;; /\/: Ought to be able to get the values in a query-arg-name-type.

(defun write-situation-definition-table (coa)
  ;; The coa can be nil.
  (declare (ignore coa))
  (html-tag-line "h2" "Situation")
  (html-aiai-table ("width=100%")
    ;; A row containing the query arg names as headings
    (html-item "tr"
      (dolist (argd *situation-definition-parameters*)
	(html-item "th align=center"
          (html-format "~A" (keyword->text (q-arg-name argd))))))
    ;; A row of input items
    (html-item "tr"
      (dolist (argd *situation-definition-parameters*)
        (html-item "td align=center"
	  (write-situation-parameter-definition-item argd))))))

(defun write-situation-parameter-definition-item (argd)
  (html-select-from-list
    :name (q-arg-name argd)
    :size 1
    :options
     (mark-selected
       (task-arg (q-arg-name argd))
       (enumerate-situation-parameter-values argd))))


;;;; Constraint-addition page

;;; /\/: Should allow the user to change any already-added constraints
;;; that have not yet been put into a plan.

(defun-for gpdt3-demo write-constraint-addition-page 
     (&optional (coa (get-coa (query-arg :n))))
  (let ((id (coa-id coa))
	(n (coa-number coa)))
    (set-user-focus-page *user* (title-for-coa coa "Add to Task")
      (coa-path-action-url :add-def coa))
    (html-standard-page
        (:title (title-for-user-and-coa coa "Add to Task")
	 :centered-title-header t)
      (write-coa-definition-page-buttons)
      (html-form "post" (path-action-url :def-add)
        (html-line "<input type=\"hidden\" name=\"n\" value=\"~D\">" id)
	(write-coa-definition-table coa :n-objectives 3 :allow-defaults nil)
	(html-line "<p>")
	(html-matrix-form-buttons (format nil "Add to COA ~A" n)))
      (html-line "<hr>")
      (write-coa-description-table)
      (write-situation-description-table))))


;;;; Tasking constraints

;;; Initial effects

(defun-for gpdt3-demo get-coa-initial-effects (coa)
  (append
    (get-coa-initial-weather-effects coa)
    (get-coa-initial-road-status-effects coa)
    (get-coa-initial-location-and-status-effects coa)))

(defun get-coa-initial-weather-effects (coa)
  (list (pv-pair '(weather) (situation-arg coa :weather))))

(defun get-coa-initial-road-status-effects (coa)
  (loop for p in *situation-definition-parameters*
	for name = (symbol-name (q-arg-name p))
	when (sequence-begins "ROAD-" name)
	collect
	  (let ((parts (break-string-at #\- name)))
	    (pv-pair `(road_status
		       ,(concat-name (second parts) "_" (third parts)))
		     (situation-arg coa (q-arg-name p))))))

(defun get-coa-initial-location-and-status-effects (coa)
  (declare (ignore coa))
  (loop for property in '(location available empty_vehicle)
	as  value    in '(Delta    true      true)
	collect
	  (pv-pair `(,property |??|) value)))

;;; Task actions

(defun-for gpdt3-demo get-coa-task-actions (coa)
  (loop for ttd from 1 while (coa-arg-exists coa ttd :action)
	when (properly-defined-objective-p coa ttd)
	collect
	  (list (string->name
		  (replace-subseq "_" " " (coa-arg coa ttd :action)))
		(coa-arg coa ttd :city))))


;;;; COA selection

(defun-for matrix-server-demo write-coa-selection-page-buttons ()
  (html-button-bar
    `(  ("Help"                    ,(web-demo-url "gpdt3-help.html"))
        ("Map"                     ,(web-demo-url "image/pacifica-map.html"))
        ("Scenario"                ,(web-demo-url "gpdt3-scenario.html"))
        ("Server status"           ,(path-action-url :server-status))
      )))


;;;; The Planner's matrix page

(defun-for gpdt3-demo write-planning-page-buttons ()
  (html-matrix-button-bar
    `(  ("Help"                    ,(web-demo-url "gpdt3-help.html"))
        ("Map"                     ,(web-demo-url "image/pacifica-map.html"))
        ("Scenario"                ,(web-demo-url "gpdt3-scenario.html"))
        ("TF file"                 ,(demo-tf-url  *demo*))
        ("Server status"           ,(path-action-url :server-status))
        ("Select evaluations"      ,(path-action-url :eval-select))
        ("Logout"                  ,(path-action-url :exit))
      )))


;;;; Question-answering

(defun-for gpdt3-demo write-question-page-buttons (coa)
  (html-button-bar
    `(("Map"                     ,(web-demo-url "image/pacifica-map.html"))
      ("TF file"                 ,(demo-tf-url *demo*))
      ("Server status"           ,(path-action-url :server-status))
      ("Change planner settings" ,(coa-path-action-url :plan-settings coa))
      )))


;;;; Returning plans to the TA

(defun-for gpdt3-demo write-return-selection-page-buttons ()
  (html-button-bar
    `(  ("Help"                    ,(web-demo-url "gpdt3-help.html"))
        ("Map"                     ,(web-demo-url "image/pacifica-map.html"))
        ("Scenario"                ,(web-demo-url "gpdt3-scenario.html"))
        ("Server status"           ,(path-action-url :server-status))
      )))


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;

