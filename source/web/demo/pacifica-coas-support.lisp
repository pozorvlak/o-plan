;;;; File: pacifica-coas-support.lsp
;;; Contains: Support code for the Pacifica COA matrix Web demo
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: May 1997
;;; Updated: Thu Jun  3 18:06:03 1999 by Jeff Dalton
;;; Copyright: (c) 1997, AIAI, University of Edinburgh

(in-package :oplan)


;;;; CGI entry point

(defun pacifica-coas-cgi ()
  (coa-matrix-cgi "pacifica-coas"))


;;;; Parameters

;;; Task parameters

(defparameter *task-parameters*
  '((:weather                (:weather)     "weather")
    (:road-delta-abyss       (:road-status) "road status")
    (:road-abyss-barnacle    (:road-status) "road status")
    (:road-barnacle-calypso  (:road-status) "road status")
    (:road-calypso-delta     (:road-status) "road status")
    (:n-gts                  (:int 0 2)     "number of GTs")
    (:n-helicopters          (:int 0 2)     "number of helicopters")))

(define-query-arg-name-type :weather (clear rain storm))

(define-query-arg-name-type :road-status (open landslide flooding))

;;; COA-definition parameters

(defun gen-coa-mission-parameters (n)
  (flet ((mkey (suffix) (coa-arg-key n suffix)))
    `((,(mkey "P")      (:checkbox)   ,(format nil "include mission ~D" n))
      (,(mkey "ACTION") (:text)       "mission action")
      (,(mkey "CITY")   (:city)       "mission city")
      (,(mkey "METHOD") (:text)       "mission method"))))

(defparameter *coa-definition-parameters*
  (append '((:n    (:int 0)    "COA number"))
	  (gen-coa-mission-parameters 1)
	  (gen-coa-mission-parameters 2)))

(define-query-arg-name-type :city (abyss barnacle calypso delta))


;;;; Matrix page items

;;; buttons

(defun write-matrix-page-buttons ()
  (html-button-bar
   `(("Help"               ,(web-demo-url "pacifica-coas-help.html"))
;    ("New task"           ,(web-special-url "pacifica-coas.html"))
     ("Start over"         ,(web-special-url "pacifica-coas.html"))
     ("Map"                ,(web-demo-url "image/pacifica-map.gif"))
     ("View TF file"       ,(web-demo-url "pacifica-coas.tf"))
     ("Select evaluations" ,(path-action-url :select-evals))
     ("Select COAs"        ,(path-action-url :select-coas))
     ("New COA"            ,(path-action-url :new-coa)))))

;;; COA description table

(defun write-coa-description-table ()
  (when (some #'coa-parameters *coas*)
    (html-tag-line "h2" "COA descriptions")
    (html-block "table border=1 cellspacing=0"
      (dolist (coa *coas*)
        ;; COA description row
	(when (coa-parameters coa)
          (html-item "tr"
            (html-item "td align=right" (html-format "~D" (coa-number coa)))
	    (write-coa-mission-cell coa 1)
	    (write-coa-mission-cell coa 2)))))))

(defun write-coa-mission-cell (coa mission-number)
  (if (coa-arg coa mission-number :p)
      (html-item "td"
	(html-line "~A ~A ~A"
		   (coa-arg coa mission-number :action)
		   (coa-arg coa mission-number :city)
		   (coa-arg coa mission-number :method)))
    (html-empty-td)))


;;; Initial situation table

(defun write-situation-description-table ()
  (html-tag-line "h2" "Initial situation")
  (html-aiai-table ()
    ;; Weather
    (html-item "tr" 
      (html-item "th align=left" "Weather")
      (html-item "td align=center colspan=3"
	(html-tag "i" "~A" (task-arg :weather))))
    ;; Resources at Delta
    (html-item "tr"
      (html-item "th align=left colspan=4" "Resources at Delta"))
    (html-item "tr"
      (html-item "td" "Ground transports")
      (html-table-task-value :n-gts)
      (html-item "td" "Helicopters")
      (html-table-task-value :n-helicopters))
    ;; Road status
    (html-item "tr"
      (html-item "th align=left colspan=4" "Road status"))
    (write-road-status-rows)
    ))

(defun write-road-status-rows ()
  (dolist (row '((:road-delta-abyss      :road-abyss-barnacle)
		 (:road-barnacle-calypso :road-calypso-delta )))
    (html-item "tr"
      (dolist (arg row)
        (html-item "td"
	  (html-line "~:(~{~A~^ - ~}~)"
		     (rest (break-string-at #\- (string arg)))))
	(html-table-task-value arg)))))

(defun html-table-task-value (arg)
  (html-item "td align=center"
    (html-tag "i" "~A" (task-arg arg))))


;;;; COA definition form

(defun write-coa-definition-page (&optional (coa-number (query-arg :n)))

  (html-tag-line "title" "COA ~D definition" coa-number)
  (html-tag-line "h1"    "COA ~D definition" coa-number)

  (html-form "post" (coa-path-action-url :define-coa coa-number)
    (html-mission-inputs 1)
    (html-mission-inputs 2)
    (html-line "<input type=\"hidden\" name=\"n\" value=\"~D\">" coa-number)
    (html-line "<p>")
    (html-line "<input type=\"submit\" value=\"Define COA ~D\">" coa-number)
    (html-line "<input type=\"reset\">"))

  (html-line "<hr>")
  (write-coa-description-table)
  (write-situation-description-table)
  (html-line "<hr>"))

(defun html-mission-inputs (m)
  (html-tag-line "h2" "Mission ~D" m)
  (html-block "table"
    (html-block "tr"
      (html-block "td"
        (html-checkbox (coa-arg-name m "p") :checked))
      (html-block "td"
        (html-select-from-list
	   :name (coa-arg-name m "action")
	   :size 1
	   :options '((:selected "Evacuate injured from")
		      "Take a medical team to"))
	(html-select-from-list
	  :name (coa-arg-name m "city")
	  :size 1
	  :options '((:selected "Abyss")
		     "Barnacle"
		     "Calypso"))
	(html-select-from-list
	  :name (coa-arg-name m "method")
	  :size 1
	  :options '((:selected "using a GT")
		     "using a helicopter"
		     "in any way possible"))))))


;;;; TF output

(defun output-coa-task (coa)
  (with-open-file (*tf-out* (coa-filename coa "task.tf")
			    :direction :output
			    :if-exists :supersede)
    (output (:stream *tf-out*)
      // ";;; COA task"
      //
      // "types ground_transport = " (generate-coa-type coa :gt) ","
      // "      helicopter       = " (generate-coa-type coa :h ) ";"
      //
      // "initially" (:include (output-coa-initial-effects coa)) ";"
      //
      // "task Pacifica_COA_" (coa-number coa) ";"
      // "  nodes"
      // "    sequential"
      // "      1 start,"
                (:include (output-coa-task-actions coa))
      // "      2 finish"
      // "    end_sequential;"
          (:include (output-coa-task-conditions coa))
      // "end_task;"
      //
      // "include \"" (web-demo-filename "pacifica-coas") "\";"
      //)))

;;; Types

(defun generate-coa-type (coa type-key)
  (declare (ignore coa))
  (ecase type-key
    (:gt
     (loop for i from 1 to (task-arg :n-gts)
	   collect (concat-string "GT" (int->string i))))
    (:h
     (loop for i from 1 to (task-arg :n-helicopters)
	   collect (concat-string "H" (int->string i))))))

;;; "Initially" effects

(defun output-coa-initial-effects (coa)
  (output-comma-separated "     "
    (append
      (generate-coa-initial-weather-effects coa)
      (generate-coa-initial-road-status-effects coa)
      (generate-coa-type-initial-effects coa :gt)
      (generate-coa-type-initial-effects coa :h))))

(defun generate-coa-initial-weather-effects (coa)
  (declare (ignore coa))
  (list (format nil "{weather} = ~A" (task-arg :weather))))

(defun generate-coa-initial-road-status-effects (coa)
  (declare (ignore coa))
  (loop for p in *task-parameters*
	for name = (symbol-name (q-arg-name p))
	when (sequence-begins "ROAD-" name)
	collect
	  (let ((parts (break-string-at #\- name)))
	    (format nil "{road_status ~:(~A_~A~)} = ~A"
		    (second parts) (third parts)
		    (task-arg (q-arg-name p))))))

(defun generate-coa-type-initial-effects (coa type-key)
  (loop for name in (generate-coa-type coa type-key)
	collect
	 (format nil "{location ~A} = Delta" name)))

;;; Task actions

(defun output-coa-task-actions (coa)
  (let ((actions (generate-coa-task-actions coa)))
    (when actions
      (output // "      parallel")
      (output-comma-separated "        "
	(loop for action in actions
	      as i from 3
	      collect (format nil "~D action ~A" i action)))
      (output // "      end_parallel,"))))

(defun generate-coa-task-actions (coa)
  (loop for mission from 1 to 2
	unless (string= (coa-arg coa mission :method) "in any way possible")
	collect
	  (format nil "{~A ~A ~A}"
		  (coa-arg coa mission :action)
		  (coa-arg coa mission :city)
		  (coa-arg coa mission :method))))

;;; Task conditions

(defun output-coa-task-conditions (coa)
  (let ((conds (generate-coa-task-conditions coa)))
    (when conds
      (output // "  conditions")
      (output-comma-separated "    " conds ";"))))

(defun generate-coa-task-conditions (coa)
  (loop for mission from 1 to 2
	when (string= (coa-arg coa mission :method) "in any way possible")
	collect
	  (format nil "achieve {~A ~A} at 2"
	      (let ((action (coa-arg coa mission :action)))
		(cond ((string= action "Evacuate injured from")
		       "evacuated injured from")
		      ((string= action "Take a medical team to")
		       "medical_team_in")
		      (t
		       (error "Don't recognize action ~S" action))))
	      (coa-arg coa mission :city))))


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
