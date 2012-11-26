;;;; File: coa-matrix-support.lisp
;;; Contains: Support code for COA matrix Web demos
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: May 1997
;;; Updated: Tue Feb 22 02:49:38 2000 by Jeff Dalton
;;; Copyright: (c) 1997, AIAI, University of Edinburgh

(in-package :oplan)


(proclaim '(special *task-parameters*
		    *coa-definition-parameters*))


;;;; CGI entry point

;;; /\/: We actually need something better than a lock file.  Suppose
;;; the user follows a link in such a way that it's displayed in a
;;; new browser window.  We can end up with two windows that want to
;;; modify the same data.  The lock file prevents them from doing it
;;; at the same time, but they could still both do it.  What we want
;;; is for only one of them to have the right to change anything.
;;; We might do it by passing around a "timestamp" that's incremented
;;; by every CGI invocation.  It would also be stored in a file.  
;;; A cgi invocation would compare the timestamp it was given (as a
;;; query or path arg) with the one in the file, and signal an error
;;; if the one in the file was newer.  Only the latest page (matrix
;;; page or COA definition form or whatever) would have the up-to-date
;;; timestamp to pass as a query or path arg.  Older pages would pass
;;; an older timestamp.

(defvar *session-id* nil)
(defvar *path-action* nil)
(defvar *path-args* nil)

(defun coa-matrix-cgi (demo-name)
  (with-web-environment demo-name
    (get-session-id-and-action)
    (handler-case
        (with-lock-file (session-filename "lock")
	  (obey-path-action)
	  (save-session-data))
      (lockout ()
	(error "One at a time, please.")))))

(defun obey-path-action ()
  (ecase *path-action*
    (:new-matrix    (write-matrix-page))
    (:select-evals  (write-eval-selection-page))
    (:set-evals     (set-evaluations)
		    (write-matrix-page))
    (:select-coas   (write-coa-selection-page))
    (:set-coas      (set-coas)
		    (write-matrix-page))
    (:new-coa       (new-coa)
		    (write-matrix-page))
    (:coa-def-form  (which-coa)
		    (write-coa-definition-page))
    (:define-coa    (define-coa)
		    (write-matrix-page))
    (:authority     (which-coa)
		    (write-authority-settings-page))
    (:plan          (which-coa)
		    (plan-for-coa)
		    (write-matrix-page))
    (:view-plan     (which-coa)
		    (write-coa-plan-view-page))
    (:explain       (which-coa)
		    (write-eval-explanation-page))
    (:def-situation (write-situation-definition-page))
    (:set-situation (set-situation)
		    (write-matrix-page))))


;;; By the time get-session-id-and-action returns, everything has been
;;; set up in a standard way that works for both starting and continuing
;;; a session.

(defun get-session-id-and-action ()
  (let ((path (get-path-info)))
    (cond ((null path)
	   ;; Begin a new session.
	   (setq *session-id* *web-demo-id*)
	   (setq *path-action* :new-matrix)
	   (initialize-session-data))
	  (t
	   ;; Continue a session.
	   (setq *session-id* (first path))
	   (setq *path-action*
		 (string->keyword (string-upcase (second path))))
	   (setq *path-args* (nthcdr 2 path))
	   (restore-session-data)))))


;;;; Path-action URLs

(defun path-action-url (action &optional arg-string)
  (web-cgi-url 
    (apply #'concat-string
	   *web-demo-name* ".cgi"
	   "/" *session-id*
	   "/" (string-downcase action)
	   (if arg-string
	       `("/" ,arg-string)
	     '()))))

;;; Coa-path-action-url is used for a link that is a command to be
;;; performed on a particular COA.

;;; Which-coa takes a path-arg added by coa-path-action-url and assigns
;;; it to query-arg :n.  [A query-arg is used partly for historical
;;; reasons -- see below].  This is how "which coa" is sent from one
;;; CGI invocation to the next when the user follows a link to define
;;; a coa, to ask for a plan, or to view plan results.

;;; Form submissions (e.g. from the COA definition form) pass "which coa"
;;; as query-arg :n, in the way form submissions normally pass query
;;; args, but as a hidden input.

(defun coa-path-action-url (action coa-number)
  (path-action-url action (format nil "~D" coa-number)))

(defun which-coa ()
  (assert *path-args*)
  (setf (query-arg :n) (string->int (first *path-args*))))

;;; /\/: Tried using query args on the end, instead of more path, but
;;; though it worked on Spottisvax, it didn't at the Institute.

#|
(defparameter *coa-path-action-parameters*
  '((:n    (:int 0)    "COA number")))

(defun path-action-url (action &optional query-arg-string)
  (web-cgi-url 
    (apply #'concat-string
	   *web-demo-name* ".cgi"
	   "/" *session-id*
	   "/" (string-downcase action)
	   (if query-arg-string
	       `("?" ,query-arg-string)
	     '()))))

(defun coa-path-action-url (action coa-number)
  (path-action-url action (format nil "n=~D" coa-number)))

(defun which-coa ()
  (parse-query-args)
  (convert-query-args *coa-path-action-parameters*))
|#


;;;; COA data

(defvar *coas* nil)

(defstruct coa
  number				;an int >= 1
  parameters				;hash-table-alist from def form
  plan-view-url				;a URL
  evaluations				;alist of (eval-name . value) pairs
  )

(defun get-coa (n)
  (or (nth (1- n) *coas*)
      (error "There is no COA ~S" n)))

(defun new-coa ()
  (let ((c (make-coa :number (1+ (length *coas*)))))
    (nconcf *coas* (list c))
    c))


;;; Getting coa parameters

;;; E.g. (coa-arg coa 1 :city) looks up the :m1-city parameter.

(defun coa-arg (coa mission-number name)
  (lookup
    (coa-arg-key mission-number name)
    (coa-parameters coa)))

(defun coa-arg-key (mission-number name)
  (string->keyword (coa-arg-name mission-number name)))

(defun coa-arg-name (mission-number name)
  (concat-string "M" (int->string mission-number) "-" name))


;;; The initial situation applies to all missions, hence no mission parameter.

(defun situation-arg (coa name)
  (lookup name (coa-parameters coa)))


;;; Per-COA files and the corresponding URLs

(defun coa-filename (coa name)
  (session-filename (format nil "coa-~D-~A" (coa-number coa) name)))

(defun coa-filename-url (coa name)
  (session-filename-url (format nil "coa-~D-~A" (coa-number coa) name)))


;;; Evaluations

;;; *Evaluations* lists the evals that _can_ be visible.  There may be
;;; other evals, not in the list.  *Visible-evaluations* lists the evals
;;; that will currently be shown.

(defparameter *evaluations*
  '((:number-of-nodes    "nodes in plan")
    (:plan-length        "longest path length")
    (:duration           "minimum duration")
    (:n-psv-object-types "object types")
    (:n-psv-values       "object values")))

(defparameter *visible-evaluations* *evaluations*)

(defstruct (plan-eval (:type list))
  name					;a keyword
  description)				;a string


;;;; Session data

;;; File names and corresponding URLs

(defvar *session-directory-name* 'not-a-session-directory-name)
(defvar *session-directory-url*  'not-a-session-directory-url )

(defun set-session-directory-and-url ()
  ;; Pretend session id is demo id
  (let ((*web-demo-id* *session-id*))
    ;; Construct names using the usual functions.
    (setq *session-directory-name* (web-tmp-filename "coas" "d"))
    (setq *session-directory-url* (web-tmp-url "coas" "d"))))

(defun make-session-directory ()
  (assert (string= *session-id* *web-demo-id*))
  (set-session-directory-and-url)
  (assert (not (probe-file *session-directory-name*)))
  (system (format nil "mkdir ~A" *session-directory-name*))
  (assert (probe-file *session-directory-name*))
  (system (format nil "chmod a+w ~A" *session-directory-name*))
  *session-directory-name*)

(defun session-filename (name)
  (concat-string *session-directory-name* "/" name))

(defun session-filename-url (name)
  (concat-string *session-directory-url* "/" name))

;;; Task-args  /\/: init-args?  Args from the initial invocation, in any case.

(defvar *task-query-args* nil)

(defun task-arg (name) (gethash name *task-query-args*))

;;; Initialization

(defun initialize-session-data ()
  ;; Get task query-args.
  (parse-query-args)
  (convert-query-args *task-parameters*)
  (setq *task-query-args* *query-arg-table*)
  ;; Make the session directory
  (make-session-directory)
  ;; Initialize COA data
  (assert (null *coas*))
  (new-coa)
  )

;;; Save

(defun save-session-data ()
  (with-open-file (data (session-filename "data")
		        :direction :output
			:if-does-not-exist :create
			:if-exists :supersede)
    (print-readably (hash-table-alist *task-query-args*) data)
    (terpri data)
    (print-readably *visible-evaluations* data)
    (terpri data)
    (print-readably *coas* data)
    (terpri data)))

;;; Restore

(defun restore-session-data ()
  (set-session-directory-and-url)
  (with-open-file (data (session-filename "data")
			:direction :input
			:if-does-not-exist :error)
    (setq *task-query-args* (alist->hash-table (read-safely data) :test 'eq))
    (setq *visible-evaluations* (read-safely data))
    (setq *coas* (read-safely data))
    (assert (every #'coa-p *coas*))))


;;;; HTML output


;;; Page footer

(defun coa-matrix-demo-page-foot ()
  (html-line "<hr>")
  (html-anchor "http://www.aiai.ed.ac.uk/"
    (format nil
      "<img width=82 height=37 alt=\"AIAI\" align=right border=0 src=~S>"
      ;; was src=\"http://www.aiai.ed.ac.uk/img/logo/small.gif\"
      (web-demo-url "image/aiai-small.gif")))
  (html-block "Address"
    (html-anchor "/~oplan" "O-Plan")))

(advice-replace 'html-standard-page-foot :coa-matrix-demo
  #'coa-matrix-demo-page-foot)


;;;; Matrix page

;;; /\/: Bat suggested "O-Plan COA Comparison Matrix"

(defun write-matrix-page ()
  (html-standard-page (:title "O-Plan COA Evaluation Matrix"
		       :title-header nil)
    (html-block "center"
      (html-tag-line "h1" "O-Plan COA Evaluation Matrix"))
    (write-matrix-page-buttons)
    (html-line "<p>")			;just for some whitespace
    (html-block "center"
      (html-block "table cellspacing=0"
        (html-item "tr"
          (html-item "td valign=top"
            (write-coa-matrix))
          (html-item "td valign=top"
            (html-box
              (html-anchor (path-action-url :new-coa) "Add COA"))))))
    (write-coa-description-table)
    (write-situation-description-table)
    (html-line "<p>")))			;to get some white space /\/


;;; The Matrix

(defun write-coa-matrix ()
  (html-aiai-table ()
    (write-coa-matrix-top-row)
    (write-coa-matrix-authority-row)
    (write-coa-matrix-plan-row)
    (dolist (e *visible-evaluations*)
      (write-coa-matrix-eval-row e))
    (write-coa-matrix-view-row)))

(defun write-coa-matrix-top-row ()
  (html-item "tr align=center"
    (html-item "th align=right" "Define task:")
    (dolist (coa *coas*)
      (html-item "td"
	(html-anchor
	  (coa-path-action-url :coa-def-form (coa-number coa))
	  (format nil "COA-~D" (coa-number coa)))))))

(defun write-coa-matrix-authority-row ()
  (html-item "tr align=center"
    (html-item "th align=right" "Set authority:")
    (dolist (coa *coas*)
      (html-item "td"
	(if (coa-parameters coa)
	    (html-anchor
	      (coa-path-action-url :authority (coa-number coa))
	      "Auth")
	  (html-line "."))))))

(defun write-coa-matrix-plan-row ()
  (html-item "tr align=center"
    (html-item "th align=right" "Generate plan:")
    (dolist (coa *coas*)
      (html-item "td"
	(if (coa-parameters coa)
	    (html-anchor
	      (coa-path-action-url :plan (coa-number coa))
	      "Plan")
	  (html-line "."))))))

(defun write-coa-matrix-eval-row (e)
  (html-item "tr align=center"
    (html-item "td align=right"
      (html-format "~A" (plan-eval-description e)))
    (dolist (coa *coas*)
      (if (null (coa-evaluations coa))
	  (html-item "td" ".")
	(html-item "td"
	  (write-eval-value-cell coa e))))))

(defun write-coa-matrix-view-row ()
  (html-item "tr align=center"
    (html-item "th align=right" "View plan:")
    (dolist (coa *coas*)
      (html-item "td"
        (if (coa-plan-view-url coa)
	    (html-anchor
	      (coa-plan-view-url coa)
	      (if (coa-evaluations coa) "View" "No plan"))
	  (html-line "."))))))

;;; Write the value of an evaluation.  For some evaluations, it is possible
;;; to "drill down" to some underlying information.  In such cases, the
;;; value is a link.

(defun write-eval-value-cell (coa e)
  (let ((value (lookup (plan-eval-name e) (coa-evaluations coa))))
    (case (plan-eval-name e)
      ((:number-of-nodes)
       (html-anchor (coa-filename-url coa "narrative.txt")
		    (princ-to-string value)))
      ((:n-psv-object-types
	:n-psv-values)
       (html-anchor (drill-down-url coa (plan-eval-name e))
		    (princ-to-string value)))
      (t
       (html-line "~A" value)))))


;;;; "Drill down" for (some) plan evaluations

(defun drill-down-url (coa eval-name)
  (path-action-url :explain (format nil "~D/~A" (coa-number coa) eval-name)))

(defun write-eval-explanation-page ()
  (let* ((coa (get-coa (query-arg :n)))
	 (eval-name (string->keyword (string-upcase (second *path-args*))))
	 (value (lookup eval-name (coa-evaluations coa))))
    (html-standard-page
        (:title (format nil ; "Explanation of COA ~D ~A"
			    "COA ~D ~A"
			(coa-number coa)
			(plan-eval-description
			  (assoc eval-name *evaluations*))))
      (flet ((explain-value-list (description eval-for-items)
	       (if (= value 0)
		   (html-line "There were no ~A." description)
		 (progn
		   (html-line "~D ~A:" value description)
		   (html-block "blockquote"
		     (html-line "~{~S~^, ~}."
		       (canonical-description-order
			 (lookup eval-for-items (coa-evaluations coa)))))))))
        (ecase eval-name
	  (:n-psv-object-types
	   (explain-value-list "object types" :psv-object-types))
	  (:n-psv-values
	   (explain-value-list "object values" :psv-values)))))))


;;;; Eval selection

(defun write-eval-selection-page ()
  (html-standard-page (:title "Select evaluation criteria")
    (html-form "post" (path-action-url :set-evals)
      (html-block "ul"
        (dolist (e *evaluations*)
          (html-item "dt"
            (html-checkbox (plan-eval-name e)
			   (member (plan-eval-name e) *visible-evaluations*
				   :key #'plan-eval-name))
	    (html-line (plan-eval-description e)))))
      (html-line "<p>")
      (html-line "<input type=\"submit\" value=\"Set criteria\">")
      (html-line "<input type=\"reset\"  value=\"Reset form\">")
      (html-paragraph
	"Initially, the criteria that are currently displayed in the matrix"
	"have their buttons in the \"yes\" (selected) position, and the"
	"criteria not being displayed have theirs in the \"no\" position."))))


;;;; Set evaluations

(defun set-evaluations ()
  (parse-query-args)
  (convert-query-args
    (mapcar #'(lambda (e) `(,(plan-eval-name e) (:checkbox)
			    ,(plan-eval-description e)))
	    *evaluations*))
  (setq *visible-evaluations*
	(loop for e in *evaluations*
	      when (query-arg (plan-eval-name e))
	      collect e)))


;;;; Set COAs

(defun set-coas ()
  (parse-query-args)
  (convert-query-args
    (mapcar #'(lambda (coa) `(,(keep-coa-arg coa) (:checkbox) "keep COA"))
	    *coas*))
  (setq *coas* (remove-if-not #'(lambda (coa) (query-arg (keep-coa-arg coa)))
			      *coas*))
  (loop for coa in *coas*
	as i from 1
	do (setf (coa-number coa) i)))

(defun keep-coa-arg (coa)
  (string->keyword (format nil "COA-~D-P" (coa-number coa))))


;;;; Define a COA

;;; Here's where we use the information from the COA definition form.

(defun define-coa ()
  (parse-query-args)
  (convert-query-args *coa-definition-parameters*)
  (let ((coa (get-coa (query-arg :n))))
    ;; Can now ask for a plan.  [Non-null coa-parameters causes
    ;; a "Plan" link to be shown in the matrix.]
    (setf (coa-parameters coa)
	  (hash-table-alist *query-arg-table*))
    ;; Cannot view the plan, because there isn't one,
    ;; even if this COA had a plan for some earlier defintion.
    (setf (coa-plan-view-url coa) 
	  nil)
    ;; Any existing evaluations are now invalid.
    (setf (coa-evaluations coa)
	  nil)
    coa))


;;;; Ask about authority

(defun write-authority-settings-page (&optional (coa (get-coa (query-arg :n))))

  (html-standard-page (:title (format nil "COA ~D authority" (coa-number coa)))

    (html-block "table"
      (html-item "tr align=center"
        (html-empty-td)
        (html-item "th" "Phase")
	(html-item "th" "Level"))
      (html-item "tr align=center"
        (html-item "th" "Plan")
	(html-item "td" "all")
	(html-item "td" "inf"))
      (html-item "tr align=center"
	(html-item "th" "Execute")
	(html-item "td" "all")
	(html-item "td" "no")))

    (html-paragraph
      "Authority cannot yet be altered in this demonstration.")))


;;;; Plan for a COA

;;; /\/: Since we reuse the results file, the URL stays the same and
;;; the browser may not realize it needs to get a new copy.

;;; After planning, (coa-plan-view-url coa) is a URL.  If there is np
;;; plan, (coa-evaluations coa) is nil; otherwise there is a plan, and
;;; (coa-evaluations coa) is a [presumably non-null] a-list.

(defparameter *coa-planning-time-limit* 60) ;seconds

(define-condition no-plan (simple-condition) ())

(defun plan-for-coa (&optional (coa (get-coa (query-arg :n))))
  ;; Clear any existing plan info
  (setf (coa-plan-view-url coa) nil
	(coa-evaluations coa) nil)
  (output-coa-task coa)
  (with-open-file (*html-out* (coa-filename coa "results.html")
			      :direction :output
			      :if-exists :supersede)
    (html-tag-line "title" "COA ~D plan views" (coa-number coa))
    (html-tag-line "h1"    "COA ~D plan views" (coa-number coa))
    (html-line "<hr>")
    (html-print-oplan-greeting)
    (handler-case (plan-coa coa)
      (timeout ()		  
	(error "Planning took more than the time limit of ~S seconds"
	       *coa-planning-time-limit*))
      (no-plan ()
	(html-line "<hr>")
	(report-no-coa-plan coa))
      (:no-error (result)
        (declare (ignore result))
	(html-line "<hr>")
	(report-coa-plan coa)))))

;;; Call the planner

(defun plan-coa (coa)
  ;; If we get this far, set the coa-plan-view-url
  (setf (coa-plan-view-url coa) (coa-filename-url coa "results.html"))
  ;; Try to find a plan
  (cond ((plan-for (coa-filename coa "task.tf")
		   (format nil "task_Pacifica_COA_~D" (coa-number coa)))
	 ;; Have a plan.
	 (setf (coa-evaluations coa)
	       (get-coa-plan-evaluations coa))
	 t)
	(t
	 ;; No plan.
	 ;; /\/: Could just return nil and check in the :no-error case.
	 (signal 'no-plan))))

#+:undef
(defun get-coa-plan-evaluations (coa)
  ;; /\/: Fake evaluations
  (setq *random-state* (make-random-state t))
  (mapcar #'(lambda (e)
	      (cons (plan-eval-name e) (random 100)))
	  *evaluations*))

(defun get-coa-plan-evaluations (coa)
  ;; /\/: May want to check that everything in *evaluations* is there.
  (declare (ignore coa))
  (send-to-oplan :eval-plan)
  (receive-else-error '(:evaluation $evaluations)
    #'(lambda (evals)
	(mapcar #'(lambda (e)
		    (let ((key (alist-key e))
			  (value (alist-value e)))
		      (cons key
			    (case key
			      (:duration
			       (let ((seconds (car value)))
				 ;; /\/: allow float rounded to 10ths?
				 (format nil "~D hrs" (/ seconds 60 60))))
			      (t value)))))
		evals))))


;;; No plan

(defun report-no-coa-plan (coa)
  (html-paragraph
    (html-line "No plan is possible."))
  (html-report-plan-statistics)
  (html-paragraph
    (html-line "Referece links:")
    (html-block "ul"
      (itemize-coa-tf-links coa)))
  (web-mail-comment-link)
  (web-note-failure "no plan was possible"))

;;; Have a plan

(defun report-coa-plan (coa)
  (html-report-plan-statistics)
  (html-paragraph
    (html-line "Results:")
    (html-block "ul"
      (itemize-coa-tf-links coa)
      (itemize-plan-descriptions coa)))
  (html-anchor (web-demo-url "output-formats.html")
    "Explain outputs")
  (html-line "<br>")
  (web-mail-comment-link)
  (web-note-success))

(defun itemize-coa-tf-links (coa)
  ;; The generated task
  (html-item "li"
    (html-anchor (coa-filename-url coa "task.tf")
		 "TF task definition"))
  ;; The TF file
  (html-item "li"
    (html-anchor (web-demo-url (concat-string *web-demo-name* ".tf"))
		 "Support TF file")))

(defun itemize-plan-descriptions (coa)

  ;; A PostScript graph

  (when t
    (request-psgraph :file-one-page
      :title (format nil "Pacifica COA ~D" (coa-number coa))
      :output-file (coa-filename coa "graph.ps"))
    (html-item "LI"
      (html-anchor (coa-filename-url coa "graph.ps")
		   "PostScript graph")))

  ;; Plan narrative

  (when t
    (request-plan-view
      :mode :narrative
      :levels :all
      :output-file (coa-filename coa "narrative.txt"))
    (html-item "LI"
      (html-anchor (coa-filename-url coa "narrative.txt")
		   "Plan narrative")))
  
  ;; World at end of node-2

  (when t
    (request-world-view "2"
      :mode :file
      :output-file (coa-filename coa "world-2.txt"))
    (html-item "LI"
      (html-anchor (coa-filename-url coa "world-2.txt")
		   "World state when the plan finishes"))))


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
