;;;; File: matrix-server-support.lsp
;;; Contains: Support code for COA matrix Web demos
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: April 1998
;;; Updated: Sun Feb 13 21:48:31 2000 by Jeff Dalton
;;; Copyright: (c) 1997, 1998, 1999, AIAI, University of Edinburgh

(in-package :oplan)

(in-local-defun-class matrix-server-demo *demo*)


;;; /\/: In theory, this file contains (only) code that is not domain-
;;; specific, and there's a separate per-domain file that contains the 
;;; domain-specific stuff.


;;;; Query-arg types

(define-query-arg-type :level (argd argval)
  (funcall (get :int* 'query-arg-converter) argd argval 0))


;;;; Matrix page

;;; /\/: Bat suggested "O-Plan COA Comparison Matrix"

(defun write-matrix-page ()
  (assert (eq (user-name *user*) :task-assigner))
  (set-user-focus-page *user* "Matrix" (path-action-url :matrix))
  (html-standard-page (:title (title-for-user "COA Evaluation Matrix")
		       :centered-title-header t)
    (write-matrix-page-buttons)
    (html-line "<p>")			;just for some whitespace /\/
    (html-block "center"
      (html-block "table cellspacing=0"
        (html-item "tr"
          (html-item "td valign=top"
            (write-coa-matrix))
          (html-item "td valign=top"
            (html-box
              (html-anchor (path-action-url :new-coa) "Add COA"))))))
    (write-coa-description-table)
    (write-situation-description-table)))

;;; The coa- and situation-description tables are optional

(defun-for matrix-server-demo write-coa-description-table ()
  nil)

(defun-for matrix-server-demo write-situation-description-table
       (&key (include-default t))
  (declare (ignore include-default))
  nil)


;;; Matrix-page buttons

;;; Here we provide a default that demos can override.

(defun-for matrix-server-demo write-matrix-page-buttons ()
  (html-matrix-button-bar
    `(("Restart"            ,(demo-restart-url))
      ("TF file"            ,(demo-tf-url *demo*))
      ("Server status"      ,(path-action-url :server-status))
      ("Select COAs"        ,(path-action-url :coa-select))
      ("Select evaluations" ,(path-action-url :eval-select))
      ("Exit"               ,(path-action-url :exit))
      )))

(defun-for matrix-server-demo demo-restart-url ()
  (server-restart-url))

(defun html-matrix-button-bar (buttons)
  ;; Adds a "Reload" button on the right, separated from the other
  ;; buttons, by having a two-item row with a 90% width td for the
  ;; main button list and a 10% td for the "Reload".
  ;; A button is (name url).
  ;; /\/: The height=100% is suppose to keep them the same height
  ;; if the page is too narrow for all text to fit on one line.
  ;; But it doesn't seem to work.
  (html-block "table width=100% cellspacing=0 cellpadding=0"
    (html-item "tr align=center valign=center"
      (html-item "td height=100% width=90%"
        (html-button-bar buttons))
      (html-item "td height=100% width=10%"
	(html-button-bar
	  `(("Reload" ,(path-action-url :matrix))))))))


;;; Matrix cells

;;; This is a mechanism that automatically grays-out cells for ghost COAs.

(defmacro html-matrix-cell ((coa &rest keyword-args) &body forms)
  `(do-html-matrix-cell
      ,coa
      #'(lambda () ,@(process-html-body-forms forms))
      ,@keyword-args))

(defun do-html-matrix-cell (coa thunk &key attributes (ghost-text "."))
  (if (coa-ghost-p coa)
      (html-item "td bgcolor=\"#eeeeee\""
	(html-line "~A" ghost-text))
    (do-html-block 
      (if attributes (concat-string "td " attributes) "td")
      thunk)))


;;; The Matrix

(defun-for matrix-server-demo write-coa-matrix ()
  (html-aiai-table ()
    (write-coa-matrix-top-row)
    (write-coa-matrix-split-row)
    (write-coa-matrix-add-constraints-row)
    (write-coa-matrix-authority-row)
    (write-coa-matrix-plan-row)
    (write-coa-matrix-eval-rows)
    (write-coa-matrix-issues-row) ; /\/: Or should issues be another eval row?
    (write-coa-matrix-view-row)
    ))

(defun write-coa-matrix-top-row (&key (title "Define task:")
				      (definition-allowed-p t))
  (html-item "tr align=center"
    (if title
	(html-item "th align=right" (html-line title))
      (html-empty-td))
    (do-visible-coas (coa)
      (let ((coa-name (coa-visible-name coa)))
	(html-item "td"
	  (if (or (coa-defined-p coa) (not definition-allowed-p))
	      (html-format coa-name)  ;not a link: don't allow redefinition /\/
	    (html-anchor
	      (coa-path-action-url :coa-def coa)
	      coa-name)))))))

(defun write-coa-matrix-split-row ()
  (html-item "tr align=center"
    (html-item "th align=right" "Split COA:")
    (do-visible-coas (coa)
      (html-matrix-cell (coa)
	(if (coa-active-p coa)
	    (html-anchor
	      (coa-path-action-url :split coa)
	      "Split")
	  (html-line "."))))))

(defun write-coa-matrix-add-constraints-row ()
  ;; It says "Add to task" to help distinguish the constraints added
  ;; here from the ones on the planning page.
  (html-item "tr align=center"
    (html-item "th align=right" "Add to task:")
    (do-visible-coas (coa)
      (html-matrix-cell (coa)
	(if (coa-active-p coa)
	    (html-anchor
	      (coa-path-action-url :add-def coa)
	      "Add")
	  (html-line "."))))))

(defun write-coa-matrix-authority-row ()
  (html-item "tr align=center"
    (html-item "th align=right" "Set authority:")
    (do-visible-coas (coa)
      (html-matrix-cell (coa)
	(cond ((not (coa-active-p coa))
	       (html-line "."))
	      ((makes-sense-to-change-authority-for-coa-p coa)
	       (html-anchor
		 (coa-path-action-url :authority coa)
		 "Auth"))
	      ((and (eq (coa-plan-status coa) :complete)
		    (null (coa-actions-to-add coa)))
	       (html-line "Auth"))
	      (t
	       (error "Unclear status for COA ~A" (coa-number coa))))))))

(defun makes-sense-to-change-authority-for-coa-p (coa)
  ;; coa-active-p and ...
  (or (member (coa-plan-status coa) '(nil :partial))
      (coa-actions-to-add coa)))

(defun write-coa-matrix-plan-row ()
  (html-item "tr align=center"
    (html-item "th align=right" "Generate plan:")
    (do-visible-coas (coa)
      (html-matrix-cell (coa)
	(cond ((not (coa-active-p coa))
	       (html-line "."))
	      ((makes-sense-to-plan-for-coa-p coa)
	       (html-anchor
		 (coa-path-action-url :plan coa)
		 "Plan"))
	      ((not (null (coa-plan-status coa)))
	       ;; Was (coa-added-actions coa) when the only demo was gpdt3 /\/
	       (html-line "Plan"))	;but not a link
	      (t
	       (html-line ".")))))))

(defun makes-sense-to-plan-for-coa-p (coa)
  ;; coa-active-p and ...
  (or (null (coa-plan-status coa))	;assume there's something /\/
      (coa-actions-to-add coa)		;e.g. gpdt3 /\/
      (coa-has-increased-authority-p coa)))

(defun coa-has-increased-authority-p (coa)
  (and (coa-authority coa)
       (coa-authority-history coa)
       (greater-autority-p (coa-authority coa)
			   (first (coa-authority-history coa)))))

(defun greater-autority-p (a1 a2)
  (&> (or (lookup :level a1) :inf)
      (or (lookup :level a2) :inf)))

(defun write-coa-matrix-eval-rows ()
  (dolist (e (user-visible-evaluations *user*))
    (write-coa-matrix-eval-row e))
  ;; Now, if there are any invisible evaluations, add an "Other evaluations"
  ;; row.
  #+:undef
  (maybe-write-coa-matrix-other-evals-row))

(defun write-coa-matrix-eval-row (e)
  (html-item "tr align=center"
    (html-item "td align=right"
      (html-format "~A" (plan-eval-description e)))
    (do-visible-coas (coa)
      (if (null (coa-evaluations coa))
	  (html-matrix-cell (coa) ".")
	(write-matrix-eval-cell coa e)))))

(defun maybe-write-coa-matrix-other-evals-row ()
  (let ((invisible-evals (user-invisible-evaluations *user*)))
    (when invisible-evals
     (html-item "tr align=center"
       (html-item "td align=right"
	 "Other evaluations")
       (do-visible-coas (coa)
	 (if (null (coa-evaluations coa))
	     (html-matrix-cell (coa) ".")
	   ;; Now it's somewhat like an issues cell
	   (write-coa-matrix-other-evals-cell coa invisible-evals)))))))

(defun write-coa-matrix-other-evals-cell (coa invisible-evals)
  (let* ((invisible-issues
	  (loop for e in invisible-evals
		for v = (coa-plan-eval-result coa e)
		append (eval-result-issues v)))
	 (outstanding
	  (remove-if #'plan-issue-done-p invisible-issues))
	 (color
	  (status-color->value (issue-status-color outstanding)))
	 (value
	  (length outstanding)))
    (html-matrix-cell (coa :attributes (format nil "bgcolor=~S" color))
      ;; No drill-down for now.  /\/
      ; (html-line "~A issue~:P" value)
      (html-line "~A" value))))

(defun write-coa-matrix-view-row ()
  (html-item "tr align=center"
    (html-item "th align=right" "View plan:")
    (do-visible-coas (coa)
      (html-matrix-cell (coa)
	(cond ((have-plan-for-coa-p coa)
	       (html-anchor (coa-path-action-url :view-plan coa) "View"))
	      ((plan-failure-for-coa-p coa)
	       (html-anchor (coa-path-action-url :no-plan coa) "No Plan"))
	      (t
	       (html-line ".")))))))

(defun write-coa-matrix-issues-row ()
  (html-item "tr align=center"
    (html-item "th align=right" "Address issues:")
    (do-visible-coas (coa)
      (if (have-plan-for-coa-p coa)	;but someday issues outside evals?  /\/
	  (write-matrix-issues-cell coa)
	(html-matrix-cell (coa) ".")))))

(defun write-matrix-issues-cell (coa)
  (let* ((issues (coa-outstanding-plan-issues coa))
	 (color (status-color->value (issue-status-color issues)))
	 (value (length issues)))
    (html-matrix-cell (coa :attributes (format nil "bgcolor=~S" color))
      (if (coa-plan-issues-p coa)	;any at all, outstanding or not
	  (html-anchor (coa-path-action-url :issues coa)
		       (int->string value))
	(html-line "0")))))


;;; Write the value of an evaluation.  For some evaluations, it is possible
;;; to "drill down" to some underlying information.  In such cases, the
;;; value is a link.

(defun write-matrix-eval-cell (coa e)
  (let* ((value (coa-plan-eval-result coa e))
	 (tag (eval-result-short-description value))
	 (issues (eval-result-issues value))
	 (color (status-color->value (issue-status-color issues))))
    (html-matrix-cell (coa :attributes (format nil "bgcolor=~S" color))
      (cond ((eval-result-url value)
	     ;; Emit a link to the URL
	     (html-anchor (eval-result-url value)
			  tag))
	    ((or issues (eval-result-report-fn value))
	     ;; Something we might drill down to.
	     (html-anchor (drill-down-url coa (plan-eval-name e))
			  tag))
	    (t
	     ;; No drill-down.
	     (html-line "~A" tag))))))


;;;; "Drill down" for (some) plan evaluations

(defun drill-down-url (coa eval-name)
  (path-action-url :explain (format nil "~D/~A" (coa-id coa) eval-name)))

(defun write-eval-explanation-page ()
  (let* ((coa (get-coa (query-arg :n)))
	 (eval-name (string->keyword (second *path-args*)))
	 (value (lookup eval-name (coa-evaluations coa))))
    (unless (eval-result-p value)
      (bogus-matrix-request-error))
    (html-standard-page
        (:title (title-for-user-and-coa coa
		  (capitalize-for-title
		    (plan-eval-description (assoc eval-name *evaluations*))))
	 :centered-title-header t)
      ;; Call the report-fn if there is one.
      (when (eval-result-report-fn value)
	(apply (eval-result-report-fn value)
	       (eval-result-report-args value)))
      ;; Describe issues if there are any, unless we're in the middle
      ;; of :question-answering.  Some evals are used as views at such
      ;; times.
      (when (and (eval-result-issues value)
		 (not (eq (coa-plan-status coa) :question)))
        (write-issue-description-table
          coa (eval-result-issues value))))))

(defun write-issue-description-table (coa issues)
  (let ((to-do (remove-if #'plan-issue-done-p issues))
	(done (remove-if-not #'plan-issue-done-p issues)))
    (when to-do
      (html-tag-line "h2" "Outstanding issues")
      (html-form "get" (coa-path-action-url :issues-handled coa)
	(html-block "table border=1 cellspacing=0"
	  (dolist (i to-do)
	    ;; Issue description row
	    (html-item "tr"
	      (html-item "td"
		(html-checkbox (plan-issue-query-arg i)
			       nil))
	      (html-item "td"
                (write-plan-issue-description i)))))
	(html-line "<p>")
	(html-matrix-form-buttons "Mark issues handled" :return-to :focus)))
    (when done
      (html-tag-line "h2" "Resolved issues")
      (html-block "table border=1 cellspacing=0"
        (dolist (i done)
	  ;; Issue description row
	  (html-item "tr"
            (html-item "td"
	      (write-plan-issue-description i))))))))

(defun plan-issue-query-arg (i)
  (string->keyword (concat-string "ISSUE-" (int->string (plan-issue-id i)))))

(defun write-plan-issue-description (i)
  (if (eq (car (plan-issue-contents i)) 'note)
      (html-format "Note:~@(~{ ~A~}~)" (cdr (plan-issue-contents i)))
    (html-format "~@(~{~A~^ ~}~)" (plan-issue-contents i))))

(defun mark-handled-plan-issues ()
  ;; N.B. parse-query-args 1st clears the arg table.
  (let* ((coa (get-coa (query-arg :n)))
	 (issues (coa-plan-issues coa)))
    (parse-query-args)
    (dolist (i issues)
      (when (query-arg (plan-issue-query-arg i))
	(setf (plan-issue-done-p i) t)))))

(defun write-issue-description-page ()
  (let ((coa (get-coa (query-arg :n))))
    (html-standard-page 
        (:title (title-for-user-and-coa coa "Issues")
		:centered-title-header t)
      (write-issue-description-table
        coa (coa-plan-issues coa)))))


;;;; COA definition page

(defun-for matrix-server-demo write-coa-definition-page (&optional (coa *coa*))
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

;;; The demo should define (write-coa-definition-table coa).
;;; There are defaults for everything else.

(defun-for matrix-server-demo write-situation-definition-table (coa)
  (declare (ignore coa))
  nil)

(defun-for matrix-server-demo write-coa-definition-page-buttons ()
  (html-button-bar
    `(("TF file"
       ,(demo-tf-url *demo*))
      ("Simple process editor"
       ,(web-demo-url "matrix-cpe/gpdt3-ref.cgi"))
      ("&lt;I-N-OVA&gt; process editor screen"
       "http://www.dai.ed.ac.uk/students/stevep/cpe/coa.gif"))))


;;;; COA definition

;;; define-coa is the main routine.  Here we provide some defaults
;;; for routines that compute certain coa slot values.

(defun-for matrix-server-demo get-coa-initial-effects (coa)
  (declare (ignore coa))
  '())

(defun-for matrix-server-demo get-coa-task-actions (coa)
  (declare (ignore coa))
  '())


;;;; Constraint-addition page

(defun-for matrix-server-demo write-constraint-addition-page 
     (&optional (coa (get-coa (query-arg :n))))
  (declare (ignore coa))
  (write-not-yet-page))


;;;; COA selection

;;; The TA's equivalent of the Planner user's ability to select which
;;; plans to return.

(defun write-coa-selection-page ()
  (set-user-focus-page *user* "COA Selection" (path-action-url :coa-select))
  (html-standard-page (:title (title-for-user "COA Selection")
		       :centered-title-header t)
    (write-coa-selection-page-buttons)
    (html-line "<p>")			;just for some whitespace /\/
    (html-form "post" (path-action-url :set-coas)
      (html-block "center"
        ; (write-coa-selection-table)
	(write-coa-selection-coa-matrix)
        (html-matrix-form-buttons "Submit selections")))
    (html-line "<hr>")
    (progn ; html-block "center"
      ; (html-tag-line "h2" "COA descriptions")
      ; (write-coa-selection-coa-matrix)
      (write-coa-description-table)
      (write-situation-description-table :include-default nil))))

(defparameter *coa-selection-options*
  '("Keep"
    "Delete"))

(defun select-coa-arg (coa)
  (string->keyword (format nil "COA-~D-P" (coa-number coa))))

(defun write-coa-selection-table ()
  (html-block "table"
    (do-visible-coas (coa)
      (html-item "tr"
	(html-item "td"
          (html-line "<b>COA-~A</b>" (coa-number coa)))
	(html-item "td"
	  (html-select-from-list
	    :name (select-coa-arg coa)
	    :size 1
	    :options (coa-selection-options coa)))))))

(defun coa-selection-options (coa)
  (declare (ignore coa))
  (mark-selected "Keep" *coa-selection-options*))

(defun-for matrix-server-demo write-coa-selection-page-buttons ()
  (html-button-bar
    `(  ("TF file"                 ,(demo-tf-url *demo*))
	("Server status"           ,(path-action-url :server-status))
      )))

(defun write-coa-selection-coa-matrix ()
  (html-aiai-table ()
    (write-coa-matrix-top-row :title nil :definition-allowed-p nil)
    (write-coa-matrix-eval-rows)
    (write-coa-matrix-view-row)
    (write-coa-matrix-coa-selection-row)))

(defun write-coa-matrix-coa-selection-row ()
  (html-item "tr align=center"
    (html-item "th align=right" "Select COA:")
    (do-visible-coas (coa)
      (html-matrix-cell (coa)
	(html-select-from-list
	  :name (select-coa-arg coa)
	  :size 1
	  :options (coa-selection-options coa))))))

;;; Set COA selection

(defun set-coa-selection ()
  (parse-query-args)
  (convert-query-args
    (mapcar #'(lambda (coa)
		`(,(select-coa-arg coa) (:optional (:text)) "coa-option"))
	    (visible-coas)))
  (do-visible-coas (coa)
    (let ((option (query-arg (select-coa-arg coa))))
      (cond ((null option))
	    ((string-equal option "Keep"))
	    ((string-equal option "Delete")
	     (setf (coa-visibility coa) nil))
	    (t
	     (error "Don't recognize coa selection option ~S for COA ~A"
		    option
		    (coa-number coa)))))))


;;;; Eval selection

;;; Lets the user pick which evaluations are visible.

(defun write-eval-selection-page ()
  (html-standard-page (:title (title-for-user "Select Evaluation Criteria")
		       :centered-title-header t)
    (html-form "post" (path-action-url :set-evals)
      (html-block "ul"
        (dolist (e *evaluations*)
          (html-item "dt"
            (html-checkbox (plan-eval-name e) (visible-evaluation-p e))
	    (html-line (plan-eval-description e)))))
      (html-line "<p>")
      (html-matrix-form-buttons "Set criteria")
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
  (setf (user-visible-evaluations *user*)
	(loop for e in *evaluations*
	      when (query-arg (plan-eval-name e))
	      collect e)))


;;;; Server status

(defun write-server-status-page ()

  (html-standard-page (:title (title-for-user "Server Status")
			      :centered-title-header t)
    (html-button-bar
      `(("Colour editor" ,(path-action-url :color-editor))))

    ;; O-Plan version
    (html-block "p"
      (html-print-oplan-greeting))

    ;; Lisp version
    (html-block "pre"
      (html-line "Using ~A ~A"
	(html-encode-pre-string (lisp-implementation-type))
	(html-encode-pre-string (lisp-implementation-version))))

    ;; Demo class
    (html-block "p"
      (html-line "Demo class: ~A" (class-name (class-of *demo*))))

    ;; Connected users
    (html-tag-line "h2" "Users")
    (write-user-address-table)

    ;; Time limit
    (html-tag-line "h2" "Parameters")
    (html-block "p"
      (html-line "Will exit if idle for: ~A"
		 (seconds->description *http-request-timeout*)))

    ;; Option tree
    (let ((option-tree
	   (progn
	     (send-to-oplan :get-option-tree)
	     (receive-else-error '(:option $tree) #'(lambda (tree) tree)))))
      (html-tag-line "h2" "Option tree")
      (html-pre-box
        (print-option-tree-description
	   option-tree *html-out*)))

    ;; COA histories
    (html-tag-line "h2" "COA histories")
    (write-coa-history-table)

    ))

(defun write-user-address-table ()
  (let ((lookup-is-possible-p nil))
    (html-aiai-table ()
      (html-item "tr align=center"
        (html-item "th" "Role")
        (html-item "th" "Address"))
      (loop for (role . address) in *role-address-alist* do
	(ensuref lookup-is-possible-p (not (have-host-name-p address)))
        (html-item "tr align=left"
          (html-item "td"
	    (html-line "~:(~A~)" role))
	  (html-item "td"
	    (html-host-address address)))))
    (when lookup-is-possible-p
      (html-small-left-explanation
        "Address lookup can be slow"))))

(defun write-coa-history-table ()
  (html-aiai-table ()
    (html-item "tr align=center"
      (html-item "th" "COA")
      (html-item "th" "Id")
      (html-item "th" "History"))
    (dolist (coa *coas*)
      (html-item "tr valign=middle"
        (html-item "td" (html-format "~A" (coa-number coa)))
	(html-item "td" (html-format "~A" (coa-id coa)))
	(if (coa-history coa)
	    (html-item "td"
	      (write-coa-history coa))
	  (html-empty-td))))))

(defun write-coa-history (coa)
  (html-block "pre"
    ""					;/\/ symmetry
    (let ((*print-pretty* t))
      (html-line "~{~S~^~%~}" (coa-history coa)))))

;;; Host / address information

(defun html-host-address (addr)
  (if (have-host-name-p addr)
      ;; We already know the name.
      (html-line "~A (~A)"
		 (get-host-name addr)		;e.g. spottisvax
		 (address-to-string addr)) 	;e.g. 192.168.1.1
    ;; We don't yet know the name.
    ;; Since it can be slow to look the name up, we won't do it
    ;; unless asked explicitly (by the user following the link).
    (html-anchor
      (http-hostname-url addr)
      (address-to-string addr))))

(defun http-hostname-url (addr)
  (path-action-url :hostname (int->string addr)))

(defun lookup-host-and-redirect-to-status-page ()
  (let ((addr (string->int (first *path-args*))))
    (get-host-name addr)
    (send-http-redirection-response *http-request* *http-io*
      :status-code 302			;moved temporarily
      :to (path-action-url :server-status))))


;;;; Color editor

(defparameter *color-editor-parameters*
  '((:green          (:hex-string)  "green status colour")
    (:orange         (:hex-string)  "orange status colour")
    (:red            (:hex-string)  "red status colour")
    (:ta-highlights  (:hex-string)  "TA's highlight colour")
    (:pl-highlights  (:hex-string)  "Planner's highlight colour")))

(define-query-arg-type :hex-string (argd argval)
  ;; Converts to e.g. "#ff3366".
  (if argval
      (if (string= "" (string-trim "0123456789AaBbCcDdEeFf" argval))
	  (concat-string "#" argval)
	(error "Not a valid hex string for ~A: ~S"
	       (q-arg-description argd) argval))
    (error "No value for ~S." (q-arg-description argd))))

(defun write-color-editor-page ()
  (html-standard-page (:title (title-for-user "Colour editor")
			      :centered-title-header t)
    (html-form "post" (path-action-url :edit-colors)
      (write-color-editing-table)
      (html-matrix-form-buttons "Change colours"
	:return-to :focus))))

(defun write-color-editing-table ()
  (html-block "table border=1 cellspacing=0 cellpadding=5"
    (html-item "tr"
      (html-item "th" "Colour")
      (html-item "th" "Value"))
    (dolist (p *color-editor-parameters*)
      (html-item "tr"
        (html-item `("td" (bgcolor ,(color-value (q-arg-name p))))
          (html-line "~A" (q-arg-description p)))
	(html-item "td"
	  (html-line "<input name=~S value=~S size=6 maxlength=6>"
	    (string (q-arg-name p))
	    (subseq (color-value (q-arg-name p)) 1))))))) ; remove the #

(defun edit-colors ()
  (parse-query-args)
  (convert-query-args *color-editor-parameters*)
  (dolist (p *color-editor-parameters*)
    (let ((name (q-arg-name p)))
      (set-color-value name (query-arg name))))
  ;; Pick up new values from current user.
  (mapc #'(lambda (e)
	    (set (alist-key e) (alist-value e)))
	(user-env *user*)))

(defun color-value (name)
  (ecase name
    ((:green :orange :red)
     (status-color->value name))
    ((:ta-highlights)
     (user-env-symbol-value *ta-user* '*html-table-highlight-color*))
    ((:pl-highlights)
     (user-env-symbol-value *planner-user* '*html-table-highlight-color*))))

(defun set-color-value (name value)
  (ecase name
    ((:green :orange :red)
     (setf (cdr (assoc name *status-colors*)) value))
    ((:ta-highlights)
     (set-user-highlight-color *ta-user* value))
    ((:pl-highlights)
     (set-user-highlight-color *planner-user* value))))

(defun set-user-highlight-color (user value)
  (dolist (var '(*html-table-highlight-color*
		 *html-button-bar-background-color*))
    (setf (user-env-symbol-value user var) value)))


;;;; Farewell

(defun write-farewell-page ()

  (html-standard-page (:title "Bye")

    (write-farewell-page-buttons)

    (html-paragraph
      "Thank you for using O-Plan.")))

(defun-for matrix-server-demo write-farewell-page-buttons ()
  (html-button-bar
    `(("O-Plan"              "http://www.aiai.ed.ac.uk/~oplan/")
      ("Web demos"           ,(web-special-url ""))
      ("Start a new server"  ,(web-special-url
			       (concat-string *web-demo-name* ".html"))))))


;;;; Ask about authority

(defun write-authority-settings-page (&optional (coa (get-coa (query-arg :n))))

  (html-standard-page
      (:title (title-for-user-and-coa coa "Authority")
       :centered-title-header t)

    (html-form "post" (path-action-url :set-authority)

      (html-line "<input type=\"hidden\" name=\"n\" value=\"~D\">"
	(coa-id coa))

      (html-headed-box ("Phase and level authority")
	(html-line "<p>")
	(html-center
	  (html-box
	    (write-authority-table (coa-authority coa)))
	  (write-authority-phase-and-level-form-inputs coa)
	  (write-action-level-table)
	  (html-anchor (path-action-url :action-levels)
	    "<small>Expand table</small>")))

      (html-center
        (html-matrix-form-buttons "Set authority")))))

(defun write-authority-phase-and-level-form-inputs (coa)
  (html-line "Grant authority to expand actions at levels up to")
  (let ((auth (or (lookup :level (coa-authority coa)) :inf)))
    (when (eq auth :inf)
      (setq auth 'inf))			;/\/
    (html-select-from-list
      :name :level
      :size 1
      :options
         (mark-selected auth
	   ;; /\/: should ask O-Plan for the levels.
	   '(0 1 2 3 inf)))))

(defun write-authority-table (authorities)
  (html-block "table"
    (html-item "tr align=center"
      (html-empty-td)
      (html-item "th" "Phase")
      (html-item "th" "Level"))
    (html-item "tr align=center"
      (html-item "th" "Plan")
      (html-item "td"			;phase
	(html-line "~(~A~)"
	  (or (lookup :phase authorities)
	      "all")))
      (html-item "td"			;level
        (html-line "~(~A~)"
	  (or (lookup :level authorities)
	      "inf"))))
    (html-item "tr align=center"
      (html-item "th" "Execute")
      (html-item "td" "all")		;phase
      (html-item "td" "no"))))		;level


(defun write-action-level-table ()
  ;; See report-level-assignments in tf-compiler/levels.lsp.
  (let* ((table (mapping action-name level))
	 (names (hash-table-keys table))
	 (levels (group-by-numeric-key names (hash-table->function table))))
    ;; Remove the tasks
    (pop levels)
    ;; Write a table starting at level 1.
    (html-aiai-table ()
      (html-item "tr"
	(html-item "th align=center" "Level")
	(html-item "th align=center" "Actions"))
      (loop for level-members in levels
	    for level from 1
	    when level-members do
	(html-item "tr"
	  (html-item "td align=center"
	    (html-line "~A" level))
	  (html-item "td"
	    (html-line "~{~A~^, ~}" 
		       (canonical-description-order level-members))))))))

(defun write-unpacked-action-level-table ()
  ;; See report-level-assignments in tf-compiler/levels.lsp.
  (let* ((table (mapping action-name level))
	 (names (hash-table-keys table))
	 (levels (group-by-numeric-key names (hash-table->function table))))
    ;; Remove the tasks
    (pop levels)
    ;; Write a table starting at level 1.
    (html-aiai-table ()
      (html-item "tr"
	(html-item "th align=center" "Level")
	(html-item "th align=center"  "Actions"))
      (loop for level-members in levels
	    for level from 1
	    when level-members do
	(html-item "tr"
	  (html-item "td align=center"
	    (html-line "~A" level))
	  (html-item "td"
	    (html-block "table"
	      (loop for name in (canonical-description-order level-members) do
                (html-item "tr"
                  (html-item "td"
		    (html-line "{~A ...}" name)))))))))))


;;;; Information about the (TF) domain

(defun write-action-levels-page ()
  (html-standard-page (:title (title-for-user "Action Levels")
		       :centered-title-header t)
    (write-unpacked-action-level-table)))


;;;; Plan views

(defun write-coa-plan-view-page (&optional (coa (get-coa (query-arg :n))))
  (html-standard-page
      (:title (title-for-user-and-coa coa "Plan Views")
       :centered-title-header t)
    (html-line "<hr>")
    (html-print-oplan-greeting)
    (html-line "<hr>")
    (html-report-plan-statistics (coa-plan-statistics coa))
    (html-paragraph
      (html-line "Results:")
      (html-block "ul"
        (itemize-coa-tf-links coa)
        (itemize-plan-descriptions coa)))
    (html-anchor (web-demo-url "output-formats.html")
      "Explain outputs")
    (html-line "<br>")
    (web-mail-comment-link)))


(defun itemize-coa-tf-links (coa)
  (declare (ignore coa))
  ;; The generated task
  #+:undef
  (html-item "li"
    (html-anchor (coa-filename-url coa "task.tf")
		 "TF task definition"))
  ;; The TF file
  (html-item "li"
    (html-anchor (demo-tf-url *demo*)
		 "Support TF file")))

(defun itemize-plan-descriptions (coa)

  ;; A PostScript graph

  (when (coa-plan-description coa)
    (html-item "li"
      (html-psgraph-links coa)))

  ;; Plan levels

  (when (coa-evaluations coa)
    (html-item "li"
      (html-anchor (drill-down-url coa :plan-levels)
		   "Plan levels")))

  ;; Plan narrative

  (when (coa-plan-description coa)
    (html-item "li"
      (html-anchor (coa-path-action-url :narrative coa)
		   "Plan narrative")))

  ;; Timeline
  (when (coa-evaluations coa)
    (html-item "li" "Timeline: "
      (html-anchor (coa-path-action-url :timeline-h coa)
		   "horizontal")
      " or "
      (html-anchor (coa-path-action-url :timeline-v coa)
		   "vertical")))
  
  ;; World at end of node-2

  (when (coa-plan-world-2 coa)
    (html-item "li"
      (html-anchor (coa-path-action-url :world-2 coa)
		   "World state when the plan finishes"))))

;;;; No Plan

;;; If we tried to get a plan and failed, this page tries to say
;;; something about the failure and maybe even try to explain.

(defun write-coa-no-plan-page (&optional (coa (get-coa (query-arg :n))))
  (declare (ignore coa))
  (write-not-yet-page))


;;;; Plan views

;;; PostScript graph

;;; The graph is produced on demand and sent as the contents of a reply
;;; to the HTTP request.  In demos in which O-Plan does not keep running,
;;; it is instead written to a file right after planning.  By generating
;;; the graph only when (if) the user asks for it, we can respond sooner
;;; after planning, and we save disk space.  (The graphs tend to be the
;;; largest of the plan views when all the views are in files.)

;;; /\/: We could easily have different links to get graphs to different
;;; levels.  [done]

(defun html-psgraph-links (coa)
  (let ((deepest-level (psgraph-deepest-level coa)))
    (html-format "PostScript graph to level ")
    (loop for i from 1 to deepest-level do
      (when (> i 1) 
	(html-format ", "))
      (when (and (= i deepest-level) (> deepest-level 1))
	(html-format "or "))
      (html-psgraph-to-depth-link coa i))))

(defun psgraph-deepest-level (coa)
  ;; First try the :plan-levels evaluation.
  ;; /\/: This uses TF compiler levels.  May need expansion level instead.
  ;; /\/: Yes, we do -- and the levels eval excludes dummy nodes.
  (if nil ; (lookup :plan-levels (coa-raw-evaluations coa))
      (let* ((plan-levels (lookup :plan-levels (coa-raw-evaluations coa)))
	     (depth-list (getf plan-levels :depth-list))
	     (deepest-level
	      (loop for (tag pattern depth) in depth-list
		    maximize depth)))
	deepest-level)
    ;; We need a 2nd method, because we migth be called when plan-evaluations
    ;; are not available.
    (if (coa-plan-description coa)
	(oplan-plan-world-viewer::get-plan-depth
	 (coa-plan-description coa))
      ;; Maybe there's no plan description either.
      0)))

(defun html-psgraph-to-depth-link (coa depth)
  (let ((d (prin1-to-string depth)))
    (html-anchor
      (concat-string (coa-path-action-url :graph coa) "/" d)
      d)))

(defun write-coa-postscript-graph (&optional (coa (get-coa (query-arg :n))))
  (let ((depth (string->int (second *path-args*)))
	(nodes 
	 (oplan-plan-world-viewer::fix-up-nodes
	  (mapcar #'oplan-plan-world-viewer::copy-node-description
	   (coa-plan-description coa)))))
    (setq nodes (oplan-plan-world-viewer::extract-plan-to-level nodes depth))
    (send-http-response-headers *http-request* *http-io*
      :content-type "application/postscript")
    (let ((*standard-output* *http-io*))
      (oplan-plan-world-viewer::new-draw-plan-psgraph
        nodes
	t						;one page
	(format nil "Pacifica COA ~A to level ~A"	;title
		(coa-number coa)
		depth)))))

;;; Plan narrative

(defun write-plan-narrative-page (&optional (coa (get-coa (query-arg :n))))
  (assert *package*)
  (let ((nodes
	 (oplan-plan-world-viewer::fix-up-nodes
	  (mapcar #'oplan-plan-world-viewer::copy-node-description
	   (coa-plan-description coa)))))
    (html-standard-page (:title (title-for-user-and-coa coa "Plan Narrative")
			 :centered-title-header t)
      (html-block "pre"
        (oplan-plan-world-viewer::write-narrative nodes *html-out*)))))

;;; World at end_of node-2

(defun write-world-2-description-page (&optional (coa
						  (get-coa (query-arg :n))))
  (html-standard-page (:title (title-for-user-and-coa coa "World State")
		       :centered-title-header t)
    (html-tag-line "h2" "World state when the plan finishes")
    (html-pv-table (coa-plan-world-2 coa))))

(defun html-pv-table (pv-pairs)
  ; (html-block "pre" (pprint pv-pairs *html-out*))
  (html-aiai-table ()
    (html-item "tr align=left"
      (html-item "th" "Pattern")
      (html-item "th" "Value"))
    (loop for (p v) in pv-pairs do
      (html-item "tr align=left"
        (html-item "td"
          (html-tag-line "tt" "~A"
            (html-encode-pre-string (princ-to-string p))))
        (html-item "td"
          (html-tag-line "tt" "~A"
            (html-encode-pre-string (princ-to-string v))))))))


;;; Timeline

(setf (get :timeline-h :path-action) 'horizontal-timeline-path-action)
(setf (get :timeline-v :path-action) 'vertical-timeline-path-action)

;;; The original, horizontal timeline

(defun horizontal-timeline-path-action ()
  (which-coa)
  (html-standard-page (:title (title-for-user-and-coa *coa* "Timeline")
		       :centered-title-header t)
    (write-horizontal-timeline
      (lookup :timeline (coa-raw-evaluations *coa*)))))

(defun write-horizontal-timeline (stages)
  (setq stages (peephole-optimize-stages stages))
  (assert (notany #'null stages))
  (html-aiai-table ()
    (write-horizontal-timeline-headers stages)
    (write-horizontal-timeline-body stages)))

(defun write-horizontal-timeline-headers (stages)
  (let ((now -1))			;so time of 1st stage will be /=
    (html-item "tr"
      (dolist (s stages)
        (let ((stage-est (getf (first s) :est)))
	  (cond ((= stage-est now)
		 (html-empty-th))
		(t
		 (setq now stage-est)
		 (html-item "th align=left"
                   (html-output (seconds->minimal-time-string now))))))))))

(defun write-horizontal-timeline-body (stages)
  (html-item "tr"
    (dolist (s stages)
      (html-item "td valign=top"
        (write-timeline-time-cell-contents s)))))

(defun write-timeline-time-cell-contents (stage)
  (html-block "pre"
    (dolist (ne stage)
      (write-timeline-ne-description ne))))

(defun write-timeline-ne-description (end &optional verb)
  ;; Max verb len is that of "begin" = 5.
  (html-xp-format "~5@A ~:W~%"
		  (or verb (getf end :end))
		  (getf end :pattern)))

;;; Vertical timeline (easier to print)

(defun vertical-timeline-path-action ()
  (which-coa)
  (html-standard-page (:title (title-for-user-and-coa *coa* "Timeline")
		       :centered-title-header t)
    (write-vertical-timeline
      (lookup :timeline (coa-raw-evaluations *coa*)))))

(defun write-vertical-timeline (stages)
  (setq stages (peephole-optimize-stages stages))
  (assert (notany #'null stages))
  (let ((now -1))
    (html-aiai-table ()
      (dolist (s stages)
	(let ((stage-est (getf (first s) :est)))
	  (when (/= stage-est now)
	    (setq now stage-est)
	    (html-item "tr"
	      (html-item "th align=center valign=top"
	        (html-output (seconds->minimal-time-string now)))))
	  (html-item "tr"
	    (html-item "td valign=top"
              (html-block "pre"
		""			;symmetry /\/
                (dolist (ne s)
                  (write-timeline-ne-description ne))))))))))

;;; Timeline "peephole optimization"

;;; If the two ends of a node could happen one right after the other,
;;; with no est difference, show them as one entity rather than two.

(defun peephole-optimize-stages (stages)
  (assert (notany #'null stages))
  (let* ((this-stage (pop stages))
	 (next-stage (pop stages))
	 (revised (make-tconc)))
    (loop
      (when this-stage
	(tconc revised this-stage))
      (when (null next-stage)
	(return (tconc-contents revised)))
      (when (and this-stage
		 (= (getf (first this-stage) :est)
		    (getf (first next-stage) :est)))
        (dolist (ne this-stage)
	  (let ((other-end (find-other-end ne next-stage)))
	    (when other-end
	      (setf (getf ne :end) :do)
	      (setq next-stage (remove-1-eq other-end next-stage))))))
      (shiftf this-stage next-stage (pop stages)))))

(defun find-other-end (ne ne-list)
  (let ((node (getf ne :node)))
    (dolist (e ne-list)
      (when (and (not (eq e ne)) (eq (getf e :node) node))
	(return e)))))


;;;; The Planner's matrix page

;;; Go-plan is called when the user selects a :plan URL on the TA matrix page.
;;; It sets things up for the planner user and write-planning-page but doesn't
;;; produce any HTML.

(defun go-plan ()
  (let* ((coa (get-coa (query-arg :n))))
    (web-note "~&To plan for COA ~A (id ~S)~%~%" (coa-number coa) (coa-id coa))
    (give-coa coa *planner-user*)))


;;; Write-planning-page is the planner user's version of write-matrix-page.

;;; If a user messes around with the browser's back button or "Go" menu,
;;; it may be possible to get here (or elsewhere!) when the user's supposed
;;; to be answering a question.  Such a user who ends up here is given the
;;; appropriate question-answering page instead of the matrix.

;;; /\/: What if > 1 coa has an outstanding question?  Signal an error?

(defun write-planning-page ()
  (assert (eq (user-name *user*) :planner))
  (let ((question-coa
	 (find-if #'(lambda (coa) (eq (coa-plan-status coa) :question))
		  *coas*)))
    (when question-coa
      (web-note "~&But there's an outstanding question for COA ~A (id ~S)~%%"
		(coa-number question-coa) (coa-id question-coa))
      (setf (query-arg :n) (coa-id question-coa))
      (write-question-answering-page)
      (return-from write-planning-page)))
  (set-user-focus-page *user* "Matrix" (path-action-url :matrix))
  (html-standard-page (:title (title-for-user "COA Evaluation Matrix")
		       :centered-title-header t)
    (write-planning-page-buttons)
    (html-line "<p>")			;just for some whitespace /\/
    (html-block "center"
      (html-block "table cellspacing=0"
        (html-item "tr"
          (html-item "td valign=bottom"
            (write-planning-coa-matrix))
          (html-item "td valign=bottom"
            (html-box
              (html-anchor (path-action-url :return-plans) "Return plans")))))
      (when (replan-authority :auto-replan-p)
	(html-block "small" "Automatic replanning is enabled.")))
    (write-planning-coa-description-table)
    (write-planning-situation-description-table)))

(defun-for matrix-server-demo write-planning-page-buttons ()
  (html-matrix-button-bar
    `(  ("TF file"                 ,(demo-tf-url *demo*))
	("Server status"           ,(path-action-url :server-status))
        ("Select evaluations"      ,(path-action-url :eval-select))
        ("Logout"                  ,(path-action-url :exit))
      )))

(defun-for matrix-server-demo write-planning-coa-matrix ()
  (html-aiai-table ()
    (write-planning-coa-matrix-top-row)
    (write-planning-coa-matrix-planner-advice-row)
    (write-planning-coa-matrix-add-constraints-row)
    (write-planning-coa-matrix-authority-row)
    (write-planning-coa-matrix-plan-or-replan-row)
    (write-planning-coa-matrix-eval-rows)
    (write-planning-coa-matrix-issues-row)
    (write-planning-coa-matrix-view-row)
    (write-planning-coa-matrix-return-plan-row)
    ))

(defun write-planning-coa-matrix-top-row ()
  ;; We don't want a highlighted label for this row, because
  ;; the row doesn't represent an action (in the planning workflow)
  ;; that the user takes.  Here we leave the cell blank.
  ; (write-coa-matrix-top-row :title "Plan option:")
  (write-coa-matrix-top-row :title nil))

(defun write-planning-coa-matrix-planner-advice-row ()
  ;; Advice has not yet been implemented, but we still have to decide
  ;; whether to have a link or not (ie whether advice *could* be supplied).
  ;; Anyway, it's very like an add-constraints row.
  (html-item "tr align=center"
    (html-item "th align=right" "Advise planner:")
    (do-visible-coas (coa)
      (html-matrix-cell (coa)
	(if (coa-active-p coa)
	    (html-anchor
	      (coa-path-action-url :advice coa)
	      "Advice")
	  (html-line "."))))))

(defun write-planning-coa-matrix-add-constraints-row ()
  (html-item "tr align=center"
    (html-item "th align=right" "Add constraints:")
    (do-visible-coas (coa)
      (html-matrix-cell (coa)
	(if (coa-active-p coa)
	    (html-anchor
	      (path-action-url :not-yet)
	      "Add")
	  (html-line "."))))))

(defun write-planning-coa-matrix-authority-row ()
  (html-item "tr align=center"
    (html-item "th align=right" "Set authority:")
    (do-visible-coas (coa)
      (html-matrix-cell (coa)
	(if (coa-active-p coa)
	    (html-anchor
	      (path-action-url :plan-settings)
	      "Auth")
	  (html-line "."))))))

(defun write-planning-coa-matrix-plan-or-replan-row ()
  ;; If the usual rules say planning is ok, write a :plan link.
  ;; Otherwise, if there's already a plan, write a :replan link.
  (html-item "tr align=center"
    (html-item "th align=right" "Generate plan:")
    (do-visible-coas (coa)
      (html-matrix-cell (coa)
	(cond ((not (coa-active-p coa))
	       (html-line "."))
	      ((makes-sense-to-plan-for-coa-p coa)
	       (html-anchor
		 (coa-path-action-url :plan coa)
		 "Plan"))
	      ((have-plan-for-coa-p coa)
	       (html-anchor
		 (coa-path-action-url :replan coa)
		 "Replan"))
	      ((coa-added-actions coa)	;not clear needed in planning matrix
	       (html-line "Plan"))	;but not a link
	      (t
	       (html-line ".")))))))

(defun write-planning-coa-matrix-eval-rows ()
  (write-coa-matrix-eval-rows))

(defun write-planning-coa-matrix-view-row ()
  (write-coa-matrix-view-row))

(defun write-planning-coa-matrix-return-plan-row ()
  ;; Very like a "View" or "Issues" row.
  (html-item "tr align=center"
    (html-item "th align=right" "Select for return:") ; was "Return plan:"
    (do-visible-coas (coa)
      (html-matrix-cell (coa)
	(cond ((or (have-plan-for-coa-p coa) (plan-failure-for-coa-p coa))
	       (html-anchor
		 (path-action-url :return-select)
		 (if (coa-return-p coa)
		     "Yes"
		   "No")))
	      (t
	       (html-line ".")))))))

(defun write-planning-coa-matrix-issues-row ()
  (write-coa-matrix-issues-row))

;;; Coa description table 

(defun write-planning-coa-description-table ()
  (write-coa-description-table))

;;; Planning situation table

(defun write-planning-situation-description-table ()
  (write-situation-description-table :include-default nil))


;;;; Planner advice

(defun write-planner-advice-page ()
  (html-standard-page (:title (title-for-user-and-coa *coa* "Advice")
		       :centered-title-header t)
    (html-invisible-box (:table "width=80% align=center")
      (html-invisible-box ()
        (html-paragraph
          "<b>Advise Planner:</b> this capability is not yet available.  It"
          "is intended to allow for the incorporation of advice to guide the"
	  "planner.  It could be provided through the addition of notes"
	  "(as in the Jotter of Nonlin or the Notepad in the O-Plan"
	  "design) which the planner could consult.  These could outline"
	  "the approach being taken to select a plan to meet the"
	  "requirements, or could give guidance on how to select activity"
	  "schemas or objects to incorporate in the plan.")))))


;;;; Planner settings page

;;; N.B. All of the current authority settings are global rather than
;;; per-coa.  Moreover, this is used (but probably shouldn't be /\/)
;;; when distinguishing between a planner-settings page reached from
;;; the matrix and one reached from a question-answering page.

(defun write-planner-settings-page (&aux coa)
  (assert (eq (user-name *user*) :planner))
  ;; A COA is specified if this is done from a question-answering page.
  ;; We need to pass the COA to :set-planner.  /\/
  (cond (*path-args*
	 (which-coa)
	 (setq coa (get-coa (query-arg :n)))
	 (unless (eq (coa-plan-status coa) :question)
	   (bogus-matrix-request-error)))
	(t
	 (set-coa :no-coa)))		;magic value /\/
  (html-standard-page (:title (title-for-user "Planner Settings")
		       :centered-title-header t)
    (html-form "post" 
        (if coa (coa-path-action-url :set-planner coa)
	        (path-action-url :set-planner))

      (html-headed-box ("Decisions during planning")
	(html-center
	  (write-schema-and-object-choice-table coa)))

      (unless coa
	;; Only if not from a question-answering page.
	(html-headed-box ("Replanning")
	  (html-center
	    (write-replan-authority-table coa))
	  (html-small-center-explanation
	    "The items on gray backgrounds are not yet available.")))

      (html-center
        (if coa
	    (html-matrix-form-buttons "Set authority" 
	      :return-to :focus)
	  (html-matrix-form-buttons "Set authority"))))))

(defun write-schema-and-object-choice-table (coa)
  (declare (ignore coa))
  (html-block "table"
    (html-item "tr"
      (html-item "td" "Schema choice")
      (html-item "td"
	(html-select-from-list :name :schema-mode :size 1
	  :options
	    (mark-selected (mode->menu-string *schema-selection-mode*)
	      '("Automatic" "Ask user")))))
    (html-item "tr"
      (html-item "td" "Object choice")
      (html-item "td"
	(html-select-from-list :name :binding-mode :size 1
	  :options
	    (mark-selected (mode->menu-string *psv-binding-mode*)
	      '("Automatic" "Ask user")))))))

(defun mode->menu-string (mode)
  (ecase mode
    (:auto "Automatic")
    (:ask  "Ask user")))

(defun menu-string->mode (str)
  (cond ((string= str "Automatic")
	 :auto)
	((string= str "Ask user")
	 :ask)
	(t
	 (error "Unknown mode string: ~S" str))))

(defun write-replan-authority-table (coa)
  (html-block "table border=1 cellspacing=0"
    (html-item "tr"
      (html-item "td"
        "Grant authority to replan automatically")
      (html-item "td"
	(html-select-from-list
          :name :auto-replan-p
	  :size 1
	  :options
	    (mark-selected 
	      (if (replan-authority :auto-replan-p) "Yes" "No")
	      '("Yes" "No")))))
    (html-item "tr"
      (html-item "td"
	"Maximum number of replans")
      (html-item "td"
	(html-select-from-list
	  :name :max-auto-replans
	  :size 1
	  :options 
	    (mark-selected (replan-authority :max-auto-replans)
	      '(1 2 3 4 5)))))
    (html-item `("tr" (bgcolor ,*unavailable-grey*))
      (html-item "td"
        "Time limit (minutes)")
      (html-item "td"
        (html-select-from-list
	  :name :auto-replan-time-limit
	  :size 1
	  :options
	    (mark-selected 1
	      '(1 2 3 4 5)))))
    (html-item "tr"
      (html-item "td"
	"Return the first plan that satisfies"
	(html-select-from-list
	  :name :filter-combiner
	  :size 1
	  :options 
	    (mark-selected
	      (string-downcase
	        (replace-subseq " " "-"
		  (symbol-name (replan-authority :filter-combiner))))
	      '("any of" "all of"))))
      (html-item "td"
	(write-replan-authory-filter-table coa)))
    ))

(defun write-replan-authory-filter-table (coa)
  (declare (ignore coa))
  (html-block "table"
    (html-item `("tr" (bgcolor ,*unavailable-grey*))
      (html-item "td"
        "Minimum duration at most")
      (html-item "td"
	(html-select-from-list
	  :name :duration-filter
	  :size 1
	  :options
	    (mark-selected 
	      (format nil "~D hour~:P" (replan-authority :duration-filter))
	      (loop for i from 3 to 48 by 3
		    collect (format nil "~D hour~:P" i))))))
    (html-item "tr"
      (html-item "td"
        "Effectiveness at least")
      (html-item "td"
	(html-select-from-list
	  :name :effectiveness-filter
	  :size 1
	  :options
	    (mark-selected
	      (format nil "~D%" (replan-authority :effectiveness-filter))
              (loop for p from 0 to 90 by 10
		    collect (format nil "~D%" p))))))))


;;;; Setting the planner settings

;;; /\/: We ought to (extend and then) use the program interface to
;;; set the schema and binding modes rather than directly setting
;;; *schema-selection-mode* and *psv-binding-mode*.

(defparameter *planner-choice-parameters*
  '((:schema-mode             (:text)    "Schema choice mode")
    (:binding-mode            (:text)    "Object choice mode")))

(defparameter *planner-replan-parameters*
  '((:auto-replan-p           (:text)    "Allow automatic replans?")
    (:max-auto-replans        (:int 1 5) "Maximum automatic replans")
    (:auto-replan-time-limit  (:text)    "Time limit on automatic replans")
    (:filter-combiner         (:keyword) "Filter combiner")
    (:duration-filter         (:text)    "Duration filter")
    (:effectiveness-filter    (:text)    "Effectiveness filter")))

#+:undef
(defparameter *planner-settings-parameters*
  (append *planner-choice-parameters*
	  *planner-replan-parameters*))

(defun set-planner-settings ()
  (parse-query-args)

  ;; A COA is specified if this is done from a question-answering page.
  ;; We need to record the COA for matrix-or-question-page.  /\/
  (if *path-args*
      (which-coa)
    (set-coa :no-coa))			;magic value /\/

  ;; Now *coa* = :no-coa if we're doing global settings, not
  ;; question-answering-page parameters.

  (unless (implies (coa-p *coa*) (eq (coa-plan-status *coa*) :question))
    (bogus-matrix-request-error))

  (convert-query-args *planner-choice-parameters*)

  ;; Schema-selection mode
  (setq *schema-selection-mode* (menu-string->mode (query-arg :schema-mode)))

  ;; Binding mode
  (setq *psv-binding-mode* (menu-string->mode (query-arg :binding-mode)))

  ;; And some replan authority
  (when (eq *coa* :no-coa)
    (convert-query-args *planner-replan-parameters*)
    (grant-replan-authority
      :auto-replan-p (string-equal (query-arg :auto-replan-p) "Yes")
      :max-auto-replans (query-arg :max-auto-replans)
      :filter-combiner (query-arg :filter-combiner)
      :duration-filter 
        (hour-string->int (query-arg :duration-filter))
      :effectiveness-filter
        (percent-string->int (query-arg :effectiveness-filter)))))

(defun percent-string->int (s)
  ;; E.g. "45" or "45%" ==> 45.
  (multiple-value-bind (i p) (break-string-at-first #\% s)
    (if (string= p "")
	(string->int i)
      (error "Illegal percent string: ~S." s))))

(defun hour-string->int (s)
  (let* ((parts (break-string-at #\space s))
	 (i (first parts))
	 (h (second parts)))
    (if (and (= (length parts) 2) (string= h "hours"))
	(string->int i)
      (error "Illegal hour string: ~S." s))))


;;;; Question-answering

(defun write-question-answering-page ()
  (let ((coa (get-coa (query-arg :n))))
    (unless (eq (coa-plan-status coa) :question)
      (bogus-matrix-request-error))
    (ecase (coa-question-kwd coa)
      (:schema-order
       (write-schema-selection-page coa))
      (:psv-binding
       (write-object-selection-page coa)))))

(defun-for matrix-server-demo write-question-page-buttons (coa)
  (html-button-bar
    `(("TF file"                 ,(demo-tf-url *demo*))
      ("Server status"           ,(path-action-url :server-status))
      ("Change planner settings" ,(coa-path-action-url :plan-settings coa))
      )))

(defun write-question-page-auto-replan-information (coa)
  (let ((enabled-p (replan-authority :auto-replan-p))
	(count (coa-auto-replan-count coa)))
    (when enabled-p
      (html-block "p" "Automatic replanning is enabled.")
      (if (> count 0)
	  (html-output
	    "The planner is working on the " (:format "~:r" count)
	    " automatic replan."//)
	(html-output
	  "The planner is working on the initial plan for"
	  " your request." //)))))

(defun write-question-page-plan-views (coa)
  (html-tag-line "h2" "Plan views")
  (html-block "ul"
    (itemize-plan-descriptions coa)))


;;;; Schema selection

;;; Schema selection question

(defun write-schema-selection-page (coa)
  (set-user-focus-page *user* (title-for-coa coa "Schema Selection")
    (coa-path-action-url :question coa))
  (html-standard-page (:title (title-for-user-and-coa coa "Schema Selection")
		       :centered-title-header t)
    (write-question-page-buttons coa)
    (write-question-page-auto-replan-information coa)
    (write-schema-selection-mini-form coa)
    (write-question-page-plan-views coa)
    (write-schema-selection-table coa)))

#+:undef
(defun write-schema-selection-page (coa)
  (set-user-focus-page *user* (title-for-coa coa "Schema Selection")
    (coa-path-action-url :question coa))
  (html-standard-page (:title (title-for-user-and-coa coa "Schema Selection")
		       :centered-title-header t)
    (write-schema-selection-page-buttons coa)
    (write-schema-selection-auto-replan-information coa)
    (write-schema-selection-form coa)
    (write-schema-selection-plan-views coa)))

;;; Schema-selection form

(defun write-schema-selection-mini-form (coa)
  ;; No description of the schemas -- that's done elsewhere.
  ;; However, we have to supply a similar h2 title.
  (let ((why (coa-question-agenda coa)))
    (html-tag-line "h2" "Select a schema to ~A"
      (ecase (car why)
	(:expand
	 (html-encode-pre-string
	  (format nil "~{~A~^ ~}" why)))
	(:achieve
	 (assert (eq (car (cadr why)) 'achievable))
	 (destructuring-bind (p v node-end) (cdr (cadr why))
	   (html-encode-pre-string
	     (format nil "achieve ~A = ~A at ~A of ~A"
		     p v (etag-end node-end) (etag-node node-end)))))
	(:fix
	 (assert (eq :expand (second why)))
	 (destructuring-bind (type p v node-end) (cdr (third why))
	   (declare (ignore type))
	   (html-encode-pre-string
	     (format nil "reestablish ~A = ~A at ~A of ~A"
		     p v (etag-end node-end) (etag-node node-end))))))))
  (html-form "post" (path-action-url :select-schema)
    ;; Put in the coa id as a hidden :n parameter.
    (html-line "<input type=\"hidden\" name=\"n\" value=\"~D\">"
	       (coa-id coa))
    ;; And the user picks ...
    (write-schema-selection-menu coa)
    (html-line "<p>")
    (write-schema-selection-form-buttons coa)))

(defun-for matrix-server-demo write-schema-selection-form-buttons (coa)
  (declare (ignore coa))
  (html-matrix-form-buttons "Select schema" :return-button nil))

#+:undef
(defun write-schema-selection-form (coa)
  ;; The full form includes the table that describes the schemas.
  (html-form "post" (path-action-url :select-schema)
    ;; Put in the coa id as a hidden :n parameter.
    (html-line "<input type=\"hidden\" name=\"n\" value=\"~D\">"
	       (coa-id coa))
    ;; Describe the possibilities in the manner of ks-user.
    (write-schema-selection-table coa)
    ;; And the user picks ...
    (write-schema-selection-menu coa)
    (html-line "<p>")
    (html-matrix-form-buttons "Select schema" :return-button nil)))

(defun write-schema-selection-menu (coa)
  (let ((names (mapcar #'car (coa-question-data coa))))
    ;(assert (length>1 names))		;/\/ ks-fix
    (html-colored-area "white"		;/\/ netscape4 makes selects gray
      (html-output
        (if (length>1 names)
	    "First choice: "
	  "Only choice: "))		;e.g. for :fix.
      (html-select-from-list
	:name :choice
	:size 1
	:options `((:selected ,(first names)) ,@(rest names))))))

;;; Schema-selection description table

(defun write-schema-selection-table (coa)
  ;; Derived rather directly from ks-user code.
  ;; Used to use (format t "~&~%Possible schemas for ~S:~%" ag-body)
  (let ((*standard-output* *html-out*)
	(*print-case* :downcase)
	(*print-pretty* t)
	(schemas-and-bindings (coa-question-data coa))
	(ag-body (coa-question-agenda coa)))
    (html-tag-line "h2" "Possible schemas")
    #+:undef
    (html-tag-line "h2"
      (concat-string "Possible schemas for <tt>"
        (html-encode-pre-string (princ-to-string ag-body)) "</tt>"))
    (html-1-table (:table "width=100% border=1 cellpadding=5")
      (html-block "pre" ""
        (write-schema-selection-descriptions schemas-and-bindings)))
    (when (loop for s+b in schemas-and-bindings
		thereis (schema-nodes (find-schema-with-name (car s+b))))
      (html-small-right-explanation 
        "Click on a schema name to get a PostScript graph"))))

(defun write-schema-selection-descriptions (schemas-and-bindings)
  (dolist (s+b schemas-and-bindings)
    (let ((schema (find-schema-with-name (car s+b)))
	  (bindings (remove :undef (cdr s+b) :key #'cadr)))
      (xp-format t "~@<~4I~A ~:@_expands ~W~:>~%"
	 (html-schema-selection-name schema)
	 (schema-expands schema))
      (when bindings
	(xp-format t "~4@Twith ~<~I~:@{~W = ~W; ~:_~}~:>~%"
	   bindings)))))

(defun html-schema-selection-name (schema)
  (if (schema-nodes schema)
      (delete #\newline
        (html-text-of
	  (html-draw-schema-anchor schema)))
    (schema-name schema)))

;;; Alternate version of the schema-selection description table

#|
(defun write-schema-selection-table (coa)
  (let ((*print-case* :downcase)
	(*print-pretty* t)
	(schemas-and-bindings (coa-question-data coa))
	(ag-body (coa-question-agenda coa)))
    (html-tag-line "h2"
      (concat-string "Possible schemas for <tt>"
        (html-encode-pre-string (princ-to-string ag-body)) "</tt>"))
    (html-box
      (html-block "table cellpadding=5"
        (dolist (s+b schemas-and-bindings)
          (let ((schema (find-schema-with-name (car s+b)))
		(bindings (remove :undef (cdr s+b) :key #'cadr)))
	    (html-item "tr valign=top"
              (html-item "td"
	        (write-schema-selection-schema-description
	          schema bindings))
	      (html-item "td"
		(html-block "pre"
                  (html-draw-schema-anchor schema "PostScript graph"))))))))))

(defun write-schema-selection-schema-description (schema bindings)
  (html-block "pre"
    (html-xp-format "~@<~4I~A ~:@_expands ~W~:>~%"
      (schema-name schema)
      (schema-expands schema))
    (when bindings
      (html-xp-format "~4@Twith ~<~I~:@{~W = ~W; ~:_~}~:>~%"
	bindings))))
|#

;;; Schema psgraphs

;;; Handles URLs produced by html-draw-schema-anchor.

(defun write-schema-graph ()
  (parse-query-args)
  (convert-query-args '((:schema (:text) "schema name")))
  (let* ((name (intern (query-arg :schema)))
	 (schema (or (find-schema-with-name name)
		     (error "No schema named ~S" name))))
    (send-http-response-headers *http-request* *http-io*
      :content-type "application/postscript")
    (let ((*standard-output* *http-io*))
      (draw-schema-psgraph schema))))

(defun html-draw-schema-anchor (schema &optional description)
  (let ((name (symbol-name (schema-name schema))))
    (html-anchor
      (concat-string (path-action-url :draw-schema)
		     "?schema=" (encode-for-url name))
      (or description
	  (string-downcase name)))))

;;; Schema selection answer

;;; We arrive here from :select-schema URLs.

(defparameter *schema-selection-parameters*
  '((:n      (:int 0)   "COA number")
    (:choice (:symbol)  "schema name")))

(defun answer-select-schema-question ()
  (parse-query-args)
  (convert-query-args *schema-selection-parameters*)
  (let* ((coa (get-coa (query-arg :n)))
	 (choice (query-arg :choice))
	 (data (coa-question-data coa))
	 (selected-entry (assoc choice data)))
    (unless (eq (coa-plan-status coa) :question)
      (bogus-matrix-request-error))
    (assert selected-entry)
    (coa-history-event coa :answer
      (coa-question-kwd coa) (coa-question-agenda coa)
      choice)
    (answer-question-for-coa coa
      ;; Put the selected entry first
      (cons selected-entry (remove-1-eq selected-entry data)))))


;;;; Variable binding / object selection

(defun write-object-selection-page (coa)
  (set-user-focus-page *user* (title-for-coa coa "Object Selection")
    (coa-path-action-url :question coa))
  (html-standard-page (:title (title-for-user-and-coa coa "Object Selection")
		       :centered-title-header t)
    (write-question-page-buttons coa)
    (write-question-page-auto-replan-information coa)
    (write-question-page-plan-views coa)
    (write-object-selection-form coa)))

(defun write-object-selection-form (coa)
  (assert (list-beginning :bind (coa-question-agenda coa)))
  (let* ((initiating-psv (second (coa-question-agenda coa)))
	 (vars (coa-question-data coa))
	 (initiating-var (find-var-with-tag initiating-psv vars))
	 (other-vars (remove-1-eq initiating-var vars)))
    (html-form "post" (path-action-url :bind-vars)
      ;; Put in the coa id as a hidden :n parameter.
      (html-line "<input type=\"hidden\" name=\"n\" value=\"~D\">"
		 (coa-id coa))
      ;; Form buttons.  Before the header for the initiating PSV
      ;; to try to make it clearer that the button applies to all
      ;; vars, not just the first.
      (write-object-selection-top-form-bottons coa)
      ;; Heading, mentioning the initiating PSV
      (html-tag-line "h2" "Select preferred values for ~A" initiating-psv)
      ;; Selection table for initiating PSV
      (write-object-selection-table (list initiating-var))
      ;; Selection table for other PSVs
      (when other-vars
        (html-tag-line "h2" "Other variables")
	(write-object-selection-table other-vars))
      ;; The submit etc buttons appear twice, because the table
      ;; for "other-vars" is sometimes rather long.
      (write-object-selection-bottom-form-buttons coa))))

(defun-for matrix-server-demo write-object-selection-top-form-bottons (coa)
  (declare (ignore coa))
  (html-matrix-form-buttons "Select objects" :return-button nil))

(defun-for matrix-server-demo write-object-selection-bottom-form-buttons (coa)
  (declare (ignore coa))
  (html-matrix-form-buttons "Select objects" :return-button nil))

(defun write-object-selection-table (vars)
  (html-aiai-table ()
    (dolist (v vars)
      (html-item "tr"
        (html-item "td"
          (write-var-description-for-object-selection v))))))

(defun write-var-description-for-object-selection (v)
  (html-block "table cellspacing=0 width='100%'"
    ;; Var header
    (html-item "tr"
      (html-item "th colspan=2"
        (html-output (first (getf v :tags)))))
    ;; Value-preference table
    (html-item "tr"
      (html-tag "td" "Preferred values:")
      (html-item "td"
        (write-var-value-preference-table v)))
    ;; Type
    (html-item "tr"
      (html-tag "td" "Type:")
      (html-tag "td" "~A" (getf v :type)))
    ;; Tags / Same
    (let ((sames (getf v :tags)))
      (when (length>1 sames)		;ie, minus the header tag
        (html-item "tr"
          (html-tag "td" "Sames:")
            (html-tag "td" "~{~S~^, ~}" (cdr sames)))))
    ;; Not-sames
    (when (getf v :not-sames)
      (html-item "tr"
        (html-tag "td" "Not-sames:")
        (if (getf v :not-sames)		; /\/ test no longer needed /\/
	    (html-tag "td" "~{~S~^, ~}" (getf v :not-sames))
	  (html-empty-td))))
    ;; Sources
    (html-item "tr valign=top"
      (html-tag "td" "Sources:")
      (html-item "td"
        (write-var-sources-descriptions v)))))

(defun write-var-value-selection-menu (v)
  (let ((values (getf v :possibles-cache)))
    (assert (length>1 values))
    (html-colored-area "white"		;/\/ Netscape4 makes selects gray
      (html-select-from-list
        :name (gensym)			;/\/
        :size 1
        :options `((:selected ,(first values)) ,@(rest values))))))

(defun write-var-value-preference-table (v)
  (let ((values (getf v :possibles-cache)))
    (html-block "table cellspacing=0"
      (html-item "tr"
	(dotimes (col 5)				;columns
	  (when values
            (html-item "td"
	      (write-value-preference-item v (pop values)))))))))

(defun write-value-preference-item (v value)
  (html-output 
    (:include (html-inline-checkbox (var-value-arg v value)))
    "&nbsp;"
    value))

(defun var-value-arg (v value)
  (let ((p (position value (getf v :possibles-cache))))
    (assert (numberp p))
    (string->keyword 
      (concat-string (first (getf v :tags)) "-" (int->string p)))))

(defun html-inline-checkbox (name &optional checked-p)
  ;; /\/: html-format rather than html-checkbox's html-line.
  (html-format "<input type=\"checkbox\" name=\"~A\"~:[~; checked~]>"
	       name checked-p))

(defun write-var-sources-descriptions (v)
  (html-block "table cellspacing=0"
    (dolist (s (getf v :sources))
      (destructuring-bind (tf-var schema-name
			   node-tag node-type node-pattern
			   node-reason)
			  s
	(html-item "tr"
          (html-tag "td" "?~A" tf-var)
	  (html-tag "td" "from schema <i>~A</i>" schema-name))
	(html-item "tr"
          (html-empty-td)		;skip tf-var col
	  (html-tag "td" "expanding <i>~A ~A ~A</i>"
		         node-tag node-type node-pattern))
	(when (list-beginning :achieve node-reason)
	  (html-item "tr"
	    (html-tag "td" "introduced to~{ ~A~}" node-reason)))))))

;;; Object selection answer

;;; We arrive here from :bind-vars URLs

(defparameter *object-selection-parameters* '())

(defun answer-select-object-question ()
  (parse-query-args)
  (convert-query-args '((:n (:int 0) "COA number")))
  (let* ((coa (get-coa (query-arg :n)))
	 (vars (coa-question-data coa)))
    (unless (eq (coa-plan-status coa) :question)
      (bogus-matrix-request-error))
    (convert-query-args
      (var-value-parameters vars))
    (let ((answer (object-selection-answer vars)))
      (coa-history-event coa :answer
	(coa-question-kwd coa) (coa-question-agenda coa)
	answer)
      (answer-question-for-coa coa answer))))

(defun var-value-parameters (vars)
  (loop for v in vars nconc
    (loop for val in (getf v :possibles-cache) collect 
      `(,(var-value-arg v val) (:checkbox) "Preferred PSV value?"))))

(defun object-selection-answer (vars)
  (loop for v in vars nconc
    (let* ((possible (getf v :possibles-cache))
	   (preferred
	    (loop for p in possible
		  when (query-arg (var-value-arg v p))
		  collect p)))
      (if preferred
	  (list `(:tags (,(first (getf v :tags)))
		  :values ,preferred))
	nil))))

;;;; Returning plans to the TA

;;; /\/: Can we return a COA when there's no plan (status = :failure)?
;;; We have to at least be able to get rid of such columns, because
;;; the Planner user may not want them cluttering up the matrix, and
;;; because when there's not even one plan for a task (think e.g. of
;;; house-4's task_build_house_to_time_0) the TA would otherwise be
;;; left with a ghost column.  But should it be up to the Planner user
;;; to get rid of "no plan" columns, or should it be possible to send
;;; them back to the TA as a kind of indication of what happened?
;;; Right now, we allow them to be returned.  It might be nice to
;;; have "delete" as the default, but that would be tricky to arrange.
;;; (We'd have to change the coa-return-p slot to coa-return-action
;;; and the values from t or nil to :yes, :no, or :delete.)

;;; The return-selection-page

(defun write-return-selection-page ()
  (set-user-focus-page *user* "COA Selection" (path-action-url :return-select))
  (html-standard-page (:title (title-for-user "COA Selection")
		       :centered-title-header t)
    (write-return-selection-page-buttons)
    (html-line "<p>")			;just for some whitespace /\/
    (html-form "post" (path-action-url :set-returns)
      (html-block "center"
        ; (write-return-selection-table)
	(html-invisible-box ()
	  (write-return-selection-coa-matrix)
	  (html-small-right-explanation
	    "Yes - return; No - do not return"))
        (html-matrix-form-buttons "Submit selections")))
    (html-line "<hr>")
    (progn ; html-block "center"
      ; (html-tag-line "h2" "COA descriptions")
      ; (write-return-selection-coa-matrix)
      (write-planning-coa-description-table)
      (write-planning-situation-description-table))))

#+:undef ; want shorter items
(defparameter *return-selection-options*
  '("Yes - return"
    "No - do not return"
    "Delete now"))

(defparameter *return-selection-options*
  '("Yes"
    "No"
    "Delete"))

(defun return-coa-arg (coa)
  (string->keyword (format nil "COA-~D-P" (coa-number coa))))

#+:undef
(defun write-return-selection-table ()
  (html-block "table"
    (do-visible-coas (coa)
      (if (have-plan-for-coa-p coa)
	  (html-item "tr"
	    (html-item "td"
              (html-line "<b>COA-~A</b>" (coa-number coa)))
	    (html-item "td"
	      (html-select-from-list
		:name (return-coa-arg coa)
		:size 1
		:options (return-selection-options coa))))))))

(defun return-selection-options (coa)
  (mark-selected
    (if (coa-return-p coa)
	"Yes"				;was "Yes - return"
      "No")				;was "No - do not return"
    *return-selection-options*))

(defun-for matrix-server-demo write-return-selection-page-buttons ()
  (html-button-bar
    `(  ("TF file"                 ,(demo-tf-url *demo*))
        ("Server status"           ,(path-action-url :server-status))
      )))

(defun write-return-selection-coa-matrix ()
  (html-aiai-table ()
    (write-planning-coa-matrix-top-row)
    (write-planning-coa-matrix-eval-rows)
    (write-planning-coa-matrix-view-row)
    (write-planning-coa-matrix-return-selection-row)))

(defun write-planning-coa-matrix-return-selection-row ()
  (html-item "tr align=center"
    (html-item "th align=right" "Select for return")
    (do-visible-coas (coa)
      (html-matrix-cell (coa)
	(if (or (have-plan-for-coa-p coa) (plan-failure-for-coa-p coa))
	    (html-select-from-list
	      :name (return-coa-arg coa)
	      :size 1
	      :options (return-selection-options coa))
	  (html-line "."))))))


;;; Set-returns -- process args from the return-selection page.

(defun set-returns ()
  (parse-query-args)
  (convert-query-args
    (mapcar #'(lambda (coa)
		`(,(return-coa-arg coa) (:optional (:text)) "return-option"))
	    (visible-coas)))
  (do-visible-coas (coa)
    (let ((option (query-arg (return-coa-arg coa))))
      (cond ((null option))
	    ((string-equal option "Yes") 	;was "Yes - return"
	     (setf (coa-return-p coa) t))
	    ((string-equal option "No")		;was "No - do not return"
	     (setf (coa-return-p coa) nil))
	    ((string-equal option "Delete")	;was "Delete now"
	     (setf (coa-visibility coa) nil))
	    (t
	     (error "Don't recognize return option ~S for COA ~A"
		    option
		    (coa-number coa)))))))


;;; Return-plans -- return selected plans

(defun return-plans ()
  (do-visible-coas (coa)
    (cond ((and (or (have-plan-for-coa-p coa) (plan-failure-for-coa-p coa))
		(coa-return-p coa))
	   (web-note "~&Returning COA ~A (id ~S)"
		     (coa-number coa) (coa-id coa))
	   (setf (coa-change-p coa) t)
	   (give-coa coa *ta-user*))
	  #+:undef
	  (t
	   (web-note "~&Making COA ~A (id ~S) invisible"
		     (coa-number coa) (coa-id coa))
	   (setf (coa-visibility coa) nil))))
  (web-note "~%~%"))


;;;; For things that aren't ready yet

;;; Write-not-yet-page handles :not-yet URLs.  They're used in cases where
;;; we want to have a link but don't yet have anything that can do the work.

(defun write-not-yet-page ()
  (html-standard-page (:title "Not Yet Implemented"
		       :centered-title-header t)
    (html-block "p"
      "You have selected an operation that has not yet been implemented.")
    (html-block "p"
      "Please use your browser's \"back\" button to return to the"
      "previous page.")))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;

