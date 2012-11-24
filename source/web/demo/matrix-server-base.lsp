;;;; File: matrix-server-base.lsp
;;; Contains: Support code for COA matrix Web demos
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: June 1998
;;; Updated: Wed Feb 23 22:21:17 2000 by Jeff Dalton
;;; Copyright: (c) 1997, 1998, 1999, AIAI, University of Edinburgh

(in-package :oplan)

(use-package :oplan-graph-util)


;;; This file contains (only) code that is not domain-specific; there are
;;; separate per-domain files that contain the domain-specific stuff.


;;;; Some parameters

(proclaim '(special *task-parameters*
		    *coa-definition-parameters*
		    *coa-addition-parameters*))


;;;; Demo classes

(defclass server-demo (web-demo)
  ())

(defclass matrix-server-demo (server-demo)
  ())

(in-local-defun-class matrix-server-demo *demo*)


;;;; User-role classes

(defclass user-role ()
  ((name
    :accessor user-name)
   (url-name
    :accessor user-url-name)
   (visible-evaluations
    :accessor user-visible-evaluations)
   (env					;a-list of special var bindings
    :accessor user-env)
   (focus-page-name
    :accessor user-focus-page-name)
   (focus-page-url
    :accessor user-focus-page-url)
   ))

(defclass ta-user (user-role)
  ((name
    :initform :task-assigner)
   (url-name
    :initform "t")
   (visible-evaluations
    :initform
      (remove-if #'(lambda (e)
		     (member (plan-eval-name e)
			     '(:n-psv-object-types :n-psv-values)))
		 (symbol-value '*evaluations*)))
   (env
    :initform
      `((*html-table-highlight-color* . ,*aiai-metal-gray*)
	(*html-button-bar-background-color* . ,*aiai-metal-gray*)))
   ))

(defclass planner-user (user-role)
  ((name
    :initform :planner)
   (url-name
    :initform "p")
   (visible-evaluations
    :initform (symbol-value '*evaluations*))
   (env
    :initform
      `(;; Use a "web-safe" approximation to LightSkyBlue1
	(*html-table-highlight-color* . ,*aiai-light-blue*)
	(*html-button-bar-background-color* . ,*aiai-light-blue*)))
   ))


;;;; User-roles

(defvar *ta-user* 'not-a-ta-user)

(defvar *planner-user* 'not-a-planner-user)

(defvar *user* 'not-a-user)


;;;; Some basic user methods

(defmethod user-env-symbol-value ((u user-role) var)
  (let ((e (assoc var (user-env u))))
    (if e
	(alist-value e)
      (error "No var ~S in env for user ~S." var u))))

(defmethod (setf user-env-symbol-value) (val (u user-role) var)
  (let ((e (assoc var (user-env u))))
    (if e
	(setf (alist-value e) val)
      (error "No var ~S in env for user ~S." var u))))

(defmethod set-user-focus-page ((u user-role) name url)
  (web-note "~&Setting ~S focus: ~S, ~S~%~%" (user-name u) name url)
  (setf (user-focus-page-name u) name
	(user-focus-page-url u) url))


;;;; Procedures required by join-server in http-server.lsp

(defun check-user-role (role)
  (ecase role ((:task-assigner :planner))))

(defun role-for-url (role)
  (ecase role
    (:task-assigner "t")
    (:planner "p")))

(add-http-startup-action 'matrix-server-init)


;;; Define the demo's URLs.

;;; /\/: Should this be part of matrix-server-init?

(defun-local add-demo-http-request-handlers ()
  (add-http-uri-interpretation
    (concat-string "/" (demo-name *demo*) "/") 
    'http-demo-action))


;;; Matrix-server-init

;;; Here's where we write information to the session file, if there
;;; is one.  This is called before any requests are handled.

(defun matrix-server-init ()
  (let ((filename (get-parameter :session-filename)))
    (when filename
      (write-server-pid-and-port filename))))


;;;; HTTP server entry point

;;; We don't need a file lock here, because the server won't process a
;;; new request until we're finished with the current one.

(setq *http-password-p* t)		;require password

(defvar *single-user-mode-p* nil)

(defvar *session-id* nil)
(defvar *path-action* nil)
(defvar *path-args* nil)

; /\/: calltrace is now part of web-support system so we don't have to
; (load-most-recent "support/calltrace")
(observe-errors-during-http-request-handling)

(defun http-demo-action (path request reply-io)
  (declare (ignore request reply-io))
  (with-calltrace
    (http-set-path-info path)
    (get-session-id-and-action)
    (progv (mapcar #'car (user-env *user*))
	   (mapcar #'cdr (user-env *user*))
      (obey-path-action))
    #+:undef
    (save-session-data)))

;;; By the time get-session-id-and-action returns, everything has been
;;; set up in a standard way that works for both starting and continuing
;;; a session.

;;; The URLs we're handling look like this:
;;;
;;;   http://Server-Host:Port/Demo-Name/...
;;;
;;; The "/..." part is available via get-path-info.
;;;
;;; The first request we see will be from a URL that looks like one
;;; of the following:
;;;
;;;   http://Server-Host:Port/Demo-Name/
;;;   http://Server-Host:Port/Demo-Name/Role
;;;
;;; where Role is "t" for Task-Assigner or "p" for Planner.  When
;;; no role is specified in the initial request, one actual user is
;;; playing both roles.
;;;
;;; All later requests are from URLs of the form
;;;
;;;    http://Server-Host:Port/Demo-Name/Role/Command
;;; or http://Server-Host:Port/Demo-Name/Role/Command/Path-Args
;;;
;;; The Command is put in *path-action* as a keyword, and the Path-Args
;;; are put in *path-args* as a list of strings (strings that were text
;;; separated by slashes in the URL).
;;;
;;; Note that Command cannot be "t" or "p" (in the URL), because we
;;; need to be able to tell initial requests from later ones.
;;;
;;; We may get two initial requests, one for each role.

(defun get-session-id-and-action ()
  (let* ((path (get-path-info))
	 (role (role-from-path-info path)))

    ;; Adjust path based on the role and on whether it's an initial request.
    (if (eq role :both)
	;; :both [= no role specified] is allowed only in initial requests.
	(assert (null path))
      ;; For "t" or "p", remove role.
      (pop path))

    ;; At this point, a null path means we have an initial request.
    (cond ((null path)
	   ;; Begin a new session or join an existing one.
	   (handle-new-user role)
	   (setq *path-action* :new-matrix)
	   (assert (implies *session-id* (not (eq role :both))))
	   (unless *session-id*
	     ;; New session.
	     (setq *session-id* *web-demo-id*)
	     ;; Note if one actual user has both roles.
	     (when (eq role :both)
	       (web-note "~&Setting single-user mode.~%~%")
	       (setq *single-user-mode-p* t))
	     (install-matrix-server-advice)
	     (initialize-session-data)))
	  (t
	   ;; Continue a session.
	   ; (setq *session-id* (pop path))
	   (assert *session-id*)
	   (setq *path-action*
		 (string->keyword (pop path)))
	   (setq *path-args* path)
	   #+:undef
	   (restore-session-data)))

    ;; Everything's now set up.

    ;; Set *user* from role
    (setq *user*
	  (ecase role
	    (:task-assigner *ta-user*)
	    (:planner *planner-user*)
	    (:both *ta-user*)))))

(defun role-from-path-info (path)
  (cond ((null path) :both)
	((string= (car path) "t") :task-assigner)
	((string= (car path) "p") :planner)))

(defvar *role-address-alist* '())

(defvar *roles-seen* '())

(defun handle-new-user (role)
  (web-note "~&New user in role ~S~%~%" role)
  (cond ((member role *roles-seen*)
	 ;; Don't allow the same role twice
	 ;; /\/: May want to say something better than "not found".
	 (bogus-matrix-request-error))
	(t
	 ;; Remember that we've seen an initial message for this role.
	 (nconcf1 *roles-seen* role)
	 (nconcf1 *role-address-alist*
		  (cons role (http-request-client-address *http-request*)))
	 ;; If we've now seen both roles, we can delete the session file.
	 (when (= (length *roles-seen*) 2)
	   (let ((filename (get-parameter :session-filename)))
	     (web-note "~&Deleting session file ~S~%~%" filename)
	     (assert (stringp filename))
	     (delete-file filename))))))

;;; We don't send the headers until just before we start sending HTML
;;; output.

(defun install-matrix-server-advice ()

  (advice+ 'do-html-standard-page :matrix-server
    #'(lambda (previous)
        #'(lambda (&rest args)
	    (send-demo-response-headers)
	    (apply previous args))))

  (advice-replace 'html-standard-page-foot :matrix-server
    #'coa-matrix-demo-page-foot)

  )

(defun send-demo-response-headers ()
  (send-http-response-headers *http-request* *http-io*
    :content-type "text/html"
    :no-cache nil #+:undef t))

;;; Often, the response is a redirection to the matrix.

(defun redirect-to-matrix-page ()
  (send-http-redirection-response *http-request* *http-io*
    :status-code 302			;moved temporarily
    :to (path-action-url :matrix)))

;;; In some cases, we redirect to the focus page instead.

(defun redirect-to-focus-page ()
  (send-http-redirection-response *http-request* *http-io*
    :status-code 302			;moved temporarily
    :to (user-focus-page-url *user*)))

;;; Bogus requests may also occur.

(defun bogus-matrix-request-error ()
  ;; /\/: For now, just say "Not found."
  (web-note "~&BOGUS REQUEST for ~S~%~%" (http-request-uri *http-request*))
  (signal-http-request-uri-not-found *http-request*))


;;;; Path-action dispatcher

;;; There's a case statement, rather than distributed defintions, to
;;; make it easier to see the overall structure.

;;; A partial convention is :X-def to get a form or some other kind of
;;; definition page for the user, and :def-X or :set-X to install the
;;; information the definition page provides.

(defun set-user (u)
  (web-note "~&Setting user = ~S~%~%" (user-name u))
  (setq *user* u))

(defun obey-path-action ()

  (case *path-action*

    ;; Matrix page
    (:new-matrix     (redirect-to-matrix-page))
    (:matrix         ;; what redirect-to-matrix-page redirects to
                     (ecase (user-name *user*)
		       (:task-assigner (write-matrix-page))
		       (:planner (write-planning-page))))

    ;; From the button bar
    (:server-status  (write-server-status-page))
    (:coa-select     (write-coa-selection-page))
    (:set-coas       (set-coa-selection)
		     (redirect-to-matrix-page))
    (:eval-select    (write-eval-selection-page))
    (:set-evals      (set-evaluations)
		     (redirect-to-matrix-page))

    ;; Domain information
    (:action-levels  (write-action-levels-page))

    ;; Planner settings
    (:plan-settings  (write-planner-settings-page))
    (:set-planner    (set-planner-settings)
		     (matrix-or-question-page))

    ;; Preparing to plan
    (:situation-def  (write-situation-definition-page))
    (:def-situation  (set-situation)
		     (redirect-to-focus-page))
    (:new-coa        (new-coa)
		     (redirect-to-matrix-page))
    (:coa-def        (which-coa)
		     (write-coa-definition-page))
    (:def-coa        (define-coa)
		     (redirect-to-matrix-page))
    (:split          (which-coa)
		     (split-coa)
		     (redirect-to-matrix-page))
    (:add-def	     (which-coa)
		     (write-constraint-addition-page))
    (:def-add	     (add-constraints-to-coa)
		     (redirect-to-matrix-page))
    (:advice         (which-coa)
		     (write-planner-advice-page))
    (:authority      (which-coa)
		     (write-authority-settings-page))
    (:set-authority  (set-coa-authority)
		     (redirect-to-matrix-page))

    ;; Planning, replanning, and looking at results
    (:plan           (which-coa)
		     (ecase (user-name *user*)
		       (:planner
			(plan-for-coa)
			(matrix-or-question-page))
		       (:task-assigner 
			(when *single-user-mode-p*
			  (set-user *planner-user*))
			(go-plan)
			(redirect-to-matrix-page))))
    (:replan         (which-coa)
		     (ecase (user-name *user*)
		       (:planner
			(replan-for-coa) 	   ;changes (query-arg :n) /\/
			(matrix-or-question-page))))
    (:explain        (which-coa)
		     (write-eval-explanation-page))
    (:view-plan      (which-coa)
		     (write-coa-plan-view-page))
    (:no-plan        (which-coa)
		     (write-coa-no-plan-page))
    (:graph	     (which-coa)
		     (write-coa-postscript-graph))
    (:narrative      (which-coa)
		     (write-plan-narrative-page))
    (:world-2        (which-coa)
		     (write-world-2-description-page))
    (:issues         (which-coa)
		     (write-issue-description-page))
    (:issues-handled (which-coa)
		     (mark-handled-plan-issues)
		     (redirect-to-focus-page))

    ;; Other information-producing actions
    (:draw-schema    (write-schema-graph))

    ;; New replan-return method
    (:return-select  (write-return-selection-page))
    (:set-returns    (set-returns)
		     (redirect-to-matrix-page))
    (:return-plans   (return-plans)
		     (when *single-user-mode-p*
		       (set-user *ta-user*))
		     (redirect-to-matrix-page))

    ;; Answering questions
    (:question       (which-coa)	;here only from focus-page URLs
		     (write-question-answering-page))
    ;; Schema selection
    (:select-schema  (answer-select-schema-question)
		     (matrix-or-question-page))
    ;; Object selection
    (:bind-vars      (answer-select-object-question)
		     (matrix-or-question-page))

    ;; Hostname lookup
    (:hostname       (lookup-host-and-redirect-to-status-page))

    ;; Color editor
    (:color-editor   (write-color-editor-page))
    (:edit-colors    (edit-colors)
		     (write-color-editor-page))

    ;; Not yet implemented
    (:not-yet        (write-not-yet-page))

    ;; Finished
    (:exit           (write-farewell-page)
		     (when (or *single-user-mode-p*
			       (eq (user-name *user*) :task-assigner))
		       ;; /\/: Planner user "Logout" does this too.
		       (exit-web-demo)))
    (t
     (let ((action (get *path-action* :path-action)))
       (if action
	   ;; A separately-defined action
	   (funcall action)
	 ;; An action we don't know about.
	 (bogus-matrix-request-error))))))

;;; /\/: The :no-coa mess below is so a question-answering page can
;;; change planner-settings.  The planner-settings cases will have a
;;; COA specified in those cases, but not otherwise.  We ought to
;;; clean up the whole way "which coa" is determined.  Moreover,
;;; it might make sense if parse-query-args was always called
;;; in one "main loop" place, rather than in every fn that needs
;;; to have the args.

;;; /\/: We should probably redirect to the question-answering page
;;; rather than write it for whatever URL got us here.

(defun matrix-or-question-page ()
  ;; /\/: :no-coa is a magic value.  nil might appear by mistake, so
  ;; we need something more distinctive.
  (let* ((n (query-arg :n))
	 (coa (if (eq n :no-coa) nil (get-coa n))))
    (if (and coa (eq (coa-plan-status coa) :question))
	(write-question-answering-page)
      (redirect-to-matrix-page))))


;;;; Path-action URLs

(defun path-action-url (action &optional arg-string)
  (http-server-url *http-port*
    (apply #'concat-string
	   *web-demo-name*
	   ; "/" *session-id*
	   "/" (user-url-name *user*)
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

;;; /\/: There should be a *coa* that's set on every request either
;;; to the right coa struct or to nil, so we could eliminate which-coa
;;; and (get-coa (query-arg :n)) calls.

;;; Form submissions (e.g. from the COA definition form) pass "which coa"
;;; as query-arg :n, in the way form submissions normally pass query
;;; args, but as a hidden input.

;;; /\/: See also drill-down-url.

;;; /\/: We should eventually use *coa* everywhere and remove the
;;; the refs to (query-arg :n).

(defun coa-path-action-url (action coa)
  (path-action-url action (format nil "~D" (coa-id coa))))

(defun which-coa ()
  (declare (special *coa*))
  (assert *path-args*)
  (setf (query-arg :n) (string->int (first *path-args*)))
  (setq *coa* (get-coa (query-arg :n))))

(defun set-coa (coa)
  (declare (special *coa*))
  (if (eq coa :no-coa)
      (setf (query-arg :n) :no-coa
	    *coa* :no-coa)
    (setf (query-arg :n) (coa-id coa)
	  *coa* coa)))


;;; Issue-status colors

;;; A useful X command is showrgb.  See also /usr/X11R6/lib/X11/rgb.txt 

;;; Light sky blue = "#87cefa"
;;; Deep sky blue  = "#00bfff"
;;; Sky blue = "#87ceeb"
;;; LightSkyBlue1 = "#b0e2ff"
;;; A reasonable light blue that will supposedly not be dithered by
;;; Web browsers is "#99ccff".  See: http://www.lynda.com/hexh.html 

;;; Richard's red: "#ffa498", and a lighter version: "#ffbfb8".
;;; Richard's so-called orange: "#ffcc5e"
;;; Another s-c orange: "#fadc96"

#+:undef
(defparameter *status-colors*
  '((:green  . "#ceffce")
    (:orange . "#f7e3ab")		;"#facd96", "#ffcdad", "#ffc7ab"
    (:red    . "#ffbfb8")))		;"#ff4040", "#ff6060", "#ffafaf"

;;; The nice pastels in the defparameter above sometimes get turned
;;; into the wrong colors in the browser.  E.g. the orange becomes
;;; yellow.  So we're trying the darker colors from ACP3:

#+:undef
(defparameter *status-colors*
  '((:green  . "#99FF99")
    (:orange . "#FFCC66")
    (:red    . "#FF6699")))

;;; But the ACP3 "red" is too close to magenta, or at any rate has
;;; too much blue.  Plausible "safe" alternatives (see ref above)
;;; are: "#ff6666" and the more intense and a bit too blue "#ff0066".
;;; Also "#ff3366"

;;; Another, probably unsafe, red is "#f63348".  It's one of the
;;; reds in the xv logo picture.

;;; "#FF3366" has too much blue on Austin's Mac, and he finds "#FF6666"
;;; not red enough (= too pink).  He likes "#FF3333", a nice crayon
;;; red, though rather saturated, expecially compared to our other
;;; colors.  But "#FF3366" is ok on Gairsay and Spottisvax.  "#FF6666"
;;; looks ok on Spottisvax but can look odd on Gairsay.

(defparameter *status-colors*
  '((:green  . "#99FF99")
    (:orange . "#FFCC66")
    (:red    . "#FF3333")))

(defun status-color->value (name)
  (or (lookup name *status-colors*)
      (error "Unknown status color: ~S." name)))

(defun issue-status-color (issues)
  (let ((note-p nil))
    (dolist (i issues (if note-p :orange :green))
      (unless (plan-issue-done-p i)
        (if (plan-issue-note-p i)
	    (setq note-p t)
	  (return :red))))))

;;; While we're at it, we'll intensify the table highlight color too.
;;; The default is *aiai-light-orange*.

; (setq *html-table-highlight-color* *aiai-medium-orange*)

;;; No, that's too sickening!

; (setq *html-table-highlight-color* "#b0e2ff") ; LightSkyBlue1

;;; Austin suggested the following "metal gray":

(defparameter *aiai-metal-gray* "#cccccc")

(setq *html-table-highlight-color* *aiai-metal-gray*)

(setq *html-button-bar-background-color* *html-table-highlight-color*)

;;; We also need a color for things (e.g. authority settings) that
;;; are not yet implemented.

(defparameter *unavailable-grey* *aiai-metal-gray*)

;;; A "safe" light blue (see above)

(defparameter *aiai-light-blue* "#99ccff")


;;;; Session data

;;; Task-args  /\/: init-args?  Args from the initial invocation, in any case.

(defvar *task-query-args* nil)

(defun task-arg (name) (gethash name *task-query-args*))

;;; Initialization

;;; Note that this is not called until we receive the first HTTP request.
;;; That's to ensure we have a user to send any error message to.

(defun initialize-session-data ()
  (declare (special *coas*))
  ;; Get task query-args.
  ; (parse-query-args)
  ; (convert-query-args *task-parameters*)
  ; (setq *task-query-args* *query-arg-table*)
  ;; Create user roles.
  (assert (and (symbolp *ta-user*) (symbolp *planner-user*))) ;not defined yet
  (setq *ta-user* (make-instance 'ta-user))
  (setq *planner-user* (make-instance 'planner-user))
  (setq *user* *ta-user*)		;needed by new-coa
  ;; Initialize COA data
  (assert (null *coas*))
  (new-coa)
  ;; Demo-specific init
  (initialize-matrix-demo)
  )


;;;; HTML output

;;; Title strings

(defun title-for-user (title)
  (ecase (user-name *user*)
    (:task-assigner
     (concat-string "O-Plan Task Assigner - " title))
    (:planner
     (concat-string "O-Plan Planner - " title))))

(defun title-for-coa (coa title)
  (declare (notinline coa-number))	;conflict w/ cgi coa struct def /\/
  (assert (stringp (coa-number coa)))	;and we've had a bug            /\/
  (concat-string "COA-" (coa-number coa) " " title))

(defun title-for-user-and-coa (coa title)
  (title-for-user
    (title-for-coa coa title)))

(defun capitalize-for-title (string)
  ;; Capitalize words except for "in", "of", "the", etc.
  (let ((words (break-string-at #\space string)))
    (concat-strings-with-separator
      " "
      (mapcar #'(lambda (word)
		  (if (trivial-title-word-p word)
		      word
		    (string-capitalize word)))
	      words))))

(defun trivial-title-word-p (word)
  (member (string->keyword word)
	  '(:a :an :the :in :of :on :for :by :to))) ;/\/ etc

;;; Colors -- see also *status-colors*

(defparameter *light-gray* "#eeeeee")

(defmacro html-colored-area (color &rest body)
  `(html-block (format nil "table cellspacing=0 cellpadding=0 bgcolor=~S"
		       ,color)
     (html-block "tr"
       (html-block "td"
         ,@body))))

;;; Some standard form buttons

;;; :return-to can be :matrix or :focus
;;; :return-text defaults to "Back to matrix without change"
;;;   for :return-to = :matrix and to something similar based on
;;;   (user-focus-page-name *user*) for :return-to = :focus.

;;; :other-submit-buttons is a list of (name value) lists, where the
;;; name are value are strings.  If one of these buttons is pressed,
;;; the form data will be send with the name and value from the submit
;;; button included.  The standard submit button does not have a name
;;; attribute and so it adds nothing to the form data.

;;; :action-buttons is a list of (text url) lists.

(defun html-matrix-form-buttons
    (submit-value &key (other-submit-buttons '())
		       (action-buttons '())
		       (return-button t)
		       (return-text nil)
		       (return-to :matrix))
  (html-block "table cellspacing=0 cellpadding=2"
    (html-item "tr"
      (unless (eq submit-value :no-submit)
        (html-item "td"
	  (html-line "<input type=\"submit\" value=\"~A\">" submit-value)))
      (loop for (name value) in other-submit-buttons do
	(html-item "td"
	  (html-line "<input type=\"submit\" name=\"~A\" value=\"~A\">"
		     name value)))
      (html-item "td"
	(html-line "<input type=\"reset\" value=\"Undo changes to form\">"))
      (loop for (text url) in action-buttons do
	(html-item "td"
          (html-1-table (:table "border=2 cellspacing=0")
            (html-anchor url text))))
      (when return-button
        (html-item "td"
	  ;; inner 1-table just to get a border
	  (html-1-table (:table "border=2 cellspacing=0")
	    (ecase return-to
	      (:matrix
	       (html-anchor (path-action-url :matrix)
		 (or return-text "Back to matrix without change")))
	      (:focus
		(html-anchor (user-focus-page-url *user*)
		  (or return-text
		      (format nil "Back to ~A without change"
			      (user-focus-page-name *user*))))))))))))

;;; Misc

(defmacro html-headed-box ((header &key td) &rest body)
  ;; :td specifies attributes for the box contents.
  `(html-block "table border=1 width=100%"
     (html-item "tr"
       (html-item `("th" ("bgcolor" ,*html-table-highlight-color*))
         (html-line "~A" ,header)))
     (html-item "tr"
       (html-item (add-html-block-attributes "td" ,td)
         ,@body))))

(defmacro html-center (&rest body)
  ;; For contexts where <center>...</center> can't be used.
  `(html-block "table width=100% cellspacing=0 cellpadding=0"
     (html-item "tr"
       (html-item "td align=center"
         ,@body))))

;;; Explanations (to go right under various tables)

;;; Width=100% is what makes align=right actually do something, for
;;; otherwise the explanation table would just be as wide as the text,
;;; and alignment wouldn't matter.

;;; Note that the table above the explanation must also be width=100%,
;;; for otherwise the explanation might extend beyond the right of the
;;; table above it.  (Of course, putting the table and the explanation
;;; inside a larger table is the "right" way to do this sort of thing.)

;;; With the two width=100% tables, no <br> should be needed to
;;; spearate them.  Indeed, a <br> lets in too much white space
;;; (at least with Netscape 3.x and with the way we place newlines
;;; in emitted tables, with "</table>" on a line of its own, etc).

(defmacro html-small-left-explanation (&rest body)
 `(html-block "table cellspacing=0 cellpadding=0"
     (html-item "tr"
       (html-item "td align=left"
         (html-block "small"
           . ,body)))))

(defmacro html-small-right-explanation (&rest body)
 `(html-block "table cellspacing=0 cellpadding=0 width=100%"
     (html-item "tr"
       (html-item "td align=right"
         (html-block "small"
           . ,body)))))

(defmacro html-small-center-explanation (&rest body)
 `(html-block "table cellspacing=0 cellpadding=0 width=100%"
     (html-item "tr"
       (html-item "td align=center"
         (html-block "small"
           . ,body)))))


;;; Page footer

(defun coa-matrix-demo-page-foot ()
  (html-line "<p>")			;just for some white space /\/
  (html-line "<hr>")
  (html-anchor "http://www.aiai.ed.ac.uk/"
    (format nil
      "<img width=82 height=37 alt=\"AIAI\" align=right border=0 src=~S>"
      ;; was src=\"http://www.aiai.ed.ac.uk/img/logo/small.gif\"
      (web-demo-url "image/aiai-small.gif")))
  (html-block "Address"
    (html-anchor "http://www.aiai.ed.ac.uk/~oplan/"
		 "O-Plan")))


;;;; Assorted utilities

(defun string->name (string)		;from the TF scanner /\/
  (values (intern (string-upcase string))))


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;

