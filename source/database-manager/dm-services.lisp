;;;; File: dm-services.lisp
;;; Contains: Contains the functions which provide the interfaces to the DM
;;; Author: Richard Kirby (rbk) and Jeff Dalton
;;; Created: Wed Oct 31 13:40:54 1990
;;; Updated: Wed Jun  9 03:11:38 1999 by Jeff Dalton
;;; Copyright: (c) 1992, 1993, 1994 AIAI, University of Edinburgh

(in-package :oplan)

;;;; Interrupt handler.

;;; Present a menu to the user for interrogating the Plan State and
;;; ADS, etc.

(defmessage (:DM :INTERRUPT) ()
  (loop
    (ecase (dm-developers-menu-choice)
      (:nodes (do-display-all-nodes *debug-io*)
	      (terpri *debug-io*))
      (:tome  (do-display-tome *debug-io*)
	      (terpri *debug-io*))
      (:gost  (do-display-gost *debug-io*)
	      (terpri *debug-io*))
      (:psvs  (psv-print-psv-table *debug-io*)
	      (terpri *debug-io*))
      (:break
       (break "Return to get DM Developer's menu."))
      (:quit
       (force-output *debug-io*)
       (return)))))

(defun dm-developers-menu-choice ()
  (menu-request
    `("-heading" "DM Developer's Menu"
      "Display nodes=:nodes"
      "Display TOME=:tome"
      "Display GOST=:gost"
      "Display PSVs=:psvs"
      "BREAK IN=:break"
      "-line"
      "QUIT=:quit")))

(defun do-display-all-nodes (stream)
  (walk-nodes #'(lambda (n) (do-display-a-node n stream))))

(defun do-display-a-node (n stream)
  (let ((start-tp (n-begin-tpoint n))
	(end-tp (n-end-tpoint n))
	(*print-level* nil)
	(*print-length* nil))
    (declare (ignore start-tp end-tp))
    (format stream "~S~10T : Begin PRENODES = ~S~%"
	    (n-tag n)
	    (mapcar #'ne-tag (ne-pre-ends (n-begin n))))
    (format stream "(~S)~10T : Begin POSTNODES = ~S~%"
	    (n-type n)
	    (mapcar #'ne-tag (ne-post-ends (n-begin n))))
    (format stream "~S~10T : End PRENODES = ~S~%"
	    (n-tag n)
	    (mapcar #'ne-tag (ne-pre-ends (n-end n))))
    (format stream "(~S)~10T : End POSTNODES = ~S~%"
	    (n-type n)
	    (mapcar #'ne-tag (ne-post-ends (n-end n))))
    (format stream "~10T : ~A~%"
	    (psv-replace-psvs-in-pattern (n-pattern n)))))

(defun do-display-tome (stream &optional (filter-out-undefs t))
  (let ((act::*bindings* nil))
    (let ((g (generator '(OPLAN-TGM::TOME ?? ??) '??))
	  (*print-level* nil)
	  (*print-length* nil))
      (format stream "~%The TOME:~%")
      (do ((item (try-next g) (try-next g)))
	  ((null item))
	(if (or (null filter-out-undefs)
		(null (eql (item-inst (value item)) :undef)))
	    (format stream "~A = ~A~%"
		    (item-inst item) (item-inst (value item))))))))

(defun do-display-gost (stream &optional (filter-out-undefs t))
  (let ((act::*bindings* nil))
    (let ((g (generator '(OPLAN-TGM::GOST ?? ?? ?? ??) '??))
	  (*print-level* nil)
	  (*print-length* nil))
      (format stream "~%The GOST:~%")
      (do ((item (try-next g) (try-next g)))
	  ((null item))
	(if (or (null filter-out-undefs)
		(null (eql (item-inst (value item)) :undef)))
	    (format stream "~A = ~A~%"
		    (item-inst item) (item-inst (value item))))))))


;;;; Schema processing

;; Takes a schema pattern, and a requirement for matching, and the possible
;; schema vars with their restrictions, and returns the required bindings of
;; variables to things in the object, if a match (taking the restrictions
;; into account), else nil.
(defun match-up (pattern object binding-list)
  (let ((act::*bindings* binding-list))
    (obmatch pattern object)))

;; Returns the variable name for a ACT:GIVEN actor.
(defun given-var (x)
  (second (third x)))

;; /\/: "db-" in db-instantiate-schema because kp_supportlib also has
;; an instantiate-schema (which does a db-request).

(defun db-instantiate-schema (schema bindings node-tag)
  ;; This function goes through some of the fields of the schema, instantiating
  ;; any where there are actors, to the specified bindings. Before that though,
  ;; if any schema vars are left unbound presently, then replace them with a
  ;; Plan State variable.
  (setq *psv-current-source-node-ref* (ref-to node-tag))
  (let ((act::*bindings* nil))
    (when (consp bindings)
      (mapc #'(lambda (x)
		(if (and (not (psv-p (var-name x)))
			 (eq (var-value x) :undef))
		    (setf (var-value x)
			  (psv-create-psv schema (var-name x) bindings))))
	    bindings)
      ;; The bindings may say something about old PSVs.
      (dolist (tuple bindings)
	(when (psv-p (var-name tuple))		;an old PSV
	  (let ((value (var-value tuple)))
	    (cond ((unified-p value)
		   (let* ((var (var-name tuple))
			  (other-var (unified-var value))
			  (other-tuple (get-tuple other-var bindings))
			  (other-value (var-value other-tuple))
			  (psv-value other-value)) ;the val we'll assign
		     ;; Note that other-value may be one of the new PSVs
		     ;; created above.
		     (assert (psv-p var))
		     (assert (not (null other-tuple)))
		     (assert (not (unified-p other-value)))
		     (when (psv-p other-var)
		       (setq psv-value other-var)) ;will bind PSV to PSV
		     (unless (psv-set-value var psv-value)
		       (return-from db-instantiate-schema
			 `(:poison :could-not-bind var psv-value)))))
		  ((eq value :undef))	;ignore
		  (t
		   ;; Bind the PSV to the specified value
		   (unless (psv-set-value (var-name tuple) value)
		     (return-from db-instantiate-schema
		       `(:poison :could-not-bind (var-name tuple) value))))))))
      ;; Now for each new psv, if the bindings tuple specifies some not sames,
      ;; add the not-sames as restrictions.
      (dolist (tuple bindings)
	(let ((tuple-value (var-value tuple)))
	  (when (psv-p tuple-value)		;a new PSV
	    (dolist (not-same (var-not-sames tuple))
	      (let ((bad-value (var-value (get-tuple not-same bindings))))
		;; bad-value is either an ordinary value or the name
		;; of a new PSV.
		(unless (eq bad-value :undef)
		  (unless (psv-add-restriction tuple-value :NOT bad-value)
		    (return-from db-instantiate-schema
		      `(:poison :could-not-restrict
				,tuple-value ,bad-value))))))))))
    (setq schema (copy-schema schema))
    (setf (schema-expands schema)
	  (instance (schema-expands schema) bindings))
    (setf (schema-only-use-for-effects schema)
	  (instance (schema-only-use-for-effects schema) bindings))
    (setf (schema-nodes schema)
	  (instance (schema-nodes schema) bindings))
    (setf (schema-conditions schema)
	  (instance (schema-conditions schema) bindings))
    (setf (schema-effects schema)
	  (instance (schema-effects schema) bindings))
    (setf (schema-resources schema)
	  (instance-if-bound (schema-resources schema) bindings))
    (setf (schema-time-windows schema)
	  (instance-if-bound (schema-time-windows schema) bindings))
    (setf (schema-constraints schema)
	  (instance (schema-constraints schema)))
    (cons schema bindings)))

(defun instance-if-bound (x bindings)
  (let ((inst-x (instance x bindings)))
    (when (contains-psvs-p inst-x)
      (cerror "continue anyway" "Put a PSV in ~S,~%giving ~S" x inst-x))
    inst-x))

(defun contains-psvs-p (x)
  (or (oplan-psv:psv-p x)
      (and (consp x)
	   (or (contains-psvs-p (car x))
	       (contains-psvs-p (cdr x))))))

(defun create-binding-tuples-from-schema (schema)
  (let ((result (mapcar
		 #'(lambda (x)
		     (if (actorp (cadr x))
			 (make-tuple (car x) :undef nil (cadr x))
			 (if (eql (cadr x) 'undef)
			     (make-tuple (car x) :undef nil actorsym)
			     (make-tuple (car x) (cadr x) nil actorsym))))
		 (schema-vars schema))))
    ;; Now for each entry in the vars-relations field, set the not sames field
    ;; if appropriate.
    (let ((act::*bindings* result))
      (dolist (not-sames (schema-relationships schema))
	(let ((var1 (given-var (car not-sames)))
	      (var2 (given-var (caddr not-sames))))
	  (add-to-not-sames var1 var2)))
      (setq result act::*bindings*))
    result))


;;;; node and link addition.

(defun add-a-link (from to)
  ;; RBK - 8/1/92 Just check that the to node-end is not already before the
  ;; from node-end, in which case return nil, so higher up can decide what
  ;; to do.
  ;; /\/: Changed to reject attempts to link a node-end after itself.
  ;; [JD June 1999]
  (cond ((equal from to)
	 (whats-going-on "Can't link ~A after itself" from)
	 nil)
	((oplan-gop:gop-precedes to from)
	 (whats-going-on "~A precedes ~A so cannot link the other way"
			 to from)
	 nil)
	(t
	 (link-etags from to))))

;; This is called from KS-ACHIEVE for adding a new node to the network to
;; satisfy a particular condition.
;; /\/: Changed to use add-a-link rather than link-etags, because
;; add-a-link makes more checks.  The need for this emerged when
;; achieve after points started being used.  [JD, June 1999]
;; Also changed add-a-node to notice that a link request had
;; failed.  Before, it just called link-etags w/o checking the result.
;;
(defun add-a-node (type pattern reason parent pre-list post-list)
  (let ((node (ads-add-node :type type :pattern pattern :reason reason
			    :base-name nil
			    :parent (and parent (get-node parent)))))
    (flet ((link (from to)
	     (unless (add-a-link from to)
	       (return-from add-a-node nil))))
      (dolist (post post-list)
        (link (list node :end) post))
      (dolist (pre pre-list)
	(link pre (list node :begin)))
      ;; Add a link from the end of NODE-1 (the start node) to this node, so
      ;; that all the initial effects are necessairly before.  But if a
      ;; global after-point is defined, use it instead of the end_of NODE-1.
      (link
         (or (global-after-point)
	     (list 'node-1 :end))
	 (list node :begin))
      node)))

(defun add-an-initial-node (schema-node)
  (ads-add-node
    :type (node-type schema-node)
    :pattern (node-pattern schema-node)
    :reason '(:expand)))

(defun add-time-window (from to min max)
  (let ((from-tp (or (and (eql from :ABST0)
			  *tpn-tpoint-at-zero*)
		     (ne-time-point (get-node-end from))))
	(to-tp (ne-time-point (get-node-end to))))
    (tpn-add-time-constraint from-tp to-tp min max)))


;;;; Node descriptions

;; address could be a filename, or a channel like :LEFTOUT to send the plan
;; fragment out on.
(defun print-plan-nodes (address)
  (if (stringp address)
      (progn
	(cerror "Does nothing" "Writing to a file not implemented.")
	nil)
      (ecase address
	(:DESCRIPTIONS
	 ;; For sending to the PW viewer.
	 (make-a-list-of-node-descriptions)))))

(defun make-a-list-of-node-descriptions ()
  (mapcar #'node->node-description (sort (list-nodes) #'node-lessp)))

(defun node->node-description (node)
  ;; N.B. The result must match the viewer's node-description structure.
  (let ((node-name  (n-tag node))
	(begin-pre  (mapcar #'ne-tag (ne-pre-ends (n-begin node))))
	(begin-post (mapcar #'ne-tag (ne-post-ends (n-begin node))))
	(end-pre    (mapcar #'ne-tag (ne-pre-ends (n-end node))))
	(end-post   (mapcar #'ne-tag (ne-post-ends (n-end node))))
	(node-type  (n-type node))
	(pattern    (oplan-psv:psv-actorise-pattern
		     (n-pattern node))))
    ;; Remove the links between the two ends of the node.
    (setq begin-post (remove (etag node-name :end) begin-post :test #'equal))
    (setq end-pre (remove (etag node-name :begin) end-pre :test #'equal))
    ;; Construct a description.
    (list node-name
	  begin-pre
	  begin-post
	  end-pre
	  end-post
	  (node-time-bounds node)
	  node-type
	  pattern)))

(defun node-time-bounds (node)
  (let ((begin-tp (n-begin-tpoint node))
	(end-tp (n-end-tpoint node)))
    (list
      (tpoint-min begin-tp)
      (tpoint-max begin-tp)
      (tpoint-min end-tp)
      (tpoint-max end-tp))))


;;;; Whats-going-on output.

;; Uses whats-going-on to stuff out the Plan State in the current context.
(defun print-database ()
  (unless *print-whats-going-on-p*
    (return-from print-database))
  (whats-going-on "Context Num = ~S" (context-number *context*))
  (whats-going-on "The Triggered Agenda Table:~%~{~S~%~}" 
		  (ctxt-symbol-value '*triggered-agenda*))
  (whats-going-on "The Untriggered Agenda Table:~%~{~S~%~}"
		  (ctxt-symbol-value '*untriggered-agenda*))
  ;; /\/: The alternatives have moved to the AM.
  ;; /\/: Send a request if we really want them.
  ;; /\/: But what if the request creates a recursion?
  ;; want them.  
  ; (whats-going-on "The Alternative Agenda Table:~%~{~S~%~}"
  ;                 *alternatives-table*)
  (whats-going-on "The TOME:~%~{~S~%~}" (tgm-make-a-list-of-the-tome t nil))
  (whats-going-on "The GOST:~%~{~S~%~}" (tgm-make-a-list-of-the-gost t nil))
  (whats-going-on "The PSVs:~%~{~A~%~}" (psv-make-a-list-of-the-psvs))
  (whats-going-on "Nodes:~%~{~S~%~}" (make-a-list-of-node-descriptions)))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
