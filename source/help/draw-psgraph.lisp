;;; Lisp code for the oplan-psgraph command

(in-package :oplan)

(defun draw-psgraph-for-plan ()
  (let ((domain (get-parameter :domain))
	(task (get-parameter :task)))
    (go-faster)
    (set-component-debug-level :all :warning)
    (if (plan-for domain task)
	(request-psgraph :viewer-one-page)
      (format t "No plan.~%"))))

