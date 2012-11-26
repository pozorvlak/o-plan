;;;; File: tests-to-html.lsp
;;; Contains: Procedures to write HTML from test sequences
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: October 1994
;;; Updated: Tue Jul  9 17:09:33 1996 by Jeff Dalton
;;; Copyright: (c) 1994, AIAI, University of Edinburgh

(in-package :oplan)

;; /\/: Move this to defsys.
(defun require-system (name &rest load-system-args)
  (let ((sys (find-system name :if-not-found :error)))
    (unless (system-load-date sys)
      (apply #'load-system name load-system-args))))

(eval-when (eval compile load)
  (require-system :auto-tester))

(import '(atest::plan-test-domain
	  atest::plan-test-task))

(defun tests-to-html (tests &key output-file title)
  (with-open-file (*html-out* output-file :direction :output)
    (test-html-opening title)
    (test-html-test-list tests)))

(defun standard-tests-to-html ()
  (tests-to-html
     atest::*standard-test-sequence*
     :output-file "web/demo/standard-examples.html"
     :title "Standard TF demonstrations"))

(defun full-tests-to-html ()
  (tests-to-html
     atest::*full-test-sequence*
     ;; For now at least, we'll use the same files as for the standard seq.
     :output-file "web/demo/standard-examples.html"
     :title "Standard TF demonstrations"))

(defun resource-tests-to-html ()
  (load "auto-tester/resource-demo-tests")
  (tests-to-html
     atest::*resource-demo-test-sequence*
     :output-file "web/demo/resource-examples.html"
     :title "Resource demonstrations"))

(defun test-html-opening (title)
  (html-tag-line "TITLE" title)
  (html-tag-line "H1" title)
  (html-line "")
  (html-paragraph
    (html-line "Select a domain to see the task and schema definitions")
    (html-line "for that domain."))
  (html-paragraph
    (html-line "Select a task to ask O-Plan to produce a plan")
    (html-line "for that task."))
  (html-paragraph
    (html-line "Domains and tasks:"))
  (html-line ""))

(defun test-html-test-list (tests)
  (html-block "UL"
    (loop for (dom . tests) in (group-tests-by-domain tests)
	  do (html-item "LI"
	       (html-anchor
		 (concat-string "show-tf/" dom ".tf")
		 (string-capitalize dom))
	       (html-block "UL"
		 (test-html-domain-tasks dom tests))))))

(defun test-html-domain-tasks (dom tests)
  (dolist (test tests)
    (let ((task (plan-test-task test)))
      (html-item "LI"
        (html-anchor
          (concat-string "plan-for-demo-tf/" dom "/" task)
	  task)))))

(defun group-tests-by-domain (tests) ; a-list
  (equivalence-classes
    tests
    :key #'plan-test-domain
    :test #'equal))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
