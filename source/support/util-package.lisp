;;;; File: util-package.lisp
;;; Contains: Definition of the UTIL package.
;;; Author: Jeff Dalton
;;; Created: February 1993
;;; Updated: Tue Jun 15 03:17:47 1999 by Jeff Dalton
;;; Copyright: (c) 1993, 1994, 1995, 1996 AIAI, University of Edinburgh.

;;; This file defines the UTIL package and (most of) its exports.

;;; Definitions are added to the UTIL package by a number of different
;;; files.  Anything that is not specific to planning and that does not
;;; have a natural home anywhere else tends to end up in UTIL.

;;; Most things in UTIL do not depend on anything from other packages,
;;; but that's not always so.  In some cases there are dependencies in
;;; both directions.  For instance, pop-gensym in is UTIL and uses
;;; context-layering; context-layering has its own package and uses
;;; some things from UTIL.  [/\/: But pop-gensym has been deleted.]

(in-package :oplan-util)

(shadowing-import '(simple-defsystem:*source-type*
		    simple-defsystem:*object-type*))

;; Macros and other compile-time defs
(export '(defun-inline defun-export defmacro-export))
(export '(label while until))
(export '(implies))
(export '(with-working-directory))
(export '(with-unix-process-io))
(export '(deletef removef appendf appendf1 nconcf nconcf1 nconcf-new ensuref))
(export '(letf* letf))
(export '(definit initialize-variable-group))
(export '(define-initializer call-initializers))
(export '(define-struct-extension))
(export '(defstruct-export import-struct import-slot-names))
(export '(output *output* define-output-expander))

;; Parameter DB
(export '(get-parameter set-parameter parameter-set-p parameter-alist))

;; Environment variable enquiries
(export '(getenv-else getenv-else-error))

;; References
(export '(ref-to ref-value reference-p))

;; Modes
(export '(flip-mode opposite-mode))

;; Strings
(export '(concat-name concat-string big-string-concat))
(export '(string->keyword string->int int->string))
(export '(break-args))

;; Lists / sequences
(export '(car-if-consp cdr-if-consp length=1 length>1))
(export '(list-beginning list-proper-prefix-p))
(export '(last-element last-elt))
(export '(replace-sublist))
(export '(take drop take-while drop-while take-until drop-until))
(export '(set-eql disjoint-sets-p))
(export '(stable-set-difference))
(export '(standard-union stable-union more-stable-union))
(export '(standard-intersection stable-intersection))
(export '(interleave transpose-list flatten-one-level))
(export '(for-adjacent-elements))
(export '(walk-tree remap-tree))
(export '(fix-member fix-delete))
(export '(remove-1-eq delete-1-eq))
(export '(make-tconc tconc lconc tconc-contents))
(export '(recons remapcar))
(export '(list-lessp))
(export '(equivalence-classes group-by-numeric-key))
(export '(rotate-list chunk-list))
(export '(find-best max-value))
(export '(equal-with-1-1-renames))
(export '(canonical-description-order recursive-alphabetic-lessp))

;; Graphs
(export '(tsort find-longest-path-lengths))

;; Functions
(export '(partial1 compose2 wrap-1-arg-memoizer))

;; Hash-tables
(export '(alist->hash-table))
(export '(hash-table-alist filtered-hash-table-alist))
(export '(hash-table-keys hash-table-values))
(export '(hash-table->function))

;; A-lists
(export '(alist-key alist-value))
(export '(alist->function lookup))

;; P-lists
(export '(walk-plist))

;; Files and I/O
(export '(with-working-directory))	;a macro
(export '(find-all-files))
(export '(load-init-file))
(export '(load-most-recent find-most-recent))
(export '(print-readably read-safely))
(export '(file->list stream->list stream->lines))
(export '(string->list))
(export '(make-null-input-stream make-null-output-stream))
(export '(make-null-io-stream))
(export '(temp-filename generate-unique-filename))
(export '(ask-for-line ask-if))
(export '(hit-return-to-continue))
(export '(big-menu-request))


;;; Implementation-specific exports

(export '(argc argv exit-lisp getenv))
(export '(working-directory-pathname change-working-directory))
(export '(system))
(export '(save-image))
(export '(xp-format set-pretty-printer))
(export '(menu-request))
(export '(unix-process-io unix-process-finish))
(export '(run-xterm-for-io terminate-xterms-if-necessary))
(export '(still-running-p))
(export '(structurep map-structure))

;;; End
