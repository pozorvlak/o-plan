;;;; File: system-definitions.lsp
;;; Contains: O-Plan system definitions
;;; Author: Jeff Dalton
;;; Created: February 1993
;;; Updated: Wed Feb 23 20:07:00 2000 by Jeff Dalton
;;; Copyright: (c) 1993 -- 1999 AIAI, University of Edinburgh

;;; O-Plan system definitions

;;; The definitions are in roughly top-down order.

;;; The usual procedure is to (compile-system 'everthing) and then
;;; build an image by starting a fresh Lisp, doing (load-system 'oplan),
;;; and saving a Lisp image.  The "Makefile" in the main source
;;; directory provides a convenient way to do this and other things.

;;; Note (if you're doing things by hand) that the first step is to
;;; load the "oplan.lsp".  It will load this file, among other things.

;;; To print a description of the system structure, evaluate
;;;   (print-system-tree 'everything)

;;; For a description of the defsystem used here, see support/defsys.lsp.

;;; /\/: We need a better, cleaner division into systems.  Even though
;;; they're all called "systems" most of the systems defined in this file
;;; are modules of other sorts; and some of the larger ones should be
;;; broken up along functional lines.  Package use also needs to be cleaned
;;; up.  At present, we have a lot of :do-after-load calls to use-package
;;; that make things visible in the OPLAN package; and some of the code
;;; that's in the OPLAN package should be given a package of its own.
;;; (We're in this mess for historical reasons.  In version 1.2, only
;;; the TF compiler was written as a system; everything else was compiled
;;; and loaded in an ad hoc way, and only the DM was actually compiled.)

;;; /\/: We may want to use a different defsystem, to explicitly
;;; distinguish between systems and modules, to put each system
;;; definition in its own file, etc.

;;; /\/: AKCL complains if packages that were used at compile-time
;;; aren't also used when the .o file is loaded even if no symbols
;;; from those packages were used in the .lsp file.


(in-package :oplan)


;;;; "Everything"

;;; The everything system requires all the others so that they can all
;;; be compiled with one command.

(defsystem (everything
	    :required-systems
	      (oplan
	       known-patches
	       help-thats-compiled
	       auto-tester
	       test-support
	       web-demos
         #+kcl http-server-interfaces
	       worlds
	       simple-defsys))
  ;; Compile this system to compile everything.
  ;; Normally, only the oplan system is loaded.
  )

(defsystem (simple-defsys :directory "support")
  ;; Here just so it will be compiled when the everything system is.
  (defsys))

(defsystem (known-patches :directory "patch"
			  :required-systems (oplan))
  ;; A nil :loader steps the files from being loaded when we're
  ;; compiling the system.  It also stops them from being loaded when
  ;; we load the system, but that's ok because they're loaded automatically
  ;; at run-time.
  )

(defsystem (help-thats-compiled :directory "help"
				:optional t
				:required-systems (oplan))
  (random-choices
    :loader nil)
  (apply-server
    :loader nil)
  )


;;;; The auto-tester

(defsystem (auto-tester :directory "auto-tester"
			:required-systems (support
					   task-assigner)
			:do-after-load
			   (use-package :atest :oplan)) ;for the user
  ("../support/fsa")
  (auto-tester
    :requires ("../support/fsa"))
  (standard-tests
    :requires (auto-tester)
    :compiler nil)
  (extra-tests
    :requires (auto-tester
	       standard-tests)
    :compiler nil)
  (release-tests
    :requires (auto-tester
	       standard-tests)		;/\/: for default-replans
    :compiler nil)
  )

(defsystem (test-support :directory "support"
			 :required-systems (support)
			 :do-after-load
			   (use-package :oplan-test-support :oplan))
  ;; The test systems that :require this are defined separately.
  (t-frame))


;;;; Web demos

(defsystem (web-demos :directory "web/demo"
		      :optional t
		      :required-systems
		        (basic-web-demos
			 cgi-matrix-demos
			 #+kcl
			 http-server-demos))
  )

(defsystem (basic-web-demos :directory "web/demo"
			    :optional t
			    :required-systems (top-level web-support))
  (island-rescue-support)
  (demo-tf-support)
  (simple-tpn-support)
  (london-underground-support)
  (get-url-support)
  (volume-groups-support))

(defsystem (cgi-matrix-demos :directory "web/demo"
			     :optional t
			     :required-systems (top-level web-support))
  (cgi-matrix-support)
  (pacifica-coas-support
    :requires (cgi-matrix-support))
  (gpdt-support
    :requires (cgi-matrix-support)))

#+kcl
(defsystem (http-server-demos
	     :directory "web/demo"
	     :optional t
	     :required-systems
	       (any-tf-matrix-demo
		any-tf-http-server-demo
		gpdt3-demo
		gpdt3-for-dera-demo
		mout1-demo
		suo-demo))
  ;; Each of these demos gets its own system rather than listing
  ;; the files here.
  )

#+kcl
(defsystem (any-tf-matrix-demo :directory "web/demo"
			       :optional t
			       :required-systems (matrix-server-support))
  (any-tf-matrix-support))

#+kcl
(defsystem (gpdt3-demo :directory "web/demo"
		       :optional t
		       :required-systems (matrix-server-support))
  (gpdt3-matrix-support))

#+kcl
(defsystem (gpdt3-for-dera-demo :directory "web/demo"
				:optional t
				:required-systems (gpdt3-demo))
  (gpdt3-for-dera-matrix-support))

#+kcl
(defsystem (mout1-demo :directory "web/demo"
		       :optional t
		       :required-systems (matrix-server-support
					  any-tf-matrix-demo))
  (mout1-matrix-support))

#+kcl
(defsystem (suo-demo :directory "web/demo"
		     :optional t
		     :required-systems (matrix-server-support))
  )


#+kcl
(defsystem (matrix-server-support
	     :directory "web/demo"
	     :required-systems (top-level web-support hidden-clos))
  ;; /\/: Why does it require top-level?
  (http-server)
  (matrix-server-base
    :requires (http-server))
  (coa-planning-support
    :requires (matrix-server-base))
  (matrix-server-support
    :requires (coa-planning-support
	       matrix-server-base
	       http-server))
  (matrix-server-exec-support
    :requires (matrix-server-support
	       matrix-server-base
	       coa-planning-support
	       http-server))
  (matrix-server-execlet-support
    :defines (:functions)
    :requires (matrix-server-exec-support))) ;and maybe others indirectly /\/


;;;; HTTP server - mode interface(s)

#+kcl
(defsystem (http-server-interfaces
	     :directory "web/demo"
	     :optional t
	     :required-systems
	       (any-tf-http-server-demo))
  ;; Each of these demos gets its own system rather than listing
  ;; the files here.
  )

#+kcl
(defsystem (any-tf-http-server-demo
	      :directory "web/demo"
	      :optional t
	      :required-systems (matrix-interface-support
				 any-tf-matrix-demo))
  (any-tf-http-server-support))


#+kcl
(defsystem (matrix-interface-support
	     :directory "web/demo"
	     :required-systems (matrix-server-support)) ; and what it requires
  ;; Provides an interface to O-Plan running in "http mode".
  (http-server-interface-support))


;;;; O-Plan itself

(defsystem (oplan
	    :directory "top"
	    :required-systems
	      (;; /\/ Initialization is (implicitly) required by all
	       ;; component systems.  Perhaps it should be in support.
	       initialization
	       ;; /\/ Put the DM first so that packages get set up
	       ;; in a way that works before other components are
	       ;; compiled.
	       database-manager
               task-assigner
	       interface-manager
	       controller
	       ;database-manager
	       pw-viewers
	       micro-exec
	       world
	       ks-platform
	       knowledge-sources
	       exec-knowledge-sources
	       top-level
	       program-interface
	       web-support
	       ))
  ;; The :required-systems include all the systems that must be
  ;; loaded.  Any files directly in the oplan system might not
  ;; really depend on many of them.  A separate top-level system
  ;; is therefore used.
  )

(defsystem (top-level
	    :directory "top"
	    :required-systems (support))
  (top-level
    :requires ("../support/run-lights"))
  ("../support/run-lights")
  (connect-mode)
  #+kcl
  (server-mode
    :defines (:functions))
  #+kcl
  (http-mode
    :defines (:functions)))

(defsystem (interface-definitions
	    :directory "top"
	    :required-systems (support))
  ;; /\/: In theory, this is all other systems should require,
  ;;      but for that it would have to include macro defs.
  (interface-package
    :defines (:package)
    :do-after-load (use-package :oplan-interface :oplan)))

(defsystem (program-interface
	    :directory "top"
	    :required-systems (support
			       interface-definitions))
  (subr-mode)
  (manual-interface
    :defines (:functions))
  (program-interface
    :defines (:functions :macros)))

(defsystem (web-support
	    :directory "web/support"
	    :optional t
	    :required-systems (support
			       interface-definitions))
  (cgi-env
    :requires (cgi-util
	       html-output))
  (html-output
    :defines (:functions :macros)
    :requires (cgi-util))
  (cgi-util
    :defines (:functions :macros))
  (load-web-config			;N.B. config loaded at build time
    :defines (:functions)		;not really /\/
    :requires (cgi-env html-output)
    :compiler nil)
  ("../../support/calltrace"))

(defsystem (initialization
	    :directory "top"
	    :required-systems (support parser-kit))
  ;; /\/: Maybe move to support?
  ;; /\/: The components ought to depend on this, but we rely on
  ;;      the oplan system loading initialization first.
  (initsystem
    :defines (:functions)		;/\/ mostly
    :do-after-load (use-package :oplan-initsystem :oplan)))


(defsystem (task-assigner :directory "task-assigner"
			  :required-systems (support
					     viewer-package))
  ;; /\/: Sneaks dates from defsys.
  (ta-toplevel)
  )

(defsystem (interface-manager :directory "interface-manager"
			      :required-systems (support
						 structure-defs))
  (im-toplevel)
  (control-panel)
  )

(defsystem (controller :directory "controller"
		       :required-systems (support
					  structure-defs))
  (am-toplevel)
  (am-services
    :requires (am-toplevel		;for defvars /\/
	       agendas))
  (agendas
    :requires (am-toplevel)
    :defines (:functions))		;and private defvars /\/
  (alternatives
    :requires (am-toplevel)
    :defines (:functions))		;and private defvars /\/
  (options
    :requires (alternatives)
    :defines (:functions))		;and private defvars /\/
  )

(defsystem (viewer-package :directory "pw-viewers"
			   :required-systems (support
					      tf-language))
  ;; Systems that contain calls to viewer code need to :require this system.
  (viewer-package
    :defines (:package)))

(defsystem (pw-viewers :directory "pw-viewers"
		       :defines (:functions)
		       :required-systems (viewer-package
					  support
					  initialization  ; shouldn't? /\/
					  task-assigner)) ; shouldn't /\/
  ;; A new viewer should :require viewer-services and will
  ;; indirectly depend on viewer-toplevel as well.
  (viewer-toplevel
    :defines (:variables :functions))
  (viewer-services
    :defines (:structures :functions)
    :requires (viewer-toplevel))
  (end-graph
    :defines (:structures :functions)
    :requires (viewer-services))
  (text-viewer
    :requires (viewer-services))
  (psgraph-viewer
    :requires (viewer-services
	       end-graph))
  (plan-narrative
    :requires (viewer-services
	       end-graph))
  (xml-viewer
    :requires (viewer-services))
  )


(defsystem (micro-exec :directory "exec"
                       :required-systems (support
                                          sim-clock))
  (micro-exec)
  )


(defsystem (world :directory "world"
                  :required-systems (support
                                     sim-clock
				     world-state-util))
  ;; Used only in the Exec
  (world-package)
  (world-parameters
    :requires (world-package))
  (world-toplevel
    :requires (world-package))
  (separate-world
    :requires (world-package))
  (world-services
    :requires (world-package
               world-toplevel           ;for variable definitions
               world-parameters))
  (world-loader
    :requires (world-package
               world-toplevel))
  (world-compiler
    :requires (world-package))
  )

(defsystem (worlds
             :directory "worlds"
             :required-systems
               (micro-exec-world))
  )

(defsystem (micro-exec-world
             :directory "worlds/micro-exec"
             :required-systems (world
				world-state-util))
  (micro-exec-world))

(defsystem (world-state-util
	     :directory "world"
	     :required-systems (support))
  (world-state-util-package)
  (world-state-util
    :requires (world-state-util-package)
    :defines (:functions)))


(defsystem (ks-platform :directory "ks-platform"
			:required-systems (support
					   structure-defs
					   shared-datastructures
					   viewer-package))
  ;; /\/: EXPAND/ACHIEVE requires the "tuple" struct from obactor-psvs
  ;; /\/: ACHIEVABLE comes from the tf-language package.
  (kp-package)
  (kp-toplevel
    :requires (kp-package))
  (kp-supportlib
    :requires (kp-package kp-toplevel))
  (ks-package
    :requires (kp-package))
)

;;; /\/: Perhaps each KS should be its own system, since different KSes
;;; may require different things?  E.g. only ks-get and ks-user need the
;;; viewer package.

(defsystem (knowledge-sources :directory "knowledge-sources"
			      :required-systems (ks-platform))
  (ks-achieve                :defines (:functions))
  (ks-add-to-task            :defines (:functions))
  (ks-bind                   :defines (:functions))
  (ks-check-plan             :defines (:functions))
  (ks-compute                :defines (:functions))
  (ks-condition              :defines (:functions))
  (ks-domain                 :defines (:functions))
  (ks-eval-plan	             :defines (:functions))
  (ks-expand                 :defines (:functions))
  (ks-expand-task            :defines (:functions))
; (ks-fix                    :defines (:functions))
  (ks-get                    :defines (:functions))
  (ks-init                   :defines (:functions))
  (ks-kill                   :defines (:functions))
  (ks-no-more-alternatives   :defines (:functions))
  (ks-or                     :defines (:functions))
  (ks-planner-finished       :defines (:functions))
  (ks-poison-state           :defines (:functions))
  (ks-question               :defines (:functions))
  (ks-replan                 :defines (:functions))
  (ks-set-task               :defines (:functions))
; (ks-teststaging            :defines (:functions))
  (ks-user                   :defines (:functions))

  (condition-support         :defines (:functions))
  (expand-support            :defines (:functions))

  )

(defsystem (exec-knowledge-sources :directory "exec-ks"
				   :required-systems (ks-platform))
  (ks-continue-execution     :defines (:functions))
  (ks-execute                :defines (:functions))
  (ks-execution-failure      :defines (:functions))
  (ks-execution-success      :defines (:functions))
  (ks-extract                :defines (:functions))
  (ks-fix                    :defines (:functions))
  (ks-unexpected-world-event :defines (:functions))
  )


;;; Database manager

(defsystem (database-manager
	     :directory "database-manager"
	     :required-systems (database
				tf-compiler
				domain-services-interface
				constraint-managers
				viewer-package
				exec-support))
  ("dm-toplevel")
  ("dm-services")
  ("domain")
  (sanity-checker)
  (plan-evaluator)
  )

(defsystem (exec-support
	     :directory "database-manager"
	     :required-systems
	       (database
		tgm
		world-state-util))
  (exec-support
    :defines (:functions)))

(defsystem (constraint-managers
	     :required-systems
	       (;; Here we list the constraint managers
		tgm
		compute-cm
		time-window-cm
		sc-rum
		use-cm
		cto-cm
		other-constraint-cm
		)))

(defsystem (tgm
	     :directory "constraint-managers/tgm"
	     :required-systems (database))
  ("always"
    :defines (:functions))
  (:qa
    :defines (:package :functions)
    :do-after-load (use-package :oplan-qa :oplan))
  (:qa-all
    :requires (:qa))
  (tgm					;also requires tgm-datastructures
    :requires (:qa))
  (world-state-cm
    :defines (:functions)
    :requires (:qa :qa-all :tgm))	;also requires tgm-datastructures
  (effect-cm
    :defines (:functions)
    :requires (:tgm))			;for some exports
					;also requires tgm-datastructures
  )

;; Simple CM systems -- one file w/ the same name as the system.
;; It's assumed that the systems don't define anything other than
;; functions, at least so far as systems that require them are concerned.
;;
(macrolet ((sys (name)
	     `(defsystem (,name :directory "constraint-managers"
				:required-systems (database))
		(,name
		  :defines (:functions)))))
  (sys compute-cm)
  (sys time-window-cm)
  (sys sc-rum)
  (sys use-cm)
  (sys cto-cm)
  (sys other-constraint-cm)
  )

(defsystem (database
	     :directory "database-manager"
	     :required-systems
	       (support
		obase
		tf-language		;/\/ must be loaded early
		structure-defs
		shared-datastructures))
  ;; /\/ Note the hidden dependency: tf-language has to be before
  ;; structure-defs and shared-datastructures, because they add
  ;; symbols to the OPLAN package that have the same names as TF
  ;; exports.
  
  ;; /\/: In a sense, psvs requires obactor-psvs, because it uses
  ;; the var struct; but it should be able to regard the defstruct
  ;; as defining only functions.

  ;; Main DM files.
  (gopher
    :defines (:functions)		;/\/ and a package
    :do-after-load (use-package :oplan-gopher :oplan))
  (psvs
    :do-after-load (use-package :oplan-psv :oplan))
  (obactor-psvs
    :requires (psvs))
  (or-tree-merger
    :defines (:functions)
    :requires (gopher psvs obactor-psvs))

  (cm-defs
    :do-after-load (use-package :oplan-cm-defs :oplan))
  (constraint-associator
    :defines (:functions)
    :requires (cm-defs
	       obactor-psvs))

  ;; /\/: Here we have some files in the OPLAN package that expect lots
  ;; of packages to be available in the use-list.  That's why lots of
  ;; systems and files have a :do-after-load that does a use-package.
  (atm)
  (triggers
    :defines (:functions)		;but *trim-agenda-or-trees* /\/
    :requires (psvs))
  (ta-questions
    :defines (:functions)
    :requires (atm))
  )

(defsystem (shared-datastructures
	     :directory "database-manager"
	     :required-systems
	       (support
		structure-defs		   ;nodes needs schema-defs /\/
		domain-services-interface) ;nodes needs action-level /\/
	     :do-after-load
	       ;; /\/: For code in the OPLAN package.
	       (use-package '(:oplan-nodes :oplan-tgm) :oplan))
  ;; Used by DM and KP.
  ("nodes")				;needs tf-langauge for 'dummy /\/
  (tpn					;shared? /\/
    :requires ("nodes"))
  ("../constraint-managers/tgm/tgm-datastructures")
  )

(defsystem (structure-defs :directory "common"
			   :required-systems (support
					      tf-language))
  ;; Definitions needed by more than one component.
  ;; /\/: But do they all go together?
  (schema-defs
    :do-after-load (use-package :oplan-schema-defs :oplan))
  (agendadef)
  (optdef)
  (or-trees				;does (in-package :QA) /\/
    :do-after-load (use-package :oplan-or-trees :oplan))
  (or-tree-functions
    ;; /\/: Not clear that this belongs here.
    ;; /\/: Also defines package exports.
    :defines (:functions)
    :requires (or-trees))
  )


;;; OBase

(defsystem (obase :directory "database-manager"
		  :required-systems
		    (support		;only for contexts? /\/
		     tf-language)	;for the VALUE symbol /\/
		  :do-after-load
		    ;; /\/ For code in the OPLAN package
		    (use-package :oplan-obase :oplan))
  (obase)
  )

;;; Domain services

(defsystem (domain-services-interface
	    :directory "common"
	    :do-after-load
	      (use-package :oplan-domain-services :oplan))
  (domain-services-package
    :defines (:package)))

;;; TF language

(defsystem (tf-language
	    :directory "tf-compiler"
	    :do-after-load
	      ;; So that we can read TF files in the OPLAN package
	      (use-package :oplan-tf :oplan)
	   )
  (tf-pack
   :defines (:package))
  )

;;; TF Compiler

(defsystem (tf-compiler
	    :directory "tf-compiler"
	    :required-systems (support
			       obase		;see com-pack /\/
			       structure-defs
			       domain-services-interface
			       tf-language
			       parser-kit)
	    :do-after-load
	      (use-package :oplan-tf-compiler :oplan))
  (com-pack				;required by all later files
    :defines (:package)
    :requires (graphs-pack))
  (scan
    :requires (com-pack))
  (com-defs
    :requires (com-pack))
  (o-face				;/\/: What does it really require?
    :defines (:functions)
    :requires (com-pack scan com-defs))
  (parser
    :defines (:functions)		;/\/: but *user-defaults*
    :requires (com-pack scan com-defs))
  (complr
    :defines (:functions)
    :requires (com-pack com-defs))
  (graphs-pack
    :defines (:package))
  (graphs
    :defines (:functions)
    :requires (com-pack graphs-pack))
  (levels
    :defines (:functions)
    :requires (com-pack com-defs
	       analysis))		;for table definitions /\/
  (analysis
    :defines (:functions)
    :requires (com-pack com-defs))
  (top-level
    :defines (:functions)
    :requires (com-pack com-defs))
  (embedded-tfc
    :defines (:functions)
    :requires (com-pack com-defs))
  )


;;; Parser Kit

(defsystem (parser-kit
	    :directory "support/parser-kit")
  (parser-kit-pack)
  (descent
    :requires (parser-kit-pack))
  )


;;; Hidden CLOS

(defsystem (hidden-clos
	    :directory "support"
	    :required-systems (support))
  (hidden-clos))


;;; Simulated time

(defsystem (sim-clock
	    :directory "support"
	    :required-systems (support))
  ;; Requires support for pseudo-processes, time-util, and defun-inline
  (sim-clock-package
    :defines (:package))
  (sim-clock
    :requires (sim-clock-package)))
	    

;;; Support

;;; /\/: May want to separate out the support needed only by -toplevel
;;; files and the like (ipc-support?), so that we don't e.g. recompile
;;; the whole DM every time we change something about p-processes.

;;; The support system contains definitions that can be used throughout
;;; O-Plan.  Higher-level systems typically have support as one of their
;;; :required-systems.  Modifications to support should be relatively
;;; rare, but when they happen the requiring systems will all have to be
;;; recompiled.  (If this becomes a problem, we could separate out the
;;; parts of support that affect compilation or else divide it into smaller
;;; systems; but it would require a fair amount of work.)

;;; The current subsystems of support are base-support, common-support,
;;; dependent-support, and external-support.

;;; Base-support contains portable code needed by both common-support
;;; and dependent-support.  This is the lowest level of "common code"
;;; in O-Plan, and everything outside external-support can assume it
;;; exists.

;;; Most of the support code is in common-support.  This code should work
;;; in at least to all the Common Lisps that will be used with O-Plan.
;;; The files in common-support may contain implementation-dependent code,
;;; but only internally.  (We allow this so that we can use self-contained
;;; packages that handle their own portability and that shouldn't be broken
;;; up just to follow the conventions we use in the body of O-Plan.)

;;; Some especially independent packages, typically code produced outside
;;; the O-Plan project, such as the XP pretty-printer, can be placed in
;;; external-support.  This code does not depend on base-support and is
;;; almost never recompiled.

;;; Most of O-Plan's own code that depends on particular Common Lisp
;;; implementations is confined to dependent-support.  There should be
;;; a version of dependent-support for each Common Lisp that O-Plan
;;; can use.  The conventions for adding new code of this sort are
;;; slightly complex, so we'll describe them here rather than force
;;; you to figure them out.

;;;  * Implementation-dependent code is placed in the UTIL package.
;;;    The package definition itself is portable.  That is, all
;;;    implementations define the same names with the same interface.

;;;  * The UTIL package is defined by the file "util-package" in the system
;;;    base-support (not in dependent-support).  All exports should be
;;;    in that file, but the actual, implementation-specific definitions
;;;    should be in files that are in dependent-support.  Those files
;;;    should all start with (in-package :oplan-util).

;;;  * Whenever possible, only implementation-dependent _functions_ should
;;;    be defined.  For instance, if a macro is needed, the implementation-
;;;    dependent aspects should be placed in functions called in the macro's
;;;    expansion, and definitions of those functions should be placed
;;;    in each version of dependent-support.

;;; This arrangement minimizes the amount of implementation-specific code
;;; and allows common-support to depend only on base-support.

(defsystem (support :directory "support"
		    :required-systems (base-support
				       dependent-support
				       external-support
				       common-support)
		    :do-after-load
		      ;; /\/: For code in the OPLAN package
		      (progn
			(use-package :oplan-developerlib :oplan)
			(use-package :oplan-ipc :oplan)
			(use-package :oplan-x :oplan)
			(use-package :oplan-ctxt :oplan)
			(use-package :oplan-time-util :oplan)
			(use-package :oplan-pseudo-process :oplan)
			(use-package :oplan-components :oplan)))
  ;; No files
  ;; /\/: Ok, one file.  It can't be in dependent-support because it
  ;; needs pseudo-process.lsp.
  #+kcl
  (kcl-select
    :defines (:functions)
    :compiler compile-with-C
    :loader load-with-C
    :do-after-load
      ;; /\/: This is just to avoid changing the source, because there
      ;; are problems recompiling such C code on the AIAI Suns right now.
      ;; [jd 28 may 99]
      (eval
       (read-from-string
	"(oplan-util:set-parameter :select-type :real)")))

  ;; /\/: We'll put the Allegro version here too
  #+:allegro
  (allegro-select
    :defines (:functions))

  ;; /\/: One more file, here because it needs the condition definitions
  ;; in dependent-support.  Maybe we need a higher-level-support system
  ;; for such things.
  (condition-util
    :defines (:functions :variables :exports :conditions))

  )


;;; KCL support

#+kcl
(defsystem (dependent-support :directory "support"
			      :required-systems
			        (base-support
				 external-support ;for XP
				 kcl-unix-support))
  (kcl-clos
    :compiler nil)
  (kcl-extensions
    :defines (:macros))
  (kcl-conditions
    :defines (:functions :macros :structures)
    :requires (kcl-extensions))
  (kcl-util
    :defines (:functions)
    :requires (#+:undef xfork))		;it's in kcl-unix-support /\/
  (kcl-xwindowio
    :defines (:functions)
    :requires (#+:undef xfork)		;it's in kcl-unix-support /\/
    :compiler compile-with-C
    :loader load-with-C)
  )

#+kcl
(defsystem (kcl-unix-support :directory "support")
  ;; /\/: The sockets file puts things in the oplan-util package,
  ;; and so depends in a sense on base-support, but it's so tricky
  ;; to compile this kind of files these days that it's best not 
  ;; to list any dependencies, since that way we can avoid recompilation
  ;; unless it's really necessary.
  (xfork
    :defines (:functions)		;and package, exports
    :compiler compile-with-C
    :loader load-with-C)
  #+(or (not sun) (and sun :gcl-2))
  (kcl-sockets
    :defines (:functions)
    :requires (xfork)
    :compiler compile-with-C
    :loader load-with-C)
  #+(and sun (not :gcl-2))
  (kcl-sockets-non-ansi
    :defines (:functions)
    :requires (xfork)
    :compiler compile-with-C
    :loader load-with-C)
  )

;;; KCL Compile/load routine for files that include definitions in C

#+kcl
(defun compile/load-file-with-C-fns (pathname library)
  ;; /\/ Wouldn't know :output-file for compile, because it's not
  ;; an arg to load and hence not given to load-with-C.  Maybe the
  ;; special compilers and loaders should be passed the file struct.
  (let ((source (merge-pathnames (make-pathname :type "lsp") pathname))
	(object (merge-pathnames (make-pathname :type "o") pathname)))
    (when (or (not (probe-file object))
	      (> (file-write-date source) (file-write-date object)))
      (safe-compile-file-with-C-fns pathname))
    #-:gcl-2
    (si:faslink object library)
    #+:gcl-2
    (load object)))

#+kcl
(defun safe-compile-file-with-C-fns (filename)
  (let ((*readtable* (copy-readtable nil))) ;for % char macro
    (compile-file filename)))

#+kcl
(defun compile-with-C (pathname &key output-file)
  ;; /\/ Ignores output file
  (safe-compile-file-with-C-fns pathname))

#+kcl
(defun load-with-C (pathname)
  (compile/load-file-with-C-fns pathname "-lc"))


;;; Lucid support

#+lucid
(defsystem (dependent-support :directory "support"
			      :required-systems (base-support))
  #-Liquid
  (lucid-conditions)
  (lucid-util
    :defines (:functions))
  (lucid-xwindowio
    :defines (:functions))
  )


;;; Allegro support

#+:allegro
(defsystem (dependent-support :directory "support"
			      :required-systems (base-support))
  (allegro-util
    :defines (:functions))
  (allegro-xwindowio
    :defines (:functions))
  )


;;; Common Support

(defsystem (common-support :directory "support"
			   :required-systems (base-support
					      external-support))
  ;; For util package and macros, see system base-support.
  (util-functions
    :defines (:functions))
  (inf-arithmetic
    :defines (:functions		;but inline functions
	      :macros))
  (struct-macros
    :defines (:macros))
  (clos-macros
    :defines (:macros))
  (simple-matcher
    :defines (:functions))		;/\/ some exports too
  (time
    :defines (:package :functions))
  (pseudo-process
    :requires (util-functions))		;for set-parameter
  (developerlib
    :requires (ipc))			;for ipc-whoami /\/
  (context)				;was in DB
  (xwindowio
    :requires (developerlib))
  ("whats-going-on"			;requires UTIL package
    :requires (developerlib))
  ;("../random/obhash"
  ;  :defines (:functions))
  ;(run-lights				;now in top-level
  ;  :requires (ipc xwindowio))
  (ipc					;/\/ --> move to after pseudo-process
    :defines (:package :macros :functions)
    :requires (util-functions		;eg concat-name for defmessage
	       pseudo-process))
  (components
    :requires (developerlib		;for var names
	       context			;for var names
	       pseudo-process		;:includes struct
	       ipc))			;for defmessage etc
  )


;;; External support

(defsystem (external-support :directory "support")
  #+kcl
  (xp)
  #+kcl
  (loop)
  (psgraph
    :defines (:functions))		;so far as we're concerned
  )


;;; Base-support contains portable code needed at compile-time by both
;;; common-support and dependent-support (and then, indirectly, by pretty
;;; much everything else).  The main effect of this system is to define
;;; the UTIL package, which has some of its function definitions filled in
;;; by dependent-support and others filled in by parts of common-support.

;;; Since the base-support part of UTIL is portable, it would normally
;;; be in common-support; but then dependent- support would require
;;; common-support (instead of base-support), a much larger system.
;;; In short, the base-support system exists to eliminate some
;;; unnecessary dependencies.  It adds some unnecessary dependencies
;;; too, because many files in common-support don't need the UTIL
;;; package.  But it allows us to use UTIL freely, without having to
;;; add "util" as a required file each time, and the definitions in
;;; UTIL should seldom require modification.

;;; /\/: Parts of developerlib should probably go here as well.

(defsystem (base-support :directory "support")
  (util-package
    :defines (:package)
    :do-after-load (use-package :oplan-util :oplan))
  (util-macros
    :defines (:macros)
    :requires (util-package))
  (arithmetic-dcls
    :defines (:macros :types)
    :requires (util-package)))

;;; End
