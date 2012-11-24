;;; trains-1.tf -- A simple transport domain.

;;; Author: Jeff Dalton

;;; Created: January 1997
;;; Updated: Fri Jun  4 04:09:20 1999 by Jeff Dalton

;;; This domain uses a map of the NE US and SE Canada similar to that
;;; used by TRAINS.  A plan can contain trips for several different
;;; trains.  A trip is introduced by an action {trip ?train ?path} in
;;; the task, where a path {A I-1 I-2 ... I-n B} specifies a route
;;; from A to B via zero or more intermediate stops I_i.  For example:
;;;
;;;    action {trip T1 {Boston Charleston Atlanta}}
;;;
;;; Trains are not allowed to return to places they have already been.
;;; This eliminates some perverse plans (though it may also eliminate
;;; some reasonable ones) and prevents infinite recursion in the expansion
;;; of "connect" actions.
;;; 
;;; A task-level "trip" action expands into a sequence of "connect"
;;; actions which (recursively) expand to fill-in gaps in the path.
;;; Direct connections then expand into actions of the form
;;; {take ?train ?from ?to}.  This level is not really necessary
;;; but makes it easier to see the actual routes.  (Otherwise,
;;; you'd have to distinguish between different cases of "connect".)
;;; The "take" actions also allow us to hide connect actions in
;;; psgraphs, giving us more readable graphs.

;;; {visited ?train ?city} is used to keep trains from returning
;;; to places they've already been.

;;; The root cause of the "lines" between cities being handled in Lisp
;;; is that we cannot have a condition only_use_if_not {line ?from ?to}.
;;; There are other ways around this problem, but they are messier.


;;; Cities in vertical slices taken W -> E:
types city = (Milwaukee Chicago Indianapolis
              Detroit Toledo Columbus Cincinnati Lexington Atlanta
              Cleveland
              Toronto Buffalo Pittsburgh Charleston Charlotte
              Syracuse Scranton Baltimore Washington Richmond Raleigh
              Montreal Burlington Albany New_York Philadelphia
              Boston),
      train = (T1 T2 T3);

compute_condition multiple_answer {connected_cities ??} = ??;

initially {visited ?? ??} = false;


;;;; Tasks

;;; The base_problem task will result in an option-1 that contains
;;; no trip actions.  Various different trips can then be added.

task base_problem;
  nodes
    sequential
      1 start,
      2 finish
    end_sequential;
end_task;

task example_1;
  nodes
    sequential
      1 start,
      parallel
        3 action {trip T1 {Boston Charleston Atlanta}}
      end_parallel,
      2 finish
    end_sequential;
end_task;

task example_2;
  nodes
    sequential
      1 start,
      parallel
        3 action {trip T1 {Boston Charleston Atlanta}},
        4 action {trip T2 {Boston New_York}} 
      end_parallel,
      2 finish
    end_sequential;
end_task;

task impossible_sequence;
  nodes
    sequential
      1 start,
      3 action {trip T1 {Charleston Charlotte Atlanta}},
      4 action {trip T1 {Atlanta Charlotte}},
      2 finish
    end_sequential;
end_task;


;;;; Action schemas

;;; Fill_path expands a {trip ?train ?path} action into a sequence of
;;; connect actions, "steps", that will expand into routes between
;;; adjacent cities in the path.  For example, for a path (a b c d),
;;; the steps are ((a b) (b c) (c d)).

schema fill_path;
  vars ?train = ?{bound},
       ?path = ?{bound},
       ?start,
       ?steps;
  expands {trip ?train ?path};
  nodes
    1 iterate action {connect ?train ?step}
        for ?step over ?steps;
  conditions
    compute {first ?path} = ?start,
    compute {path_steps ?path} = ?steps;
  effects
    {visited ?train ?start} = true at begin_of 1;
end_schema;

;;; Connect_directly expands a {connect {?from ?to}} action when
;;; when there is a direct line between ?from and ?to.

schema connect_directly;
  vars ?train = ?{type train},
       ?from = ?{type city},
       ?to =   ?{type city};
  vars_relations ?from /= ?to;
  expands {connect ?train {?from ?to}};
  nodes 1 action {take ?train ?from ?to};
  conditions
    compute {directly_connected ?from ?to},
    only_use_if {visited ?train ?to} = false at begin_of self;
  effects 
    {visited ?train ?to} = true at end_of self;
end_schema;

;;; Connect expands a {connect {?from ?to}} action when there is no
;;; no direct line between ?from and ?to and hence an intermediate
;;; destination is needed.

schema connect;
  vars ?train = ?{type train},
       ?from = ?{type city},
       ?to =   ?{type city},
       ?via =  ?{type city};
  vars_relations ?from /= ?to, ?via /= ?from, ?via /= ?to;
  expands {connect ?train {?from ?to}};
  nodes
    sequential
      1 action {connect ?train {?from ?via}},
      2 action {connect ?train {?via ?to}}
    end_sequential;
  conditions
    compute {directly_connected ?from ?to} = false,
    compute {connected_cities ?from} = ?via,
    only_use_if {visited ?train ?via} = false;
end_schema;

;;; Take_from_to is the primitive for expanding {take ?train ?from ?to}.
;;; The easiest way to determine the actual train movements in a plan may
;;; be to extract the take actions.

schema take_from_to;
  vars ?train = ?{type train},
       ?from  = ?{type city},
       ?to    = ?{type city};
  expands {take ?train ?from ?to};
end_schema;


;;;; Lisp definitions

language lisp;

  (defmacro connections (from)
    `(get ,from 'connections))

  (defmacro line (from to)
    `(progn
       (nconcf-new (connections ',from) ',to)
       (nconcf-new (connections ',to  ) ',from)))

  ;; Connections listed are those towards the (logical) NE .. S
  (line Milwaukee Chicago)
  (line Chicago Toledo) 
  (line Chicago Indianapolis)
  (line Indianapolis Columbus)
  (line Indianapolis Cincinnati)
  (line Indianapolis Lexington)
  (line Detroit Toronto)
  (line Detroit Toledo)
  (line Toledo Cleveland)
  (line Toledo Columbus)
  (line Columbus Pittsburgh)
  (line Columbus Cincinnati)
  (line Cincinnati Charleston)
  (line Cincinnati Lexington)
  (line Lexington Atlanta)
  (line Atlanta Charlotte)
  (line Cleveland Buffalo)
  (line Toronto Montreal)
  (line Toronto Buffalo)
  (line Buffalo Syracuse)
  (line Buffalo Pittsburgh)
  (line Pittsburgh Scranton)
  (line Pittsburgh Charleston)
  (line Charleston Richmond)
  (line Charleston Charlotte)
  (line Charlotte Raleigh)
  (line Syracuse Albany)
  (line Syracuse Scranton)
  (line Scranton New_York)
  (line Scranton Philadelphia)
  (line Scranton Baltimore)
  (line Baltimore Philadelphia)
  (line Baltimore Washington)
  (line Washington Richmond)
  (line Richmond Raleigh)
  ; Raleigh
  (line Montreal Burlington)
  (line Burlington Albany)
  (line Albany Boston)
  (line Albany New_York)
  ; New_York
  (line Philadelphia New_York)
  ; Boston

  (defun directly_connected (from to)
    (member to (connections from)))

  (defun connected_cities (from)
    (connections from))

  (defun path_steps (path)
    ;; E.g. from path (a b c d) to steps ((a b) (b c) (c d)).
    (let ((steps '()))
      (for-adjacent-elements
         #'(lambda (from to)
             (push (list from to) steps))
         path)
      (nreverse steps)))

  ;; Hide connect actions in psgraphs.
  (defun-for-domain psgraph-hidden-actions ()
    '(connect))

end_language;
