;;;
;;; Simple block stacking domain for O-Plan2.
;;;
;;; This is coded in a similar fashion to that handled by earlier planners
;;; (such as NOAH and NONLIN).  The schemas are not sufficiently general
;;; to generate all possible plans (due in particular to over constrained
;;; condition typoes - only_use_if being used when it is not appropriate
;;; for block stacking domains.
;;;
;;; Several schemas are provided and are intended for specified purposes.
;;; This differs from the simpler, less efficient, but correct
;;; blocks_1.tf block stacking domain description.
;;;
;;; BAT   5-Dec-76: Nonlin TF.
;;; KWC   9-Sep-85: Converted to O-Plan1 TF.
;;; RBK  27-Sep-91: Added object_types for defining type restrictions for
;;;                 variables.
;;; BD   12-May-92: Validated as adhering to O-Plan2 TF.
;;; RBK   2-Jul-92: Condition types corrected.
;;; BAT  28-Jul-92: Task schemas made regular.
;;; BAT  30-Apr-93: Coments added to mention only_use_if handling restrictions.
;;;

;;;
;;; give conditions and effects at same end of a node so they are instantaneous
;;;

defaults condition_node_end = begin_of,
         condition_contributor_node_end = begin_of,
         effect_node_end = begin_of;

;;;
;;; Always defines statements which can never be refuted by action effects
;;;

always {cleartop table};

;;;
;;; Objects in the world.
;;;

types objects = (a b c table), 
      movable_objects = (a b c);

schema puton;
  vars ?x = ?{type movable_objects}, 
       ?y = ?{type objects},
       ?z = ?{type objects};
  vars_relations ?x /= ?y, ?x /= ?z, ?y /= ?z;
  expands {puton ?x ?y};
  conditions only_use_if {cleartop ?x},
             only_use_if {cleartop ?y},
             only_use_for_query {on ?x ?z};
  effects
          {on ?x ?y}    = true,
          {cleartop ?y} = false,
          {on ?x ?z}    = false,
          {cleartop ?z} = true;
end_schema;

schema makeon;
  vars ?x = ?{type movable_objects},  
       ?y = ?{type objects};
  vars_relations ?x /= ?y;
  only_use_for_effects {on ?x ?y} = true;
  nodes 1 action {puton ?x ?y};
  conditions achieve {cleartop ?x} at 1,
             achieve {cleartop ?y} at 1;
  effects {cleartop ?x} = true,
          {cleartop ?y} = false;
end_schema;

schema makeclear;
  vars ?x = ?{type movable_objects}, 
       ?y = ?{type movable_objects};
  vars_relations ?x /= ?y;
  only_use_for_effects {cleartop ?x} = true;
  nodes 1 action {puton ?y table};
  conditions only_use_if {on ?y ?x} at 1,
             achieve  {cleartop ?y} at 1;
end_schema;

task stack_ABC;
  nodes 1 start,
        2 finish;
  orderings 1 ---> 2;
  conditions achieve {on a b} = true at 2,
             achieve {on b c} = true at 2;
  effects {on c a} at 1,
          {on a table} at 1,
          {on b table} at 1,
          {cleartop c} at 1,
          {cleartop b} at 1;
end_task;

task stack_ABC_2;
  nodes 1 start,
        2 finish;
  orderings 1 ---> 2;
  conditions achieve {on a b} = true at 2,
             achieve {on b c} = true at 2;
  effects {on a b} at 1,
          {on c table} at 1,
          {on b table} at 1,
          {cleartop c} at 1,
          {cleartop a} at 1;
end_task;

task stack_CBA;
  nodes 1 start,
        2 finish;
  orderings 1 ---> 2;
  conditions achieve {on c b} = true at 2,
             achieve {on b a} = true at 2;
  effects {on c a} at 1,
          {on a table} at 1,
          {on b table} at 1,
          {cleartop c} at 1,
          {cleartop b} at 1;
end_task;

task stack_BAC;
  nodes 1 start,
        2 finish;
  orderings 1 ---> 2;
  conditions achieve {on b a} = true at 2,
             achieve {on a c} = true at 2;
  effects {on c a} at 1,
          {on a table} at 1,
          {on b table} at 1,
          {cleartop c} at 1,
          {cleartop b} at 1;
end_task;













