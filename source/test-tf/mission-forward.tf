;;; Missionaries and cannibals

;;; Author: Jeff Dalton

;;; Updated: Fri Nov 25 00:03:57 1994 by Jeff Dalton

;;; This is very different from earlier versions.

;;; Recursive expansion is used to work forward from the initial
;;; state while earlier versions used achieve conditions to work
;;; backwards from the goal.  When working forwards, the only_use_if
;;; for the "tried" condition acts as a filter on schema selection 
;;; (rather than just setting up a protected range so that attempts
;;; to revisit the same state fail when they try to assert a "tried"
;;; effect for the state).  So working forward is easier to follow
;;; (try setting the schema select mode to :ask) and more efficient.
;;; However, the recursive expansion results in long node numbers
;;; (e.g., node-3-2-2-2-2).

;;; Determining the next state and checking whether it's safe is all
;;; done in one compute condition.  It would be possible to do more of
;;; the work in O-Plan (and less in Lisp) by using more compute conditions.

;;; There's one schema for each number (1,2) of missionaries / cannibals
;;; to move, plus a schema for moving one of each, but each schema handles
;;; moves in both directions.

;;; The first solution is obtained without any backtracking.  This is
;;; presumably just luck and due to the order of the "move_" schemas.
;;; No attempt was made to "tune" by altering their positions.


;;; A state has the form {boat_bank m_left c_left m_right c_right}.

;;; Presumably we could omit the counts for one side, since they're
;;; implicit in the counts for the other.

language lisp;

  (defun safe (m c)
    (and (safe1 m c)      ; on one bank
         (safe1 (- 3 m)   ; and the other bank
                (- 3 c))))

  (defun safe1 (m c)      ; on one bank
    (and (>= m 0)
         (>= c 0)
         (or (= m 0)
             (>= m c))))

  (defun state_p (x)                     ; roughly...
    (and (listp x) (= (length x) 5)))

  (defun next_state (state m c)
    (apply #'get_next_state m c state))

  (defun get_next_state (m c bank ml cl mr cr)
    ;; move m missionaries and c cannibals from the given bank to
    ;; its opposite.
    (multiple-value-bind (new-bank new-ml new-cl new-mr new-cr)
        (ecase bank
          (left  (values 'right (- ml m) (- cl c) (+ mr m) (+ cr c)))
          (right (values 'left  (+ ml m) (+ cl c) (- mr m) (- cr c))))
      (if (safe new-ml new-cl)
          (list new-bank new-ml new-cl new-mr new-cr)
        'not-safe)))

end_language;


;;; Tasks

initially {tried {left  ?? ?? ?? ??}} = false,
          {tried {right ?? ?? ?? ??}} = false;

task mc_problem;
  nodes 1 start,
        2 finish,
        3 action {from {left 3 3 0 0}};
  orderings 1 ---> 3, 3 ---> 2;
end_task;


;;; The empty action

schema finished;
  expands {from {right 0 0 3 3}};
end_schema;


;;; A primitive

schema move;
  vars ?m = ?{satisfies numberp},
       ?c = ?{satisfies numberp};
  expands {move ?m ?c};
end_schema;


;;; Moving people

;;; Meta-schemas would be useful here.  The schemas differ only in
;;; the numbers given to move and next_state.  /\/

schema move_2_m;
  vars ?state = ?{satisfies state_p},
       ?new_state = ?{satisfies state_p};
  expands {from ?state};
  nodes
     sequential
        1 action {move 2 0},
        2 action {from ?new_state}
     end_sequential;
  conditions
     compute {next_state ?state 2 0} = ?new_state,
     only_use_if {tried ?new_state} = false;
  effects {tried ?state} = true at begin_of self;
end_schema;

schema move_2_c;
  vars ?state = ?{satisfies state_p},
       ?new_state = ?{satisfies state_p};
  expands {from ?state};
  nodes
     sequential
        1 action {move 0 2},
        2 action {from ?new_state}
     end_sequential;
  conditions
     compute {next_state ?state 0 2} = ?new_state,
     only_use_if {tried ?new_state} = false;
  effects {tried ?state} = true at begin_of self;
end_schema;

schema move_1_m;
  vars ?state = ?{satisfies state_p},
       ?new_state = ?{satisfies state_p};
  expands {from ?state};
  nodes
     sequential
        1 action {move 1 0},
        2 action {from ?new_state}
     end_sequential;
  conditions
     compute {next_state ?state 1 0} = ?new_state,
     only_use_if {tried ?new_state} = false;
  effects {tried ?state} = true at begin_of self;
end_schema;

schema move_1_c;
  vars ?state = ?{satisfies state_p},
       ?new_state = ?{satisfies state_p};
  expands {from ?state};
  nodes
     sequential
        1 action {move 0 1},
        2 action {from ?new_state}
     end_sequential;
  conditions
     compute {next_state ?state 0 1} = ?new_state,
     only_use_if {tried ?new_state} = false;
  effects {tried ?state} = true at begin_of self;
end_schema;

schema move_1_m_1_c;
  vars ?state = ?{satisfies state_p},
       ?new_state = ?{satisfies state_p};
  expands {from ?state};
  nodes
     sequential
        1 action {move 1 1},
        2 action {from ?new_state}
     end_sequential;
  conditions
     compute {next_state ?state 1 1} = ?new_state,
     only_use_if {tried ?new_state} = false;
  effects {tried ?state} = true at begin_of self;
end_schema;

;;; End
