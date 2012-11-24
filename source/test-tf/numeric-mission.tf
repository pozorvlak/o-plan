;;; Missionaries and cannibals

;;; Author: Jeff Dalton

;;; A state has the form {boat_bank m_left c_left m_right c_right}.

;;; Presumably we could omit the counts for one side, since they're
;;; implicit in the counts for the other.

always 
       ;;; Safety of (m,c): similar to >=, except for m = 0.
       ;;; However, some of these are actually unsafe, since
       ;;; they imply an unsafe situation on the opposite bank.
       ;;; This is ok though, because we check both banks.

       {safe 3 3}, {safe 3 2}, {safe 3 1}, {safe 3 0},
       {safe 2 2}, {safe 2 1}, {safe 2 0},
       {safe 1 1}, {safe 1 0},
       {safe 0 0}, {safe 0 1}, {safe 0 2}, {safe 0 3};


;;; Tasks

task mc_problem;
  nodes 1 start,
        2 finish;
  orderings 1 ---> 2;
  conditions achieve {state} = {right 0 0 3 3} at 2;
  effects {state} = {left 3 3 0 0} at 1,
          {tried ?? ?? ?? ?? ??} = false at 1;
end_task;


;;; Schemas that move people to left to right

schema move_2_missionaries_right;
  vars ?cl = ?{bound},
       ?cr = ?{bound},
       ?new_ml = ?{and ?{bound} ?{has + 2 ?ml}},
       ?new_mr = ?{and ?{bound} ?{has - 2 ?mr}};
  expands {move 2 m r};
  only_use_for_effects {state} = {right ?new_ml ?cl ?new_mr ?cr};
  ;;; want {left ?new_ml+2 ?cl ?new_mr-2 ?cr}
  ;;; hence ?ml = ?new_ml + 2,
  ;;;       ?mr = ?new_mr - 2
  local_vars ?ml,
             ?mr;
  conditions only_use_if {safe ?ml ?cl},
             only_use_if {safe ?mr ?cr},
             only_use_if {tried left ?ml ?cl ?mr ?cr} = false,
             achieve {state} = {left ?ml ?cl ?mr ?cr};
  effects {tried left ?ml ?cl ?mr ?cr} = true;
end_schema;

schema move_2_cannibals_right;
  vars ?ml = ?{bound},
       ?mr = ?{bound},
       ?new_cl = ?{and ?{bound} ?{has + 2 ?cl}},
       ?new_cr = ?{and ?{bound} ?{has - 2 ?cr}};
  expands {move 2 c r};
  only_use_for_effects {state} = {right ?ml ?new_cl ?mr ?new_cr};
  local_vars ?cl,
             ?cr;
  conditions only_use_if {safe ?ml ?cl},
             only_use_if {safe ?mr ?cr},
             only_use_if {tried left ?ml ?cl ?mr ?cr} = false,
             achieve {state} = {left ?ml ?cl ?mr ?cr};
  effects {tried left ?ml ?cl ?mr ?cr} = true;
end_schema;


schema move_1_missionary_right;
  vars ?cl = ?{bound},
       ?cr = ?{bound},
       ?new_ml = ?{and ?{bound} ?{has + 1 ?ml}},
       ?new_mr = ?{and ?{bound} ?{has - 1 ?mr}};
  expands {move 1 m r};
  only_use_for_effects {state} = {right ?new_ml ?cl ?new_mr ?cr};
  ;;; want {left ?new_ml+1 ?cl ?new_mr-1 ?cr}
  ;;; hence ?ml = ?new_ml + 1,
  ;;;       ?mr = ?new_mr - 1
  local_vars ?ml,
             ?mr;
  conditions only_use_if {safe ?ml ?cl},
             only_use_if {safe ?mr ?cr},
             only_use_if {tried left ?ml ?cl ?mr ?cr} = false,
             achieve {state} = {left ?ml ?cl ?mr ?cr};
  effects {tried left ?ml ?cl ?mr ?cr} = true;
end_schema;

schema move_1_cannibal_right;
  vars ?ml = ?{bound},
       ?mr = ?{bound},
       ?new_cl = ?{and ?{bound} ?{has + 1 ?cl}},
       ?new_cr = ?{and ?{bound} ?{has - 1 ?cr}};
  expands {move 1 c r};
  only_use_for_effects {state} = {right ?ml ?new_cl ?mr ?new_cr};
  local_vars ?cl,
             ?cr;
  conditions only_use_if {safe ?ml ?cl},
             only_use_if {safe ?mr ?cr},
             only_use_if {tried left ?ml ?cl ?mr ?cr} = false,
             achieve {state} = {left ?ml ?cl ?mr ?cr};
  effects {tried left ?ml ?cl ?mr ?cr} = true;
end_schema;

schema move_1_of_each_right;
  vars ?new_ml = ?{and ?{bound} ?{has + 1 ?ml}},
       ?new_cl = ?{and ?{bound} ?{has + 1 ?cl}},
       ?new_mr = ?{and ?{bound} ?{has - 1 ?mr}},
       ?new_cr = ?{and ?{bound} ?{has - 1 ?cr}};
  expands {move 1 m 1 c r};
  only_use_for_effects {state} = {right ?new_ml ?new_cl ?new_mr ?new_cr};
  local_vars ?ml,
             ?cl,
             ?mr,
             ?cr;
  conditions only_use_if {safe ?ml ?cl},
             only_use_if {safe ?mr ?cr},
             only_use_if {tried left ?ml ?cl ?mr ?cr} = false,
             achieve {state} = {left ?ml ?cl ?mr ?cr};
  effects {tried left ?ml ?cl ?mr ?cr} = true;
end_schema;


;;; Schemas that move people right to left

schema move_1_missionary_left;
  vars ?cl = ?{bound},
       ?cr = ?{bound},
       ?new_ml = ?{and ?{bound} ?{has - 1 ?ml}},
       ?new_mr = ?{and ?{bound} ?{has + 1 ?mr}};
  expands {move 1 m l};
  only_use_for_effects {state} = {left ?new_ml ?cl ?new_mr ?cr};
  local_vars ?ml,
             ?mr;
  ;;; want {right ?new_ml-1 ?cl ?new_mr+1 ?cr}
  ;;; hence ?ml = ?new_ml - 1,
  ;;;       ?mr = ?new_mr + 1
  conditions only_use_if {safe ?ml ?cl},
             only_use_if {safe ?mr ?cr},
             only_use_if {tried right ?ml ?cl ?mr ?cr} = false,
             achieve {state} = {right ?ml ?cl ?mr ?cr};
  effects {tried right ?ml ?cl ?mr ?cr} = true;
end_schema;

schema move_1_cannibal_left;
  vars ?ml = ?{bound},
       ?mr = ?{bound},
       ?new_cl = ?{and ?{bound} ?{has - 1 ?cl}},
       ?new_cr = ?{and ?{bound} ?{has + 1 ?cr}};
  expands {move 1 c l};
  only_use_for_effects {state} = {left ?ml ?new_cl ?mr ?new_cr};
  local_vars ?cl,
             ?cr;
  conditions only_use_if {safe ?ml ?cl},
             only_use_if {safe ?mr ?cr},
             only_use_if {tried right ?ml ?cl ?mr ?cr} = false,
             achieve {state} = {right ?ml ?cl ?mr ?cr};
  effects {tried right ?ml ?cl ?mr ?cr} = true;
end_schema;

schema move_1_of_each_left;
  vars ?new_ml = ?{and ?{bound} ?{has - 1 ?ml}},
       ?new_cl = ?{and ?{bound} ?{has - 1 ?cl}},
       ?new_mr = ?{and ?{bound} ?{has + 1 ?mr}},
       ?new_cr = ?{and ?{bound} ?{has + 1 ?cr}};
  expands {move 1 m 1 c l};
  only_use_for_effects {state} = {left ?new_ml ?new_cl ?new_mr ?new_cr};
  local_vars ?ml,
             ?cl,
             ?mr,
             ?cr;
  conditions only_use_if {safe ?ml ?cl},
             only_use_if {safe ?mr ?cr},
             only_use_if {tried right ?ml ?cl ?mr ?cr} = false,
             achieve {state} = {right ?ml ?cl ?mr ?cr};
  effects {tried right ?ml ?cl ?mr ?cr} = true;
end_schema;


schema move_2_missionaries_left;
  vars ?cl = ?{bound},
       ?cr = ?{bound},
       ?new_ml = ?{and ?{bound} ?{has - 2 ?ml}},
       ?new_mr = ?{and ?{bound} ?{has + 2 ?mr}};
  expands {move 2 m l};
  only_use_for_effects {state} = {left ?new_ml ?cl ?new_mr ?cr};
  local_vars ?ml,
             ?mr;
  conditions only_use_if {safe ?ml ?cl},
             only_use_if {safe ?mr ?cr},
             only_use_if {tried right ?ml ?cl ?mr ?cr} = false,
             achieve {state} = {right ?ml ?cl ?mr ?cr};
  effects {tried right ?ml ?cl ?mr ?cr} = true;
end_schema;

schema move_2_cannibals_left;
  vars ?ml = ?{bound},
       ?mr = ?{bound},
       ?new_cl = ?{and ?{bound} ?{has - 2 ?cl}},
       ?new_cr = ?{and ?{bound} ?{has + 2 ?cr}};
  expands {move 2 c l};
  only_use_for_effects {state} = {left ?ml ?new_cl ?mr ?new_cr};
  local_vars ?cl,
             ?cr;
  conditions only_use_if {safe ?ml ?cl},
             only_use_if {safe ?mr ?cr},
             only_use_if {tried right ?ml ?cl ?mr ?cr} = false,
             achieve {state} = {right ?ml ?cl ?mr ?cr};
  effects {tried right ?ml ?cl ?mr ?cr} = true;
end_schema;

;;; End


