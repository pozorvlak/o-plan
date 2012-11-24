;;; File: multiple-answer-compute-example.tf
;;; Contains: Multiple-answer compute or-tree merging examples
;;; Created: Jeff Dalton, November 1996
;;; Updated: Sun Mar 21 00:58:24 1999 by Jeff Dalton

types place = (new_york london newcastle munich);

compute_condition multiple_answer {some_from ??} = ??;

language lisp;

  (defun some_from (items)
    items)

end_language;

;;; Example 1 -- two solutions

task example_1;
  nodes 
     1 start,
     2 finish,
     3 action {go_here_and_there};
  orderings 1 ---> 3, 3 ---> 2;
end_task;

;;; Example 2 -- one solution from an or-tree merge

task example_2;
  nodes 
     1 start,
     2 finish,
     3 action {go_here_and_there2};
  orderings 1 ---> 3, 3 ---> 2;
end_task;

;;; Example 3 -- one schema fails from an or-tree merge, and a second
;;; schema allows one solution as in Example 2.

task example_3;
  nodes 
     1 start,
     2 finish,
     3 action {go_here_and_there3};
  orderings 1 ---> 3, 3 ---> 2;
end_task;

schema go_here_and_there;
  vars ?place = ?{type place};
  expands {go_here_and_there};
  nodes
    1 action {go_to ?place};
  conditions
    compute {some_from {newcastle london}} = ?place;
end_schema;

schema go_here_and_there2;
  vars ?place = ?{type place};
  expands {go_here_and_there2};
  nodes
    1 action {go_to ?place};
  conditions
    compute {some_from {newcastle london}} = ?place,
    compute {some_from {new_york newcastle}} = ?place;
end_schema;

schema losing_go_here_and_there3;
  vars ?place = ?{type place};
  expands {go_here_and_there3};
  nodes
    1 action {go_to ?place};
  conditions
    compute {some_from {newcastle london}} = ?place,
    compute {some_from {new_york}} = ?place;
end_schema;

schema go_here_and_there3;
  vars ?place = ?{type place};
  expands {go_here_and_there3};
  nodes
    1 action {go_to ?place};
  conditions
    compute {some_from {newcastle london}} = ?place,
    compute {some_from {new_york newcastle}} = ?place;
end_schema;

;;; The primitive action

schema go_to;
  vars ?where;
  expands {go_to ?where};
end_schema;
