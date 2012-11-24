;;; Recursive expansion test

;;; Author: Jeff Dalton

;;; Updated: Thu May 28 15:14:31 1998 by Jeff Dalton

task expand_2_3;
  nodes 1 start,
        2 finish,
        3 action {expand top 2 3};
  orderings 1 ---> 3, 3 ---> 2;
end_task;

task expand_3_2;
  nodes 1 start,
        2 finish,
        3 action {expand top 3 2};
  orderings 1 ---> 3, 3 ---> 2;
end_task;

task expand_4_3;
  nodes 1 start,
        2 finish,
        3 action {expand top 4 3};
  orderings 1 ---> 3, 3 ---> 2;
end_task;

task expand_2_6;
  nodes 1 start,
        2 finish,
        3 action {expand top 2 6};
  orderings 1 ---> 3, 3 ---> 2;
end_task;

task expand_10_1;
  nodes 1 start,
        2 finish,
        3 action {expand top 10 1};
  orderings 1 ---> 3, 3 ---> 2;
end_task;

task expand_100_1;
  nodes 1 start,
        2 finish,
        3 action {expand top 100 1};
  orderings 1 ---> 3, 3 ---> 2;
end_task;

task expand_1000_1;
  nodes 1 start,
        2 finish,
        3 action {expand top 1000 1};
  orderings 1 ---> 3, 3 ---> 2;
end_task;

task expand_5000_1;
  nodes 1 start,
        2 finish,
        3 action {expand top 5000 1};
  orderings 1 ---> 3, 3 ---> 2;
end_task;

task expand_10000_1;
  nodes 1 start,
        2 finish,
        3 action {expand top 10000 1};
  orderings 1 ---> 3, 3 ---> 2;
end_task;

schema expand;
  vars ?branching = ?{satisfies numberp},
       ?depth     = ?{satisfies numberp},
       ?d         = ?{satisfies numberp},
       ?children  = ?{satisfies listp};
  expands {expand ?? ?branching ?depth};
  nodes 
    1 foreach action {expand ?c ?branching ?d}
        for ?c over ?children;
  conditions 
    compute {> ?depth 0},
    compute {- ?depth 1} = ?d,
    compute {enumerate 1 ?branching} = ?children;
end_schema;

schema leaf;
  expands {expand ?? ?? 0};
end_schema;

language lisp;

  #+:undef
  (defun enumerate (from to)
    (if (> from to)
        '()
      (cons from (enumerate (1+ from) to))))

  (defun enumerate (fr to) (loop for i from fr to to collect i))

end_language;
