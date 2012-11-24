;;; File: use-example.tf
;;; Created: Jeff Dalton, 24 march 1996
;;; Updated: Wed Dec  4 21:03:27 1996 by Jeff Dalton

types usable = (hands table);

initially {location self} = home;

task get_to_work_via_expansion;
  nodes 
     1 start,
     2 finish,
     3 action {get_to_work self};
  orderings 1 ---> 3, 3 ---> 2;
  time_windows 8:00 at 1;
end_task;

task get_to_work_via_conditions;
  nodes 
     sequential
        1 start,
        2 finish
     end_sequential;
  conditions
     achieve {location self} = work at 2;
  time_windows 8:00 at 1;
end_task;

schema get_to_work;
  expands {get_to_work self};
  only_use_for_effects {location self} = work;
  nodes 
     sequential
        1 action {get_dressed},
        parallel
           2 action {read_paper},
           3 action {eat_breakfast},
           4 action {make_tea}
        end_parallel,
        5 action {go_to work}
     end_sequential;
end_schema;

;;; Primitives

schema get_dressed;
  expands {get_dressed};
  effects {dressed} = true;
  time_windows duration self = 10 minutes;
end_schema;

schema eat_breakfast;
  vars ?resource = ?{type usable};
  expands {eat_breakfast};
  effects {breakfast_eaten} = true;
  time_windows duration self = 2 minutes .. 30 minutes;
  use ?resource;
end_schema;

schema read_paper;
  vars ?resource = ?{type usable};
  expands {read_paper};
  effects {paper_read} = true;
  time_windows duration self = 1 minutes .. 30 minutes;
  use ?resource;
end_schema;

schema make_tea;
  vars ?resource = ?{type usable};
  expands {make_tea};
  effects {tea_made} = true;
  time_windows duration self = 1 minutes .. 30 minutes;
  use ?resource;
end_schema;

schema walk_to_work;
  expands {go_to work};
  effects {walked} = true;
  time_windows duration self = 15 minutes .. 20 minutes;
end_schema;

;;; Note that taking the bus can be slower than walking, 
;;; because we may have to wait for the bus and we might
;;; get caught in traffic.

;;; schema take_bus_to_work;
;;;   expands {go_to work};
;;;   effects {took_bus} = true;
;;;   time_windows duration self = 5 minutes .. 30 minutes;
;;; end_schema;
