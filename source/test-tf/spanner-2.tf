;;; File: spanner.tf
;;; Purpose: to demonstrate the need for an achieve condition span
;;;          outside of the schema that introduces it.
;;; Created: Jeff Dalton, 4 June 1999

;;; This is a simpler alternative to spanner.tf and one that does
;;; not require that you edit the file to see some of the possibilities.

types room = (clean_room),
      room_status = (clean dirty);

initially {status clean_room} = clean;

;;; Task test_0 has no solutions.

task test_0;
  nodes
    1 start,
    2 finish,
    sequential
       3 action {use_room_carelessly clean_room},
       4 action {do_assembly_0 clean_room}
    end_sequential;
end_task;

;;; Task test_1 has one solution

task test_1;
  nodes
    1 start,
    2 finish,
    sequential
       3 action {use_room_carelessly clean_room},
       4 action {do_assembly_1 clean_room}
    end_sequential;
end_task;

schema careless_use;
  vars ?room = ?{type room};
  expands {use_room_carelessly ?room};
  effects {status ?room} = dirty;
end_schema;

schema do_assembly_0;
  vars ?room = ?{type room};
  expands {do_assembly_0 ?room};
  conditions achieve {status ?room} = clean after begin_of self;
end_schema;

schema do_assembly_1;
  vars ?room = ?{type room};
  expands {do_assembly_1 ?room};
  conditions achieve {status ?room} = clean after end_of start;
end_schema;

schema room_cleaner;
  vars ?room = ?{type room};
  expands {clean_room ?room};
  only_use_for_effects {status ?room} = clean;
end_schema;
