;;; A simple house-building domain with resources,
;;; written in a different way.

;;; Author: Jeff Dalton

;;; Created: 02 March 1994
;;; Updated: Wed Mar  2 18:05:47 1994 by Jeff Dalton

;;; Wolf-proof houses are more expensive.

;;; Tasks:
;;;
;;;   build_house
;;;     Cost is no object, but there's no requirement that the
;;;     house be secure.
;;;   build_secure_house
;;;     The house must be wolf-proof.
;;;     (Only one solution.)
;;;   better_build_secure_house
;;;     Like build_secure_house, but uses a more (search-) efficient builder.
;;;   build_cheap_house
;;;     The house must be inexpensive.
;;;     (More solutions than you expect.)
;;;   build_cheap_secure_house
;;;     The house must be inexpensive and wolf-proof.
;;;     (No solutions.)

;;; It is expected that a number of schemas can be combined once
;;; variables and expressions can be more widely used and once compute
;;; conditions are available.

types material = (straw sticks bricks);

always {proof_against wolf bricks} = true;

resource_units pounds = count;

resource_types
  consumable_strictly {resource money} = pounds,
  consumable_strictly {resource straw},
  consumable_strictly {resource sticks},
  consumable_strictly {resource bricks};

task build_house;
  nodes 1 start,  
        2 finish,
        3 action {build house};
  orderings 1 ---> 3, 3 ---> 2;
end_task;

task build_secure_house;
  nodes 1 start,  
        2 finish,
        3 action {build house},
        4 action {check security};
  orderings 1 ---> 3, 3 ---> 4, 4 ---> 2;
  resources consumes {resource money} = 0 .. 2000 pounds overall;
end_task;

task better_build_secure_house;
  nodes 1 start,  
        2 finish,
        3 action {build secure house};
  orderings 1 ---> 3,
            3 ---> 2;
  resources consumes {resource money} = 0 .. 2000 pounds overall;
end_task;

task build_cheap_house;
  nodes 1 start,  
        2 finish,
        3 action {build house};
  orderings 1 ---> 3, 3 ---> 2;
  resources consumes {resource money} = 0 .. 500 pounds overall;
end_task;

task build_cheap_secure_house;
  nodes 1 start,  
        2 finish,
        3 action {build house},
        4 action {check security};
  orderings 1 ---> 3, 3 ---> 4, 4 ---> 2;
  resources consumes {resource money} = 0 .. 500 pounds overall;
end_task;

schema house_builder;
  vars ?material = ?{type material};
  expands {build house};
  nodes 1 action {purchase ?material},
        2 action {make_walls_from ?material},
        3 action {install door},
        4 action {install windows};
  orderings 1 ---> 2, 2 ---> 3, 2 ---> 4;
  conditions supervised {have ?material} at 2 from [1],
             supervised {walls built} at 3 from [2],
             supervised {walls built} at 4 from [2];
end_schema;

;;; /\/: Security_checker is a separate schema, because we can't
;;; put the materials check directly in the task schema.  Secure_
;;; house_builder is better way of doing this, because it doesn't
;;; lead to so much search.

schema security_checker;
  expands {check security};
  local_vars ?material = ?{type material};
  conditions unsupervised {proof_against wolf ?material},
             unsupervised {walls_of ?material},
             unsupervised {wolf_proof door},
             unsupervised {wolf_proof windows};
end_schema;

schema secure_house_builder;
  expands {build secure house};
  nodes 1 action {build_walls bricks},
        2 action {install door},
        3 action {install windows},
        4 dummy;
  orderings 1 ---> 2, 1 ---> 3, 2 ---> 4, 3 ---> 4;
  conditions supervised {walls_of bricks} at 4 from [1],
             unsupervised {proof_against wolf bricks} at 1,
             supervised {walls built} at 2 from [1],
             supervised {walls built} at 3 from [1],
             supervised {wolf_proof door} at 4 from [2],
             supervised {wolf_proof windows} at 4 from [3];
end_schema;

schema build_walls;
  vars ?material = ?{type material};
  expands {build_walls ?material};
  nodes     1 action {purchase ?material},
            2 action {make_walls_from ?material};
  orderings 1 ---> 2;
  conditions supervised {have ?material} at 2 from [1];
end_schema;


;;; Some primitive actions.

schema purchase_straw;
  expands {purchase straw};
  only_use_for_effects {have straw};
  resources consumes {resource money} = 100 pounds,
            consumes {resource straw} = 1000;
end_schema;

schema purchase_sticks;
  expands {purchase sticks};
  only_use_for_effects {have sticks};
  resources consumes {resource money} = 200 pounds,
            consumes {resource sticks} = 1000;
end_schema;

schema purchase_bricks;
  expands {purchase bricks};
  only_use_for_effects {have bricks};
  resources consumes {resource money} = 1000 pounds,
            consumes {resource bricks} = 1000;
end_schema;

schema make_straw_walls;
  expands {make_walls_from straw};
  only_use_for_effects {walls built},
                       {walls_of straw};
  resources consumes {resource money} = 100 pounds;
end_schema;

schema make_stick_walls;
  expands {make_walls_from sticks};
  only_use_for_effects {walls built},
                       {walls_of sticks};
  resources consumes {resource money} = 200 pounds;
end_schema;

schema make_brick_walls;
  expands {make_walls_from bricks};
  only_use_for_effects {walls built},
                       {walls_of bricks};
  resources consumes {resource money} = 500 pounds;
end_schema;

schema install_wolf_proof_door;
  expands {install door};
  only_use_for_effects {door installed},
                       {wolf_proof door};
  resources consumes {resource money} = 100 pounds;
end_schema;

schema install_door;
  expands {install door};
  only_use_for_effects {door installed};
  resources consumes {resource money} = 50 pounds;
end_schema;

schema install_wolf_proof_windows;
  expands {install windows};
  only_use_for_effects {windows installed},
                       {wolf_proof windows};
  resources consumes {resource money} = 100 pounds;
end_schema;

schema install_windows;
  expands {install windows};
  only_use_for_effects {windows installed};
  resources consumes {resource money} = 50 pounds;
end_schema;
