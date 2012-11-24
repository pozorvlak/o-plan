;;;
;;; pacifica-gf: No recursive evacuate
;;;

;;;                 Domain Fact and Type Definitions

types
      truck 	  = (Truck_1 Truck_2),
      helicopter  = (Heli_1),
      vehicle     = (Truck_1 Truck_2),
      city        = (Abyss Barnacle Calypso Delta Exodus);

always {capacity truck} = 35,
       {capacity helicopter} = 25,
       {evacuate_to Delta};

initially
	{people_in Abyss}    = 50,
	{people_in Barnacle} = 100,
	{people_in Calypso}  = 20,
	{people_in Delta}    = 0,
	{people_in Exodus}   = 20,
	{people_in Truck_1} = 0,
	{people_in Truck_2} = 0,
	{people_in Heli_1} = 0;


;;;                       Task Definitions

task test_choose_two_trucks;
  nodes sequential
          1 start,
	  parallel
            3 action {transport count 20 from Calypso to Delta},
            4 action {transport count 20 from Exodus to Delta}
	  end_parallel,
          2 finish
        end_sequential;
end_task;

task simple_accumulation_test;
  nodes sequential
          1 start,
            3 action {unload count 20 from Truck_1 at Delta},
            4 action {unload count 20 from Truck_2 at Delta},
          2 finish
        end_sequential;
end_task;

;;;
;;; Similar, but vehicle not specified (let oplan choose it).
;;;
schema transport_choose_vehicle;
  vars ?count    = ?{satisfies numberp},
       ?from     = ?{type city},
       ?to       = ?{type city},
       ?vehicle  = ?{type vehicle};
  expands {transport count ?count from ?from to ?to};
  nodes
    sequential
      1 action {move vehicle ?vehicle from Delta to ?from},
      2 action {load count ?count into ?vehicle at ?from},
      3 action {move vehicle ?vehicle from ?from to ?to},
      4 action {unload count ?count from ?vehicle at ?to}
    end_sequential;
  use ?vehicle across self,
      ?from across 2,
      ?to across 4;
end_schema;

;;;
;;;------------------------------------------------------------------------
;;;          Primitive Level Schemas
;;;

schema load;
  vars	?count = ?{satisfies numberp},
	?vehicle = ?{type vehicle},
	?city = ?{type city},
	?num_before = ?{satisfies numberp},
	?num_after = ?{satisfies numberp};
  expands {load count ?count into ?vehicle at ?city};
  conditions only_use_if {people_in ?city} = ?num_before at begin_of self,
	     compute {- ?num_before ?count} = ?num_after;
  effects {people_in ?city} = ?num_after at end_of self;
  time_windows duration self = 30 minutes .. 60 minutes;
end_schema;

schema unload;
  vars	?count = ?{satisfies numberp},
	?vehicle = ?{type vehicle},
	?city = ?{type city},
	?num_before = ?{satisfies numberp},
	?num_after = ?{satisfies numberp};
  expands {unload count ?count from ?vehicle at ?city};
  conditions only_use_if {people_in ?city} = ?num_before at begin_of self,
	     compute {+ ?num_before ?count} = ?num_after;
  effects {people_in ?city} = ?num_after at end_of self;
  time_windows duration self = 30 minutes .. 60 minutes;
end_schema;

schema move_by_truck;
  vars	?vehicle = ?{type truck},
	?from    = ?{type city},
	?to      = ?{type city};
  expands {move vehicle ?vehicle from ?from to ?to};
  time_windows duration self = 4 hours .. 6 hours;
end_schema;

