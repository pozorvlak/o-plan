;;; File: bute:/home/oplan/development/prerelease/demo/tf/pacifica4.tf
;;; Contains: TF for Pacifica repair demonstration for Year 3
;;; Author: Brian Drabble (bd@bute)
;;; Created: Brian Drabble 7th March 1995
;;; Updated: Thu Jun  8 01:49:31 1995 by Jeff Dalton

;;; Purpose: PRECiS NEO (Non-combatant Evacuation Operations) Scenario.
;;;          Domain description for transportation logistics problems.
;;; Created:  Brian Drabble:  6th April 1994
;;;
;;; Pacifica is an island state in the Pacific.  Due to its inaccessibility
;;; over the centuries, it remains shrouded in mystery.  Parts of its terrain
;;; remain unexplored...
;;;
;;; Status Notes: Refer to the file BD-README for details
;;;
;;; world model:
;;;   {country ?{type location}} = ?{type country}
;;;   {in_use_for_gt ?{type ground_transport}}
;;;                              = in_transit, available, ?{type city}
;;;   {in_use_for_at ?{type air_transport}}
;;;                              = in_transit, available, ?{type city}
;;;   {location_gt ?{type ground_transport}
;;;                              = ?{type location}
;;;   {location_at ?{type air_transport}
;;;                              = ?{type location}
;;;   {people_at_POE_from ?{type city}} = 0, 50
;;;                              POE is Point Of Embarkation
;;;   {nationals out} = true
;;;
;;; The schemas contained within this files are used as repair actions for
;;; the precis domain. Within the domain a number of changes can take place
;;; either in terms of the task, i.e. new evacuuees need to be picked up or
;;; in terms of the environment in which failures of transports can occur.
;;; The failure of transports occur due to either blown engines or blown
;;; tyres.
;;;
;;; The repair actions available are as follows:
;;;
;;; repair-tyre : The driver of the ground transport can fix the tyre
;;;               by the side of the road. The effect of the repair
;;;               action is to delay the ground transport by a fixed
;;;               amount of time.
;;;
;;; repair-engine : The engine can only be fixed by a repair crew which is
;;;                 dispatched from Delta with a tow truck. The transport is
;;;                 then towed to Delta for repairs. The evacuees remain with
;;;                 truck while it is being towed.
;;;
;;; move-by-air : The failure of the transport occurs in a time critical
;;;               situation and there is insufficient time to tow the broken
;;;               transport to Delta. The evacuees are moved from the broken
;;;               ground transport by helicopter to Delta and the transport
;;;               is abandoned.
;;;
;;; move-by-road: This is similar to the move-by-air schema except that the
;;;               ecacuees are moved by another ground transport instead of
;;;               by helicopter.

types 
 ;;;
 ;;; Transport Assets
 ;;;
     ground_transport         = (GT1 GT2),
     repair_truck             = (RT1),
     ground_vehicles          = (GT1 GT2 RT1),
     helicopter               = (AT1),
     air_transporter_cargo    = (C5 C130 C141),
     air_transporter_evacuees = (B707),
 ;;;
 ;;; Geograpghic Information
 ;;;
     country          = (Pacifica Hawaii_USA),
     location         = (Abyss Barnacle Calypso Delta Honolulu Breakdown),
     city             = (Abyss Barnacle Calypso Delta),
     air_base         = (Delta Honolulu),
     transport_use    = (in_transit available Abyss Barnacle Calypso Delta);

 ;;;
 ;;; resource information
 ;;;
 ;;;   resource_units gallons = count;

resource_types
     consumable_strictly {resource aviation_fuel_delta_tank1} = gallons,
     consumable_strictly {resource diesel_fuel_delta_tank2} = gallons;

always {country Abyss} = Pacifica,
       {country Barnacle} = Pacifica,
       {country Calypso} = Pacifica,
       {country Delta} = Pacifica,
       {country Honolulu} = Hawaii_USA;

;;;
;;;------------------------------------------------------------------------
;;;                        Task Definitions

task Operation_Repair;
  nodes 1 start,
        2 finish,
        3 action {evacuate_stranded_people by_road},
        4 action {evacuate_stranded_people by_air},
        5 action {repair blown_tyre},
        6 action {repair blown_engine in_situ};
  orderings 1 ---> 3, 1 ---> 4, 1 ---> 5, 1 ---> 6,
            3 ---> 2, 4 ---> 2, 5 ---> 2, 6 ---> 2;
end_task;

task Operation_Columbus;
  nodes 1  start,
        2  finish,
        3  action {transport_helicopters Honolulu Delta},
	4  action {transport_ground_transports Honolulu Delta},
	5  dummy,
        6  action {transport Abyss Delta},
        7  action {transport Barnacle Delta},
        8  action {transport Calypso Delta},
	9  dummy,
        10 action {fly_passengers Delta Honolulu},
        11 action {transport_ground_transports Delta Honolulu},
	12 action {transport_helicopters Delta Honolulu};

  orderings 1 ---> 3, 1 ---> 4, 3 ---> 5, 4 ---> 5, 5 ---> 6,
            5 ---> 7, 5 ---> 8, 6 ---> 9, 7 ---> 9, 8 ---> 9,
            9 --->10, 9 --->11, 9 --->12, 10---> 2, 11---> 2,
	    12---> 2;

  effects {location_gt GT1} = Honolulu at 1,
          {location_gt GT2} = Honolulu at 1,
          {location_at AT1} = Honolulu at 1,
          {location_at AT2} = Honolulu at 1,
          {at C5} = Honolulu at 1,
          {at C141} = Honolulu at 1,
          {at C130} = Honolulu at 1,          
          {at B707} = Delta at 1,

          {people_at_POE_from Abyss} = 0 at 1,
          {people_at_POE_from Barnacle} = 0 at 1,
          {people_at_POE_from Calypso} = 0 at 1,
          {in_use_for_gt GT1} = in_transit at 1,
          {in_use_for_gt GT2} = in_transit at 1,
          {in_use_for_at AT1} = in_transit at 1,
	  {in_use_for_at AT2} = in_transit at 1;

;;;          {engine_status AT1} = working at 1,
;;;          {engine_status GT1} = working at 1,
;;;          {engine_status GT2} = working at 1,

;;;          {tyre_status GT1} = working at 1,
;;;          {tyre_status GT2} = working at 1;
  resources 
     consumes {resource aviation_fuel_delta_tank1} = 0 .. 160 gallons overall,
     consumes {resource diesel_fuel_delta_tank2} = 0 .. 80 gallons overall;
end_task;

;;;
;;;------------------------------------------------------------------------
;;;                       Support Operators

schema transport_ground_transports;
;;;
;;; This schema is used to provide ground transportation from
;;; one air base location to another air base location.  This must take
;;; place by using a cargo plane to fly the nominated transportation
;;; vehicles from the source base to the destination base.
;;;
;;; fly all transport explictly from one base to another. No "forall" yet.
;;;
  vars  ?FROM = ?{type air_base},
        ?TO   = ?{type air_base},
        ?USE1 = ?{and ?{type transport_use} ?{not ?{type city}}},
        ?USE2 = ?{and ?{type transport_use} ?{not ?{type city}}};

  expands {transport_ground_transports ?FROM ?TO};

  only_use_for_effects {location_gt GT1} = ?TO,
                       {location_gt GT2} = ?TO;

  conditions achieve {at C141} = ?FROM,    ;;; make air lift available
             unsupervised {location_gt GT1} = ?FROM,
             unsupervised {location_gt GT2} = ?FROM,
             unsupervised {in_use_for_gt GT1} = ?USE1,
             unsupervised {in_use_for_gt GT2} = ?USE2;

  effects {at C141} = ?TO,
          {in_use_for_gt GT1} = in_transit at begin_of self,
          {in_use_for_gt GT2} = in_transit at begin_of self,
          {in_use_for_gt GT1} = available at end_of self,
          {in_use_for_gt GT2} = available at end_of self;
end_schema;

;;;
;;;------------------------------------------------------------------------
;;;

schema transport_helicopters;
;;;
;;; This schema is used to provide air transportation from
;;; one air base location to another air base location.  This must take
;;; place by using a cargo plane to fly the nominated transportation
;;; helicopters from the source base to the destination base.
;;;
;;; fly all transport explictly from one base to another. No "forall" yet.
;;;
;;;
  vars  ?FROM = ?{type air_base},
        ?TO   = ?{type air_base},
        ?USE1 = ?{and ?{type transport_use} ?{not ?{type city}}},
        ?USE2 = ?{and ?{type transport_use} ?{not ?{type city}}};

  expands {transport_helicopters ?FROM ?TO};

  only_use_for_effects {location_at AT1} = ?TO,
                       {location_at AT2} = ?TO;

  conditions achieve {at C5} = ?FROM,    ;;; make air lift available
             unsupervised {location_at AT1} = ?FROM,
             unsupervised {location_at AT2} = ?FROM, 
             unsupervised {in_use_for_at AT1} = ?USE1,
             unsupervised {in_use_for_at AT2} = ?USE2;

  effects {at C5} = ?TO,
          {in_use_for_at AT1} = in_transit at begin_of self,
          {in_use_for_at AT2} = in_transit at begin_of self,
          {in_use_for_at AT1} = available at end_of self,
          {in_use_for_at AT2} = available at end_of self;
end_schema;

;;;
;;;------------------------------------------------------------------------
;;;

schema ground_transport_evacuees;
;;;
;;; This schema functionally transports people from one location
;;; to another via ground transportation.  The transporation vehicle
;;; must be taken from an air_base and return to an air_base
;;;
  vars  ?COUNTRY = ?{type country},
        ?LOC1 = ?{type air_base},
        ?LOC2 = ?{type city},
        ?LOC3 = ?{type air_base},
        ?GT   = ?{type ground_transport};

  expands {transport ?LOC2 ?LOC3};

  nodes 1 action {prepare_transport ?GT},
        2 action {drive ?GT ?LOC2},
        3 action {load_gt ?GT ?LOC2},
        4 action {drive ?GT ?LOC3},
        5 action {unload_gt ?GT ?LOC3};

  orderings 1 ---> 2, 2 ---> 3, 3 ---> 4, 4 ---> 5;

  conditions only_use_if {country ?LOC2} = ?COUNTRY,
             only_use_if {country ?LOC3} = ?COUNTRY,
             ;;; above only allows one air_base per country at present (92.12).
             ;;; could make unsupervised if need more than 1 - search increased
             ;;; next could be achieve to pick up available transports?
             unsupervised {location_gt ?GT} = ?LOC1 at 1,
             unsupervised {country ?LOC1} = ?COUNTRY at 1,
             unsupervised {in_use_for_gt ?GT} = available at 1,
             supervised {in_use_for_gt ?GT} = ?LOC2 at 5 from begin_of 1,
             supervised {tyre_status ?GT} = working at 5 from 1,
             supervised {engine_status ?GT} = working at 5 from 1;

  effects {in_use_for_gt ?GT} = ?LOC2 at begin_of 1,  ;;; reserve transport
          {people_at_POE_from ?LOC2} = 50 at 5;
end_schema;

;;;
;;;------------------------------------------------------------------------
;;;

schema air_transport_evacuees;
;;;
;;; This schema functionally transports people from one location
;;; to another via a helicopter.  The helicopter leaves from an 
;;; air_base and return to an air_base afterwards
;;;
  vars  ?COUNTRY = ?{type country},
        ?LOC1 = ?{type air_base},
        ?LOC2 = ?{type city},
        ?LOC3 = ?{type air_base},
        ?Heli = ?{type helicopter};

  expands {transport ?LOC2 ?LOC3};

  nodes 1 action {prepare_helicopter ?Heli},
        2 action {fly ?Heli ?LOC2},
        3 action {load_at ?Heli ?LOC2},
        4 action {fly ?Heli ?LOC3},
        5 action {unload_at ?Heli ?LOC3};

  orderings 1 ---> 2, 2 ---> 3, 3 ---> 4, 4 ---> 5;

  conditions only_use_if {country ?LOC2} = ?COUNTRY,
             only_use_if {country ?LOC3} = ?COUNTRY,
             unsupervised {location_at ?Heli} = ?LOC1 at 1,
             unsupervised {country ?LOC1} = ?COUNTRY at 1,
             unsupervised {in_use_for_at ?Heli} = available at 1,
             supervised {in_use_for_at ?Heli} = ?LOC2 at 5 from begin_of 1,
             supervised {engine_status ?Heli} = working at 5 from 1;

  effects {in_use_for_at ?Heli} = ?LOC2 at begin_of 1,  ;;; reserve transport
          {people_at_POE_from ?LOC2} = 50 at 5;
end_schema;

;;;
;;;------------------------------------------------------------------------
;;;

schema fly_passengers;
;;;
;;; This schema transports the evacuees from the airport at Delta to 
;;; Honolulu. The evacuees must be moved by passenger aircraft.
;;; ideally universally quantify {people_at_POE_from ??} = 0;
;;;
  vars ?TO   = ?{type air_base},
       ?FROM = ?{type air_base};

  expands {fly_passengers ?FROM ?TO};

  ;;; B707 assumed to be at FROM location at present
  ;;; conditions on availability of B707 needed later
  effects {at B707} = ?TO,
          {people_at_POE_from Abyss} = 0,
          {people_at_POE_from Barnacle} = 0,
          {people_at_POE_from Calypso} = 0,
          {nationals out} = true;
end_schema;

;;;
;;;------------------------------------------------------------------------
;;;

schema prepare_the_GT_vehicle;
  vars ?GT = ?{type ground_transport};
  expands {prepare_transport ?GT};
  effects {tyre_status ?GT} = working,
          {engine_status ?GT} = working;
end_schema;

;;;
;;;------------------------------------------------------------------------
;;;

schema prepare_the_helicopter;
  vars ?AT = ?{type helicopter};
  expands {prepare_helicopter ?AT};
  effects {engine_status ?AT} = working;
end_schema;

;;;
;;;------------------------------------------------------------------------
;;;

schema drive_ground_transport;
  vars ?GT   = ?{type ground_vehicles},
       ?LOC  = ?{type location};

  expands {drive ?GT ?LOC};

  effects {location_gt ?GT} = ?LOC;

  resources consumes {resource diesel_fuel_delta_tank2} = 10 gallons;
end_schema;

;;;
;;;------------------------------------------------------------------------
;;;

schema load_ground_transport;
  vars  ?GT   = ?{type ground_transport},
        ?LOC  = ?{type location};

  expands {load_gt ?GT ?LOC};

  conditions unsupervised {location_gt ?GT} = ?LOC,
             unsupervised {in_use_for_gt ?GT} = ?LOC;
end_schema;

;;;
;;;------------------------------------------------------------------------
;;;

schema unload_ground_transport;
  vars ?GT  = ?{type ground_transport},
       ?LOC = ?{type location};

  expands {unload_gt ?GT ?LOC};

  conditions unsupervised {location_gt ?GT} = ?LOC;

  effects {in_use_for_gt ?GT} = available;
end_schema;

;;;
;;;------------------------------------------------------------------------
;;;

schema fly_helicopter_transport;
  vars ?Heli = ?{type helicopter},
       ?LOC  = ?{type location};


  expands {fly ?Heli ?LOC};

  effects {location_at ?Heli} = ?LOC;

  resources consumes {resource aviation_fuel_delta_tank1} = 30 gallons;
end_schema;

;;;
;;;------------------------------------------------------------------------
;;;

schema load_helicopter_transport;
  vars  ?Heli = ?{type helicopter},
        ?LOC  = ?{type location};

  expands {load_at ?Heli ?LOC};

  conditions unsupervised {location_at ?Heli} = ?LOC,
             unsupervised {in_use_for_at ?Heli} = ?LOC;
end_schema;

;;;
;;;------------------------------------------------------------------------
;;;

schema unload_helicopter_transport;
  vars ?Heli= ?{type helicopter},
       ?LOC = ?{type location};

  expands {unload_at ?Heli ?LOC};

  conditions unsupervised {location_at ?Heli} = ?LOC;

  effects {in_use_for_at ?Heli} = available;
end_schema;

;;;------------------------------------------------------------------------
;;;   Repair actions to fix blown tyres and engines or evacuate people
;;;

schema Repair_the_tyre;
   vars ?gt = ?{type ground_transport};
   expands {repair blown_tyre};

   only_use_for_effects {tyre_status ?GT} = working;

   nodes 1 action {remove blown_tyre},
         2 action {replace tyre};

   orderings 1 ---> 2;
end_schema;

;;;
;;;------------------------------------------------------------------------
;;;

schema Repair_the_engine;
   vars ?rt = ?{type repair_truck},
        ?gt = ?{type ground_transport};

   expands {repair blown_engine in_situ};

   only_use_for_effects {engine_status ?gt} = working;

   nodes 1 action {assemble ?rt crew},
         2 action {drive ?rt Breakdown},
         3 action {pickup broken_gt},
         4 action {drive ?rt Delta};

   orderings 1 ---> 2, 2 ---> 3, 3 ---> 4;
end_schema;

;;;
;;;------------------------------------------------------------------------
;;;

schema Move_the_evacuees_by_air;
   vars ?at = ?{type helicopter},
        ?vehicle;

   expands {evacuate_stranded_people by_air};

   only_use_for_effects {engine_status ?vehicle} = working;

   nodes 1 action {scramble ?at crew},
         2 action {fly ?at Breakdown},
         3 action {load_at ?at Breakdown},
         4 action {fly ?at Delta};

   orderings 1 ---> 2, 2 ---> 3, 3 ---> 4;
end_schema;

;;;
;;;------------------------------------------------------------------------
;;;

schema Move_evacuees_by_road;
   vars ?gt = ?{type ground_transport},
        ?vehicle;
 
   expands {evacuate_stranded_people by_road};
 
   only_use_for_effects {engine_status ?vehicle} = working;

   nodes 1 action {assemble ?gt crew},
         2 action {drive ?gt Breakdown},
         3 action {load_gt ?gt Breakdown},
         4 action {drive ?gt Delta};

   orderings 1 ---> 2, 2 ---> 3, 3 ---> 4;
end_schema;

;;;------------------------------------------------------------------------
;;;  Primitives for the repair actions
;;;

schema scramble_the_AT_crew;
  vars ?at = ?{type helicopter};
  expands {scramble ?at crew};
  only_use_for_effects {status AT crew} =  scrambled;
end_schema;

;;;
;;;------------------------------------------------------------------------
;;;

schema assemble_the_GT_crew;
  vars ?vehicle = ?{type ground_vehicles};
  expands {assemble ?vehicle crew};
  only_use_for_effects {status ?vehicle crew} =  assembled;
end_schema;

;;;
;;;------------------------------------------------------------------------
;;;

schema pick_up_the_broken_GT;
   expands {pickup broken_gt};
   only_use_for_effects {broken_down GT} = picked_up;
end_schema;

;;;
;;;------------------------------------------------------------------------
;;;

schema remove_the_blown_tyre;
   expands {remove blown_tyre};
   only_use_for_effects {blown tyre} = removed;
end_schema;

;;;
;;;------------------------------------------------------------------------
;;;

schema replace_the_tyre;
   expands {replace tyre};
   only_use_for_effects {new tyre} = replaced;
end_schema;

;;;------------------------------------------------------------------------
;;; End of Domain Encoding
;;;




