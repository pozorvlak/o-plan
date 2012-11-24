;;; File: san-roberto-thread-2.tf
;;; Purpose: Experimentation with encoding SUO thread 2.
;;; Three platoons are in postion to resuce hostages from the town hall,
;;; the hospital, and the church.  This file encodes the exsploits of
;;; 1st_platoon, the unit charged with taking the church.
;;; Created:  Peter Jarvis   01-Jun-99
;;; Modified: Jeff Dalton    08-Jun-99

types 
  Platoon  = (1st_Platoon 
              2nd_Platoon 
              3rd_Platoon 
              4th_Platoon),
                        
  Squad    = (Team_A_1st_Platoon 
              Team_B_1st_Platoon 
              Team_C_1st_Platoon),
                      
  Sniper   = (Sniper1
              Sniper2),
                    
  Building = (Building31
              Building32
              Building33
              Building34
              Building41
              Building46);
        
always
    ;;; Decompostitions
    {squads_in 1st_platoon}
       = {Team_A_1st_Platoon Team_B_1st_Platoon Team_C_1st_Platoon},

    ;;; Fields of fire
    {can_fire_from Building31 to Building32},
    {can_fire_from Building31 to Building33},
    {can_fire_from Building31 to Building34};
 

;;; ***************
;;; TASK DEFINITIONS
;;; ***************

task PDA_thread_2_situation_as_in_phase_2_report;
  ;;; This task follows the arc outlines in the SUO OPORDS and Phase 2 report.
  ;;; Friendly forces will be in Sniper1's location but not Sniper2's
  ;;; Platoon 2 is in a postion to put fire on Sniper 1.
  nodes 
    1 start,
    2 finish,
    3 action {suppress Sniper1 1st_Platoon},
    4 action {suppress Sniper2 1st_Platoon},
    5 action {clear_under_suppressed_sniper_fire
               Building33
               Team_C_1st_Platoon
               Team_B_1st_Platoon};
  orderings 
    1 ---> 3, 1 ---> 4, 3 --->5, 4--->5, 5--->2;
  effects
    {historic_interest Building33}   = valuable at 1,
    {friendly_occupancy Building31}  = actual at 1,
    {friendly_occupancy Building32}  = planned at 1,
    {friendly_occupancy Building34}  = actual at 1,
    {friendly_occupancy Building41}  = planned at 1,
    {friendly_occupancy Building46}  = not_planned at 1,
    {location_of Sniper1}            = Building32 at 1,
    {location_of Sniper2}            = Building46 at 1,
    {location_of Team_A_1st_Platoon} = Cover_Positions_Building34 at 1,
    {location_of Team_B_1st_Platoon} = Building34 at 1,
    {location_of Team_C_1st_Platoon} = Building34 at 1,
    {location_of 2nd_Platoon}        = Building31 at 1,
    {status_AFSS 1st_Platoon}        = available at 1,
    {status_TACAir 1st_Platoon}      = available at 1;
end_task;


task PDA_thread_2_no_tac_air_or_afss;
   nodes 
    1 start,
    2 finish,
    3 action {suppress Sniper1 1st_Platoon},
    4 action {suppress Sniper2 1st_Platoon},
    5 action {clear_under_suppressed_sniper_fire
                Building33
                Team_C_1st_Platoon
                Team_B_1st_Platoon};
  orderings 
    1 ---> 3, 1 ---> 4, 3 --->5, 4--->5, 5--->2;
  effects
    {historic_interest Building33}   = valuable at 1,
    {friendly_occupancy Building31}  = actual at 1,
    {friendly_occupancy Building32}  = planned at 1,
    {friendly_occupancy Building34}  = actual at 1,
    {friendly_occupancy Building41}  = planned at 1,
    {friendly_occupancy Building46}  = not_planned at 1,
    {location_of Sniper1}            = Building32 at 1,
    {location_of Sniper2}            = Building46 at 1,
    {location_of Team_A_1st_Platoon} = Cover_Positions_Building34 at 1,
    {location_of Team_B_1st_Platoon} = Building34 at 1,
    {location_of Team_C_1st_Platoon} = Building34 at 1,
    {location_of 2nd_Platoon}        = Building31 at 1,
    {status_AFSS 1st_Platoon}        = unavailable at 1,
    {status_TACAir 1st_Platoon}      = unavailable at 1;
end_task;

task PDA_thread_2_friendly_units_also_in_building_46;
   nodes 
    1 start,
    2 finish,
    3 action {suppress Sniper1 1st_Platoon},
    4 action {suppress Sniper2 1st_Platoon},
    5 action {clear_under_suppressed_sniper_fire
               Building33 Team_C_1st_Platoon
               Team_B_1st_Platoon};
  orderings 
    1 ---> 3, 1 ---> 4, 3 --->5, 4--->5, 5--->2;
  effects
    {historic_interest Building33}   = valuable at 1,
    {friendly_occupancy Building31}  = actual at 1,
    {friendly_occupancy Building32}  = planned at 1,
    {friendly_occupancy Building34}  = actual at 1,
    {friendly_occupancy Building41}  = planned at 1,
    {friendly_occupancy Building46}  = planned at 1,
    {location_of Sniper1}            = Building32 at 1,
    {location_of Sniper2}            = Building46 at 1,
    {location_of Team_A_1st_Platoon} = Cover_Positions_Building34 at 1,
    {location_of Team_B_1st_Platoon} = Building34 at 1,
    {location_of Team_C_1st_Platoon} = Building34 at 1,
    {location_of 2nd_Platoon}        = Building31 at 1,
    {status_AFSS 1st_Platoon}        = available at 1,
    {status_TACAir 1st_Platoon}      = available at 1;
end_task;


task PDA_thread_2_no_air_Support_and_no_friendly_support;
   nodes 
    1 start,
    2 finish,
    3 action {suppress Sniper1 1st_Platoon},
    4 action {suppress Sniper2 1st_Platoon},
    5 action {clear_under_suppressed_sniper_fire
               Building33
               Team_C_1st_Platoon
               Team_B_1st_Platoon};
  orderings 
    1 ---> 3, 1 ---> 4, 3 --->5, 4--->5, 5--->2;
  effects
    {historic_interest Building33}   = valuable at 1,
    {friendly_occupancy Building31}  = actual at 1,
    {friendly_occupancy Building32}  = planned at 1,
    {friendly_occupancy Building34}  = actual at 1,
    {friendly_occupancy Building41}  = planned at 1,
    {friendly_occupancy Building46}  = planned at 1,
    {location_of Sniper1}            = Building32 at 1,
    {location_of Sniper2}            = Building46 at 1,
    {location_of Team_A_1st_Platoon} = Cover_Positions_Building34 at 1,
    {location_of Team_B_1st_Platoon} = Building34 at 1,
    {location_of Team_C_1st_Platoon} = Building46 at 1,
    {location_of 2nd_Platoon}        = Building46 at 1,
    {status_AFSS 1st_Platoon}        = unavailable at 1,
    {status_TACAir 1st_Platoon}      = unavailable at 1;
end_task;


;;; *****************
;;; SCHEMA DEFINTIONS
;;; *****************


;;; First Modelling level: General tactics for handling sniper fire.

schema Suppress_Sniper_with_Indirect_Fires_AFSS_Option;
  vars 
    ?the_sniper       = ?{type Sniper},
    ?sniper_location  = ?{type Building},
    ?unit             = ?{type Platoon};
  expands 
    {suppress ?the_sniper ?unit};
  nodes        
    1 action {lauch_AFSS_at ?sniper_location ?unit};
  conditions 
    ;;; first only_use_if designed to bind sniper_location variable only
    only_use_if {location_of ?the_sniper} = ?sniper_location,
    only_use_if {friendly_occupancy ?sniper_location} = not_planned,
    only_use_if {status_AFSS ?unit} = available;
end_schema;

schema Suppress_Sniper_with_Indirect_Fires_TAC_air_Option;
  vars 
    ?the_sniper       = ?{type Sniper},
    ?sniper_location  = ?{type Building},
    ?unit             = ?{type Platoon};
  expands 
    {suppress ?the_sniper ?unit};
  nodes        
    1 action {request_TAC_air_on ?sniper_location ?unit},
    2 action {throw_flair ?sniper_location ?unit};
  orderings
    1--->2;
  conditions 
    ;;; first only_use_if desiged to bind sniper_location variable only
    only_use_if {location_of ?the_sniper} = ?sniper_location,
    only_use_if {friendly_occupancy ?sniper_location} = not_planned,
    only_use_if {status_TACAir ?unit} = available;
end_schema;

schema Suppress_Sniper_with_other_friendly_unit;
  vars 
    ?the_sniper              = ?{type Sniper},
    ?sniper_location         = ?{type Building},
    ?unit                    = ?{type Platoon},
    ?friendly_unit           = ?{type Platoon},
    ?friendly_unit_location  = ?{type Building};
  vars_relations ?unit /= ?friendly_unit;
  expands 
    {suppress ?the_sniper ?unit};
  nodes        
    1 action {lay_suppressing_fire ?sniper_location ?friendly_unit};
  conditions 
    ;;; first only_use_if desiged to bind sniper_location variable only
    only_use_if {location_of ?the_sniper} = ?sniper_location,
    only_use_if {location_of ?friendly_unit} = ?friendly_unit_location,
    only_use_if {can_fire_from ?friendly_unit_location to ?sniper_location};
end_schema;

schema Suppress_Sniper_with_well_aimed_smoke;
  ;;; this is the simplist way of suppressing a sniper.
  ;;; basically, place well aimed smoke in front of his location
  vars 
    ?the_sniper              = ?{type Sniper},
    ?sniper_location         = ?{type Building},
    ?unit                    = ?{type Platoon};
  expands 
    {suppress ?the_sniper ?unit};
  nodes
    1 action {lay_accurate_smoke ?sniper_location ?unit};
  conditions 
    ;;; first only_use_if desiged to bind sniper_location variable only
    only_use_if {location_of ?the_sniper} = ?sniper_location;
end_schema;

schema clear_a_building_under_suppressed_sniper_fire;
  vars 
    ?building_to_clear  = ?{type Building},
    ?cover_squad = ?{type Squad},
    ?attack_squad = ?{type Squad};
  expands 
    {clear_under_suppressed_sniper_fire
        ?building_to_clear ?attack_squad ?cover_squad};
  nodes
    1 action {lay_down_smoke ?building_to_clear ?cover_squad ?attack_squad},
    2 action {cover_movement ?cover_squad ?attack_squad},
    3 action {storm_objective ?building_to_clear ?attack_squad};
  orderings
    1--->2, 1--->3;
  conditions
    supervised {concealment_for ?attack_squad} = smoke
                  at 3 from end_of 1,
    supervised {cover_fire_for  ?attack_squad} = ?cover_squad
                  at 3 from begin_of 2;
end_schema;


;;; Primitive Modelling Level

;;; In most cases, the types should be more general.

schema lauch_AFSS_at_location;
  vars ?target = ?{type Building},
       ?unit   = ?{type Platoon};
  expands {lauch_AFSS_at ?target ?unit};
end_schema;

schema suppress_location;
  vars ?target = ?{type Building},
       ?unit   = ?{type Platoon};
  expands {lay_suppressing_fire ?target ?unit};
end_schema;

schema lay_down_smoke;
  vars ?target      = ?{type Building},
       ?cover_unit  = ?{type Squad},
       ?attack_unit = ?{type Squad};
  expands {lay_down_smoke ?target ?cover_unit ?attack_unit};
  effects
     {concealment_for ?attack_unit} = smoke;
end_schema;

schema storm_objective;
  vars ?objective = ?{type Building},
       ?unit      = ?{type Squad};
  expands {storm_objective ?objective ?unit};
end_schema;

schema cover_movement;
  vars ?cover_unit  = ?{type Squad},
       ?attack_unit = ?{type Squad};
  expands {cover_movement ?cover_unit ?attack_unit};
  effects
     {cover_fire_for ?attack_unit} = ?cover_unit at begin_of self;
end_schema;

schema request_TAC_Air;
  vars ?target = ?{type Building},
       ?unit   = ?{type Platoon};
  expands {request_TAC_air_on ?target ?unit};
end_schema;

schema throw_flair;
  vars ?target = ?{type Building},
       ?unit   = ?{type Platoon};
  expands {throw_flair ?target ?unit};
end_schema;

schema lay_accurate_smoke;
  vars ?target = ?{type Building},
       ?unit   = ?{type Platoon};
  expands {lay_accurate_smoke ?target ?unit};
end_schema;

;;; End
