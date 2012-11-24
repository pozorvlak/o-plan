;;; File: mout1.tf
;;; Contains: TF for the MOUT SUO-PDA demo
;;; Created: John Levine, August 1999
;;; Updated: Wed Sep  8 15:29:29 1999 by John Levine
;;; Copyright: (c) 1999, AIAI, University of Edinburgh

types 
  platoon  = (platoon_1 platoon_2 platoon_4),
  building = (building_11 building_12 building_21 building_22 building_31
              building_32 building_33 building_34 building_35 building_41
              building_42 building_43 building_44 building_45 building_46),
  orp      = (orp_11 orp_21 orp_22 orp_31 orp_34 orp_35 orp_42 orp_46);

always
  {orp_to_use building_11} = orp_11,
  {orp_to_use building_21} = orp_21,
  {orp_to_use building_22} = orp_22,
  {orp_to_use building_31} = orp_31,
  {orp_to_use building_34} = orp_34,
  {orp_to_use building_35} = orp_35,
  {orp_to_use building_42} = orp_42,
  {orp_to_use building_46} = orp_46,
  {possible_toehold building_32 building_21} = true,
  {possible_toehold building_32 building_22} = true,
  {possible_toehold building_32 building_31} = true,
  {possible_toehold building_33 building_31} = true,
  {possible_toehold building_33 building_34} = true,
  {possible_toehold building_33 building_35} = true,
  {possible_toehold building_33 building_46} = true,
  {possible_toehold building_41 building_11} = true,
  {possible_toehold building_41 building_22} = true,
  {possible_toehold building_41 building_42} = true,
  {possible_toehold building_41 building_46} = true,
  {friendly_unit platoon_1 platoon_2} = true,
  {friendly_unit platoon_1 platoon_4} = true,
  {friendly_unit platoon_2 platoon_1} = true,
  {friendly_unit platoon_2 platoon_4} = true,
  {friendly_unit platoon_4 platoon_1} = true,
  {friendly_unit platoon_4 platoon_2} = true,
  {can_fire_from building_42 building_11} = true,
  {can_fire_from building_42 building_12} = true,
  {can_fire_from building_31 building_21} = true,
  {can_fire_from building_32 building_22} = true,
  {can_fire_from building_34 building_31} = true,
  {can_fire_from building_31 building_32} = true,
  {can_fire_from building_31 building_33} = true,
  {can_fire_from building_31 building_34} = true,
  {can_fire_from building_46 building_35} = true,
  {can_fire_from building_42 building_41} = true,
  {can_fire_from building_11 building_42} = true,
  {can_fire_from building_42 building_43} = true,
  {can_fire_from building_42 building_44} = true,
  {can_fire_from building_46 building_45} = true,
  {can_fire_from building_34 building_46} = true;

task demo_coa_1;
  nodes
    1 start,
    2 finish,
    3 action {rescue_hostages platoon_1 building_33 building_34}, 
    4 action {rescue_hostages platoon_2 building_32 building_21},
    5 action {rescue_hostages platoon_4 building_41 building_46};
  orderings
    1 ---> 3, 1 ---> 4, 1 ---> 5, 
    3 ---> 2, 4 ---> 2, 5 ---> 2;
  effects
    {obstacles orp_11 building_11}    = {} at 1,
    {obstacles orp_21 building_21}    = {} at 1,
    {obstacles orp_22 building_22}    = {} at 1,
    {obstacles orp_31 building_31}    = {} at 1,
    {obstacles orp_34 building_34}    = {} at 1,
    {obstacles orp_35 building_35}    = {} at 1,
    {obstacles orp_42 building_42}    = {} at 1,
    {obstacles orp_46 building_46}    = {} at 1,
    {snipers building_11 building_41} = {} at 1,
    {snipers building_21 building_32} = {} at 1,
    {snipers building_22 building_32} = {} at 1,
    {snipers building_22 building_41} = {} at 1,
    {snipers building_31 building_32} = {} at 1,
    {snipers building_31 building_33} = {} at 1,
    {snipers building_34 building_33} = {} at 1,
    {snipers building_35 building_33} = {} at 1,
    {snipers building_42 building_41} = {} at 1,
    {snipers building_46 building_33} = {} at 1,
    {snipers building_46 building_41} = {} at 1,
    {historic_interest building_33}   = valuable at 1,
    {friendly_occupancy building_11}  = not_planned at 1,
    {friendly_occupancy building_12}  = not_planned at 1,
    {friendly_occupancy building_21}  = planned at 1,
    {friendly_occupancy building_22}  = not_planned at 1,
    {friendly_occupancy building_31}  = not_planned at 1,
    {friendly_occupancy building_32}  = planned at 1,
    {friendly_occupancy building_33}  = planned at 1,
    {friendly_occupancy building_34}  = planned at 1,
    {friendly_occupancy building_35}  = not_planned at 1,
    {friendly_occupancy building_41}  = planned at 1,
    {friendly_occupancy building_42}  = not_planned at 1,
    {friendly_occupancy building_43}  = not_planned at 1,
    {friendly_occupancy building_44}  = not_planned at 1,
    {friendly_occupancy building_45}  = not_planned at 1,
    {friendly_occupancy building_46}  = planned at 1,
    {location_of platoon_1}           = landing_zone at 1,
    {location_of platoon_2}           = landing_zone at 1,
    {location_of platoon_4}           = landing_zone at 1,
    {status_afss platoon_1}           = available at 1,
    {status_tacair platoon_1}         = available at 1;
end_task;

task demo_coa_2;
  nodes
    1 start,
    2 finish,
    3 action {rescue_hostages platoon_1 building_33 building_34}, 
    4 action {rescue_hostages platoon_2 building_32 building_31},
    5 action {rescue_hostages platoon_4 building_41 building_46};
  orderings
    1 ---> 3, 1 ---> 4, 1 ---> 5, 
    3 ---> 2, 4 ---> 2, 5 ---> 2;
  effects
    {obstacles orp_11 building_11}    = {} at 1,
    {obstacles orp_21 building_21}    = {} at 1,
    {obstacles orp_22 building_22}    = {} at 1,
    {obstacles orp_31 building_31}    = {} at 1,
    {obstacles orp_34 building_34}    = {} at 1,
    {obstacles orp_35 building_35}    = {} at 1,
    {obstacles orp_42 building_42}    = {} at 1,
    {obstacles orp_46 building_46}    = {} at 1,
    {snipers building_11 building_41} = {} at 1,
    {snipers building_21 building_32} = {} at 1,
    {snipers building_22 building_32} = {} at 1,
    {snipers building_22 building_41} = {} at 1,
    {snipers building_31 building_32} = {} at 1,
    {snipers building_31 building_33} = {} at 1,
    {snipers building_34 building_33} = {} at 1,
    {snipers building_35 building_33} = {} at 1,
    {snipers building_42 building_41} = {} at 1,
    {snipers building_46 building_33} = {} at 1,
    {snipers building_46 building_41} = {} at 1,
    {historic_interest building_33}   = valuable at 1,
    {friendly_occupancy building_11}  = not_planned at 1,
    {friendly_occupancy building_12}  = not_planned at 1,
    {friendly_occupancy building_21}  = not_planned at 1,
    {friendly_occupancy building_22}  = not_planned at 1,
    {friendly_occupancy building_31}  = planned at 1,
    {friendly_occupancy building_32}  = planned at 1,
    {friendly_occupancy building_33}  = planned at 1,
    {friendly_occupancy building_34}  = planned at 1,
    {friendly_occupancy building_35}  = not_planned at 1,
    {friendly_occupancy building_41}  = planned at 1,
    {friendly_occupancy building_42}  = not_planned at 1,
    {friendly_occupancy building_43}  = not_planned at 1,
    {friendly_occupancy building_44}  = not_planned at 1,
    {friendly_occupancy building_45}  = not_planned at 1,
    {friendly_occupancy building_46}  = planned at 1,
    {location_of platoon_1}           = landing_zone at 1,
    {location_of platoon_2}           = landing_zone at 1,
    {location_of platoon_4}           = landing_zone at 1,
    {status_afss platoon_1}           = available at 1,
    {status_tacair platoon_1}         = available at 1;
end_task;

task demo_coa_3;
  nodes
    1 start,
    2 finish,
    3 action {rescue_hostages platoon_1 building_33 building_34}, 
    4 action {rescue_hostages platoon_2 building_32 building_31},
    5 action {rescue_hostages platoon_4 building_41 building_42};
  orderings
    1 ---> 3, 1 ---> 4, 1 ---> 5, 
    3 ---> 2, 4 ---> 2, 5 ---> 2;
  effects
    {obstacles orp_11 building_11}    = {} at 1,
    {obstacles orp_21 building_21}    = {} at 1,
    {obstacles orp_22 building_22}    = {} at 1,
    {obstacles orp_31 building_31}    = {} at 1,
    {obstacles orp_34 building_34}    = {} at 1,
    {obstacles orp_35 building_35}    = {} at 1,
    {obstacles orp_42 building_42}    = {} at 1,
    {obstacles orp_46 building_46}    = {} at 1,
    {snipers building_11 building_41} = {} at 1,
    {snipers building_21 building_32} = {} at 1,
    {snipers building_22 building_32} = {} at 1,
    {snipers building_22 building_41} = {} at 1,
    {snipers building_31 building_32} = {} at 1,
    {snipers building_31 building_33} = {} at 1,
    {snipers building_34 building_33} = {} at 1,
    {snipers building_35 building_33} = {} at 1,
    {snipers building_42 building_41} = {} at 1,
    {snipers building_46 building_33} = {} at 1,
    {snipers building_46 building_41} = {} at 1,
    {historic_interest building_33}   = valuable at 1,
    {friendly_occupancy building_11}  = not_planned at 1,
    {friendly_occupancy building_12}  = not_planned at 1,
    {friendly_occupancy building_21}  = not_planned at 1,
    {friendly_occupancy building_22}  = not_planned at 1,
    {friendly_occupancy building_31}  = planned at 1,
    {friendly_occupancy building_32}  = planned at 1,
    {friendly_occupancy building_33}  = planned at 1,
    {friendly_occupancy building_34}  = planned at 1,
    {friendly_occupancy building_35}  = not_planned at 1,
    {friendly_occupancy building_41}  = planned at 1,
    {friendly_occupancy building_42}  = planned at 1,
    {friendly_occupancy building_43}  = not_planned at 1,
    {friendly_occupancy building_44}  = not_planned at 1,
    {friendly_occupancy building_45}  = not_planned at 1,
    {friendly_occupancy building_46}  = not_planned at 1,
    {location_of platoon_1}           = landing_zone at 1,
    {location_of platoon_2}           = landing_zone at 1,
    {location_of platoon_4}           = landing_zone at 1,
    {status_afss platoon_1}           = available at 1,
    {status_tacair platoon_1}         = available at 1;
end_task;

task demo_coa_4;
  nodes
    1 start,
    2 finish,
    3 action {rescue_hostages platoon_1 building_33 building_34}, 
    4 action {rescue_hostages platoon_2 building_32 building_31},
    5 action {rescue_hostages platoon_4 building_41 building_42};
  orderings
    1 ---> 3, 1 ---> 4, 1 ---> 5, 
    3 ---> 2, 4 ---> 2, 5 ---> 2;
  effects
    {obstacles orp_11 building_11}    = {} at 1,
    {obstacles orp_21 building_21}    = {} at 1,
    {obstacles orp_22 building_22}    = {} at 1,
    {obstacles orp_31 building_31}    = {} at 1,
    {obstacles orp_34 building_34}    = {} at 1,
    {obstacles orp_35 building_35}    = {} at 1,
    {obstacles orp_42 building_42}    = {minefield concertina_wire} at 1,
    {obstacles orp_46 building_46}    = {} at 1,
    {snipers building_11 building_41} = {} at 1,
    {snipers building_21 building_32} = {} at 1,
    {snipers building_22 building_32} = {} at 1,
    {snipers building_22 building_41} = {} at 1,
    {snipers building_31 building_32} = {} at 1,
    {snipers building_31 building_33} = {} at 1,
    {snipers building_34 building_33} = {building_46 building_32} at 1,
    {snipers building_35 building_33} = {} at 1,
    {snipers building_42 building_41} = {} at 1,
    {snipers building_46 building_33} = {} at 1,
    {snipers building_46 building_41} = {} at 1,
    {historic_interest building_33}   = valuable at 1,
    {friendly_occupancy building_11}  = not_planned at 1,
    {friendly_occupancy building_12}  = not_planned at 1,
    {friendly_occupancy building_21}  = not_planned at 1,
    {friendly_occupancy building_22}  = not_planned at 1,
    {friendly_occupancy building_31}  = planned at 1,
    {friendly_occupancy building_32}  = planned at 1,
    {friendly_occupancy building_33}  = planned at 1,
    {friendly_occupancy building_34}  = planned at 1,
    {friendly_occupancy building_35}  = not_planned at 1,
    {friendly_occupancy building_41}  = planned at 1,
    {friendly_occupancy building_42}  = planned at 1,
    {friendly_occupancy building_43}  = not_planned at 1,
    {friendly_occupancy building_44}  = not_planned at 1,
    {friendly_occupancy building_45}  = not_planned at 1,
    {friendly_occupancy building_46}  = not_planned at 1,
    {location_of platoon_1}           = landing_zone at 1,
    {location_of platoon_2}           = landing_zone at 1,
    {location_of platoon_4}           = landing_zone at 1,
    {status_afss platoon_1}           = available at 1,
    {status_tacair platoon_1}         = available at 1;
end_task;

;;; Level 1 schemas.

schema rescue_hostages;
  vars ?platoon = ?{type platoon},
       ?target  = ?{type building},
       ?toehold = ?{type building},
       ?orp     = ?{type orp};
  expands {rescue_hostages ?platoon ?target ?toehold};
  nodes
    1 action {move_from_lz_to_orp ?platoon ?orp},
    2 action {move_to_toehold ?platoon ?orp ?toehold},
    3 action {take_toehold ?platoon ?toehold},
    4 action {move_to_target ?platoon ?toehold ?target},
    5 action {take_target ?platoon ?target},
    6 action {evacuate_hostages ?platoon ?target};
  orderings
    1 ---> 2, 2 ---> 3, 3 ---> 4, 4 ---> 5, 5 ---> 6;
  conditions
    only_use_if {orp_to_use ?toehold} = ?orp,
    only_use_if {possible_toehold ?target ?toehold} = true;
end_schema;

;;; Level 2 schemas.

schema move_from_lz_to_orp;
  vars ?platoon = ??,
       ?orp     = ??;
  expands {move_from_lz_to_orp ?platoon ?orp};
  effects
    {at_orp ?platoon ?orp} = true,
    {location_of ?platoon} = ?orp;
  time_windows duration self = 80 minutes;
end_schema;

schema move_to_toehold;
  vars ?platoon = ??,
       ?orp     = ??,
       ?toehold = ??;
  expands {move_to_toehold ?platoon ?orp ?toehold};
  conditions
    achieve {obstacles ?orp ?toehold} = {};
  effects
    {location_of ?platoon} = ?toehold;
  time_windows duration self = 15 minutes;
end_schema;

schema remove_obstacles;
  vars ?obstacles = ?{satisfies nonempty},
       ?orp       = ??,
       ?toehold   = ??,
       ?platoon   = ??;
  expands {remove_obstacles ?platoon ?obstacles};
  only_use_for_effects
    {obstacles ?orp ?toehold} = {};
  nodes
    1 iterate
        action {remove_obstacle ?platoon ?obstacle}
        for ?obstacle over ?obstacles;
  conditions
    only_use_if {at_orp ?platoon ?orp} = true,
    only_use_if {obstacles ?orp ?toehold} = ?obstacles;
end_schema;

schema remove_obstacle_by_going_around;
  vars ?platoon  = ??,
       ?obstacle = ??;
  expands {remove_obstacle ?obstacle ?platoon};
  nodes
    1 action {go_around_obstacle ?platoon ?obstacle};
  time_windows duration self = 5 minutes;
end_schema;

schema remove_obstacle_by_cutting_through;
  vars ?platoon  = ??;
  expands {remove_obstacle ?platoon concertina_wire};
  nodes
    1 action {cut_through concertina_wire ?platoon};
  time_windows duration self = 10 minutes;
end_schema;

schema remove_obstacle_using_satchel_charges;
  vars ?platoon  = ??;
  expands {remove_obstacle ?platoon concertina_wire};
  nodes
    1 action {lay_satchel_charges concertina_wire ?platoon};
  time_windows duration self = 	5 minutes;
end_schema;

schema remove_obstacle_using_bangalore_torpedoes;
  vars ?platoon  = ??;
  expands {remove_obstacle ?platoon concertina_wire};
  nodes
    1 action {fire_bangalore_torpedoes concertina_wire ?platoon};
  time_windows duration self = 45 minutes;
end_schema;

schema remove_obstacle_using_mine_detectors;
  vars ?platoon  = ??;
  expands {remove_obstacle ?platoon minefield};
  nodes
    1 action {go_through_minefield_using_mine_detectors minefield ?platoon};
  time_windows duration self = 55 minutes;
end_schema;

schema remove_obstacle_using_a_flail;
  vars ?platoon  = ??;
  expands {remove_obstacle ?platoon minefield};
  nodes
    1 action {clear_minefield_using_a_flail minefield ?platoon};
  time_windows duration self = 180 minutes;
end_schema;

schema take_toehold;
  vars ?platoon = ?{type platoon},
       ?building = ?{type building},
       ?duration;
  expands {take_toehold ?platoon ?building};
  conditions
    compute {toehold_time ?building} = ?duration;
  effects
    {in_toehold ?platoon ?building} = true;
  time_windows duration self = ?duration;
end_schema;

schema move_to_target;
  vars ?platoon  = ??,
       ?toehold  = ?{type building},
       ?toehold1 = ?{type building},
       ?toehold2 = ?{type building},
       ?toehold4 = ?{type building},
       ?target   = ??;
  expands {move_to_target ?platoon ?toehold ?target};
  conditions
    unsupervised {in_toehold platoon_1 ?toehold1} = true,
    unsupervised {in_toehold platoon_2 ?toehold2} = true,
    unsupervised {in_toehold platoon_4 ?toehold4} = true,
    achieve {snipers ?toehold ?target} = {};
  effects
    {location_of ?platoon} = ?target;
  time_windows duration self = 15 minutes;
end_schema;

schema remove_snipers;
  vars ?snipers = ?{satisfies nonempty},
       ?toehold = ??,
       ?target  = ??,
       ?platoon = ??;
  expands {remove_snipers ?platoon ?snipers};
  only_use_for_effects
    {snipers ?toehold ?target} = {};
  nodes
    1 foreach
        action {remove_sniper ?platoon ?sniper}
        for ?sniper over ?snipers;
  conditions
    only_use_if {in_toehold ?platoon ?toehold} = true,
    only_use_if {snipers ?toehold ?target} = ?snipers;
end_schema;

schema remove_sniper_with_afss;
  vars ?sniper  = ??,
       ?platoon = ??;
  expands {remove_sniper ?platoon ?sniper};
  nodes
    1 action {launch_afss_at ?sniper ?platoon};
  conditions
    only_use_if {friendly_occupancy ?sniper} = not_planned,
    only_use_if {status_afss ?platoon} = available;
  time_windows duration self = 5 minutes;
end_schema;

schema remove_sniper_with_tacair;
  vars ?sniper  = ??,
       ?platoon = ??;
  expands {remove_sniper ?platoon ?sniper};
  nodes
    1 action {request_tacair_on ?sniper ?platoon},
    2 action {throw_flair ?sniper ?platoon};
  orderings
    1 ---> 2;
  conditions
    only_use_if {friendly_occupancy ?sniper} = not_planned,
    only_use_if {status_tacair ?platoon} = available;
  time_windows duration self = 15 minutes;
end_schema;

schema remove_sniper_with_other_friendly_unit;
  vars ?sniper                 = ??,
       ?platoon                = ??,
       ?friendly_unit          = ??,
       ?friendly_unit_location = ??;
  expands {remove_sniper ?platoon ?sniper};
  nodes
    1 action {lay_suppressing_fire ?sniper ?friendly_unit};
  conditions
    only_use_if {can_fire_from ?friendly_unit_location ?sniper} = true,
    only_use_if {in_toehold ?friendly_unit ?friendly_unit_location} = true, 
    only_use_if {friendly_unit ?platoon ?friendly_unit} = true;
  time_windows duration self = 5 minutes;
end_schema;

schema remove_sniper_with_well_aimed_smoke;
  vars ?sniper  = ??,
       ?platoon = ??;
  expands {remove_sniper ?platoon ?sniper};
  nodes
    1 action {lay_accurate_smoke ?sniper ?platoon};
  time_windows duration self = 5 minutes;
end_schema;

schema take_target;
  vars ?platoon = ??,
       ?target  = ??;
  expands {take_target ?platoon ?target};
  time_windows duration self = 15 minutes;
end_schema;

schema evacuate_hostages;
  vars ?platoon = ??,
       ?target  = ??;
  expands {evacuate_hostages ?platoon ?target};
  effects
    {location_of ?platoon} = evacuation_point;
  time_windows duration self = 60 minutes;
end_schema;

;;; Level 3.

schema launch_afss_at;
  vars ?target  = ??,
       ?platoon = ??;
  expands {launch_afss_at ?target ?platoon};
end_schema;

schema request_tacair_on;
  vars ?target  = ??,
       ?platoon = ??;
  expands {request_tacair_on ?target ?platoon};
end_schema;

schema throw_flair;
  vars ?target  = ??,
       ?platoon = ??;
  expands {throw_flair ?target ?platoon};
end_schema;

schema lay_suppressing_fire;
  vars ?target  = ??,
       ?platoon = ??;
  expands {lay_suppressing_fire ?target ?platoon};
end_schema;

schema lay_accurate_smoke;
  vars ?target  = ??,
       ?platoon = ??;
  expands {lay_accurate_smoke ?target ?platoon};
end_schema;

schema go_around_obstacle;
  vars ?obstacle = ??,
       ?platoon  = ??;
  expands {go_around_obstacle ?obstacle ?platoon};
end_schema;

schema cut_through;
  vars ?obstacle = ??,
       ?platoon  = ??;
  expands {cut_through ?obstacle ?platoon};
end_schema;

schema lay_satchel_charges;
  vars ?obstacle = ??,
       ?platoon  = ??;
  expands {lay_satchel_charges ?obstacle ?platoon};
end_schema;

schema fire_bangalore_torpedoes;
  vars ?obstacle = ??,
       ?platoon  = ??;
  expands {fire_bangalore_torpedoes ?obstacle ?platoon};
end_schema;

schema go_through_minefield_using_mine_detectors;
  vars ?obstacle = ??,
       ?platoon  = ??;
  expands {go_through_minefield_using_mine_detectors ?obstacle ?platoon};
end_schema;

schema clear_minefield_using_a_flail;
  vars ?obstacle = ??,
       ?platoon  = ??;
  expands {clear_minefield_using_a_flail ?obstacle ?platoon};
end_schema;

language lisp;

  (defparameter *toehold-times*
    '((building_11 . 15)
      (building_21 . 65)
      (building_22 . 30)
      (building_31 . 30)
      (building_34 . 10)
      (building_35 . 10)
      (building_42 . 35)
      (building_46 . 25)))

  (defun toehold_time (building)
    (* 60 (or (lookup building *toehold-times*)
              (error "Unknown building ~S." building))))

  (defun nonempty (x) (consp x))

end_language;

;;; End of file.

