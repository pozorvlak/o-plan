;;; File: gpdt3-web-demo.tf
;;; Contains: TF for the Pacifica COA matrix demo
;;; Created: John Levine, June 1997
;;; Updated: Thu Apr 22 23:54:06 1999 by Jeff Dalton
;;; Copyright: (c) 1997, AIAI, University of Edinburgh

;;; These are the tasks John used for testing, plus some other ones.
;;; The "{weather} = storm" effect has been commented out of the
;;; "initially" to make it clear how it varies in different tasks.  [JD]

initially
  ;;; {weather} = storm,
  {road_status Abyss_Exodus} = open,
  {road_status Delta_Abyss} = open,
  {road_status Abyss_Barnacle} = open,
  {road_status Barnacle_Calypso} = open,
  {road_status Calypso_Delta} = open,
  {location ??} = Delta,
  {available ??} = true,
  {empty_vehicle ??} = true;

task test_move_injured_by_road;   ;;; "by road" because {weather} = storm
  nodes
    1 start,
    2 finish,
    3 action {evacuate_injured Abyss};
  orderings
    1 ---> 3, 3 ---> 2;
  effects
    {weather} = storm at 1;
end_task;

task this_is_coa_2;
  nodes
    1 start,
    2 finish,
    3 action {evacuate_injured Abyss},
    4 action {evacuate_injured Barnacle},
    5 action {evacuate_injured Calypso};
  orderings
    1 ---> 3, 1 ---> 4, 1 ---> 5, 
    3 ---> 2, 4 ---> 2, 5 ---> 2;
  effects
    {weather} = storm at 1;
end_task;

task this_is_coa_2_2;
  nodes
    1 start,
    2 finish,
    3 action {evacuate_injured Abyss},
    4 action {evacuate_injured Barnacle},
    5 action {evacuate_injured Calypso},
    6 action {repair_gas_leak Barnacle},
    7 action {defuse_terrorist_bomb Barnacle};
  orderings
    1 ---> 3, 1 ---> 4, 1 ---> 5, 1 ---> 6, 1 ---> 7, 
    3 ---> 2, 4 ---> 2, 5 ---> 2, 6 ---> 2, 7 ---> 2;
  effects
    {weather} = storm at 1;
end_task;

task this_is_coa_3;
  nodes
    1 start,
    2 finish,
    3 action {send_medical_team Abyss},
    4 action {send_medical_team Barnacle},
    5 action {send_medical_team Calypso},
    6 action {repair_gas_leak Barnacle},
    7 action {defuse_terrorist_bomb Barnacle};
  orderings
    1 ---> 3, 1 ---> 4, 1 ---> 5, 1 ---> 6, 1 ---> 7, 
    3 ---> 2, 4 ---> 2, 5 ---> 2, 6 ---> 2, 7 ---> 2;
  effects
    {weather} = storm at 1;
end_task;

task this_is_coa_4;
  nodes
    1 start,
    2 finish,
    3 action {send_medical_team Abyss},
    4 action {evacuate_injured Barnacle},
    5 action {send_medical_team Calypso},
    6 action {repair_gas_leak Barnacle},
    7 action {defuse_terrorist_bomb Barnacle};
  orderings
    1 ---> 3, 1 ---> 4, 1 ---> 5, 1 ---> 6, 1 ---> 7, 
    3 ---> 2, 4 ---> 2, 5 ---> 2, 6 ---> 2, 7 ---> 2;
  effects
    {weather} = storm at 1;
end_task;

task this_is_coa_5;
  nodes
    1 start,
    2 finish,
    3 action {send_medical_team Abyss},
    4 action {evacuate_injured Barnacle},
    5 action {send_medical_team Calypso},
    6 action {repair_gas_leak Barnacle},
    7 action {defuse_terrorist_bomb Barnacle},
    8 action {evacuate_population Barnacle};
  orderings
    1 ---> 3, 1 ---> 4, 1 ---> 5, 1 ---> 6, 1 ---> 7, 1 ---> 8,
    3 ---> 2, 4 ---> 2, 5 ---> 2, 6 ---> 2, 7 ---> 2, 8 ---> 2;
  effects
    {weather} = storm at 1;
end_task;

task this_is_coa_5_clear;
  nodes
    1 start,
    2 finish,
    3 action {send_medical_team Abyss},
    4 action {evacuate_injured Barnacle},
    5 action {send_medical_team Calypso},
    6 action {repair_gas_leak Barnacle},
    7 action {defuse_terrorist_bomb Barnacle},
    8 action {evacuate_population Barnacle};
  orderings
    1 ---> 3, 1 ---> 4, 1 ---> 5, 1 ---> 6, 1 ---> 7, 1 ---> 8,
    3 ---> 2, 4 ---> 2, 5 ---> 2, 6 ---> 2, 7 ---> 2, 8 ---> 2;
  effects
    {weather} = clear at 1;
end_task;

task do_all_task_types_clear;
  nodes
  sequential
    1 start,
    parallel
       3 action {evacuate_injured Abyss},
       4 action {evacuate_population Abyss},
       5 action {evacuate_with_medical_team Abyss},
       6 action {send_medical_supplies Abyss},
       7 action {send_emergency_food Abyss},
       8 action {send_medical_team Abyss},
       9 action {repair_gas_leak Abyss},
      10 action {defuse_terrorist_bomb Abyss},
      11 action {build_emergency_housing Abyss},
      12 action {repair_power_station_turbine Abyss},
      13 action {provide_immediate_assistance Abyss},
      14 action {shut_down_power_station Abyss}
    end_parallel,
    2 finish
  end_sequential;
  effects
    {weather} = clear at 1;
end_task;

task do_all_task_types_rain;
  nodes
  sequential
    1 start,
    parallel
       3 action {evacuate_injured Abyss},
       4 action {evacuate_population Abyss},
       5 action {evacuate_with_medical_team Abyss},
       6 action {send_medical_supplies Abyss},
       7 action {send_emergency_food Abyss},
       8 action {send_medical_team Abyss},
       9 action {repair_gas_leak Abyss},
      10 action {defuse_terrorist_bomb Abyss},
      11 action {build_emergency_housing Abyss},
      12 action {repair_power_station_turbine Abyss},
      13 action {provide_immediate_assistance Abyss},
      14 action {shut_down_power_station Abyss}
    end_parallel,
    2 finish
  end_sequential;
  effects
    {weather} = rain at 1;
end_task;

task do_all_task_types_storm;
  nodes
  sequential
    1 start,
    parallel
       3 action {evacuate_injured Abyss},
       4 action {evacuate_population Abyss},
       5 action {evacuate_with_medical_team Abyss},
       6 action {send_medical_supplies Abyss},
       7 action {send_emergency_food Abyss},
       8 action {send_medical_team Abyss},
       9 action {repair_gas_leak Abyss},
      10 action {defuse_terrorist_bomb Abyss},
      11 action {build_emergency_housing Abyss},
      12 action {repair_power_station_turbine Abyss},
      13 action {provide_immediate_assistance Abyss},
      14 action {shut_down_power_station Abyss}
    end_parallel,
    2 finish
  end_sequential;
  effects
    {weather} = storm at 1;
end_task;

include "../web/demo/gpdt3";
