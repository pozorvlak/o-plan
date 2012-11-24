
;;; COA task

initially
     {weather} = storm,
     {road_status Delta_Abyss} = open,
     {road_status Abyss_Barnacle} = open,
     {road_status Barnacle_Calypso} = open,
     {road_status Calypso_Delta} = open,
     {location ??} = Delta,
     {available ??} = true,
     {empty_vehicle ??} = true;

task Pacifica_COA_1;
  nodes
    sequential
      1 start,
      parallel
        3 action {evacuate_with_medical_team abyss},
        4 action {evacuate_with_medical_team barnacle},
        5 action {evacuate_with_medical_team calypso},
        6 action {evacuate_with_medical_team barnacle},
        7 action {evacuate_with_medical_team calypso},
        8 action {send_emergency_food calypso},
        9 action {repair_gas_leak barnacle},
       10 action {defuse_terrorist_bomb abyss}
      end_parallel,
      2 finish
    end_sequential;
end_task;

include "gpdt";
