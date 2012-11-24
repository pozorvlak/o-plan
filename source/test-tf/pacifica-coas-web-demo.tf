;;; Here are some sample tasks of the sort produced by the demo.

types ground_transport = (GT1 GT2),
      helicopter = (H1 H2);

initially
  {location GT1} = Delta,
  {location GT2} = Delta,
  {location H1}  = Delta,
  {location H2}  = Delta;

task evac_injured_from_Calypso_by_helicopter;
  nodes
    sequential
      1 start,
      3 action {evacuate injured from Calypso using a helicopter},
      2 finish
    end_sequential;
end_task;

task med_team_to_Abyss_by_helicopter;
  nodes
    sequential
      1 start,
      3 action {take a medical team to Abyss using a helicopter},
      2 finish
    end_sequential;
end_task;

task evac_injured_from_Calypso_by_gt;
  nodes
    sequential
      1 start,
      3 action {evacuate injured from Calypso using a GT},
      2 finish
    end_sequential;
end_task;

task med_team_to_Abyss_by_gt;
  nodes
    sequential
      1 start,
      3 action {take a medical team to Abyss using a GT},
      2 finish
    end_sequential;
end_task;

include "../web/demo/pacifica-coas";
