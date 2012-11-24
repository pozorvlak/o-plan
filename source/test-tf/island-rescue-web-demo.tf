;;; This is the default task produced by the demo form.

;;; Rescue task

types ground_transport = (GT1 GT2);

always {gt_capacity 20};

initially
     {evac_status Abyss   } = {100   0},
     {evac_status Barnacle} = { 50   0},
     {evac_status Calypso } = { 20   0},
     {in_use_for GT1} = available,
     {in_use_for GT2} = available;

task Pacifica_evacuation;
  nodes sequential
          1 start,
          2 finish
        end_sequential;
  conditions
     achieve {evac_status Abyss   } = {  0 100} at 2,
     achieve {evac_status Barnacle} = {  0  50} at 2,
     achieve {evac_status Calypso } = {  0  20} at 2,
     achieve {safe_at Delta} at 2;
  time_windows 0..3~00:00 at 2;
end_task;

include "../web/demo/island-rescue";
