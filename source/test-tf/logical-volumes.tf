;;; Sample TF for logical volume commands

types volume_group = (VG1 VG2),
      logical_volume = (LV1 LV2 LV3),
      physical_volume = (PV1 PV2 PV3);

initially
  {logical_contents VG1} = {LV1 LV2},
  {physical_contents VG1} = {PV1 PV2};


task example_1;
  nodes
    sequential
      1 start,
      3 action {vgreduce VG1 PV2},
      2 finish
    end_sequential;
end_task;

task example_2;
  nodes
    sequential
      1 start,
      3 action {vgremove VG1},
      2 finish
    end_sequential;
end_task;


;;; Remove volume group

schema vgremove;
  vars ?vg = ?{type volume_group},
       ?pv = ?{type physical_volume};
  expands {vgremove ?vg};
  nodes
    sequential
      1 action {type {usr_sbin_vgremove ?vg taking ?pv}},
      2 action {type {rm_r ?vg}}
    end_sequential;
  conditions
    achieve {physical_contents ?vg} = {?pv},
    achieve {logical_contents ?vg} = {};
  effects
    {physical_contents ?vg} = {};         ;;; remove final physical volume
  ;;; Omitting effect:
  ;;;  Volume Group doesn't exist anymore
end_schema;


;;; Remove Physical Volume from Volume Group

schema vgreduce;
  vars ?vg = ?{type volume_group},
       ?pv = ?{type physical_volume};
  expands {vgreduce ?vg ?pv};
  nodes
    1 action {remove_physical_volume ?vg ?pv};
  ;;; Omitting effect:
  ;;;  Physical Volume doesn't exist anymore
end_schema;

;;; /\/: The vgreduce_body schema exists only because the only_use_if
;;;      for physical_contents has to be pushed down a level.

schema vgreduce_body;
  vars ?vg = ?{type volume_group},
       ?pv = ?{type physical_volume},
       ?pvs = ?{satisfies consp},
       ?new_pvs = ?{satisfies consp};
  expands {remove_physical_volume ?vg ?pv};
  nodes
    1 action {type {usr_sbin_vgreduce ?vg ?pv}};
  conditions
    only_use_if {physical_contents ?vg} = ?pvs,
    compute {remove ?pv ?pvs} = ?new_pvs,
    achieve {logical_contents ?vg} = {};
  effects
    {physical_contents ?vg} = ?new_pvs;
end_schema;


;;; Remove Logical Volume

schema lvremove;
  vars ?lv = ?{type logical_volume};
  expands {lvremove ?lv};
  nodes
    sequential
      1 action {type {usr_umount FS_name}},
      2 action {type {usr_sbin_lvremove_f ?lv}}
    end_sequential;
  ;;; Omitting all else.
end_schema;


;;; Remove Physical Volume from Volume Group (when pre-conditions above are
;;; not met)

;;; This action requires the generation of the following plan:
;;;   Delete All Logical Volumes (because pre-conditions above are not met)
;;;   Remove Physical Volume

;;; /\/: We just remove all logical volumes via achieve

schema remove_all_logical_volumes;
  vars ?vg = ?{type volume_group},
       ?lvs = ?{satisfies consp};
  expands {clear_logical_volumes ?vg};
  only_use_for_effects
    {logical_contents ?vg} = {};
  nodes
    1 iterate action {lvremove ?lv}
        for ?lv over ?lvs;
  conditions
    only_use_if {logical_contents ?vg} = ?lvs;
end_schema;


;;; Remove Volume Group (when pre-conditions above are not met)

;;; This action requires the generation of the following plan:
;;;   Delete All Logical Volumes  (because pre-condition above are not me)
;;;   Remove All Physical Volumes except the last one (because
;;;      pre-condition above are not me)
;;;   Remove the Volume Group along with the last physical volume

schema reduce_physical_volumes;
  vars ?vg = ?{type volume_group},
       ?pv = ?{type physical_volume},
       ?pvs = ?{satisfies consp},
       ?pv_rest = ?{satisfies consp};
  expands {reduce_physical_volumes ?vg};
  only_use_for_effects {physical_contents ?vg} = {?pv};
  nodes
    1 iterate action {type {usr_sbin_vgreduce ?vg ?pv}}
        for ?pv over ?pv_rest;
  conditions
    only_use_if {physical_contents ?vg} = ?pvs,
    compute {last ?pvs} = {?pv},
    compute {butlast ?pvs} = ?pv_rest;
end_schema;


;;; Primitive for saying what commands to type

schema type;
  vars ?command;
  expands {type ?command};
end_schema;
