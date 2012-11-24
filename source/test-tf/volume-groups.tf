;;; Sample TF for logical volume commands

;;; Author: Jeff Dalton

;;; Created: May 1997
;;; Updated: Wed May 21 14:29:48 1997 by Jeff Dalton


;;; It is assumed that the relevant state of the world can be
;;; represented as in the "initially" below.

types volume_group = (vg0 vg1 vg2),
      logical_volume = (lv1 lv2 lv3 lv4),
      physical_volume = (pv1 pv2 pv3 pv4),
      file_system = (fs1 fs2 fs3 fs4);

initially

  {volume_group vg0} = true,

  {volume_group vg1} = true,
  {logical_contents vg1} = {lv1 lv2},
  {physical_contents vg1} = {pv1 pv2 pv3},

  {swap_on ??} = false,

  {mounted_file_systems lv1} = {fs1},
  {mounted_file_systems lv2} = {fs2},
  {mounted_file_systems lv3} = {fs3},
  {mounted_file_systems lv4} = {fs4};


;;; Example tasks

task example_1;
  nodes
    sequential
      1 start,
      3 action {vgremove vg1},
      2 finish
    end_sequential;
end_task;

task example_2;
  nodes
    sequential
      1 start,
      3 action {vgreduce vg1 pv2},
      2 finish
    end_sequential;
end_task;

task web_demo_base;
  nodes
    sequential
      1 start,
      3 action {vgremove vg0},
      2 finish
    end_sequential;
end_task;


;;; Remove volume group

;;; Before the vgremove command can be used to remove the volume
;;; group, it's necessary that no logical volumes, and only one
;;; physical volume, remain in the volume group.

schema vgremove;
  vars ?vg = ?{type volume_group},
       ?pv = ?{type physical_volume};
  expands {vgremove ?vg};
  nodes
    sequential
      1 action {comment {physical volume ?pv will be removed automatically}},
      2 action {type {usr_sbin_vgremove ?vg}},
      3 action {type {rm_r ?vg}}
    end_sequential;
  conditions
    only_use_if {volume_group VG1},
    achieve {physical_contents ?vg} = {?pv},
    achieve {logical_contents ?vg} = {};
  effects
    {physical_contents ?vg} = {},         ;;; remove final physical volume
    {volume_group ?vg} = false;           ;;; volume group no longer exists
end_schema;


;;; Reduce volume group

;;; This is the other high-level action.

;;; Before removing a physical volume, it's necessary
;;; to remove all logical volumes residing on that physical volume.
;;; Here we assume that all physical volumes in the volume group are
;;; used by all logical volumes -- so all logical volumes in the
;;; volume group must be removed.

schema vgreduce;
  vars ?vg = ?{type volume_group},
       ?pv = ?{type physical_volume};
  expands {vgreduce ?vg ?pv};
  nodes
    1 action {remove_physical_volume ?vg ?pv};
end_schema;

schema remove_physical_volume;
  vars ?vg = ?{type volume_group},
       ?pv = ?{type physical_volume},
       ?pvs = ?{satisfies consp},
       ?new_pvs = ?{satisfies consp};
  expands {remove_physical_volume ?vg ?pv};
  nodes
    1 action {primitive_vgreduce ?vg ?pv};
  conditions
    only_use_if {physical_contents ?vg} = ?pvs,
    compute {remove ?pv ?pvs} = ?new_pvs,
    achieve {logical_contents ?vg} = {};
  effects
    {physical_contents ?vg} = ?new_pvs;
end_schema;



;;; The "contents" preconditions for vgremove and vgreduce are handled
;;; by the following schemas.

;;; Reduce_physical_volumes removes all but one physical volume from
;;; a volume group.  Before removing a physical volume, it's necessary
;;; to remove all logical volumes residing on that physical volume.
;;; Here we assume that all physical volumes in the volume group are
;;; used by all logical volumes -- so all logical volumes in the
;;; volume group must be removed.

schema reduce_physical_volumes;
  vars ?vg = ?{type volume_group},
       ?pv = ?{type physical_volume},
       ?pvs = ?{satisfies consp},
       ?pv_rest = ?{satisfies consp};
  expands {reduce_physical_volumes ?vg};
  only_use_for_effects {physical_contents ?vg} = {?pv};
  nodes
    1 iterate action {primitive_vgreduce ?vg ?pv}
        for ?pv over ?pv_rest;
  conditions
    only_use_if {physical_contents ?vg} = ?pvs,
    compute {last ?pvs} = {?pv},
    compute {butlast ?pvs} = ?pv_rest,
    achieve {logical_contents ?vg} = {};
end_schema;

;;; Remove_logical_volumes removes all logical volumes from a
;;; volume group.

schema remove_logical_volumes;
  vars ?vg = ?{type volume_group},
       ?lvs = ?{satisfies consp};
  expands {remove_logical_volumes ?vg};
  only_use_for_effects {logical_contents ?vg} = {};
  nodes
    1 iterate action {primitive_lvremove ?lv}
        for ?lv over ?lvs;
  conditions
    only_use_if {logical_contents ?vg} = ?lvs;
end_schema;


;;; Now we have schemas that respresent the vgreduce and lvremove
;;; commands.

;;; Remove Physical Volume from Volume Group

;;; This schema has only "local" conditions.

schema primitive_vgreduce;
  vars ?vg = ?{type volume_group},
       ?pv = ?{type physical_volume};
  expands {primitive_vgreduce ?vg ?pv};
  nodes
    1 action {type {usr_sbin_vgreduce ?vg ?pv}};
end_schema;


;;; Remove Logical Volume

;;; This schema has only "local" conditions.

;;; Any file system on the logical volume must be unmounted.
;;; There must (also?) be no swap space on the logical volume.
;;; Here, we simply assume there is a file system called FS_name
;;; and we explicitly include the command to unmount it.
;;; In a more complete representation of the domain, there would
;;; be schemas that handled file systems and unmounting.

;;; Another limitation here is that the schema doesn't refer to
;;; anything that represents which volume group the logical volume
;;; is in, though it could look at the logical_contents facts.

;;; The actual lvremove command can remove > 1 logical volume.

schema primitive_lvremove;
  vars ?lv = ?{type logical_volume};
  expands {primitive_lvremove ?lv};
  nodes
    1 action {type {usr_sbin_lvremove_f ?lv}};
  conditions
    only_use_if {swap_on ?lv} = false,
    achieve {mounted_file_systems ?lv} = {};
end_schema;


;;; Unmounting file systems

schema umount_file_systems;
  vars ?lv = ?{type logical_volume},
       ?fs_list;
  only_use_for_effects {mounted_file_systems ?lv} = {};
  nodes
    1 iterate action {primitive_umount ?fs}
        for ?fs over ?fs_list;
  conditions
    only_use_if {mounted_file_systems ?lv} = ?fs_list;
end_schema;

schema primitive_umount;
  vars ?fs = ?{type file_system};
  expands {primitive_umount ?fs};
  nodes 
    1 action {type {usr_umount ?fs}};
end_schema;


;;; Primitive for saying what commands to type

schema type;
  vars ?command;
  expands {type ?command};
end_schema;


;;; Primitive for explicit comments

schema comment;
  vars ?contents;
  expands {comment ?contents};
end_schema;
