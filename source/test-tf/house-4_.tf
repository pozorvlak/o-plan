;;; House Building Domain - with time windows
;;;
;;; BAT   5-Dec-76: Nonlin TF.
;;; KWC   9-Sep-85: Converted to O-Plan1 TF.
;;; BD   12-May-92: Converted to O-Plan2 TF.
;;; BAT  20-Nov-92: Time Windows set, multiple kitchen options,
;;;                 ground_condition waiting added
;;; JD   30-Nov-92: Adjusted time windows.
;;; JD   18-Mar-99: 
;;;

task build_house;
  nodes 1 start,  
        2 finish,
        3 action {build_house};
  orderings 1 ---> 3, 3 ---> 2;
  effects {ground_condition} = ready at 1;
end_task;

task build_house_to_time_0;
  nodes 1 start,  
        2 finish,
        3 action {build_house};
  orderings 1 ---> 3, 3 ---> 2;
  effects {ground_condition} = unsuitable at begin_of 1;
  time_windows duration 3 = 0..35 days;
end_task;

task build_house_to_time_1;
  nodes 1 start,  
        2 finish,
        3 action {build_house};
  orderings 1 ---> 3, 3 ---> 2;
  effects {ground_condition} = ready at begin_of 1;
  time_windows duration 3 = 0..30 days;
end_task;

task build_house_to_time_2;
  nodes 1 start,  
        2 finish,
        3 action {build_house};
  orderings 1 ---> 3, 3 ---> 2;
  effects {ground_condition} = ready at begin_of 1;
  time_windows  0~09:00 at begin_of 1,   ;;; start day 0 at 09:00
               35~09:00 at end_of 2;     ;;; finish by day 35 at 09:00
end_task;

schema build;
  expands {build_house};        ;;; this expands the top level action
  nodes     1 action {excavate_and_pour_footers    },  ;;; some are primitive
            2 action {pour_concrete_foundations    },
            3 action {erect_frame_and_roof         },
            4 action {lay_brickwork                },
            5 action {finish_roofing_and_flashing  },
            6 action {fasten_gutters_and_downspouts},
            7 action {finish_grading               },
            8 action {pour_walks_and_landscape     },
            9 action {install_services             },  ;;; some are not.
           10 action {decorate                     };
  orderings 1 ---> 2,  2 ---> 3,  3 ---> 4,  4 ---> 5,
            5 ---> 6,  6 ---> 7,  7 ---> 8;
  ;;; actions 9 & 10 are not ordered wrt other actions - they are in parallel
  conditions   supervised {footers_poured        } at 2 from [1],
               supervised {foundations_laid      } at 3 from [2],
               supervised {frame_and_roof_erected} at 4 from [3],
               supervised {brickwork_done        } at 5 from [4],
               supervised {roofing_finished      } at 6 from [5],
               supervised {gutters_etc_fastened  } at 7 from [6],
             unsupervised {storm_drains_laid     } at 7,
               supervised {grading_done          } at 8 from [7];
end_schema;
  
schema service_1;
  expands {install_services};
  only_use_for_effects {installed_services 1};
  nodes     1 action {install_drains           },
            2 action {lay_storm_drains         },
            3 action {install_rough_plumbing   },
            4 action {install_finished plumbing},
            5 action {install_rough_wiring     },
            6 action {finish_electrical_work   },
            7 action {install_kitchen_equipment},
            8 action {install_air_conditioning };
  orderings 1 ---> 3,  3 ---> 4,  5 ---> 6,  3 ---> 7,  5 ---> 7;
  conditions   supervised {drains_installed        } at 3 from [1],
               supervised {rough_plumbing_installed} at 4 from [3],
               supervised {rough_wiring_installed  } at 6 from [5],
               supervised {rough_plumbing_installed} at 7 from [3],
               supervised {rough_wiring_installed  } at 7 from [5],
             unsupervised {foundations_laid        } at 1,
             unsupervised {foundations_laid        } at 2,
             unsupervised {frame_and_roof_erected  } at 5,
             unsupervised {frame_and_roof_erected  } at 8,
             unsupervised {basement_floor_laid     } at 8,
             unsupervised {flooring_finished       } at 4,
             unsupervised {flooring_finished       } at 7,
             unsupervised {painted                 } at 6;
  time_windows duration self = 0 days .. 24 days;
end_schema;
           
schema decor;
  expands {decorate};
  nodes     1 action {fasten_plaster_and_plaster_board},
            2 action {pour_basement_floor             },
            3 action {lay_finished_flooring           },
            4 action {finish_carpentry                },
            5 action {sand_and_varnish_floors         },
            6 action {paint                           };
  orderings  2 ---> 3,  3 ---> 4,  4 ---> 5,  1 ---> 3,  6 ---> 5;
  conditions unsupervised {rough_plumbing_installed   } at 1,
             unsupervised {rough_wiring_installed     } at 1,
             unsupervised {air_conditioning_installed } at 1,
             unsupervised {drains_installed           } at 2,
             unsupervised {plumbing_finished          } at 6,
             unsupervised {kitchen_equipment_installed} at 6,
               supervised {plastering_finished        } at 3 from [1],
               supervised {basement_floor_laid        } at 3 from [2],
               supervised {flooring_finished          } at 4 from [3],
               supervised {carpentry_finished         } at 5 from [4],
               supervised {painted                    } at 5 from [6];
  time_windows duration self = 0 days .. 23 days;
end_schema;

;;; Now for completeness a list of primitive actions. Primitives are
;;; defined as having no nodes list and must have an expands pattern.

schema excavate_when_unsuitable;
  expands {excavate_and_pour_footers};
  only_use_for_effects {footers_poured} = true;
  conditions only_use_if {ground_condition} = unsuitable;
  effects {ground_condition} = ready;
  time_windows duration self = 14 days;
end_schema;

schema excavate_when_ready;
  expands {excavate_and_pour_footers};
  only_use_for_effects {footers_poured} = true;
  conditions only_use_if {ground_condition} = ready;
  time_windows duration self = 4 days;
end_schema;

schema pour_concrete;
  expands {pour_concrete_foundations};
  only_use_for_effects {foundations_laid} = true;
  time_windows duration self = 2 days;
end_schema;

schema erect_frame;
  expands {erect_frame_and_roof};
  only_use_for_effects {frame_and_roof_erected} = true;
  time_windows duration self = 4 days;
end_schema;

schema brickwork;
  expands {lay_brickwork};
  only_use_for_effects {brickwork_done} = true;
  time_windows duration self = 7 days;
end_schema;

schema finish_roofing;
  expands {finish_roofing_and_flashing};
  only_use_for_effects {roofing_finished} = true;
  time_windows duration self = 3 days;
end_schema;

schema fasten_gutters;
  expands {fasten_gutters_and_downspouts};
  only_use_for_effects {gutters_etc_fastened} = true;
  time_windows duration self = 1 days;
end_schema;

schema finish_grading;
  expands {finish_grading};
  only_use_for_effects {grading_done} = true;
  time_windows duration self = 2 days;
end_schema;

schema pour_walks;
  expands {pour_walks_and_landscape};
  only_use_for_effects {landscaping_done} = true;
  time_windows duration self = 5 days;
end_schema;

schema install_drains;
  expands {install_drains};
  only_use_for_effects {drains_installed} = true;
  time_windows duration self = 1 days;
end_schema;

schema lay_storm;
  expands {lay_storm_drains};
  only_use_for_effects {storm_drains_laid} = true;
  time_windows duration self = 1 days;
end_schema;

schema rough_plumbing;
  expands {install_rough_plumbing};
  only_use_for_effects {rough_plumbing_installed} = true;
  time_windows duration self = 1 days;
end_schema;

schema install_finished_plumbing;
  expands {install_finished plumbing};
  only_use_for_effects {plumbing_finished} = true;
  time_windows duration self = 2 days;
end_schema;

schema rough_wiring;
  expands {install_rough_wiring};
  only_use_for_effects {rough_wiring_installed} = true;
  time_windows duration self = 2 days;
end_schema;

schema finish_electrical;
  expands {finish_electrical_work};
  only_use_for_effects {electrical_work_finished} = true;
  time_windows duration self = 1 days;
end_schema;

schema install_kitchen_luxury;
  expands {install_kitchen_equipment};
  only_use_for_effects {kitchen_equipment_installed} = true,
                       {installed_level_kitchen} = luxury;
  time_windows duration self = 3 days;
end_schema;

schema install_kitchen_standard;
  expands {install_kitchen_equipment};
  only_use_for_effects {kitchen_equipment_installed} = true,
                       {installed_level_kitchen} = standard;
  time_windows duration self = 2 days;
end_schema;

schema install_air;
  expands {install_air_conditioning};
  only_use_for_effects {air_conditioning_installed} = true;
  time_windows duration self = 3 days;
end_schema;

schema fasten_plaster;
  expands {fasten_plaster_and_plaster_board};
  only_use_for_effects {plastering_finished } = true;
  time_windows duration self = 7 days;
end_schema;

schema pour_basement;
  expands {pour_basement_floor};
  only_use_for_effects {basement_floor_laid } = true;
  time_windows duration self = 2 days;
end_schema;

schema lay_flooring;
  expands {lay_finished_flooring};
  only_use_for_effects {flooring_finished} = true;
  time_windows duration self = 3 days;
end_schema;

schema finish_carpentry;
  expands {finish_carpentry};
  only_use_for_effects {carpentry_finished} = true;
  time_windows duration self = 3 days;
end_schema;

schema sand;
  expands {sand_and_varnish_floors};
  only_use_for_effects {floors_finished} = true;
  time_windows duration self = 2 days;
end_schema;

schema paint;
  expands {paint};
  only_use_for_effects {painted} = true;
  time_windows duration self = 3 days;
end_schema;

