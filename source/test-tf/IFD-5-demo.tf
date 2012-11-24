;;; File: ISAT-workflow5.tf
;;;
;;; Purpose: Air Campaign Planning Scenario. Domain to show O-Plan acting
;;;          as a Workflow Planning Aid, intended to act as a "qualifier"
;;;          for entry into the DARPA/Rome Laboratory Planning Initiative
;;;          Integrated Feasibility Demonstration IFD-5.
;;;
;;;          The work was intended to develop the notion of a "planning
;;;          process" level, which itself produced a "process product"
;;;          that was a plan for a domain involving military actions.
;;;
;;; Domain Concept: Austin Tate
;;;
;;; Air Campaign Planning Models: Based on ACP models created by
;;;      John Kingston and Terri Lydiard with the support of
;;;      Lt. Cadenas, Checkmate, The Pentagon, and Dave Hess, SAIC.
;;;
;;; Created : Brian Drabble 29th February 1996
;;; Modified: Brian Drabble 1-Mar-96:
;;;           - Added Capability Primitives to Model.
;;;
;;;           Brian Drabble 12-Jun-96
;;;           - New IDEF-3 diagrams incorporated.
;;;           - Capabilities for EXPECT and O-Plan added.
;;;           - Process Products and their attributes added.
;;;
;;;           Brian Drabble 27-Nov-96
;;;           - Removed the problem of duplicate right hand side values
;;;             for the contents level of the JIPTL.
;;;           - Created a new process product JIPTL-Cutoff.
;;;           - Added the attribute of approval/recommeded to model.
;;;
;;;           Austin Tate 2-May-98
;;;           -  Domain purpose added and credits completed.
;;;
;;; The ACPT system provides support to the JFACC planning cell in defining
;;; different levels of objectives and the targets associated with them. The
;;; ACPT system provides partial coverage of the full ACP process. A report
;;; is available which detailed where the coverage is provided.
;;;
;;; Assumptions on Agenda Entries that come from ACPT:
;;;
;;;     1.  The source agent in the agenda_issue structure is always filled 
;;;         in.
;;;
;;;     2.  The name of the source agent is the one which placed the entry 
;;;         on the agenda. In all cases where O-Plan introduced an entry, 
;;;         this will be O_Plan_WFM.
;;;
;;;     3.  The destination agent in the agenda_issue structure will only be
;;;         filled in when there is an agent capable of carrying it out.
;;;
;;;     4.  In those cases where the capability needs to be decomposed the
;;;         destination agent will differ for lower levels. It may or may not 
;;;         be filled in (it will be O_Plan_WFM if filled in. NB. There are 
;;;         cases where a decomposition shares the same verb as a primitive
;;;         capability, e.g.,
;;;
;;;         {agenda_issue user       ?? Develop {JFACC_plan}  {}},
;;;
;;;         {agenda_issue O_Plan_WFM ?? Develop {tanker_flow} {}},
;;;
;;;     The first requires further decomposition while the second is a 
;;;     primitive.
;;;
;;; Status Notes: 
;;;
;;;	1.  The system works on the single task and is capable of 
;;;         developing the simple plans quickly. The version of O-Plan
;;;         used for validating and testing was Release Version 2.3.
;;;
;;;     2.  There needs to be a method which captures a task description
;;;         from ACPT and provides it in a form that O-Plan can take
;;;         as a task package.
;;;
;;;     3.  The activities in the plan are described as follows:
;;;         {verb noun_phrase qualifier}
;;;
;;;     4.  The always facts have been used to describe the capabilities of
;;;         the different tools and systems. In future the "Tool Registry"
;;;         of ACPT will need to define this in the task defintion
;;;
;;;     5.  O-Plan needs to have the ability to be tasked to deal with only
;;;         spcified nodes in the task description and to ignore the rest
;;;
;;;     6.  This version uses Plan State Variables (PSVs) instead of the 
;;;         ?? match specification. This will create "chains" of PSVs 
;;;         which will be reconcilled when the PSV is bound.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

types process_products = (master_air_attack_plan air_tasking_order
                          JFACC_guidance_letter prioritised_target_list
			  recommended_JIPTL),

      product_attributes = (draft issued circulated available not_available),

      product_perspectives = (risk cost threat_potential quality),

      product_entities = (plan_activities schedule_reservations),

      primitive_pp = (developing_objectives recommended_defense 
		      recommended_apportionment recommended_target
                      JFACC_guidance CAS_sorties target_selection
		      weaponeering_assessment weaponeering_force_support
                      mission_requirements ec_planning tanker_flow
                      Airspace air_tasking_order SPINS target_route
                      targets support_requirements sorties TOT_flow
                      current_status nominated_targets targets
                      prioritised_target_list air_control_order),
  
      verb_list = (Review Modify Build Support Develop Approve Allocate
		   Perform Weaponeer Coordinate Provide Deconflict 
		   Finalise Produce Release Integrate Prioritise Match
		   Consider Identify Group Calculate),

      agent = (O_plan SIPE EXPECT User),
      WF_planner = (O_Plan_WFM);

always  {source_agent} = oplan_WFM,

	{has_capability EXPECT Review},

	{has_capability O_Plan Modify},
	{has_capability O_Plan Develop},

	{has_capability SIPE Modify},
	{has_capability SIPE Build},

	{has_capability User Support},
	{has_capability User Approve},
	{has_capability User Allocate},
	{has_capability User Perform},
	{has_capability User Weaponeer},
	{has_capability User Coordinate},
	{has_capability User Provide},
	{has_capability User Deconflict},
	{has_capability User Finalise},
	{has_capability User Produce},
	{has_capability User Release},
	{has_capability User Integrate},
	{has_capability User Prioritise},
	{has_capability User Match},
	{has_capability User Consider},
	{has_capability User Group},
	{has_capability User Calculate}, 
	{has_capability User Identify};

Initially 

        {status current_status} = available,
	{status intelligence_situation} = available,
        {status unit_information} = available,
	{status SPINS} = available,
        {status JTF_guidance} = available,
        {status JFACC_guidance_letter} = available,
        {status operations_feedback} = available,
        {status plans_mission_feedback} = available,
        {status target_development} = available,
        {status target_planning_updates} = available,
        {status JIPTL} = available,
        {status JIPTL_cutoff} = available,
        {status ICM_MOM_supports} = available,
        {status EC_targets} = available,
        {status EC_planning} = available, 
        {status JMEM_filter} = available,
        {status TNL} = available, 
	{status Service_target_nominations} = available,
        {status JAG} = available,
        {status air_space_management_plan} = available,
	{status tanker_asset_data} = available,

        {contents_level target_development} = on_going,
        {contents_level JFACC_guidance_letter} = draft,
        {contents_level air_space_management_plan} = current,
        {approval_status JIPTL} = complete,

        {resource AOC_intelligence_group} = unallocated,
	{resource JFACC_planner} = unallocated,
	{resource Combat_planning_cell} = unallocated,
	{resource Combat_operations_cell} = unallocated;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Top Level Task Description Level

task Full_Air_Campaign_Planning_Process;
  nodes 1 start,
        2 finish,
	3 action {Full_Air_Campaign_Planning_Process};
  orderings 1 ---> 3, 3 ---> 2;
end_task;

task Partial_Air_Campaign_Planning_Process;
  nodes 1 start,
        2 finish,
	3 action {Partial_Plan_Refinement};
  orderings 1 ---> 3, 3 ---> 2;
end_task;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ACP Domain "Task" Description

schema Full_Air_Campaign_Planning_Process;
  vars ?dst1 = ?{type agent}, ?dst2 = ?{type agent}, ?dst3 = ?{type agent};
  expands {Full_Air_Campaign_Planning_Process};
  nodes 1 action {agenda_issue user ?dst1 Develop {JFACC_plan}{}},
	2 action {agenda_issue user ?dst2 Develop {JIPTL}{}},
	3 action {agenda_issue user ?dst3 Develop {Mission_plan}{}};

  orderings 1 ---> 2, 2 ---> 3;
end_schema;

schema Partial_Plan_Refinement;
  vars ?dst1 = ?{type agent}, ?dst2 = ?{type agent}, ?dst3 = ?{type agent},
       ?dst4 = ?{type agent}, ?dst5 = ?{type agent};
  expands {Partial_Plan_Refinement};
  nodes 
    1 dummy,
    2 dummy,
    3 action {agenda_issue O_Plan_WFM ?dst1 Perform {target_selection}{}},
    4 action {agenda_issue O_Plan_WFM ?dst2 Perform {weaponeering_assessment}{broad}},
    5 action {agenda_issue O_Plan_WFM ?dst3 Develop {recommended_JIPTL} {}},
    6 action {agenda_issue O_Plan_WFM ?dst4 Perform {weaponeering_force_support}{}},
    7 action {agenda_issue O_Plan_WFM ?dst5 Develop {master_air_attack_plan}{}}; 

  orderings
     1 ---> 3, 1 ---> 5, 5 ---> 6, 3 ---> 4, 4 ---> 7, 6 ---> 7, 7 ---> 2,
     end_of 4 ---> end_of 5;

  conditions 
    unsupervised {status JTF_guidance} = available at 3,
    unsupervised {status operations_feedback} = available at 3,
    unsupervised {status target_development} = available at 3,
    unsupervised {contents_level target_development} = on_going at 3,
    unsupervised {status JIPTL} = available at 3,
    unsupervised {status JIPTL_cutoff} = available at 3,
    unsupervised {status ICM_MOM_supports} = available at 3,
    unsupervised {status EC_targets} = available at 3,
    unsupervised {status JFACC_guidance_letter} = available at 4,
    unsupervised {contents_level JFACC_guidance_letter} = approved at 4,
    unsupervised {status JMEM_filter} = available at 4,
    unsupervised {status current_status} = available at 5,
    unsupervised {status Service_target_nominations} = available at 5,
    unsupervised {status JFACC_guidance_letter} = available at 5,
    unsupervised {contents_level JFACC_guidance_letter} = approved at 5,
    unsupervised {status JAG} = available at 5,
    unsupervised {status JIPTL} = available at 6,
    unsupervised {approval_status JIPTL} = complete at 6,
    unsupervised {status JFACC_guidance_letter} = available at 6,
    unsupervised {contents_level JFACC_guidance_letter} = approved at 6,
    unsupervised {status unit_information} = available at 6,
    unsupervised {status JMEM_filter} = available at 6,

    ;;; conditions of the Master Air Attack Plan
    unsupervised {status JIPTL} = available at 7,
    unsupervised {approval_status JIPTL} = complete at 7,

    unsupervised {status operations_feedback} = available at 7,
    unsupervised {status TNL} = available at 7,
    unsupervised {status current_status} = available at 7,
    unsupervised {status unit_information} = available at 7,
    unsupervised {status SPINS} = available at 7,

    unsupervised {status JIPTL} = available at 7,
    unsupervised {rec_status JIPTL} = complete at 7,

    unsupervised {contents_level air_space_management_plan} = current at 7,
    unsupervised {status air_space_management_plan} = available at 7,
    unsupervised {status CAS_ATO_sorties} = available at 7,
    unsupervised {status DCA_ATO_inputs} = available at 7,

    supervised {status special_weapons_requirements} = available at 5 from 4,
    supervised {status vunerability_assessment} = available at 5 from 4,
    supervised {status target_list} = available at 4 from 3,
    supervised {contents_level target_list} = potential at 4 from 3,
    supervised {resource AOC_intelligence_group} = allocated at end_of 3 from begin_of 3,
    supervised {status target_list} = available at 5 from 3,
    supervised {contents_level target_list} = potential at 5 from 3,
    supervised {rec_status JIPTL} = complete at 6 from 5;

  effects
    {status JTF_guidance} = available at begin_of self,
    {status operations_feedback} = available at begin_of self,
    {status target_development} = available at begin_of self,
    {contents_level target_development} = on_going at begin_of self,
    {status JIPTL} = available at begin_of self,
    {status JIPTL_cutoff} = available at begin_of self,
    {status ICM_MOM_supports} = available at begin_of self,
    {status EC_targets} = available at begin_of self,
    {status JFACC_guidance_letter} = available at begin_of self,
    {contents_level JFACC_guidance_letter} = approved at begin_of self, 
    {status JMEM_filter} = available at begin_of self,
    {status current_status} = available at begin_of self,
    {status Service_target_nominations} = available at begin_of self,
    {status JAG} = available at begin_of self,
    {status JIPTL} = available at begin_of self,
    {approval_status  JIPTL} = complete at begin_of self,

    ;;; conditions of the Master Air Attack Plan
    {status TNL} = available at begin_of self,
    {status current_status} = available at begin_of self,
    {status unit_information} = available at begin_of self,
    {status SPINS} = available at begin_of self,
 
    {status JIPTL} = available at begin_of self,
    {rec_status JIPTL} = complete at begin_of self,

    {contents_level air_space_management_plan} = current at begin_of self,
    {status air_space_management_plan} = available at begin_of self,
    {status CAS_ATO_sorties} = available at begin_of self,
    {status DCA_ATO_inputs} = available at begin_of self,
    {status JFACC_guidance_letter} = available at begin_of self,
    {contents_level JFACC_guidance_letter} = approved at begin_of self,

    {resource AOC_intelligence_group} = unallocated at begin_of self,
    {status target_list} = available at begin_of self,
    {contents_level target_list} = potential at begin_of self,
    {status special_weapons_requirements} = available at begin_of self,
    {status vunerability_assessment} = available at begin_of self,
    {status JIPTL} = available at begin_of self;

  time_windows
    delay_between begin_of 3 and begin_of 5 = 0;

end_schema;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;;; Main Activities of the ACP Domain

schema Phase_1_JFACC_Planning;
  vars ?src, ?dst1 = ?{type agent}, ?dst2 = ?{type agent}, 
       ?dst3 = ?{type agent}, ?dst4 = ?{type agent}, ?dst5 = ?{type agent}, 
       ?dst6 = ?{type agent};
  expands {agenda_issue ?src ?dst1 Develop {JFACC_plan} {}};
  nodes 
    sequential
      1 action {agenda_issue O_Plan_WFM ?dst2 Support {developing_objectives}{}},
      parallel
	2 action {agenda_issue O_Plan_WFM ?dst3 Develop {recommended_defense}{}},
	3 action {agenda_issue O_Plan_WFM ?dst4 Develop {recommended_apportionment}{}},
	4 action {agenda_issue O_Plan_WFM ?dst5 Develop {recommended_target}{}}
      end_parallel,
      5 action {agenda_issue O_Plan_WFM ?dst6 Approve {JFACC_guidance}{}}
    end_sequential;

  conditions
    unsupervised {resource JFACC_planner} = unallocated at begin_of 1,
    unsupervised {resource Combat_planning_cell} = unallocated at begin_of 4,
    unsupervised {resource Combat_operations_cell} = unallocated at begin_of 4,
    unsupervised {resource JFACC_planner} = unallocated at begin_of 5,

    unsupervised {status current_status} = available at 2,
    unsupervised {status unit_information} = available at 2,
    unsupervised {status JTF_guidance} = available at 2,
    unsupervised {status current_status} = available at 3,
    unsupervised {status JTF_guidance} = available at 3,
    unsupervised {status current_status} = available at 4,
    unsupervised {status JTF_guidance} = available at 4,
    unsupervised {status JFACC_guidance_letter} = available at 5,
    unsupervised {contents_level JFACC_guidance_letter} = draft at 5,

    supervised {resource Combat_operations_cell} = allocated at end_of 4 from begin_of 4,
    supervised {resource Combat_planning_cell} = allocated at end_of 4 from begin_of 4,
    supervised {resource JFC_planner} = allocated at end_of 5 from begin_of 5,
    supervised {resource JFACC_planner} = allocated at end_of 1 from begin_of 1,
    supervised {status op_plan_obj_guidance} = available at 2 from 1,
    supervised {contents_level op_plan_obj_guidance} = recommended at 2 from 1,
    supervised {status op_plan_obj_guide_rec} = available at 3 from 1,
    supervised {contents_level op_plan_obj_guidance} = recommended at 3 from 1,
    supervised {status op_plan_obj_guide_rec} = available at 4 from 1,
    supervised {contents_level op_plan_obj_guidance} = recommended at 4 from 1,
    supervised {status target_selection_criteria} = available at 4 from 1;

  effects
    {resource JFACC_planner} = allocated at begin_of 1,
    {resource JFACC_planner} = unallocated at end_of 1,
    {resource Combat_planning_cell} = allocated at begin_of 4,
    {resource Combat_planning_cell} = unallocated at end_of 4,
    {resource Combat_operations_cell} = allocated at begin_of 4,
    {resource Combat_operations_cell} = unallocated at end_of 4,
    {resource JFC_planner} = allocated at begin_of 5,
    {resource JFC_planner} = unallocated at end_of 5,
    {status op_plan_obj_guidance} = available at 1,
    {contents_level op_plan_obj_guidance} = recommended at 1,
    {status target_selection_criteria} = available at 1,
    {status DCA_ATO_inputs} = available at 2,
    {status defense_posture} = available at 2,
    {contents_level defense_posture} = recommended at 2,
    {status apportioned_sorties} = available at 3,
    {contents_level opportioned_sorties} = recommended at 3,
    {status target_objectives} = available at 4,
    {contents_level target_objectives} = recommended at 4,
    {status JFACC_guidance_letter} = available at 5,
    {contents_level JFACC_guidance_letter} = approved at 5;

end_schema;

schema Phase_2_JIPTL_Planning;
  vars ?src, ?dst1 = ?{type agent}, ?dst2 = ?{type agent}, ?dst3 = ?{type agent}, 
       ?dst4 = ?{type agent}, ?dst5 = ?{type agent}, ?dst6 = ?{type agent};
  expands {agenda_issue ?src ?dst1 Develop {JIPTL}{}};
  nodes 
    1 action {agenda_issue O_Plan_WFM ?dst2 Allocate {CAS_sorties}{}},
    2 action {agenda_issue O_Plan_WFM ?dst3 Perform {target_selection}{}},
    3 action {agenda_issue O_Plan_WFM ?dst4 Perform {weaponeering_assessment}{broad}},
    4 action {agenda_issue O_Plan_WFM ?dst5 Develop {recommended_JIPTL} {}},
    5 action {agenda_issue O_Plan_WFM ?dst6 Perform {weaponeering_force_support}{}};

  orderings end_of 3 ---> end_of 4,
	    2 ---> 3,
            4 ---> 5;
  conditions
    unsupervised {resource AOC_intelligence_group} = unallocated at begin_of 1,
    unsupervised {resource AOC_intelligence_group} = unallocated at begin_of 2,

    unsupervised {status JFACC_guidance_letter} = available at 1,
    unsupervised {contents_level JFACC_guidance_letter} = approved at 1,
    unsupervised {status unit_information} = available at 1,
    unsupervised {status JTF_guidance} = available at 2,
    unsupervised {status operations_feedback} = available at 2,
    unsupervised {status target_development} = available at 2,
    unsupervised {contents_level target_development} = on_going at 2,
    unsupervised {status JIPTL} = available at 2,
    unsupervised {approval_status JIPTL} = complete at 2,
    unsupervised {status ICM_MOM_supports} = available at 2,
    unsupervised {status EC_targets} = available at 2,
    unsupervised {status JFACC_guidance_letter} = available at 3,
    unsupervised {contents_level JFACC_guidance_letter} = approved at 3,
    unsupervised {status JMEM_filter} = available at 3,
    unsupervised {status current_status} = available at 4,
    unsupervised {status Service_target_nominations} = available at 4,
    unsupervised {status JFACC_guidance_letter} = available at 4,
    unsupervised {contents_level JFACC_guidance_letter} = approved at 4,
    unsupervised {status JAG} = available at 4,
    unsupervised {status JIPTL} = available at 5,
    unsupervised {status JFACC_guidance_letter} = available at 5,
    unsupervised {contents_level JFACC_guidance_letter} = approved at 5,
    unsupervised {status unit_information} = available at 5,
    unsupervised {status JMEM_filter} = available at 5,

    supervised {status special_weapons_requirements} = available at end_of 4 from  3,
    supervised {status vunerability_assessment} = available at end_of 4 from 3,
    supervised {status target_list} = available at 3 from 2,
    supervised {contents_level target_list} = potential at 3 from 2,
    supervised {resource AOC_intelligence_group} = allocated at end_of 2 from begin_of 2,
    supervised {resource AOC_intelligence_group} = allocated at end_of 1 from begin_of 1,
    supervised {status target_list} = available at 4 from 2,
    supervised {contents_level target_list} = potential at 4 from 2,

    supervised {rec_status JIPTL} = complete at 5 from 4;

  effects
    {resource AOC_intelligence_group} = allocated at end_of 1,
    {resource AOC_intelligence_group} = unallocated at end_of 2,
    {status CAS_ATO_sorties} = available at 1,
    {status target_list} = available at 2,
    {contents_level target_list} = potential at 2,
    {status special_weapons_requirements} = available at begin_of 3,
    {status vunerability_assessment} = available at begin_of 3,
    {status JIPTL} = available at 4,
    {rec_status JIPTL} = complete at 4,
    {status TNL} = available at 5;

  time_windows
    delay_between begin_of 1 and begin_of 2 = 0,
    delay_between begin_of 2 and begin_of 4 = 0;
end_schema;
 
schema Phase_3_Mission_Planning;
  vars ?src, ?dst1 = ?{type agent}, ?dst2 = ?{type agent}, ?dst3 = ?{type agent}, 
       ?dst4 = ?{type agent}, ?dst5 = ?{type agent}, ?dst6 = ?{type agent}, 
       ?dst7 = ?{type agent}, ?dst8 = ?{type agent}, ?dst9 = ?{type agent}, 
       ?dst10 = ?{type agent}, ?dst11 = ?{type agent};
  expands {agenda_issue ?src ?dst1 Develop {Mission_plan}{}};
  nodes 
    1 action {agenda_issue O_Plan_WFM ?dst2 Develop {master_air_attack_plan}{}},
    2 action {agenda_issue O_Plan_WFM ?dst3 Coordinate {mission_requirements}{}},
    3 action {agenda_issue O_Plan_WFM ?dst4 Provide {ec_planning}{}},
    4 action {agenda_issue O_Plan_WFM ?dst5 Develop {tanker_flow}{}},
    5 action {agenda_issue O_Plan_WFM ?dst6 Deconflict {Airspace}{}},
    6 action {agenda_issue O_Plan_WFM ?dst7 Finalise {SPINS}{}},
    7 action {agenda_issue O_Plan_WFM ?dst8 Finalise {air_control_order}{}},
    8 action {agenda_issue O_Plan_WFM ?dst9 Produce {air_tasking_order}{}},
    9 action {agenda_issue O_Plan_WFM ?dst10 Finalise {air_tasking_order} {QCl}},
    10 action {agenda_issue O_Plan_WFM ?dst11 Release {air_tasking_order}{}};

  orderings 
    1 ---> 2, 1 ---> 3, 1 ---> 4, 5 ---> 7, 8 ---> 9, 9 ---> 10,
    begin_of 2 ---> begin_of 3,
    begin_of 2 ---> begin_of 4,
    begin_of 2 ---> begin_of 6,
    begin_of 3 ---> begin_of 6,
    begin_of 4 ---> begin_of 6,
    begin_of 6 ---> begin_of 8,
    begin_of 7 ---> begin_of 8,
    end_of 2 ---> end_of 3,
    end_of 2 ---> end_of 4,
    end_of 4 ---> end_of 5,

    end_of 2 ---> end_of 6,
    end_of 3 ---> end_of 6,
    end_of 4 ---> end_of 6,
    end_of 6 ---> end_of 8,
    end_of 7 ---> end_of 6;

  conditions
    unsupervised {status JIPTL} = available at 1,
    unsupervised {approval_status JIPTL} = complete at 1,
    unsupervised {status JIPTL} = available at 3,
    unsupervised {approval_status JIPTL} = complete at 3,
    unsupervised {status JIPTL} = available at 3,
    unsupervised {rec_status JIPTL} = complete at 3,
    unsupervised {status operations_feedback} = available at 1,
    unsupervised {status operations_feedback} = available at 5,
    unsupervised {status TNL} = available at 1,
    unsupervised {status current_status} = available at 1,
    unsupervised {status unit_information} = available at 1,
    unsupervised {status unit_information} = available at 2,
    unsupervised {status SPINS} = available at 1,
    unsupervised {status JIPTL} = available at 1,
    unsupervised {rec_status JIPTL} = complete at 1,
    unsupervised {contents_level air_space_management_plan} = current at 1,
    unsupervised {status air_space_management_plan} = available at 1,
    unsupervised {status CAS_ATO_sorties} = available at 1,
    unsupervised {status DCA_ATO_inputs} = available at 1,
    unsupervised {status JFACC_guidance_letter} = available at 1,
    unsupervised {contents_level JFACC_guidance_letter} = approved at 1,
    unsupervised {status JFACC_guidance_letter} = available at 9,
    unsupervised {contents_level JFACC_guidance_letter} = approved at 9,
    unsupervised {status EC_planning} = available at 2,
    unsupervised {status target_planning_updates} = available at 2,
    unsupervised {status plans_mission_feedback} = available at 4,
    unsupervised {status plans_mission_feedback} = available at 5,
    unsupervised {status tanker_asset_data} = available at 4,
    unsupervised {status intelligence_situation} = available at 5,
    unsupervised {status air_space_management_plan} = available at 5,

    supervised {status EC_support_requests} = available at 3 from 2,
    supervised {status refuelling_requests} = available at 4 from 2,
    supervised {status ACM_requirements} = available at 5 from 4,
    supervised {status air_space_management_changes} = available at 7 from 5,
    supervised {status EC_SPINS} = available at 6 from 3,
    supervised {status Tanker_SPINS} = available at 6 from 4,
    supervised {status Target_mission_SPINS} = available at 6 from 2,
    supervised {status Air_Control_Order} = available at 8 from 6,
    supervised {status SPINS} = available at 8 from 6,
    supervised {status Tanker_mission_data} = available at 8 from 4,
    supervised {status Target_mission_data} = available at 8 from 2,
    supervised {status EC_Planning} = available at 8 from 3,
    supervised {status Air_Tasking_Order} = available at 9 from 8,
    supervised {contents_level Air_Tasking_Order} = unreleased at 9 from 8,
    supervised {contents_level Air_Tasking_Order} = approved at 10 from 9;

  effects
    {status EC_support_requests} = available at 2,
    {status Target_mission_data} = available at 2,
    {status Target_mission_SPINS} = available at 2,
    {status Unit_Heads_UP} = available at 2,
    {status refuelling_requests} = available at 2,
    {status EC_planning} = available at 3,
    {status EC_SPINS} = available at 3,
    {status ACM_requirements} = available at 4,
    {status Tanker_mission_data} = available at 4,
    {status Tanker_SPINS} = available at 4,
    {status air_space_management_changes} = available at 5,
    {status Air_Control_Order} = available at 7,
    {status SPINS} = available at 6,
    {status Air_Control_Order_Summary} = available at 8,
    {status Air_Tasking_Order} = available at 8,
    {contents_level Air_Tasking_Order} = unreleased at 8,
    {status Air_Tasking_Order} = available at 9,
    {contents_level Air_Tasking_Order} = approved at 9,
    {status Air_Tasking_Order} = available at 10,
    {contents_level Air_Tasking_Order} = released at 10;
    
  time_windows
    delay_between begin_of 3 and begin_of 4 = 0,
    delay_between begin_of 4 and begin_of 5 = 0,
    delay_between end_of 3 and end_of 4 = 0,
    delay_between end_of 7 and end_of 8 = 0;

end_schema;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Intermediate Level Support Activities

schema Develop_the_recommended_JIPTL;
  vars ?dst1 = ?{type agent}, ?dst2 = ?{type agent}, ?dst3 = ?{type agent};
  expands {agenda_issue O_Plan_WFM ?dst1 Develop {recommended_JIPTL} ??};
  nodes 
    sequential
      1 action {agenda_issue O_Plan_WFM ?dst2 Integrate {nominated_targets} {}},
      2 action {agenda_issue O_Plan_WFM ?dst3 Prioritise {integrated_targets} {}}
   end_sequential;
  conditions
    unsupervised {status operations_feedback} = available at 1,
    unsupervised {status EC_targets} = available at 1,
    unsupervised {status Service_target_nominations} = available at 1,
    unsupervised {status JFACC_guidance_letter} = available at 1,
    unsupervised {contents_level JFACC_guidance_letter} = approved at 1,
    unsupervised {status current_status} = available at 2,
    unsupervised {status intelligence_situation} = available at 2,
    unsupervised {status Service_target_nominations} = available at 2,
    unsupervised {status JFACC_guidance_letter} = available at 2,
    unsupervised {status air_space_management_plan} = available at 2,
    supervised {status JIPTL_list} = available at 2 from 1;

  effects
    {status JIPTL_list} = available at 1;

end_schema;

schema Develop_the_Master_Air_Attack_Plan;
  vars ?dst1 = ?{type agent}, ?dst2 = ?{type agent}, ?dst3 = ?{type agent}, 
       ?dst4 = ?{type agent}, ?dst5 = ?{type agent}, ?dst6 = ?{type agent}, 
       ?dst7 = ?{type agent};
  expands {agenda_issue O_Plan_WFM ?dst1 Develop {master_air_attack_plan} ??};
  nodes
    1 action {agenda_issue O_Plan_WFM ?dst2 Consider {target_route}{threat}},
    2 action {agenda_issue O_Plan_WFM ?dst3 Group {targets}{}},
    3 action {agenda_issue O_Plan_WFM ?dst4 Identify {support_requirements}{}},
    4 action {agenda_issue O_Plan_WFM ?dst5 Prioritise {support_requirements}{}},
    5 action {agenda_issue O_Plan_WFM ?dst6 Develop {TOT_flow}{rough}},
    6 action {agenda_issue O_Plan_WFM ?dst7 Calculate {sorties}{}};

  orderings begin_of 1 ---> begin_of 2,
            end_of 1 ---> end_of 2,
	    begin_of 1 ---> begin_of 6,
	    2 ---> 3,
	    2 ---> 4,
	    4 ---> 5,
	    6 ---> 5;

  conditions
    unsupervised {status JIPTL} = available at 1,
    unsupervised {approval_status JIPTL} = complete at 1,
    unsupervised {status JFACC_guidance_letter} = available at 5,
    unsupervised {contents_level JFACC_guidance_letter} = approved at 5,
    unsupervised {status JIPTL} = available at 2,
    unsupervised {approval_status JIPTL} = complete at 2, ;;; should be approved
    unsupervised {status TNL} = available at 1,
    unsupervised {status TNL} = available at 2,
    unsupervised {status operations_feedback} = available at 2,
    unsupervised {status unit_information} = available at 3,
    unsupervised {status intelligence_situation} = available at 1,
    unsupervised {status CAS_ATO_sorties} = available at 5,

    supervised {status perceived_target_route_threat} = available at end_of 2 from 1,
    supervised {status target_groupings} = available at 3 from 2,
    supervised {status sorties} = available at 5 from 6,
    supervised {status support_requirements} = available at 5 from 3,
    supervised {status target_groupings} = available at 5 from 2;

  effects
    {status perceived_target_route_threat} = available at 1,
    {status target_groupings} = available at 2,
    {status support_requirements} = available at 3,
    {status sorties} = available at 6,
    {status sortie_target_flow} = available at 5,
    {contents_level sortie_target_flow} = rough at 5;

  time_windows 
    delay_between begin_of 1 and begin_of 6 = 0;

end_schema;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Lower Level Support Activities

schema Develop_the_Prioritised_Integrated_Targets;
  vars ?dst1 = ?{type agent}, ?dst2 = ?{type agent}, ?dst3 = ?{type agent}, 
       ?dst4 = ?{type agent};
  expands {agenda_issue O_Plan_WFM ?dst1 Prioritise {integrated_targets} ??};
  nodes
    sequential
      1 action {agenda_issue O_Plan_WFM ?dst2 Review {current_status}{}},
      2 action {agenda_issue O_Plan_WFM ?dst3 Match {targets} {JFACC_guidance_letter}},
      3 action {agenda_issue O_Plan_WFM ?dst4 Build {prioritised_target_list}{}}
    end_sequential;

  conditions
    unsupervised {status current_status} = available at 1,
    unsupervised {status intelligence_situation} = available at 2,
    unsupervised {status intelligence_situation} = available at 3,
    unsupervised {status JIPTL_list} = available at 2,
    unsupervised {approval_status JIPTL} = complete at 3,
    unsupervised {status JIPTL} = available at 3,
    unsupervised {status TNL} = available at 3,

    supervised {status status_impact} = available at 2 from 1,
    supervised {status target_priorities} = available at 3 from 2,
    supervised {contents_level target_priorities} = proposed at 3 from 2;

  effects
    {status status_impact} = available at 1,
    {status target_priorities} = available at 2,
    {contents_level target_priorities} = proposed at 2;

end_schema;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mapping of the Agent Capabilties to the requirement
;;;
;;; This schema identifies the tool with the correct capability to address
;;; the activity needs

schema Find_Capability_and_Construct_Issue;
  vars ?verb,
       ?nouns,
       ?noun = ?{type primitive_pp},
       ?qualifier,
       ?dst,
       ?src = ?{type WF_planner};
  expands {agenda_issue ?src ?dst ?verb ?nouns ?qualifier};
  conditions
     compute {first ?nouns} = ?noun,
     only_use_if {has_capability ?dst ?verb};
end_schema;




