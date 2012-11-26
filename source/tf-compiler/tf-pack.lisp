;;;; File: tf-pack.lisp
;;; Contains: Package definition for the TF language
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: March 1992
;;; Updated: Sun Aug 22 23:31:01 1999 by Jeff Dalton
;;; Copyright: (c) 1992, 1993, 1994 AIAI, University of Edinburgh


;;; The "TF" package exports all the symbols used in the TF language.
;;; When reading a TF file, the current package must be one that uses
;;; the "TF" package.


(in-package "TF" :nicknames '("OPLAN-TF" "TF-LANGUAGE"))

(export
  '(& |(| |)| * + |,| - --- ---> |..| / /= |:| |;| < <= = => > >=
    achieve
    achievable				;/\/ internal condition type
    achieve_after_point
    across				;/\/ extension for 3.x
    action 
    after 
    allocates 
    always 
    and 
    at 
    author 
    before 
    begin_of 
    between 
    compute 
    compute_condition 
    condition_node_end
    condition_contributor_node_end
    conditions 
    constraint_syntax			;/\/ extension for 3.x
    constraint_types			;/\/ extension for 3.x
    consumable_strictly 
    consumes 
    count 
    date 
    days 
    deallocates 
    defaults 
    delay_between 
    depends_on 
    description 
    domain_rules 
    dummy 
    duration 
    earliest_finish_of_plan 
    effects 
    effect_node_end 
    endschema 
    end_meta_process_schema 
    end_meta_schema 
    end_of
    end_parallel			;extension /\/
    end_process_schema 
    end_repair_schema
    end_schema
    end_sequential			;extension /\/
    end_task 
    end_tf_info 
    entity_detail 
    et 
    event 
    expands 
    finish 
    for 
    forall				;for_all  ? /\/
    foreach				;for_each ? /\/
    from 
    history 
    hours 
    ideal 
    impossible
    include
    incremental 
    inf 
    infinity 
    info 
    information 
    initially 
    initial_resources 
    initial_time 
    instance_of
    instantiates
    iterate 
    language 
    latest_finish_of_plan 
    levels_output 
    link_from_node_end 
    link_selection 
    link_to_node_end 
    lisp 
    local_vars 
    lt 
    max 
    meta_process_schema 
    meta_schema 
    min 
    minutes 
    mode_selection 
    multiple_answer 
    no 
    nodes 
    none 
    notepad 
    number_of_nodes 
    occurs_at 
    only_use_for_effects 
    only_use_for_query 
    only_use_for_resources 
    only_use_if 
    orderings 
    over 
    overall
    parallel				;extension /\/
    plan_output 
    plan_viewer 
    prefer_plans_with 
    prefer_schemas 
    process_schema 
    produces 
    program 
    repair_schema
    resource 
    resource_at_node_end 
    resource_conversions 
    resource_output 
    resource_overall 
    resource_types 
    resource_units
    resource_usage_node_end
    resources 
    schema 
    seconds 
    self
    sequential				;extension /\/
    sequence				;extension /\/
    set 
    size 
    snapshot 
    some 
    start 
    string 
    supervised 
    task 
    tf_info 
    tf_input 
    time_window_node_end
    time_windows
    title 
    true 
    types 
    undef 
    unlimited 
    unsupervised 
    use 
    v 
    value 
    variable_restriction 
    vars 
    vars_relations 
    weight 
    with 
    world_viewer 
    yes 
    [ ] ^ { } ~))

;;; End
