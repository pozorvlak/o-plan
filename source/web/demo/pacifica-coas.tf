;;;; File: pacifica-coas.tf
;;; Contains: TF for the Pacifica COA matrix demo
;;; Created: Jeff Dalton, May 1997
;;; Updated: Mon May 26 02:55:00 1997 by Jeff Dalton

types city      = (Abyss Barnacle Calypso Delta),
      location  = (Abyss Barnacle Calypso Delta),
      road      = (Delta_Abyss Abyss_Barnacle
                   Barnacle_Calypso Calypso_Delta);

always
  {evac_destination Delta},
  {medical_team_base Delta};


;;;; Top-level schemas

;;; Helicopter schemas 1st, since we prefer them.  /\/

;;; Evacuate injured ...

schema evac_injured_h;
  vars ?from = ?{type city},
       ?to   = ?{type city},
       ?h    = ?{type helicopter};
  expands {evacuate injured from ?from using a helicopter};
  only_use_for_effects {evacuated injured from ?from};
  nodes
    sequential
      1 action {fly_helicopter ?h ?from ?to}
    end_sequential;
  conditions
    only_use_if {evac_destination ?to},
    achieve {location ?h} = ?from at begin_of self;
  use ?h across self;
end_schema;

schema evac_injured_gt;
  vars ?from = ?{type city},
       ?to   = ?{type city},
       ?gt   = ?{type ground_transport};
  expands {evacuate injured from ?from using a GT};
  only_use_for_effects {evacuated injured from ?from};
  nodes
    sequential
      1 action {drive_gt ?gt ?from ?to}
    end_sequential;
  conditions
    only_use_if {evac_destination ?to},
    achieve {location ?gt} = ?from at begin_of self;
  use ?gt across self;
end_schema;

;;; Take a medical team ...

schema take_med_team_h;
  vars ?from = ?{type city},
       ?to   = ?{type city},
       ?h    = ?{type helicopter};
  expands {take a medical team to ?to using a helicopter};
  only_use_for_effects {medical_team_in ?to};
  nodes
    sequential
      1 action {fly_helicopter ?h ?from ?to}
    end_sequential;
  conditions
    only_use_if {medical_team_base ?from},
    achieve {location ?h} = ?from;
  use ?h across self;
end_schema;

schema take_med_team_gt;
  vars ?from = ?{type city},
       ?to   = ?{type city},
       ?gt   = ?{type ground_transport};
  expands {take a medical team to ?to using a GT};
  only_use_for_effects {medical_team_in ?to};
  nodes
    sequential
      1 action {drive_gt ?gt ?from ?to}
    end_sequential;
  conditions
    only_use_if {medical_team_base ?from},
    achieve {location ?gt} = ?from;
  use ?gt across self;
end_schema;


;;;; Helicopters

;;; /\/: "achieve after" would let the move_helicopter be inside the
;;; top-level schema's "use" constraint's range; then it wouldn't
;;; need its own "use" constraint, and we wouldn't need two schemas.

schema move_helicopter;
  vars ?from = ?{type city},
       ?to   = ?{type city},
       ?h    = ?{type helicopter};
  expands {move_helicopter ?h ?from ?to};
  only_use_for_effects {location ?h} = ?to;
  conditions
    unsupervised {location ?h} = ?from;
  use ?h across self;  
  time_windows duration self = 1 hours;
end_schema;

schema fly_helicopter;
  vars ?from = ?{type city},
       ?to   = ?{type city},
       ?h    = ?{type helicopter};
  expands {fly_helicopter ?h ?from ?to};
  effects {location ?h} = ?to;
  time_windows duration self = 1 hours;
end_schema;


;;;; Ground transports

schema move_gt;
  vars ?from = ?{type city},
       ?to   = ?{type city},
       ?gt   = ?{type ground_transport};
  expands {move_gt ?gt ?from ?to};
  only_use_for_effects {location ?gt} = ?to;
  conditions
    unsupervised {location ?gt} = ?from;
  use ?gt across self;
  time_windows duration self = 2 hours;
end_schema;

schema drive_gt;
  vars ?from = ?{type city},
       ?to   = ?{type city},
       ?gt   = ?{type ground_transport};
  expands {drive_gt ?gt ?from ?to};
  effects {location ?gt} = ?to;
  time_windows duration self = 2 hours;
end_schema;


;;; End
