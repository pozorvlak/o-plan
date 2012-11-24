;;; File: london-underground.tf
;;; Contains: TF for the O-Plan London Underground Web demo
;;; Created: Jeff Dalton, 29 March 1995
;;; Updated: Thu Dec  5 02:24:49 1996 by Jeff Dalton

types 
  station =
   (aldwych baker_street bank bond_street cannon_street charing_cross
    earls_court elephant_and_castle embankment euston gatwick
    gloucester_road green_park heathrow holborn kennington
    kings_cross leicester_square liverpool_street marylebone
    mile_end monument moorgate neasden notting_hill_gate
    oxford_circus paddington piccadilly_circus south_kensington
    stratford tottenham_court_road tower_hill victoria
    warren_street waterloo),

  line =
   (bakerloo central circle district district_west docklands_light_railway
    escalator_between_bank_and_monument gatwick_express jubilee
    metropolitan northern_city northern_west piccadilly
    piccadilly_aldwych victoria waterloo_and_city);

initially {visited_station ??} = false,
          {used_line ??} = false;


;;; Sample tasks

task bank_to_charing_cross;
  nodes 1 start,
        2 finish,
        3 action {from bank};
  orderings 1 ---> 3, 3 ---> 2;
  effects {goal} = charing_cross at 1;
end_task;

task kings_cross_to_south_kensington;
  ;;; Just take the Piccadilly line.
  ;;; Might take the circle line.
  nodes 1 start,
        2 finish,
        3 action {from kings_cross};
  orderings 1 ---> 3, 3 ---> 2;
  effects {goal} = south_kensington at 1;
end_task;

task kings_cross_to_heathrow;
  ;;; Just take the Piccadilly line.
  nodes 1 start,
        2 finish,
        3 action {from kings_cross};
  orderings 1 ---> 3, 3 ---> 2;
  effects {goal} = heathrow at 1;
end_task;

task heathrow_to_neasden;
  nodes 1 start,
        2 finish,
        3 action {from heathrow};
  orderings 1 ---> 3, 3 ---> 2;
  effects {goal} = neasden at 1;
end_task;

task heathrow_to_moorgate;
  nodes 1 start,
        2 finish,
        3 action {from heathrow};
  orderings 1 ---> 3, 3 ---> 2;
  effects {goal} = moorgate at 1;
end_task;


;;; The final step

schema same_line;
  vars ?from = ?{type station},
       ?goal = ?{type station},
       ?line = ?{type line};
  expands {from ?from};
  nodes 1 action {take ?line to ?goal};
  conditions
     only_use_if {goal} = ?goal,
     compute {shared_line ?from ?goal} = ?line;
end_schema;


;;; A primitive

schema take_line_to_station;
  vars ?line = ?{type line},
       ?to   = ?{type station};
  expands {take ?line to ?to};
end_schema;


;;; One schema per line

meta_schema line_schema;
  instantiates {line_schema ?line};
  vars ?from = ?{type station},
       ?goal = ?{type station},
       ?to   = ?{type station},
       ?via  = ?{type line};
  expands {from ?from};
  nodes
     sequential
        1 action {take ?via to ?to},
        2 action {from ?to}
     end_sequential;
  conditions
     only_use_if {goal} = ?goal,
     compute {shared_line ?from ?goal} = nil,
     compute {transfer_station ?from ?line} = {?to ?via},
     only_use_if {visited_station ?to} = false,
     only_use_if {used_line ?via} = false;
  effects {visited_station ?to} = true at begin_of self,
          {used_line ?via} = true at begin_of self;
end_meta_schema;

schema bakerloo;
  instance_of {line_schema bakerloo};
end_schema;

schema central;
  instance_of {line_schema central};
end_schema;

schema circle;
  instance_of {line_schema circle};
end_schema;

schema district;
  instance_of {line_schema district};
end_schema;

schema district_west;
  instance_of {line_schema district_west};
end_schema;

schema docklands_light_railway;
  instance_of {line_schema docklands_light_railway};
end_schema;

schema escalator_between_bank_and_monument;
  instance_of {line_schema escalator_between_bank_and_monument};
end_schema;

schema gatwick_express;
  instance_of {line_schema gatwick_express};
end_schema;

schema jubilee;
  instance_of {line_schema jubilee};
end_schema;

schema metropolitan;
  instance_of {line_schema metropolitan};
end_schema;

schema northern_city;
  instance_of {line_schema northern_city};
end_schema;

schema northern_west;
  instance_of {line_schema northern_west};
end_schema;

schema piccadilly;
  instance_of {line_schema piccadilly};
end_schema;

schema piccadilly_aldwych;
  instance_of {line_schema piccadilly_aldwych};
end_schema;

schema victoria;
  instance_of {line_schema victoria};
end_schema;

schema waterloo_and_city;
  instance_of {line_schema waterloo_and_city};
end_schema;


;;; Support code

language lisp;

  (defun shared_line (station-1 station-2)
    (first
      (intersection
        (station-lines station-1)
        (station-lines station-2))))

  (defun transfer_station (at-station desired-line) ; -> (to-station via-line)
    (dolist (via-line (station-lines at-station))
      (let ((transfer-stations
             (intersection (line-stations via-line)
                           (line-stations desired-line))))
        (when transfer-stations
          (return 
            (list (first transfer-stations)
                  via-line))))))

end_language;
