(* will populate Core_error.errors *)
val check : Rule.t -> Core_error.t list

(* to test -check_rules *)
val run_checks :
  Core_scan_config.t ->
  (Fpath.t -> Rule.t list) ->
  Fpath.t (* metachecks *) ->
  Fpath.t list (* rules *) ->
  Core_error.t list

(* -check_rules *)
val check_files :
  (unit -> Core_scan_config.t) ->
  (Fpath.t -> Rule.t list) ->
  Fpath.t list ->
  unit

(* -stat_rules *)
val stat_files : (Fpath.t -> Rule.t list) -> Fpath.t list -> unit
