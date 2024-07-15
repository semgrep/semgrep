val check : Rule.t -> Core_error.t list

(* to test -check_rules *)
val run_checks :
  < Cap.tmp > ->
  Core_scan_config.t ->
  (Fpath.t -> (Rule.t list, Rule.error) Result.t) ->
  Fpath.t (* metachecks *) ->
  Fpath.t list (* rules *) ->
  Core_error.t list

(* -check_rules *)
val check_files :
  < Cap.stdout ; Cap.tmp > ->
  (unit -> Core_scan_config.t) ->
  (Fpath.t -> (Rule.t list, Rule.error) Result.t) ->
  Fpath.t list ->
  unit

(* -stat_rules *)
val stat_files :
  < Cap.stdout > ->
  (Fpath.t -> (Rule.t list, Rule.error) Result.t) ->
  Fpath.t list ->
  unit
