(* will populate Semgrep_error_code.errors *)
val check : Rule.t -> Semgrep_error_code.error list

(* to test -check_rules *)
val run_checks :
  Runner_config.t ->
  (Common.filename -> Rule.t list) ->
  Common.filename (* metachecks *) ->
  Common.filename list (* rules *) ->
  Semgrep_error_code.error list

(* -check_rules *)
val check_files :
  (unit -> Runner_config.t) ->
  (Common.filename -> Rule.t list) ->
  Common.filename list ->
  unit

(* -stat_rules *)
val stat_files :
  (Common.filename -> Rule.t list) -> Common.filename list -> unit
