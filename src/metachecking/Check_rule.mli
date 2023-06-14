(* will populate Semgrep_error_code.errors *)
val check : Rule.t -> Semgrep_error_code.error list

(* to test -check_rules *)
val run_checks :
  Runner_config.t ->
  (Fpath.t -> Rule.t list) ->
  Fpath.t (* metachecks *) ->
  Fpath.t list (* rules *) ->
  Semgrep_error_code.error list

(* -check_rules *)
val check_files :
  (unit -> Runner_config.t) -> (Fpath.t -> Rule.t list) -> Fpath.t list -> unit

(* -stat_rules *)
val stat_files : (Fpath.t -> Rule.t list) -> Fpath.t list -> unit
