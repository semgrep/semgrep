(* will populate Semgrep_error_code.errors *)
val check : Rule.t -> Semgrep_error_code.error list

(* -check_rules *)
val check_files :
  (Common.filename -> Rule.t list) -> Common.filename list -> unit

(* -stat_rules *)
val stat_files :
  (Common.filename -> Rule.t list) -> Common.filename list -> unit
