(*s: semgrep/metachecking/Check_rule.mli *)

(* will populate Error_code.errors *)
val check : Rule.t -> Error_code.error list

(* -check_rules *)
val check_files :
  (Common.filename -> Rule.t list) -> Common.filename list -> unit

(* -stat_rules *)
val stat_files :
  (Common.filename -> Rule.t list) -> Common.filename list -> unit

(*e: semgrep/metachecking/Check_rule.mli *)
