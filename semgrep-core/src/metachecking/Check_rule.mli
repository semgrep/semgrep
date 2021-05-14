(*s: semgrep/metachecking/Check_rule.mli *)

(* will populate Error_code.errors *)
val check : Rule.t -> unit

val check_files :
  (Common.filename -> Rule.t list) -> Common.filename list -> unit

val stat_files :
  (Common.filename -> Rule.t list) -> Common.filename list -> unit

(*e: semgrep/metachecking/Check_rule.mli *)
