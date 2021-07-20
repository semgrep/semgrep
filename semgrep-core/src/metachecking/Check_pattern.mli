(*s: semgrep/parsing/Check_pattern.mli *)

(*s: signature [[Check_semgrep.check_pattern]] *)

(* Check semgrep patterns for potential issues. *)
val check : Lang.t -> Pattern.t -> (* TODO Error_code.error list *) unit

(*e: signature [[Check_semgrep.check_pattern]] *)
(*e: semgrep/parsing/Check_pattern.mli *)
