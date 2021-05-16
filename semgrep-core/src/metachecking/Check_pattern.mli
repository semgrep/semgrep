(*s: semgrep/parsing/Check_pattern.mli *)

(*s: signature [[Check_semgrep.check_pattern]] *)

val check : Lang.t -> AST_generic.any -> unit
(** Check semgrep patterns for potential issues. Will raise exn if issue. *)

(*e: signature [[Check_semgrep.check_pattern]] *)
(*e: semgrep/parsing/Check_pattern.mli *)
