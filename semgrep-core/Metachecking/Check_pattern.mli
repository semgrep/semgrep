(*s: semgrep/parsing/Check_pattern.mli *)

(*s: signature [[Check_semgrep.check_pattern]] *)
(** Check semgrep patterns for potential issues. Will raise exn if issue. *)
val check : Lang.t -> AST_generic.any -> unit
(*e: signature [[Check_semgrep.check_pattern]] *)
(*e: semgrep/parsing/Check_pattern.mli *)
