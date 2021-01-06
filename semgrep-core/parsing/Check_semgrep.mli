(*s: semgrep/parsing/Check_semgrep.mli *)

(*s: signature [[Check_semgrep.check_pattern]] *)
(** Check sgrep patterns for potential issues. Will raise exn if issue. *)
val check_pattern : Lang.t -> AST_generic.any -> unit
(*e: signature [[Check_semgrep.check_pattern]] *)

(*s: signature [[Check_semgrep.parse_check_pattern]] *)
(*e: signature [[Check_semgrep.parse_check_pattern]] *)
(*e: semgrep/parsing/Check_semgrep.mli *)
