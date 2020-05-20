(*s: semgrep/parsing/Check_semgrep.mli *)

(*s: signature [[Check_semgrep.check_pattern]] *)
(** Check sgrep patterns for potential issues. *)
val check_pattern : Lang.t -> AST_generic.any -> AST_generic.any
(*e: signature [[Check_semgrep.check_pattern]] *)

(*s: signature [[Check_semgrep.parse_check_pattern]] *)
(** Parse an sgrep pattern and then check it. *)
val parse_check_pattern : Lang.t -> string -> AST_generic.any
(*e: signature [[Check_semgrep.parse_check_pattern]] *)
(*e: semgrep/parsing/Check_semgrep.mli *)
