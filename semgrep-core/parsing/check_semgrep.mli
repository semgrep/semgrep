(*s: semgrep/parsing/check_semgrep.mli *)

(*s: signature [[Check_semgrep.check_pattern]] *)
(** Check sgrep patterns for potential issues. *)
val check_pattern : Lang.t -> Ast_generic.any -> Ast_generic.any
(*e: signature [[Check_semgrep.check_pattern]] *)

(*s: signature [[Check_semgrep.parse_check_pattern]] *)
(** Parse an sgrep pattern and then check it. *)
val parse_check_pattern : Lang.t -> string -> Ast_generic.any
(*e: signature [[Check_semgrep.parse_check_pattern]] *)
(*e: semgrep/parsing/check_semgrep.mli *)
