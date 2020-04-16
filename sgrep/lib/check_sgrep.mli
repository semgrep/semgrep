
(** Check sgrep patterns for potential issues. *)
val check_pattern : Lang.t -> Ast_generic.any -> Ast_generic.any

(** Parse an sgrep pattern and then check it. *)
val parse_check_pattern : Lang.t -> string -> Ast_generic.any
