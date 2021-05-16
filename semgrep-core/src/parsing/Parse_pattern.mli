(*s: semgrep/parsing/Parse_pattern.mli *)

val parse_pattern : Lang.t -> ?print_errors:bool -> string -> Pattern.t

val dump_tree_sitter_pattern_cst : Lang.t -> Common.filename -> unit

(*e: semgrep/parsing/Parse_pattern.mli *)
