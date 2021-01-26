(*s: pfff/lang_GENERIC/parsing/Parse_generic.mli *)

(*s: signature [[Parse_generic.parse_with_lang]] *)
val parse_with_lang: Lang.t -> Common.filename ->
  AST.program * Parse_info.parsing_stat
(*e: signature [[Parse_generic.parse_with_lang]] *)

(*s: signature [[Parse_generic.parse_program]] *)
(* infer the language from the filename *)
val parse_program: Common.filename -> AST.program
(*e: signature [[Parse_generic.parse_program]] *)

(*s: signature [[Parse_generic.parse_pattern]] *)
(* for sgrep *)
val parse_pattern: Lang.t -> string -> AST_generic.any
(*e: signature [[Parse_generic.parse_pattern]] *)
(*e: pfff/lang_GENERIC/parsing/Parse_generic.mli *)
