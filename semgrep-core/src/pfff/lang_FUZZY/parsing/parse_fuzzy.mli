
(* mostly for fuzzy sgrep/spatch *)
val parse_with_lang: Lang_fuzzy.t -> Common.filename -> Ast_fuzzy.trees

val parse: Common.filename -> Ast_fuzzy.trees

val parse_and_tokens_with_lang:
  Lang_fuzzy.t -> Common.filename ->
  Ast_fuzzy.trees * (Parse_info.token_kind * Parse_info.t) list

val parse_pattern: Lang_fuzzy.t -> string -> Ast_fuzzy.trees
