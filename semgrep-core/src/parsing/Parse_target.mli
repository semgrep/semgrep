(*s: semgrep/parsing/Parse_target.mli *)

type parsing_result = {
  ast : AST_generic.program;
  errors : Error_code.error list;
  stat : Parse_info.parsing_stat;
}

(* This uses either the pfff or tree-sitter parsers.
 * This also resolve names and propagate constants.
 *)
val parse_and_resolve_name_use_pfff_or_treesitter :
  Lang.t -> Common.filename -> parsing_result

(* used only for testing purpose *)
val just_parse_with_lang : Lang.t -> Common.filename -> parsing_result

val parse_program : Common.filename -> AST_generic.program

(* used by Parse_pattern *)
val lang_to_python_parsing_mode : Lang.t -> Parse_python.parsing_mode

(*e: semgrep/parsing/Parse_target.mli *)
