val just_parse_with_lang :
  Lang.t -> Common.filename -> Parse_target.parsing_result

(* used by Parse_pattern_bis *)
val lang_to_python_parsing_mode : Lang.t -> Parse_python.parsing_mode
