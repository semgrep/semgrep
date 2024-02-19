val just_parse_with_lang : Lang.t -> Fpath.t -> Parsing_result2.t

(* used by Parse_pattern2 *)
val lang_to_python_parsing_mode : Lang.t -> Parse_python.parsing_mode
