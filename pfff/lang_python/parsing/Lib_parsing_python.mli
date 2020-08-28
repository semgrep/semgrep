(*s: pfff/lang_python/parsing/Lib_parsing_python.mli *)
(*s: signature [[Lib_parsing_python.find_source_files_of_dir_or_files]] *)
val find_source_files_of_dir_or_files: 
  Common.path list -> Common.filename list
(*e: signature [[Lib_parsing_python.find_source_files_of_dir_or_files]] *)

(*s: signature [[Lib_parsing_python.ii_of_any]] *)
val ii_of_any: AST_python.any -> Parse_info.t list
(*e: signature [[Lib_parsing_python.ii_of_any]] *)
(*e: pfff/lang_python/parsing/Lib_parsing_python.mli *)
