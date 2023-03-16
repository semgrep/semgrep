val is_js_script : Common.filename -> bool

val find_source_files_of_dir_or_files :
  ?include_scripts:bool -> Fpath.t -> Fpath.t list

val ii_of_any : Ast_js.any -> Parse_info.t list
