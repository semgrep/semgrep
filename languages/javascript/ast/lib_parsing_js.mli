val is_js_script : Fpath.t -> bool

val find_source_files_of_dir_or_files :
  ?include_scripts:bool -> Fpath.t list -> Fpath.t list

val ii_of_any : Ast_js.any -> Tok.t list
