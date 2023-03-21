val find_source_files_of_dir_or_files : Fpath.t list -> Fpath.t list
val find_ml_files_of_dir_or_files : Fpath.t list -> Fpath.t list
val find_cmt_files_of_dir_or_files : Fpath.t list -> Fpath.t list

(* use generc AST if you need to get ii_of_any
   val ii_of_any: Ast_ml.any -> Parse_info.t list
*)
