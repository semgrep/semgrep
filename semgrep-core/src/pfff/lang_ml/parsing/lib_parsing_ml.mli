
val find_source_files_of_dir_or_files:
  Common.path list -> Common.filename list

val find_ml_files_of_dir_or_files:
  Common.path list -> Common.filename list
val find_cmt_files_of_dir_or_files:
  Common.path list -> Common.filename list

(* use generc AST if you need to get ii_of_any
   val ii_of_any: Ast_ml.any -> Parse_info.t list
*)
