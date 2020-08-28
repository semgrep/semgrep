
val find_source_files_of_dir_or_files: 
  Common.path list -> Common.filename list

val find_ml_files_of_dir_or_files: 
  Common.path list -> Common.filename list
val find_cmt_files_of_dir_or_files: 
  Common.path list -> Common.filename list

val ii_of_any: Cst_ml.any -> Parse_info.t list
