
val tags_of_files_or_dirs:
  ?verbose:bool ->
  Common.path list ->
  (Common.filename * Tags_file.tag list) list
