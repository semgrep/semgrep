type path = string

val get_targets :
  includes:string list ->
  excludes:string list ->
  max_target_bytes:int ->
  respect_git_ignore:bool ->
  path list ->
  path list * Output_from_core_t.skipped_target list
