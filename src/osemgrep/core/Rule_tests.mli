val is_config_fixtest_suffix : Fpath.t -> bool
val is_config_test_suffix : Fpath.t -> bool
val is_config_suffix : Fpath.t -> bool
val get_config_filenames : Fpath.t -> Fpath.t list

val get_config_test_filenames :
  original_config:Fpath.t ->
  configs:Fpath.t list ->
  original_target:Fpath.t ->
  (Fpath.t * Fpath.t list) list
