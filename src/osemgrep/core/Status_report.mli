val pp_status :
  num_rules:int ->
  num_targets:int ->
  respect_git_ignore:bool ->
  Lang_job.t list ->
  Format.formatter ->
  unit
