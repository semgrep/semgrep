val pp_status :
  num_rules:int ->
  num_targets:int ->
  respect_gitignore:bool ->
  Lang_job.t list ->
  Format.formatter ->
  unit
