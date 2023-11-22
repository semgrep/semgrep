val pp_summary :
  respect_gitignore:bool ->
  maturity:Maturity.t ->
  max_target_bytes:int ->
  skipped_groups:Skipped_report.skipped_targets_grouped ->
  Format.formatter ->
  unit ->
  unit
