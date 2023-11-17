val pp_summary :
  Format.formatter ->
  bool
  * Maturity.t
  * int
  * Fpath.t list
  * Skipped_report.skipped_targets_grouped ->
  unit
