val pp_targets_debug :
  Format.formatter ->
  Scanning_root.t list
  * Semgrep_output_v1_t.skipped_target list
  * Fpath.t list (* final targets *) ->
  unit
