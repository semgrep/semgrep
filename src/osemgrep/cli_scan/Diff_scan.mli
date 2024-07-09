val scan_baseline :
  < Cap.chdir ; Cap.tmp > ->
  Scan_CLI.conf ->
  string (* baseline commit *) ->
  Fpath.t list ->
  Rule.rules ->
  Profiler.t ->
  (Fpath.t list ->
  ?diff_config:Differential_scan_config.t ->
  Rule.rules ->
  unit ->
  Core_result.result_or_exn) ->
  Core_result.result_or_exn
