type diff_scan_func =
  ?diff_config:Differential_scan_config.t ->
  Fpath.t list ->
  Rule.rules ->
  Core_result.result_or_exn

val scan_baseline :
  < Cap.chdir ; Cap.tmp > ->
  Scan_CLI.conf ->
  Profiler.t ->
  string (* baseline commit *) ->
  Fpath.t list ->
  Rule.rules ->
  diff_scan_func ->
  Core_result.result_or_exn
