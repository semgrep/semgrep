val check : Rule.t -> Core_error.t list

(* to test -check_rules *)
val run_checks :
  < Core_scan.caps ; Cap.readdir ; .. > ->
  Fpath.t (* metachecks *) ->
  Fpath.t list (* rules *) ->
  Core_error.t list

(* -check_rules *)
val check_files :
  < Cap.stdout ; Core_scan.caps ; Cap.readdir ; .. > ->
  Core_scan_config.output_format ->
  Fpath.t list ->
  unit

(* -stat_rules *)
val stat_files : < Cap.stdout ; Cap.readdir ; .. > -> Fpath.t list -> unit
