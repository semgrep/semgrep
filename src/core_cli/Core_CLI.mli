(* entry point of semgrep-core *)
val main : Cap.all_caps -> string array -> unit

(* internals used also in semgrep-core-proprietary *)
val lang : Analyzer.t option ref
val ncores : int ref
val debug : bool ref
val profile : bool ref
val log_to_file : Fpath.t option ref
val trace : bool ref
val env_extra : string

(* compute Core_scan_config.t given command-line flags *)
val mk_config : unit -> Core_scan_config.t

val output_core_results :
  < Cap.stdout ; Cap.stderr ; Cap.exit > ->
  Core_result.result_or_exn ->
  Core_scan_config.t ->
  unit
(** [output_core_results] takes the results of a core scan and
    format the results on stdout either in a JSON or Textual format
    (depending on the value in config.output_format)
*)

val options :
  < Cap.exec ; Cap.exit ; Cap.stdout ; Cap.tmp ; .. > ->
  (unit -> Arg_.action_spec list) ->
  Arg_.cmdline_options

val action : string ref
val all_actions : Cap.all_caps -> unit -> Arg_.action_spec list
val register_exception_printers : unit -> unit

(* this can raise exn; useful in test context *)
val main_exn : Cap.all_caps -> string array -> unit
