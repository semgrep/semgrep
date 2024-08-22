(* entry point of semgrep-core *)
val main : Cap.all_caps -> string array -> unit

(* internals used also in semgrep-core-proprietary *)
val lang : Xlang.t option ref
val ncores : int ref
val debug : bool ref
val profile : bool ref
val log_to_file : Fpath.t option ref
val env_extra : string
val mk_config : unit -> Core_scan_config.t

val options :
  < Cap.exec ; Cap.exit ; Cap.stdout ; Cap.tmp ; .. > ->
  (unit -> Arg_.action_spec list) ->
  Arg_.cmdline_options

val action : string ref
val all_actions : Cap.all_caps -> unit -> Arg_.action_spec list
val register_exception_printers : unit -> unit

(* DO NOT USE! *)
val mk_config_DEPRECATED : unit -> Core_scan_config.t
