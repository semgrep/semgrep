open Lsp
open Types

type t = {
  capabilities : ServerCapabilities.t;
  incoming : Lwt_io.input_channel;
  outgoing : Lwt_io.output_channel;
  config : Runner_config.t; (* ... *)
  root : string;
  cached_rules : Runner_config.rule_source option;
  documents :
    (string, (Semgrep_output_v1_t.core_match * Rule.rule) list) Hashtbl.t;
  mutable next_id : int;
  only_git_dirty : bool;
}

val create : ServerCapabilities.t -> Runner_config.t -> t
val targets : t -> Input_to_core_t.targets Lwt.t
val load_rules : t -> t
val hrules : t -> Rule.hrules
val record_results : t -> Reporting.t list -> string list -> unit
val scanned_files : t -> string list
