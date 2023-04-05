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
    (Uri.t, (Semgrep_output_v1_t.core_match * Rule.rule) list) Hashtbl.t;
  mutable next_id : int;
}

val targets : t -> Input_to_core_t.targets Lwt.t
val load_rules : t -> t
val hrules : t -> Rule.hrules
