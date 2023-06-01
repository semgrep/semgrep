open Lsp
open Types

type t = {
  capabilities : ServerCapabilities.t;
  incoming : Lwt_io.input_channel;
  outgoing : Lwt_io.output_channel;
  config : Runner_config.t;
  root : string;
  cached_rules : Runner_config.rule_source option;
  documents : (string, Processed_run.t list) Hashtbl.t;
  only_git_dirty : bool;  (** Only scan files that are dirty in git *)
}

val create : ServerCapabilities.t -> Runner_config.t -> t
(** Create a session given server capabilities and a config *)

val targets : t -> Input_to_core_t.targets
(** Get the targets for a given session. Filters based on git status, and targets provided by the python wrapper *)

val load_rules : t -> t
(** Load rules from provided config, and save them in cached_rules *)

val hrules : t -> Rule.hrules
(** Get the hashtable of cached rules *)

val record_results : t -> Processed_run.t list -> Fpath.t list -> unit
(** Saves scan results for a file for later, such as when we need to generate code actions*)

val scanned_files : t -> string list
(** Get what files we have scanned so far*)
