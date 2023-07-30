open Lsp
open Types

type rule_cache = { mutable rules : Rule.t list; lock : Lwt_mutex.t }
(** Cache of active rules. Protected by mutex as [cache_rules] below can be called asynchronously,
    and so this cache needs to be safe *)

type t = {
  capabilities : ServerCapabilities.t;
  incoming : Lwt_io.input_channel;
  outgoing : Lwt_io.output_channel;
  workspace_folders : Fpath.t list;
  documents : (Fpath.t, Semgrep_output_v1_t.cli_match list) Hashtbl.t;
  cached_rules : rule_cache;
  user_settings : UserSettings.t;
}

val create : ServerCapabilities.t -> t
(** [create capabilities] creates a [Session.t] given server capabilities *)

val cache_rules : t -> unit Lwt.t
(** [cache_rules t] caches the rules for the session. Fetches rules from any configured source
    as in [t.user_settings], and CI if an api token is available. This is an asynchronous operation,
    and so the rules are stored in a [rule_cache] *)

val targets : t -> Fpath.t list
(** [targets t] returns the list of targets for the session. This is a list of files in
    workspace folders, with includes and excludes in [t.user_settings], and git
    status taken into account if [t.user_settings.only_git_dirty] is set *)

val runner_conf : t -> Core_runner.conf
(** [runner_conf t] returns the configuration for the runner. This includes scan settings from
    [t.user_settings] *)

val scanned_files : t -> Fpath.t list
(** [scanned_files t] returns the list of files that have been scanned in the session *)

val record_results :
  t -> Semgrep_output_v1_t.cli_match list -> Fpath.t list -> unit
(** [record_results t results files] records the results of a scan in the session. This is
    used when generating code actions (such as autofix) *)

val update_workspace_folders :
  ?added:Fpath.t list -> ?removed:Fpath.t list -> t -> t
(** [update_workspace_folders ?added ?removed t] updates the workspace folders in the session.
    This is used when the client sends a [workspace/didChangeWorkspaceFolders] notification *)
