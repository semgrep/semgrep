open Lsp
open Types

type session_cache = {
  mutable rules : Rule.t list;
      (* Rules can take a long time to fetch + load, so we want to minimize it *)
  mutable skipped_fingerprints : string list;
  (* Skipped fingerprints need to be fetched from the app, so we only want to do this every so often.
   * These come from the same place ci rules do, so we fetch them at the same time as the above rules
   *)
  lock : Lwt_mutex.t;
}
(** Cache of rules that will be run, and skipped fingerprints. Protected by mutex as [cache_session] below
  * can be called asynchronously, and so this cache needs to be safe
  *)

type t = {
  capabilities : ServerCapabilities.t;
  incoming : Lwt_io.input_channel;
  outgoing : Lwt_io.output_channel;
  workspace_folders : Fpath.t list;
  cached_scans : (Fpath.t, Semgrep_output_v1_t.cli_match list) Hashtbl.t;
  cached_session : session_cache;
  user_settings : User_settings.t;
  is_intellij : bool;
}

val create : ServerCapabilities.t -> t
(** [create capabilities] creates a [Session.t] given server capabilities *)

val cache_session : t -> unit Lwt.t
(** [cache_session t] caches the rules and skipped fingerprints for the session. Fetches rules from any configured source
    as in [t.user_settings], and CI if an api token is available. This is an asynchronous operation,
    and so the rules are stored in a [session_cache] *)

val targets : t -> Fpath.t list
(** [targets t] returns the list of targets for the session. This is a list of files in
    workspace folders, with includes and excludes in [t.user_settings], and git
    status taken into account if [t.user_settings.only_git_dirty] is set *)

val runner_conf : t -> Core_runner.conf
(** [runner_conf t] returns the configuration for the runner. This includes scan settings from
    [t.user_settings] *)

val scanned_files : t -> Fpath.t list
(** [scanned_files t] returns the list of files that have been scanned in the session *)

val previous_scan_of_file :
  t -> Fpath.t -> Semgrep_output_v1_t.cli_match list option
(** [previous_scan_of_file session path] returns the last results of a scan on a file if it exists *)

val record_results :
  t -> Semgrep_output_v1_t.cli_match list -> Fpath.t list -> unit
(** [record_results t results files] records the results of a scan in the session. This is
    used when generating code actions (such as autofix) *)

val update_workspace_folders :
  ?added:Fpath.t list -> ?removed:Fpath.t list -> t -> t
(** [update_workspace_folders ?added ?removed t] updates the workspace folders in the session.
    This is used when the client sends a [workspace/didChangeWorkspaceFolders] notification *)
