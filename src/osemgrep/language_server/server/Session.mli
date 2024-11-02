open Lsp
open Types

val scan_config_parser_ref : (string -> Semgrep_output_v1_t.scan_config) ref
(** [scan_config_parser_ref] is a reference to a function that parses a scan
    config from a string *)

(* =~ Core_scan.caps + random + network + tmp *)
type caps =
  < Cap.random
  ; Cap.network
  ; Cap.tmp
  ; Cap.fork
  ; Cap.time_limit
  ; Cap.memory_limit >

type session_cache = {
  mutable rules : Rule.t list;
  (* Rules can take a long time to fetch + load, so we want to minimize it *)
  mutable skipped_app_fingerprints : string list;
  (* Skipped fingerprints need to be fetched from the app, so we only want to do
     this every so often. * These come from the same place ci rules do, so we
     fetch them at the same time as the above rules *)
  mutable open_documents : Fpath.t list;
  (* Files open in the session *)
  mutable initialized : bool;
      (* keep track of if the cache is initialized so we can let users know if they
         try to do something that need the rules downloaded like a full workspace
         scan *)
  mutable deployment_id : int option;
  (* Deployment id for the session *)
  lock : Lwt_mutex.t;
}
(** Cache of rules that will be run, and skipped fingerprints. Protected by
    mutex as [cache_session] below * can be called asynchronously, and so this
    cache needs to be safe
*)

type t = {
  capabilities : ServerCapabilities.t;
  workspace_folders : Fpath.t list;
  cached_workspace_targets : (Fpath.t, Fpath.t list) Hashtbl.t;
  cached_scans : (Fpath.t, Semgrep_output_v1_t.cli_match list) Hashtbl.t;
  cached_session : session_cache;
  skipped_local_fingerprints : string list;
  user_settings : User_settings.t;
  search_config : Search_config.t option;
  metrics : LS_metrics.t;
  is_intellij : bool;
  caps : caps; [@opaque]
}

val show : t -> string
(** [show t] returns a string representation of the session *)

val create : caps -> ServerCapabilities.t -> t
(** [create caps capabilities] creates a [Session.t] given server capabilities *)

val send_metrics :
  ?core_time:Semgrep_output_v1_j.profile ->
  ?profiler:Profiler.t ->
  ?cli_output:Semgrep_output_v1_j.cli_output ->
  t ->
  unit
(** [send_metrics t] sends metrics to the server *)

val save_local_skipped_fingerprints : t -> unit
(** [save_local_skipped_fingerprints t] saves the skipped fingerprints in the session to disk *)

val load_local_skipped_fingerprints : t -> t
(** [load_local_skipped_fingerprints t] loads the skipped fingerprints in the session from disk *)

val cache_session : t -> unit Lwt.t
(** [cache_session t] caches the rules and skipped fingerprints for the session. Fetches rules from any configured source
    as in [t.user_settings], and CI if an api token is available. This is an asynchronous operation,
    and so the rules are stored in a [session_cache] *)

val cache_workspace_targets : t -> unit
(** [cache_workspace_targets t] caches the targets for the session. This is a list of files in
    workspace folders, with includes and excludes in [t.user_settings], and git
    status taken into account if [t.user_settings.only_git_dirty] is set *)

val targets : t -> Fpath.t list
(** [targets t] returns the list of targets for the session. This is a list of files in
    workspace folders, with includes and excludes in [t.user_settings], and git
    status taken into account if [t.user_settings.only_git_dirty] is set *)

val runner_conf : t -> Core_runner.conf
(** [runner_conf t] returns the configuration for the runner. This includes scan settings from
    [t.user_settings] *)

val skipped_fingerprints : t -> string list
(** [skipped_fingerprints t] returns the list of skipped fingerprints for the session. This is
    a list of fingerprints in [t.skipped_local_fingerprints] and [t.cached_session.skipped_fingerprints] *)

val scanned_files : t -> Fpath.t list
(** [scanned_files t] returns the list of files that have been scanned in the session *)

val previous_scan_of_file :
  t -> Fpath.t -> Semgrep_output_v1_t.cli_match list option
(** [previous_scan_of_file session path] returns the last results of a scan on a file if it exists *)

val add_skipped_fingerprint : t -> string -> t
(** [add_skipped_fingerprint t fingerprint] adds a fingerprint to the list of skipped fingerprints in the session *)

val add_open_document : t -> Fpath.t -> unit Lwt.t
(** [add_open_document t path] adds a file to the list of open documents in the session *)

val remove_open_document : t -> Fpath.t -> unit Lwt.t
(** [remove_open_document t path] removes a file from the list of open documents in the session *)

val remove_open_documents : t -> Fpath.t list -> unit Lwt.t
(** [remove_open_documents t path] removes all files from the list of open documents in the session *)

val record_results :
  t -> Semgrep_output_v1_t.cli_match list -> Fpath.t list -> unit
(** [record_results t results files] records the results of a scan in the session. This is
    used when generating code actions (such as autofix) *)

val update_workspace_folders :
  ?added:Fpath.t list -> ?removed:Fpath.t list -> t -> t
(** [update_workspace_folders ?added ?removed t] updates the workspace folders in the session.
    This is used when the client sends a [workspace/didChangeWorkspaceFolders] notification *)
