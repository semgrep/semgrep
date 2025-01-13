(* Austin Theriault
 *
 * Copyright (C) Semgrep, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
(* Commentary *)
(* Session contains info that would be helpful to persist between each server *)
(* request. I.e. parsed rules, settings, open documents etc. We try and store *)
(* as little as possible in the type, and try to derive as much as possible *)
(* instead.*)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
module Env = Semgrep_envvars
open Lsp
open Types
open Fpath_.Operators
module Out = Semgrep_output_v1_t
module OutJ = Semgrep_output_v1_j

(*****************************************************************************)
(* Refs *)
(*****************************************************************************)

let scan_config_parser_ref = ref OutJ.scan_config_of_string

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* =~ Core_scan.caps + random + network + tmp *)
type caps =
  < Cap.random
  ; Cap.network
  ; Cap.tmp
  ; Cap.fork
  ; Cap.time_limit
  ; Cap.memory_limit >

(* We really don't want mutable state in the server.
   This is the only exception since this stuff requires network requests that
   we want to do asynchronously *)
type session_cache = {
  mutable rules : Rule.t list; [@opaque]
  mutable skipped_app_fingerprints : string list;
  mutable open_documents : Fpath.t list;
  mutable initialized : bool;
  mutable deployment_id : int option;
  lock : Lwt_mutex.t; [@opaque]
}
[@@deriving show]

type t = {
  capabilities : ServerCapabilities.t;
      [@printer
        fun fmt c ->
          Yojson.Safe.pretty_print fmt (ServerCapabilities.yojson_of_t c)]
  workspace_folders : Fpath.t list;
  cached_workspace_targets : (Fpath.t, Fpath.t list) Hashtbl.t; [@opaque]
  cached_scans : (Fpath.t, Out.cli_match list) Hashtbl.t; [@opaque]
  cached_session : session_cache;
  skipped_local_fingerprints : string list;
  user_settings : User_settings.t;
  search_config : Search_config.t option;
  metrics : LS_metrics.t;
  is_intellij : bool;
  caps : caps; [@opaque]
}
[@@deriving show]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let create caps capabilities =
  let cached_session =
    {
      rules = [];
      skipped_app_fingerprints = [];
      lock = Lwt_mutex.create ();
      open_documents = [];
      deployment_id = None;
      initialized = false;
    }
  in
  {
    capabilities;
    workspace_folders = [];
    cached_workspace_targets = Hashtbl.create 10;
    cached_scans = Hashtbl.create 10;
    cached_session;
    skipped_local_fingerprints = [];
    user_settings = User_settings.default;
    search_config = None;
    metrics = LS_metrics.default;
    is_intellij = false;
    caps;
  }

let dirty_paths_of_folder folder =
  let git_repo = Git_wrapper.project_root_for_files_in_dir folder in
  if Option.is_some git_repo then
    let dirty_paths = Git_wrapper.dirty_paths ~cwd:folder () in
    Some (List_.map (fun x -> folder // x) dirty_paths)
  else None

(* TODO: registry caching is not anymore in semgrep-OSS! *)
let decode_rules caps data =
  CapTmp.with_temp_file caps#tmp ~contents:data ~suffix:".json" (fun file ->
      match
        Rule_fetching.load_rules_from_file ~rewrite_rule_ids:false ~origin:App
          caps file
      with
      | Ok res ->
          Logs.app (fun m ->
              m "Loaded %d rules from Semgrep Deployment"
                (List.length res.rules));
          Logs.app (fun m ->
              m "Got %d errors from Semgrep Deployment"
                (List.length res.invalid_rules));
          res
      | Error _err ->
          (* There shouldn't be any errors, because we got these rules from CI. *)
          failwith "impossible: received invalid rules from Deployment")

let get_targets session (root : Fpath.t) =
  let targets_conf =
    User_settings.find_targets_conf_of_t session.user_settings
  in
  let proj_root = Rfpath.of_fpath_exn root in
  let targets, _errors, _skipped_targets =
    Find_targets.get_target_fpaths
      {
        targets_conf with
        force_project_root = Some (Find_targets.Filesystem proj_root);
      }
      [ Scanning_root.of_fpath root ]
  in
  targets

let send_metrics ?core_time ?profiler ?cli_output session =
  if session.metrics.client_metrics.enabled then (
    let settings = Semgrep_settings.load () in
    let api_token = settings.Semgrep_settings.api_token in
    let anonymous_user_id = settings.Semgrep_settings.anonymous_user_id in
    Metrics_.init session.caps ~anonymous_user_id ~ci:false;
    api_token
    |> Option.iter (fun (_token : Auth.token) ->
           Metrics_.g.payload.environment.isAuthenticated <- true);
    Git_wrapper.project_url () |> Option.iter Metrics_.add_project_url_hash;
    cli_output
    |> Option.iter (fun (o : OutJ.cli_output) -> Metrics_.add_errors o.errors);
    profiler |> Option.iter Metrics_.add_profiling;
    Metrics_.add_rules_hashes_and_rules_profiling ?profiling:core_time
      session.cached_session.rules;
    Metrics_.g.payload.extension.machineId <-
      session.metrics.client_metrics.machineId;
    Metrics_.g.payload.extension.isNewAppInstall <-
      Some session.metrics.client_metrics.isNewAppInstall;
    Metrics_.g.payload.extension.sessionId <-
      session.metrics.client_metrics.sessionId;
    Metrics_.g.payload.extension.version <-
      session.metrics.client_metrics.extensionVersion;
    Metrics_.g.payload.extension.ty <-
      Some session.metrics.client_metrics.extensionType;
    Metrics_.g.payload.extension.ignoreCount <-
      Some session.metrics.ignore_count;
    Metrics_.g.payload.extension.autofixCount <-
      Some session.metrics.autofix_count;
    Metrics_.g.payload.environment.deployment_id <-
      session.cached_session.deployment_id;
    Metrics_.prepare_to_send ();
    (* Lwt.async really ok here since we hope metrics send but it doesn't
       impact state at all so *)
    Lwt.dont_wait
      (fun () ->
        (* Don't worry if metrics fail to send, and don't notify user *)
        Semgrep_Metrics.send_async session.caps)
      (fun exn ->
        Logs.err (fun m ->
            m "Failed to send metrics: %s" (Printexc.to_string exn))))

(*****************************************************************************)
(* State getters *)
(*****************************************************************************)

let auth_token () =
  match !Semgrep_envvars.v.app_token with
  | Some token -> Some token
  | None ->
      let settings = Semgrep_settings.load () in
      settings.api_token

let scan_config_of_token caps = function
  | Some token -> (
      let caps = Auth.cap_token_and_network token caps in
      let%lwt config_string =
        Semgrep_App.fetch_scan_config_string_async caps ~sca:false ~dry_run:true
          ~full_scan:true ~repository:""
      in
      match config_string with
      | Ok config_string ->
          (* TODO: Check config string hash, and update rules iff its different,
           * and cache rules in file. See [scan_config_parser_ref] declaration
           * for why we do this
           *)
          let scan_config = !scan_config_parser_ref config_string in
          Lwt.return_some scan_config
      | Error e ->
          Logs.warn (fun m -> m "Failed to fetch scan config: %s" e);
          Lwt.return_none)
  | _ -> Lwt.return_none

let fetch_ci_rules_and_origins caps =
  let token = auth_token () in
  let%lwt scan_config_opt = scan_config_of_token caps token in

  let rules_opt =
    Option.bind scan_config_opt (fun scan_config ->
        Some (decode_rules caps scan_config.rule_config))
  in
  Lwt.return rules_opt

let cache_workspace_targets session =
  let folders = session.workspace_folders in
  let targets = List_.map (fun f -> (f, get_targets session f)) folders in
  List.iter
    (fun (folder, targets) ->
      Hashtbl.replace session.cached_workspace_targets folder targets)
    targets

(* This is dynamic so if the targets file is updated we don't have to restart
 *)
let targets session =
  (* These are "dirty paths" because they may not necessarily be files. They may also be folders.
   *)
  let dirty_paths_by_workspace =
    List_.map (fun f -> (f, dirty_paths_of_folder f)) session.workspace_folders
  in
  let member_folder_dirty_files file folder =
    let dirty_paths_opt = List.assoc folder dirty_paths_by_workspace in
    match dirty_paths_opt with
    | None -> true
    | Some dirty_paths ->
        List.exists
          (fun dirty_path ->
            if Fpath.is_dir_path dirty_path then Fpath.is_prefix dirty_path file
            else file = dirty_path)
          dirty_paths
  in
  let member_workspace_folder file (folder : Fpath.t) =
    Fpath.is_prefix folder file
    && ((not session.user_settings.only_git_dirty)
       || member_folder_dirty_files file folder)
  in
  let member_workspaces t =
    List.exists (fun f -> member_workspace_folder t f) session.workspace_folders
  in
  let workspace_targets f =
    Hashtbl.find_opt session.cached_workspace_targets f
    |> Option.value ~default:[]
  in
  let targets =
    session.workspace_folders |> List.concat_map workspace_targets
  in
  (* Filter targets by if only_git_dirty, if they are a dirty file *)
  targets |> List.filter member_workspaces

let fetch_rules session =
  let%lwt ci_rules =
    if session.user_settings.ci then fetch_ci_rules_and_origins session.caps
    else Lwt.return_none
  in
  let home = !Semgrep_envvars.v.user_home_dir in
  let rules_source =
    session.user_settings.configuration |> List_.map Fpath.v
    |> List_.map Fpath.normalize
    |> List_.map (fun f ->
           let p = Fpath.rem_prefix (Fpath.v "~/") f in
           Option.bind p (fun f -> Some (home // f)) |> Option.value ~default:f)
    |> List_.map Fpath.to_string
  in
  let rules_source =
    if rules_source = [] && ci_rules = None then (
      Logs.debug (fun m -> m "No rules source specified, using auto");
      [ "auto" ])
    else rules_source
  in
  let%lwt rules_and_errors =
    Lwt_list.map_p
      (fun source ->
        let in_docker = !Semgrep_envvars.v.in_docker in
        let config = Rules_config.parse_config_string ~in_docker source in
        (* TODO: registry_caching is not anymore in semgrep-OSS! *)
        Rule_fetching.rules_from_dashdash_config_async
          ~rewrite_rule_ids:true (* default *)
          ~token_opt:(auth_token ()) session.caps config)
      rules_source
  in

  let rules_and_origins, errors =
    let rules_and_origins_nested, errors_nested =
      Common2.unzip rules_and_errors
    in
    (List_.flatten rules_and_origins_nested, List_.flatten errors_nested)
  in

  Logs.info (fun m ->
      m "Got %d errors while refreshing rules in language server"
        (List.length errors));

  let rules_and_origins =
    match ci_rules with
    | Some r -> r :: rules_and_origins
    | None -> rules_and_origins
  in
  let rules, invalid_rules =
    Rule_fetching.partition_rules_and_invalid rules_and_origins
  in
  let rules =
    List_.deduplicate_gen
      ~get_key:(fun r -> Rule_ID.to_string (fst r.Rule.id))
      rules
  in
  let rule_filtering_conf =
    Rule_filtering.
      {
        exclude_rule_ids = [];
        severity = [];
        (* Exclude these as they require the pro engine which we don't support *)
        exclude_products = [ `SCA; `Secrets ];
      }
  in
  let rules, invalid_rules =
    (Rule_filtering.filter_rules rule_filtering_conf rules, invalid_rules)
  in

  Lwt.return (rules, invalid_rules)

let fetch_skipped_app_fingerprints caps =
  (* At some point we should allow users to ignore ids locally *)
  let auth_token = auth_token () in
  let%lwt deployment_opt = scan_config_of_token caps auth_token in
  match deployment_opt with
  | Some deployment -> Lwt.return deployment.triage_ignored_match_based_ids
  | None -> Lwt.return []

(* Useful for when we need to reset diagnostics, such as when changing what
 * rules we've run *)
let scanned_files session =
  (* We can get duplicates apparently *)
  Hashtbl.fold (fun file _ acc -> file :: acc) session.cached_scans []
  |> List.sort_uniq Fpath.compare

let skipped_fingerprints session =
  let skipped_fingerprints =
    session.cached_session.skipped_app_fingerprints
    @ session.skipped_local_fingerprints
  in
  List.sort_uniq String.compare skipped_fingerprints

let runner_conf session =
  User_settings.core_runner_conf_of_t session.user_settings

let previous_scan_of_file session file =
  Hashtbl.find_opt session.cached_scans file

let save_local_skipped_fingerprints session =
  let save_dir =
    !Env.v.user_dot_semgrep_dir / "cache" / "fingerprinted_ignored_findings"
  in
  UFile.make_directories save_dir;
  let save_file_name =
    String.concat "_"
      (List_.map (fun f -> f |> Fpath.basename) session.workspace_folders)
    ^ ".txt"
  in
  let save_file = save_dir / save_file_name in
  let skipped_fingerprints = skipped_fingerprints session in
  let skipped_fingerprints = String.concat "\n" skipped_fingerprints in
  UFile.with_open_out save_file (fun (_pr, chan) ->
      output_string chan skipped_fingerprints)

let load_local_skipped_fingerprints session =
  let save_dir = !Env.v.user_dot_semgrep_dir / "cache" / "fingerprints" in
  let save_file_name =
    String.concat "_"
      (List_.map (fun f -> f |> Fpath.basename) session.workspace_folders)
    ^ ".txt"
  in
  let save_file = save_dir / save_file_name in
  if not (Sys.file_exists !!save_file) then session
  else
    let skipped_local_fingerprints =
      UFile.read_file save_file |> String.split_on_char '\n'
      |> List.filter (fun s -> s <> "")
    in
    { session with skipped_local_fingerprints }

let fetch_deployment_id caps =
  let auth_token = auth_token () in
  match auth_token with
  | Some token -> (
      let caps = Auth.cap_token_and_network token caps in
      let%lwt deployment_opt =
        Semgrep_App.get_deployment_from_token_async caps
      in
      match deployment_opt with
      | Some deployment -> Lwt.return_some deployment.id
      | None -> Lwt.return None)
  | None -> Lwt.return None

(*****************************************************************************)
(* State setters *)
(*****************************************************************************)

let cache_session session =
  let%lwt rules, _ = fetch_rules session in
  let%lwt skipped_app_fingerprints =
    fetch_skipped_app_fingerprints session.caps
  in
  let%lwt deployment_id = fetch_deployment_id session.caps in
  Lwt_mutex.with_lock session.cached_session.lock (fun () ->
      session.cached_session.deployment_id <- deployment_id;
      session.cached_session.rules <- rules;
      session.cached_session.skipped_app_fingerprints <-
        skipped_app_fingerprints;
      session.cached_session.initialized <- true;
      Lwt.return_unit)

let add_skipped_fingerprint session fingerprint =
  {
    session with
    skipped_local_fingerprints =
      fingerprint :: session.skipped_local_fingerprints;
  }

let add_open_document session file =
  Lwt_mutex.with_lock session.cached_session.lock (fun () ->
      session.cached_session.open_documents <-
        file :: session.cached_session.open_documents;
      Lwt.return_unit)

let remove_open_document session file =
  Lwt_mutex.with_lock session.cached_session.lock (fun () ->
      session.cached_session.open_documents <-
        List.filter
          (fun f -> not (Fpath.equal f file))
          session.cached_session.open_documents;
      Lwt.return_unit)

let remove_open_documents session files =
  Lwt_mutex.with_lock session.cached_session.lock (fun () ->
      session.cached_session.open_documents <-
        List.filter
          (fun f -> not (List.mem f files))
          session.cached_session.open_documents;
      Lwt.return_unit)

let update_workspace_folders ?(added = []) ?(removed = []) session =
  let workspace_folders =
    session.workspace_folders
    |> List.filter (fun folder -> not (List.mem folder removed))
    |> List.append added
  in
  { session with workspace_folders }

let record_results session results files =
  let results_by_file =
    Assoc.group_by (fun (r : Out.cli_match) -> r.path) results
  in
  List.iter (fun f -> Hashtbl.replace session.cached_scans f []) files;
  List.iter
    (fun (f, results) -> Hashtbl.add session.cached_scans f results)
    results_by_file;
  ()
