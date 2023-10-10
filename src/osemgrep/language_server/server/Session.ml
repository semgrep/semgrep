open Lsp
open Types
open File.Operators
module Out = Semgrep_output_v1_t

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* We really don't wan't mutable state in the server.
   This is the only exception *)
type session_cache = {
  mutable rules : Rule.t list;
  mutable skipped_fingerprints : string list;
  lock : Lwt_mutex.t;
}

type t = {
  capabilities : ServerCapabilities.t;
  incoming : Lwt_io.input_channel;
  outgoing : Lwt_io.output_channel;
  workspace_folders : Fpath.t list;
  cached_scans : (Fpath.t, Out.cli_match list) Hashtbl.t;
  cached_session : session_cache;
  user_settings : User_settings.t;
  is_intellij : bool;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let create capabilities =
  let cached_session =
    { rules = []; skipped_fingerprints = []; lock = Lwt_mutex.create () }
  in
  {
    capabilities;
    incoming = Lwt_io.stdin;
    outgoing = Lwt_io.stdout;
    workspace_folders = [];
    cached_scans = Hashtbl.create 10;
    cached_session;
    user_settings = User_settings.default;
    is_intellij = false;
  }

let dirty_files_of_folder folder =
  let git_repo = Git_wrapper.is_git_repo folder in
  if git_repo then
    let dirty_files = Git_wrapper.dirty_files folder in
    Some (Common.map (fun x -> folder // x) dirty_files)
  else None

let decode_rules data =
  Common2.with_tmp_file ~str:data ~ext:"json" (fun file ->
      let file = Fpath.v file in
      let res =
        Rule_fetching.load_rules_from_file ~origin:Other_origin
          ~registry_caching:true file
      in
      Logs.info (fun m -> m "Loaded %d rules from CI" (List.length res.rules));
      Logs.info (fun m -> m "Got %d errors from CI" (List.length res.errors));
      res)

(*****************************************************************************)
(* State getters *)
(*****************************************************************************)

let auth_token () =
  match !Semgrep_envvars.v.app_token with
  | Some token -> Some token
  | None ->
      let settings = Semgrep_settings.load () in
      settings.api_token

(* This is dynamic so if the targets file is updated we don't have to restart
 * (and reparse rules...).
 * Once osemgrep is ready, we can just use its target manager directly here
 *)
let targets session =
  let dirty_files =
    Common.map (fun f -> (f, dirty_files_of_folder f)) session.workspace_folders
  in
  let member_folder_dirty_files file folder =
    let dirty_files = List.assoc folder dirty_files in
    match dirty_files with
    | None -> true
    | Some files -> List.mem file files
  in
  let member_workspace_folder file folder =
    Fpath.is_prefix folder file
    && ((not session.user_settings.only_git_dirty)
       || member_folder_dirty_files file folder)
  in
  let member_workspaces t =
    List.exists (fun f -> member_workspace_folder t f) session.workspace_folders
  in
  let workspace_targets f =
    let targets_conf =
      User_settings.find_targets_conf_of_t session.user_settings
    in
    Find_targets.get_targets { targets_conf with project_root = Some f } [ f ]
    |> fst
  in
  let targets =
    session.workspace_folders |> List.concat_map workspace_targets
  in
  (* Filter targets by if only_git_dirty, if they are a dirty file *)
  targets |> List.filter member_workspaces

let fetch_ci_rules_and_origins () =
  let token = auth_token () in
  match token with
  | Some token ->
      let%lwt res =
        Semgrep_App.fetch_scan_config_async ~token ~sca:false ~dry_run:true
          ~full_scan:true ~repository:""
      in
      let conf =
        match res with
        | Ok scan_config -> Some (decode_rules scan_config.rule_config)
        | Error e ->
            Logs.warn (fun m -> m "Failed to fetch rules from CI: %s" e);
            None
      in
      Lwt.return conf
  | _ -> Lwt.return None

(* TODO Default to auto *)
let fetch_rules session =
  let%lwt ci_rules =
    if session.user_settings.ci then fetch_ci_rules_and_origins ()
    else Lwt.return_none
  in
  let home = Unix.getenv "HOME" |> Fpath.v in
  let rules_source =
    session.user_settings.configuration |> Common.map Fpath.v
    |> Common.map Fpath.normalize
    |> Common.map (fun f ->
           let p = Fpath.rem_prefix (Fpath.v "~/") f in
           Option.bind p (fun f -> Some (home // f)) |> Option.value ~default:f)
    |> Common.map Fpath.to_string
  in
  let rules_source =
    if rules_source = [] && ci_rules = None then (
      Logs.debug (fun m -> m "No rules source specified, using auto");
      [ "auto" ])
    else rules_source
  in
  let%lwt rules_and_origins =
    Lwt_list.map_p
      (fun source ->
        let in_docker = !Semgrep_envvars.v.in_docker in
        let config = Rules_config.parse_config_string ~in_docker source in
        Rule_fetching.rules_from_dashdash_config_async
          ~rewrite_rule_ids:true (* default *)
          ~token_opt:(auth_token ()) ~registry_caching:true config)
      rules_source
  in
  let rules_and_origins = List.flatten rules_and_origins in
  let rules_and_origins =
    match ci_rules with
    | Some r ->
        Logs.info (fun m -> m "Got %d rules from CI" (List.length r.rules));
        r :: rules_and_origins
    | None ->
        Logs.info (fun m -> m "No rules from CI");
        rules_and_origins
  in
  let rules, errors =
    Rule_fetching.partition_rules_and_errors rules_and_origins
  in
  let rules =
    Common.uniq_by
      (fun r1 r2 -> Rule_ID.equal (fst r1.Rule.id) (fst r2.Rule.id))
      rules
  in
  let rule_filtering_conf =
    Rule_filtering.{ exclude_rule_ids = []; severity = [] }
  in
  let rules, errors =
    (Rule_filtering.filter_rules rule_filtering_conf rules, errors)
  in

  Lwt.return (rules, errors)

let fetch_skipped_fingerprints () =
  (* At some point we should allow users to ignore ids locally *)
  let auth_token = auth_token () in
  match auth_token with
  | Some token -> (
      let%lwt deployment_opt =
        Semgrep_App.get_scan_config_from_token_async ~token
      in
      match deployment_opt with
      | Some deployment -> Lwt.return deployment.triage_ignored_match_based_ids
      | None -> Lwt.return [])
  | None -> Lwt.return []

let cache_session session =
  let%lwt rules, _ = fetch_rules session in
  let%lwt skipped_fingerprints = fetch_skipped_fingerprints () in
  Lwt_mutex.with_lock session.cached_session.lock (fun () ->
      session.cached_session.rules <- rules;
      session.cached_session.skipped_fingerprints <- skipped_fingerprints;
      Lwt.return_unit)

(* Useful for when we need to reset diagnostics, such as when changing what
 * rules we've run *)
let scanned_files session =
  (* We can get duplicates apparently *)
  Hashtbl.fold (fun file _ acc -> file :: acc) session.cached_scans []
  |> List.sort_uniq Fpath.compare

let runner_conf session =
  User_settings.core_runner_conf_of_t session.user_settings

let previous_scan_of_file session file =
  Hashtbl.find_opt session.cached_scans file

(*****************************************************************************)
(* State setters *)
(*****************************************************************************)

let update_workspace_folders ?(added = []) ?(removed = []) session =
  let workspace_folders =
    session.workspace_folders
    |> List.filter (fun folder -> not (List.mem folder removed))
    |> List.append added
  in
  { session with workspace_folders }

let record_results session results files =
  let results_by_file =
    Common.group_by (fun (r : Out.cli_match) -> Fpath.v r.path) results
  in
  List.iter (fun f -> Hashtbl.replace session.cached_scans f []) files;
  List.iter
    (fun (f, results) -> Hashtbl.add session.cached_scans f results)
    results_by_file;
  ()
