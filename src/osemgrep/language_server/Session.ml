open Lsp
open Types
open File.Operators
module Out = Semgrep_output_v1_t

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* We really don't wan't mutable state in the server.
   This is the only exception *)
type rule_cache = { mutable rules : Rule.t list; lock : Lwt_mutex.t }

type t = {
  capabilities : ServerCapabilities.t;
  incoming : Lwt_io.input_channel;
  outgoing : Lwt_io.output_channel;
  workspace_folders : Fpath.t list;
  documents : (Fpath.t, Out.cli_match list) Hashtbl.t;
  cached_rules : rule_cache;
  user_settings : UserSettings.t;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let create capabilities =
  let cached_rules = { rules = []; lock = Lwt_mutex.create () } in
  {
    capabilities;
    incoming = Lwt_io.stdin;
    outgoing = Lwt_io.stdout;
    workspace_folders = [];
    documents = Hashtbl.create 10;
    cached_rules;
    user_settings = UserSettings.default;
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
        Rule_fetching.load_rules_from_file ~registry_caching:true file
      in
      Logs.info (fun m -> m "Loaded %d rules from CI" (List.length res.rules));
      Logs.info (fun m -> m "Got %d errors from CI" (List.length res.errors));
      { res with origin = None })

(*****************************************************************************)
(* State getters *)
(*****************************************************************************)

let auth_token () =
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
      UserSettings.find_targets_conf_of_t session.user_settings
    in
    Find_targets.get_targets { targets_conf with project_root = Some f } [ f ]
    |> fst
  in
  let targets =
    session.workspace_folders |> List.map workspace_targets |> List.flatten
  in
  (* Filter targets by if only_git_dirty, if they are a dirty file *)
  targets |> List.filter member_workspaces

let fetch_ci_rules_and_origins () =
  let token = auth_token () in
  match token with
  | Some token ->
      let%lwt scan_config =
        Scan_helper.fetch_scan_config_async ~token ~sca:false ~dry_run:true
          ~full_scan:true ""
      in
      let conf =
        match scan_config with
        | Ok rules -> Some (decode_rules rules)
        | _ -> None
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
    session.user_settings.configuration |> List.map Fpath.v
    |> List.map Fpath.normalize
    |> List.map (fun f ->
           let p = Fpath.rem_prefix (Fpath.v "~/") f in
           Option.bind p (fun f -> Some (home // f)) |> Option.value ~default:f)
    |> List.map Fpath.to_string
  in
  let rules_source =
    if rules_source = [] && ci_rules = None then [ "auto" ] else rules_source
  in
  let%lwt rules_and_origins =
    Lwt_list.map_p
      (fun source ->
        let kind = Semgrep_dashdash_config.parse_config_string source in
        Rule_fetching.rules_from_dashdash_config_async
          ~token_opt:(auth_token ()) ~registry_caching:true kind)
      rules_source
  in
  let rules_and_origins = List.flatten rules_and_origins in
  let rules_and_origins =
    match ci_rules with
    | Some r ->
        Logs.info (fun m -> m "Got %d rules from CI" (List.length r.rules));
        r :: rules_and_origins
    | None -> rules_and_origins
  in
  let rules, errors =
    Rule_fetching.partition_rules_and_errors rules_and_origins
  in
  let rules =
    Common.uniq_by
      (fun r1 r2 -> Rule.ID.equal (fst r1.Rule.id) (fst r2.Rule.id))
      rules
  in
  let rule_filtering_conf =
    Rule_filtering.{ exclude_rule_ids = []; severity = [] }
  in
  let rules, errors =
    (Rule_filtering.filter_rules rule_filtering_conf rules, errors)
  in

  Lwt.return (rules, errors)

let cache_rules session =
  let%lwt rules, _ = fetch_rules session in
  Lwt_mutex.with_lock session.cached_rules.lock (fun () ->
      session.cached_rules.rules <- rules;
      Lwt.return_unit)

(* Useful for when we need to reset diagnostics, such as when changing what
 * rules we've run *)
let scanned_files session =
  (* We can get duplicates apparently *)
  Hashtbl.fold (fun file _ acc -> file :: acc) session.documents []
  |> List.sort_uniq Fpath.compare

let runner_conf session =
  UserSettings.core_runner_conf_of_t session.user_settings

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
  List.iter (fun f -> Hashtbl.replace session.documents f []) files;
  List.iter
    (fun (f, results) -> Hashtbl.add session.documents f results)
    results_by_file;
  ()
