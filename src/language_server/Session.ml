open Lsp
open Types
open File.Operators
module In = Input_to_core_t

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t = {
  capabilities : ServerCapabilities.t;
  incoming : Lwt_io.input_channel;
  outgoing : Lwt_io.output_channel;
  config : Runner_config.t; (* ... *)
  (* TODO: Fpath.t option? *)
  workspace_folders : Fpath.t list;
  cached_rules : Runner_config.rule_source option;
  (* TODO: use Fpath.t for the key *)
  documents :
    (string, (Semgrep_output_v1_t.core_match * Rule.rule) list) Hashtbl.t;
  only_git_dirty : bool;
  (* Whether or not we should allow hovering of AST nodes *)
  do_hover : bool;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let create capabilities config =
  {
    capabilities;
    config;
    incoming = Lwt_io.stdin;
    outgoing = Lwt_io.stdout;
    workspace_folders = [];
    (* TODO: should be a None? or should pass root to create()? *)
    cached_rules = None;
    documents = Hashtbl.create 10;
    only_git_dirty = true;
    do_hover = false;
  }

let dirty_files_of_folder folder =
  let git_repo = Git_wrapper.is_git_repo folder in
  if git_repo then
    let dirty_files = Git_wrapper.dirty_files folder in
    Some (Common.map (fun x -> folder // x) dirty_files)
  else None

(*****************************************************************************)
(* State getters *)
(*****************************************************************************)

(* This is dynamic so if the targets file is updated we don't have to restart
 * (and reparse rules...).
 * Once osemgrep is ready, we can just use its target manager directly here
 *)
let targets session =
  let config = session.config in
  let targets =
    match config.target_source with
    | Some (Targets targets) -> targets
    | Some (Target_file _) ->
        let targets, _ = Run_semgrep.targets_of_config config [] in
        targets
    | None -> failwith "No targets provided"
  in
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
    && ((not session.only_git_dirty) || member_folder_dirty_files file folder)
  in
  let member_workspaces (t : In.target) =
    List.exists
      (fun f -> member_workspace_folder (Fpath.v t.path) f)
      session.workspace_folders
  in
  (* Filter targets by if only_git_dirty, if they are a dirty file *)
  let target_mappings =
    targets.target_mappings |> List.filter member_workspaces
  in
  { targets with target_mappings }

(* Can be useful in places *)
let hrules session =
  let rules =
    match session.cached_rules with
    | Some (Rules rules) -> rules
    | Some (Rule_file _)
    | None ->
        []
  in
  Rule.hrules_of_rules rules

(* Useful for when we need to reset diagnostics, such as when changing what
 * rules we've run *)
let scanned_files session =
  (* We can get duplicates apparently *)
  Hashtbl.fold (fun file _ acc -> file :: acc) session.documents []
  |> List.sort_uniq String.compare

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

let load_rules session =
  let config = session.config in
  let rules =
    match config.rule_source with
    | Some (Rule_file file) ->
        let (rules, _), _ =
          Common.with_time (fun () ->
              Parse_rule.parse_and_filter_invalid_rules file)
        in
        rules
    | Some (Rules rules) -> rules
    | None -> failwith "No rules provided"
  in
  { session with cached_rules = Some (Rules rules) }

let record_results session results files =
  let results_by_file =
    results
    |> Common.group_by (fun ((m, _) : Processed_run.t) ->
           Fpath.v m.location.path)
  in
  (* Clear out all results first *)
  files |> List.iter (fun file -> Hashtbl.add session.documents !!file []);
  results_by_file
  |> List.iter (fun (file, results) ->
         Hashtbl.add session.documents !!file results)
