open Lsp
open Types
open File.Operators

type t = {
  capabilities : ServerCapabilities.t;
  incoming : Lwt_io.input_channel;
  outgoing : Lwt_io.output_channel;
  config : Runner_config.t; (* ... *)
  (* TODO: Fpath.t option? *)
  root : string;
  cached_rules : Runner_config.rule_source option;
  (* TODO: use Fpath.t for the key *)
  documents :
    (string, (Semgrep_output_v1_t.core_match * Rule.rule) list) Hashtbl.t;
  only_git_dirty : bool;
}

let create capabilities config =
  {
    capabilities;
    config;
    incoming = Lwt_io.stdin;
    outgoing = Lwt_io.stdout;
    root = "";
    (* TODO: should be a None? or should pass root to create()? *)
    cached_rules = None;
    documents = Hashtbl.create 10;
    only_git_dirty = true;
  }

(* This is dynamic so if the targets file is updated we don't have to restart
 * (and reparse rules...).
 * Once osemgrep is ready, we can just use its target manager directly here
 *)
let targets session =
  let config = session.config in
  let git_repo = Git_wrapper.is_git_repo () in
  let dirty_files =
    if git_repo then
      let dirty_files = Git_wrapper.dirty_files () in
      Common.map (fun x -> Fpath.v session.root // x) dirty_files
    else []
  in
  let targets =
    match config.target_source with
    | Some (Targets targets) -> targets
    | Some (Target_file _) ->
        let targets, _ = Run_semgrep.targets_of_config config [] in
        targets
    | None -> failwith "No targets provided"
  in
  let target_mappings =
    targets.target_mappings
    |> List.filter (fun (t : Input_to_core_t.target) ->
           (not (session.only_git_dirty && git_repo))
           || List.mem (Fpath.v t.path) dirty_files)
  in

  { targets with target_mappings }

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

let record_results session results (files : Fpath.t list) =
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

(* Useful for when we need to reset diagnostics, such as when changing what
 * rules we've run *)
let scanned_files session =
  (* We can get duplicates apparently *)
  Hashtbl.fold (fun file _ acc -> file :: acc) session.documents []
  |> List.sort_uniq String.compare
