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
    (string, (Semgrep_output_v1_t.core_match * Rule.rule) list) Hashtbl.t;
  mutable next_id : int;
  only_git_dirty : bool;
}

let create capabilities config =
  {
    capabilities;
    config;
    incoming = Lwt_io.stdin;
    outgoing = Lwt_io.stdout;
    root = "";
    cached_rules = None;
    documents = Hashtbl.create 10;
    next_id = 0;
    only_git_dirty = true;
  }

(* This is dynamic so if the targets file is updated we don't have to restart (and reparse rules...) *)
let targets session =
  let config = session.config in
  let%lwt git_repo = Git_helper.is_git_repo () in
  let%lwt dirty_files =
    if git_repo then
      let%lwt dirty_files = Git_helper.dirty_files () in
      Lwt_list.map_p
        (fun file -> Lwt.return (Filename.concat session.root file))
        dirty_files
    else Lwt.return []
  in
  let targets =
    match config.target_source with
    | Some (Targets targets) -> targets
    | Some (Target_file _) ->
        let targets, _ = Run_semgrep.targets_of_config config [] in
        targets
    | None -> failwith "No targets provided"
  in
  let%lwt target_mappings =
    Lwt_list.filter_p
      (fun (t : Input_to_core_t.target) ->
        ((not (session.only_git_dirty && git_repo))
        || List.mem t.path dirty_files)
        |> Lwt.return)
      targets.target_mappings
  in
  Lwt.return { targets with target_mappings }

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

let hrules session =
  let rules =
    match session.cached_rules with
    | Some (Rules rules) -> rules
    | Some (Rule_file _) -> []
    | None -> failwith "No rules provided"
  in
  Rule.hrules_of_rules rules

let record_results session results files =
  let results_by_file =
    Common2.group_by_mapped_key
      (fun ((m, _) : Processed_run.t) -> m.location.path)
      results
  in
  (* Clear out all results first *)
  Common2.iter (fun file -> Hashtbl.add session.documents file []) files;
  Common2.iter
    (fun (file, results) -> Hashtbl.add session.documents file results)
    results_by_file

let scanned_files session =
  Hashtbl.fold (fun file _ acc -> file :: acc) session.documents []
