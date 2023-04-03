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
    (Uri.t, (Semgrep_output_v1_t.core_match * Rule.rule) list) Hashtbl.t;
}

(* This is dynamic so if the targets file is updated we don't have to restart (and reparse rules...) *)
let targets session =
  let config = session.config in
  match config.target_source with
  | Some (Targets targets) -> targets
  | Some (Target_file _) ->
      let targets, _ = Run_semgrep.targets_of_config config [] in
      targets
  | None -> failwith "No targets provided"

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
