(* Full result information *)
(* TODO: We could use atdgen to specify those to factorize type definitions
 * that have to be present anyway in Output_from_core.atd.
 *)

(* Different formats for profiling information as we have access to more data *)

(* Save time information as we run each rule *)

type rule_profiling = {
  rule_id : Rule.rule_id;
  parse_time : float;
  match_time : float;
}
[@@deriving show]

type times = { parse_time : float; match_time : float }

(* Save time information as we run each file *)

type file_profiling = {
  file : Common.filename;
  rule_times : rule_profiling list;
  run_time : float;
}
[@@deriving show]

type partial_profiling = {
  file : Common.filename;
  rule_times : rule_profiling list;
}
[@@deriving show]

(* Result object for the entire rule *)

type final_profiling = {
  rules : Rule.rule list;
  rules_parse_time : float;
  file_times : file_profiling list;
}
[@@deriving show]

type final_result = {
  matches : Pattern_match.t list;
  errors : Semgrep_error_code.error list;
  skipped_targets : Output_from_core_t.skipped_target list;
  skipped_rules : Rule.invalid_rule_error list;
  final_profiling : final_profiling option;
}
[@@deriving show]

(* For each file, substitute in the profiling type we have *)

type 'a match_result = {
  matches : Pattern_match.t list;
  errors : Semgrep_error_code.error list;
  skipped_targets : Output_from_core_t.skipped_target list;
  profiling : 'a;
}
[@@deriving show]

(* Create empty versions of match results *)

let empty_partial_profiling file = { file; rule_times = [] }

let empty_rule_profiling rule =
  { rule_id = fst rule.Rule.id; parse_time = 0.0; match_time = 0.0 }

let empty_semgrep_result =
  {
    matches = [];
    errors = [];
    skipped_targets = [];
    profiling = { parse_time = 0.0; match_time = 0.0 };
  }

let empty_final_result =
  {
    matches = [];
    errors = [];
    skipped_targets = [];
    skipped_rules = [];
    final_profiling = None;
  }

(* Augment reported information with additional info *)

let add_run_time :
    float -> partial_profiling match_result -> file_profiling match_result =
 fun run_time
     { matches; errors; skipped_targets; profiling = { file; rule_times } } ->
  {
    matches;
    errors;
    skipped_targets;
    profiling = { file; rule_times; run_time };
  }

let add_rule : Rule.rule -> times match_result -> rule_profiling match_result =
 fun rule
     {
       matches;
       errors;
       skipped_targets;
       profiling = { parse_time; match_time };
     } ->
  {
    matches;
    errors;
    skipped_targets;
    profiling = { rule_id = fst rule.Rule.id; parse_time; match_time };
  }

(* Aggregate a list of semgrep results into one returned object *)

let collate_pattern_results results =
  let unzip_results l =
    let rec unzip all_matches all_errors all_skipped all_parse_time
        all_match_time = function
      | {
          matches;
          errors;
          skipped_targets;
          profiling = { parse_time; match_time };
        }
        :: l ->
          unzip (matches :: all_matches) (errors :: all_errors)
            (skipped_targets :: all_skipped)
            (parse_time +. all_parse_time)
            (match_time +. all_match_time)
            l
      | [] ->
          ( List.rev all_matches,
            List.rev all_errors,
            List.rev all_skipped,
            all_parse_time,
            all_match_time )
    in
    unzip [] [] [] 0.0 0.0 l
  in
  let matches, errors, skipped, parse_time, match_time =
    unzip_results results
  in
  {
    matches = List.flatten matches;
    errors = List.flatten errors;
    skipped_targets = List.flatten skipped;
    profiling = { parse_time; match_time };
  }

let collate_rule_results :
    string -> rule_profiling match_result list -> partial_profiling match_result
    =
 fun file results ->
  let unzip_results l =
    let rec unzip all_matches all_errors all_skipped all_profiling = function
      | { matches; errors; skipped_targets; profiling } :: l ->
          unzip (matches :: all_matches) (errors :: all_errors)
            (skipped_targets :: all_skipped)
            (profiling :: all_profiling)
            l
      | [] ->
          ( List.rev all_matches,
            List.rev all_errors,
            List.rev all_skipped,
            List.rev all_profiling )
    in
    unzip [] [] [] [] l
  in
  let matches, errors, skipped, profiling = unzip_results results in
  {
    matches = List.flatten matches;
    errors = List.flatten errors;
    skipped_targets = List.flatten skipped;
    profiling = { file; rule_times = profiling };
  }

let make_final_result results rules ~report_time ~rules_parse_time =
  let matches = results |> Common.map (fun x -> x.matches) |> List.flatten in
  let errors = results |> Common.map (fun x -> x.errors) |> List.flatten in
  let skipped_targets =
    results |> Common.map (fun x -> x.skipped_targets) |> List.flatten
  in
  let file_times = results |> Common.map (fun x -> x.profiling) in
  let final_profiling =
    if report_time then Some { rules; rules_parse_time; file_times } else None
  in
  { matches; errors; skipped_targets; final_profiling; skipped_rules = [] }
