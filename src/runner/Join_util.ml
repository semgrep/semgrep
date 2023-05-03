open Runner_config
open Common
module R = Rule
module RP = Report
module JoinRuleMap = Map.Make (String)

let join_rule_to_rules rule join_info table =
  let step_to_rule i (step : R.step_info) =
    let step_id =
      (spf "%s_r2c_internal_join_suffix_%d" (fst rule.R.id) i, snd rule.R.id)
    in
    let mode =
      match step.R.step_formula with
      | R.Step_taint t -> `Taint t
      | R.Step_search s -> `Search s
    in
    let languages = step.R.step_languages in
    let paths = step.R.step_paths in
    Hashtbl.add table (fst step_id) (rule, i);
    { rule with R.id = step_id; mode; languages; paths }
  in
  join_info |> mapi step_to_rule

let extract_join_rules config parsed_rules =
  if config.target_source <> None then
    failwith "join mode experiment does not work with targets file yet";

  let rules, invalid_rules = parsed_rules in
  let join_rules_table = Hashtbl.create 10 in
  let rules_with_join_rules_resolved =
    rules
    |> List.concat_map (fun r ->
           match r.R.mode with
           | `Join join_info -> join_rule_to_rules r join_info join_rules_table
           | `Taint _
           | `Search _
           | `Extract _ ->
               [ r ])
  in
  (* Use a Hashtbl to add the rules to simplify the code but then move it
     to a Map for later use *)
  let join_map = JoinRuleMap.of_seq (Hashtbl.to_seq join_rules_table) in
  (join_map, (rules_with_join_rules_resolved, invalid_rules))

let unify_results _join_rule_map res =
  (* Conservatively does not require that the order of the results match the order
     of the rules. Making that assumption might give us a small speedup but would
     be more fragile *)
  pr2 (RP.show_final_result res);
  res
