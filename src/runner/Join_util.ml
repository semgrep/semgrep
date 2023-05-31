open Runner_config
open Common
module R = Rule
module RP = Report
module C = Common2
module P = Pattern_match
module M = Metavariable
module MvarMap = Map.Make (String)

module MvarSet = Set.Make (struct
  type t = Metavariable.mvalue

  let compare a b = if M.Syntactic.equal_mvalue a b then 0 else compare a b
end)

module JoinRuleMap = Map.Make (struct
  type t = R.rule_id

  let compare a b = compare (R.ID.to_string a) (R.ID.to_string b)
end)

(* Functions for Join Mode
   Conservatively does not require that the order of the results match the order
   of the rules. Making that assumption might give us a small speedup but would
   be more fragile *)

type matches_by_step = { step_id : R.rule_id; matches : P.t list }
[@@deriving show]

let join_rule_to_rules rule join_info table =
  let n = List.length join_info in
  let step_to_rule i (step : R.step) =
    let id_str = spf "%s__step_%d" (fst rule.R.id :> string) i in
    let step_id = (R.ID.of_string id_str, snd rule.R.id) in
    let mode =
      match step.R.step_mode with
      | `Taint t -> `Taint t
      | `Search s -> `Search s
    in
    let languages = step.R.step_languages in
    let paths = step.R.step_paths in
    (* TODO a bit redundant to have n in each entry but it's simpler *)
    Hashtbl.add table (fst step_id) (rule, i, n);
    { rule with R.id = step_id; mode; languages; paths }
  in
  join_info |> mapi step_to_rule

let extract_join_rules config parsed_rules =
  let rules, invalid_rules = parsed_rules in
  let join_rules_table = Hashtbl.create 10 in
  let rules_with_join_rules_resolved =
    rules
    |> List.concat_map (fun r ->
           match r.R.mode with
           | `Step join_info -> join_rule_to_rules r join_info join_rules_table
           | `Taint t -> [ { r with mode = `Taint t } ]
           | `Search s -> [ { r with mode = `Search s } ]
           | `Extract e -> [ { r with mode = `Extract e } ])
  in
  if Hashtbl.length join_rules_table > 0 && config.target_source <> None then
    failwith "join mode experiment does not work with targets file yet";

  (* Use a Hashtbl to add the rules to simplify the code but then move it
     to a Map for later use *)
  let join_map = JoinRuleMap.of_seq (Hashtbl.to_seq join_rules_table) in
  (join_map, (rules_with_join_rules_resolved, invalid_rules))

let get_final_match_of_matches join_rule_map matches_by_step =
  List.find_map
    (fun step ->
      let rule, i, n = JoinRuleMap.find step.step_id join_rule_map in
      if Int.equal i (n - 1) then Some (rule, step) else None)
    matches_by_step

(* Consider a rule with three steps, where step 1 binds $A and $B,
   step 2 binds $B and $C, and step 3 binds $A, $B, and $C. When
   processing step 2, we need to intersect the values of $C, but
   add all bindings for $A and $C. *)
let intersect_mvar_maps prev_map cur_map =
  MvarMap.merge
    (fun _mvar prev_mvals cur_mvals ->
      match (prev_mvals, cur_mvals) with
      | Some prev_mvals, Some cur_mvals ->
          Some (MvarSet.inter prev_mvals cur_mvals)
      | Some prev_mvals, None -> Some prev_mvals
      | None, Some cur_mvals -> Some cur_mvals
      | None, None -> None)
    prev_map cur_map

let print_updated_matches config print_match has_join_steps matches =
  if config.output_format =*= Text then
    if has_join_steps then
      pr
        "\n\n\
         ---------------------------------------------------\n\n\
         The previous matches include matches for join steps. Here are the \
         final matches:\n";
  List.iter
    (fun match_ -> print_match config match_ Metavariable.ii_of_mval)
    matches

let unify_join_results config print_match join_rule_map res =
  let rule_for_step_id (id : Rule.rule_id) =
    match JoinRuleMap.find_opt id join_rule_map with
    | Some (rule, _i, _n) -> Some (fst rule.R.id)
    | None -> None
  in
  (* TODO this currently just fixes the matches, but we probably also want to adjust
     the errors, etc *)
  let matches_by_step =
    C.group
      (fun a b -> R.equal_rule_id a.P.rule_id.id b.P.rule_id.id)
      res.RP.matches
    |> Common.map (fun (C.Nonempty (m, _) as matches) ->
           { step_id = m.P.rule_id.id; matches = C.nonempty_to_list matches })
  in
  let matches_by_rule =
    C.group
      (fun a b ->
        Option.equal R.equal_rule_id
          (rule_for_step_id a.step_id)
          (rule_for_step_id b.step_id))
      matches_by_step
  in
  let matches_by_join_rules, matches_by_normal_rules =
    List.partition
      (fun (C.Nonempty (m, _)) ->
        match rule_for_step_id m.step_id with
        | Some _ -> true
        | None -> false)
      matches_by_rule
  in
  let matches_by_join_rules =
    Common.map C.nonempty_to_list matches_by_join_rules
  in
  let matches_by_normal_rules =
    Common.map C.nonempty_to_list matches_by_normal_rules
  in
  let process_steps_for_rule (matches_by_step : matches_by_step list) =
    (* TODO this logic isn't actually quite right. See `tests/join/abc_harder`
       for an example of why *)
    let intersect_mvars mvar_bindings
        ({ step_id = _; matches } : matches_by_step) =
      let mvars_by_steps = matches |> List.concat_map (fun m -> m.P.env) in
      let mvar_bindings_for_step =
        List.fold_left
          (fun mvar_bindings_acc (mvar, mval) ->
            MvarMap.update mvar
              (function
                | Some mvals ->
                    Some (MvarSet.union (MvarSet.singleton mval) mvals)
                | None -> Some (MvarSet.singleton mval))
              mvar_bindings_acc)
          MvarMap.empty mvars_by_steps
      in
      intersect_mvar_maps mvar_bindings mvar_bindings_for_step
    in
    let mvar_bindings =
      List.fold_left intersect_mvars MvarMap.empty matches_by_step
    in
    (* Filter the matches for the final step because that's the only one we return matches for *)
    match get_final_match_of_matches join_rule_map matches_by_step with
    | Some (rule, final_step) ->
        let matches_for_join_rule =
          List.filter
            (fun m ->
              List.for_all
                (fun (mvar, mval) ->
                  match MvarMap.find_opt mvar mvar_bindings with
                  | Some mvals -> MvarSet.mem mval mvals
                  | None -> true)
                m.P.env)
            final_step.matches
        in
        Common.map
          (fun m ->
            let { P.rule_id; _ } = m in
            let rule_id =
              {
                rule_id with
                id = fst rule.R.id;
                languages =
                  Option.value ~default:[] rule.R.languages.target_selector;
              }
            in
            { m with P.rule_id })
          matches_for_join_rule
    | None -> []
  in
  let join_matches =
    List.concat_map process_steps_for_rule matches_by_join_rules
  in
  let normal_matches =
    matches_by_normal_rules |> List.concat
    |> List.concat_map (fun { step_id = _; matches } -> matches)
  in
  let matches = join_matches @ normal_matches in
  matches
  |> print_updated_matches config print_match (matches_by_join_rules <> []);
  { res with matches }

let unify_results config print_match join_rule_map res =
  if JoinRuleMap.is_empty join_rule_map then res
  else unify_join_results config print_match join_rule_map res
