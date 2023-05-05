open Runner_config
open Common
module R = Rule
module RP = Report
module C = Common2
module P = Pattern_match
module M = Metavariable
module JoinRuleMap = Map.Make (String)
module MvarMap = Map.Make (String)

module MvarSet = Set.Make (struct
  type t = Metavariable.mvalue

  let compare a b = if M.equal_mvalue a b then 0 else compare a b
end)

(* Functions for Join Mode
   Conservatively does not require that the order of the results match the order
   of the rules. Making that assumption might give us a small speedup but would
   be more fragile *)

type matches_by_step = { step_id : string; matches : P.t list }
[@@deriving show]

let join_rule_to_rules rule join_info table =
  let step_to_rule i (step : R.step_info) =
    let step_id = (spf "%s__step_%d" (fst rule.R.id) i, snd rule.R.id) in
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

let unify_results join_rule_map res =
  let rule_for_step_id id =
    match JoinRuleMap.find_opt id join_rule_map with
    | Some (rule, _n) -> Some (fst rule.R.id)
    | None -> None
  in
  (* TODO this currently just fixes the matches, but we probably also want to adjust
     the errors, etc *)
  let matches_by_step =
    C.group (fun a b -> a.P.rule_id.id = b.P.rule_id.id) res.RP.matches
    |> Common.map (fun (C.Nonempty (m, _) as matches) ->
           { step_id = m.P.rule_id.id; matches = C.nonempty_to_list matches })
  in
  let matches_by_rule =
    C.group
      (fun a b ->
        Option.equal String.equal
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
    (* This is pretty specialized for intersection, may want to change how the
       code's laid out if we decide to support other conditions for OSS *)
    let intersect_mvars mvar_bindings
        ({ step_id = _; matches } : matches_by_step) =
      pr2 "Step:\n";
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
      MvarMap.mapi
        (fun mvar mvals ->
          match MvarMap.find_opt mvar mvar_bindings_for_step with
          | Some mvals_for_step -> MvarSet.inter mvals mvals_for_step
          | None -> mvals)
        mvar_bindings
    in
    let mvar_bindings =
      List.fold_left intersect_mvars MvarMap.empty matches_by_step
    in
    (* Print mvar_bindings *)
    MvarMap.iter
      (fun mvar mvals ->
        Printf.printf "mvar: %s\n" mvar;
        MvarSet.iter
          (fun mval ->
            Printf.printf "mval: %s\n" (Metavariable.show_mvalue mval))
          mvals)
      mvar_bindings;
    (* Filter the matches for the final step *)
    (* Find the final step of the rule because that's the only one we return matches for *)
    let rule, final_step =
      match
        List.find_map
          (fun step ->
            let rule, n = JoinRuleMap.find step.step_id join_rule_map in
            if Int.equal n (List.length matches_by_step - 1) then
              Some (rule, step)
            else None)
          matches_by_step
      with
      | Some x -> x
      | None ->
          failwith
            (spf "Could not find final step for join rule, expected it to be %d"
               (List.length matches_by_step))
    in
    (* Filter the matches for the final step *)
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
            languages = Xlang.to_langs rule.R.languages;
          }
        in
        { m with P.rule_id })
      matches_for_join_rule
  in
  let join_matches = Common.map process_steps_for_rule matches_by_join_rules in
  let normal_matches =
    matches_by_normal_rules |> List.concat
    |> List.concat_map (fun { step_id = _; matches } -> matches)
  in
  let res = { res with matches = List.concat join_matches @ normal_matches } in
  pr2 (RP.show_final_result res);
  res
