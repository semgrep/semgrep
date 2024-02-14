(* Yoann Padioleau
 *
 * Copyright (C) 2019-2022 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Common
open Fpath_.Operators
module R = Rule
module RP = Core_result
module Resp = Semgrep_output_v1_t
module E = Core_error
module OutJ = Semgrep_output_v1_t

let tags = Logs_.create_tags [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Small wrapper around Match_search_mode and Match_tainting_mode
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* This can be captured in Run_semgrep.ml *)
exception File_timeout of Rule_ID.t list

(* TODO make this one of the Semgrep_error_code exceptions *)
exception Multistep_rules_not_available
(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let timeout_function (rule : Rule.t) file timeout f =
  let saved_busy_with_equal = !AST_generic_equals.busy_with_equal in
  let timeout = if timeout <= 0. then None else Some timeout in
  match
    Time_limit.set_timeout_opt ~name:"Match_rules.timeout_function" timeout f
  with
  | Some res -> Some res
  | None ->
      (* Note that we could timeout while testing the equality of two ASTs and
       * `busy_with_equal` will then erroneously have a `<> Not_busy` value. *)
      AST_generic_equals.busy_with_equal := saved_busy_with_equal;
      Logs.warn (fun m ->
          m ~tags "timeout for rule %s on file %s"
            (Rule_ID.to_string (fst rule.id))
            file);
      None

let skipped_target_of_rule (file_and_more : Xtarget.t) (rule : R.rule) :
    Resp.skipped_target =
  let rule_id, _ = rule.id in
  let details =
    Some
      (spf
         "No need to perform deeper matching because target does not contain \
          some elements necessary for the rule to match '%s'"
         (Rule_ID.to_string rule_id))
  in
  {
    path = file_and_more.path.internal_path_to_content;
    reason = Irrelevant_rule;
    details;
    rule_id = Some rule_id;
  }

let is_relevant_rule_for_xtarget r xconf xtarget =
  let { path = { internal_path_to_content; _ }; lazy_content; _ } : Xtarget.t =
    xtarget
  in
  let xconf = Match_env.adjust_xconfig_with_rule_options xconf r.R.options in
  let is_relevant =
    match xconf.filter_irrelevant_rules with
    | NoPrefiltering -> true
    | PrefilterWithCache cache -> (
        match Analyze_rule.regexp_prefilter_of_rule ~cache:(Some cache) r with
        | None -> true
        | Some (prefilter_formula, func) ->
            let content = Lazy.force lazy_content in
            let s = Semgrep_prefilter_j.string_of_formula prefilter_formula in
            Logs.debug (fun m ->
                m ~tags "looking for %s in %s" s !!internal_path_to_content);
            func content)
  in
  if not is_relevant then
    Logs.debug (fun m ->
        m ~tags "skipping rule %s for %s"
          (Rule_ID.to_string (fst r.R.id))
          !!internal_path_to_content);
  is_relevant

(* This function separates out rules into groups of taint rules by languages,
   all of the nontaint rules, and the rules which we skip due to prefiltering.
*)
let group_rules xconf rules xtarget =
  let relevant_taint_rules, relevant_nontaint_rules, skipped_rules =
    rules
    |> Either_.partition_either3 (fun r ->
           let relevant_rule = is_relevant_rule_for_xtarget r xconf xtarget in
           match r.R.mode with
           | _ when not relevant_rule -> Right3 r
           | `Taint _ as mode -> Left3 { r with mode }
           | (`Extract _ | `Search _) as mode -> Middle3 { r with mode }
           | `Steps _ ->
               UCommon.pr2 (Rule.show_rule r);
               raise Multistep_rules_not_available)
  in
  (* Taint rules are only relevant to each other if they are meant to be
     analyzing the same language.
     So we group the taint rules by common language, before passing them
     to [Match_tainting_mode.check_rules].
  *)
  let relevant_taint_rules_groups =
    relevant_taint_rules
    |> List_.map (fun r -> (r.R.target_analyzer, r))
    |> Assoc.group_assoc_bykey_eff |> List_.map snd
  in
  (relevant_taint_rules_groups, relevant_nontaint_rules, skipped_rules)

(* Given a thunk [f] that computes the results of running the engine on a single
    rule, this function simply instruments the computation on a single rule with
    some boilerplate logic, like setting the last matched rule, timing out if
    it takes too long, and producing a faulty match result in that case.

    In particular, we need this to call [Match_tainting_mode.check_rules], which
    will iterate over each rule in a different place, and so needs access to this
    logic.
*)
let per_rule_boilerplate_fn ~timeout ~timeout_threshold =
  let cnt_timeout = ref 0 in
  let rule_timeouts = ref [] in
  fun file rule f ->
    let rule_id = fst rule.R.id in
    Rule.last_matched_rule := Some rule_id;
    let res_opt =
      Profiling.profile_code
        (spf "real_rule:%s" (Rule_ID.to_string rule_id))
        (fun () ->
          (* here we handle the rule! *)
          timeout_function rule file timeout f)
    in
    match res_opt with
    | Some res -> res
    | None ->
        incr cnt_timeout;
        Stack_.push rule_id rule_timeouts;
        if timeout_threshold > 0 && !cnt_timeout >= timeout_threshold then
          raise (File_timeout !rule_timeouts);
        let loc = Tok.first_loc_of_file file in
        let error = E.mk_error (Some rule_id) loc "" OutJ.Timeout in
        RP.make_match_result []
          (Core_error.ErrorSet.singleton error)
          (Core_profiling.empty_rule_profiling rule)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let check ~match_hook ~timeout ~timeout_threshold
    ?(dependency_match_table : Match_dependency.dependency_match_table option)
    (xconf : Match_env.xconfig) rules xtarget =
  let get_dep_matches =
    match dependency_match_table with
    | Some table -> Hashtbl.find_opt table
    | None -> fun _ -> None
  in
  let { path = { internal_path_to_content; _ }; lazy_ast_and_errors; xlang; _ }
      : Xtarget.t =
    xtarget
  in
  Logs.debug (fun m ->
      m ~tags "checking %s with %d rules" !!internal_path_to_content
        (List.length rules));
  (match (!Profiling.profile, xlang) with
  (* coupling: see Run_semgrep.xtarget_of_file() *)
  | Profiling.ProfAll, Xlang.L (_lang, []) ->
      Logs.debug (fun m ->
          m ~tags "forcing parsing of AST outside of rules, for better profile");
      Lazy.force lazy_ast_and_errors |> ignore
  | _else_ -> ());
  let per_rule_boilerplate_fn =
    per_rule_boilerplate_fn ~timeout ~timeout_threshold
      !!internal_path_to_content
  in

  (* We separate out the taint rules specifically, because we may want to
     do some rule-wide optimizations, which require analyzing more than
     just one rule at once.

     The taint rules are "grouped", see [group_rule] for more.
  *)
  let relevant_taint_rules_groups, relevant_nontaint_rules, skipped_rules =
    group_rules xconf rules xtarget
  in

  let res_taint_rules =
    relevant_taint_rules_groups
    |> List.concat_map (fun relevant_taint_rules ->
           Match_tainting_mode.check_rules ~get_dep_matches ~match_hook
             ~per_rule_boilerplate_fn relevant_taint_rules xconf xtarget)
  in
  let res_nontaint_rules =
    relevant_nontaint_rules
    |> List_.map (fun r ->
           let dependency_matches = get_dep_matches (fst r.R.id) in
           let xconf =
             Match_env.adjust_xconfig_with_rule_options xconf r.R.options
           in
           per_rule_boilerplate_fn
             (r :> R.rule)
             (fun () ->
               (* dispatching *)
               match r.R.mode with
               | `Search _ as mode ->
                   Match_search_mode.check_rule ?dependency_matches
                     { r with mode } match_hook xconf xtarget
               | `Extract extract_spec ->
                   Match_search_mode.check_rule
                     { r with mode = `Search extract_spec.R.formula }
                     match_hook xconf xtarget
               | `Steps _ -> raise Multistep_rules_not_available))
  in
  let res_total = res_taint_rules @ res_nontaint_rules in
  let res =
    RP.collate_rule_results xtarget.path.internal_path_to_content res_total
  in
  let extra =
    match res.extra with
    | Core_profiling.Debug { skipped_targets; profiling } ->
        let skipped =
          List_.map (skipped_target_of_rule xtarget) skipped_rules
        in
        Core_profiling.Debug
          { skipped_targets = skipped @ skipped_targets; profiling }
    | Core_profiling.Time profiling -> Core_profiling.Time profiling
    | Core_profiling.No_info -> Core_profiling.No_info
  in
  { res with extra }
