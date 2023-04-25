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
module R = Rule
module RP = Report
module Resp = Output_from_core_t
module E = Semgrep_error_code
module Out = Output_from_core_t

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Small wrapper around Match_search_mode and Match_tainting_mode
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* This can be captured in Run_semgrep.ml *)
exception File_timeout

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let timeout_function rule file timeout f =
  let saved_busy_with_equal = !AST_utils.busy_with_equal in
  let timeout = if timeout <= 0. then None else Some timeout in
  match
    Time_limit.set_timeout_opt ~name:"Match_rules.timeout_function" timeout f
  with
  | Some res -> Some res
  | None ->
      (* Note that we could timeout while testing the equality of two ASTs and
       * `busy_with_equal` will then erroneously have a `<> Not_busy` value. *)
      AST_utils.busy_with_equal := saved_busy_with_equal;
      logger#info "timeout for rule %s on file %s" (fst rule.R.id) file;
      None

let skipped_target_of_rule (file_and_more : Xtarget.t) (rule : R.rule) :
    Resp.skipped_target =
  let rule_id, _ = rule.id in
  let details =
    spf
      "No need to perform deeper matching because target does not contain some \
       elements necessary for the rule to match '%s'"
      rule_id
  in
  {
    path = file_and_more.file;
    reason = Irrelevant_rule;
    details;
    rule_id = Some rule_id;
  }

(* This function separates out rules into groups of taint rules by languages,
   all of the nontaint rules, and the rules which we skip due to prefiltering.
*)
let group_rules xconf rules xtarget =
  let { Xtarget.file; lazy_content; _ } = xtarget in
  let relevant_taint_rules, relevant_nontaint_rules, skipped_rules =
    rules
    |> Common.partition_either3 (fun r ->
           let xconf =
             Match_env.adjust_xconfig_with_rule_options xconf r.R.options
           in
           let relevant_rule =
             if xconf.filter_irrelevant_rules then (
               match Analyze_rule.regexp_prefilter_of_rule r with
               | None -> true
               | Some (prefilter_formula, func) ->
                   let content = Lazy.force lazy_content in
                   let s =
                     Semgrep_prefilter_j.string_of_formula prefilter_formula
                   in
                   logger#trace "looking for %s in %s" s file;
                   func content)
             else true
           in
           if not relevant_rule then
             logger#trace "skipping rule %s for %s" (fst r.R.id) file;
           match r.R.mode with
           | _ when not relevant_rule -> Right3 r
           | `Taint _ as mode -> Left3 { r with mode }
           | (`Extract _ | `Search _) as mode -> Middle3 { r with mode })
  in
  (* Taint rules are only relevant to each other if they are meant to be
     analyzing the same language.
     So we group the taint rules by common language, before passing them
     to [Match_tainting_mode.check_rules].
  *)
  let relevant_taint_rules_groups =
    relevant_taint_rules
    |> Common.map (fun r -> (r.R.languages, r))
    |> Common.group_assoc_bykey_eff |> Common.map snd
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
  fun file rule f ->
    let rule_id = fst rule.R.id in
    Rule.last_matched_rule := Some rule_id;
    let res_opt =
      Profiling.profile_code (spf "real_rule:%s" rule_id) (fun () ->
          (* here we handle the rule! *)
          timeout_function rule file timeout f)
    in
    match res_opt with
    | Some res -> res
    | None ->
        incr cnt_timeout;
        if timeout_threshold > 0 && !cnt_timeout >= timeout_threshold then
          raise File_timeout;
        let loc = Tok.first_loc_of_file file in
        let error = E.mk_error ~rule_id:(Some rule_id) loc "" Out.Timeout in
        RP.make_match_result []
          (Report.ErrorSet.singleton error)
          (RP.empty_rule_profiling rule)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let check ~match_hook ~timeout ~timeout_threshold (xconf : Match_env.xconfig)
    rules xtarget =
  let { Xtarget.file; lazy_ast_and_errors; _ } = xtarget in
  logger#trace "checking %s with %d rules" file (List.length rules);
  if !Profiling.profile =*= Profiling.ProfAll then (
    logger#info "forcing eval of ast outside of rules, for better profile";
    Lazy.force lazy_ast_and_errors |> ignore);

  let per_rule_boilerplate_fn =
    per_rule_boilerplate_fn ~timeout ~timeout_threshold file
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
           Match_tainting_mode.check_rules ~match_hook ~per_rule_boilerplate_fn
             relevant_taint_rules xconf xtarget)
  in
  let res_nontaint_rules =
    relevant_nontaint_rules
    |> Common.map (fun r ->
           let xconf =
             Match_env.adjust_xconfig_with_rule_options xconf r.R.options
           in
           per_rule_boilerplate_fn
             (r :> R.rule)
             (fun () ->
               (* dispatching *)
               match r.R.mode with
               | `Search _ as mode ->
                   Match_search_mode.check_rule { r with mode } match_hook xconf
                     xtarget
               | `Extract extract_spec ->
                   Match_search_mode.check_rule
                     { r with mode = `Search extract_spec.R.formula }
                     match_hook xconf xtarget
               | `Join _ -> raise Common.Todo))
  in
  let res_total = res_taint_rules @ res_nontaint_rules in
  let res = RP.collate_rule_results xtarget.Xtarget.file res_total in
  let extra =
    match res.extra with
    | RP.Debug { skipped_targets; profiling } ->
        let skipped =
          Common.map (skipped_target_of_rule xtarget) skipped_rules
        in
        RP.Debug { skipped_targets = skipped @ skipped_targets; profiling }
    | Time profiling -> Time profiling
    | No_info -> No_info
  in
  { res with extra }
