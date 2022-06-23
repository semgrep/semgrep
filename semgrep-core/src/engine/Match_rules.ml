(* Yoann Padioleau
 *
 * Copyright (C) 2019-2022 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
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
    Common.set_timeout_opt ~name:"Match_rules.timeout_function" timeout f
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

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let check ~match_hook ~timeout ~timeout_threshold default_config rules xtarget =
  let { Xtarget.file; lazy_content; lazy_ast_and_errors; _ } = xtarget in
  logger#trace "checking %s with %d rules" file (List.length rules);
  if !Common.profile = Common.ProfAll then (
    logger#info "forcing eval of ast outside of rules, for better profile";
    Lazy.force lazy_ast_and_errors |> ignore);

  let cnt_timeout = ref 0 in

  let res_rules, skipped_rules =
    rules
    |> Common.partition_either (fun r ->
           let relevant_rule =
             if !Flag_semgrep.filter_irrelevant_rules then (
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
           if not relevant_rule then (
             logger#trace "skipping rule %s for %s" (fst r.R.id) file;
             Right r)
           else
             let rule_id = fst r.R.id in
             Rule.last_matched_rule := Some rule_id;
             Common.profile_code (spf "real_rule:%s" rule_id) (fun () ->
                 let match_result =
                   timeout_function r file timeout (fun () ->
                       (* dispatching *)
                       match r.R.mode with
                       | `Search _ as mode ->
                           Match_search_mode.check_rule { r with mode }
                             match_hook default_config xtarget
                       | `Taint _ as mode ->
                           (* TODO: 'debug_taint' should just be part of 'res'
                            * (i.e., add a "debugging" field to 'Report.match_result'). *)
                           let res, _TODO_debug_taint =
                             Match_tainting_mode.check_rule { r with mode }
                               match_hook default_config xtarget
                           in
                           res
                       | `Extract extract_spec ->
                           (* TODO: need another module for extract? *)
                           Match_search_mode.check_rule
                             { r with mode = `Search extract_spec.pformula }
                             match_hook default_config xtarget)
                 in
                 match match_result with
                 | Some res -> Left res
                 (* Note that because we now parse lazily a file, this rule timeout
                  * can actually correspond to a parsing file timeout. *)
                 | None ->
                     incr cnt_timeout;
                     if
                       timeout_threshold > 0
                       && !cnt_timeout >= timeout_threshold
                     then raise File_timeout;
                     let loc = Parse_info.first_loc_of_file file in
                     Left
                       {
                         RP.matches = [];
                         errors =
                           [
                             E.mk_error ~rule_id:(Some rule_id) loc ""
                               Out.Timeout;
                           ];
                         skipped_targets = [];
                         profiling = RP.empty_rule_profiling r;
                       }))
  in
  let skipped = Common.map (skipped_target_of_rule xtarget) skipped_rules in
  let res = RP.collate_rule_results xtarget.Xtarget.file res_rules in
  { res with skipped_targets = skipped @ res.skipped_targets }
