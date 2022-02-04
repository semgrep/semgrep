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

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Small wrapper around Match_search_rules and Match_tainting_rules
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* This can be captured in Run_semgrep.ml *)
exception File_timeout of Common.filename

(* Locally-raised exn.
 * Note that because we now parse lazily a file, the rule timeout can
 * actually correspond to a parsing file timeout.
 *)
exception Rule_timeout

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* TODO: get rid of Rule_timeout and return an option, so simpler? *)
let timeout_function rule file timeout f =
  let saved_busy_with_equal = !AST_utils.busy_with_equal in
  let timeout = if timeout <= 0. then None else Some timeout in
  match
    Common.set_timeout_opt ~name:"Match_rules.timeout_function" timeout f
  with
  | Some res -> res
  | None ->
      (* Note that we could timeout while testing the equality of two ASTs and
       * `busy_with_equal` will then erroneously have a `<> Not_busy` value. *)
      AST_utils.busy_with_equal := saved_busy_with_equal;
      logger#info "timeout for rule %s on file %s" (fst rule.R.id) file;
      raise Rule_timeout

let filter_and_partition_rules rules file_and_more =
  let { Xtarget.file; lazy_content; _ } = file_and_more in
  let rules, skipped_rules =
    rules
    |> List.partition (fun r ->
           let relevant_rule =
             if !Flag_semgrep.filter_irrelevant_rules then (
               match Analyze_rule.regexp_prefilter_of_rule r with
               | None -> true
               | Some (re, f) ->
                   let content = Lazy.force lazy_content in
                   logger#trace "looking for %s in %s" re file;
                   f content)
             else true
           in
           if not relevant_rule then
             logger#trace "skipping rule %s for %s" (fst r.R.id) file;
           relevant_rule)
  in
  let search_rules, taint_rules = R.partition_rules rules in
  (search_rules, taint_rules, skipped_rules)

let skipped_target_of_rule (file_and_more : Xtarget.t) (rule : R.rule) :
    Resp.skipped_target =
  let rule_id, _ = rule.id in
  let details =
    spf "target doesn't contain some elements required by rule '%s'" rule_id
  in
  {
    path = file_and_more.file;
    reason = Irrelevant_rule;
    details;
    rule_id = Some rule_id;
  }

let lazy_force x = Lazy.force x [@@profiling]

(*
   Check search-mode rules.
   Return matches, errors, match time.

   NOTE: We used to filter irrelevant rules here, but now this is done in
        Match_rules.check! If you call this function directly, there is no
        filtering of irrelevant rules.
*)
let check_search_rules ~match_hook ~timeout default_config rules xtarget =
  let { Xtarget.file; lazy_ast_and_errors; _ } = xtarget in
  logger#trace "checking %s with %d rules" file (List.length rules);
  if !Common.profile = Common.ProfAll then (
    logger#info "forcing eval of ast outside of rules, for better profile";
    lazy_force lazy_ast_and_errors |> ignore);

  (* TODO: have ~timeout_threshold and raise File_timeout if we get
   * too many rule timeouts
   *)
  rules
  |> List.map (fun (r, pformula) ->
         let rule_id = fst r.R.id in
         Rule.last_matched_rule := Some rule_id;
         Common.profile_code (spf "real_rule:%s" rule_id) (fun () ->
             try
               timeout_function r file timeout (fun () ->
                   Match_search_rules.check_rule r match_hook default_config
                     pformula xtarget)
             with Rule_timeout ->
               let loc = Parse_info.first_loc_of_file file in
               {
                 RP.matches = [];
                 errors =
                   [ E.mk_error ~rule_id:(Some rule_id) loc "" E.Timeout ];
                 skipped = [];
                 profiling = RP.empty_rule_profiling r;
               }))

(*
   Check tainting-mode rules.
   Return matches, errors, match time.
*)
let check_tainting_rules ~match_hook default_config taint_rules xtarget =
  match taint_rules with
  | [] -> []
  | __else__ ->
      let { Xtarget.file; xlang; lazy_ast_and_errors; _ } = xtarget in
      let lang =
        match xlang with
        | L (lang, _) -> lang
        | LGeneric
        | LRegex ->
            failwith "taint-mode and generic/regex matching are incompatible"
      in
      (* TODO can we move this outside to Match_rules? *)
      let (ast, errors), parse_time =
        Common.with_time (fun () -> lazy_force lazy_ast_and_errors)
      in
      taint_rules
      |> List.map (fun ((rule, _) as taint_rule) ->
             let matches, match_time =
               Common.with_time (fun () ->
                   Match_tainting_rules.check_rule match_hook default_config
                     taint_rule file lang ast)
             in
             {
               RP.matches;
               errors;
               skipped = [];
               profiling =
                 { RP.rule_id = fst rule.Rule.id; parse_time; match_time };
             })

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let check ~match_hook ~timeout default_config rules xtarget =
  let search_rules, taint_rules, skipped_rules =
    filter_and_partition_rules rules xtarget
  in
  let res_search =
    check_search_rules ~match_hook ~timeout default_config search_rules xtarget
  in
  let res_taint =
    check_tainting_rules ~match_hook default_config taint_rules xtarget
  in
  let skipped = Common.map (skipped_target_of_rule xtarget) skipped_rules in
  let res =
    RP.collate_rule_results xtarget.Xtarget.file (res_search @ res_taint)
  in
  { res with skipped = skipped @ res.skipped }
