(* Yoann Padioleau
 *
 * Copyright (C) 2019-2021 r2c
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
module FM = File_and_more
module Resp = Semgrep_core_response_t

let logger = Logging.get_logger [ __MODULE__ ]

let lazy_force x = Lazy.force x [@@profiling]

let check_taint hook default_config taint_rules equivs file_and_more =
  match taint_rules with
  | [] -> RP.empty_semgrep_result
  | __else__ ->
      let { FM.file; xlang; lazy_ast_and_errors; _ } = file_and_more in
      let lang =
        match xlang with
        | L (lang, _) -> lang
        | LGeneric
        | LRegex ->
            failwith "taint-mode and generic/regex matching are incompatible"
      in
      let (ast, errors), parse_time =
        Common.with_time (fun () -> lazy_force lazy_ast_and_errors)
      in
      let matches, match_time =
        Common.with_time (fun () ->
            Tainting_generic.check hook default_config taint_rules equivs file
              lang ast)
      in
      {
        RP.matches;
        errors;
        skipped = [];
        profiling = { RP.parse_time; match_time };
      }

let filter_and_partition_rules rules file_and_more =
  let { FM.file; lazy_content; _ } = file_and_more in
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

let skipped_target_of_rule (file_and_more : FM.t) (rule : R.rule) :
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

let check hook default_config rules equivs file_and_more =
  let search_rules, taint_rules, skipped_rules =
    filter_and_partition_rules rules file_and_more
  in
  let res_search =
    Match_rules.check hook default_config search_rules equivs file_and_more
  in
  let res_taint =
    check_taint hook default_config taint_rules equivs file_and_more
  in
  let skipped =
    Common.map (skipped_target_of_rule file_and_more) skipped_rules
  in
  let res = RP.collate_semgrep_results [ res_search; res_taint ] in
  { res with skipped = skipped @ res.skipped }
