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

module R = Rule
module RP = Report

let lazy_force x = Lazy.force x [@@profiling]

let check_taint hook default_config taint_rules equivs file_and_more =
  match taint_rules with
  | [] -> RP.empty_semgrep_result
  | __else__ ->
      let file, _xlang, lazy_ast_and_errors = file_and_more in
      let (ast, errors), parse_time =
        Common.with_time (fun () -> lazy_force lazy_ast_and_errors)
      in
      let matches, match_time =
        Common.with_time (fun () ->
            Tainting_generic.check hook default_config taint_rules equivs file
              ast)
      in
      { RP.matches; errors; profiling = { RP.parse_time; match_time } }

let check hook default_config rules equivs file_and_more =
  let search_rules, taint_rules = R.partition_rules rules in
  let res_search =
    Match_rules.check hook default_config search_rules equivs file_and_more
  in
  let res_taint =
    check_taint hook default_config taint_rules equivs file_and_more
  in
  RP.collate_semgrep_results [ res_search; res_taint ]
