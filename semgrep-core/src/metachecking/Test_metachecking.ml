(* Yoann Padioleau
 *
 * Copyright (C) 2021 r2c
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
module FT = File_type
module R = Rule
module E = Semgrep_error_code

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let config =
  {
    Runner_config.log_config_file = "";
    log_to_file = None;
    test = false;
    debug = false;
    profile = false;
    report_time = false;
    error_recovery = false;
    profile_start = 0.0;
    pattern_string = "";
    pattern_file = "";
    rules_file = "";
    extractors_file = "";
    equivalences_file = "";
    metatypes_file = "";
    lang = None;
    output_format = Text;
    match_format = Matching_report.Normal;
    mvars = [];
    lsp = false;
    timeout = 0.0;
    timeout_threshold = 0;
    max_memory_mb = 0;
    max_match_per_file = 10000;
    ncores = 1;
    parsing_cache_dir = "";
    target_file = "";
    action = "";
    version = "test";
    roots = [];
  }

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let test_rules ?(unit_testing = false) xs =
  let fullxs, _skipped_paths =
    xs
    |> File_type.files_of_dirs_or_files (function
         | FT.Config FT.Yaml -> true
         | _ -> false)
    |> Common.exclude (fun filepath ->
           (* .test.yaml files are YAML target files rather than config files! *)
           Filename.check_suffix filepath ".test.yaml"
           || Filename.check_suffix filepath ".rule.yaml")
    |> Skip_code.filter_files_if_skip_list ~root:xs
  in

  let newscore = Common2.empty_score () in
  let ext = "rule" in
  let total_mismatch = ref 0 in

  fullxs
  |> List.iter (fun file ->
         logger#info "processing rule file %s" file;

         (* just a sanity check *)
         (* rules |> List.iter Check_rule.check; *)
         let target =
           try
             let d, b, ext = Common2.dbe_of_filename file in
             Common2.readdir_to_file_list d @ Common2.readdir_to_link_list d
             |> Common.find_some (fun file2 ->
                    let path2 = Filename.concat d file2 in
                    (* Config files have a single .yaml extension (assumption),
                     * but test files may have multiple extensions, e.g.
                     * ".test.yaml" (YAML test files), ".sites-available.conf",
                     * ... *)
                    match Common2.dbe_of_filename_many_ext_opt file2 with
                    | None -> None
                    | Some (_, b2, ext2) ->
                        if
                          b = b2 && ext <> ext2
                          (* .yaml.j2 files are Jinja2 templates to generate Semgrep config files *)
                          && ext2 <> "yaml.j2"
                          (* ugly: jsonnet exclusion below because of some .jsonnet and .yaml
                           * ambiguities in tests/OTHER/rules *)
                          && ext2 <> "jsonnet"
                        then Some path2
                        else None)
           with
           | Not_found -> failwith (spf "could not find a target for %s" file)
         in
         logger#info "processing target %s" target;
         (* expected *)
         (* not tororuleid! not ok:! *)
         let regexp = ".*\\b\\(ruleid\\|todook\\):.*" in
         let expected_error_lines =
           E.expected_error_lines_of_files ~regexp [ target ]
         in

         (* actual *)
         Flag_semgrep.with_opt_cache := false;
         let actual_errors =
           try
             Check_rule.run_checks config Parse_rule.parse file [ target ]
           with
           | exn ->
               failwith (spf "exn on %s (exn = %s)" file (Common.exn_to_s exn))
         in
         actual_errors
         |> List.iter (fun e ->
                logger#info "found error: %s" (E.string_of_error e));
         match
           E.compare_actual_to_expected actual_errors expected_error_lines
         with
         | Ok () -> Hashtbl.add newscore file Common2.Ok
         | Error (num_errors, msg) ->
             pr2 msg;
             Hashtbl.add newscore file (Common2.Pb msg);
             total_mismatch := !total_mismatch + num_errors;
             if unit_testing then Alcotest.fail msg);
  if not unit_testing then
    Parse_info.print_regression_information ~ext xs newscore;
  pr2 (spf "total mismatch: %d" !total_mismatch)
