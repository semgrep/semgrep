(* Yoann Padioleau
 *
 * Copyright (C) 2021-2023 Semgrep Inc.
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
module FT = File_type
module R = Rule
module E = Core_error
module RP = Core_result
module In = Input_to_core_t

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This module allows to use semgrep-core -test_rules <files_or_dirs>
 * to automatically run all the semgrep rules in yaml files or directories
 * and make sure they match correctly (as specified by the special ruleid:
 * comment in it).
 *
 * This is also now used for regression testing as part of 'make test' in
 * semgrep-core. See Unit_engine.full_rule_semgrep_rules_regression_tests().
 *
 * This module provides a service similar to what semgrep --test provides,
 * but without requiring the Python wrapper. It is also significantly
 * faster than semgrep --test (not sure why).
 *
 * LATER: merge with osemgrep Test_subcommand.ml
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let (xlangs_of_rules : Rule.t list -> Xlang.t list) =
 fun rs ->
  rs |> List_.map (fun r -> r.R.target_analyzer) |> List.sort_uniq compare

let first_xlang_of_rules (rs : Rule.t list) : Xlang.t =
  match rs with
  | [] -> failwith "no rules"
  | { R.target_analyzer = x; _ } :: _ -> x

let single_xlang_from_rules file rules =
  let xlangs = xlangs_of_rules rules in
  match xlangs with
  | [] -> failwith (spf "no language found in %s" !!file)
  | [ x ] -> x
  | _ :: _ :: _ ->
      let fst = first_xlang_of_rules rules in
      pr2
        (spf "too many languages found in %s, picking the first one: %s" !!file
           (Xlang.show fst));
      fst

let find_target_of_yaml_file_opt file =
  let d, b, ext = Common2.dbe_of_filename file in
  Common2.readdir_to_file_list d @ Common2.readdir_to_link_list d
  |> Common.find_some_opt (fun file2 ->
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
               (* .yaml.j2 are Jinja2 templates to generate Semgrep files *)
               && ext2 <> "yaml.j2"
               (* those are autofix test files that should be skipped *)
               && (not (ext2 =~ ".*fixed"))
               (* ugly: jsonnet exclusion below because of some .jsonnet and
                * .yaml ambiguities in tests/rules
                *)
               && ext2 <> "jsonnet"
             then Some path2
             else None)

let find_target_of_yaml_file file =
<<<<<<< HEAD
  try
    let d, b, ext = Common2.dbe_of_filename file in
    Common2.readdir_to_file_list d @ Common2.readdir_to_link_list d
    |> List_.find_some (fun file2 ->
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
                 (* .yaml.j2 are Jinja2 templates to generate Semgrep files *)
                 && ext2 <> "yaml.j2"
                 (* those are autofix test files that should be skipped *)
                 && (not (ext2 =~ ".*fixed"))
                 (* ugly: jsonnet exclusion below because of some .jsonnet and
                  * .yaml ambiguities in tests/rules
                  *)
                 && ext2 <> "jsonnet"
               then Some path2
               else None)
  with
  | Not_found -> failwith (spf "could not find a target for %s" file)
=======
  match find_target_of_yaml_file_opt file with
  | Some x -> x
  | None -> failwith (spf "could not find a target for %s" file)
>>>>>>> 10801f357 (Work around missing target files needed earlier than previously anticipated)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let make_test_rule_file ~unit_testing ~get_xlang ~prepend_lang ~newscore
    ~total_mismatch file =
  let test () =
    logger#info "processing rule file %s" !!file;
    match Parse_rule.parse file with
    | [] -> logger#info "file %s is empty or all rules were skipped" !!file
    | rules -> (
        (* just a sanity check *)
        (* rules |> List.iter Check_rule.check; *)
        let xlang =
          match get_xlang with
          | Some fn -> fn file rules
          | None -> single_xlang_from_rules file rules
        in
        let target = find_target_of_yaml_file !!file |> Fpath.v in
        logger#info "processing target %s" !!target;
        (* ugly: this is just for tests/rules/inception2.yaml, to use JSON
           to parse the pattern but YAML to parse the target *)
        let xlang =
          match (xlang, Lang.langs_of_filename target) with
          | L (l, [ l2 ]), xs when not (List.mem l xs) ->
              pr2 (spf "switching to another language: %s" (Lang.show l2));
              Xlang.L (l2, [])
          | _ -> xlang
        in

        (* expected *)
        (* not tororuleid! not ok:! not todook:
           see https://semgrep.dev/docs/writing-rules/testing-rules/
           for the meaning of those labels.
        *)
        let regexp = ".*\\b\\(ruleid\\|todook\\):.*" in
        let expected_error_lines =
          E.expected_error_lines_of_files ~regexp [ !!target ]
        in

        (* actual *)
        let lazy_ast_and_errors =
          lazy
            (match xlang with
            | L (lang, _) ->
                let { Parsing_result2.ast; skipped_tokens; _ } =
                  Parse_target.parse_and_resolve_name lang !!target
                in
                (ast, skipped_tokens)
            | LRegex
            | LSpacegrep
            | LAliengrep ->
                assert false)
        in
        let xtarget =
          {
            Xtarget.file = target;
            xlang;
            lazy_content = lazy (File.read_file target);
            lazy_ast_and_errors;
          }
        in
        E.g_errors := [];
        Core_profiling.mode := MTime;
        let rules, extract_rules =
          Either_.partition_either
            (fun r ->
              match r.Rule.mode with
              | `Extract _ as e -> Right { r with mode = e }
              | mode -> Left { r with mode })
            rules
        in
        (* coupling: This is basically duplicated from Core_scan
           TODO we should test extract mode through integration tests
           rather than duplicating all this *)
        let (extracted_targets : Extract.extracted_target_and_adjuster list) =
          Match_extract_mode.extract
            ~match_hook:(fun _ _ -> ())
            ~timeout:0. ~timeout_threshold:0 extract_rules xtarget
        in
        let adjusters =
          Extract.adjusters_of_extracted_targets extracted_targets
        in
        let in_targets : In.target list =
          extracted_targets
          |> List_.map
               (fun Extract.{ extracted = Extracted path; analyzer; _ } ->
                 { In.path = !!path; analyzer; products = Product.all })
        in

        let xconf = Match_env.default_xconfig in
        let res =
          try
            (* TODO: Maybe we should be using Run_semgrep running the same
               functions as for Semgrep CLI. *)
            Match_rules.check
              ~match_hook:(fun _ _ -> ())
              ~timeout:0. ~timeout_threshold:0 xconf rules xtarget
          with
          | exn ->
              failwith (spf "exn on %s (exn = %s)" !!file (Common.exn_to_s exn))
        in
        (* Check that the result can be marshalled, as this will be needed
           when using Parmap! See PA-1724. *)
        (try Marshal.to_string res [ Marshal.Closures ] |> ignore with
        | exn ->
            failwith (spf "exn on %s (exn = %s)" !!file (Common.exn_to_s exn)));
        let eres =
          try
            in_targets
            |> List_.map (fun (t : In.target) ->
                   let file = t.path in
                   let xlang = t.analyzer in
                   let lazy_ast_and_errors =
                     lazy
                       (match xlang with
                       | L (lang, _) ->
                           let { Parsing_result2.ast; skipped_tokens; _ } =
                             Parse_target.parse_and_resolve_name lang file
                           in
                           (ast, skipped_tokens)
                       | LRegex
                       | LSpacegrep
                       | LAliengrep ->
                           assert false)
                   in
                   let xtarget =
                     {
                       Xtarget.file = Fpath.v file;
                       xlang;
                       lazy_content = lazy (Common.read_file file);
                       lazy_ast_and_errors;
                     }
                   in
                   let matches =
                     Match_rules.check
                       ~match_hook:(fun _ _ -> ())
                       ~timeout:0. ~timeout_threshold:0 xconf rules xtarget
                   in
                   (* adjust the match location for extracted files *)
                   match
                     Hashtbl.find_opt adjusters.loc_adjuster
                       (Extracted (Fpath.v file))
                   with
                   | Some match_result_loc_adjuster ->
                       match_result_loc_adjuster matches
                   | None -> matches)
          with
          | exn ->
              failwith (spf "exn on %s (exn = %s)" !!file (Common.exn_to_s exn))
        in
        res :: eres
        |> List.iter (fun (res : Core_result.matches_single_file) ->
               match res.extra with
               | Debug _
               | No_info ->
                   failwith
                     "Impossible; type of res should match Report.mode, which \
                      we force to be MTime"
               | Time { profiling } ->
                   profiling.rule_times
                   |> List.iter
                        (fun (rule_time : Core_profiling.rule_profiling) ->
                          if not (rule_time.match_time >= 0.) then
                            (* match_time could be 0.0 if the rule contains no
                               pattern or if the
                               rules are skipped. Otherwise it's positive. *)
                            failwith
                              (spf
                                 "invalid value for match time: %g (rule: %s, \
                                  target: %s)"
                                 rule_time.match_time !!file !!target);
                          if not (rule_time.parse_time >= 0.) then
                            (* same for parse time *)
                            failwith
                              (spf
                                 "invalid value for parse time: %g (rule: %s, \
                                  target: %s)"
                                 rule_time.parse_time !!file !!target)));
        res :: eres
        |> List.iter (fun (res : Core_result.matches_single_file) ->
               res.matches |> List.iter Core_json_output.match_to_push_error);
        (if not (E.ErrorSet.is_empty res.errors) then
           let errors =
             E.ErrorSet.elements res.errors
             |> List_.map Core_error.show |> String.concat "-----\n"
           in
           failwith (spf "parsing error(s) on %s:\n%s" !!file errors));
        let actual_errors = !E.g_errors in
        E.g_errors := [];
        actual_errors
        |> List.iter (fun e ->
               logger#info "found error: %s" (E.string_of_error e));
        match
          E.compare_actual_to_expected actual_errors expected_error_lines
        with
        | Ok () -> Hashtbl.add newscore !!file Common2.Ok
        | Error (num_errors, msg) ->
            pr2 msg;
            pr2 "---";
            Hashtbl.add newscore !!file (Common2.Pb msg);
            total_mismatch := !total_mismatch + num_errors;
            if unit_testing then Alcotest.fail msg)
  in
  match !!file |> find_target_of_yaml_file_opt with
  | Some target_path ->
      (* This assumes we can guess the target programming language
         from the file extension. *)
      let langs = target_path |> Fpath.v |> Lang.langs_of_filename in
      let tags = Test_tags.tags_of_langs langs in
      let name =
        if prepend_lang then
          let langs =
            match langs with
            | [] -> [ "Generic" ]
            | _ -> List_.map Lang.to_capitalized_alnum langs
          in
          let lang = langs |> String.concat " " in
          spf "%s %s" lang !!file
        else !!file
      in
      Alcotest_ext.create ~tags name test
  | None ->
      (* TODO: mark the test as xfail (expected to fail) instead of skipped
         and add "missing target file" as the reason *)
      let name = spf "Missing target file for rule file %s" !!file in
      Alcotest_ext.create ~skipped:true name test

let make_tests ?(unit_testing = false) ?(get_xlang = None)
    ?(prepend_lang = false) xs =
  let fullxs, _skipped_paths =
    xs |> File.files_of_dirs_or_files_no_vcs_nofilter
    |> List.filter Parse_rule.is_valid_rule_filename
    |> Skip_code.filter_files_if_skip_list ~root:xs
  in

  let newscore = Common2.empty_score () in
  let ext = "rule" in
  let total_mismatch = ref 0 in

  let tests =
    fullxs
    |> List_.map
         (make_test_rule_file ~unit_testing ~get_xlang ~prepend_lang ~newscore
            ~total_mismatch)
  in
  let print_summary () =
    if not unit_testing then
      Parsing_stat.print_regression_information ~ext xs newscore;
    pr2 (spf "total mismatch: %d" !total_mismatch)
  in
  (tests, total_mismatch, print_summary)

let test_rules ?unit_testing xs =
  let paths = Fpath_.of_strings xs in
  let tests, total_mismatch, print_summary = make_tests ?unit_testing paths in
  tests |> List.iter (fun (test : Alcotest_ext.test) -> test.func ());
  print_summary ();
  if !total_mismatch > 0 then exit 1
