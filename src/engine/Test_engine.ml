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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This module allows to use semgrep-core -test_rules <files_or_dirs>
 * to automatically run all the semgrep rules in yaml files or directories
 * and make sure they match correctly (as specified by the special 'ruleid:'
 * comment in it).
 *
 * This is also now used for regression testing as part of 'make test' in
 * semgrep-core. See Unit_engine.full_rule_semgrep_rules_regression_tests().
 *
 * This module provides a service similar to what semgrep --test provides,
 * but without requiring the Python wrapper. It is also significantly
 * faster than semgrep --test.
 *
 * LATER: merge with osemgrep Test_subcommand.ml
 *)

(*****************************************************************************)
(* Xlang helpers *)
(*****************************************************************************)

let (xlangs_of_rules : Rule.t list -> Xlang.t list) =
 fun rs ->
  rs |> List_.map (fun r -> r.R.target_analyzer) |> List.sort_uniq compare

let first_xlang_of_rules (rs : Rule.t list) : Xlang.t =
  match rs with
  | [] -> failwith "no rules"
  | { R.target_analyzer = x; _ } :: _ -> x

let single_xlang_from_rules (file : Fpath.t) (rules : Rule.t list) : Xlang.t =
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

(*****************************************************************************)
(* Xtarget helpers *)
(*****************************************************************************)

let xtarget_of_target (xlang : Xlang.t) (target : Fpath.t) : Xtarget.t =
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
  {
    Xtarget.file = target;
    xlang;
    lazy_content = lazy (UFile.read_file target);
    lazy_ast_and_errors;
  }

(*****************************************************************************)
(* target helpers *)
(*****************************************************************************)

let find_target_of_yaml_file_opt (file : Fpath.t) : Fpath.t option =
  let d, b, ext = Common2.dbe_of_filename !!file in
  Common2.readdir_to_file_list d @ Common2.readdir_to_link_list d
  |> List_.find_some_opt (fun file2 ->
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
             then Some (Fpath.v path2)
             else None)

let find_target_of_yaml_file (file : Fpath.t) : Fpath.t =
  match find_target_of_yaml_file_opt file with
  | Some x -> x
  | None -> failwith (spf "could not find a target for %s" !!file)

(*****************************************************************************)
(* Other helpers *)
(*****************************************************************************)

let test_name_for_target ~prepend_lang (langs : Language.t list)
    (rule_file : Fpath.t) : string =
  if prepend_lang then
    let langs =
      match langs with
      | [] -> [ "Generic" ]
      | _ -> List_.map Lang.to_capitalized_alnum langs
    in
    let lang = langs |> String.concat " " in
    spf "%s %s" lang !!rule_file
  else !!rule_file

(*****************************************************************************)
(* Checks *)
(*****************************************************************************)

(* Check that the result can be marshalled, as this will be needed
   when using Parmap! See PA-1724.
*)
let check_can_marshall (rule_file : Fpath.t) (res : RP.matches_single_file) :
    unit =
  try Marshal.to_string res [ Marshal.Closures ] |> ignore with
  | exn ->
      failwith (spf "exn on %s (exn = %s)" !!rule_file (Common.exn_to_s exn))

let check_profiling (rule_file : Fpath.t) (target : Fpath.t)
    (res : RP.matches_single_file) : unit =
  match res.extra with
  | Debug _
  | No_info ->
      failwith
        "Impossible; type of res should match Report.mode, which we force to \
         be MTime"
  | Time { profiling } ->
      profiling.rule_times
      |> List.iter (fun (rule_time : Core_profiling.rule_profiling) ->
             if not (rule_time.match_time >= 0.) then
               (* match_time could be 0.0 if the rule contains no pattern or if the
                  rules are skipped. Otherwise it's positive.
               *)
               failwith
                 (spf "invalid value for match time: %g (rule: %s, target: %s)"
                    rule_time.match_time !!rule_file !!target);
             if not (rule_time.parse_time >= 0.) then
               (* same for parse time *)
               failwith
                 (spf "invalid value for parse time: %g (rule: %s, target: %s)"
                    rule_time.parse_time !!rule_file !!target))

let check_parse_errors (rule_file : Fpath.t) (errors : Core_error.ErrorSet.t) :
    unit =
  if not (E.ErrorSet.is_empty errors) then
    let errors =
      E.ErrorSet.elements errors |> List_.map Core_error.show
      |> String.concat "-----\n"
    in
    failwith (spf "parsing error(s) on %s:\n%s" !!rule_file errors)

(*****************************************************************************)
(* Extract mode special handling *)
(*****************************************************************************)

let run_check_for_extract_rules (extract_rules : Rule.extract_rule list)
    (rules : Rule.t list) (rule_file : Fpath.t) (xtarget : Xtarget.t) :
    RP.matches_single_file list =
  (* coupling: This is basically duplicated from Core_scan
     TODO we should test extract mode through integration tests
     rather than duplicating all this
  *)
  let (extracted_targets : Extract.extracted_target_and_adjuster list) =
    Match_extract_mode.extract
      ~match_hook:(fun _ _ -> ())
      ~timeout:0. ~timeout_threshold:0 extract_rules xtarget
  in
  let adjusters = Extract.adjusters_of_extracted_targets extracted_targets in
  try
    extracted_targets
    |> List_.map
         (fun Extract.{ extracted = Extracted file; analyzer = xlang; _ } ->
           let xtarget = xtarget_of_target xlang file in
           let xconf = Match_env.default_xconfig in
           Match_rules.check
             ~match_hook:(fun _ _ -> ())
             ~timeout:0. ~timeout_threshold:0 xconf rules xtarget
           |> Extract.adjust_location_extracted_targets_if_needed adjusters file)
  with
  | exn ->
      failwith (spf "exn on %s (exn = %s)" !!rule_file (Common.exn_to_s exn))

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

let make_test_rule_file ?(fail_callback = fun _i m -> Alcotest.fail m)
    ?(get_xlang = single_xlang_from_rules) ?(prepend_lang = false)
    (rule_file : Fpath.t) : unit Alcotest_ext.t =
  let test () =
    Logs.info (fun m -> m "processing rule file %s" !!rule_file);
    let rules = Parse_rule.parse rule_file in
    (* TODO? sanity check rules |> List.iter Check_rule.check; *)
    if rules =*= [] then
      Logs.err (fun m ->
          m "file %s is empty or all rules were skipped" !!rule_file)
    else
      let xlang = get_xlang rule_file rules in
      let target = find_target_of_yaml_file rule_file in
      Logs.info (fun m -> m "processing target %s" !!target);

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
      let xtarget = xtarget_of_target xlang target in
      let xconf = Match_env.default_xconfig in
      let rules, extract_rules = Extract.partition_rules rules in

      E.g_errors := [];
      Core_profiling.mode := MTime;
      let res =
        try
          (* !!!!let's go!!!! *)
          Match_rules.check
            ~match_hook:(fun _ _ -> ())
            ~timeout:0. ~timeout_threshold:0 xconf rules xtarget
        with
        | exn ->
            failwith
              (spf "exn on %s (exn = %s)" !!rule_file (Common.exn_to_s exn))
      in
      check_can_marshall rule_file res;
      check_parse_errors rule_file res.errors;
      let eres =
        run_check_for_extract_rules extract_rules rules rule_file xtarget
      in

      res :: eres
      |> List.iter (fun (res : Core_result.matches_single_file) ->
             check_profiling rule_file target res;
             res.matches |> List.iter Core_json_output.match_to_push_error);
      let actual_errors = !E.g_errors in
      E.g_errors := [];
      actual_errors
      |> List.iter (fun e ->
             Logs.debug (fun m -> m "found error: %s" (E.string_of_error e)));
      match E.compare_actual_to_expected actual_errors expected_error_lines with
      | Ok () -> ()
      | Error (num_errors, msg) ->
          pr2 msg;
          pr2 "---";
          fail_callback num_errors msg
  in

  (* end of let test () *)
  match find_target_of_yaml_file_opt rule_file with
  | Some target_path ->
      (* This assumes we can guess the target programming language
         from the file extension. *)
      let langs = Lang.langs_of_filename target_path in
      let tags = Test_tags.tags_of_langs langs in
      let name = test_name_for_target ~prepend_lang langs rule_file in
      Alcotest_ext.create ~tags name test
  | None ->
      (* TODO: mark the test as xfail (expected to fail) instead of skipped
         and add "missing target file" as the reason *)
      let name = spf "Missing target file for rule file %s" !!rule_file in
      Alcotest_ext.create ~skipped:true name test

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let make_tests ?fail_callback ?get_xlang ?prepend_lang (xs : Fpath.t list) :
    unit Alcotest_ext.t list =
  xs |> UFile.files_of_dirs_or_files_no_vcs_nofilter
  |> List.filter Parse_rule.is_valid_rule_filename
  |> List_.map (make_test_rule_file ?fail_callback ?get_xlang ?prepend_lang)

(* semgrep-core -test_rules *)
let test_rules (paths : Fpath.t list) : unit =
  let total_mismatch = ref 0 in
  let fail_callback num_errors _msg =
    total_mismatch := !total_mismatch + num_errors
  in
  let tests = make_tests ~fail_callback paths in
  tests |> List.iter (fun (test : Alcotest_ext.test) -> test.func ());
  pr2 (spf "total mismatch: %d" !total_mismatch);
  if !total_mismatch > 0 then exit 1
[@@action]
