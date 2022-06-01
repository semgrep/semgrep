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
module RP = Report

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(*
let (lang_of_rules: Rule.t list -> Lang.t) = fun rs ->
  match rs |> Common.find_some_opt (fun r ->
    match r.R.languages with
    | R.L (l, _) -> Some l
    | _ -> None
  ) with
  | Some l -> l
  | None -> failwith "could not find a language"
*)

let (xlangs_of_rules : Rule.t list -> Xlang.t list) =
 fun rs -> rs |> Common.map (fun r -> r.R.languages) |> List.sort_uniq compare

let first_xlang_of_rules rs =
  match rs with
  | [] -> failwith "no rules"
  | { R.languages = x; _ } :: _ -> x

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let make_tests ?(unit_testing = false) xs =
  let fullxs, _skipped_paths =
    xs
    |> File_type.files_of_dirs_or_files (function
         | FT.Config FT.Yaml -> true
         (* old: we were allowing Jsonnet before, but better to skip
          * them for now to avoid adding a jsonnet dependency in our docker/CI
          * FT.Config ((* | FT.Json*) FT.Jsonnet) when not unit_testing -> true
          *)
         | _ -> false)
    |> Common.exclude (fun filepath ->
           (* .test.yaml files are YAML target files rather than config files! *)
           Filename.check_suffix filepath ".test.yaml")
    |> Skip_code.filter_files_if_skip_list ~root:xs
  in

  let newscore = Common2.empty_score () in
  let ext = "rule" in
  let total_mismatch = ref 0 in

  let tests =
    fullxs
    |> Common.map (fun file ->
           let test () =
             logger#info "processing rule file %s" file;
             let rules = Parse_rule.parse file in
             (* just a sanity check *)
             (* rules |> List.iter Check_rule.check; *)
             let xlangs = xlangs_of_rules rules in
             let xlang =
               match xlangs with
               | [] -> failwith (spf "no language found in %s" file)
               | [ x ] -> x
               | _ :: _ :: _ ->
                   let fst = first_xlang_of_rules rules in
                   pr2
                     (spf
                        "too many languages found in %s, picking the first \
                         one: %s"
                        file (Xlang.show fst));
                   fst
             in
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
               | Not_found ->
                   failwith (spf "could not find a target for %s" file)
             in
             logger#info "processing target %s" target;
             (* ugly: this is just for tests/OTHER/rules/inception2.yaml, to use JSON
              * to parse the pattern but YAML to parse the target *)
             let xlang =
               match (xlang, Lang.langs_of_filename target) with
               | L (l, [ l2 ]), xs when not (List.mem l xs) ->
                   pr2 (spf "switching to another language: %s" (Lang.show l2));
                   Xlang.L (l2, [])
               | _ -> xlang
             in

             (* expected *)
             (* not tororuleid! not ok:! *)
             let regexp = ".*\\b\\(ruleid\\|todook\\):.*" in
             let expected_error_lines =
               E.expected_error_lines_of_files ~regexp [ target ]
             in

             (* actual *)
             let lazy_ast_and_errors =
               lazy
                 (match xlang with
                 | L (lang, _) ->
                     let { Parse_target.ast; skipped_tokens; _ } =
                       Parse_target.parse_and_resolve_name lang target
                     in
                     (ast, skipped_tokens)
                 | LRegex
                 | LGeneric ->
                     assert false)
             in
             let xtarget =
               {
                 Xtarget.file = target;
                 xlang;
                 lazy_content = lazy (Common.read_file target);
                 lazy_ast_and_errors;
               }
             in
             E.g_errors := [];
             Flag_semgrep.with_opt_cache := false;
             let config = Config_semgrep.default_config in
             let res =
               try
                 Match_rules.check
                   ~match_hook:(fun _ _ _ _ -> ())
                   ~timeout:0. ~timeout_threshold:0 (config, []) rules xtarget
               with
               | exn ->
                   failwith
                     (spf "exn on %s (exn = %s)" file (Common.exn_to_s exn))
             in
             res.profiling.rule_times
             |> List.iter (fun rule_time ->
                    if not (rule_time.RP.match_time >= 0.) then
                      (* match_time could be 0.0 if the rule contains no pattern or if the
                         rules are skipped. Otherwise it's positive. *)
                      failwith
                        (spf
                           "invalid value for match time: %g (rule: %s, \
                            target: %s)"
                           rule_time.RP.match_time file target);
                    if not (rule_time.RP.parse_time >= 0.) then
                      (* same for parse time *)
                      failwith
                        (spf
                           "invalid value for parse time: %g (rule: %s, \
                            target: %s)"
                           rule_time.RP.parse_time file target));

             res.matches |> List.iter JSON_report.match_to_error;
             if not (res.errors = []) then
               failwith (spf "parsing error on %s" file);

             let actual_errors = !E.g_errors in
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
                 if unit_testing then Alcotest.fail msg
           in
           (file, test))
  in
  let print_summary () =
    if not unit_testing then
      Parse_info.print_regression_information ~ext xs newscore;
    pr2 (spf "total mismatch: %d" !total_mismatch)
  in
  (tests, print_summary)

let test_rules ?unit_testing xs =
  let tests, print_summary = make_tests ?unit_testing xs in
  tests |> List.iter (fun (_name, test) -> test ());
  print_summary ()
