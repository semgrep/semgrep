(* Yoann Padioleau
 *
 * Copyright (C) 2021 r2c
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
open Common
module FT = File_type
module R = Rule
module E = Error_code
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

let (xlangs_of_rules : Rule.t list -> Rule.xlang list) =
 fun rs -> rs |> List.map (fun r -> r.R.languages) |> List.sort_uniq compare

let first_xlang_of_rules rs =
  match rs with [] -> failwith "no rules" | { R.languages = x; _ } :: _ -> x

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let test_rules ?(ounit_context = false) xs =
  let fullxs =
    xs
    |> File_type.files_of_dirs_or_files (function
         | FT.Config FT.Yaml -> true
         (* old: we were allowing Jsonnet before, but better to skip
          * them for now to avoid adding a jsonnet dependency in our docker/CI
          * FT.Config ((* | FT.Json*) FT.Jsonnet) when not ounit_context -> true
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

  fullxs
  |> List.iter (fun file ->
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
                    "too many languages found in %s, picking the first one: %s"
                    file (Rule.show_xlang fst));
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
           with Not_found ->
             failwith (spf "could not find a target for %s" file)
         in
         logger#info "processing target %s" target;
         (* ugly: this is just for tests/OTHER/rules/inception2.yaml, to use JSON
          * to parse the pattern but YAML to parse the target *)
         let xlang =
           match (xlang, Lang.langs_of_filename target) with
           | R.L (l, [ l2 ]), xs when not (List.mem l xs) ->
               pr2 (spf "switching to another language: %s" (Lang.show l2));
               R.L (l2, [])
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
             ( match xlang with
             | R.L (lang, _) ->
                 let { Parse_target.ast; errors; _ } =
                   Parse_target.parse_and_resolve_name_use_pfff_or_treesitter
                     lang target
                 in
                 (ast, errors)
             | R.LNone | R.LGeneric -> raise Impossible )
         in
         E.g_errors := [];
         Flag_semgrep.with_opt_cache := false;
         let config = Config_semgrep.default_config in
         let res =
           try
             Match_rules.check
               (fun _ _ _ -> ())
               config rules []
               (target, xlang, lazy_ast_and_errors)
           with exn ->
             failwith (spf "exn on %s (exn = %s)" file (Common.exn_to_s exn))
         in
         if not (res.profiling.match_time >= 0.) then
           (* match_time could be 0.0 if the rule contains no pattern or if the
              rules are skipped. Otherwise it's positive. *)
           failwith
             (spf "invalid value for match time: %g" res.profiling.match_time);
         if not (res.profiling.parse_time >= 0.) then
           (* same for parse time *)
           failwith
             (spf "invalid value for parse time: %g" res.profiling.parse_time);

         res.matches |> List.iter JSON_report.match_to_error;
         if not (res.errors = []) then failwith (spf "parsing error on %s" file);

         let actual_errors = !E.g_errors in
         actual_errors
         |> List.iter (fun e ->
                logger#info "found error: %s" (E.string_of_error e));
         try
           E.compare_actual_to_expected actual_errors expected_error_lines;
           Hashtbl.add newscore file Common2.Ok
         with OUnitTest.OUnit_failure s when not ounit_context ->
           pr2 s;
           Hashtbl.add newscore file (Common2.Pb s);
           (* coupling: ugly: with Error_code.compare_actual_to_expected *)
           if
             s
             =~ "it should find all reported errors and no more (\\([0-9]+\\) \
                 errors)"
           then
             let n = Common.matched1 s |> int_of_string in
             total_mismatch := !total_mismatch + n
           else failwith (spf "wrong unit failure format: %s" s));
  if not ounit_context then (
    Parse_info.print_regression_information ~ext xs newscore;
    pr2 (spf "total mismatch: %d" !total_mismatch) );
  ()
