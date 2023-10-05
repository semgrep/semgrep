(* Yoann Padioleau
 *
 * Copyright (C) 2020-2023 r2c
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
module Out = Semgrep_output_v1_j
module E = Core_error

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Semgrep-core scan logic. The command-line parsing part is now done in
 * Core_CLI.ml. Here the functions are all passed a Core_scan_config.t
 * structure.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let timeout_function file timeout f =
  let timeout = if timeout <= 0. then None else Some timeout in
  match
    Time_limit.set_timeout_opt ~name:"Run_semgrep.timeout_function" timeout f
  with
  | Some res -> res
  | None ->
      let loc = Tok.first_loc_of_file file in
      let err = E.mk_error None loc "" Out.Timeout in
      Common.push err E.g_errors

(* for -e/-f *)
let parse_pattern lang_pattern str =
  try Parse_pattern.parse_pattern lang_pattern ~print_errors:false str with
  | exn ->
      logger#error "parse_pattern: exn = %s" (Common.exn_to_s exn);
      Rule.raise_error None
        (InvalidRule
           ( InvalidPattern
               (str, Xlang.of_lang lang_pattern, Common.exn_to_s exn, []),
             Rule_ID.of_string "no-id",
             Tok.unsafe_fake_tok "no loc" ))
  [@@profiling]

let output_core_results (result_or_exn : Core_result.result_or_exn)
    (config : Core_scan_config.t) : unit =
  (* note: uncomment the following and use semgrep-core -stat_matches
   * to debug too-many-matches issues.
   * Common2.write_value matches "/tmp/debug_matches";
   *)
  match config.output_format with
  | Json _ -> (
      let res =
        match result_or_exn with
        | Ok r -> r
        | Error (exn, core_error_opt) ->
            let err =
              match core_error_opt with
              | Some err -> err
              | None -> E.exn_to_error None "" exn
            in
            Core_result.mk_final_result_with_just_errors [ err ]
      in
      let res =
        Core_json_output.core_output_of_matches_and_errors
          (Some Autofix.render_fix) res
      in
      (* one-off experiment, delete it at some point (March 2023) *)
      let res =
        if !Flag_semgrep.raja then Raja_experiment.adjust_core_output res
        else res
      in
      (*
        Not pretty-printing the json output (Yojson.Safe.prettify)
        because it kills performance, adding an extra 50% time on our
        old calculate_ci_perf.py benchmark.
        User should use an external tool like jq or ydump (latter comes with
        yojson) for pretty-printing json.
      *)
      let s = Out.string_of_core_output res in
      logger#info "size of returned JSON string: %d" (String.length s);
      pr s;
      match result_or_exn with
      | Error (e, _) -> Core_exit_code.exit_semgrep (Unknown_exception e)
      | Ok _ -> ())
  | Text -> (
      match result_or_exn with
      | Ok res ->
          if config.matching_explanations then
            res.explanations
            |> Option.iter (List.iter Matching_explanation.print);
          (* the match has already been printed above. We just print errors here *)
          if not (null res.errors) then (
            pr "WARNING: some files were skipped or only partially analyzed:";
            res.errors |> List.iter (fun err -> pr (E.string_of_error err)))
      | Error (exn, _) -> Exception.reraise exn)

(*****************************************************************************)
(* semgrep-core -rules *)
(*****************************************************************************)

let semgrep_core_with_rules_and_formatted_output (config : Core_scan_config.t) :
    unit =
  let res = Core_scan.scan_with_exn_handler config in
  output_core_results res config

(*****************************************************************************)
(* semgrep-core -e/-f *)
(*****************************************************************************)

let minirule_of_pattern lang pattern_string pattern =
  {
    Mini_rule.id = Rule_ID.of_string "anon-pattern";
    pattern_string;
    pattern;
    inside = false;
    message = "";
    severity = Error;
    langs = [ lang ];
    fix = None;
  }

(* less: could be nice to generalize to rule_of_config, but we sometimes
 * need to generate a rule, sometimes a minirule
 *)
let pattern_of_config lang (config : Core_scan_config.t) =
  match (config.pattern_file, config.pattern_string) with
  | None, None -> failwith "I need a pattern; use -f or -e"
  | Some _s1, Some _s2 ->
      failwith "I need just one pattern; use -f OR -e (not both)"
  | Some file, None ->
      let s = File.read_file file in
      (parse_pattern lang s, s)
  (* this is for Emma, who often confuses -e with -f :) *)
  | None, Some s when s =~ ".*\\.sgrep$" ->
      failwith "you probably want -f with a .sgrep file, not -e"
  | None, Some s -> (parse_pattern lang s, s)

(* simpler code path compared to scan() *)
(* FIXME: don't use a different processing logic depending on the output
   format:
   - Pass a hook to semgrep_with_patterns for printing matches incrementally.
   - Have semgrep_with_patterns return the results and errors.
   - Print the final results (json or text) using dedicated functions.
*)
let semgrep_core_with_one_pattern (config : Core_scan_config.t) : unit =
  assert (config.rule_source =*= None);

  (* TODO: support generic and regex patterns as well. See code in Deep.
   * Just use Parse_rule.parse_xpattern xlang (str, fk)
   *)
  let lang = Xlang.lang_of_opt_xlang_exn config.lang in
  let pattern, pattern_string = pattern_of_config lang config in

  match config.output_format with
  | Json _ ->
      let rule, rules_parse_time =
        Common.with_time (fun () ->
            let fk = Tok.unsafe_fake_tok "" in
            let xlang = Xlang.L (lang, []) in
            let xpat =
              Xpattern.mk_xpat
                (Xpattern.Sem (lazy pattern, lang))
                (pattern_string, fk)
            in
            Rule.rule_of_xpattern xlang xpat)
      in
      let res = Core_scan.scan config (([ rule ], []), rules_parse_time) in
      let json =
        Core_json_output.core_output_of_matches_and_errors
          (Some Autofix.render_fix) res
      in
      let s = Out.string_of_core_output json in
      pr s
  | Text ->
      let minirule, _rules_parse_time =
        Common.with_time (fun () ->
            [ minirule_of_pattern lang pattern_string pattern ])
      in
      (* simpler code path than in scan() *)
      let target_info, _skipped = Core_scan.targets_of_config config in
      let files =
        target_info |> Common.map (fun (t : Input_to_core_t.target) -> t.path)
      in
      (* sanity check *)
      if config.filter_irrelevant_rules then
        logger#warning "-fast does not work with -f/-e, or you need also -json";
      files
      |> List.iter (fun file ->
             logger#info "processing: %s" file;
             let process file =
               timeout_function file config.timeout (fun () ->
                   let ast =
                     Parse_target.parse_and_resolve_name_warn_if_partial lang
                       file
                   in
                   Match_patterns.check
                     ~hook:(fun match_ ->
                       Core_scan.print_match config match_
                         Metavariable.ii_of_mval)
                     ( Rule_options.default_config,
                       Core_scan.parse_equivalences config.equivalences_file )
                     minirule (file, lang, ast)
                   |> ignore)
             in

             if not config.error_recovery then
               E.try_with_print_exn_and_reraise file (fun () -> process file)
             else E.try_with_exn_to_error file (fun () -> process file));

      let n = List.length !E.g_errors in
      if n > 0 then pr2 (spf "error count: %d" n)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let semgrep_core_dispatch (config : Core_scan_config.t) : unit =
  if config.rule_source <> None then
    semgrep_core_with_rules_and_formatted_output config
  else semgrep_core_with_one_pattern config
