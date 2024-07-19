(* Yoann Padioleau
 *
 * Copyright (C) 2020-2023 Semgrep Inc.
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
module Out = Semgrep_output_v1_j
module E = Core_error

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
  | Some res -> Ok res
  | None ->
      let loc = Tok.first_loc_of_file !!file in
      let err = E.mk_error loc Out.Timeout in
      Error err

(* for -e/-f *)
let parse_pattern lang_pattern str =
  let error_of_string s =
    let id = Rule_ID.of_string_exn "no-id" in
    let tok = Tok.unsafe_fake_tok "no loc" in
    let xlang = Xlang.of_lang lang_pattern in
    Error
      (Rule_error.Error.mk_error
         (InvalidRule (InvalidPattern (str, xlang, s, []), id, tok)))
  in
  match Parse_pattern.parse_pattern lang_pattern str with
  | Ok pat -> Ok pat
  | Error s -> error_of_string s
  | exception exn ->
      let s = Common.exn_to_s exn in
      Logs.err (fun m -> m "exception in parse pattern: exn = %s" s);
      error_of_string s
[@@profiling]

let output_core_results (caps : < Cap.stdout ; Cap.exit >)
    (result_or_exn : Core_result.result_or_exn) (config : Core_scan_config.t) :
    unit =
  (* note: uncomment the following and use semgrep-core -stat_matches
   * to debug too-many-matches issues.
   * Common2.write_value matches "/tmp/debug_matches";
   *)
  match config.output_format with
  | Json _ -> (
      let res =
        match result_or_exn with
        | Ok r -> r
        | Error exn ->
            let err = E.exn_to_error None Fpath_.fake_file exn in
            Core_result.mk_result_with_just_errors [ err ]
      in
      let res = Core_json_output.core_output_of_matches_and_errors res in
      (*
        Not pretty-printing the json output (Yojson.Safe.prettify)
        because it kills performance, adding an extra 50% time on our
        old calculate_ci_perf.py benchmark.
        User should use an external tool like jq or ydump (latter comes with
        yojson) for pretty-printing json.
      *)
      let s = Out.string_of_core_output res in
      Logs.debug (fun m ->
          m "size of returned JSON string: %d" (String.length s));
      CapConsole.print caps#stdout s;
      match result_or_exn with
      | Error exn ->
          Core_exit_code.exit_semgrep caps#exit (Unknown_exception exn)
      | Ok _ -> ())
  | Text -> (
      match result_or_exn with
      | Ok res ->
          if config.matching_explanations then
            res.explanations
            |> Option.iter (List.iter Matching_explanation.print);
          (* the match has already been printed above. We just print errors here *)
          if not (List_.null res.errors) then (
            Logs.warn (fun m ->
                m "some files were skipped or only partially analyzed");
            res.errors
            |> List.iter (fun err ->
                   Logs.warn (fun m -> m "%s" (E.string_of_error err))))
      | Error exn -> Exception.reraise exn)

(*****************************************************************************)
(* semgrep-core -rules *)
(*****************************************************************************)

let semgrep_core_with_rules_and_formatted_output
    (caps : < Cap.stdout ; Cap.tmp ; Cap.exit >) (config : Core_scan_config.t) :
    unit =
  let res = Core_scan.scan (caps :> < Cap.tmp >) config in
  output_core_results (caps :> < Cap.stdout ; Cap.exit >) res config

(*****************************************************************************)
(* semgrep-core -e/-f *)
(*****************************************************************************)

let minirule_of_pattern lang pattern_string pattern =
  {
    Mini_rule.id = Rule_ID.of_string_exn "anon-pattern";
    pattern_string;
    pattern;
    inside = false;
    message = "";
    metadata = None;
    severity = `Error;
    langs = [ lang ];
    fix = None;
    fix_regexp = None;
  }

(* less: could be nice to generalize to rule_of_config, but we sometimes
 * need to generate a rule, sometimes a minirule
 *)
let pattern_of_config lang (config : Core_scan_config.t) =
  let pattern_string =
    match (config.pattern_file, config.pattern_string) with
    | None, None -> failwith "I need a pattern; use -f or -e"
    | Some _s1, Some _s2 ->
        failwith "I need just one pattern; use -f OR -e (not both)"
    | Some file, None -> UFile.read_file file
    (* this is for Emma, who often confuses -e with -f :) *)
    | None, Some s when s =~ ".*\\.sgrep$" ->
        failwith "you probably want -f with a .sgrep file, not -e"
    | None, Some s -> s
  in
  match parse_pattern lang pattern_string with
  | Ok pat -> (pat, pattern_string)
  | Error e -> failwith ("parsing error: " ^ Rule_error.string_of_error e)

(* simpler code path compared to scan() *)
(* FIXME: don't use a different processing logic depending on the output
   format:
   - Pass a hook to semgrep_with_patterns for printing matches incrementally.
   - Have semgrep_with_patterns return the results and errors.
   - Print the final results (json or text) using dedicated functions.
*)
let semgrep_core_with_one_pattern (caps : < Cap.stdout ; Cap.tmp >)
    (config : Core_scan_config.t) : unit =
  assert (config.rule_source =*= None);

  (* TODO: support generic and regex patterns as well. See code in Deep.
   * Just use Parse_rule.parse_xpattern xlang (str, fk)
   *)
  let lang = Xlang.lang_of_opt_xlang_exn config.lang in
  let pattern, pattern_string = pattern_of_config lang config in

  match config.output_format with
  | Json _ -> (
      let rule =
        let fk = Tok.unsafe_fake_tok "" in
        let xlang = Xlang.L (lang, []) in
        let xpat =
          Xpattern.mk_xpat (Xpattern.Sem (pattern, lang)) (pattern_string, fk)
        in
        Rule.rule_of_xpattern xlang xpat
      in
      let config = { config with rule_source = Some (Rules [ rule ]) } in
      let res = Core_scan.scan (caps :> < Cap.tmp >) config in
      match res with
      | Error exn -> Exception.reraise exn
      | Ok res ->
          let json = Core_json_output.core_output_of_matches_and_errors res in
          let s = Out.string_of_core_output json in
          CapConsole.print caps#stdout s)
  | Text ->
      let minirule, _rules_parse_time =
        Common.with_time (fun () ->
            [ minirule_of_pattern lang pattern_string pattern ])
      in
      (* simpler code path than in scan() *)
      let target_info, _skipped =
        Core_scan.targets_of_config (caps :> < Cap.tmp >) config
      in
      let files = target_info |> List_.map Target.internal_path in
      (* sanity check *)
      if config.filter_irrelevant_rules then
        Logs.warn (fun m ->
            m "-fast does not work with -f/-e, or you need also -json");
      let errors =
        files
        |> List.concat_map (fun (file : Fpath.t) ->
               Logs.info (fun m -> m "processing: %s" !!file);
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
                         Core_scan.parse_equivalences config.equivalences_file
                       )
                       minirule
                       (file, File file, lang, ast)
                     |> ignore)
               in

               match
                 if not config.error_recovery then
                   E.try_with_log_exn_and_reraise file (fun () -> process file)
                 else E.try_with_result_to_error file (fun () -> process file)
               with
               | Ok _ -> []
               | Error e -> [ e ])
      in
      let n = List.length errors in
      if n > 0 then Logs.err (fun m -> m "error count: %d" n)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let semgrep_core_dispatch (caps : Cap.all_caps) (config : Core_scan_config.t) :
    unit =
  if config.rule_source <> None then
    semgrep_core_with_rules_and_formatted_output
      (caps :> < Cap.stdout ; Cap.exit ; Cap.tmp >)
      config
  else semgrep_core_with_one_pattern (caps :> < Cap.stdout ; Cap.tmp >) config
