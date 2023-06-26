(* Yoann Padioleau
 *
 * Copyright (C) 2019-2021 r2c
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
open File.Operators
module FT = File_type
open Rule
module R = Rule
module E = Semgrep_error_code
module P = Pattern_match
module RP = Report
module SJ = Output_from_core_j
module Set = Set_
module Out = Output_from_core_t

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Checking the checker (metachecking).
 *
 * The goal of this module is to detect bugs, performance issues, or
 * to suggest the use of certain features in semgrep rules. This enables
 * `semgrep --check-rules`, which users can run to verify their rules
 *
 * Called via `semgrep-core -check_rules <metachecks> <files or dirs>`
 *
 * There are two kinds of checks:
 * - OCaml checks implemented directly in this file
 * - semgrep checks passed in via metachecks
 *     (by default, semgrep will call this with the rules in the rulepack
 *      https://semgrep.dev/p/semgrep-rule-lints)
 *
 * Both are necessary because semgrep checks are easier to write, especially
 * for security experts, but not all checks can be expressed with semgrep.
 * Whenever possible, add a check by contributing to p/semgrep-rule-lints
 *
 * We chose to have `-check_rules` run both kinds of checks to make the
 * logic in semgrep simpler. This way, semgrep will receive a list of errors
 * and only have to display them.
 *
 * Alternatives considered were having `-check_rules` only run the OCaml checks
 * and have semgrep call `semgrep-core -rules` on the checks
 *
 * TODO: make it possible to run `semgrep-core -check_rules` with no metachecks
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
exception No_metacheck_file of string

type env = { r : Rule.t; errors : E.error list ref }

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let error env t s =
  let loc = Tok.unsafe_loc_of_tok t in
  let _check_idTODO = "semgrep-metacheck-builtin" in
  let rule_id, _ = env.r.id in
  let err = E.mk_error ~rule_id:(Some rule_id) loc s Out.SemgrepMatchFound in
  Common.push err env.errors

(*****************************************************************************)
(* Checks *)
(*****************************************************************************)

let unknown_metavar_in_comparison env f =
  let mvar_is_ok mv mvs = Set.mem mv mvs in
  let rec collect_metavars f : MV.mvar Set.t =
    match f with
    | P { pat; pstr = pstr, _; pid = _pid } ->
        (* TODO currently this guesses that the metavariables are the strings
           that have a valid metavariable name. We should ideally have each
           matcher expose the metavariables it detects. *)
        (* First get the potential metavar ellipsis words *)
        let words_with_dot = Str.split (Str.regexp "[^a-zA-Z0-9_\\.$]") pstr in
        let ellipsis_metavars =
          words_with_dot |> List.filter Metavariable.is_metavar_ellipsis
        in
        (* Then split the individual metavariables *)
        let words = List.concat_map (String.split_on_char '.') words_with_dot in
        let metavars = words |> List.filter Metavariable.is_metavar_name in
        (* Then, for a pattern-regex, get all the named capture groups, and
           account for the metavariables introduced by their matches.
        *)
        let regexp_captured_mvars =
          match pat with
          | Xpattern.Regexp s -> Metavariable.mvars_of_regexp_string s
          | __else__ -> []
        in
        [ metavars; ellipsis_metavars; regexp_captured_mvars ]
        |> Common.map Set.of_list
        |> List.fold_left Set.union Set.empty
    | Inside (_, f) -> collect_metavars f
    | Not (_, _) -> Set.empty
    | Or (_, xs) ->
        let mv_sets = Common.map collect_metavars xs in
        List.fold_left
          (* TODO originally we took the intersection, since strictly
           * speaking a metavariable needs to be in all cases of a pattern-either
           * to be bound. However, due to how the pattern is transformed, this
           * is not always enforced, so the metacheck is too strict
           *
          (fun acc mv_set ->
            if acc == Set.empty then mv_set else Set.inter acc mv_set)
           *)
            (fun acc mv_set -> Set.union acc mv_set)
          Set.empty mv_sets
    | And (_, { conjuncts; conditions; focus }) ->
        let mv_sets = Common.map collect_metavars conjuncts in
        let mvs =
          List.fold_left
            (fun acc mv_set -> Set.union acc mv_set)
            Set.empty mv_sets
        in
        (* Check that all metavariables in this and-clause's metavariable-comparison clauses appear somewhere else *)
        let mv_error mv t =
          (* TODO make this message more helpful by detecting specific
             variants of this *)
          error env t
            (mv
           ^ " is used in a 'metavariable-*' conditional or \
              'focus-metavariable' operator but is never bound by a positive \
              pattern (or is only bound by negative patterns like \
              'pattern-not')")
        in
        conditions
        |> List.iter (fun (t, metavar_cond) ->
               match metavar_cond with
               | CondEval _ -> ()
               | CondRegexp (mv, _, _) ->
                   if not (mvar_is_ok mv mvs) then mv_error mv t
               | CondType (mv, _, _, _) ->
                   if not (mvar_is_ok mv mvs) then mv_error mv t
               | CondNestedFormula (mv, _, _) ->
                   if not (mvar_is_ok mv mvs) then mv_error mv t
               | CondAnalysis (mv, _) ->
                   if not (mvar_is_ok mv mvs) then mv_error mv t);
        focus
        |> List.iter (fun (t, mv_list) ->
               mv_list
               |> List.iter (fun mv ->
                      if not (mvar_is_ok mv mvs) then mv_error mv t));
        mvs
  in
  let _ = collect_metavars f in
  ()

(* call Check_pattern subchecker *)
let check_pattern (lang : Xlang.t) f =
  visit_new_formula
    (fun { pat; pstr = _pat_str; pid = _ } _ ->
      match (pat, lang) with
      | Sem ((lazy semgrep_pat), _lang), L (lang, _rest) ->
          Check_pattern.check lang semgrep_pat
      | Spacegrep _spacegrep_pat, LSpacegrep -> ()
      | Aliengrep _aliengrep_pat, LAliengrep -> ()
      | Regexp _, _ -> ()
      | _ -> raise Impossible)
    f

(*****************************************************************************)
(* Formula *)
(*****************************************************************************)

let check_formula env (lang : Xlang.t) f =
  check_pattern lang f;
  unknown_metavar_in_comparison env f;
  List.rev !(env.errors)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let check r =
  (* less: maybe we could also have formula_old specific checks *)
  match r.mode with
  | `Search f
  | `Extract { formula = f; _ } ->
      check_formula { r; errors = ref [] } r.languages.target_analyzer f
  | `Taint _ -> (* TODO *) []
  | `Step _ -> (* TODO *) []

let semgrep_check config metachecks rules =
  let match_to_semgrep_error m =
    let loc, _ = m.P.range_loc in
    (* TODO use the end location in errors *)
    let s = m.rule_id.message in
    let _check_id = m.rule_id.id in
    (* TODO: why not set ~rule_id here?? bug? *)
    E.mk_error ~rule_id:None loc s Out.SemgrepMatchFound
  in
  let config =
    {
      config with
      Runner_config.lang = Some (Xlang.of_lang Yaml);
      rule_source = Some (Rule_file metachecks);
      output_format = Json true;
      (* the targets are actually the rules! metachecking! *)
      roots = rules;
    }
  in
  let _success, res, _targets =
    Run_semgrep.semgrep_with_raw_results_and_exn_handler config
  in
  res.matches |> Common.map match_to_semgrep_error

(* TODO *)

(* We parse the parsing function fparser (Parser_rule.parse) to avoid
 * circular dependencies.
 * Similar to Test_parsing.test_parse_rules.
 *)
let run_checks config fparser metachecks xs =
  let yaml_xs, skipped_paths =
    xs
    |> File_type.files_of_dirs_or_files (function
         | FT.Config (FT.Yaml (*FT.Json |*) | FT.Jsonnet) -> true
         | _ -> false)
    |> Skip_code.filter_files_if_skip_list ~root:xs
  in
  let rules, more_skipped_paths =
    List.partition (fun file -> not (!!file =~ ".*\\.test\\.yaml")) yaml_xs
  in
  let _skipped_paths = more_skipped_paths @ skipped_paths in
  match rules with
  | [] ->
      logger#error
        "no valid yaml rules to run on (.test.yaml files are excluded)";
      []
  | _ ->
      let semgrep_found_errs = semgrep_check config metachecks rules in
      let ocaml_found_errs =
        rules
        |> List.concat_map (fun file ->
               logger#info "processing %s" !!file;
               try
                 let rs = fparser file in
                 rs |> List.concat_map (fun file -> check file)
               with
               (* TODO this error is special cased because YAML files that *)
               (* aren't semgrep rules are getting scanned *)
               | R.Err (R.InvalidYaml _) -> []
               | exn ->
                   let e = Exception.catch exn in
                   [ E.exn_to_error !!file e ])
      in
      semgrep_found_errs @ ocaml_found_errs

let check_files mk_config fparser input =
  let config = mk_config () in
  let errors =
    match input with
    | []
    | [ _ ] ->
        raise
          (No_metacheck_file
             "check_rules needs a metacheck file or directory and rules to run \
              on")
    | metachecks :: xs -> run_checks config fparser metachecks xs
  in
  match config.output_format with
  | Text -> List.iter (fun err -> pr2 (E.string_of_error err)) errors
  | Json _ ->
      let res = { RP.empty_final_result with errors } in
      (* for the stats.okfiles, but we don't care? *)
      let nfiles = 0 in
      let json =
        JSON_report.match_results_of_matches_and_errors None nfiles res
      in
      pr (SJ.string_of_core_match_results json)

let stat_files fparser xs =
  let fullxs, _skipped_paths =
    xs
    |> File_type.files_of_dirs_or_files (function
         | FT.Config (FT.Yaml (*FT.Json |*) | FT.Jsonnet) -> true
         | _ -> false)
    |> Skip_code.filter_files_if_skip_list ~root:xs
  in
  let good = ref 0 in
  let bad = ref 0 in
  fullxs
  |> List.iter (fun file ->
         logger#info "processing %s" !!file;
         let rs = fparser file in
         rs
         |> List.iter (fun r ->
                let res = Analyze_rule.regexp_prefilter_of_rule r in
                match res with
                | None ->
                    incr bad;
                    pr2
                      (spf "PB: no regexp prefilter for rule %s:%s" !!file
                         (fst r.id :> string))
                | Some (f, _f) ->
                    incr good;
                    let s = Semgrep_prefilter_j.string_of_formula f in
                    pr2 (spf "regexp: %s" s)));
  pr2 (spf "good = %d, no regexp found = %d" !good !bad)
