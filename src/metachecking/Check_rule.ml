(* Yoann Padioleau
 *
 * Copyright (C) 2019-2024 Semgrep Inc.
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
open Rule
module R = Rule
module E = Core_error
module PM = Core_match
module RP = Core_result
module SJ = Semgrep_output_v1_j
module Set = Set_
module Out = Semgrep_output_v1_t

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
 * TODO: merge code with `osemgrep validate`
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
exception No_metacheck_file of string

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let error (rule : Rule.t) (t : Tok.t) (s : string) : Core_error.t =
  let loc =
    match Tok.loc_of_tok t with
    | Ok x -> Some x
    (* TODO? Logs? *)
    | Error _ -> None
  in
  let _check_idTODO = "semgrep-metacheck-builtin" in
  let rule_id, _ = rule.id in
  E.mk_error ~rule_id ~msg:s ?loc Out.SemgrepMatchFound

(*****************************************************************************)
(* Checks *)
(*****************************************************************************)

let mv_error env mv t =
  (* TODO make this message more helpful by detecting specific
     variants of this *)
  error env t
    (mv
   ^ " is used in a 'metavariable-*' conditional or 'focus-metavariable' \
      operator but is never bound by a positive pattern (or is only bound by \
      negative patterns like 'pattern-not')")

let mvar_is_ok mv mvs =
  (* TODO: remove first condition when we kill numeric capture groups *)
  Mvar.is_metavar_for_capture_group mv || Set.mem mv mvs

let check_mvars_of_condition env bound_mvs (t, condition) =
  match condition with
  | CondEval _ -> []
  | CondRegexp (mv, _, _)
  | CondType (mv, _, _, _)
  | CondName { mvar = mv; _ }
  | CondNestedFormula (mv, _, _)
  | CondAnalysis (mv, _) ->
      if not (mvar_is_ok mv bound_mvs) then [ mv_error env mv t ] else []

let check_mvars_of_focus r bound_mvs (t, mv_list) =
  mv_list
  |> List.concat_map (fun mv ->
         if not (mvar_is_ok mv bound_mvs) then [ mv_error r mv t ] else [])

let unknown_metavar_in_comparison r f =
  let rec collect_metavars parent_mvs { f; conditions; focus; fix = _; as_ } :
      Mvar.t Set.t * Core_error.t list =
    (* Check the metavariables in the conditions (e.g. metavariable-pattern).
       From here on, both the metavariables from the conjuncts and the
       metavariables from the parent are already bound *)
    let inner_mvs, inner_errors = collect_metavars' parent_mvs f in
    let bound_mvs_for_conds = Set.union inner_mvs parent_mvs in
    let errors =
      conditions
      |> List.concat_map (check_mvars_of_condition r bound_mvs_for_conds)
    in
    (* Now collect the metavariables defined in the conditions, which could
       be used in the focus-metavariable clauses, and check nested formulas *)
    let cond_mvs, cond_errors =
      conditions
      |> List_.map (fun (_, condition) ->
             match condition with
             | CondEval _
             | CondType _
             | CondAnalysis _
             | CondName _ ->
                 (Set.empty, [])
             | CondRegexp (_, regex, _) ->
                 (Mvar.mvars_of_regexp_string regex |> Set_.of_list, [])
             | CondNestedFormula (_, _, formula) ->
                 collect_metavars bound_mvs_for_conds formula)
      |> List.fold_left
           (fun (mvars_acc, errors_acc) (mvars, errors) ->
             (Set.union mvars_acc mvars, errors @ errors_acc))
           (Set.empty, [])
    in

    (* Check the focus-metavariable clauses last since they can use metavariables
       in any clause within the And *)
    let bound_mvs_for_focus = Set.union cond_mvs bound_mvs_for_conds in
    let focus_errors =
      focus |> List.concat_map (check_mvars_of_focus r bound_mvs_for_focus)
    in
    (* Return only the metavariables that were newly bound in this node *)
    let mvs = Set.union cond_mvs inner_mvs in
    let mvs_with_as =
      match as_ with
      | None -> mvs
      | Some as_ -> Set.add as_ mvs
    in
    (mvs_with_as, focus_errors @ cond_errors @ errors @ inner_errors)
  and collect_metavars' parent_mvs kind : Mvar.t Set.t * Core_error.t list =
    match kind with
    | P { pat; pstr = pstr, _; pid = _pid } ->
        (* TODO currently this guesses that the metavariables are the strings
           that have a valid metavariable name. We should ideally have each
           matcher expose the metavariables it detects. *)
        (* First get the potential metavar ellipsis words *)
        let words_with_dot = Str.split (Str.regexp "[^a-zA-Z0-9_\\.$]") pstr in
        let ellipsis_metavars =
          words_with_dot |> List.filter Mvar.is_metavar_ellipsis
        in
        (* Then split the individual metavariables *)
        let words = List.concat_map (String.split_on_char '.') words_with_dot in
        let metavars = words |> List.filter Mvar.is_metavar_name in
        (* Then, for a pattern-regex, get all the named capture groups, and
           account for the metavariables introduced by their matches.
        *)
        let regexp_captured_mvars =
          match pat with
          | Xpattern.Regexp s -> Mvar.mvars_of_regexp_string s
          | __else__ -> []
        in
        ( [ metavars; ellipsis_metavars; regexp_captured_mvars ]
          |> List_.map Set.of_list
          |> List.fold_left Set.union Set.empty,
          [] )
    | Inside (_, f)
    | Anywhere (_, f) ->
        collect_metavars parent_mvs f
    | Not (_, _) -> (Set.empty, [])
    | And (_, xs)
    | Or (_, xs) ->
        (* Collect and check from the conjuncts. Pass down the metavariables
           from the parent *)
        let mv_sets = List_.map (collect_metavars parent_mvs) xs in
        List.fold_left
          (* TODO originally we took the intersection, since strictly
           * speaking a metavariable needs to be in all cases of a pattern-either
           * to be bound. However, due to how the pattern is transformed, this
           * is not always enforced, so the metacheck is too strict
           *)
            (fun (acc, acc_errors) (mv_set, errors) ->
            (Set.union acc mv_set, errors @ acc_errors))
          (Set.empty, []) mv_sets
  in
  let _, errors = collect_metavars Set.empty f in
  List.rev errors

(* call Check_pattern subchecker *)
exception CheckPatternFailure of string wrap

let check_pattern (lang : Analyzer.t) f =
  try
    Ok
      ((* TODO: can we ditch the exceptions and just have this be some sort of
          catamorphism? Would be nice to be able to easily return all the errors
          without needing a ref. *)
       Visit_rule.visit_xpatterns
         (fun { pat; pstr = _pat_str, t; pid = _ } ~inside:_ ->
           match (pat, lang) with
           | Sem (semgrep_pat, _lang), L (lang, _rest) -> (
               match Check_pattern.check lang semgrep_pat with
               | Ok () -> ()
               | Error s -> raise (CheckPatternFailure (s, t)))
           | Spacegrep _spacegrep_pat, LSpacegrep -> ()
           | Aliengrep _aliengrep_pat, LAliengrep -> ()
           | Regexp _, _ -> ()
           | _ -> raise Impossible)
         f)
  with
  | CheckPatternFailure s -> Error s

(*****************************************************************************)
(* Formula *)
(*****************************************************************************)

let check_formula r (lang : Analyzer.t) f =
  let errors =
    check_pattern lang f
    |> Result.map_error (fun (s, t) -> error r t s)
    |> Base.Result.error |> Option.to_list
  in
  errors @ unknown_metavar_in_comparison r f

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let check r =
  (* less: maybe we could also have formula_old specific checks *)
  match r.mode with
  | `Search f
  | `Extract { formula = f; _ } ->
      check_formula r r.target_analyzer f
  | `Taint _ -> (* TODO *) []
  | `Steps _ -> (* TODO *) []
  | `SCA _ -> (* TODO *) []

let semgrep_check (caps : < Core_scan.caps ; .. >) (metachecks : Fpath.t)
    (rules : Fpath.t list) : Core_error.t list =
  let match_to_semgrep_error (m : Core_match.t) : Core_error.t =
    let loc, _ = m.range_loc in
    (* TODO use the end location in errors *)
    let s = m.rule_id.message in
    let _check_id = m.rule_id.id in
    (* TODO: why not set ~rule_id here?? bug? *)
    E.mk_error ~msg:s ~loc Out.SemgrepMatchFound
  in
  (* LATER: what if the rule is written in Jsonnet or JSON ? *)
  let lang : Lang.t = Yaml in
  (* the targets are actually the rules! metachecking! *)
  let targets : Target.t list =
    rules
    |> List_.map (fun file -> Target.mk_target (Analyzer.of_lang lang) file)
  in
  let (config : Core_scan_config.t) =
    {
      Core_scan_config.default with
      rule_source = Rule_file metachecks;
      target_source = Targets targets;
      (* we're used from pysemgrep --validate *)
      output_format = Json true;
    }
  in
  let res = Core_scan.scan caps config in
  match res with
  | Ok result ->
      result.processed_matches
      |> List_.map (fun (m : Core_result.processed_match) -> m.pm)
      |> List_.map match_to_semgrep_error
  | Error exn -> Exception.reraise exn

let run_checks (caps : < Core_scan.caps ; .. >) (metachecks : Fpath.t)
    (xs : Fpath.t list) : Core_error.t list =
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
      Logs.err (fun m ->
          m "no valid yaml rules to run on (.test.yaml files are excluded)");
      []
  | _ ->
      let semgrep_found_errs = semgrep_check caps metachecks rules in
      let ocaml_found_errs =
        rules
        |> List.concat_map (fun file ->
               Logs.info (fun m ->
                   m "run_checks: processing rule file %s" !!file);
               match Parse_rule.parse file with
               | Ok rs -> rs |> List.concat_map (fun file -> check file)
               (* TODO this error is special cased because YAML files that
                  aren't semgrep rules are getting scanned *)
               | Error ({ kind = InvalidYaml _; _ } : Rule_error.t) -> []
               | Error e -> [ Core_error.error_of_rule_error e ]
               | exception exn ->
                   let e = Exception.catch exn in
                   [ E.exn_to_error ~file e ])
      in
      semgrep_found_errs @ ocaml_found_errs

(* for semgrep-core -check_rules, called from pysemgrep --validate
 * caps = Core_scan.caps + Cap.stdout
 *)
let check_files
    (caps : < Cap.stdout ; Cap.fork ; Cap.time_limit ; Cap.memory_limit ; .. >)
    (output_format : Core_scan_config.output_format) (input : Fpath.t list) :
    unit =
  let errors =
    match input with
    | []
    | [ _ ] ->
        raise
          (No_metacheck_file
             "check_rules needs a metacheck file or directory and rules to run \
              on")
    | metachecks :: xs -> run_checks caps metachecks xs
  in
  match output_format with
  | NoOutput -> ()
  | Text ->
      errors
      |> List.iter (fun err ->
             Logs.err (fun m -> m "%s" (E.string_of_error err)))
  | Json _ ->
      let (res : Core_result.t) =
        Core_result.mk_result_with_just_errors errors
      in
      let json = Core_json_output.core_output_of_matches_and_errors res in
      CapConsole.print caps#stdout (SJ.string_of_core_output json)

(* for semgrep-core -stat_rules *)
let stat_files (caps : < Cap.stdout >) xs =
  let fullxs, _skipped_paths =
    xs
    |> File_type.files_of_dirs_or_files (function
         | FT.Config (FT.Yaml (*FT.Json |*) | FT.Jsonnet) -> true
         | _ -> false)
    |> Skip_code.filter_files_if_skip_list ~root:xs
  in
  let good = ref 0 in
  let bad = ref 0 in
  let cache = Some (Hashtbl.create 101) in
  fullxs
  |> List.iter (fun file ->
         Logs.info (fun m -> m "stat_files: processing rule file %s" !!file);
         match Parse_rule.parse file with
         | Ok rs ->
             rs
             |> List.iter (fun r ->
                    let res = Analyze_rule.regexp_prefilter_of_rule ~cache r in
                    match res with
                    | None ->
                        incr bad;
                        Logs.warn (fun m ->
                            m "no regexp prefilter for rule %s:%s" !!file
                              (Rule_ID.to_string (fst r.id)))
                    | Some (f, _f) ->
                        incr good;
                        let s = Semgrep_prefilter_j.string_of_formula f in
                        Logs.debug (fun m -> m "regexp: %s" s))
         | Error e ->
             Logs.warn (fun m ->
                 m "stat_files: error in %a: %s" Fpath.pp file
                   (Rule_error.string_of_error e)));
  CapConsole.print caps#stdout
    (spf "good = %d, no regexp found = %d" !good !bad)
