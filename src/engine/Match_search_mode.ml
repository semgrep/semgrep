(* Yoann Padioleau
 *
 * Copyright (C) 2019-2022 r2c
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
module R = Rule
module XP = Xpattern
module MR = Mini_rule
module PM = Pattern_match
module G = AST_generic
module MV = Metavariable
module RP = Report
module RM = Range_with_metavars
module E = Semgrep_error_code
module ME = Matching_explanation
module GG = Generic_vs_generic
open Match_env

let logger = Logging.get_logger [ __MODULE__ ]

(* Debugging flags.
 * Note that semgrep-core -matching_explanations can also be useful to debug.
 *)
let debug_timeout = ref false
let debug_matches = ref false

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The core engine of Semgrep.
 *
 * This module implements the boolean composition of patterns.
 * See Match_patterns.ml for the code to handle a single pattern and
 * the visitor/matching engine.
 *
 * Thus, we can decompose the engine in 3 main components:
 *  - composing matching results using boolean/set logic (this file)
 *  - visiting code (=~ Match_patterns.ml)
 *  - matching code (=~ Generic_vs_generic.ml)
 *
 * There are also "preprocessing" steps before:
 *  - parsing (lexing, parsing) rules, code, patterns
 *  - normalizing (convert to a generic AST)
 *  - naming (but bugs probably)
 *  - typing (propagating type decls at least and small inference)
 *  - analyzing (dataflow constant propagation)
 *    but we could do much more: deep static analysis using Datalog?
 *
 * TODO
 *  - associate the metavariable-regexp to the appropriate pattern
 *    to get the right scope (right now we accept the wrong scope but
 *    this forces to extend some ranges with metavars from other ranges)
 *
 * LATER (if we decide to rewrite the whole python wrapper code in OCaml):
 *  - paths
 *  - autofix
 *  - adjust messages with metavariable content
 *  - ...
 *
 * FUTURE WORK:
 * update: TODO move in DeepSemgrep
 * Right now we just analyze one file at a time. Later we could
 * maybe take a list of files and do some global analysis for:
 *     * caller/callee in different files
 *       which can be useful to understand keyword arguments
 *     * inheritance awareness, because right now we can't match
 *       code that inherits indirectly form a class mentioned in a pattern
 * There are different options for such global analysis:
 *  - generate a giant file a la CIL, but scale?
 *    (there is a recent LLVM project that does the same)
 *  - do it via a 2 passes process. 1st pass iterates over all files, report
 *    already matches, record semantic information (e.g., inheritance tree,
 *    call graph, etc.) as it goes, and let the matching engine report
 *    todo_second_pass if for example is_children returned a Maybe.
 *    Then in 2nd pass just process the files that were marked as todo.
 *  - use LSP, so don't even need 2 pass and can even work when passing
 *    a single file or subdir to semgrep
 *
 * Note that we opted here for simple patterns with simple extensions
 * to the grammar (metavar, ellipsis) with simple (but powerful) logic
 * compositions of patterns.
 * Coccinelle instead opted for very complex patterns and using CTL to
 * hold of that together.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* The types are now defined in Match_env.ml *)

type selector = {
  mvar : MV.mvar;
  pattern : AST_generic.any;
  pid : int;
  pstr : string R.wrap;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let xpatterns_in_formula (e : R.formula) : (Xpattern.t * bool) list =
  let res = ref [] in
  e |> R.visit_new_formula (fun xpat b -> Common.push (xpat, b) res);
  !res

let partition_xpatterns xs =
  let semgrep = ref [] in
  let spacegrep = ref [] in
  let aliengrep = ref [] in
  let regexp = ref [] in
  xs
  |> List.iter (fun (xpat, inside) ->
         let { XP.pid; pstr = str, _; pat } = xpat in
         match pat with
         | XP.Sem (x, _lang) -> Common.push (x, inside, pid, str) semgrep
         | XP.Spacegrep x -> Common.push (x, pid, str) spacegrep
         | XP.Aliengrep x -> Common.push (x, pid, str) aliengrep
         | XP.Regexp x ->
             Common.push (Regexp_engine.pcre_compile x, pid, str) regexp);
  (List.rev !semgrep, List.rev !spacegrep, List.rev !aliengrep, List.rev !regexp)

let group_matches_per_pattern_id (xs : Pattern_match.t list) :
    id_to_match_results =
  let h = Hashtbl.create 101 in
  xs
  |> List.iter (fun m ->
         let id = int_of_string (m.PM.rule_id.id :> string) in
         Hashtbl.add h id m);
  h

let error_with_rule_id rule_id (error : E.error) =
  match error.typ with
  (* Don't add the rule id for consistency with other parse errors *)
  | PartialParsing _ -> error
  | _ -> { error with rule_id = Some rule_id }

let lazy_force x = Lazy.force x [@@profiling]
let fb = Tok.unsafe_fake_bracket

(*****************************************************************************)
(* Adapters *)
(*****************************************************************************)

(* old: Before we used to pass the entire rule and we got the list of languages
 * from there. But after metavariable-pattern we can now recursively call
 * evaluate_formula using a different language than the one used by the rule!
 * If the rule is a generic one, but the nested pattern formula is not, then
 * this will raise Impossible... Thus, now we have to pass the language(s) that
 * we are specifically targeting. *)
let (mini_rule_of_pattern :
      Xlang.t ->
      Rule.t ->
      Pattern.t * bool * Xpattern.pattern_id * string ->
      MR.t) =
 fun xlang rule (pattern, inside, id, pstr) ->
  {
    MR.id = Rule_ID.of_string (string_of_int id);
    pattern;
    inside;
    (* parts that are not really needed I think in this context, since
     * we just care about the matching result.
     *)
    message = "";
    severity = R.Error;
    languages =
      (match xlang with
      | L (x, xs) -> x :: xs
      | LRegex
      | LSpacegrep
      | LAliengrep ->
          raise Impossible);
    (* useful for debugging timeout *)
    pattern_string = pstr;
    fix = rule.Rule.fix;
  }

(*****************************************************************************)
(* Debugging semgrep *)
(*****************************************************************************)

let debug_semgrep config mini_rules file lang ast =
  (* process one mini rule at a time *)
  logger#info "DEBUG SEMGREP MODE!";
  mini_rules
  |> List.concat_map (fun mr ->
         logger#debug "Checking mini rule with pattern %s" mr.MR.pattern_string;
         let res =
           Match_patterns.check
             ~hook:(fun _ -> ())
             config [ mr ] (file, lang, ast)
         in
         if !debug_matches then
           (* TODO
              let json = res |> List.map (fun x ->
                   x |> JSON_report.match_to_match_ |> SJ.string_of_match_) in
              let json_uniq = Common.uniq_by ( = ) json in
              let res_uniq =
                Common.uniq_by (AST_utils.with_structural_equal PM.equal) res
              in
              logger#debug
                "Found %d mini rule matches (uniq = %d) (json_uniq = %d)"
                (List.length res) (List.length res_uniq) (List.length json_uniq);
           *)
           res |> List.iter (fun m -> logger#debug "match = %s" (PM.show m));
         res)

(*****************************************************************************)
(* Evaluating Semgrep patterns *)
(*****************************************************************************)

let matches_of_patterns ?mvar_context ?range_filter rule (xconf : xconfig)
    (xtarget : Xtarget.t)
    (patterns : (Pattern.t Lazy.t * bool * Xpattern.pattern_id * string) list) :
    RP.times RP.match_result =
  let { Xtarget.file; xlang; lazy_ast_and_errors; lazy_content = _ } =
    xtarget
  in
  let config = (xconf.config, xconf.equivs) in
  match xlang with
  | Xlang.L (lang, _) ->
      let (ast, skipped_tokens), parse_time =
        Common.with_time (fun () -> lazy_force lazy_ast_and_errors)
      in
      let matches, match_time =
        Common.with_time (fun () ->
            let mini_rules =
              patterns
              |> Common.map (function (lazy pat), b, c, d ->
                     mini_rule_of_pattern xlang rule (pat, b, c, d))
            in

            if !debug_timeout || !debug_matches then
              (* debugging path *)
              debug_semgrep config mini_rules !!file lang ast
            else
              (* regular path *)
              Match_patterns.check
                ~hook:(fun _ -> ())
                ?mvar_context ?range_filter config mini_rules (!!file, lang, ast))
      in
      let errors = Parse_target.errors_from_skipped_tokens skipped_tokens in
      RP.make_match_result matches errors { RP.parse_time; match_time }
  | _ -> RP.empty_semgrep_result

(*****************************************************************************)
(* Specializations *)
(*****************************************************************************)

(* This code used to live in Specialize_formula.
    We shouldn't need to translate the entire formula for just the
    `And`s, though. I think we can just split the positives, negatives,
    and selectors when we actually get to evaluating it.
*)
let selector_equal s1 s2 = s1.mvar = s2.mvar

let selector_from_formula f =
  match f with
  | R.P { Xpattern.pat = Sem ((lazy pattern), _); pid; pstr } -> (
      match pattern with
      | G.E { e = G.N (G.Id ((mvar, _), _)); _ } when MV.is_metavar_name mvar ->
          Some { mvar; pattern; pid; pstr }
      | _ -> None)
  | _ -> None

let rec remove_selectors (selector, acc) formulas =
  match formulas with
  | [] -> (selector, acc)
  | x :: xs ->
      let selector, acc =
        match (selector, selector_from_formula x) with
        | None, None -> (None, x :: acc)
        | Some s, None -> (Some s, x :: acc)
        | None, Some s -> (Some s, acc)
        | Some s1, Some s2 when selector_equal s1 s2 -> (Some s1, acc)
        | Some s1, Some _s2 ->
            (* patterns:
                * ...
                * - pattern: $X
                * - pattern: $Y
            *)
            (* TODO: Should we fail here or just reported as a warning? This
                * is something to catch with the meta-checker. *)
            (Some s1, x :: acc)
      in
      remove_selectors (selector, acc) xs

let specialize_and ({ conjuncts; _ } : Rule.conjunction) =
  let pos, neg = Rule.split_and conjuncts in
  let selector_opt, pos =
    (* We only want a selector if there is something to select from. *)
    match remove_selectors (None, []) pos with
    | _, [] -> (None, pos)
    | sel, pos -> (sel, pos)
  in
  (selector_opt, pos, neg)

let run_selector_on_ranges env selector_opt ranges =
  match (selector_opt, ranges) with
  | _, [] ->
      (* Empty context, nothing to select from. *)
      []
  | None, ranges ->
      (* Nothing to select. *)
      ranges
  | Some { pattern; pid; pstr; _ }, ranges ->
      (* Find matches of `pattern` *but* only inside `ranges`, this prevents
       * matching and allocations that are wasteful because they are going to
       * be thrown away later when interesecting the results.
       *)
      let range_filter (tok1, tok2) =
        let r = Range.range_of_token_locations tok1 tok2 in
        List.exists (fun rwm -> Range.( $<=$ ) r rwm.RM.r) ranges
      in
      let patterns = [ (lazy pattern, false, pid, fst pstr) ] in
      let res =
        matches_of_patterns ~range_filter env.rule env.xconf env.xtarget
          patterns
      in
      logger#info "run_selector_on_ranges: found %d matches"
        (List.length res.matches);
      res.matches
      |> Common.map RM.match_result_to_range
      |> RM.intersect_ranges env.xconf.config !debug_matches ranges

let apply_focus_on_ranges env (focus_mvars_list : R.focus_mv_list list)
    (ranges : RM.ranges) : RM.ranges =
  let intersect (r1 : RM.t) (r2 : RM.t) : RM.t option =
    if Range.( $<=$ ) r1.r r2.r then Some r1
    else if Range.( $<=$ ) r2.r r1.r then Some r2
    else None
  in
  (* this will return a list of new ranges that have been restricted by the variables in focus_mvars *)
  let apply_focus_mvars (focus_mvars : MV.mvar list) (range : RM.t) : RM.t stack
      =
    (* A list that groups each metavariable under all of the `focus-metavariables`
     * within a `patterns` with the "metavariable value" that each one captures.
     * We then expand each tuple with more information about the matches by
     * transforming them to different types.
     *)
    let fm_mvals =
      focus_mvars
      |> List.fold_left
           (fun acc focus_mvar ->
             let res =
               List.find_opt
                 (fun (mvar, _mval) -> MV.equal_mvar focus_mvar mvar)
                 range.RM.mvars
             in
             match res with
             | None -> acc
             | Some (_mvar, mval) -> (focus_mvar, mval) :: acc)
           []
    in
    let fm_mval_range_locs =
      fm_mvals
      |> Common.map_filter (fun (focus_mvar, mval) ->
             let* range_loc =
               AST_generic_helpers.range_of_any_opt (MV.mvalue_to_any mval)
             in
             Some (focus_mvar, mval, range_loc))
    in
    let focus_matches =
      fm_mval_range_locs
      |> Common.map (fun (focus_mvar, mval, range_loc) ->
             {
               PM.rule_id = fake_rule_id (-1, focus_mvar);
               PM.file = !!(env.xtarget.file);
               PM.range_loc;
               PM.tokens = lazy (MV.ii_of_mval mval);
               PM.env = range.mvars;
               PM.taint_trace = None;
               engine_kind = PM.OSS;
             })
    in
    let focused_ranges =
      (* Filter out focused ranges that are outside of the original range *)
      Common.map_filter
        (fun fms -> intersect (RM.match_result_to_range fms) range)
        focus_matches
    in
    (* A union of the fm_mval ranges *)
    focused_ranges
  in
  let apply_focus_mvars_list init_range =
    (* focus_mvars_list and focus_ranges_list are lists of focus statements under a single `patterns`,
     * ie: [focus-metavariable: $A, $B; focus-metavariable: $X, $Y]
     *)
    let focused_ranges_list =
      focus_mvars_list
      |> Common.map (fun (_tok, focus_mvars) ->
             (* focus_mvars is a list of the metavariables under a single
                focus-metavariable statement.

                eg: focus_mvars = [$A, $B] for
                " - focus-metavariable:
                    - $A
                    - $B"*)
             let focused_ranges = apply_focus_mvars focus_mvars init_range in
             focused_ranges)
    in
    let intersect_ranges (list1 : RM.t stack) (list2 : RM.t stack) : RM.t stack
        =
      match (list1, list2) with
      | [ range1 ], [ range2 ] -> (
          match intersect range1 range2 with
          | Some r -> [ r ]
          | None -> [])
      | [ _ ], []
      | [], [ _ ] ->
          []
      | [], _
      | _, [] ->
          failwith "Cannot have an empty `focus-metavariables`"
      | _, _ ->
          failwith
            "Semgrep currently does not support multiple `focus-metavariable` \
             statements with multiple metavariables under a single `patterns`."
    in
    let rec intersect_ranges_list (l : RM.t stack stack) : RM.t stack =
      match l with
      | [] -> failwith "No focus-metavariable statements found."
      | [ first ] -> first
      | first :: second :: tail ->
          (* Intersecting two separate focus-metavariable statements at a time *)
          intersect_ranges_list ([ intersect_ranges first second ] @ tail)
    in
    intersect_ranges_list focused_ranges_list
  in
  match focus_mvars_list with
  | [] -> ranges
  | _ -> List.concat_map apply_focus_mvars_list ranges

(*****************************************************************************)
(* Evaluating xpatterns *)
(*****************************************************************************)

let matches_of_xpatterns ~mvar_context rule (xconf : xconfig)
    (xtarget : Xtarget.t) (xpatterns : (Xpattern.t * bool) list) :
    RP.times RP.match_result =
  let { Xtarget.file; lazy_content; _ } = xtarget in
  (* Right now you can only mix semgrep/regexps and spacegrep/regexps, but
   * in theory we could mix all of them together. This is why below
   * I don't match over xlang and instead assume we could have multiple
   * kinds of patterns at the same time.
   *)
  let patterns, spacegreps, aliengreps, regexps =
    partition_xpatterns xpatterns
  in

  (* final result *)
  RP.collate_pattern_results
    [
      matches_of_patterns ~mvar_context rule xconf xtarget patterns;
      Xpattern_match_spacegrep.matches_of_spacegrep xconf spacegreps !!file;
      Xpattern_match_aliengrep.matches_of_aliengrep aliengreps lazy_content
        !!file;
      Xpattern_match_regexp.matches_of_regexs regexps lazy_content !!file;
    ]
  [@@profiling]

(*****************************************************************************)
(* Maching explanations helpers *)
(*****************************************************************************)

let if_explanations (env : env) (ranges : RM.ranges)
    (children : ME.t option list) (op, tok) : ME.t option =
  if env.xconf.matching_explanations then
    let matches =
      ranges
      |> Common.map (fun range ->
             RM.range_to_pattern_match_adjusted env.rule range)
    in
    let xs = Common.map_filter (fun x -> x) children in
    let expl = { ME.op; pos = tok; children = xs; matches } in
    Some expl
  else None

let children_explanations_of_xpat (env : env) (xpat : Xpattern.t) : ME.t list =
  if env.xconf.matching_explanations then
    match xpat.pat with
    (* TODO: generalize to more patterns *)
    | Sem
        ( (lazy
            (G.Ss
              [ s1; { s = G.ExprStmt ({ e = G.Ellipsis _; _ }, _); _ }; s2 ])),
          _lang ) ->
        let subs = [ G.S s1; G.S s2 ] in
        (* we could optimize and run matches_of_patterns() below once
         * per all the subs, but that would require to generate
         * new xpat pid, and map the results back to their corresponding
         * pattern id (see group_matches_per_pattern_id), which is
         * not worth it given this code is executed only when
         * one requests -matching_explanations
         *)
        let children =
          subs
          |> Common.map (fun pat ->
                 let match_result =
                   matches_of_patterns env.rule env.xconf env.xtarget
                     [ (lazy pat, false, xpat.pid, "TODO") ]
                 in
                 let matches = match_result.matches in
                 (* TODO: equivalent to an abstract_content, so not great *)
                 let pstr = JSON_report.metavar_string_of_any pat in
                 (* TODO: could use first_info_of_any pat, but not sure the
                  * tok position in pat are related to the rule of the intermediate
                  * file used to parse the pattern in xpat.pat.
                  *)
                 let pos = snd xpat.pstr in
                 (* less: in theory we could decompose again pat and get children*)
                 { ME.op = Out.XPat pstr; pos; matches; children = [] })
        in
        (* less: add a Out.EllipsisAndStmts intermediate? *)
        children
    | _ -> []
  else []

(*****************************************************************************)
(* Metavariable condition evaluation *)
(*****************************************************************************)

let rec filter_ranges (env : env) (xs : (RM.t * MV.bindings list) list)
    (cond : R.metavar_cond) : (RM.t * MV.bindings list) list =
  xs
  |> Common.map_filter (fun (r, new_bindings) ->
         let map_bool r b = if b then Some (r, new_bindings) else None in
         let bindings = r.RM.mvars in
         match cond with
         | R.CondEval e ->
             let env = Eval_generic.bindings_to_env env.xconf.config bindings in
             Eval_generic.eval_bool env e |> map_bool r
         | R.CondNestedFormula (mvar, opt_lang, formula) -> (
             (* TODO: could return expl for nested matching! *)
             match
               Metavariable_pattern.get_nested_metavar_pattern_bindings
                 get_nested_formula_matches env r mvar opt_lang formula
             with
             | [] -> None
             | bindings -> Some (r, bindings @ new_bindings))
         | R.CondType (mvar, opt_lang, _, t) -> (
             let mvalue_to_expr m =
               match Metavariable.mvalue_to_any m with
               | G.E e -> Some e
               | _ -> None
             in
             match List.assoc_opt mvar bindings >>= mvalue_to_expr with
             | Some e ->
                 let lang =
                   match Option.value opt_lang ~default:env.xtarget.xlang with
                   | Xlang.L (lang, _) -> lang
                   | Xlang.LRegex
                   | Xlang.LSpacegrep
                   | Xlang.LAliengrep ->
                       raise Impossible
                 in
                 let ast, _ = Lazy.force env.xtarget.lazy_ast_and_errors in
                 (* This will incur some cost from the visitor, but it shouldn't be much. *)
                 let env =
                   Matching_generic.environment_of_any lang env.xconf.config
                     (G.Pr ast)
                 in
                 let matches =
                   GG.m_compatible_type lang
                     (mvar, Tok.unsafe_fake_tok "")
                     t e env
                 in
                 logger#info
                   "range %d-%d filtered from metavar %s type mismatch."
                   r.r.start r.r.end_ mvar;
                 (* the type can also contain metavariables, but we probably
                  * don't want to use that in other parts of the rules, so it's
                  * probably fine to just check whether the match is empty or
                  * not *)
                 matches <> [] |> map_bool r
             | None ->
                 error env
                   (spf "couldn't find metavar %s in the match results." mvar);
                 Some (r, new_bindings))
         (* todo: would be nice to have CondRegexp also work on
          * eval'ed bindings.
          * We could also use re.match(), to be close to python, but really
          * Eval_generic must do something special here with the metavariable
          * which may not always be a string. The regexp is really done on
          * the text representation of the metavar content.
          *)
         | R.CondRegexp (mvar, re_str, const_prop) ->
             let fk = Tok.unsafe_fake_tok "" in
             let fki = AST_generic.empty_id_info () in
             let e =
               (* old: spf "semgrep_re_match(%s, \"%s\")" mvar re_str
                * but too many possible escaping problems, so easier to build
                * an expression manually.
                *)
               let re_exp = G.L (G.String (fb (re_str, fk))) |> G.e in
               let mvar_exp = G.N (G.Id ((mvar, fk), fki)) |> G.e in
               let call_re_match re_exp str_exp =
                 G.Call
                   ( G.DotAccess
                       ( G.N (G.Id (("re", fk), fki)) |> G.e,
                         fk,
                         FN (Id (("match", fk), fki)) )
                     |> G.e,
                     (fk, [ G.Arg re_exp; G.Arg str_exp ], fk) )
                 |> G.e
               in
               let call_str x =
                 G.Call
                   (G.N (G.Id (("str", fk), fki)) |> G.e, (fk, [ G.Arg x ], fk))
                 |> G.e
               in
               (* We generate a fake expression:
                     re.match(re_str, str($MVAR))
                  that matches `re_str` against the string representation of
                  $MVAR's value. *)
               call_re_match re_exp (call_str mvar_exp)
             in
             let config = env.xconf.config in
             let env =
               if const_prop && config.constant_propagation then
                 Eval_generic.bindings_to_env config bindings
               else Eval_generic.bindings_to_env_just_strings config bindings
             in
             Eval_generic.eval_bool env e |> map_bool r
         | R.CondAnalysis (mvar, CondEntropy) ->
             let bindings = r.mvars in
             Metavariable_analysis.analyze_string_metavar env bindings mvar
               Entropy.has_high_score
             |> map_bool r
         | R.CondAnalysis (mvar, CondReDoS) ->
             let bindings = r.mvars in
             let analyze re_str =
               logger#debug
                 "Analyze regexp captured by %s for ReDoS vulnerability: %s"
                 mvar re_str;
               match ReDoS.find_vulnerable_subpatterns re_str with
               | Ok [] -> false
               | Ok subpatterns ->
                   subpatterns
                   |> List.iter (fun pat ->
                          logger#info
                            "The following subpattern was predicted to be \
                             vulnerable to ReDoS attacks: %s"
                            pat);
                   true
               | Error () ->
                   logger#debug
                     "Failed to parse metavariable %s's value as a regexp: %s"
                     mvar re_str;
                   false
             in
             Metavariable_analysis.analyze_string_metavar env bindings mvar
               analyze
             |> map_bool r)

and get_nested_formula_matches env formula range =
  let res, final_ranges =
    matches_of_formula
      { env.xconf with nested_formula = true }
      env.rule env.xtarget formula (Some range)
  in
  (* Update the error messages with some more context. Otherwise it will appear
   * as if, for example, we encountered a parse error while parsing the original
   * file as the original language. *)
  let nested_errors =
    let lang = env.xtarget.xlang |> Xlang.to_string in
    let rule = fst env.rule.id in
    res.RP.errors
    |> RP.ErrorSet.map (fun err ->
           let msg =
             spf
               "When parsing a snippet as %s for metavariable-pattern in rule \
                '%s', %s"
               lang
               (rule :> string)
               err.msg
           in
           { err with msg })
  in
  env.errors := Report.ErrorSet.union nested_errors !(env.errors);
  final_ranges

(*****************************************************************************)
(* Formula evaluation *)
(*****************************************************************************)

and evaluate_formula (env : env) (opt_context : RM.t option) (e : R.formula) :
    RM.ranges * Matching_explanation.t option =
  match e with
  | R.P ({ XP.pid = id; pstr = pstr, tok; _ } as xpat) ->
      let match_results =
        try Hashtbl.find_all env.pattern_matches id with
        | Not_found -> []
      in
      let kind = if Xpattern.is_regexp xpat then RM.Regexp else RM.Plain in
      let ranges =
        match_results
        |> Common.map RM.match_result_to_range
        |> Common.map (fun r -> { r with RM.kind })
      in
      (* we can decompose the pattern in subpatterns to provide
          * intermediate explanations for complex patterns like A...B
      *)
      let children =
        children_explanations_of_xpat env xpat |> Common.map (fun x -> Some x)
      in
      let expl = if_explanations env ranges children (Out.XPat pstr, tok) in
      (ranges, expl)
  | R.Inside (tok, formula) ->
      let ranges, expls = evaluate_formula env opt_context formula in
      let expl = if_explanations env ranges [ expls ] (Out.Inside, tok) in
      (Common.map (fun r -> { r with RM.kind = RM.Inside }) ranges, expl)
  | R.Or (tok, xs) ->
      let ranges, expls =
        xs |> Common.map (evaluate_formula env opt_context) |> Common2.unzip
      in
      let ranges = List.flatten ranges in
      let expl = if_explanations env ranges expls (Out.Or, tok) in
      (ranges, expl)
  | R.And (t, ({ conditions = conds; focus; _ } as conj)) -> (
      (* we now treat pattern: and pattern-inside: differently. We first
          * process the pattern: and then the pattern-inside.
          * This fixed only one mismatch in semgrep-rules.
          *
          * old: the old code was simpler ... but incorrect.
          *  (match pos with
          *  | [] -> failwith "empty And; no positive terms in And"
          *  | start::pos ->
          *     let res = evaluate_formula env start in
          *    let res = pos |> List.fold_left (fun acc x ->
          *      intersect_ranges acc (evaluate_formula env x)
          * ...
      *)

      (* First, split up the conjunction. *)
      let selector_opt, pos, neg = specialize_and conj in

      (* let's start with the positive ranges *)
      let posrs, posrs_expls =
        Common.map (evaluate_formula env opt_context) pos |> Common2.unzip
      in
      (* subtle: we need to process and intersect the pattern-inside after
       * (see tests/rules/inside.yaml).
       * TODO: this is ugly; AND should be commutative, so we should just
       * merge ranges, not just filter one or the other.
       * update: however we have some tests that rely on pattern-inside:
       * being special, see tests/rules/and_inside.yaml.
       *)
      let posrs, posrs_inside =
        posrs
        |> Common.partition_either (fun xs ->
               match xs with
               (* todo? should we double check they are all inside? *)
               | { RM.kind = Inside; _ } :: _ -> Right xs
               | _ -> Left xs)
      in
      let all_posr =
        match posrs @ posrs_inside with
        | [] -> (
            match opt_context with
            | None -> failwith "empty And; no positive terms in And"
            | Some r -> [ [ r ] ])
        | ps -> ps
      in
      match all_posr with
      | [] -> failwith "FIXME"
      | posr :: posrs ->
          let ranges = posr in
          let ranges =
            posrs
            |> List.fold_left
                 (fun acc r ->
                   RM.intersect_ranges env.xconf.config !debug_matches acc r)
                 ranges
          in
          (* optimization of `pattern: $X` *)
          let ranges = run_selector_on_ranges env selector_opt ranges in

          (* let's remove the negative ranges *)
          let ranges, negs_expls =
            neg
            |> List.fold_left
                 (fun (ranges, acc_expls) (tok, x) ->
                   let ranges_neg, expl = evaluate_formula env opt_context x in
                   let ranges =
                     RM.difference_ranges env.xconf.config ranges ranges_neg
                   in
                   let expl =
                     if_explanations env ranges [ expl ] (Out.Negation, tok)
                   in
                   (ranges, expl :: acc_expls))
                 (ranges, [])
          in
          (* let's apply additional filters.
           * TODO: Note that some metavariable-regexp may be part of an
           * AND where not all patterns define the metavar, e.g.,
            *   pattern-inside: def $FUNC() ...
           *   pattern: return $X
           *   metavariable-regexp: $FUNC regex: (foo|bar)
           * in which case the order in which we do the operation matters
           * (at this point intersect_range will have filtered the
           *  range of the pattern_inside).
           * alternative solutions?
           *  - bind closer metavariable-regexp with the relevant pattern
           *  - propagate metavariables when intersecting ranges
           *  - distribute filter_range in intersect_range?
           * See https://github.com/returntocorp/semgrep/issues/2664
           *)
          let ranges, filter_expls =
            conds
            |> List.fold_left
                 (fun (ranges_with_bindings, acc_expls) (tok, cond) ->
                   let ranges_with_bindings =
                     filter_ranges env ranges_with_bindings cond
                   in
                   let expl =
                     if_explanations env
                       (Common.map fst ranges_with_bindings)
                       []
                       (Out.Filter (Tok.content_of_tok tok), tok)
                   in
                   (ranges_with_bindings, expl :: acc_expls))
                 (Common.map (fun x -> (x, [])) ranges, [])
          in

          (* Here, we unpack all the persistent bindings for each instance of the inner
             `metavariable-pattern`s that succeeded.

             We just take those persistent bindings and add them to the original range,
             now that we're done with the filtering step.
          *)
          let ranges_with_persistent_bindings =
            ranges
            |> List.concat_map (fun (r, new_bindings_list) ->
                   (* At a prior step, we ensured that all these new bindings were nonempty.
                      We should keep around a copy of the original range, because otherwise
                      if we have no new bindings to add, we'll kill the range.
                   *)
                   r
                   :: (new_bindings_list
                      |> Common.map (fun new_bindings ->
                             { r with RM.mvars = new_bindings @ r.RM.mvars })))
          in

          let ranges =
            apply_focus_on_ranges env focus ranges_with_persistent_bindings
          in
          let focus_expls =
            match focus with
            | [] -> []
            (* less: what if have multiple focus-metavariable? *)
            | (tok, _mvar) :: _rest ->
                [
                  if_explanations env ranges []
                    (Out.Filter "metavariable-focus", tok);
                ]
          in
          let expl =
            if_explanations env ranges
              (posrs_expls @ negs_expls @ filter_expls @ focus_expls)
              (Out.And, t)
          in
          (ranges, expl))
  | R.Not _ -> failwith "Invalid Not; you can only negate inside an And"

and matches_of_formula xconf rule xtarget formula opt_context :
    RP.rule_profiling RP.match_result * RM.ranges =
  let xpatterns = xpatterns_in_formula formula in
  let mvar_context : Metavariable.bindings option =
    Option.map (fun s -> s.RM.mvars) opt_context
  in
  let res =
    matches_of_xpatterns mvar_context rule xconf xtarget xpatterns
    |> RP.add_rule rule
  in
  logger#trace "found %d matches" (List.length res.matches);
  (* match results per minirule id which is the same than pattern_id in
   * the formula *)
  let pattern_matches_per_id = group_matches_per_pattern_id res.matches in
  let env =
    {
      xconf;
      pattern_matches = pattern_matches_per_id;
      xtarget;
      rule;
      errors = ref Report.ErrorSet.empty;
    }
  in
  logger#trace "evaluating the formula";
  let final_ranges, expl = evaluate_formula env opt_context formula in
  logger#trace "found %d final ranges" (List.length final_ranges);
  let res' =
    {
      res with
      RP.errors = Report.ErrorSet.union res.RP.errors !(env.errors);
      explanations = Option.to_list expl;
    }
  in
  (res', final_ranges)
  [@@profiling]

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let check_rule ({ R.mode = `Search formula; _ } as r) hook xconf xtarget =
  let rule_id = fst r.id in
  let res, final_ranges = matches_of_formula xconf r xtarget formula None in
  let errors = res.errors |> Report.ErrorSet.map (error_with_rule_id rule_id) in
  {
    res with
    RP.matches =
      final_ranges
      |> Common.map (RM.range_to_pattern_match_adjusted r)
      (* dedup similar findings (we do that also in Match_patterns.ml,
       * but different mini-rules matches can now become the same match)
       *)
      |> PM.uniq
      |> before_return (fun v ->
             v
             |> List.iter (fun (m : Pattern_match.t) ->
                    let str = spf "with rule %s" (rule_id :> string) in
                    hook str m));
    errors;
  }
