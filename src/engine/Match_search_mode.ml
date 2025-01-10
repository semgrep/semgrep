(* Yoann Padioleau
 *
 * Copyright (C) 2019-2022 Semgrep Inc.
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
module R = Rule
module XP = Xpattern
module MR = Mini_rule
module PM = Core_match
module G = AST_generic
module MV = Metavariable
module RP = Core_result
module RM = Range_with_metavars
module E = Core_error
module ME = Matching_explanation
module GG = Generic_vs_generic
module OutJ = Semgrep_output_v1_j
module Log = Log_engine.Log
open Match_env

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
  e
  |> Visit_rule.visit_xpatterns (fun xpat ~inside:b ->
         Stack_.push (xpat, b) res);
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
         | XP.Sem (x, _lang) -> Stack_.push (x, inside, pid, str) semgrep
         | XP.Spacegrep x -> Stack_.push (x, pid, str) spacegrep
         | XP.Aliengrep x -> Stack_.push (x, pid, str) aliengrep
         | XP.Regexp x -> Stack_.push (Pcre2_.pcre_compile x, pid, str) regexp);
  (List.rev !semgrep, List.rev !spacegrep, List.rev !aliengrep, List.rev !regexp)

let group_matches_per_pattern_id (xs : Core_match.t list) : id_to_match_results
    =
  let h = Hashtbl.create 101 in
  xs
  |> List.iter (fun (m : PM.t) ->
         let id = int_of_string (Rule_ID.to_string m.rule_id.id) in
         Hashtbl_.push h id m);
  h

let error_with_rule_id rule_id (error : Core_error.t) =
  match error.typ with
  (* Don't add the rule id for consistency with other parse errors *)
  | PartialParsing _ -> error
  | _ -> { error with rule_id = Some rule_id }

let lazy_force x = Lazy.force x [@@profiling]

(* `fold_with_expls` is a left fold across a list of things, while
   accumulating an explanation for each item. it preserves the
   explanations in the same order, though.
*)
let fold_with_expls ~f ~init env xs =
  let acc, expls =
    xs
    |> List.fold_left
         (fun (acc_ranges, (acc_expls : ME.t option list)) x ->
           let new_ranges, new_expl = f env acc_ranges x in
           (new_ranges, new_expl :: acc_expls))
         (init, [])
  in
  (acc, List.rev expls)

let pms_of_ranges env (ranges : RM.ranges) : PM.t list =
  ranges |> List_.map (RM.range_to_pattern_match_adjusted env.rule)

let formula_has_as_metavariable (f : R.formula) =
  let ans = ref false in
  Visit_rule.visit_formula
    (fun (f : R.formula) -> ans := !ans || Option.is_some f.as_)
    f;
  !ans

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
      Analyzer.t ->
      Rule.t ->
      Pattern.t * bool * Xpattern.pattern_id * string ->
      MR.t) =
 fun analyzer rule (pattern, inside, id, pstr) ->
  {
    MR.id = Rule_ID.of_string_exn (string_of_int id);
    pattern;
    inside;
    metadata = rule.metadata;
    (* parts that are not really needed I think in this context, since
     * we just care about the matching result.
     *)
    message = "";
    severity = `Error;
    langs =
      (match analyzer with
      | L (x, xs) -> x :: xs
      | LRegex
      | LSpacegrep
      | LAliengrep ->
          raise Impossible);
    (* useful for debugging timeout *)
    pattern_string = pstr;
    fix = rule.Rule.fix;
    fix_regexp = rule.Rule.fix_regexp;
  }

(*****************************************************************************)
(* Debugging semgrep *)
(*****************************************************************************)

let debug_semgrep config mini_rules file lang ast =
  (* process one mini rule at a time *)
  Log.debug (fun m -> m "DEBUG SEMGREP MODE!");
  mini_rules
  |> List.concat_map (fun mr ->
         Log.debug (fun m ->
             m "Checking mini rule with pattern %s" mr.MR.pattern_string);
         let res =
           Match_patterns.check
             ~hook:(fun _ -> ())
             config [ mr ]
             (file, File file, lang, ast)
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
           res
           |> List.iter (fun m ->
                  Log.debug (fun p -> p "match = %s" (PM.show m)));
         res)

(*****************************************************************************)
(* Evaluating Semgrep patterns *)
(*****************************************************************************)

let matches_of_patterns ~has_as_metavariable ?mvar_context ?range_filter rule
    (xconf : xconfig) (xtarget : Xtarget.t)
    (patterns : (Pattern.t * bool * Xpattern.pattern_id * string) list) :
    Core_profiling.times Core_result.match_result =
  let {
    path = { origin; internal_path_to_content };
    analyzer;
    lazy_ast_and_errors;
    lazy_content = _;
  } : Xtarget.t =
    xtarget
  in
  let config = (xconf.config, xconf.equivs) in
  match analyzer with
  | Analyzer.L (lang, _) ->
      let (ast, skipped_tokens), parse_time =
        Common.with_time (fun () -> lazy_force lazy_ast_and_errors)
      in
      let matches, match_time =
        Common.with_time (fun () ->
            let mini_rules =
              patterns
              |> List_.map (function pat, b, c, d ->
                     mini_rule_of_pattern analyzer rule (pat, b, c, d))
            in

            if !debug_timeout || !debug_matches then
              (* debugging path *)
              debug_semgrep config mini_rules internal_path_to_content lang ast
            else
              (* regular path *)
              Match_patterns.check
                ~hook:(fun _ -> ())
                ~has_as_metavariable ?mvar_context ?range_filter config
                mini_rules
                (internal_path_to_content, origin, lang, ast))
      in
      let errors = Parse_target.errors_from_skipped_tokens skipped_tokens in
      RP.mk_match_result matches errors
        { Core_profiling.parse_time; match_time }
  | _ -> Core_result.empty_match_result

(*****************************************************************************)
(* Specializations *)
(*****************************************************************************)

(* This code used to live in Specialize_formula.
    We shouldn't need to translate the entire formula for just the
    `And`s, though. I think we can just split the positives, negatives,
    and selectors when we actually get to evaluating it.
*)
let selector_equal s1 s2 = s1.mvar = s2.mvar

let selector_from_formula ({ f; _ } : Rule.formula) =
  match f with
  | R.P { Xpattern.pat = Sem (pattern, _); pid; pstr } -> (
      match pattern with
      | G.E { e = G.N (G.Id ((mvar, _), _)); _ } when Mvar.is_metavar_name mvar
        ->
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

let specialize_and (conjuncts : Rule.formula list) =
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
      let patterns = [ (pattern, false, pid, fst pstr) ] in
      let res =
        matches_of_patterns ~has_as_metavariable:env.has_as_metavariable
          ~range_filter env.rule env.xconf env.xtarget patterns
      in
      Log.debug (fun m ->
          m "run_selector_on_ranges: found %d matches" (List.length res.matches));
      res.matches
      |> List_.map RM.match_result_to_range
      |> RM.intersect_ranges env.xconf.config ~debug_matches:!debug_matches
           ranges

let apply_focus_on_ranges (env : env) (focus_mvars_list : R.focus_mv_list list)
    (ranges : RM.ranges) : RM.ranges =
  let intersect (r1 : RM.t) (r2 : RM.t) : RM.t option =
    if Range.( $<=$ ) r1.r r2.r then Some r1
    else if Range.( $<=$ ) r2.r r1.r then Some r2
    else None
  in
  (* this will return a list of new ranges that have been restricted by the variables in focus_mvars *)
  let apply_focus_mvars (focus_mvars : MV.mvar list) (range : RM.t) : RM.t list
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
      |> List_.filter_map (fun (focus_mvar, mval) ->
             let* range_loc =
               AST_generic_helpers.range_of_any_opt (MV.mvalue_to_any mval)
             in
             Some (focus_mvar, mval, range_loc))
    in
    let focus_matches =
      fm_mval_range_locs
      |> List_.map (fun (focus_mvar, mval, range_loc) ->
             {
               PM.rule_id = fake_rule_id (-1, focus_mvar);
               path = env.xtarget.path;
               range_loc;
               (* as-metavariable: *)
               ast_node =
                 (if env.has_as_metavariable then Some (MV.mvalue_to_any mval)
                  else None);
               tokens = lazy (MV.ii_of_mval mval);
               env = range.mvars;
               taint_trace = None;
               engine_of_match = `OSS;
               validation_state = `No_validator;
               severity_override = None;
               metadata_override = None;
               sca_match = None;
               fix_text = None;
               facts = [];
             })
    in
    let focused_ranges =
      (* Filter out focused ranges that are outside of the original range *)
      List_.filter_map
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
      |> List_.map (fun (_tok, focus_mvars) ->
             (* focus_mvars is a list of the metavariables under a single
                focus-metavariable statement.

                eg: focus_mvars = [$A, $B] for
                " - focus-metavariable:
                    - $A
                    - $B"*)
             let focused_ranges = apply_focus_mvars focus_mvars init_range in
             focused_ranges)
    in
    let intersect_ranges (list1 : RM.t list) (list2 : RM.t list) : RM.t list =
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
    let rec intersect_ranges_list (l : RM.t list list) : RM.t list =
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

let apply_as_on_ranges ranges as_ =
  ranges
  |> List_.map (fun (range : RM.t) ->
         {
           range with
           mvars =
             (match range.origin.ast_node with
             | Some node -> (as_, MV.mvalue_of_any node) :: range.mvars
             | None -> (
                 let tokens = Lazy.force range.origin.tokens in
                 match (range.origin.path.origin, tokens) with
                 | File _, [] ->
                     Log.warn (fun m ->
                         m "Got empty tokens when using as-metavariable");
                     range.mvars
                 | File fpath, fst_tok :: _ ->
                     ( as_,
                       Text
                         ( Range.content_at_range fpath range.r,
                           fst_tok,
                           Common2.list_last tokens ) )
                     :: range.mvars
                 | _ ->
                     Log.debug (fun m ->
                         m "unable to apply as operator to gitblob match");
                     range.mvars));
         })

(*****************************************************************************)
(* Evaluating xpatterns *)
(*****************************************************************************)

let matches_of_xpatterns ~has_as_metavariable ~mvar_context rule
    (xconf : xconfig) (xtarget : Xtarget.t)
    (xpatterns : (Xpattern.t * bool) list) :
    Core_profiling.times Core_result.match_result =
  let ({ path = { internal_path_to_content; origin }; lazy_content; _ }
        : Xtarget.t) =
    xtarget
  in
  (* Right now you can only mix semgrep/regexps and spacegrep/regexps, but
   * in theory we could mix all of them together. This is why below
   * I don't match over analyzer and instead assume we could have multiple
   * kinds of patterns at the same time.
   *)
  let patterns, spacegreps, aliengreps, regexps =
    partition_xpatterns xpatterns
  in

  (* final result *)
  RP.collate_pattern_results
    [
      matches_of_patterns ~has_as_metavariable ?mvar_context rule xconf xtarget
        patterns;
      Xpattern_match_spacegrep.matches_of_spacegrep xconf spacegreps
        internal_path_to_content origin;
      Xpattern_match_aliengrep.matches_of_aliengrep aliengreps lazy_content
        internal_path_to_content origin;
      Xpattern_match_regexp.matches_of_regexs regexps lazy_content
        internal_path_to_content origin;
    ]
[@@profiling]

(*****************************************************************************)
(* Maching explanations helpers *)
(*****************************************************************************)

let if_explanations ?extra (env : env) (ranges : RM.ranges)
    (children : ME.t option list) (op, tok) : ME.t option =
  if env.xconf.matching_explanations then
    let matches = pms_of_ranges env ranges in
    let xs = List_.filter_map (fun x -> x) children in
    let expl = { ME.op; pos = tok; children = xs; matches; extra } in
    Some expl
  else None

let children_explanations_of_xpat (env : env) (xpat : Xpattern.t) : ME.t list =
  if env.xconf.matching_explanations then
    match xpat.pat with
    (* TODO: generalize to more patterns *)
    | Sem
        ( G.Ss [ s1; { s = G.ExprStmt ({ e = G.Ellipsis _; _ }, _); _ }; s2 ],
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
          |> List_.map (fun pat ->
                 let match_result =
                   matches_of_patterns
                     ~has_as_metavariable:env.has_as_metavariable env.rule
                     env.xconf env.xtarget
                     [ (pat, false, xpat.pid, "TODO") ]
                 in
                 let matches = match_result.matches in
                 (* TODO: equivalent to an abstract_content, so not great *)
                 let pstr = Core_json_output.metavar_string_of_any pat in
                 (* TODO: could use first_info_of_any pat, but not sure the
                  * tok position in pat are related to the rule of the intermediate
                  * file used to parse the pattern in xpat.pat.
                  *)
                 let pos = snd xpat.pstr in
                 (* less: in theory we could decompose again pat and get children*)
                 {
                   ME.op = XPat pstr;
                   pos;
                   matches;
                   children = [];
                   extra = None;
                 })
        in
        (* less: add a Out.EllipsisAndStmts intermediate? *)
        children
    | _ -> []
  else []

(* This helper is for making the explanations in a `Rule.formula`, after making
   the call to evaluate the `Rule.formula_kind`.
   We mostly split it up here for brevity, because it clutters up `evaluate_formula`.
*)
let mk_expls_after_formula_kind ~formula_kind_expls ~filter_expls ~focus_expls
    ({ conditions; focus; f = _; as_ = _; fix = _ } : Rule.formula) env ranges =
  match formula_kind_expls with
  | None -> None
  | Some ({ ME.children; extra; _ } as me) ->
      let children =
        List_.map (fun x -> Some x) children @ filter_expls @ focus_expls
        |> List_.filter_map Fun.id
      in
      let extra =
        (* if we didn't have any filter steps, then we shouldn't change
           any of the fields here
           in particular, we should avoid accidentally setting `before_filter_matches`
           to `Some []` or the whole extra to `Some`
        *)
        match (conditions, focus) with
        | [], [] -> extra
        | _ -> (
            (* As a corollary of having to replace the matches below, we need to
               put these pre-filtering matches somewhere else.
               We elect to store it in the `extra`.
            *)
            let before_filter_matches = pms_of_ranges env ranges in
            match extra with
            | None -> Some (ME.mk_extra ~before_filter_matches ())
            | Some extra ->
                Some
                  {
                    extra with
                    before_filter_matches = Some before_filter_matches;
                  })
      in
      Some
        {
          me with
          ME.children;
          ME.extra;
          (* We must replace the matches here, or else the matches of the root
             node will not be those that the tree finally outputs.
             For instance, if we had:
             all:
               - pattern: foo(...)
               - not: foo(1)
             and there were 3 instances of `foo(...)` but 2 instances of `foo(1)`,
             without this step the root explanation would have 3 matches, instead
             of the correct 1.
          *)
          matches = pms_of_ranges env ranges;
        }

(*****************************************************************************)
(* Metavariable condition evaluation *)
(*****************************************************************************)

let hook_pro_entropy_analysis :
    (mode:Rule.entropy_analysis_mode -> string -> bool) option ref =
  ref None

let hook_pro_metavariable_name :
    (G.expr -> Rule.metavar_cond_name -> bool) option ref =
  ref None

let rec filter_ranges (env : env) (xs : (RM.t * MV.bindings list) list)
    (cond : R.metavar_cond) : (RM.t * MV.bindings list) list =
  let file = env.xtarget.path.internal_path_to_content in
  xs
  |> List_.filter_map (fun (r, new_bindings) ->
         let map_bool r b = if b then Some (r, new_bindings) else None in
         let bindings = r.RM.mvars in
         match cond with
         | R.CondEval e ->
             let env =
               Eval_generic.bindings_to_env env.xconf.config ~file bindings
             in
             Eval_generic.eval_bool env e r.origin.facts bindings |> map_bool r
         | R.CondNestedFormula (mvar, opt_lang, formula) -> (
             (* TODO: could return expl for nested matching! *)
             match
               Metavariable_pattern.get_nested_metavar_pattern_bindings
                 get_nested_formula_matches env r mvar opt_lang formula
             with
             | [] -> None
             | bindings -> Some (r, bindings @ new_bindings))
         | R.CondType (mvar, opt_lang, _, ts) -> (
             let* mval = List.assoc_opt mvar bindings in
             match Metavariable.mvalue_to_expr mval with
             | Some e ->
                 let lang =
                   match
                     Option.value opt_lang ~default:env.xtarget.analyzer
                   with
                   | Analyzer.L (lang, _) -> lang
                   | Analyzer.LRegex
                   | Analyzer.LSpacegrep
                   | Analyzer.LAliengrep ->
                       raise Impossible
                 in
                 let ast, _ = Lazy.force env.xtarget.lazy_ast_and_errors in
                 (* This call iterates over the program's top-level statements, and
                    thus incurs some cost, but it shouldn't be much.
                 *)
                 let env =
                   Matching_generic.environment_of_program lang env.xconf.config
                     ast
                 in
                 let matches =
                   (* We check whether any of the types listed in the
                      `type` field match. These types are treated as
                      connected by "or" logical operators. *)
                   ts
                   |> List.concat_map (fun t ->
                          GG.m_compatible_type lang
                            (mvar, Tok.unsafe_fake_tok "")
                            t e env)
                 in

                 (* the type can also contain metavariables, but we probably
                  * don't want to use that in other parts of the rules, so it's
                  * probably fine to just check whether the match is empty or
                  * not *)
                 matches <> [] |> map_bool r
             | None ->
                 error env
                   (spf "couldn't find metavar %s in the match results." mvar);
                 None)
         | R.CondName ({ mvar; _ } as cond) -> (
             let find_name env e cond =
               match !hook_pro_metavariable_name with
               | None ->
                   error env
                     "semgrep-internal-metavariable-name operator is only \
                      supported in the Pro engine";
                   false
               | Some f -> f e cond
             in
             let* mval = List.assoc_opt mvar bindings in
             match Metavariable.mvalue_to_expr mval with
             | Some e -> find_name env e cond |> map_bool r
             | None ->
                 error env
                   (spf "couldn't find metavar %s in the match results." mvar);
                 None)
         (* todo: would be nice to have CondRegexp also work on
          * eval'ed bindings.
          * We could also use re.match(), to be close to python, but really
          * Eval_generic must do something special here with the metavariable
          * which may not always be a string. The regexp is really done on
          * the text representation of the metavar content.
          *)
         | R.CondRegexp (mvar, re_str, const_prop) -> (
             let config = env.xconf.config in
             let env =
               if const_prop && config.constant_propagation then
                 Eval_generic.bindings_to_env config ~file bindings
               else
                 Eval_generic.bindings_to_env_just_strings config ~file bindings
             in
             (* TODO: could return expl for nested matching! *)
             match
               Metavariable_regex.get_metavar_regex_capture_bindings env ~file r
                 (mvar, re_str)
             with
             | None -> None
             (* The bindings we get back are solely the new capture group metavariables. We need
              * to combine them with the metavariables from the original match.
              *)
             | Some capture_bindings -> Some (r, capture_bindings @ new_bindings)
             )
         | R.CondAnalysis (mvar, CondEntropyV2 mode) -> (
             match !hook_pro_entropy_analysis with
             | None ->
                 (* TODO - nice UX handling of this for pysemgrep - tell the user
                  * that they ran a rule in OSS w/o Pro hook and so their rule
                  * didn't do anything
                  *)
                 (* nosemgrep: no-logs-in-library *)
                 Logs.err (fun m ->
                     m
                       "EntropyV2 rule encountered without loading proprietary \
                        plugin");
                 None
             | Some f ->
                 let bindings = r.mvars in
                 Metavariable_analysis.analyze_string_metavar env bindings mvar
                   (f ~mode)
                 |> map_bool r)
         | R.CondAnalysis (mvar, CondEntropy) ->
             let bindings = r.mvars in
             Metavariable_analysis.analyze_string_metavar env bindings mvar
               Entropy.has_high_score
             |> map_bool r
         | R.CondAnalysis (mvar, CondReDoS) ->
             let bindings = r.mvars in
             let analyze re_str =
               Log.debug (fun m ->
                   m "Analyze regexp captured by %s for ReDoS vulnerability: %s"
                     mvar re_str);
               match ReDoS.find_vulnerable_subpatterns re_str with
               | Ok [] -> false
               | Ok subpatterns ->
                   subpatterns
                   |> List.iter (fun pat ->
                          Log.debug (fun m ->
                              m
                                "The following subpattern was predicted to be \
                                 vulnerable to ReDoS attacks: %s"
                                pat));
                   true
               | Error () ->
                   Log.warn (fun m ->
                       m
                         "Failed to parse metavariable %s's value as a regexp: \
                          %s"
                         mvar re_str);
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
    let lang = env.xtarget.analyzer |> Analyzer.to_string in
    let rule = fst env.rule.id in
    res.RP.errors
    |> E.ErrorSet.map (fun err ->
           let msg =
             spf
               "When parsing a snippet as %s for metavariable-pattern in rule \
                '%s', %s"
               lang (Rule_ID.to_string rule) err.msg
           in
           { err with msg })
  in
  env.errors := E.ErrorSet.union nested_errors !(env.errors);
  final_ranges

(*****************************************************************************)
(* Formula evaluation *)
(*****************************************************************************)

and evaluate_formula env opt_context
    ({ f; focus; conditions; fix; as_ } as formula : Rule.formula) =
  let formula_kind_ranges, expls = evaluate_formula_kind env opt_context f in
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
      * See https://github.com/semgrep/semgrep/issues/2664
  *)
  let ranges, filter_expls =
    fold_with_expls
      ~f:(fun env ranges (tok, cond) ->
        let ranges_with_bindings = filter_ranges env ranges cond in
        let expl =
          if_explanations env
            (List_.map fst ranges_with_bindings)
            []
            (OutJ.Filter (Tok.content_of_tok tok), tok)
        in
        (ranges_with_bindings, expl))
      ~init:(List_.map (fun x -> (x, [])) formula_kind_ranges)
      env conditions
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
              |> List_.map (fun new_bindings ->
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
          if_explanations env ranges [] (OutJ.Filter "metavariable-focus", tok);
        ]
  in

  (* Populate each match with the enclosed `fix: ...` autofix, if it exists.
     This means that outer-scoped fixes can "overwrite" inner ones, if they are
     layered on top of each other.
  *)
  let ranges =
    match fix with
    | None -> ranges
    | Some fix ->
        List_.map
          (fun (r : RM.t) ->
            { r with origin = { r.origin with fix_text = Some fix } })
          ranges
  in

  let ranges =
    match as_ with
    | None -> ranges
    | Some as_ -> apply_as_on_ranges ranges as_
  in

  (* This code path only matters for matching explanations.
     Here, we synthesize the information from all the explanations in a `formula`,
     and combine it with the information we obtained before from the `formula_kind`
  *)
  let new_expls =
    mk_expls_after_formula_kind ~formula_kind_expls:expls ~filter_expls
      ~focus_expls formula env ranges
  in

  (ranges, new_expls)

and evaluate_formula_kind env opt_context (kind : Rule.formula_kind) =
  match kind with
  | R.P ({ XP.pid = id; pstr = pstr, tok; _ } as xpat) ->
      let match_results = Hashtbl_.get_stack env.pattern_matches id in
      let kind = if Xpattern.is_regexp xpat then RM.Regexp else RM.Plain in
      let ranges =
        match_results
        |> List_.map RM.match_result_to_range
        |> List_.map (fun r -> { r with RM.kind })
      in
      (* we can decompose the pattern in subpatterns to provide
          * intermediate explanations for complex patterns like A...B
      *)
      let children =
        children_explanations_of_xpat env xpat |> List_.map (fun x -> Some x)
      in
      let expl = if_explanations env ranges children (OutJ.XPat pstr, tok) in
      (ranges, expl)
  | R.Inside (tok, formula) ->
      let ranges, expls = evaluate_formula env opt_context formula in
      let expl = if_explanations env ranges [ expls ] (OutJ.Inside, tok) in
      (List_.map (fun r -> { r with RM.kind = RM.Inside }) ranges, expl)
  | R.Anywhere (tok, formula) ->
      let ranges, expls = evaluate_formula env opt_context formula in
      let expl = if_explanations env ranges [ expls ] (OutJ.Anywhere, tok) in
      (List_.map (fun r -> { r with RM.kind = RM.Anywhere }) ranges, expl)
  | R.Or (tok, xs) ->
      let ranges, expls =
        xs |> List_.map (evaluate_formula env opt_context) |> Common2.unzip
      in
      let ranges = List_.flatten ranges in
      let expl = if_explanations env ranges expls (OutJ.Or, tok) in
      (ranges, expl)
  | R.And (t, conj) -> (
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
        List_.map (evaluate_formula env opt_context) pos |> Common2.unzip
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
        |> Either_.partition (fun xs ->
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
                   RM.intersect_ranges env.xconf.config
                     ~debug_matches:!debug_matches acc r)
                 ranges
          in
          (* optimization of `pattern: $X` *)
          let ranges_before_negation =
            run_selector_on_ranges env selector_opt ranges
          in

          (* let's remove the negative ranges *)
          let ranges, negs_expls =
            fold_with_expls
              ~f:(fun env ranges (tok, x) ->
                let ranges_neg, expl = evaluate_formula env opt_context x in
                let ranges =
                  RM.difference_ranges env.xconf.config ranges ranges_neg
                in
                let expl =
                  if_explanations env ranges [ expl ] (OutJ.Negation, tok)
                in
                (ranges, expl))
              ~init:ranges_before_negation env neg
          in

          let expl =
            (* We reverse these negation explanations, because we folded across them from
               the left, meaning they are in the opposite order as in the original rule.
            *)
            if_explanations env ranges
              (posrs_expls @ negs_expls)
              (* These explanations are useful, because without it, we cannot
                 recover the matches incoming to the first Not node.
                 This makes it difficult to tell from purely the matching explanation
                 whether a match was removed by positive intersection or negation.
              *)
              ~extra:
                (ME.mk_extra
                   ~before_negation_matches:
                     (pms_of_ranges env ranges_before_negation)
                   ())
              (OutJ.And, t)
          in
          (ranges, expl))
  | R.Not _ -> failwith "Invalid Not; you can only negate inside an And"

and matches_of_formula xconf rule xtarget formula opt_context :
    Core_profiling.rule_profiling Core_result.match_result * RM.ranges =
  let xpatterns = xpatterns_in_formula formula in
  let mvar_context : Metavariable.bindings option =
    Option.map (fun s -> s.RM.mvars) opt_context
  in
  let has_as_metavariable = formula_has_as_metavariable formula in
  Log.debug (fun m ->
      m "formula for %s has as_metavariable: %b"
        (Rule_ID.to_string (fst rule.id))
        has_as_metavariable);
  let res =
    matches_of_xpatterns ~has_as_metavariable ~mvar_context rule xconf xtarget
      xpatterns
    |> RP.add_rule rule
  in
  Log.info (fun m -> m "found %d matches" (List.length res.matches));
  (* match results per minirule id which is the same than pattern_id in
   * the formula *)
  let pattern_matches_per_id = group_matches_per_pattern_id res.matches in
  let env =
    {
      xconf;
      pattern_matches = pattern_matches_per_id;
      xtarget;
      rule;
      has_as_metavariable;
      errors = ref E.ErrorSet.empty;
    }
  in
  Log.info (fun m -> m "evaluating the formula");
  let final_ranges, expl = evaluate_formula env opt_context formula in
  Log.info (fun m -> m "found %d final ranges" (List.length final_ranges));
  let res' =
    {
      res with
      RP.errors = E.ErrorSet.union res.RP.errors !(env.errors);
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

  let%trace_debug sp = "Match_search_mode.check_rule" in
  Tracing.add_data_to_span sp
    [
      ("rule_id", `String (rule_id |> Rule_ID.to_string)); ("taint", `Bool false);
    ];

  let res, final_ranges = matches_of_formula xconf r xtarget formula None in
  let errors = res.errors |> E.ErrorSet.map (error_with_rule_id rule_id) in
  {
    res with
    RP.matches =
      final_ranges
      |> List_.map (RM.range_to_pattern_match_adjusted r)
      (* dedup similar findings (we do that also in Match_patterns.ml,
       * but different mini-rules matches can now become the same match)
       *)
      |> PM.uniq
      |> hook;
    errors;
  }
