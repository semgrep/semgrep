(* Yoann Padioleau, Cooper Pierce

   Copyright (C) Semgrep Inc.

   This library is free software; you can redistribute it and/or modify it
   under the terms of the GNU Lesser General Public License version 2.1 as
   published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE. See the file LICENSE for more details.
 *)
open Common
module String_set = Analyze_pattern.String_set
module MvarSet = Analyze_pattern.MvarSet

(* NOTE "AND vs OR and filter_map":
   We cannot use `List.filter_map` for `R.Or`, because it has the wrong
   semantics. We use `None` to say "we can't handle this", or in other words,
   "we assume this pattern can match", or just "true"! So in an AND we can
   remove those "true" terms, but in an OR we need to reduce the entire OR to
   "true". Therefore, `List.filter_map` works for AND-semantics, but for
   OR-semantics we need `option_map`. *)
let option_map f xs =
  List.fold_left
    (fun acc x ->
      let* ys = acc in
      let* y = f x in
      Some (y :: ys))
    (Some []) xs

type 'pred requirement_tree = 'pred Formula.t [@@deriving show]

let map_requirement_tree_opt = Formula.map_opt

type pattern_predicate =
  | LPat of Xpattern.t  (** A pattern from the rule.*)
  | LCond of Rule.metavar_cond
      (** A condition on some metavariable from the rule. *)
[@@deriving show]

(*****************************************************************************)
(* Step 1: extract patterns from the rule *)
(*****************************************************************************)

let rec required_patterns_of_formula ({ f; conditions; _ } : Rule.formula) :
    pattern_predicate requirement_tree option =
  let augment_with_conditions (conditions : (Tok.t * Rule.metavar_cond) list)
      (x : pattern_predicate requirement_tree) =
    Formula.and_
      (x
      :: List.map
           (fun (_, cond) : _ requirement_tree -> Formula.pred (LCond cond))
           conditions)
  and required_patterns_of_formula_kind (kind : Rule.formula_kind) :
      pattern_predicate requirement_tree option =
    match kind with
    | P pat -> Some (Formula.pred (LPat pat))
    | Not (_, formula) -> (
        match formula.f with
        | P _ -> None
        (* double negation *)
        | Not (_, f) -> required_patterns_of_formula f
        (* todo? apply De Morgan's law? *)
        | Or (_, _xs) -> None
        | And _ -> None
        | Inside _ -> None
        | Anywhere _ -> None)
    | Inside (_, formula)
    | Anywhere (_, formula) ->
        required_patterns_of_formula formula
    | And (_, xs) ->
        List.filter_map required_patterns_of_formula xs |> Formula.and_
    | Or (_, xs) ->
        (* See NOTE "AND vs OR and filter_map". *)
        let* xs = option_map required_patterns_of_formula xs in
        Formula.or_ xs
  in
  let* pats = required_patterns_of_formula_kind f in
  augment_with_conditions conditions pats

(*****************************************************************************)
(* Step 2: simplify the patterns *)
(*****************************************************************************)

type metavariable_and_strings_predicate =
  | StringsAndMvars of string list * Metavariable.mvar list
  | Regex of string
  | MvarRegexp of Metavariable.mvar * string * bool
[@@deriving show]

type env = { interfile : bool; is_id_mvar : Metavariable.mvar -> bool }

(* Here we overapproximate and just look for _ONE_ occurrence of an $MVAR in an
   "identifier position" (cf., 'Analyze_pattern.extract_mvars_in_id_position').
   But, in a `pattern-either` we could have an $MVAR in an identifier position
   in one pattern, and the same $MVAR in a non-identifier position in another
   one. In those cases we may still end up skipping files that we should not
   skip. *)
let id_mvars_of_formula ~interfile f =
  let id_mvars = ref Analyze_pattern.MvarSet.empty in
  f
  |> Visit_rule.visit_xpatterns (fun xp ~inside:_ ->
      match xp with
      | { pat = Sem (pat, lang); _ } ->
          id_mvars :=
            Analyze_pattern.(
              extract_mvars_in_id_position ~lang ~interfile pat
              |> MvarSet.union !id_mvars)
      | __else__ -> ());
  !id_mvars

let metavariables_and_strings_of_pattern (env : env) (pat : Xpattern.t) :
    metavariable_and_strings_predicate requirement_tree option =
  let open Formula in
  match pat.pat with
  | Sem (pat, lang) ->
      let ids, mvars =
        Analyze_pattern.extract_strings_and_mvars ~lang ~interfile:env.interfile
          pat
      in
      Some
        (pred (StringsAndMvars (String_set.to_list ids, MvarSet.to_list mvars)))
  | Regexp re -> Some (pred (Regex re))
  (* turn out some genergc spacegrep rules can also be slow and a prefilter
    is also useful there *)
  | Spacegrep pat ->
      let ids, mvars =
        Analyze_spacegrep.extract_strings_and_mvars_spacegrep pat
      in
      Some (pred (StringsAndMvars (ids, mvars)))
  (* TODO? do we need to prefilter aliengrep rules? they are supposed to be
     compiled in effective Pcre_.t (see Pat_compile.t) regexps *)
  | Aliengrep _ -> None

let metavariables_and_strings_of_condition (env : env) (x : Rule.metavar_cond) :
    metavariable_and_strings_predicate requirement_tree option =
  match x with
  | CondEval _ -> None
  | CondNestedFormula _ -> None
  | CondRegexp (mvar, re, const_prop) ->
      if env.is_id_mvar mvar then
        Some (Formula.pred (MvarRegexp (mvar, re, const_prop)))
      else None
  (* TODO? maybe we should extract the strings from the type constraint *)
  | CondType _ -> None
  | CondName _ -> None
  | CondAnalysis _ -> None

let simplify_patterns env cnf =
  map_requirement_tree_opt
    (function
      | LPat pat -> metavariables_and_strings_of_pattern env pat
      | LCond x -> metavariables_and_strings_of_condition env x)
    cnf

(*****************************************************************************)
(* Step 3: convert to purely textual predicates *)
(*****************************************************************************)
(* TODO: filter patterns without idents but with mvar mentioned
 * in an And in another branch.
 * TODO: replace some Strings [], MVar where mvar mentioned in a
 * MvarRegexp into a Regexp2
 *)

(* Now remove the predicate references to metavariables and directly have
   predicates on the entire text stream. *)
let rec textual_requirements_of_simplified :
    metavariable_and_strings_predicate requirement_tree ->
    Predicate.t requirement_tree option =
  let no_regex_special_chars (s : string) =
    (* Compare with <https://www.pcre.org/original/doc/html/pcrepattern.html>:

      There are two different sets of metacharacters: those that are recognized
      anywhere in the pattern except within square brackets, and those that are
      recognized within square brackets. Outside square brackets, the
      metacharacters are as follows:

        \      general escape character with several uses
        ^      assert start of string (or line, in multiline mode)
        $      assert end of string (or line, in multiline mode)
        .      match any character except newline (by default)
        [      start character class definition
        |      start of alternative branch
        (      start subpattern
        )      end subpattern
        ?      extends the meaning of (; also 0 or 1 quantifier; also
                 quantifier minimizer
        *      0 or more quantifier
        +      1 or more quantifier; also "possessive quantifier"
        {      start min/max quantifier *)
    Base.String.for_all
    (* TODO: This could be slightly improved, to allow for escaped uses of
         these. This would mean we could identify more regex which would be
         legal to transform to string predicates, which should be more
         efficient to check. Additionally, extending this to be able to
         identify regex we could transform to a reasonable set of strings
         (e.g., /a|b|c/ -> "a", "b", "c") would be nice. *)
      ~f:(function
        | '\\'
        | '^'
        | '$'
        | '.'
        | '['
        | '|'
        | '('
        | ')'
        | '?'
        | '*'
        | '+'
        | '{' ->
            false
        | _ -> true)
      s
  in
  (* NOTE: Lacks exception handling for malformed regexes. This should be OK
     because we parsed the rule already, but this is rather fragile. We should
     consider making our regex serialisable (the blocker as of May 2025 for
     having Xpattern.Regexp store the regex value directly). *)
  let module P = Predicate in
  let module F = Formula in
  function
  | And xs -> List.filter_map textual_requirements_of_simplified xs |> F.and_
  | Or xs ->
      let* xs = option_map textual_requirements_of_simplified xs in
      F.or_ xs
  | Pred (StringsAndMvars (xs, _)) ->
      F.and_ (List.map (fun x -> F.pred (P.String x)) xs)
  | Pred (Regex re) ->
      if no_regex_special_chars re then Some (F.pred (P.String re))
      else Some (F.pred (P.Regex (Pcre2_.pcre_compile re)))
  | Pred (MvarRegexp (_mvar, re_str, _const_prop)) ->
      (* The original regexp is meant to apply on a substring.
           We rewrite them to remove end-of-string anchors if possible. *)
      let* re =
        Pcre2_.remove_end_of_string_assertions (Pcre2_.pcre_compile re_str)
      in
      Some (F.pred (P.Regex re))

type prefilter = Predicate.t requirement_tree [@@deriving show]

let create_prefilter (env : env) f =
  let* f = required_patterns_of_formula f in
  let* f = simplify_patterns env f in
  let* f = textual_requirements_of_simplified f in
  Some f
[@@profiling]

let prefilter_of_formula ~interfile ~analyzer f : prefilter option =
  let is_id_mvar =
    (* When the target-analyzer is Spacegrep/Aliengrep, then we can always use
       `metavariable-regex`es for pre-filtering, because there is no constant
       folding in generic mode. But, when running semantic analysis for a
       specific language, we only use a `metavariable-regex` for pre-filtering
       if the metavariable meets certain conditions. Note that, in general, if
       we're looking for a string matching a certain regex, that regex may
       not match the source file, but there could be a string expression that
       would match it at runtime, and that can be known to Semgrep statically
       via constant folding. *)
    match (analyzer : Analyzer.t) with
    | LRegex
    | LSpacegrep
    | LAliengrep ->
        Fun.const true
    | L _ ->
        let id_mvars = id_mvars_of_formula ~interfile f in
        fun mvar -> Analyze_pattern.MvarSet.mem mvar id_mvars
  in
  create_prefilter { interfile; is_id_mvar } f

let prefilter_of_taint_rule ~interfile ~analyzer (_rule_id, rule_tok)
    ({ sources = _, source_patterns; sinks = _, sink_patterns; _ } :
      Rule.taint_spec) =
  if interfile then
    (* Taint rules are a lot more complex to prefilter. Even if the target does
       not match any source or sink, it may be calling a function in another file
       that returns taint, and passing a taint to another function (in yet another
       file) that leads to sink. *)
    None
  else
    (* If there are any sources or sinks that are ranges-based as opposed to formula-based,
       we want to not only keep the formula based sources or sinks, but to overall produce
       an empty list for our sources or sinks.
       This will signal that we would like the formula to be unconstrained in sources or
       sinks.
       Otherwise, we might incorrectly constrain based off of only the formula, which is
       incorrect, as matches for range-based rules can occur anywhere.
     *)
    let have_source_with_ranges =
      List.exists
        (fun (src : Rule.taint_source) ->
          match src.source_formula with
          | Rule.Ranges _ -> true
          | Rule.Formula _ -> false)
        source_patterns
    in
    let have_sink_with_ranges =
      List.exists
        (fun (sink : Rule.taint_sink) ->
          match sink.sink_formula with
          | Rule.Ranges _ -> true
          | Rule.Formula _ -> false)
        sink_patterns
    in
    (* We must be able to match some source _and_ some sink. *)
    let sources =
      if have_source_with_ranges then []
      else
        source_patterns
        |> List.filter_map (fun (src : Rule.taint_source) ->
            match src.source_formula with
            | Rule.Formula f -> Some f
            | Rule.Ranges _ -> None)
    in
    let sinks =
      if have_sink_with_ranges then []
      else
        sink_patterns
        |> List.filter_map (fun (sink : Rule.taint_sink) ->
            match sink.sink_formula with
            | Rule.Formula f -> Some f
            | Rule.Ranges _ -> None)
    in
    (* Note that this formula would likely not yield any meaningful result
       if executed by search-mode, but it works for the purpose of this
       analysis! *)
    prefilter_of_formula ~interfile ~analyzer
      Rule.(
        And (rule_tok, [ f (Or (rule_tok, sources)); f (Or (rule_tok, sinks)) ])
        |> f)

let generate_prefilter_internal ~interfile
    ({ id = rule_id, _; _ } as r : Rule.t) =
  try
    match r.mode with
    | `Search f
    | `Extract { formula = f; _ } -> (
        match prefilter_of_formula ~interfile ~analyzer:r.target_analyzer f with
        | Some x -> Some x
        | None ->
            Log.info (fun m ->
                m "Unable to generate prefilter for formula in %a" Rule_ID.pp
                  rule_id);
            None)
    | `Taint spec ->
        prefilter_of_taint_rule ~interfile ~analyzer:r.target_analyzer r.id spec
    | `Steps _ -> (* TODO *) None
    | `Join _ -> (* not supported *) None
    | `SCA _ -> None
  with
  | Stack_overflow ->
      Log.err (fun m ->
          m "Stack overflow when generating prefilter for %a" Rule_ID.pp rule_id);
      None

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* Memoized prefilter generation *)
let generate_prefilter ~interfile =
  let key_fn = fun ({ id = key, _; _ } : Rule.t) -> key in
  SharedMemo.make_with_key_fn key_fn (generate_prefilter_internal ~interfile)

(* Alias for the lower-level function to match the mli *)
let generate_prefilter_from_formula = prefilter_of_formula
