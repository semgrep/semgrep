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
module Log = Log_optimizing.Log

(* NOTE "AND vs OR and filter_map":
   We cannot use `List_.filter_map` for `R.Or`, because it has the wrong
   semantics. We use `None` to say "we can't handle this", or in other words,
   "we assume this pattern can match", or just "true"! So in an AND we can
   remove those "true" terms, but in an OR we need to reduce the entire OR to
   "true". Therefore, `List_.filter_map` works for AND-semantics, but for
   OR-semantics we need `option_map`. *)
let option_map f xs =
  List.fold_left
    (fun acc x ->
      let* ys = acc in
      let* y = f x in
      Some (y :: ys))
    (Some []) xs

type 'pred requirement_tree =
  | And of 'pred requirement_tree list  (** A conjunction of requirements. *)
  | Or of 'pred requirement_tree list  (** A disjunction of requirements. *)
  | Pred of 'pred  (** A single requirement. *)
[@@deriving show]

let rec map_requirement_tree_opt f = function
  | And xs -> (
      match List_.filter_map (map_requirement_tree_opt f) xs with
      | [] -> None
      | _ :: _ as xs -> Some (And xs))
  | Or xs ->
      let* xs = option_map (map_requirement_tree_opt f) xs in
      Some (Or xs)
  | Pred x -> f x

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
  let augment_with_conditions x =
    let conditions =
      List_.map
        (fun (_, cond) : _ requirement_tree -> Pred (LCond cond))
        conditions
    in
    And (x :: conditions)
  and required_patterns_of_formula_kind (kind : Rule.formula_kind) :
      pattern_predicate requirement_tree option =
    match kind with
    | P pat -> Some (Pred (LPat pat))
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
        let ys = List_.filter_map required_patterns_of_formula xs in
        if List_.null ys then None else Some (And ys)
    | Or (_, xs) ->
        (* See NOTE "AND vs OR and filter_map". *)
        let* ys = option_map required_patterns_of_formula xs in
        if List_.null ys then None else Some (Or ys)
  in
  required_patterns_of_formula_kind f |> Option.map augment_with_conditions

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
  match pat.pat with
  | Sem (pat, lang) ->
      let ids, mvars =
        Analyze_pattern.extract_strings_and_mvars ~lang ~interfile:env.interfile
          pat
      in
      Some
        (Pred (StringsAndMvars (String_set.to_list ids, MvarSet.to_list mvars)))
  | Regexp re -> Some (Pred (Regex re))
  (* turn out some genergc spacegrep rules can also be slow and a prefilter
    is also useful there *)
  | Spacegrep pat ->
      let ids, mvars =
        Analyze_spacegrep.extract_strings_and_mvars_spacegrep pat
      in
      Some (Pred (StringsAndMvars (ids, mvars)))
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
        Some (Pred (MvarRegexp (mvar, re, const_prop)))
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

type textual_predicate =
  | Strings of string list
      (** A list of string literals. All must be matched. *)
  | Regex of Pcre2_.t  (** A compiled regex. *)
[@@deriving show]

let textual_requirements_of_simplified =
  (* NOTE: Lacks exception handling for malformed regexes. This should be OK
     because we parsed the rule already, but this is rather fragile. We should
     consider making our regex serialisable (the blocker as of May 2025 for
     having Xpattern.Regexp store the regex value directly). *)
  map_requirement_tree_opt (function
    | StringsAndMvars ([], _) -> None
    | StringsAndMvars (xs, _) -> Some (Pred (Strings xs))
    | Regex re_str -> Some (Pred (Regex (Pcre2_.pcre_compile re_str)))
    | MvarRegexp (_mvar, re_str, _const_prop) ->
        (* The original regexp is meant to apply on a substring.
             We rewrite them to remove end-of-string anchors if possible. *)
        let* re =
          Pcre2_.remove_end_of_string_assertions (Pcre2_.pcre_compile re_str)
        in
        Some (Pred (Regex re)))

(*****************************************************************************)
(* Run the regexps *)
(*****************************************************************************)

let rec eval_textual_predicates
    (requirement : textual_predicate requirement_tree) big_str =
  match requirement with
  | And xs -> List.for_all (fun x -> eval_textual_predicates x big_str) xs
  | Or xs ->
      (* An empty list is true here since we need to be conservative if we
         failed to generate any constraints, even though generally an empty or
         would be false (identity). This is due to the fact that in principle
         this list being empty means we failed to generate some condition to
         check. If we have no condition to check, we still need to run the
         rule, so the prefiler must select for further matching (i.e., true). *)
      if List_.null xs then true
      else List.exists (fun x -> eval_textual_predicates x big_str) xs
  | Pred x -> (
      match x with
      | Strings xs ->
          xs
          |> List.for_all (fun id ->
                 Log.debug (fun m -> m "check for the presence of %S" id);
                 (* TODO: matching_exact_word does not work, why??
                     because string literals and metavariables are put under
                     Strings? *)
                 let re = Pcre2_.matching_exact_string id in
                 (* Note that in case of a PCRE error, we want to assume
                     that the rule is relevant, hence error -> true! *)
                 Pcre2_.unanchored_match ~on_error:true re big_str)
      | Regex re -> Pcre2_.unanchored_match ~on_error:true re big_str)
[@@profiling]

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* see mli for more information
 * TODO: use a record.
 *)
type prefilter = textual_predicate requirement_tree [@@deriving show]

exception EmptyOr
exception EmptyAnd

let rec prefilter_formula_of_prefilter (formula : prefilter) :
    Semgrep_prefilter_t.formula =
  match formula with
  | And xs -> begin
      let xs' = xs |> List_.map prefilter_formula_of_prefilter in
      match xs' with
      | [] -> raise EmptyAnd
      | [ x ] -> x
      | xs -> `And xs
    end
  | Or ys -> begin
      let ys' = ys |> List_.map prefilter_formula_of_prefilter in
      match ys' with
      | [] -> raise EmptyOr
      | [ x ] -> x
      | xs -> `Or xs
    end
  | Pred x -> (
      match x with
      | Strings xs -> `Pred (`Idents xs)
      | Regex re ->
          let re_str = Pcre2_.show re in
          `Pred (`Regexp re_str))
[@@profiling]

let create_prefilter (env : env) f =
  let* f = required_patterns_of_formula f in
  let* f = simplify_patterns env f in
  let* f = textual_requirements_of_simplified f in
  Some f
[@@profiling]

let check_prefilter prefilter content =
  eval_textual_predicates prefilter content

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
    (* We must be able to match some source _and_ some sink. *)
    let sources =
      source_patterns
      |> List_.map (fun (src : Rule.taint_source) -> src.source_formula)
    in
    let sinks =
      sink_patterns
      |> List_.map (fun (sink : Rule.taint_sink) -> sink.sink_formula)
    in
    (* Note that this formula would likely not yield any meaningful result
       if executed by search-mode, but it works for the purpose of this
       analysis! *)
    prefilter_of_formula ~interfile ~analyzer
      Rule.(
        And (rule_tok, [ f (Or (rule_tok, sources)); f (Or (rule_tok, sinks)) ])
        |> f)

let generate_prefilter ~interfile ({ id = rule_id, _; _ } as r : Rule.t) =
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
    | `SCA _ -> None
  with
  | Stack_overflow ->
      Log.err (fun m ->
          m "Stack overflow when generating prefilter for %a" Rule_ID.pp rule_id);
      None

let prefilter_of_rule ~interfile =
  let key_fn = fun ({ id = key, _; _ } : Rule.t) -> key in
  SharedMemo.make_with_key_fn key_fn (generate_prefilter ~interfile)

module Private = struct
  let prefilter_of_formula = prefilter_of_formula
end
