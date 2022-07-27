(* Yoann Padioleau
 *
 * Copyright (C) 2019-2022 r2c
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
module G = AST_generic
module MV = Metavariable

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Data structures to represent a Semgrep rule (=~ AST of a rule).
 *
 * See also Mini_rule.ml where formula and many other features disappear.
 *
 *)

(*****************************************************************************)
(* Position information *)
(*****************************************************************************)

(* This is similar to what we do in AST_generic to get precise
 * error location when a rule is malformed.
 *)
type tok = AST_generic.tok [@@deriving show, eq, hash]
type 'a wrap = 'a AST_generic.wrap [@@deriving show, eq, hash]

(* To help report pattern errors in simple mode in the playground *)
type 'a loc = {
  pattern : 'a;
  t : tok;
  path : string list; (* path to pattern in YAML rule *)
}
[@@deriving show, eq]

(*****************************************************************************)
(* Formula (patterns boolean composition) *)
(*****************************************************************************)

(* Classic boolean-logic/set operators with text range set semantic.
 * The main complication is the handling of metavariables and especially
 * negation in the presence of metavariables.
 *
 * less? enforce invariant that Not can only appear in And?
 *)
type formula =
  (* pattern: and pattern-inside: are actually slightly different so
   * we need to keep the information around.
   * (see tests/OTHER/rules/inside.yaml)
   * The same is true for pattern-not and pattern-not-inside
   * (see tests/OTHER/rules/negation_exact.yaml)
   *)
  | P of Xpattern.t (* a leaf pattern *) * inside option
  | And of conjunction
  | Or of tok * formula list
  (* There are currently restrictions on where a Not can appear in a formula.
   * It must be inside an And to be intersected with "positive" formula.
   * TODO? Could this change if we were moving to a different range semantic?
   *)
  | Not of tok * formula

(* The conjuncts must contain at least
 * one positive "term" (unless it's inside a CondNestedFormula, in which
 * case there is not such a restriction).
 * See also split_and().
 *)
and conjunction = {
  tok : tok;
  (* pattern-inside:'s and pattern:'s *)
  conjuncts : formula list;
  (* metavariable-xyz:'s *)
  conditions : (tok * metavar_cond) list;
  (* focus-metavariable:'s *)
  focus : (tok * MV.mvar) list;
}

(* todo: try to remove this at some point, but difficult. See
 * https://github.com/returntocorp/semgrep/issues/1218
 *)
and inside = Inside

and metavar_cond =
  | CondEval of AST_generic.expr (* see Eval_generic.ml *)
  (* todo: at some point we should remove CondRegexp and have just
   * CondEval, but for now there are some
   * differences between using the matched text region of a metavariable
   * (which we use for MetavarRegexp) and using its actual value
   * (which we use for MetavarComparison), which translate to different
   * calls in Eval_generic.ml
   * update: this is also useful to keep separate from CondEval for
   * the "regexpizer" optimizer (see Analyze_rule.ml).
   *)
  | CondRegexp of MV.mvar * Xpattern.regexp * bool (* constant-propagation *)
  | CondAnalysis of MV.mvar * metavar_analysis_kind
  | CondNestedFormula of MV.mvar * Xlang.t option * formula

and metavar_analysis_kind = CondEntropy | CondReDoS [@@deriving show, eq]

(*****************************************************************************)
(* Old Formula style *)
(*****************************************************************************)

(* Unorthodox original pattern compositions.
 * See also the JSON schema in rule_schema.yaml
 *)
type formula_old =
  (* pattern: *)
  | Pat of Xpattern.t
  (* pattern-not: *)
  | PatNot of tok * Xpattern.t
  (* metavariable-xyz: *)
  | PatExtra of tok * extra
  (* focus-metavariable: *)
  | PatFocus of tok * MV.mvar
  (* pattern-inside: *)
  | PatInside of Xpattern.t
  (* pattern-not-inside: *)
  | PatNotInside of tok * Xpattern.t
  (* pattern-either: Or *)
  | PatEither of tok * formula_old list
  (* patterns: And *)
  | Patterns of tok * formula_old list
  (* for fields handled in Python like depends-on *)
  | PatFilteredInPythonTodo of tok

(* extra conditions, usually on metavariable content *)
and extra =
  | MetavarRegexp of MV.mvar * Xpattern.regexp * bool
  | MetavarPattern of MV.mvar * Xlang.t option * formula_old
  | MetavarComparison of metavariable_comparison
  | MetavarAnalysis of MV.mvar * metavar_analysis_kind
(* old: | PatWherePython of string, but it was too dangerous.
 * MetavarComparison is not as powerful, but safer.
 *)

(* See also engine/Eval_generic.ml *)
and metavariable_comparison = {
  metavariable : MV.mvar option;
  comparison : AST_generic.expr;
  (* I don't think those are really needed; they can be inferred
   * from the values *)
  strip : bool option;
  base : int option;
}
[@@deriving show, eq]

(*****************************************************************************)
(* Final formula *)
(*****************************************************************************)

(* pattern formula *)
type pformula = New of formula | Old of formula_old [@@deriving show, eq]

(*****************************************************************************)
(* Taint-specific types *)
(*****************************************************************************)

type taint_source = {
  formula : pformula;
  label : string;  (** The label to attach to the data *)
  requires : AST_generic.expr;
      (** A Boolean formula over taint labels, the expression that
       is being checked as a source must satisfy this in order to the
       label to be produced. Note that with 'requires' a taint source
       behaves a bit like a propagator ... *)
}
[@@deriving show]

type taint_sanitizer = {
  not_conflicting : bool;
      (** If [not_conflicting] is enabled, the sanitizer cannot conflict with
    a sink or a source (i.e., match the exact same range) otherwise
    it is filtered out. This allows to e.g. declare `$F(...)` as a sanitizer,
    to assume that any other function will handle tainted data safely.
    Without this, `$F(...)` would automatically sanitize any other function
    call acting as a sink or a source.

    THINK: In retrospective, I'm not sure this was a good idea. We should add
    an option to disable the assumption that function calls always propagate
    taint, and deprecate not-conflicting sanitizers. *)
  formula : pformula;
}
[@@deriving show]

type taint_propagator = {
  formula : pformula;
  from : MV.mvar wrap;
  to_ : MV.mvar wrap;
}
[@@deriving show]
(** e.g. if we want to specify that adding tainted data to a `HashMap` makes the
 * `HashMap` tainted too, then "formula" could be `(HashMap $H).add($X)`,
 * with "from" being `$X` and "to" being `$H`. So if `$X` is tainted then `$H`
 * will also be marked as tainted. *)

type taint_sink = {
  formula : pformula;  (** A Boolean formula over taint labels. *)
  requires : AST_generic.expr;
}
[@@deriving show]

type taint_spec = {
  sources : taint_source list;
  propagators : taint_propagator list;
  sanitizers : taint_sanitizer list;
  sinks : taint_sink list;
}
[@@deriving show]

(* Method to combine extracted ranges within a file:
    - either treat them as separate files; or
    - concatentate them together
*)
type extract_reduction = Separate | Concat [@@deriving show]

type extract_spec = {
  pformula : pformula;
  reduce : extract_reduction;
  dst_lang : Xlang.t;
  extract : string;
}
[@@deriving show]

(*****************************************************************************)
(* The rule *)
(*****************************************************************************)

(* TODO? just reuse Error_code.severity *)
type severity = Error | Warning | Info | Inventory | Experiment
[@@deriving show]

type 'mode rule_info = {
  (* MANDATORY fields *)
  id : rule_id wrap;
  mode : 'mode;
  message : string; (* Currently a dummy value for extract mode rules *)
  severity : severity; (* Currently a dummy value for extract mode rules *)
  languages : Xlang.t;
  (* OPTIONAL fields *)
  options : Config_semgrep.t option;
  (* deprecated? todo: parse them *)
  equivalences : string list option;
  fix : string option;
  fix_regexp : (Xpattern.regexp * int option * string) option;
  paths : paths option;
  (* ex: [("owasp", "A1: Injection")] but can be anything *)
  metadata : JSON.t option;
}

and rule_id = string

and paths = {
  (* not regexp but globs *)
  include_ : string list;
  exclude : string list;
}
[@@deriving show]

type search_mode = [ `Search of pformula ] [@@deriving show]
type taint_mode = [ `Taint of taint_spec ] [@@deriving show]
type extract_mode = [ `Extract of extract_spec ] [@@deriving show]
type mode = [ search_mode | taint_mode | extract_mode ] [@@deriving show]
type search_rule = search_mode rule_info [@@deriving show]
type taint_rule = taint_mode rule_info [@@deriving show]
type extract_rule = extract_mode rule_info [@@deriving show]
type rule = mode rule_info [@@deriving show]

(* alias *)
type t = rule [@@deriving show]
type rules = rule list [@@deriving show]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let partition_rules (rules : rules) :
    search_rule list * taint_rule list * extract_rule list =
  rules
  |> Common.partition_either3 (fun r ->
         match r.mode with
         | `Search _ as s -> Left3 { r with mode = s }
         | `Taint _ as t -> Middle3 { r with mode = t }
         | `Extract _ as e -> Right3 { r with mode = e })

(*****************************************************************************)
(* Error Management *)
(*****************************************************************************)

(* This is used to let the user know which rule the engine was using when
 * a Timeout or OutOfMemory exn occured.
 *)
let (last_matched_rule : rule_id option ref) = ref None

(* Those are recoverable errors; We can just skip the rules containing
 * those errors.
 * less: use a record
 * alt: put in Output_from_core.atd?
 *)
type invalid_rule_error = invalid_rule_error_kind * rule_id * Parse_info.t

and invalid_rule_error_kind =
  | InvalidLanguage of string (* the language string *)
  (* TODO: the Parse_info.t for InvalidPattern is not precise for now;
   * it corresponds to the start of the pattern *)
  | InvalidPattern of
      string (* pattern *)
      * Xlang.t
      * string (* exn *)
      * string list (* yaml path *)
  | InvalidRegexp of string (* PCRE error message *)
  | DeprecatedFeature of string (* e.g., pattern-where-python: *)
  | MissingPositiveTermInAnd
  | InvalidOther of string
[@@deriving show]

let string_of_invalid_rule_error_kind = function
  | InvalidLanguage language -> spf "invalid language %s" language
  | InvalidRegexp message -> spf "invalid regex %s" message
  (* coupling: this is actually intercepted in
   * Semgrep_error_code.exn_to_error to generate a PatternParseError instead
   * of a RuleParseError *)
  | InvalidPattern (pattern, xlang, message, _yaml_path) ->
      spf
        "Invalid pattern for %s: %s\n\
         ----- pattern -----\n\
         %s\n\
         ----- end pattern -----\n"
        (Xlang.to_string xlang) message pattern
  | MissingPositiveTermInAnd ->
      "you need at least one positive term (not just negations or conditions)"
  | DeprecatedFeature s -> spf "deprecated feature: %s" s
  | InvalidOther s -> s

(* General errors

   TODO: define one exception for all this because pattern-matching
   on exceptions has no exhaustiveness checking.
*)
exception InvalidRule of invalid_rule_error
exception InvalidYaml of string * Parse_info.t
exception DuplicateYamlKey of string * Parse_info.t
exception UnparsableYamlException of string
exception ExceededMemoryLimit of string

let string_of_invalid_rule_error ((kind, rule_id, pos) : invalid_rule_error) =
  spf "invalid rule %s, %s: %s" rule_id
    (Parse_info.string_of_info pos)
    (string_of_invalid_rule_error_kind kind)

(*
   Exception printers for Printexc.to_string.
*)
let opt_string_of_exn (exn : exn) =
  match exn with
  | InvalidRule x -> Some (string_of_invalid_rule_error x)
  | InvalidYaml (msg, pos) ->
      Some (spf "invalid YAML, %s: %s" (Parse_info.string_of_info pos) msg)
  | DuplicateYamlKey (key, pos) ->
      Some
        (spf "invalid YAML, %s: duplicate key %S"
           (Parse_info.string_of_info pos)
           key)
  | UnparsableYamlException s ->
      (* TODO: what's the string s? *)
      Some (spf "unparsable YAML: %s" s)
  | ExceededMemoryLimit s ->
      (* TODO: what's the string s? *)
      Some (spf "exceeded memory limit: %s" s)
  | _ -> None

(* to be called by the application's main() *)
let register_exception_printer () = Printexc.register_printer opt_string_of_exn

(*****************************************************************************)
(* Visitor/extractor *)
(*****************************************************************************)
(* currently used in Check_rule.ml metachecker *)
let rec visit_new_formula f formula =
  match formula with
  | P (p, _) -> f p
  | Not (_, x) -> visit_new_formula f x
  | Or (_, xs)
  | And { conjuncts = xs; _ } ->
      xs |> List.iter (visit_new_formula f)

(* used by the metachecker for precise error location *)
let tok_of_formula = function
  | And { tok = t; _ }
  | Or (t, _)
  | Not (t, _) ->
      t
  | P (p, _) -> snd p.pstr

let kind_of_formula = function
  | P _ -> "pattern"
  | Or _
  | And _
  | Not _ ->
      "formula"

(*****************************************************************************)
(* Converters *)
(*****************************************************************************)

(* Substitutes `$MVAR` with `int($MVAR)` in cond. *)
let rewrite_metavar_comparison_strip mvar cond =
  let visitor =
    Map_AST.mk_visitor
      {
        Map_AST.default_visitor with
        Map_AST.kexpr =
          (fun (k, _) e ->
            (* apply on children *)
            let e = k e in
            match e.G.e with
            | G.N (G.Id ((s, tok), _idinfo)) when s = mvar ->
                let py_int = G.Id (("int", tok), G.empty_id_info ()) in
                G.Call (G.N py_int |> G.e, G.fake_bracket [ G.Arg e ]) |> G.e
            | _ -> e);
      }
  in
  visitor.Map_AST.vexpr cond

(* TODO This is ugly because depends-on is inside the formula
   but handled in Python. It might be that the only sane answer is
   to port it to OCaml *)
let remove_noop (e : formula_old) : formula_old =
  let valid_formula x =
    match x with
    | PatFilteredInPythonTodo _ -> false
    | _ -> true
  in
  let rec aux e =
    match e with
    | PatEither (t, xs) ->
        let xs = Common.map aux (List.filter valid_formula xs) in
        PatEither (t, xs)
    | Patterns (t, xs) -> (
        let xs' = Common.map aux (List.filter valid_formula xs) in
        (* If the only thing in Patterns is a PatFilteredInPythonTodo key,
           after this filter it will be an empty And. To prevent
           an error, check for that *)
        match (xs, xs') with
        | [ x ], [] -> aux x
        | _ -> Patterns (t, xs'))
    | PatFilteredInPythonTodo t ->
        (* If a PatFilteredInPythonTodo key is the only thing on the top
           level, return no matches *)
        PatEither (t, [])
    | _ -> e
  in
  aux e

(* return list of "positive" x list of Not *)
let split_and : formula list -> formula list * formula list =
 fun xs ->
  xs
  |> Common.partition_either (fun e ->
         match e with
         (* positives *)
         | P _
         | And _
         | Or _ ->
             Left e
         (* negatives *)
         | Not (_, f) -> Right f)

let rec (convert_formula_old :
          ?in_metavariable_pattern:bool ->
          rule_id:rule_id ->
          formula_old ->
          formula) =
 fun ?(in_metavariable_pattern = false) ~rule_id e ->
  let rec aux e =
    match e with
    | Pat x -> P (x, None)
    | PatInside x -> P (x, Some Inside)
    | PatNot (t, x) -> Not (t, P (x, None))
    | PatNotInside (t, x) -> Not (t, P (x, Some Inside))
    | PatEither (t, xs) ->
        let xs = Common.map aux xs in
        Or (t, xs)
    | Patterns (t, xs) ->
        let fs, conds, focus = Common.partition_either3 aux_and xs in
        let pos, _ = split_and fs in
        if pos = [] && not in_metavariable_pattern then
          raise (InvalidRule (MissingPositiveTermInAnd, rule_id, t));
        And { tok = t; conjuncts = fs; conditions = conds; focus }
    | PatExtra (t, _) ->
        raise
          (InvalidYaml
             ("metavariable conditions must be inside a 'patterns:'", t))
    | PatFocus (t, _) ->
        raise
          (InvalidYaml ("'focus-metavariable:' must be inside a 'patterns:'", t))
    | PatFilteredInPythonTodo t -> raise (InvalidYaml ("Unexpected key", t))
  and aux_and e =
    match e with
    | PatExtra (t, x) ->
        let e = convert_extra ~t ~rule_id x in
        Middle3 (t, e)
    | PatFocus (t, mvar) -> Right3 (t, mvar)
    | _ -> Left3 (aux e)
  in
  aux (remove_noop e)

and convert_extra ~t ~rule_id x =
  match x with
  | MetavarRegexp (mvar, re, const_prop) -> CondRegexp (mvar, re, const_prop)
  | MetavarPattern (mvar, opt_xlang, formula_old) ->
      let formula =
        convert_formula_old ~in_metavariable_pattern:true ~rule_id formula_old
      in
      CondNestedFormula (mvar, opt_xlang, formula)
  | MetavarComparison comp -> (
      match comp with
      (* do we care about strip and base? should not Eval_generic handle it?
       * - base is handled automatically, in the Generic AST all integer
       *   literals are normalized and represented in base 10.
       * - for strip the user should instead use a more complex condition that
       *   converts the string into a number (e.g., "1234" in 1234).
       *)
      | { metavariable = mvar; comparison; strip; base = _NOT_NEEDED } ->
          let cond =
            (* if strip=true we rewrite the condition and insert Python's `int`
             * function to parse the integer value of mvar. *)
            match (mvar, strip) with
            | _, None
            | _, Some false ->
                comparison
            | None, Some true ->
                (* This error should be caught already in Parse_rule, this is just
                   defensive programming. *)
                let error_msg =
                  "'metavariable-comparison' is missing 'metavariable' despite \
                   'strip: true'"
                in
                logger#error "%s" error_msg;
                raise (InvalidRule (InvalidOther error_msg, rule_id, t))
            | Some mvar, Some true ->
                rewrite_metavar_comparison_strip mvar comparison
          in
          CondEval cond)
  | MetavarAnalysis (mvar, kind) -> CondAnalysis (mvar, kind)

let formula_of_pformula ?in_metavariable_pattern ~rule_id = function
  | New f -> f
  | Old oldf -> convert_formula_old ?in_metavariable_pattern ~rule_id oldf
