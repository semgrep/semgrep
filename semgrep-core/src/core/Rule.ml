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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Data structures to represent a Semgrep rule (=~ AST of a rule).
 *
 * See also Mini_rule.ml where formula and many other features disappear.
 *
 * TODO:
 *  - parse equivalences
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
(* Extended patterns *)
(*****************************************************************************)

type xpattern = {
  pat : xpattern_kind;
  (* Regarding @equal below, even if two patterns have different indentation,
   * we still consider them equal in the metachecker context.
   * We rely only on the equality on pat, which will
   * abstract away line positions.
   * TODO: right now we have some false positives, e.g., in Python
   * assert(...) and assert ... are considered equal AST-wise
   * but it might be a bug!.
   *)
  pstr : string wrap; [@equal fun _ _ -> true]
  (* Unique id, incremented via a gensym()-like function in mk_pat().
   * This is used to run the patterns in a formula in a batch all-at-once
   * and remember what was the matching results for a certain pattern id.
   *)
  pid : pattern_id; [@equal fun _ _ -> true]
}

and xpattern_kind =
  | Sem of Pattern.t * Lang.t (* language used for parsing the pattern *)
  | Spacegrep of Spacegrep.Pattern_AST.t
  | Regexp of regexp
  | Comby of string

and regexp = Regexp_engine.Pcre_engine.t

(* used in the engine for rule->mini_rule and match_result gymnastic *)
and pattern_id = int [@@deriving show, eq]

(* helpers *)

let count = ref 0

let mk_xpat pat pstr =
  incr count;
  { pat; pstr; pid = !count }

let is_regexp xpat =
  match xpat.pat with
  | Regexp _ -> true
  | _ -> false

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
  | P of xpattern (* a leaf pattern *) * inside option
  (* see Specialize_formula.split_and() *)
  | And of conjunction
  | Or of tok * formula list
  (* There are currently restrictions on where a Not can appear in a formula.
   * It must be inside an And to be intersected with "positive" formula.
   * But this could change? If we were moving to a different range semantic?
   *)
  | Not of tok * formula

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
  | CondRegexp of MV.mvar * regexp
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
  | Pat of xpattern
  (* pattern-not: *)
  | PatNot of tok * xpattern
  (* metavariable-xyz: *)
  | PatExtra of tok * extra
  (* focus-metavariable: *)
  | PatFocus of tok * MV.mvar
  (* pattern-inside: *)
  | PatInside of xpattern
  (* pattern-not-inside: *)
  | PatNotInside of tok * xpattern
  (* pattern-either: Or *)
  | PatEither of tok * formula_old list
  (* patterns: And *)
  | Patterns of tok * formula_old list
  (* for fields handled in Python like depends-on *)
  | PatFilteredInPythonTodo of tok

(* extra conditions, usually on metavariable content *)
and extra =
  | MetavarRegexp of MV.mvar * regexp
  | MetavarPattern of MV.mvar * Xlang.t option * formula_old
  | MetavarComparison of metavariable_comparison
  | MetavarAnalysis of MV.mvar * metavar_analysis_kind
  (* arbitrary code! dangerous! *)
  | PatWherePython of string

(* See also engine/Eval_generic.ml *)
and metavariable_comparison = {
  metavariable : MV.mvar;
  comparison : AST_generic.expr;
  (* I don't think those are really needed; they can be inferred
   * from the values *)
  strip : bool option;
  base : int option;
}
[@@deriving show, eq]

(* pattern formula *)
type pformula = New of formula | Old of formula_old [@@deriving show, eq]

(*****************************************************************************)
(* The rule *)
(*****************************************************************************)

(* alt:
 *     type common = { id : string; ... }
 *     type search = { common : common; formula : pformula; }
 *     type taint  = { common : common; spec : taint_spec; }
 *     type rule   = Search of search | Taint of taint
 *)

type sanitizer_spec = {
  not_conflicting : bool;
      (** If [not_conflicting] is enabled, the sanitizer cannot conflict with
    a sink or a source (i.e., match the exact same range) otherwise
    it is filtered out. This allows to e.g. declare `$F(...)` as a sanitizer,
    to assume that any other function will handle tainted data safely.
    Without this, `$F(...)` would automatically sanitize any other function
    call acting as a sink or a source. *)
  pformula : pformula;
}
[@@deriving show]

type taint_spec = {
  sources : pformula list;
  sanitizers : sanitizer_spec list;
  sinks : pformula list;
}
[@@deriving show]

type mode = Search of pformula | Taint of taint_spec [@@deriving show]

(* TODO? just reuse Error_code.severity *)
type severity = Error | Warning | Info | Inventory [@@deriving show]

type rule = {
  (* MANDATORY fields *)
  id : rule_id wrap;
  mode : mode;
  message : string;
  severity : severity;
  languages : Xlang.t;
  (* OPTIONAL fields *)
  options : Config_semgrep.t option;
  (* deprecated? todo: parse them *)
  equivalences : string list option;
  fix : string option;
  fix_regexp : (regexp * int option * string) option;
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

(* alias *)
type t = rule [@@deriving show]
type rules = rule list [@@deriving show]

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
  | InvalidOther of string

let string_of_invalid_rule_error_kind = function
  | InvalidLanguage language -> spf "invalid language %s" language
  | InvalidRegexp message -> spf "invalid regex %s" message
  (* coupling: this is actually intercepted in
   * Semgrep_error_code.exn_to_error to generate a PatternParseError instead
   * of a RuleParseError *)
  | InvalidPattern (_pattern, xlang, _message, _yaml_path) ->
      spf "Invalid pattern for %s" (Xlang.to_string xlang)
  | InvalidOther s -> s

exception InvalidRule of invalid_rule_error

(* general errors *)
exception InvalidYaml of string * Parse_info.t
exception DuplicateYamlKey of string * Parse_info.t
exception UnparsableYamlException of string
exception ExceededMemoryLimit of string

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
        let any = "a^" in
        Pat (mk_xpat (Regexp (any, SPcre.regexp any)) (any, t))
    | _ -> e
  in
  aux e

let rec (convert_formula_old : formula_old -> formula) =
 fun e ->
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
        let e = convert_extra x in
        Middle3 (t, e)
    | PatFocus (t, mvar) -> Right3 (t, mvar)
    | _ -> Left3 (aux e)
  in
  aux (remove_noop e)

and convert_extra x =
  match x with
  | MetavarRegexp (mvar, re) -> CondRegexp (mvar, re)
  | MetavarPattern (mvar, opt_xlang, formula_old) ->
      let formula = convert_formula_old formula_old in
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
            match strip with
            | None
            | Some false ->
                comparison
            | Some true -> rewrite_metavar_comparison_strip mvar comparison
          in
          CondEval cond)
  | MetavarAnalysis (mvar, kind) -> CondAnalysis (mvar, kind)
  | PatWherePython _ ->
      (*
  logger#debug "convert_extra: %s" s;
  Parse_rule.parse_metavar_cond s
*)
      failwith (Common.spf "convert_extra: TODO: %s" (show_extra x))

let formula_of_pformula = function
  | New f -> f
  | Old oldf -> convert_formula_old oldf

let partition_rules rules =
  rules
  |> Common.partition_either (fun r ->
         match r.mode with
         | Search f -> Left (r, f)
         | Taint s -> Right (r, s))
