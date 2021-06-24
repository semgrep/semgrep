(*s: semgrep/core/Rule.ml *)
(*s: pad/r2c copyright *)
(* Yoann Padioleau
 *
 * Copyright (C) 2019-2021 r2c
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
(*e: pad/r2c copyright *)
module G = AST_generic
module MV = Metavariable

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Data structure representing a Semgrep rule.
 *
 * See also Mini_rule.ml where formula and many other features disappears.
 *
 * TODO:
 *  - parse equivalences
 *)

(*****************************************************************************)
(* Extended languages and patterns *)
(*****************************************************************************)

(* less: merge with xpattern_kind? *)
type xlang =
  (* for "real" semgrep (the first language is used to parse the pattern) *)
  | L of Lang.t * Lang.t list
  (* for pattern-regex (less: rename LRegex? *)
  | LNone
  (* for spacegrep *)
  | LGeneric
[@@deriving show]

type regexp = Regexp_engine.Pcre_engine.t [@@deriving show, eq]

type xpattern = {
  pat : xpattern_kind;
  (* two patterns may have different indentation, we don't care. We can
   * rely on the equality on pat, which will do the right thing (e.g., abstract
   * away line position).
   * TODO: right now we have some false positives because
   * for example in Python assert(...) and assert ... are considered equal
   * AST-wise, but it might be a bug!.
   *)
  pstr : string; [@equal fun _ _ -> true]
  (* unique id, incremented via a gensym()-like function in mk_pat() *)
  pid : pattern_id; [@equal fun _ _ -> true]
}

and xpattern_kind =
  | Sem of Pattern.t * Lang.t (* language used for parsing the pattern *)
  | Spacegrep of Spacegrep.Pattern_AST.t
  | Regexp of regexp
  | Comby of string

(* used in the engine for rule->mini_rule and match_result gymnastic *)
and pattern_id = int [@@deriving show, eq]

let count = ref 0

let mk_xpat pat pstr =
  incr count;
  { pat; pstr; pid = !count }

let is_regexp xpat = match xpat.pat with Regexp _ -> true | _ -> false

(*****************************************************************************)
(* Formula (patterns boolean composition) *)
(*****************************************************************************)

(* Classic boolean-logic/set operators with text range set semantic.
 * The main complication is the handling of metavariables and especially
 * negation in the presence of metavariables.
 * TODO: add tok (Parse_info.t) for good metachecking error locations.
 *)
type formula =
  | Leaf of leaf
  | And of formula list
  | Or of formula list
  (* There are restrictions on where a Not can appear in a formula. It
   * should always be inside an And to be intersected with "positive" formula.
   *)
  | Not of formula

(* todo: try to remove this at some point, but difficult. See
 * https://github.com/returntocorp/semgrep/issues/1218
 *)
and inside = Inside

and leaf =
  (* Turn out, pattern: and pattern-inside: are slightly different so
   * we need to keep the information around.
   * (see tests/OTHER/rules/inside.yaml)
   * The same is true for pattern-not and pattern-not-inside
   * (see tests/OTHER/rules/negation_exact.yaml)
   *)
  | P of xpattern (* a leaf pattern *) * inside option
  | MetavarCond of metavar_cond

and metavar_cond =
  | CondGeneric of AST_generic.expr (* see Eval_generic.ml *)
  (* todo: at some point we should remove CondRegexp and have just
   * CondGeneric, but for now there are some
   * differences between using the matched text region of a metavariable
   * (which we use for MetavarRegexp) and using its actual value
   * (which we use for MetavarComparison), which translate to different
   * calls in Eval_generic.ml
   * update: this is also useful to keep separate from CondGeneric for
   * the "regexpizer" optimizer (see Analyze_rule.ml).
   *)
  | CondRegexp of MV.mvar * regexp
  | CondPattern of MV.mvar * Lang.t option * formula
[@@deriving show, eq]

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
  | PatNot of xpattern
  | PatExtra of extra
  (* pattern-inside: *)
  | PatInside of xpattern
  (* pattern-not-inside: *)
  | PatNotInside of xpattern
  (* pattern-either: Or *)
  | PatEither of formula_old list
  (* patterns: And *)
  | Patterns of formula_old list

(* extra conditions, usually on metavariable content *)
and extra =
  | MetavarRegexp of MV.mvar * regexp
  (* TODO: Generalize `Lang.t option` to `xlang`. *)
  | MetavarPattern of MV.mvar * Lang.t option * formula
  | MetavarComparison of metavariable_comparison
  | PatWherePython of string

(* arbitrary code, dangerous! *)

(* See also matching/eval_generic.ml *)
and metavariable_comparison = {
  metavariable : MV.mvar;
  comparison : AST_generic.expr;
  (* see Eval_generic.ml *)
  strip : bool option;
  base : int option;
}
[@@deriving show, eq]

(* pattern formula *)
type pformula = New of formula | Old of formula_old [@@deriving show, eq]

(*****************************************************************************)
(* The rule *)
(*****************************************************************************)

type rule = {
  (* mandatory fields *)
  id : string;
  formula : pformula;
  message : string;
  severity : Mini_rule.severity;
  languages : xlang;
  file : string;
  (* for metachecking error location *)
  (* optional fields *)
  equivalences : string list option;
  settings : Config_semgrep.t option;
  (* TODO: parse them *)
  fix : string option;
  fix_regexp : (regexp * int option * string) option;
  paths : paths option;
  (* ex: [("owasp", "A1: Injection")] but can be anything *)
  metadata : JSON.t option;
}

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
(* Visitor *)
(*****************************************************************************)
(* currently used in Check_rule.ml metachecker *)
let rec visit_new_formula f formula =
  match formula with
  | Leaf (P (p, _)) -> f p
  | Leaf (MetavarCond _) -> ()
  | Not x -> visit_new_formula f x
  | Or xs | And xs -> xs |> List.iter (visit_new_formula f)

(*****************************************************************************)
(* Converter *)
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
            match e with
            | G.N (G.Id ((s, tok), _idinfo)) as x when s = mvar ->
                let py_int = G.Id (("int", tok), G.empty_id_info ()) in
                G.Call (G.N py_int, G.fake_bracket [ G.Arg x ])
            | _ -> e);
      }
  in
  visitor.Map_AST.vexpr cond

let convert_extra x =
  match x with
  | MetavarRegexp (mvar, re) -> CondRegexp (mvar, re)
  | MetavarPattern (mvar, opt_lang, formula) ->
      CondPattern (mvar, opt_lang, formula)
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
            | None | Some false -> comparison
            | Some true -> rewrite_metavar_comparison_strip mvar comparison
          in
          CondGeneric cond )
  | PatWherePython _ ->
      (*
  logger#debug "convert_extra: %s" s;
  Parse_rule.parse_metavar_cond s
*)
      failwith (Common.spf "convert_extra: TODO: %s" (show_extra x))

let (convert_formula_old : formula_old -> formula) =
 fun e ->
  let rec aux e =
    match e with
    | Pat x -> Leaf (P (x, None))
    | PatInside x -> Leaf (P (x, Some Inside))
    | PatNot x -> Not (Leaf (P (x, None)))
    | PatNotInside x -> Not (Leaf (P (x, Some Inside)))
    | PatEither xs ->
        let xs = List.map aux xs in
        Or xs
    | Patterns xs ->
        let xs = List.map aux xs in
        And xs
    | PatExtra x ->
        let e = convert_extra x in
        Leaf (MetavarCond e)
  in
  aux e

let formula_of_pformula = function
  | New f -> f
  | Old oldf -> convert_formula_old oldf

let formula_of_rule r = formula_of_pformula r.formula

(*e: semgrep/core/Rule.ml *)
