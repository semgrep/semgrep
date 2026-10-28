(*
   Copyright (c) 2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* File-level rule prefilters. Rule analysis produces predicate formulas; on
   each target we evaluate those formulas with short-circuiting and reuse the
   result of predicates shared by multiple rules. *)
type predicate = Predicate.t =
  | String of { needle : Base.String.t; case_sensitive : Base.Bool.t }
  | Regex of Pcre2_.t
[@@deriving show, eq, ord, hash]

type t = {
  formula : predicate Formula.t;
      (** Necessary conditions for a rule to match the target contents. *)
  generated : bool;
      (** Whether [Analyze_rule] produced this formula, allowing [check_many]
          to memoize its predicate results across rules for one target.

          Generated regex predicates use uniform compile options. This matters
          because [Pcre2_.t] equality and hashing only consider the pattern
          text, so regexes with different options can otherwise share a cache
          key despite producing different results. [of_formula] sets this to
          [false] for caller-supplied formulas, which bypass memoization. *)
}

let pp fmt t = Formula.pp pp_predicate fmt t.formula
let show t = Format.asprintf "%a" pp t
let generated formula = { formula; generated = true }

let of_rule ?(interfile = false) (rule : Rule.t) : t option =
  (* Use analysis logic from Analyze_rule - no conversion needed since Predicate.t = Predicate.t *)
  Analyze_rule.generate_prefilter ~interfile rule |> Option.map generated

let check ?utf8_valid (prefilter : t) (content : string) : bool =
  Formula.eval
    (fun pred -> Predicate.eval ?utf8_valid pred content)
    prefilter.formula

let check_many ?utf8_valid (prefilters : t list) (content : string) : bool list
    =
  (* Different rules often ask the same question about a file. Evaluate each
     predicate only on its first use, while preserving Formula.eval's order
     and short-circuiting. Rule-generated regexes all use the same compile
     options, so equal predicates have equal results. Predicates supplied by
     callers via [of_formula] may use different regex options and bypass the
     cache. This table is local to the file and safe with parallel targets. *)
  let cache = Base.Hashtbl.create (module Predicate) in
  let eval pred =
    match Base.Hashtbl.find cache pred with
    | Some result -> result
    | None ->
        let result = Predicate.eval ?utf8_valid pred content in
        Base.Hashtbl.set cache ~key:pred ~data:result;
        result
  in
  List.map
    (fun prefilter ->
      if prefilter.generated then Formula.eval eval prefilter.formula
      else check ?utf8_valid prefilter content)
    prefilters

let to_formula (prefilter : t) : predicate Formula.t = prefilter.formula

let of_formula (formula : predicate Formula.t) : t =
  { formula; generated = false }

let to_semgrep_formula (prefilter : t) : Semgrep_prefilter_t.formula =
  (* Convert to the existing prefilter format for serialization *)
  let rec convert_formula = function
    | Formula.And xs -> (
        let xs' = List.map convert_formula xs in
        match xs' with
        | [] -> failwith "Empty And not supported"
        | [ x ] -> x
        | xs -> `And xs)
    | Formula.Or xs -> (
        let xs' = List.map convert_formula xs in
        match xs' with
        | [] -> failwith "Empty Or not supported"
        | [ x ] -> x
        | xs -> `Or xs)
    | Formula.Pred (String { needle = s; case_sensitive = true }) ->
        `Pred (`Idents [ s ])
    | Formula.Pred (String { needle = s; case_sensitive = false }) ->
        (* The external format has no case-insensitive string predicate, so we
           express it as a case-insensitive regexp of the (quoted) literal. *)
        `Pred (`Regexp ("(?i)" ^ Pcre2_.quote s))
    | Formula.Pred (Regex re) ->
        let re_str = Pcre2_.show re in
        `Pred (`Regexp re_str)
  in
  convert_formula prefilter.formula

module Private = struct
  let of_formula ~interfile ~analyzer (rule_formula : Rule.formula) : t option =
    (* No conversion needed - Analyze_rule returns the right type directly! *)
    Analyze_rule.generate_prefilter_from_formula ~interfile ~analyzer
      rule_formula
    |> Option.map generated
end
