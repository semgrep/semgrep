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
type predicate = Predicate.t =
  | String of { needle : Base.String.t; case_sensitive : Base.Bool.t }
  | Regex of Pcre2_.t
[@@deriving show, eq, ord, hash]

type t = predicate Formula.t [@@deriving show]

let of_rule ?(interfile = false) (rule : Rule.t) : t option =
  (* Use analysis logic from Analyze_rule - no conversion needed since Predicate.t = Predicate.t *)
  Analyze_rule.generate_prefilter ~interfile rule

let check (prefilter : t) (content : string) : bool =
  Formula.eval (fun pred -> Predicate.eval pred content) prefilter

let check_many (prefilters : t list) (content : string) : bool list =
  List.map (fun prefilter -> check prefilter content) prefilters

let to_formula (prefilter : t) : predicate Formula.t = prefilter
let of_formula (formula : predicate Formula.t) : t = formula

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
  convert_formula prefilter

module Private = struct
  let of_formula ~interfile ~analyzer (rule_formula : Rule.formula) : t option =
    (* No conversion needed - Analyze_rule returns the right type directly! *)
    Analyze_rule.generate_prefilter_from_formula ~interfile ~analyzer
      rule_formula
end
