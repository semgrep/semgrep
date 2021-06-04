(* Yoann Padioleau
 *
 * Copyright (C) 2021 r2c
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
module Re = Regexp_engine.Re_engine
module Pcre = Regexp_engine.Pcre_engine
module R = Rule
module MV = Metavariable

let logger = Logging.get_logger [ __MODULE__ ]

[@@@warning "-32"] (* for the unused pp_ coming from deriving show *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Analyzing a semgrep rule for optimization purpose.
 *
 * Analyze_pattern.ml tries to extract a regexp from a pattern
 * in order to skip certain target files. However, it processes only
 * one pattern at a time and is not aware of the context in which this
 * pattern is used. For example in:
 *
 *  id: eval-not-in-foo
 *  patterns:
 *    - pattern: eval(...)
 *    - pattern-not:
 *        require("foo.js")
 *        ...
 *
 * the current Mini_rules_filter used in Semgrep_generic will just see a flat
 * list of patterns, and will look separately for 'eval' and 'foo.js'
 * and filter certain patterns; But really, even if 'foo.js' is mentioned
 * in a file, we should completly skip the file if 'eval' is not in the file
 * because after all 'foo.js' is mentioned in a pattern-not context!
 *
 *
 * There are many optimization opportunities when semgrep-core can see
 * the whole rule:
 *  - skip the pattern-not when computing the regexp
 *  - TODO if a pattern is very general (e.g., $FOO()), but is
 *    also mentioned in a metavariable-regexp, then we can use this
 *    regexp to filter the rule/target file.
 *  - TODO if a pattern is very general (e.g., $PROP), but reference
 *    metavariables used in all the patterns of a disjunction, then you
 *    can skip this pattern
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* Conjonctive normal form (CNF).
 *
 * Why a CNF instead of a DNF (Disjunctive normal form)?
 * Because in the context of producing a regexp, regexps are good at
 * representing Or, but not so great for And.
 *
 * todo? the evaluation engine prefers to work on DNF where negation
 * must be inside a And, so maybe better to work also on DNF here?
 * and skip correctly the negations?
 * update: I now use run_cnf_step2 to eval a cnf, so I could do it
 * on a dnf too because I don't anymore reduce everything to a single regexp
 * (it's fast enough to run many regexps on a file).
 *
 *)
type 'a cnf = And of 'a disj list

(* no need for negation, they are filtered *)
and 'a disj = Or of 'a list [@@deriving show]

(* can't filter a file if there's no specific identifier in the pattern *)
exception GeneralPattern

exception EmptyAnd

exception EmptyOr

(*****************************************************************************)
(* Step0: a complex formula to a CNF *)
(*****************************************************************************)
(* Transforming a complex formula to a simple CNF formula.
 *
 * old: I used to do the CNF transformation while still having negations in the
 * formula, and later in step1 remove the Not. However, this does not work!
 * Indeed, in:
 *    (foo/\not xxx) \/ (bar/\not yyy),
 * we should analyze a file if 'foo' *or* 'bar' are in the file. However,
 * the CNF transformation would distribute the \/ with:
 *    (foo\/bar) /\ (foo\/not yyy) /\ (not xxx\/bar) /\ (not xxx\/not yyy)
 * but then in step1 if we remove the not, we get:
 *    (foo\/bar) /\ foo /\ bar
 * and suddently we strongly require 'foo' *and* 'bar' to be in the file.
 * Thus, we must filter the Not before doing the CNF conversion!
 *)

(* less: move the Not to leaves, applying DeMorgan, and then filter them? *)
let rec (remove_not : Rule.formula -> Rule.formula option) =
 fun f ->
  match f with
  | R.And xs ->
      let ys = Common.map_filter remove_not xs in
      if null ys then failwith "null And after remove_not" else Some (R.And ys)
  | R.Or xs ->
      let ys = Common.map_filter remove_not xs in
      if null ys then failwith "null Or after remove_not" else Some (R.Or ys)
  | R.Not f -> (
      match f with
      | R.Leaf _ -> None
      (* double negation *)
      | R.Not f -> remove_not f
      (* todo? apply De Morgan's law? *)
      | R.Or _xs -> failwith "Not Or"
      | R.And _xs -> failwith "Not And" )
  | R.Leaf x -> Some (R.Leaf x)

let remove_not_final f =
  match remove_not f with Some f -> f | None -> failwith "no formula"

type step0 = L of Rule.leaf
(*old: does not work: | Not of Rule.leaf | Pos of Rule.leaf *)
[@@deriving show]

type cnf_step0 = step0 cnf [@@deriving show]

(* reference? https://www.cs.jhu.edu/~jason/tutorials/convert-to-CNF.html *)
let rec (cnf : Rule.formula -> cnf_step0) =
 fun f ->
  match f with
  | R.Leaf x -> And [ Or [ L x ] ]
  | R.Not _f ->
      (* should be filtered by remove_not *)
      failwith "call remove_not before cnf"
  (* old:
   * (match f with
   * | R.Leaf x -> And [Or [Not x]]
   * (* double negation *)
   * | R.Not f -> cnf f
   * (* de Morgan's laws *)
   * | R.Or _xs -> failwith "Not Or"
   * | R.And _xs -> failwith "Not And"
   * )
   *)
  | R.And xs ->
      let ys = List.map cnf xs in
      And (ys |> List.map (function And ors -> ors) |> List.flatten)
  | R.Or xs ->
      let ys = List.map cnf xs in
      let rec aux ys =
        match ys with
        | [] -> And []
        | [ x ] -> x
        | [ And ps; And qs ] ->
            And
              ( ps
              |> List.map (fun pi ->
                     let ands =
                       qs
                       |> List.map (fun qi ->
                              let (Or pi_ors) = pi in
                              let (Or qi_ors) = qi in
                              let ors = pi_ors @ qi_ors in
                              Or ors)
                     in
                     ands)
              |> List.flatten )
        | x :: xs ->
            let y = aux xs in
            aux [ x; y ]
      in
      aux ys

(*****************************************************************************)
(* Step1: just collect strings, mvars, regexps *)
(*****************************************************************************)
type step1 =
  | StringsAndMvars of string list * MV.mvar list
  | Regexp of Rule.regexp
  | MvarRegexp of MV.mvar * Rule.regexp
[@@deriving show]

type cnf_step1 = step1 cnf [@@deriving show]

(* simple for now, don't do any conversion *)

(*
let rec (and_step1: Rule.formula -> cnf_step1) = fun f ->
  match f with
  | R.And xs -> And (xs |> Common.map_filter or_step1)
  | _ -> And ([f] |> Common.map_filter or_step1)
and or_step1 f =
  match f with
  | R.Or xs ->
      let ys = (xs |> Common.map_filter leaf_step1) in
      if null ys
      then None
      else (Some (Or ys))
  | _ -> let ys = ([f] |> Common.map_filter leaf_step1) in
      if null ys
      then None
      else (Some (Or ys))

and leaf_step1 f =
  match f with
  | R.And _ | R.Or _ -> failwith "nested And or Or"
  (* we can filter that *)
  | R.Not _ -> None
  | R.Leaf (R.P pat) -> xpat_step1 pat
  | R.Leaf (R.MetavarCond x) ->
      metavarcond_step1 x
*)

let rec (and_step1 : cnf_step0 -> cnf_step1) =
 fun cnf -> match cnf with And xs -> And (xs |> Common.map_filter or_step1)

and or_step1 cnf =
  match cnf with
  | Or xs ->
      let ys = xs |> Common.map_filter leaf_step1 in
      if null ys then None else Some (Or ys)

and leaf_step1 f =
  match f with
  (* old: we can't filter now; too late, see comment above on step0 *)
  (*  | Not _ -> None *)
  | L (R.P (pat, _inside)) -> xpat_step1 pat
  | L (R.MetavarCond x) -> metavarcond_step1 x

and xpat_step1 pat =
  match pat.R.pat with
  | R.Sem (pat, lang) ->
      let ids, mvars = Analyze_pattern.extract_strings_and_mvars ~lang pat in
      Some (StringsAndMvars (ids, mvars))
  (* less: could also extract ids and mvars, but maybe no need to
   * prefilter for spacegrep; it is probably fast enough already
   *)
  | R.Regexp re -> Some (Regexp re)
  (* todo? *)
  | R.Spacegrep _ -> None
  | R.Comby _ -> None

and metavarcond_step1 x =
  match x with
  | R.CondGeneric _ -> None
  | R.CondPattern _ -> None
  | R.CondRegexp (mvar, re) ->
      (* bugfix: if the metavariable-regexp is "^(foo|bar)$" we
       * don't want to keep it because it can't be used on the whole file.
       * TODO: remove the anchor so it's usable?
       *)
      if regexp_contain_anchor re then None else Some (MvarRegexp (mvar, re))

(* todo: check for other special chars? *)
and regexp_contain_anchor (s, _re) = s =~ ".*[^$]"

(*****************************************************************************)
(* Step2: no more metavariables *)
(*****************************************************************************)
(* TODO: filter patterns without idents but with mvar mentioned
 * in an And in another branch.
 * TODO: replace some Idents [], MVar where mvar mentioned in a
 * MvarRegexp into a Regexp2
 *)

let and_step1bis_filter_general (And xs) =
  let has_empty_idents, rest =
    xs
    |> Common.partition_either (function Or xs ->
           if
             xs
             |> List.exists (function
                  | StringsAndMvars ([], _) -> true
                  | _ -> false)
           then Left (Or xs)
           else Right (Or xs))
  in
  (* TODO: regression on vertx-sqli.yaml   *)
  let filtered =
    has_empty_idents
    |> Common.map_filter (fun (Or xs) ->
           let xs' =
             xs
             |> Common.exclude (function
                  | StringsAndMvars ([], mvars) ->
                      mvars
                      |> List.exists (fun mvar ->
                             rest
                             |> List.exists (function Or xs ->
                                    xs
                                    |> List.for_all (function
                                         | StringsAndMvars (_, mvars) ->
                                             List.mem mvar mvars
                                         | Regexp _ -> false
                                         | MvarRegexp (mvar2, _) -> mvar2 = mvar)))
                  | _ -> false)
           in
           if null xs' then None else Some (Or xs'))
  in
  And (filtered @ rest)
  [@@profiling]

type step2 =
  | Idents of string list
  (* a And *)
  | Regexp2 of Rule.regexp
[@@deriving show]

type cnf_step2 = step2 cnf [@@deriving show]

let or_step2 (Or xs) =
  (* sanity check *)
  xs
  |> List.iter (function
       | StringsAndMvars ([], _) -> raise GeneralPattern
       | _ -> ());
  let ys =
    xs
    |> List.map (function
         | StringsAndMvars (xs, _) -> Idents xs
         | Regexp re -> Regexp2 re
         | MvarRegexp (_mvar, re) -> Regexp2 re)
  in
  Or ys

let and_step2 (And xs) =
  let ys = xs |> List.map or_step2 in
  And ys

(*****************************************************************************)
(* Final Step: just regexps? *)
(*****************************************************************************)
(*
(* support alt which can be convenient *)
type regexp = Regexp_engine.Re_engine.t
[@@deriving show]

type final_step = Re of regexp
[@@deriving show]

(* just a And *)
type cnf_final = AndFinal of final_step list
[@@deriving show]

let or_final (Or xs) =
  let ys = xs |> List.map (function
   | Idents [] -> raise Impossible
   (* take the first one *)
   | Idents (x::_) -> Re.matching_exact_string x
   | Regexp2 (s, _re) ->
        (* PCRE regular expression not supported by Re, grrr *)
        try Re.regexp s
        with _ -> failwith (spf "Could not parse regexp: %s" s)
   ) in
   match ys with
   | [] -> None
   | y::ys ->
    let combined =
      ys |> List.fold_left (fun acc e -> Re.alt acc e) y
    in
    Some (Re combined)

(* TODO: normalize, merge similar Idents *)

(* TODO: detect if all cases are an Idents in which case you can lift
 * up the Idents in an AndFinal
 *)
let and_final (And disjs) =
  AndFinal (disjs |> Common.map_filter or_final)

(* todo: instead of running multiple times for the AndFinal, we could
 * do an or, look at the matched string and detect which parts of the
 * AndFinal it is, increment a counter, and at the end make sure we've
 * found each AndFinal elements.
 *)
let _run_final (AndFinal xs) big_str =
  xs |> List.for_all (fun (Re re) ->
    Re.run re big_str
  )
[@@profiling]

  let final = and_final cnf in
(*  pr2 (show_cnf_final final); *)
  final, cnf_step2

 *)

(*****************************************************************************)
(* Run the regexps *)
(*****************************************************************************)

let eval_and p (And xs) =
  if null xs then raise EmptyAnd;
  xs
  |> List.for_all (function Or xs ->
         if null xs then raise EmptyOr;
         xs |> List.exists (fun x -> p x) |> fun v ->
         if not v then logger#info "this Or failed: %s" (Common.dump (Or xs));
         v)

let run_cnf_step2 cnf big_str =
  cnf
  |> eval_and (function
       | Idents xs ->
           xs
           |> List.for_all (fun id ->
                  (* TODO: matching_exact_word does not work, why?? *)
                  let re = Pcre.matching_exact_string id in
                  Pcre.run re big_str)
       | Regexp2 re -> Pcre.run re big_str)
  [@@profiling]

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let compute_final_cnf f =
  let f = remove_not_final f in
  let cnf = cnf f in
  logger#ldebug (lazy (spf "cnf0 = %s" (show_cnf_step0 cnf)));
  (* let cnf = and_step1 f in *)
  let cnf = and_step1 cnf in
  logger#ldebug (lazy (spf "cnf1 = %s" (show_cnf_step1 cnf)));
  (* TODO: regression on vertx-sqli.yaml
     let cnf = and_step1bis_filter_general cnf in
     logger#ldebug (lazy (spf "cnf1bis = %s" (show_cnf_step1 cnf)));
  *)
  let cnf = and_step2 cnf in
  logger#ldebug (lazy (spf "cnf2 = %s" (show_cnf_step2 cnf)));
  cnf
  [@@profiling]

let str_final final = show_cnf_step2 final [@@profiling]

let regexp_prefilter_of_formula f =
  try
    let final = compute_final_cnf f in
    Some
      ( str_final final,
        fun big_str ->
          try
            run_cnf_step2 final big_str
            (* run_cnf_step2 (And [Or [Idents ["jsonwebtoken"]]]) big_str *)
          with
          (* can happen in spacegrep rules as we don't extract anything from t *)
          | EmptyAnd | EmptyOr ->
            true )
  with GeneralPattern -> None

let hmemo = Hashtbl.create 101

let regexp_prefilter_of_rule r =
  let k = r.R.file ^ "." ^ r.R.id in
  Common.memoized hmemo k (fun () ->
      r |> Rule.formula_of_rule |> regexp_prefilter_of_formula)
