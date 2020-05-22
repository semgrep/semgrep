(*s: semgrep/matching/Semgrep_generic.ml *)
(* Yoann Padioleau
 *
 * Copyright (C) 2011 Facebook
 * Copyright (C) 2019 r2c
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
open AST_generic

module V = Visitor_AST
module M = Map_AST
module AST = AST_generic
module Err = Error_code
module PI = Parse_info
module R = Rule
module Eq = Equivalence
module Res = Match_result
module GG = Generic_vs_generic
module MV = Metavars_generic
module Flag = Flag_semgrep

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Main matching engine behind sgrep. This module implements mainly
 * the expr/stmt visitor, while generic_vs_generic does the matching.
 *
 * history: this file was split in sgrep_generic.ml for -e/-f and
 * sgrep_lint_generic.ml for -rules_file. The -e/-f returns results as
 * it goes and takes a single pattern while -rules_file applies a list
 * of patterns and return a result just at the end. We have now factorized
 * the two files because of many bugs and discrepancies between the
 * two operating modes. It was easy to forget to add a new feature in
 * one of the file. Now -rules_file and -e/-f work mostly the same.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*s: type [[Semgrep_generic.matcher]] *)
type ('a, 'b) matcher = 'a -> 'b ->
  Metavars_generic.metavars_binding list
(*e: type [[Semgrep_generic.matcher]] *)

(*****************************************************************************)
(* Matchers *)
(*****************************************************************************)

(*s: function [[Semgrep_generic.match_e_e]] *)
let match_e_e pattern e =
  let env = Matching_generic.empty_environment () in
  GG.m_expr pattern e env
(*e: function [[Semgrep_generic.match_e_e]] *)

(*s: function [[Semgrep_generic.match_st_st]] *)
let match_st_st pattern e =
  let env = Matching_generic.empty_environment () in
  GG.m_stmt pattern e env
(*e: function [[Semgrep_generic.match_st_st]] *)

(*s: function [[Semgrep_generic.match_sts_sts]] *)
let match_sts_sts pattern e =
  let env = Matching_generic.empty_environment () in
  GG.m_stmts_deep pattern e env
(*e: function [[Semgrep_generic.match_sts_sts]] *)

(*s: function [[Semgrep_generic.match_any_any]] *)
(* for unit testing *)
let match_any_any pattern e =
  let env = Matching_generic.empty_environment () in
  GG.m_any pattern e env
(*e: function [[Semgrep_generic.match_any_any]] *)

(*****************************************************************************)
(* Matchers for code equivalence mode *)
(*****************************************************************************)

(*s: function [[Semgrep_generic.match_e_e_for_equivalences]] *)
let match_e_e_for_equivalences a b =
  Common.save_excursion Flag.equivalence_mode true (fun () ->
  Common.save_excursion Flag.go_deeper_expr false (fun () ->
  Common.save_excursion Flag.go_deeper_stmt false (fun () ->
    match_e_e a b
  )))
(*e: function [[Semgrep_generic.match_e_e_for_equivalences]] *)

(*****************************************************************************)
(* Substituters *)
(*****************************************************************************)
(*s: function [[Semgrep_generic.subst_e]] *)
let subst_e (bindings: MV.metavars_binding) e =
  let visitor = M.mk_visitor { M.default_visitor with
    M.kexpr = (fun (k, _) x ->
      match x with
      | AST.Id ((str,_tok), _id_info) when MV.is_metavar_name str ->
          (match List.assoc_opt str bindings with
          | Some (AST.E e) ->
              (* less: abstract-line? *)
              e
          | Some _ ->
             failwith (spf "incompatible metavar: %s, was expecting an expr"
                      str)
          | None ->
             failwith (spf "could not find metavariable %s in environment"
                      str)
          )
      | _ -> k x
    );
   }
  in
  visitor.M.vexpr e
(*e: function [[Semgrep_generic.subst_e]] *)

(*****************************************************************************)
(* Applying code equivalences *)
(*****************************************************************************)

(*s: function [[Semgrep_generic.apply_equivalences]] *)
let apply_equivalences equivs any =
  let expr_rules = ref [] in
  let stmt_rules = ref [] in

  equivs |> List.iter (fun {Eq. left; op; right; _ } ->
    match left, op, right with
    | E l, Eq.Equiv, E r ->
          Common.push (l, r) expr_rules;
          Common.push (r, l) expr_rules;
    | E l, Eq.Imply, E r ->
          Common.push (l, r) expr_rules;
    | S l, Eq.Equiv, S r ->
          Common.push (l, r) stmt_rules;
          Common.push (r, l) stmt_rules;
    | S l, Eq.Imply, S r ->
          Common.push (l, r) stmt_rules;
    | _ -> failwith "only expr and stmt equivalence patterns are supported"
  );
  (* the order matters, keep the original order reverting Common.push *)
  let expr_rules = List.rev !expr_rules in
  let _stmt_rulesTODO = List.rev !stmt_rules in

  let visitor = M.mk_visitor { M.default_visitor with
    M.kexpr = (fun (k, _) x ->
       (* transform the children *)
       let x' = k x in

       let rec aux xs =
         match xs with
         | [] -> x'
         | (l, r)::xs ->
           (* look for a match on original x, not x' *)
           let matches_with_env = match_e_e_for_equivalences l x in
           (match matches_with_env with
           (* todo: should generate a Disj for each possibilities? *)
           | env::_xs ->
           (* Found a match *)
             let alt = subst_e env r (* recurse on r? *) in
             if Lib_AST.abstract_position_info_any (AST.E x) =*=
                Lib_AST.abstract_position_info_any (AST.E alt)
             then x'
             (* disjunction (if different) *)
             else AST.DisjExpr (x', alt)

           (* no match yet, trying another equivalence *)
           | [] -> aux xs
           )
        in
        aux expr_rules
    );
    M.kstmt = (fun (_k, _) x ->
      x
    );
   } in
  visitor.M.vany any
(*e: function [[Semgrep_generic.apply_equivalences]] *)


(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(*s: function [[Semgrep_generic.check2]] *)
let check2 ~hook rules equivs file lang ast =

  let matches = ref [] in

  (* rewrite code, e.g., A != B is rewritten as !(A == B)
   * update: this is less necessary once you have user-defined
   * code equivalences (see Equivalence.ml).
   *)
  let prog = Normalize_AST.normalize (Pr ast) lang in

  let expr_rules = ref [] in
  let stmt_rules = ref [] in
  let stmts_rules = ref [] in
  (*s: [[Semgrep_generic.check2()]] populate [[expr_rules]] and other *)
  rules |> List.iter (fun rule ->
    (* less: normalize the pattern? *)
    let any = rule.R.pattern in
    (*s: [[Semgrep_generic.check2()]] apply equivalences to rule pattern [[any]] *)
    let any = apply_equivalences equivs any in
    (*e: [[Semgrep_generic.check2()]] apply equivalences to rule pattern [[any]] *)
    match any with
    | E pattern  -> Common.push (pattern, rule) expr_rules
    | S pattern -> Common.push (pattern, rule) stmt_rules
    | Ss pattern -> Common.push (pattern, rule) stmts_rules
    | _ -> failwith "only expr, stmt, and stmts patterns are supported"
  );
  (*e: [[Semgrep_generic.check2()]] populate [[expr_rules]] and other *)

  let visitor = V.mk_visitor { V.default_visitor with
    (*s: [[Semgrep_generic.check2()]] visitor fields *)
    V.kexpr = (fun (k, _) x ->
      (* this could be quite slow ... we match many sgrep patterns
       * against an expression recursively
       *)
      !expr_rules |> List.iter (fun (pattern, rule) ->
         let matches_with_env = match_e_e pattern x in
         if matches_with_env <> []
         then (* Found a match *)
           matches_with_env |> List.iter (fun env ->
             Common.push { Res. rule; file; env; code = E x } matches;
             let matched_tokens = lazy (Lib_AST.ii_of_any (E x)) in
             hook env matched_tokens
         )
      );
      (* try the rules on subexpressions *)
      (* this can recurse to find nested matching inside the
       * matched code itself *)
      k x
    );
    (*x: [[Semgrep_generic.check2()]] visitor fields *)
    (* mostly copy paste of expr code but with the _st functions *)
    V.kstmt = (fun (k, _) x ->
      !stmt_rules |> List.iter (fun (pattern, rule) ->
         let matches_with_env = match_st_st pattern x in
         if matches_with_env <> []
         then (* Found a match *)
           matches_with_env |> List.iter (fun env ->
             Common.push { Res. rule; file; env; code = S x } matches;
             let matched_tokens = lazy (Lib_AST.ii_of_any (S x)) in
             hook env matched_tokens
           )
      );
      (* try the rules on substatements and subexpressions *)
      k x
    );
    (*x: [[Semgrep_generic.check2()]] visitor fields *)
    V.kstmts = (fun (k, _) x ->
      (* this is potentially slower than what we did in Coccinelle with
       * CTL. We try every sequences. Hopefully the first statement in
       * the pattern will filter lots of sequences so we need to do
       * the heavy stuff (e.g., handling '...' between statements) rarely.
       *)
      !stmts_rules |> List.iter (fun (pattern, rule) ->
         let matches_with_env = match_sts_sts pattern x in
         if matches_with_env <> []
         then (* Found a match *)
           matches_with_env |> List.iter (fun env ->
             Common.push { Res. rule; file; env; code = Ss x } matches;
             let matched_tokens = lazy (Lib_AST.ii_of_any (Ss x)) in
             hook env matched_tokens
           )
      );
      k x
    );
    (*e: [[Semgrep_generic.check2()]] visitor fields *)
  }
  in
  (* later: opti: dont analyze certain ASTs if they do not contain
   * certain constants that interect with the pattern?
   * But this requires to analyze the pattern to extract those
   * constants (name of function, field, etc.).
   *)
  visitor prog;

  !matches |> List.rev
(*e: function [[Semgrep_generic.check2]] *)

(*s: function [[Semgrep_generic.check]] *)
let check ~hook rules equivs file lang =
  Common.profile_code "Sgrep_generic.check" (
    fun () -> check2 ~hook rules equivs file lang
  )
(*e: function [[Semgrep_generic.check]] *)
(*e: semgrep/matching/Semgrep_generic.ml *)
