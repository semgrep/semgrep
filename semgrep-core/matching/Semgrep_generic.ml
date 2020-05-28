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
let match_e_e2 pattern e =
  let env = Matching_generic.empty_environment () in
  GG.m_expr pattern e env
(*e: function [[Semgrep_generic.match_e_e]] *)
let match_e_e ruleid a b =
 Common.profile_code "Semgrep.match_e_e" (fun () ->
    Common.profile_code ("rule:" ^ ruleid) (fun () ->
      match_e_e2 a b))

(*s: function [[Semgrep_generic.match_st_st]] *)
let match_st_st2 pattern e =
  let env = Matching_generic.empty_environment () in
  GG.m_stmt pattern e env
(*e: function [[Semgrep_generic.match_st_st]] *)
let match_st_st ruleid a b =
  Common.profile_code "Semgrep.match_st_st" (fun () ->
    Common.profile_code ("rule:" ^ ruleid) (fun () ->
      match_st_st2 a b))

(*s: function [[Semgrep_generic.match_sts_sts]] *)
let match_sts_sts2 pattern e =
  let env = Matching_generic.empty_environment () in
  (* When matching statements, we need not only to report whether
   * there is match, but also the actual statements that were matched.
   * Indeed, even if we want the implicit '...' at the end of
   * a sequence of statements pattern (AST_generic.Ss) to match all
   * the rest, we don't want to report the whole Ss as a match but just
   * the actually matched subset.
   * To do so would require to change the interface of a matcher
   * to not only return the matched environment but also the matched
   * statements. This would require in turn to provide new versions
   * for >>=, >||>, etc.
   * Instead, we can abuse the environment to also record the
   * matched statements! This is a bit ugly, but the alternative might
   * be worse.
   *
   * TODO? do we need to generate unique key? we don't want
   * nested calls to m_stmts_deep to polluate our metavar? We need
   * to pass the key to m_stmts_deep?
   *)
  let key = MV.matched_statements_special_mvar in
  let env = (key, Ss [])::env in

  let res = GG.m_stmts_deep pattern e env in

  res |> List.map (fun tin ->
    match List.assoc_opt key tin with
    | Some (Ss xs) ->
          (* we use List.rev because Generic_vs_generic.env_add_matched_stmt
           * adds the matched statements gradually at the beginning
           * of the list
           *)
          List.remove_assoc key tin, (Ss (List.rev xs))
    | _ -> raise Impossible
  )
(*e: function [[Semgrep_generic.match_sts_sts]] *)
let match_sts_sts ruleid a b =
  Common.profile_code "Semgrep.match_sts_sts" (fun () ->
    Common.profile_code ("rule:" ^ ruleid) (fun () ->
      match_sts_sts2 a b))

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
let match_e_e_for_equivalences ruleid a b =
  Common.save_excursion Flag.equivalence_mode true (fun () ->
  Common.save_excursion Flag.go_deeper_expr false (fun () ->
  Common.save_excursion Flag.go_deeper_stmt false (fun () ->
    match_e_e ruleid a b
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
let apply_equivalences2 equivs any =
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
           let matches_with_env = match_e_e_for_equivalences "<equivalence>"
                    l x in
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
let apply_equivalences a b =
  Common.profile_code "Semgrep.apply_equivalences" (fun () ->
      apply_equivalences2 a b)

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
         let matches_with_env = match_e_e rule.R.id pattern x in
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
         let matches_with_env = match_st_st rule.R.id pattern x in
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
         let matches_with_env = match_sts_sts rule.R.id pattern x in
         if matches_with_env <> []
         then (* Found a match *)
           matches_with_env |> List.iter (fun (env, matched_statements) ->
             Common.push { Res. rule; file; env; code = matched_statements }
               matches;
             let matched_tokens = lazy (Lib_AST.ii_of_any matched_statements)
             in
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
let check ~hook a b c d e =
  Common.profile_code "Semgrep.check" (fun () -> check2 ~hook a b c d e)
(*e: function [[Semgrep_generic.check]] *)
(*e: semgrep/matching/Semgrep_generic.ml *)
