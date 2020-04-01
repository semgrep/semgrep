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
open Ast_generic

module V = Visitor_ast
module Ast = Ast_generic
module E = Error_code
module PI = Parse_info
module R = Rule
module M = Match_result
module GG = Generic_vs_generic


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

type ('a, 'b) matcher = 'a -> 'b ->
  Metavars_generic.metavars_binding list

 
let match_e_e pattern e = 
  let env = GG.empty_environment () in
  GG.m_expr pattern e env

let match_st_st pattern e = 
  let env = GG.empty_environment () in
  GG.m_stmt pattern e env

let match_sts_sts pattern e = 
  let env = GG.empty_environment () in
  GG.m_stmts_deep pattern e env

(* for unit testing *)
let match_any_any pattern e = 
  let env = GG.empty_environment () in
  GG.m_any pattern e env

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let check2 ~hook rules _equivalences file ast =

   let matches = ref [] in

  (* rewrite code, e.g., A != B is rewritten as !(A == B) 
   * update: this is less necessary once you have user-defined
   * code equivalences (see equivalence.ml).
   *)
  let prog = Normalize_ast.normalize (Pr ast) in

  let expr_rules = ref [] in
  let stmt_rules = ref [] in
  let stmts_rules = ref [] in
  rules |> List.iter (fun rule ->
    (* less: normalize the pattern? *)
    match rule.R.pattern with
    | E pattern  -> Common.push (pattern, rule) expr_rules
    | S pattern -> Common.push (pattern, rule) stmt_rules
    | Ss pattern -> Common.push (pattern, rule) stmts_rules
    | _ -> failwith "only expr, stmt, and stmts patterns are supported"
  );

  let visitor = V.mk_visitor { V.default_visitor with
    V.kexpr = (fun (k, _) x ->
      (* this could be quite slow ... we match many sgrep patterns
       * against an expression recursively
       *)
      !expr_rules |> List.iter (fun (pattern, rule) -> 
         let matches_with_env = match_e_e pattern x in
         if matches_with_env <> []
         then (* Found a match *)
           matches_with_env |> List.iter (fun env ->
             Common.push { M. rule; file; env; code = E x } matches;
             let matched_tokens = lazy (Lib_ast.ii_of_any (E x)) in
             hook env matched_tokens
         )
      );
      (* try the rules on subexpressions *)
      (* this can recurse to find nested matching inside the
       * matched code itself *)
      k x
    );

    (* mostly copy paste of expr code but with the _st functions *)
    V.kstmt = (fun (k, _) x ->
      !stmt_rules |> List.iter (fun (pattern, rule) -> 
         let matches_with_env = match_st_st pattern x in
         if matches_with_env <> []
         then (* Found a match *)
           matches_with_env |> List.iter (fun env ->
             Common.push { M. rule; file; env; code = S x } matches;
             let matched_tokens = lazy (Lib_ast.ii_of_any (S x)) in
             hook env matched_tokens
           )
      );
      (* try the rules on substatements and subexpressions *)
      k x
    );

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
             Common.push { M. rule; file; env; code = Ss x } matches;
             let matched_tokens = lazy (Lib_ast.ii_of_any (Ss x)) in
             hook env matched_tokens
           )
      );
      k x
    );
  }
  in
  (* later: opti: dont analyze certain ASTs if they do not contain
   * certain constants that interect with the pattern?
   * But this requires to analyze the pattern to extract those
   * constants (name of function, field, etc.).
   *)
  visitor prog;
  !matches |> List.rev

let check ~hook a b c =
  Common.profile_code "Sgrep_generic.check" (fun () -> check2 ~hook a b c)
