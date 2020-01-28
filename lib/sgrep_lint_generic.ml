(* Yoann Padioleau
 *
 * Copyright (C) 2011 Facebook
 * Copyright (C) 2019 r2c
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
open Ast_generic
module V = Visitor_ast
module E = Error_code
module PI = Parse_info
module R = Rule

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The goal of this module is to make it easy to add lint rules by using
 * sgrep patterns. You just have to store in a special file the patterns
 * and the corresponding warning you want the linter to raise.
 *
 * update: if you need advanced patterns with boolean logic (which used
 * to be partially provided by the hacky OK error keyword), use
 * instead the sgrep python wrapper!
 *
 * todo: factorize code with sgrep_lint.ml
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let error matched_tokens _env rule =
  let tok = List.hd matched_tokens in
  match rule.R.severity with
  | R.Error ->
      E.error tok (E.SgrepLint (rule.R.id, rule.R.message))
  | R.Warning ->
      E.warning tok (E.SgrepLint (rule.R.id, rule.R.message))


(*****************************************************************************)
(* Checking *)
(*****************************************************************************)

let check2 rules ast =

  (* todo: use Normalize_ast.normalize like in sgrep_generic.ml *)

  (* This is similar to what we do in main_sgrep.ml except we apply
   * a list of sgrep patterns to the AST.
   *)

  let stmt_rules = ref [] in
  let expr_rules = ref [] in
  (* we use List.rev here because we push2 below and we want the
   * first rule in the files to be also the first rules in the x_rules
   * lists for the 'OK' to work (see the comment above about OK).
   *)
  List.rev rules |> List.iter (fun rule ->
    match rule.R.pattern with
    | E pattern  -> Common.push (pattern, rule) expr_rules
    | S pattern -> Common.push (pattern, rule) stmt_rules
    | _ -> failwith "only expr and stmt patterns supported for now"
  );

  let visitor = V.mk_visitor { V.default_visitor with
    V.kexpr = (fun (k, _) expr ->
      (* this could be quite slow ... we match many sgrep patterns
       * against an expression recursively
       *)
      !expr_rules |> List.iter (fun (pattern, rule) -> 
         let matches_with_env = Sgrep_generic.match_e_e pattern expr in
         if matches_with_env <> []
         then (* Found a match *)
           let matched_tokens = Lib_ast.ii_of_any (E expr) in
           error matched_tokens matches_with_env rule
      );
      (* try the rules on subexpressions *)
      k expr
    );

    (* mostly copy paste of expr code but with the _st functions *)
    V.kstmt = (fun (k, _) stmt ->
      !stmt_rules |> List.iter (fun (pattern, rule) -> 
         let matches_with_env = Sgrep_generic.match_st_st pattern stmt in
         if matches_with_env <> []
         then (* Found a match *)
           let matched_tokens = Lib_ast.ii_of_any (S stmt) in
           error matched_tokens matches_with_env rule
      );
      (* try the rules on substatements and subexpressions *)
      k stmt
    );
  }
  in
  visitor (Pr ast);
  ()

let check rules a =
  Common.profile_code "Sgrep_lint.check" (fun () -> check2 rules a)
