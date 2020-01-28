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

(*****************************************************************************)
(* Checking *)
(*****************************************************************************)

(* coupling: this is similar to what we do in sgrep_generic.ml except we apply
 * a list of sgrep patterns to the AST.
 *)
let check2 rules file ast =
  (* todo: use Normalize_ast.normalize like in sgrep_generic.ml *)

  let matches = ref [] in

  let expr_rules = ref [] in
  let stmt_rules = ref [] in
  let stmts_rules = ref [] in
  rules |> List.iter (fun rule ->
    match rule.R.pattern with
    | E pattern  -> Common.push (pattern, rule) expr_rules
    | S pattern -> Common.push (pattern, rule) stmt_rules
    | Ss pattern -> Common.push (pattern, rule) stmts_rules
    | _ -> failwith "only expr, stmt, and stmts patterns are supported"
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
           matches_with_env |> List.iter (fun env ->
            Common.push
             { Match_result. rule; file; code = E expr; env = env }
             matches;
         )
      );
      (* try the rules on subexpressions *)
      k expr
    );

    (* mostly copy paste of expr code but with the _st functions *)
    V.kstmt = (fun (k, _) x ->
      !stmt_rules |> List.iter (fun (pattern, rule) -> 
         let matches_with_env = Sgrep_generic.match_st_st pattern x in
         if matches_with_env <> []
         then (* Found a match *)
           matches_with_env |> List.iter (fun env ->
            Common.push
             { Match_result. rule; file; code = S x; env = env }
             matches;
            )
      );
      (* try the rules on substatements and subexpressions *)
      k x
    );

    V.kstmts = (fun (k, _) x ->
      !stmts_rules |> List.iter (fun (pattern, rule) -> 
         let matches_with_env = Sgrep_generic.match_sts_sts pattern x in
         if matches_with_env <> []
         then (* Found a match *)
           matches_with_env |> List.iter (fun env ->
            Common.push
             { Match_result. rule; file; code = Ss x; env = env }
             matches;
            )
      );
      k x
    );
  }
  in
  visitor (Pr ast);
  !matches |> List.rev

let check a b c =
  Common.profile_code "Sgrep_lint.check" (fun () -> check2 a b c)
