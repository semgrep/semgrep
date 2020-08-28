(* Yoann Padioleau
 *
 * Copyright (C) 2012 Facebook
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

open Cst_php
module Ast = Cst_php
module V = Visitor_php
module E = Error_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Lint checks:
 *  - Assignment in boolean context
 *)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let check_boolean_expr expr =
  (Expr expr) |> V.mk_visitor { V.default_visitor with
    V.kexpr = (fun (k, visitor) e ->
      (match e with
      (* putting extra parenthesis around an assignement silence the
       * lint error. This is consistent with what gcc does.
       *
       * todo? but people sometimes put extra parenthesis just to not
       * rely on priority of operators.
       *)
      | (ParenExpr (_, ((Assign (lval, _, subexpr))), _)) ->
          (* recurse only on subparts to avoid triggering rule below *)
          visitor (Expr lval);
          visitor (Expr subexpr);
          ()
      | (Assign (_lval, tok, _expr)) ->
          E.warning tok E.AssignInBooleanContext;
          k e
      | _ -> k e
      );
    );

    (* we don't want to flag code doing 'if(foo($arg1 = true))' because
     * many people currently use this idiom to emulate keyword parameters
     *)
    V.kargument = (fun (k, visitor) arg ->
      match arg with
      | Arg (Assign (lval, _tok, subexpr)) ->
          visitor (Expr lval);
          visitor (Expr subexpr);
      | _ -> k arg
    );
  }

let check ast =
  let visitor = V.mk_visitor { V.default_visitor with
    V.kstmt = (fun (k, _) st ->
      (match st with
      | Do (_, _, _, (_, expr, _), _)
      | If (_, (_, expr, _), _, _, _)
      | Switch (_, (_, expr, _), _)
      | While (_, (_, expr, _), _)
        ->
          check_boolean_expr expr
      | For (_, _, _for_expr1, _, for_expr2, _, _for_expr3, _, _stmt) ->
          for_expr2 |> Ast.uncomma |> List.iter (check_boolean_expr)
      | _ -> ()
      );
      (* recurse, call continuation *)
      k st
    );
    V.kexpr = (fun (k,_) e ->
      (match e with
      (* could do the case sensitivity check on all keywords, but
       * this one in particular seems to happen a lot
       *)
      | InstanceOf (e, tok, _classname) ->
          let str = Parse_info.str_of_info tok in
          let lower = String.lowercase_ascii str in
          if not (str =$= lower)
          then E.warning tok E.CaseSensitivityKeyword;
          k e
      | _ -> ()
      );
      (* recurse, call continuation *)
      k e
    );
  }
  in
  visitor (Program ast)
