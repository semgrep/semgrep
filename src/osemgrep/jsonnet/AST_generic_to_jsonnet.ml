(* Yoann Padioleau
 *
 * Copyright (C) 2022 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Common
module A = AST_jsonnet
module G = AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Convert a generic AST to an AST_jsonnet program. This is used
 * in Rule_fetching import_callback to convert a YAML program
 * in a Jsonnet program so Jsonnet policy can manipulate
 * legacy YAML rules.
 *
 * This is the similar to the reverse of Manifest_jsonnet_to_AST_generic
 * (used by Parse_rule.ml), but here we produce an AST_jsonnet instead
 * of a Value_jsonnet.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let error tk s = raise (Parsing_error.Other_error (s, tk))
let fb = Tok.unsafe_fake_bracket

(*****************************************************************************)
(* Expr to expr *)
(*****************************************************************************)

let rec expr_to_expr (e : G.expr) : A.expr =
  let error () =
    let tk = AST_generic_helpers.first_info_of_any (G.E e) in
    error tk
      (spf "generic construct without a jsonnet equivalent: %s" (G.show_expr e))
  in
  match e.G.e with
  | G.L literal -> (
      match literal with
      | G.Null tk -> A.L (A.Null tk)
      | G.Bool (b, tk) -> A.L (A.Bool (b, tk))
      | G.Int (Some i64, tk) -> A.L (A.Number (Int64.to_string i64, tk))
      | G.Float (Some f, tk) -> A.L (A.Number (string_of_float f, tk))
      | G.String x -> A.L (A.Str (A.mk_string_ x))
      | _else_ -> error ())
  (* in some YAML constructs like
   *   - metavariable-regex:
   *       metavariable: $EXN
   * the $EXN is actually passed as an Id, not a String
   * TODO? double check is_metavariable str?
   *)
  | G.N (G.Id ((str, tk), _idinfo)) -> A.L (A.Str (A.mk_string_ (fb (str, tk))))
  | G.Container (kind, (l, xs, r)) -> (
      match kind with
      | G.Array ->
          let arr_inside = xs |> List_.map (fun e -> expr_to_expr e) in
          A.A (l, A.Array arr_inside, r)
      | G.Dict ->
          let members =
            xs
            |> List_.map (fun e ->
                   match e.G.e with
                   | G.Container (G.Tuple, (l, [ k; v ], _)) ->
                       let fld_name =
                         match k.G.e with
                         | G.L (G.String x) -> A.FStr (A.mk_string_ x)
                         | _else_ -> error ()
                       in
                       A.OField
                         {
                           fld_name;
                           fld_attr = None;
                           fld_hidden = (A.Visible, l);
                           fld_value = expr_to_expr v;
                         }
                   | _else_ -> error ())
          in
          A.O (l, Object members, r)
      | _else_ -> error ())
  | _else_ -> error ()

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let program (xs : G.program) : A.program =
  match xs with
  | [ { G.s = G.ExprStmt (e, _sc); _ } ] -> expr_to_expr e
  | _else_ -> failwith "not a single toplevel expression"
