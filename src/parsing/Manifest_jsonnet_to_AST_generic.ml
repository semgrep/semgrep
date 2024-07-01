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
module G = AST_generic
module A = AST_jsonnet
module E = Eval_jsonnet_envir
module V = Value_jsonnet

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Similar to Manifest_jsonnet.ml but converting to AST_generic instead
 * so we can convert a jsonnet rule in Parse_rule.ml
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let error tk s = raise (Parsing_error.Other_error (s, tk))
let fb = Tok.unsafe_fake_bracket

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* Mostly a copy of Eval_jsonnet_envir.manifest_value, but
 * converting to AST_generic instead of JSON
 *)
let rec value_to_expr (v : V.t) : G.expr =
  match v with
  | V.Primitive x ->
      let literal =
        match x with
        | Null tk -> G.Null tk
        | Bool (b, tk) -> G.Bool (b, tk)
        | Double (f, tk) -> G.Float (Some f, tk)
        | Str (s, tk) -> G.String (fb (s, tk))
      in
      G.L literal |> G.e
  | Lambda ({ f_tok = tk; _ }, _locals) -> error tk "Lambda value"
  | Array (l, arr, r) ->
      let xs =
        arr |> Array.to_list
        |> List_.map (fun (entry : V.lazy_value) ->
               value_to_expr
                 (match entry.lv with
                 | Closure (env, e) ->
                     let finalv = E.eval_program_with_env env e in
                     entry.lv <- Val finalv;
                     finalv
                 (* impossible too? *)
                 | Val v -> v
                 | Unevaluated _ -> raise Impossible))
      in
      G.Container (G.Array, (l, xs, r)) |> G.e
  | Object (l, (_assertsTODO, fields), r) ->
      (* TODO: evaluate asserts *)
      let xs =
        fields
        |> List_.filter_map (fun { V.fld_name; fld_hidden; fld_value } ->
               match fst fld_hidden with
               | A.Hidden -> None
               | A.Visible
               | A.ForcedVisible ->
                   let v =
                     match fld_value.lv with
                     | Closure (env, e) ->
                         let finalv = E.eval_program_with_env env e in
                         fld_value.lv <- Val finalv;
                         finalv
                     | Val v -> v
                     (* impossible? *)
                     | Unevaluated _ -> raise Impossible
                   in
                   let e = value_to_expr v in
                   let k = G.L (G.String (fb fld_name)) |> G.e in
                   Some (G.keyval k (snd fld_name) e))
      in
      G.Container (G.Dict, (l, xs, r)) |> G.e

let manifest_value (v : V.t) : G.program =
  let e = value_to_expr v in
  [ G.exprstmt e ]
