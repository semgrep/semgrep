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

module G = AST_generic
module A = AST_jsonnet
module E = Eval_jsonnet
open Core_jsonnet

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

let rec value_to_expr (v : value_) : G.expr =
  match v with
  | Primitive x ->
      let literal =
        match x with
        | Null tk -> G.Null tk
        | Bool (b, tk) -> G.Bool (b, tk)
        | Double (f, tk) -> G.Float (Some f, tk)
        | Str (s, tk) -> G.String (fb (s, tk))
      in
      G.L literal |> G.e
  | Function { f_tok = tk; _ } -> error tk "Function value"
  | Array (l, arr, r) ->
      let xs =
        arr |> Array.to_list
        |> Common.map (fun entry ->
               (*let v = Lazy.force lzv.v*)
               let v = E.eval_program (fst entry) (snd entry) in
               value_to_expr v)
      in
      G.Container (G.Array, (l, xs, r)) |> G.e
  | ObjectVal (l, (_assertsTODO, fields), r) ->
      (* TODO: evaluate asserts *)
      let xs =
        fields
        |> Common.map_filter (fun { vfld_name; vfld_hidden; vfld_value } ->
               match fst vfld_hidden with
               | A.Hidden -> None
               | A.Visible
               | A.ForcedVisible ->
                   (*let v = Lazy.force fld_value.v *)
                   let v = E.eval_program (fst vfld_value) (snd vfld_value) in
                   let e = value_to_expr v in
                   let k = G.L (G.String (fb vfld_name)) |> G.e in
                   Some (G.keyval k (snd vfld_name) e))
      in
      G.Container (G.Dict, (l, xs, r)) |> G.e

let manifest_value (v : value_) : G.program =
  let e = value_to_expr v in
  [ G.exprstmt e ]
