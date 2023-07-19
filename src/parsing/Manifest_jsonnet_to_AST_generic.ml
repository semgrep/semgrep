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

let rec value_to_expr (v : V.value_) : G.expr =
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
  | Lambda { f_tok = tk; _ } -> error tk "Lambda value"
  | Array (l, arr, r) ->
      let xs =
        arr |> Array.to_list
        |> Common.map (fun (entry : V.lazy_value) ->
               value_to_expr
                 (match entry.value with
                 | Val v -> v
                 | Unevaluated e -> E.eval_program_with_env entry.env e))
      in
      G.Container (G.Array, (l, xs, r)) |> G.e
  | Object (l, (_assertsTODO, fields), r) ->
      (* TODO: evaluate asserts *)
      let xs =
        fields
        |> Common.map_filter (fun { V.fld_name; fld_hidden; fld_value } ->
               match fst fld_hidden with
               | A.Hidden -> None
               | A.Visible
               | A.ForcedVisible ->
                   (*let v = Lazy.force fld_value.v *)
                   let v =
                     match fld_value.value with
                     | Val v -> v
                     | Unevaluated e -> E.eval_program_with_env fld_value.env e
                   in
                   let e = value_to_expr v in
                   let k = G.L (G.String (fb fld_name)) |> G.e in
                   Some (G.keyval k (snd fld_name) e))
      in
      G.Container (G.Dict, (l, xs, r)) |> G.e

let manifest_value (v : V.value_) : G.program =
  let e = value_to_expr v in
  [ G.exprstmt e ]
