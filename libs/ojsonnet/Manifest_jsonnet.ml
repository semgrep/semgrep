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
module J = JSON
module V = Value_jsonnet
module A = AST_jsonnet

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* See https://jsonnet.org/ref/spec.html#manifestation
 *
 * old: was passing an env but it can't work because all the Local
 * are implicitely defined via closures in env.locals lazy values.
 *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

exception Error of string * Tok.t

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let error tk s =
  (* TODO? if Parse_info.is_fake tk ... *)
  raise (Error (s, tk))

let sv e = V.show_value_ e

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let rec manifest_value (v : Value_jsonnet.value_) : JSON.t =
  match v with
  | V.Primitive x -> (
      match x with
      | V.Null _t -> J.Null
      | V.Bool (b, _tk) -> J.Bool b
      | V.Double (f, _tk) -> J.Float f
      | V.Str (s, _tk) -> J.String s)
  | V.Function { f_tok = tk; _ } -> error tk (spf "Function value: %s" (sv v))
  | V.Array (_, arr, _) ->
      J.Array
        (arr |> Array.to_list
        |> Common.map (fun lzv ->
               let v = Lazy.force lzv.V.v in
               manifest_value v))
  | V.Object (_l, (_assertsTODO, fields), _r) as _o ->
      (* TODO: evaluate asserts *)
      let xs =
        fields
        |> Common.map_filter (fun { V.fld_name; fld_hidden; fld_value } ->
               match fst fld_hidden with
               | A.Hidden -> None
               | A.Visible
               | A.ForcedVisible ->
                   let v = Lazy.force fld_value.v in
                   let j = manifest_value v in
                   Some (fst fld_name, j))
      in
      J.Object xs
