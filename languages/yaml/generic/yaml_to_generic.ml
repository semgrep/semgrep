(* Yoann Padioleau
 *
 * Copyright (C) 2025 Semgrep Inc.
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
open AST_yaml
module A = AST_yaml
module G = AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* AST_yaml to AST_generic
 *
 * Normally the `<lang>_to_generic` files only convert from the language specific
 * ast (AST_yaml) to the generic ast. The entry points of this file actually take
 * the file or str and produce the generic ast representation. This is due to
 * backwards compatibility with the original version of this file.
 *
 * TODO the goal is to match our standard convention
 *
 * history: most of the old code of yaml_to_generic.ml is now in
 * Parse_yaml.ml
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let fb = Tok.unsafe_fake_bracket

(*****************************************************************************)
(* Converter *)
(*****************************************************************************)
let rec value (v : A.value) : G.expr =
  match v with
  | Null tok -> G.L (G.Null tok) |> G.e
  | Bool (b, tok) -> G.L (G.Bool (b, tok)) |> G.e
  | S (s, tok) -> G.L (G.String (fb (s, tok))) |> G.e
  | Float (fopt, tok) -> G.L (G.Float (fopt, tok)) |> G.e
  | Sequence (lbra, xs, rbra) ->
      let xs' = List_.map value xs in
      G.Container (G.Array, (lbra, xs', rbra)) |> G.e
  | Mapping (lbra, kvs, rbra) ->
      let kvs' = List_.map value kvs in
      G.Container (G.Dict, (lbra, kvs', rbra)) |> G.e
  | KV (lbra, (k, v), rbra) ->
      let k' = value k in
      let v' = value v in
      G.Container (G.Tuple, (lbra, [ k'; v' ], rbra)) |> G.e
  | Alias (name, v) ->
      let e = value v in
      G.Alias (name, e) |> G.e
  | Tag (tag, v) ->
      let e = value v in
      let type_ = G.ty_builtin tag in
      G.Cast (type_, snd tag, e) |> G.e
  | OtherMapping (t1, t2) ->
      G.OtherExpr (("Mapping", t1), [ G.Str (fb ("", t2)) ]) |> G.e
  | Metavar (s, tok) -> G.N (G.Id ((s, tok), G.empty_id_info ())) |> G.e
  | Ellipsis tok -> G.Ellipsis tok |> G.e

let doc_to_generic (doc : A.document) : G.program =
  doc |> List_.map value |> List_.map G.exprstmt

let any_to_generic (any : A.any) : G.any =
  match any with
  | Doc doc -> G.Pr (doc_to_generic doc)
  | V v -> G.E (value v)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse_yaml_file ~is_target (file : Fpath.t) str =
  let doc = Parse_yaml.parse_yaml_file ~is_target file str in
  doc_to_generic doc

let any str =
  let x = Parse_yaml.any str in
  any_to_generic x

let program (file : Fpath.t) =
  let doc = Parse_yaml.parse file in
  doc_to_generic doc
