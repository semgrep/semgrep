(* Brandon Wu
 *
 * Copyright (C) 2019-2024 r2c
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
open AST_ql
module G = AST_generic
module H = AST_generic_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* AST_ql to AST_generic.
 *
 * See AST_generic.ml for more information.
 *
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let id x = x
let option = Option.map
let list = List_.map
let string = id
let bool = id
let fake tok s = Tok.fake_tok tok s
let unsafe_fake s = Tok.unsafe_fake_tok s
let fb = Tok.unsafe_fake_bracket

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let info x = x

let wrap _of_a (v1, v2) =
  let v1 = _of_a v1 and v2 = info v2 in
  (v1, v2)

let bracket of_a (t1, x, t2) = (info t1, of_a x, info t2)
