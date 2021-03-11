
(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
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
open Ast_json

module G = AST_generic


let program ast =
  let e = Js_to_generic.expr ast in
  [G.exprstmt e]


let any x =
  match x with
  | E e ->
      G.E (Js_to_generic.expr e)
  | PartialSingleField (v1, v2, v3) ->
      G.Partial (G.PartialSingleField (v1, v2, Js_to_generic.expr v3))
