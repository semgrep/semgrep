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
module V = Value_jsonnet
module G = AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Similar to Manifest_jsonnet.ml but converting to AST_generic instead
 * so we can convert a jsonnet rule in Parse_rule.ml
 *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let manifest_value (_v : V.value_) : G.program = failwith "TODO: manifest_value"
