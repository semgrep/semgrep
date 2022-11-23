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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Jsonnet "values".
 *
 * See https://jsonnet.org/ref/spec.html#jsonnet_values
 *)

type value_ =
  | Primitive of primitive
  | Object of object_
  | Function of Core_jsonnet.function_definition
  | Array of Core_jsonnet.expr list

(* mostly like AST_jsonnet.literal but with evaluated Double instead of Number *)
and primitive =
  | Null of AST_jsonnet.tok
  | Bool of bool AST_jsonnet.wrap
  | Double of float AST_jsonnet.wrap
  | Str of AST_jsonnet.string_

and object_ = Core_jsonnet.obj_assert list * field list

and field = {
  fld_name : AST_jsonnet.string_;
  fld_hidden : AST_jsonnet.hidden AST_jsonnet.wrap;
  fld_value : Core_jsonnet.expr;
}
