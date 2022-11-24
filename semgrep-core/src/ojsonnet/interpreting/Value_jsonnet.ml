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
module A = AST_jsonnet
module C = Core_jsonnet

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Jsonnet "values".
 *
 * See https://jsonnet.org/ref/spec.html#jsonnet_values
 *
 * TODO? store the value in Array and Object once they have been computed
 * once? To not redo the work everytime?
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type value_ =
  | Primitive of primitive
  | Object of object_ A.bracket
  | Function of C.function_definition
  (* Note that the array element are not values! They are evaluated lazily *)
  | Array of C.expr array A.bracket

(* mostly like AST_jsonnet.literal but with evaluated Double instead of
 * Number and a simplified string!
 * TODO: float enough for Double? string good enough for unicode? codepoints?
 *)
and primitive =
  | Null of A.tok
  | Bool of bool A.wrap
  | Double of float A.wrap
  | Str of string A.wrap

and object_ = C.obj_assert list * field list

and field = {
  (* like Str *)
  fld_name : string A.wrap;
  fld_hidden : A.hidden A.wrap;
  (* Note that the field value are actually not values! They are evaluated
   * lazily. Only the fld_name is "resolved" to a string. *)
  fld_value : C.expr;
}
[@@deriving show]
