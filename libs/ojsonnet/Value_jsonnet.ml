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
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type value_ =
  | Primitive of primitive
  | Object of object_ A.bracket
  | Function of C.function_definition
  | Array of lazy_value array A.bracket

(* mostly like AST_jsonnet.literal but with evaluated Double instead of
 * Number and a simplified string!
 * Is float good enough? That's what we use in JSON.t so should be good.
 * TODO? string good enough for unicode? codepoints?
 *)
and primitive =
  | Null of A.tok
  | Bool of bool A.wrap
  | Double of float A.wrap
  | Str of string A.wrap

and object_ = C.obj_assert list * field list

(* opti? make it a hashtbl of string -> field for faster lookup? *)
and field = {
  (* like Str, strictly evaluated! *)
  fld_name : string A.wrap;
  fld_hidden : A.hidden A.wrap;
  fld_value : lazy_value;
}

(* old: was just C.expr but this can't work because manifest
 * can't be passed the correct environment to evaluate the array
 * elts or fields
 *)
and lazy_value = {
  (* lazy closures built from a call to Eval_jsonnet.eval_expr *)
  v : value_ Lazy.t;
  (* just for debugging as we can't inspect closures *)
  e : C.expr;
}
[@@deriving show]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let empty_obj : value_ =
  let fk = Tok.unsafe_fake_tok "" in
  Object (fk, ([], []), fk)
