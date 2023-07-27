(* Yoann Padioleau
 * Sophia Roshal
 *
 * Copyright (C) 2022 Semgrep Inc.
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

module A = AST_jsonnet

(*****************************************************************************)
(* Env *)
(*****************************************************************************)

type local_id = LSelf | LSuper | LId of string

(* This used to be wrapped in an explicit "lazy" rather than keeping around an
   environment however, this does not work with the object merge + operator,
   since we need to be able to access the environment in which fields of the
   object are evaluated in. It is also neccesary to keep around and
   environment even for values, since there could be nested objects/arrays
   which also have lazy semantics themselves, and thus again need to be able
   to modify a specifc environment
*)
and val_or_unevaluated_ =
  | Val of value_
  | Unevaluated of Core_jsonnet_subst.expr

and lazy_value = val_or_unevaluated_

(*****************************************************************************)
(* Values *)
(*****************************************************************************)
and value_ =
  | Primitive of primitive
  | Object of object_ A.bracket
  | Lambda of Core_jsonnet_subst.function_definition
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

and object_ = asserts list * value_field list

(* opti? make it a hashtbl of string -> field for faster lookup? *)
and value_field = {
  (* like Str, strictly evaluated! *)
  fld_name : string A.wrap;
  fld_hidden : A.hidden A.wrap;
  fld_value : lazy_value;
}

and asserts = Core_jsonnet_subst.obj_assert [@@deriving show]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let empty_obj : value_ =
  let fk = Tok.unsafe_fake_tok "" in
  Object (fk, ([], []), fk)

(*****************************************************************************)
(* Program *)
(*****************************************************************************)
