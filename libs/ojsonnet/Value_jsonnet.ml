(* Yoann Padioleau
 * Sophia Roshal
 *
 * Copyright (C) 2022-2023 Semgrep Inc.
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

module Core = Core_jsonnet

(*****************************************************************************)
(* Values *)
(*****************************************************************************)
type t =
  | Primitive of primitive
  | Object of object_ Core.bracket
  | Lambda of Core.function_definition
  | Array of lazy_value array Core.bracket

(* mostly like AST_jsonnet.literal but with evaluated Double instead of
 * Number and a simplified string!
 * Is float good enough? That's what we use in JSON.t so should be good.
 * TODO? string good enough for unicode? codepoints?
 *)
and primitive =
  | Null of Core.tok
  | Bool of bool Core.wrap
  | Double of float Core.wrap
  | Str of string Core.wrap

and object_ = asserts list * value_field list

(* opti? make it a hashtbl of string -> field for faster lookup? *)
and value_field = {
  (* like Str above, the field name is strictly evaluated! *)
  fld_name : string Core.wrap;
  fld_hidden : Core.hidden Core.wrap;
  fld_value : lazy_value;
}

and asserts = Core.obj_assert * env

(*****************************************************************************)
(* Lazy values *)
(*****************************************************************************)

(* A lazy value was represented before simply as a closure.
 * This was also represented as an explicit "lazy" rather than keeping around
 * an environment. However, this does not work with the object
 * merge + operator, since we need to be able to access the environment in
 * which fields of the object are evaluated in. We can't just build
 * a closure, that implicitely has an environment. We need to make
 * explicit the closure.
 * The environment is also neccesary to keep around and for values, since
 * there could be nested objects/arrays which also have lazy semantics
 * themselves, and thus again need to be able to modify a specifc environment
 *)
and lazy_value =
  (* for strict *)
  | Val of t
  (* for strict actually but just to handle Self, or could be for envir too *)
  | Lv of t Lazy.t
  (* for subst *)
  | Unevaluated of Core.expr
  (* for envir *)
  | Closure of env * Core.expr

(*****************************************************************************)
(* Env *)
(*****************************************************************************)
and env = {
  (* There are currently two implementations of evaluation, one in
   * Eval_jsonnet, the other in Eval_jsonnet_subst. The former
   * uses an environment, while the later uses lambda calculus
   * substitutions. Currently, the substitution version passes more
   * tests but is inneficient. We currently keep both implementations to
   * possibly mix the two, to get the efficiency benifit of environments
   * while keeping the simpler super/self implementation given
   * by substitution model. In the substitution model, we always
   * just pass an empty environment into the value right now.
   * it is probably simpler and more efficient to use a classic
   * environment where the locals are defined. Jsonnet uses lazy
   * evaluation so we model this by allowing unevaluated expressions in
   * environment below.
   *)
  locals : (local_id, lazy_value) Map_.t;
  (* for call tracing *)
  depth : int;
  (* methods to help factorize code between the Eval_jsonnet_xxx.ml *)
  eval_expr : env -> Core_jsonnet.expr -> t;
  eval_expr_for_call : env -> Core_jsonnet.expr -> t;
  eval_std_filter_element :
    env -> Tok.t -> Core_jsonnet.function_definition -> lazy_value -> t * env;
  to_lazy_value : env -> Core_jsonnet.expr -> lazy_value;
}

and local_id = LSelf | LSuper | LId of string
[@@deriving show { with_path = false }]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let empty_obj : t =
  let fk = Tok.unsafe_fake_tok "" in
  Object (fk, ([], []), fk)

let empty_env =
  {
    locals = Map_.empty;
    depth = 0;
    eval_expr = (fun _ _ -> failwith "TODO: eval_expr not implemented");
    eval_std_filter_element =
      (fun _ _ -> failwith "TODO: eval_std_filter_element not implemented");
    eval_expr_for_call =
      (fun _ _ -> failwith "TODO: eval_expr_for_call not implemented");
    to_lazy_value = (fun _ _ -> failwith "TODO: to_lazy_value not implemented");
  }
