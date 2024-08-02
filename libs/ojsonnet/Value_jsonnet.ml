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
module Core = Core_jsonnet

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Jsonnet "values".
 *
 * See https://jsonnet.org/ref/spec.html#jsonnet_values
 *)

(*****************************************************************************)
(* Values *)
(*****************************************************************************)
type t =
  | Primitive of primitive
  | Array of lazy_value array Core.bracket
  | Object of object_ Core.bracket
  (* TODO: rename to Closure *)
  | Lambda of Core.function_definition * locals

(* Similar to AST_jsonnet.literal but with evaluated Double instead of
 * Number and a simplified string!
 *)
and primitive =
  | Null of Core.tok
  | Bool of bool Core.wrap
  (* float should be good enough; that's what we use in JSON.t *)
  | Double of float Core.wrap
  (* TODO? string good enough for unicode? codepoints? *)
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

(* Jsonnet is a lazy language, so lazy values have a special importance.
 * A lazy value was represented originally simply as "t Lazy.t". However, this
 * does not work with the object merge '+' operator, because we need to
 * access the environment in which fields of the object are evaluated in. We
 * can't just build a closure (a Lazy.t), that implicitly has an environment;
 * we need to make explicit the environment (see Closure below).
 * In fact, this is also needed to implement the "late-bound" 'self'.
 * The environment is also necessary to keep around and for values, since
 * there could be nested objects/arrays which also have lazy semantics
 * themselves, and thus again need to be able to modify a specific environment.
 * TODO: putting opaque below here because get stack overflow otherwise
 * when printing values.
 *)
and lazy_value = { mutable lv : lazy_value_kind [@opaque] }

and lazy_value_kind =
  (* when we know the value, which is useful to bind Self/Super *)
  | Val of t
  (* for the environment-style evaluator
   * TODO: rename to LazyVal and use locals instead of env
   *)
  | Closure of env * Core.expr
  (* for the lambda calculus substitution model *)
  | Unevaluated of Core.expr

(*****************************************************************************)
(* Env *)
(*****************************************************************************)
and env = {
  (* There are currently two implementations of evaluation, one in
   * Eval_jsonnet_envir, the other in Eval_jsonnet_subst. The former
   * uses an environment, while the later uses lambda calculus
   * substitutions. Currently, the substitution version passes more
   * tests but is inneficient. We currently keep both implementations to
   * possibly mix the two, to get the efficiency benifit of environments
   * while keeping the simpler super/self implementation given
   * by substitution model.
   *)
  locals : locals;
  (* for call tracing *)
  depth : int;
  in_debug_call : bool;
  (* methods to help factorize code between the Eval_jsonnet_xxx.ml *)
  eval_expr : env -> Core_jsonnet.expr -> t;
  eval_expr_for_call : env -> Core_jsonnet.expr -> t;
  eval_std_filter_element :
    env -> Tok.t -> Core_jsonnet.function_definition -> lazy_value -> t * env;
  eval_plus_object :
    env ->
    Tok.t ->
    object_ Core.bracket ->
    object_ Core.bracket ->
    object_ Core.bracket;
  to_lazy_value : env -> Core_jsonnet.expr -> lazy_value;
  to_value : env -> lazy_value -> t;
  tostring : t -> string;
}

(* TODO? split? move self/super in separate field outside locals? *)
and local_id = LSelf | LSuper | LId of string

and locals = (local_id, lazy_value) Map_.t
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
    in_debug_call = false;
    (* fake implem; Each Eval_jsonnet_xxx.ml need to define those methods *)
    eval_expr = (fun _ _ -> failwith "TODO: eval_expr not implemented");
    eval_std_filter_element =
      (fun _ _ -> failwith "TODO: eval_std_filter_element not implemented");
    eval_plus_object =
      (fun _ _ _ _ -> failwith "TODO: eval_plus_object not implemented");
    eval_expr_for_call =
      (fun _ _ -> failwith "TODO: eval_expr_for_call not implemented");
    to_lazy_value = (fun _ _ -> failwith "TODO: to_lazy_value not implemented");
    to_value = (fun _ _ -> failwith "TODO: to_value not implemented");
    tostring = (fun _ -> failwith "TODO: tostring not implemented");
  }
