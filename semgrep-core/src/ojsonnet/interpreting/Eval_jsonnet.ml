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
open Core_jsonnet

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Core_jsonnet to Value_jsonnet Jsonnet evaluator.
 *
 * See https://jsonnet.org/ref/spec.html#semantics
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type _env = unit

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let todo _env _v = failwith "TODO"
let eval_string _env v = v
let eval_list f env x = x |> Common.map (fun x -> f env x)

(*****************************************************************************)
(* Evaluator *)
(*****************************************************************************)

let eval_tok _env v = v

let eval_wrap ofa env (v1, v2) =
  let v1 = ofa env v1 in
  (v1, v2)

let eval_bracket ofa env (v1, v2, v3) =
  let v2 = ofa env v2 in
  (v1, v2, v3)

let eval_ident _env v = v

let rec eval_expr env v =
  match v with
  | L v -> todo env v
  | O v ->
      let v = (eval_bracket eval_obj_inside) env v in
      todo env v
  | Array v ->
      let v = (eval_bracket (eval_list eval_expr)) env v in
      todo env v
  | Id v ->
      let v = (eval_wrap eval_string) env v in
      todo env v
  | IdSpecial v ->
      let v = (eval_wrap eval_special) env v in
      todo env v
  | Local (v1, v2, v3, v4) ->
      let v1 = eval_tok env v1 in
      let v2 = (eval_list eval_bind) env v2 in
      let v3 = eval_tok env v3 in
      let v4 = eval_expr env v4 in
      todo env (v1, v2, v3, v4)
  | ArrayAccess (v1, v2) ->
      let v1 = eval_expr env v1 in
      let v2 = (eval_bracket eval_expr) env v2 in
      todo env (v1, v2)
  | Call (v1, v2) ->
      let v1 = eval_expr env v1 in
      let v2 = (eval_bracket (eval_list eval_argument)) env v2 in
      todo env (v1, v2)
  | UnaryOp (v1, v2) ->
      let v1 = todo env v1 in
      let v2 = eval_expr env v2 in
      todo env (v1, v2)
  | BinaryOp (v1, v2, v3) ->
      let v1 = eval_expr env v1 in
      let v2 = (eval_wrap eval_binary_op) env v2 in
      let v3 = eval_expr env v3 in
      todo env (v1, v2, v3)
  | If (v1, v2, v3, v4) ->
      let v1 = eval_tok env v1 in
      let v2 = eval_expr env v2 in
      let v3 = eval_expr env v3 in
      let v4 = eval_expr env v4 in
      todo env (v1, v2, v3, v4)
  | Lambda v ->
      let v = eval_function_definition env v in
      todo env v
  | Error (v1, v2) ->
      let v1 = eval_tok env v1 in
      let v2 = eval_expr env v2 in
      todo env (v1, v2)

and eval_special env v =
  match v with
  | Self -> todo env
  | Super -> todo env

and eval_argument env v =
  match v with
  | Arg v ->
      let v = eval_expr env v in
      todo env v
  | NamedArg (v1, v2, v3) ->
      let v1 = eval_ident env v1 in
      let v2 = eval_tok env v2 in
      let v3 = eval_expr env v3 in
      todo env (v1, v2, v3)

and eval_binary_op env v =
  match v with
  | Plus -> todo env
  | Minus -> todo env
  | Mult -> todo env
  | Div -> todo env
  | LSL -> todo env
  | LSR -> todo env
  | Lt -> todo env
  | LtE -> todo env
  | Gt -> todo env
  | GtE -> todo env
  | And -> todo env
  | Or -> todo env
  | BitAnd -> todo env
  | BitOr -> todo env
  | BitXor -> todo env

and eval_bind env v =
  match v with
  | B (v1, v2, v3) ->
      let v1 = eval_ident env v1 in
      let v2 = eval_tok env v2 in
      let v3 = eval_expr env v3 in
      todo env (v1, v2, v3)

and eval_function_definition _env v = v

(*
and eval_parameter env v =
  match v with
  | P (v1, v2, v3) ->
      let v1 = eval_ident env v1 in
      let v2 = eval_tok env v2 in
      let v3 = eval_expr env v3 in
      todo env (v1, v2, v3)
*)

and eval_obj_inside env v =
  match v with
  | Object (v1, v2) ->
      let v1 = (eval_list eval_obj_assert) env v1 in
      let v2 = (eval_list eval_field) env v2 in
      todo env (v1, v2)
  | ObjectComp v ->
      let v = eval_obj_comprehension env v in
      todo env v

and eval_obj_assert env v =
  (fun env (v1, v2) ->
    let v1 = eval_tok env v1 in
    let v2 = eval_expr env v2 in
    todo env (v1, v2))
    env v

and eval_field env _v = todo env "TODO: field"

and eval_field_name env v =
  match v with
  | FExpr v ->
      let v = (eval_bracket eval_expr) env v in
      todo env v

and eval_obj_comprehension env v =
  (fun env (v1, v2, v3, v4) ->
    let v1 = eval_field_name env v1 in
    let v2 = eval_tok env v2 in
    let v3 = eval_expr env v3 in
    let v4 = eval_for_comp env v4 in
    todo env (v1, v2, v3, v4))
    env v

and eval_for_comp env v =
  (fun env (v1, v2, v3, v4) ->
    let v1 = eval_tok env v1 in
    let v2 = eval_ident env v2 in
    let v3 = eval_tok env v3 in
    let v4 = eval_expr env v4 in
    todo env (v1, v2, v3, v4))
    env v

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let eval (e : Core_jsonnet.program) : Value_jsonnet.value_ =
  let env = () in
  eval_expr env e
