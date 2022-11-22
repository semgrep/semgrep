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
open AST_jsonnet
module C = Core_jsonnet

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* AST_jsonnet to Core_jsonnet
 *
 * See https://jsonnet.org/ref/spec.html#desugaring
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type env = { within_an_object : bool }

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let todo _env _v = failwith "TODO"

(*****************************************************************************)
(* Builtins *)
(*****************************************************************************)

(* todo? auto generate the right name in otarzan? *)
let desugar_string _env x = x
let desugar_bool _env x = x
let desugar_list f env x = x |> Common.map (fun x -> f env x)
let desugar_option f env x = x |> Option.map (fun x -> f env x)

(*****************************************************************************)
(* Boilerplate *)
(*****************************************************************************)

(* this code started from otarzan generated boilerplate from AST_jsonnet.ml *)

let desugar_tok _env v = v

let desugar_wrap ofa env (v1, v2) =
  let v1 = ofa env v1 in
  let v2 = desugar_tok env v2 in
  todo env (v1, v2)

let desugar_bracket ofa env (v1, v2, v3) =
  let v1 = desugar_tok env v1 in
  let v2 = ofa env v2 in
  let v3 = desugar_tok env v3 in
  todo env (v1, v2, v3)

let desugar_ident env v : C.ident = (desugar_wrap desugar_string) env v

let rec desugar_expr env v : C.expr = desugar_expr_kind env v

and desugar_expr_kind env v =
  match v with
  | L v ->
      let v = desugar_literal env v in
      todo env v
  | O v ->
      let v = (desugar_bracket desugar_obj_inside) env v in
      todo env v
  | A v ->
      let v = (desugar_bracket desugar_arr_inside) env v in
      todo env v
  | Id v ->
      let v = (desugar_wrap desugar_string) env v in
      todo env v
  | IdSpecial v ->
      let v = (desugar_wrap desugar_special) env v in
      todo env v
  | Local (v1, v2, v3, v4) ->
      let v1 = desugar_tok env v1 in
      let v2 = (desugar_list desugar_bind) env v2 in
      let v3 = desugar_tok env v3 in
      let v4 = desugar_expr env v4 in
      todo env (v1, v2, v3, v4)
  | DotAccess (v1, v2, v3) ->
      let v1 = desugar_expr env v1 in
      let v2 = desugar_tok env v2 in
      let v3 = desugar_ident env v3 in
      todo env (v1, v2, v3)
  | ArrayAccess (v1, v2) ->
      let v1 = desugar_expr env v1 in
      let v2 = (desugar_bracket desugar_expr) env v2 in
      todo env (v1, v2)
  | Call (v1, v2) ->
      let v1 = desugar_expr env v1 in
      let v2 = (desugar_bracket (desugar_list desugar_argument)) env v2 in
      todo env (v1, v2)
  | UnaryOp (v1, v2) ->
      let v1 = (desugar_wrap desugar_unary_op) env v1 in
      let v2 = desugar_expr env v2 in
      todo env (v1, v2)
  | BinaryOp (v1, v2, v3) ->
      let v1 = desugar_expr env v1 in
      let v2 = (desugar_wrap desugar_binary_op) env v2 in
      let v3 = desugar_expr env v3 in
      todo env (v1, v2, v3)
  | If (v1, v2, v3, v4) ->
      let v1 = desugar_tok env v1 in
      let v2 = desugar_expr env v2 in
      let v3 = desugar_expr env v3 in
      (* let v4 = (desugar_option TodoTyTuple) env v4 in *)
      let v4 = todo env v4 in
      todo env (v1, v2, v3, v4)
  | Lambda v ->
      let v = desugar_function_definition env v in
      todo env v
  | I v ->
      let v = desugar_import env v in
      todo env v
  | Assert (v1, v2, v3) ->
      let v1 = desugar_assert_ env v1 in
      let v2 = desugar_tok env v2 in
      let v3 = desugar_expr env v3 in
      todo env (v1, v2, v3)
  | Error (v1, v2) ->
      let v1 = desugar_tok env v1 in
      let v2 = desugar_expr env v2 in
      todo env (v1, v2)
  | ParenExpr v ->
      let v = (desugar_bracket desugar_expr) env v in
      todo env v
  | TodoExpr (v1, v2) ->
      let v1 = (desugar_wrap desugar_string) env v1 in
      let v2 = (desugar_list desugar_expr) env v2 in
      todo env (v1, v2)

and desugar_literal env v =
  match v with
  | Null v ->
      let v = desugar_tok env v in
      todo env v
  | Bool v ->
      let v = (desugar_wrap desugar_bool) env v in
      todo env v
  | Number v ->
      let v = (desugar_wrap desugar_string) env v in
      todo env v
  | Str v ->
      let v = desugar_string_ env v in
      todo env v

and desugar_string_ env v =
  (fun env (v1, v2, v3) ->
    let v1 = (desugar_option desugar_verbatim) env v1 in
    let v2 = desugar_string_kind env v2 in
    let v3 = (desugar_bracket desugar_string_content) env v3 in
    todo env (v1, v2, v3))
    env v

and desugar_verbatim env v = desugar_tok env v

and desugar_string_kind env v =
  match v with
  | SingleQuote -> todo env
  | DoubleQuote -> todo env
  | TripleBar -> todo env

and desugar_string_content env v =
  (desugar_list (desugar_wrap desugar_string)) env v

and desugar_special env v =
  match v with
  | Self -> todo env
  | Super -> todo env
  | Dollar -> todo env

and desugar_argument env v =
  match v with
  | Arg v ->
      let v = desugar_expr env v in
      todo env v
  | NamedArg (v1, v2, v3) ->
      let v1 = desugar_ident env v1 in
      let v2 = desugar_tok env v2 in
      let v3 = desugar_expr env v3 in
      todo env (v1, v2, v3)

and desugar_unary_op env v =
  match v with
  | UPlus -> todo env
  | UMinus -> todo env
  | UBang -> todo env
  | UTilde -> todo env

and desugar_binary_op env v =
  match v with
  | Plus -> todo env
  | Minus -> todo env
  | Mult -> todo env
  | Div -> todo env
  | Mod -> todo env
  | LSL -> todo env
  | LSR -> todo env
  | Lt -> todo env
  | LtE -> todo env
  | Gt -> todo env
  | GtE -> todo env
  | Eq -> todo env
  | NotEq -> todo env
  | And -> todo env
  | Or -> todo env
  | BitAnd -> todo env
  | BitOr -> todo env
  | BitXor -> todo env

and desugar_assert_ env v =
  (fun env (v1, v2, v3) ->
    let v1 = todo env v1 in
    (* let v2 = (desugar_option TodoTyTuple) env v2 in *)
    let v2 = todo env v2 in
    let v3 = todo env v3 in
    todo env (v1, v2, v3))
    env v

and desugar_arr_inside env v =
  match v with
  | Array v ->
      let v = (desugar_list desugar_expr) env v in
      todo env v
  | ArrayComp v ->
      let v = (desugar_comprehension desugar_expr) env v in
      todo env v

and desugar_comprehension ofa env v =
  (fun env (v1, v2, v3) ->
    let v1 = ofa env v1 in
    let v2 = todo env v2 in
    let v3 = (desugar_list desugar_for_or_if_comp) env v3 in
    todo env (v1, v2, v3))
    env v

and desugar_for_or_if_comp env v =
  match v with
  | CompFor v ->
      let v = desugar_for_comp env v in
      todo env v
  | CompIf v ->
      let v = desugar_if_comp env v in
      todo env v

and desugar_for_comp env v =
  (fun env (v1, v2, v3, v4) ->
    let v1 = todo env v1 in
    let v2 = todo env v2 in
    let v3 = todo env v3 in
    let v4 = desugar_expr env v4 in
    todo env (v1, v2, v3, v4))
    env v

and desugar_if_comp env v =
  (fun env (v1, v2) ->
    let v1 = desugar_tok env v1 in
    let v2 = desugar_expr env v2 in
    todo env (v1, v2))
    env v

and desugar_bind env v =
  match v with
  | B (v1, v2, v3) ->
      let v1 = desugar_ident env v1 in
      let v2 = desugar_tok env v2 in
      let v3 = desugar_expr env v3 in
      todo env (v1, v2, v3)

and desugar_function_definition env _v =
  ignore desugar_parameter;
  todo env "RECORD"

and desugar_parameter env v =
  match v with
  | P (v1, v2) ->
      let v1 = desugar_ident env v1 in
      (* let v2 = (desugar_option TodoTyTuple) env v2 in *)
      let v2 = todo env v2 in
      todo env (v1, v2)

and desugar_obj_inside env v =
  match v with
  | Object v ->
      let v = (desugar_list desugar_obj_member) env v in
      todo env v
  | ObjectComp v ->
      let v = desugar_obj_comprehension env v in
      todo env v

and desugar_obj_member env v =
  match v with
  | OLocal v ->
      let v = desugar_obj_local env v in
      todo env v
  | OField v ->
      let v = desugar_field env v in
      todo env v
  | OAssert v ->
      let v = desugar_assert_ env v in
      todo env v

and desugar_field env _v =
  ignore (desugar_field_name, desugar_attribute, desugar_hidden);
  todo env "RECORD"

and desugar_field_name env v =
  match v with
  | FId v ->
      let v = desugar_ident env v in
      todo env v
  | FStr v ->
      let v = desugar_string_ env v in
      todo env v
  | FDynamic v ->
      let v = (desugar_bracket desugar_expr) env v in
      todo env v

and desugar_hidden env v =
  match v with
  | Colon -> todo env
  | TwoColons -> todo env
  | ThreeColons -> todo env

and desugar_attribute env v =
  match v with
  | PlusField v ->
      let v = desugar_tok env v in
      todo env v

and desugar_obj_local env v =
  (fun env (v1, v2) ->
    let v1 = desugar_tok env v1 in
    let v2 = desugar_bind env v2 in
    todo env (v1, v2))
    env v

and desugar_obj_comprehension env _v = todo env "RECORD"

and desugar_import env v =
  match v with
  | Import (v1, v2) ->
      let v1 = desugar_tok env v1 in
      let v2 = desugar_string_ env v2 in
      todo env (v1, v2)
  | ImportStr (v1, v2) ->
      let v1 = desugar_tok env v1 in
      let v2 = desugar_string_ env v2 in
      todo env (v1, v2)

(*
let desugar_any env v :  =
  match v with
  | E v ->
      let v = desugar_expr env v in
      todo env v
*)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let desugar_program (x : program) : C.program =
  let env = { within_an_object = false } in
  let core = desugar_expr env x in
  core
