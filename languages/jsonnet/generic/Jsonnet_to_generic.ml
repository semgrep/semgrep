(* Yoann Padioleau
 *
 * Copyright (C) 2023 r2c
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
module G = AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* AST_jsonnet to AST_generic.
 *
 * See AST_generic.ml for more information.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let map_string _env x = x
let map_bool _env x = x
let map_list f env xs = Common.map (f env) xs
let map_option f env x = Option.map (f env) x
let todo _env _v = failwith "TODO"

(*****************************************************************************)
(* Boilerplate *)
(*****************************************************************************)

let map_tok _env v = v

let map_wrap ofa env (v1, v2) =
  let v1 = ofa env v1 in
  let v2 = map_tok env v2 in
  todo env (v1, v2)

let map_bracket ofa env (v1, v2, v3) =
  let v1 = map_tok env v1 in
  let v2 = ofa env v2 in
  let v3 = map_tok env v3 in
  todo env (v1, v2, v3)

let map_ident env v = (map_wrap map_string) env v

let rec map_expr env v : G.expr =
  match v with
  | L v ->
      let v = map_literal env v in
      todo env v
  | O v ->
      let v = (map_bracket map_obj_inside) env v in
      todo env v
  | A v ->
      let v = (map_bracket map_arr_inside) env v in
      todo env v
  | Id v ->
      let v = (map_wrap map_string) env v in
      todo env v
  | IdSpecial v ->
      let v = (map_wrap map_special) env v in
      todo env v
  | Local (v1, v2, v3, v4) ->
      let v1 = map_tok env v1 in
      let v2 = (map_list map_bind) env v2 in
      let v3 = map_tok env v3 in
      let v4 = map_expr env v4 in
      todo env (v1, v2, v3, v4)
  | DotAccess (v1, v2, v3) ->
      let v1 = map_expr env v1 in
      let v2 = map_tok env v2 in
      let v3 = map_ident env v3 in
      todo env (v1, v2, v3)
  | ArrayAccess (v1, v2) ->
      let v1 = map_expr env v1 in
      let v2 = (map_bracket map_expr) env v2 in
      todo env (v1, v2)
  | SliceAccess (v1, v2) ->
      let v1 = map_expr env v1 in
      let map_tuple env (v1, v2, v3) =
        let v1 = (map_option map_expr) env v1 in
        let v2 = (map_option map_expr) env v2 in
        let v3 = (map_option map_expr) env v3 in
        todo env (v1, v2, v3)
      in
      let v2 = (map_bracket map_tuple) env v2 in
      todo env (v1, v2)
  | Call (v1, v2) ->
      let v1 = map_expr env v1 in
      let v2 = (map_bracket (map_list map_argument)) env v2 in
      todo env (v1, v2)
  | UnaryOp (v1, v2) ->
      let v1 = (map_wrap map_unary_op) env v1 in
      let v2 = map_expr env v2 in
      todo env (v1, v2)
  | BinaryOp (v1, v2, v3) ->
      let v1 = map_expr env v1 in
      let v2 = (map_wrap map_binary_op) env v2 in
      let v3 = map_expr env v3 in
      todo env (v1, v2, v3)
  | If (v1, v2, v3, v4) ->
      let v1 = map_tok env v1 in
      let v2 = map_expr env v2 in
      let v3 = map_expr env v3 in
      let map_tuple env (v1, v2) =
        let v1 = map_tok env v1 in
        let v2 = map_expr env v2 in
        todo env (v1, v2)
      in
      let v4 = (map_option map_tuple) env v4 in
      todo env (v1, v2, v3, v4)
  | AdjustObj (v1, v2) ->
      let v1 = map_expr env v1 in
      let v2 = (map_bracket map_obj_inside) env v2 in
      todo env (v1, v2)
  | Lambda v ->
      let v = map_function_definition env v in
      todo env v
  | I v ->
      let v = map_import env v in
      todo env v
  | Assert (v1, v2, v3) ->
      let v1 = map_assert_ env v1 in
      let v2 = map_tok env v2 in
      let v3 = map_expr env v3 in
      todo env (v1, v2, v3)
  | Error (v1, v2) ->
      let v1 = map_tok env v1 in
      let v2 = map_expr env v2 in
      todo env (v1, v2)
  | ParenExpr v ->
      let v = (map_bracket map_expr) env v in
      todo env v

and map_literal env v =
  match v with
  | Null v ->
      let v = map_tok env v in
      todo env v
  | Bool v ->
      let v = (map_wrap map_bool) env v in
      todo env v
  | Number v ->
      let v = (map_wrap map_string) env v in
      todo env v
  | Str v ->
      let v = map_string_ env v in
      todo env v

and map_string_ env (v1, v2, v3) =
  let v1 = (map_option map_verbatim) env v1 in
  let v2 = map_string_kind env v2 in
  let v3 = (map_bracket map_string_content) env v3 in
  todo env (v1, v2, v3)

and map_verbatim env v = map_tok env v

and map_string_kind env v =
  match v with
  | SingleQuote -> todo env ()
  | DoubleQuote -> todo env ()
  | TripleBar -> todo env ()

and map_string_content env v = (map_list (map_wrap map_string)) env v

and map_special env v =
  match v with
  | Self -> todo env ()
  | Super -> todo env ()
  | Dollar -> todo env ()

and map_argument env v : G.argument =
  match v with
  | Arg v ->
      let v = map_expr env v in
      todo env v
  | NamedArg (v1, v2, v3) ->
      let v1 = map_ident env v1 in
      let v2 = map_tok env v2 in
      let v3 = map_expr env v3 in
      todo env (v1, v2, v3)

and map_unary_op env v =
  match v with
  | UPlus -> todo env ()
  | UMinus -> todo env ()
  | UBang -> todo env ()
  | UTilde -> todo env ()

and map_binary_op env v =
  match v with
  | Plus -> todo env ()
  | Minus -> todo env ()
  | Mult -> todo env ()
  | Div -> todo env ()
  | Mod -> todo env ()
  | LSL -> todo env ()
  | LSR -> todo env ()
  | Lt -> todo env ()
  | LtE -> todo env ()
  | Gt -> todo env ()
  | GtE -> todo env ()
  | Eq -> todo env ()
  | NotEq -> todo env ()
  | In -> todo env ()
  | And -> todo env ()
  | Or -> todo env ()
  | BitAnd -> todo env ()
  | BitOr -> todo env ()
  | BitXor -> todo env ()

and map_assert_ env (v1, v2, v3) =
  let v1 = map_tok env v1 in
  let v2 = map_expr env v2 in
  let map_tuple env (v1, v2) =
    let v1 = map_tok env v1 in
    let v2 = map_expr env v2 in
    todo env (v1, v2)
  in
  let v3 = (map_option map_tuple) env v3 in
  todo env (v1, v2, v3)

and map_arr_inside env v =
  match v with
  | Array v ->
      let v = (map_list map_expr) env v in
      todo env v
  | ArrayComp v ->
      let v = (map_comprehension map_expr) env v in
      todo env v

and map_comprehension ofa env (v1, v2, v3) =
  let v1 = ofa env v1 in
  let v2 = map_for_comp env v2 in
  let v3 = (map_list map_for_or_if_comp) env v3 in
  todo env (v1, v2, v3)

(* TODO: use forall type 'a. *)
and map_comprehension2 ofa env (v1, v2, v3) =
  let v1 = ofa env v1 in
  let v2 = map_for_comp env v2 in
  let v3 = (map_list map_for_or_if_comp) env v3 in
  todo env (v1, v2, v3)

and map_for_or_if_comp env v =
  match v with
  | CompFor v ->
      let v = map_for_comp env v in
      todo env v
  | CompIf v ->
      let v = map_if_comp env v in
      todo env v

and map_for_comp env (v1, v2, v3, v4) =
  let v1 = map_tok env v1 in
  let v2 = map_ident env v2 in
  let v3 = map_tok env v3 in
  let v4 = map_expr env v4 in
  todo env (v1, v2, v3, v4)

and map_if_comp env (v1, v2) =
  let v1 = map_tok env v1 in
  let v2 = map_expr env v2 in
  todo env (v1, v2)

and map_bind env v =
  match v with
  | B (v1, v2, v3) ->
      let v1 = map_ident env v1 in
      let v2 = map_tok env v2 in
      let v3 = map_expr env v3 in
      todo env (v1, v2, v3)

and map_function_definition env v =
  let { f_tok; f_params; f_body } = v in
  let f_tok = map_tok env f_tok in
  let f_params = (map_bracket (map_list map_parameter)) env f_params in
  let f_body = map_expr env f_body in
  todo env (f_tok, f_params, f_body)

and map_parameter env v =
  match v with
  | P (v1, v2) ->
      let v1 = map_ident env v1 in
      let map_tuple env (v1, v2) =
        let v1 = map_tok env v1 in
        let v2 = map_expr env v2 in
        todo env (v1, v2)
      in
      let v2 = (map_option map_tuple) env v2 in
      todo env (v1, v2)

and map_obj_inside env v =
  match v with
  | Object v ->
      let v = (map_list map_obj_member) env v in
      todo env v
  | ObjectComp v ->
      let v = map_obj_comprehension env v in
      todo env v

and map_obj_member env v =
  match v with
  | OLocal v ->
      let v = map_obj_local env v in
      todo env v
  | OField v ->
      let v = map_field env v in
      todo env v
  | OAssert v ->
      let v = map_assert_ env v in
      todo env v

and map_field env v =
  let { fld_name; fld_attr; fld_hidden; fld_value } = v in
  let fld_name = map_field_name env fld_name in
  let fld_attr = (map_option map_attribute) env fld_attr in
  let fld_hidden = (map_wrap map_hidden) env fld_hidden in
  let fld_value = map_expr env fld_value in
  todo env (fld_name, fld_attr, fld_hidden, fld_value)

and map_field_name env v =
  match v with
  | FId v ->
      let v = map_ident env v in
      todo env v
  | FStr v ->
      let v = map_string_ env v in
      todo env v
  | FDynamic v ->
      let v = (map_bracket map_expr) env v in
      todo env v

and map_hidden env v =
  match v with
  | Visible -> todo env ()
  | Hidden -> todo env ()
  | ForcedVisible -> todo env ()

and map_attribute env v =
  match v with
  | PlusField v ->
      let v = map_tok env v in
      todo env v

and map_obj_local env (v1, v2) =
  let v1 = map_tok env v1 in
  let v2 = map_bind env v2 in
  todo env (v1, v2)

and map_obj_comprehension env v =
  let { oc_locals1; oc_comp; oc_locals2 } = v in
  let oc_locals1 = (map_list map_obj_local) env oc_locals1 in
  let map_tuple env (v1, v2, v3) =
    let v1 = (map_bracket map_expr) env v1 in
    let v2 = map_tok env v2 in
    let v3 = map_expr env v3 in
    todo env (v1, v2, v3)
  in
  let oc_comp = (map_comprehension2 map_tuple) env oc_comp in
  let oc_locals2 = (map_list map_obj_local) env oc_locals2 in
  { oc_locals1; oc_comp; oc_locals2 }

and map_import env v =
  match v with
  | Import (v1, v2) ->
      let v1 = map_tok env v1 in
      let v2 = map_string_ env v2 in
      todo env (v1, v2)
  | ImportStr (v1, v2) ->
      let v1 = map_tok env v1 in
      let v2 = map_string_ env v2 in
      todo env (v1, v2)

let map_program env v : G.program =
  let e = map_expr env v in
  todo env e

let map_any env v : G.any =
  match v with
  | E v ->
      let v = map_expr env v in
      todo env v

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let program xs = map_program () xs
let any x = map_any () x
