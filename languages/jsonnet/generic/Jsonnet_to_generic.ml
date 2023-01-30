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
  (v1, v2)

let map_bracket ofa env (v1, v2, v3) =
  let v1 = map_tok env v1 in
  let v2 = ofa env v2 in
  let v3 = map_tok env v3 in
  (v1, v2, v3)

let map_ident env v = (map_wrap map_string) env v

let rec map_expr env v : G.expr =
  match v with
  | L v ->
      let lit = map_literal env v in
      G.L lit |> G.e
  | O v ->
      let v = (map_bracket map_obj_inside) env v in
      todo env v
  | A v ->
      let v = (map_bracket map_arr_inside) env v in
      todo env v
  | Id v ->
      let id = map_ident env v in
      G.N (G.Id (id, G.empty_id_info ())) |> G.e
  | IdSpecial v ->
      let f, tk = (map_wrap map_special) env v in
      f tk
  | Local (v1, v2, v3, v4) ->
      let v1 = map_tok env v1 in
      let v2 = (map_list map_bind) env v2 in
      let v3 = map_tok env v3 in
      let v4 = map_expr env v4 in
      todo env (v1, v2, v3, v4)
  | DotAccess (v1, v2, v3) ->
      let e = map_expr env v1 in
      let tdot = map_tok env v2 in
      let fld = map_ident env v3 in
      G.DotAccess (e, tdot, G.FN (G.Id (fld, G.empty_id_info ()))) |> G.e
  | ArrayAccess (v1, v2) ->
      let e = map_expr env v1 in
      let idx = (map_bracket map_expr) env v2 in
      G.ArrayAccess (e, idx) |> G.e
  | SliceAccess (v1, v2) ->
      let e = map_expr env v1 in
      let map_tuple env (v1, v2, v3) =
        let v1 = (map_option map_expr) env v1 in
        let v2 = (map_option map_expr) env v2 in
        let v3 = (map_option map_expr) env v3 in
        (v1, v2, v3)
      in
      let indices = (map_bracket map_tuple) env v2 in
      G.SliceAccess (e, indices) |> G.e
  | Call (v1, v2) ->
      let e = map_expr env v1 in
      let args = (map_bracket (map_list map_argument)) env v2 in
      G.Call (e, args) |> G.e
  | UnaryOp (v1, v2) ->
      let v1 = (map_wrap map_unary_op) env v1 in
      let v2 = map_expr env v2 in
      G.opcall v1 [ v2 ]
  | BinaryOp (v1, v2, v3) ->
      let v1 = map_expr env v1 in
      let v2 = (map_wrap map_binary_op) env v2 in
      let v3 = map_expr env v3 in
      G.opcall v2 [ v1; v3 ]
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
      let def = map_function_definition env v in
      G.Lambda def |> G.e
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
      G.ParenExpr v |> G.e

and map_literal env v : G.literal =
  match v with
  | Null v ->
      let v = map_tok env v in
      G.Null v
  | Bool v ->
      let v = (map_wrap map_bool) env v in
      G.Bool v
  | Number v ->
      let v = (map_wrap map_string) env v in
      todo env v
  | Str v ->
      let v = map_string_ env v in
      G.String v

and map_string_ env (v1, v2, v3) : string G.wrap =
  let v1 = (map_option map_verbatim) env v1 in
  let v2 = map_string_kind env v2 in
  let v3 = (map_bracket map_string_content) env v3 in
  todo env (v1, v2, v3)

and map_verbatim env v = map_tok env v

and map_string_kind _env v =
  match v with
  | SingleQuote -> ()
  | DoubleQuote -> ()
  | TripleBar -> ()

and map_string_content env v = (map_list (map_wrap map_string)) env v

and map_special _env v =
  match v with
  | Self -> fun tk -> G.IdSpecial (G.Self, tk) |> G.e
  | Super -> fun tk -> G.IdSpecial (G.Super, tk) |> G.e
  | Dollar -> fun tk -> G.N (G.Id (("$", tk), G.empty_id_info ())) |> G.e

and map_argument env v : G.argument =
  match v with
  | Arg v ->
      let e = map_expr env v in
      G.Arg e
  | NamedArg (v1, v2, v3) ->
      let id = map_ident env v1 in
      let _teq = map_tok env v2 in
      let e = map_expr env v3 in
      G.ArgKwd (id, e)

and map_unary_op _env v =
  match v with
  | UPlus -> G.Plus
  | UMinus -> G.Minus
  | UBang -> G.Not
  | UTilde -> G.BitNot (* actually lognot? *)

and map_binary_op _env v =
  match v with
  | Plus -> G.Plus
  | Minus -> G.Minus
  | Mult -> G.Mult
  | Div -> G.Div
  | Mod -> G.Mod
  | LSL -> G.LSL
  | LSR -> G.LSR
  | Lt -> G.Lt
  | LtE -> G.LtE
  | Gt -> G.Gt
  | GtE -> G.GtE
  | Eq -> G.Eq
  | NotEq -> G.NotEq
  | In -> G.In
  | And -> G.And
  | Or -> G.Or
  | BitAnd -> G.BitAnd
  | BitOr -> G.BitOr
  | BitXor -> G.BitXor

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

and map_for_or_if_comp env v : G.for_or_if_comp =
  match v with
  | CompFor v ->
      let v = map_for_comp env v in
      v
  | CompIf v ->
      let v = map_if_comp env v in
      v

and map_for_comp env (v1, v2, v3, v4) =
  let tfor = map_tok env v1 in
  let id = map_ident env v2 in
  let tin = map_tok env v3 in
  let e = map_expr env v4 in
  G.CompFor (tfor, G.PatId (id, G.empty_id_info ()), tin, e)

and map_if_comp env (v1, v2) =
  let tif = map_tok env v1 in
  let e = map_expr env v2 in
  G.CompIf (tif, e)

and map_bind env v =
  match v with
  | B (v1, v2, v3) ->
      let v1 = map_ident env v1 in
      let v2 = map_tok env v2 in
      let v3 = map_expr env v3 in
      (v1, v2, v3)

and map_function_definition env v : G.function_definition =
  let { f_tok; f_params; f_body } = v in
  let f_tok = map_tok env f_tok in
  let _l, fparams, _r = (map_bracket (map_list map_parameter)) env f_params in
  let f_body = map_expr env f_body in
  {
    fkind = (G.LambdaKind, f_tok);
    fparams;
    fbody = G.FBExpr f_body;
    frettype = None;
  }

and map_parameter env v : G.parameter =
  match v with
  | P (v1, v2) ->
      let id = map_ident env v1 in
      let map_tuple env (v1, v2) =
        let _teq = map_tok env v1 in
        let e = map_expr env v2 in
        e
      in
      let pdefault = (map_option map_tuple) env v2 in
      G.Param { (G.param_of_id id) with pdefault }

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
      let id = map_ident env v in
      G.FN (G.Id (id, G.empty_id_info ()))
  | FStr v ->
      let s, tk = map_string_ env v in
      G.FN (G.Id ((s, tk), G.empty_id_info ()))
  | FDynamic v ->
      let l, e, r = (map_bracket map_expr) env v in
      G.FDynamic (G.ParenExpr (l, e, r) |> G.e)

and map_hidden _env v =
  match v with
  | Visible -> ()
  | Hidden -> ()
  | ForcedVisible -> ()

and map_attribute env v =
  match v with
  | PlusField v ->
      let v = map_tok env v in
      v

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
  todo env (oc_locals1, oc_comp, oc_locals2)

and map_import env v =
  match v with
  | Import (v1, v2) ->
      let timport = map_tok env v1 in
      let str = map_string_ env v2 in
      todo env (timport, str)
  | ImportStr (v1, v2) ->
      let timportstr = map_tok env v1 in
      let str = map_string_ env v2 in
      todo env (timportstr, str)

let map_program env v : G.program =
  let e = map_expr env v in
  [ G.exprstmt e ]

let map_any env v : G.any =
  match v with
  | E v ->
      let e = map_expr env v in
      G.E e

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let program xs = map_program () xs
let any x = map_any () x
