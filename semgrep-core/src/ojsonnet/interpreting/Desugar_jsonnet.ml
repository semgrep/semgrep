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
open Common
open AST_jsonnet
module C = Core_jsonnet

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* AST_jsonnet to Core_jsonnet.
 *
 * See https://jsonnet.org/ref/spec.html#desugaring
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type env = {
  within_an_object : bool; (* TODO: pwd, current_file, import_callback, etc. *)
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let fk = Parse_info.unsafe_fake_info ""
let true_ = L (Bool (true, fk))

let mk_str_literal (str, tk) =
  L (Str (None, DoubleQuote, (fk, [ (str, tk) ], fk)))

let mk_core_str_literal (str, tk) =
  C.L (C.Str (None, C.DoubleQuote, (fk, [ (str, tk) ], fk)))

let mk_DotAccess_std id : expr =
  let std_id = ("std", fk) in
  DotAccess (Id std_id, fk, id)

and expr_or_null v : expr =
  match v with
  | None -> L (Null fk)
  | Some e -> e

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
  (v1, v2)

let desugar_bracket ofa env (v1, v2, v3) =
  let v1 = desugar_tok env v1 in
  let v2 = ofa env v2 in
  let v3 = desugar_tok env v3 in
  (v1, v2, v3)

let desugar_ident env v : C.ident = (desugar_wrap desugar_string) env v

let rec desugar_expr env v : C.expr =
  try desugar_expr_aux env v with
  | Failure "TODO" ->
      pr2 (spf "TODO: construct not handled:\n %s" (show_expr v));
      failwith "TODO2"

and desugar_expr_aux env v =
  match v with
  | L v ->
      let v = desugar_literal env v in
      C.L v
  | O v ->
      let v = (desugar_bracket desugar_obj_inside) env v in
      todo env v
  | A v ->
      let v = (desugar_bracket desugar_arr_inside) env v in
      todo env v
  | Id v ->
      let v = (desugar_wrap desugar_string) env v in
      C.Id v
  (* TODO? IdSpecial (Dollar, tdollar) ?? *)
  | IdSpecial v ->
      let v = (desugar_wrap desugar_special) env v in
      C.IdSpecial v
  | Local (v1, v2, v3, v4) ->
      let tlocal = desugar_tok env v1 in
      let binds = (desugar_list desugar_bind) env v2 in
      let tsemi = desugar_tok env v3 in
      let e = desugar_expr env v4 in
      C.Local (tlocal, binds, tsemi, e)
  (* no need to handle specially super.id, handled by desugar_special *)
  | DotAccess (v1, v2, v3) ->
      let e = desugar_expr env v1 in
      let tdot = desugar_tok env v2 in
      let id = desugar_ident env v3 in
      C.ArrayAccess (e, (tdot, mk_core_str_literal id, tdot))
  | ArrayAccess (v1, v2) ->
      let v1 = desugar_expr env v1 in
      let v2 = (desugar_bracket desugar_expr) env v2 in
      C.ArrayAccess (v1, v2)
  | SliceAccess (e, (l, (e1opt, e2opt, e3opt), r)) ->
      let e' = expr_or_null e1opt in
      let e'' = expr_or_null e2opt in
      let e''' = expr_or_null e3opt in
      let std_slice = mk_DotAccess_std ("slice", l) in
      desugar_expr env
        (Call (std_slice, (l, [ Arg e; Arg e'; Arg e''; Arg e''' ], r)))
  | Call (v1, v2) ->
      let v1 = desugar_expr env v1 in
      let v2 = (desugar_bracket (desugar_list desugar_argument)) env v2 in
      C.Call (v1, v2)
  | UnaryOp (v1, v2) ->
      let v1 = (desugar_wrap desugar_unary_op) env v1 in
      let v2 = desugar_expr env v2 in
      C.UnaryOp (v1, v2)
  | BinaryOp (v1, (NotEq, t), v3) ->
      desugar_expr env (UnaryOp ((UBang, t), BinaryOp (v1, (Eq, t), v3)))
  | BinaryOp (v1, (Eq, t), v3) ->
      let std_equals = mk_DotAccess_std ("equals", t) in
      desugar_expr env (Call (std_equals, (fk, [ Arg v1; Arg v3 ], fk)))
  | BinaryOp (v1, (Mod, t), v3) ->
      let std_equals = mk_DotAccess_std ("mod", t) in
      desugar_expr env (Call (std_equals, (fk, [ Arg v1; Arg v3 ], fk)))
  (* no need to handle specially e in super, handled by desugar_special *)
  | BinaryOp (e, (In, t), e') ->
      let std_objectHasEx = mk_DotAccess_std ("objectHasEx", t) in
      desugar_expr env
        (Call (std_objectHasEx, (fk, [ Arg e'; Arg e; Arg true_ ], fk)))
  (* general case *)
  | BinaryOp (v1, v2, v3) ->
      let v1 = desugar_expr env v1 in
      let v2 = (desugar_wrap desugar_binary_op) env v2 in
      let v3 = desugar_expr env v3 in
      C.BinaryOp (v1, v2, v3)
  | AdjustObj (v1, v2) -> desugar_expr env (BinaryOp (v1, (Plus, fk), O v2))
  | If (tif, e, e', else_opt) ->
      let tif = desugar_tok env tif in
      let e = desugar_expr env e in
      let e' = desugar_expr env e' in
      let e'' =
        match else_opt with
        | Some (_telse, e'') -> desugar_expr env e''
        | None -> C.L (C.Null fk)
      in
      C.If (tif, e, e', e'')
  | Lambda v ->
      let v = desugar_function_definition env v in
      C.Lambda v
  | I v ->
      let v = desugar_import env v in
      todo env v
  | Assert ((tassert, e, None), tsemi, e') ->
      let assert_failed_str = mk_str_literal ("Assertion failed", tassert) in
      desugar_expr env
        (Assert ((tassert, e, Some (fk, assert_failed_str)), tsemi, e'))
  | Assert ((tassert, e, Some (tcolon, e')), _tsemi, e'') ->
      desugar_expr env (If (tassert, e, e'', Some (tcolon, Error (fk, e'))))
  | Error (v1, v2) ->
      let v1 = desugar_tok env v1 in
      let v2 = desugar_expr env v2 in
      C.Error (v1, v2)
  | ParenExpr v ->
      let _, e, _ = (desugar_bracket desugar_expr) env v in
      e

and desugar_literal env v =
  match v with
  | Null v ->
      let v = desugar_tok env v in
      C.Null v
  | Bool v ->
      let v = (desugar_wrap desugar_bool) env v in
      C.Bool v
  | Number v ->
      let v = (desugar_wrap desugar_string) env v in
      C.Number v
  | Str v ->
      let v = desugar_string_ env v in
      C.Str v

and desugar_string_ env v =
  (fun env (v1, v2, v3) ->
    let v1 = (desugar_option desugar_verbatim) env v1 in
    let v2 = desugar_string_kind env v2 in
    let v3 = (desugar_bracket desugar_string_content) env v3 in
    (v1, v2, v3))
    env v

and desugar_verbatim env v = desugar_tok env v

and desugar_string_kind _env v =
  match v with
  | SingleQuote -> C.SingleQuote
  | DoubleQuote -> C.DoubleQuote
  | TripleBar -> C.TripleBar

and desugar_string_content env v =
  (desugar_list (desugar_wrap desugar_string)) env v

and desugar_special env v =
  match v with
  | Self -> C.Self
  | Super -> C.Super
  | Dollar -> todo env ()

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

and desugar_unary_op _env v =
  match v with
  | UPlus -> C.UPlus
  | UMinus -> C.UMinus
  | UBang -> C.UBang
  | UTilde -> C.UTilde

(* the assert false are here because the cases should be handled by the
 * caller in BinaryOp
 *)
and desugar_binary_op _env v =
  match v with
  | Plus -> C.Plus
  | Minus -> C.Minus
  | Mult -> C.Mult
  | Div -> C.Div
  | Mod -> assert false
  | LSL -> C.LSL
  | LSR -> C.LSR
  | Lt -> C.Lt
  | LtE -> C.LtE
  | Gt -> C.Gt
  | GtE -> C.GtE
  | Eq -> assert false
  | NotEq -> assert false
  | And -> C.And
  | Or -> C.Or
  | BitAnd -> C.BitAnd
  | BitOr -> C.BitOr
  | BitXor -> C.BitXor
  | In -> assert false

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
      C.Array v
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

(* The desugaring of method was already done at parsing time,
 * so no need to handle id with parameters here. See AST_jsonnet.bind comment.
 *)
and desugar_bind env v =
  match v with
  | B (v1, v2, v3) ->
      let id = desugar_ident env v1 in
      let teq = desugar_tok env v2 in
      let e = desugar_expr env v3 in
      C.B (id, teq, e)

and desugar_function_definition env { f_tok; f_params; f_body } =
  let f_tok = desugar_tok env f_tok in
  let f_params =
    desugar_bracket (desugar_list desugar_parameter) env f_params
  in
  let f_body = desugar_expr env f_body in
  { C.f_tok; f_params; f_body }

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
      let id = desugar_ident env v in
      C.FExpr (C.Id id)
  | FStr v ->
      let v = desugar_string_ env v in
      C.FExpr (C.L (C.Str v))
  | FDynamic v ->
      let _l, v, _r = (desugar_bracket desugar_expr) env v in
      C.FExpr v

and desugar_hidden _env v =
  match v with
  | Colon -> C.Colon
  | TwoColons -> C.TwoColons
  | ThreeColons -> C.ThreeColons

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

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let desugar_program (e : program) : C.program =
  let env = { within_an_object = false } in
  (* TODO: skipped for now because std.jsonnet contains too many complicated
   * things we don't handle, and it actually does not even parse right now.
   *)
  (*
  let std = Std_jsonnet.get_std_jsonnet () in
  let e =
    (* 'local std = e_std; e' *)
    Local (fk, [B (("std", fk), fk, std)], fk, e) in
  *)
  let core = desugar_expr env e in
  core
