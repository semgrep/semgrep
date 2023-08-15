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
open File.Operators
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

(* Hook to customize how ojsonnet should resolve import expressions
 * (`local $NAME = import $PATH`). The first parameter below is the base
 * directory of the file currently processed and the second is the $PATH
 * above in the local expression.
 * The hook should return an AST_jsonnet expression if it can handle
 * the PATH or None, in which case it will default to a regular
 * jsonnet file import as in `local x = import "foo.jsonnet".
 *
 * This callback is useful for example in osemgrep to let ojsonnet
 * import yaml files (e.g., local x = import 'foo.yaml') or rules from the
 * registry (e.g., local x = import 'p/python').
 *)
type import_callback =
  Common.filename (* a directory *) -> string -> AST_jsonnet.expr option

let default_callback _ _ = None

type env = {
  (* like in Python jsonnet binding, "the base is the directly of the file" *)
  base : Common.filename; (* a directory *)
  import_callback : import_callback;
  (* TODO: cache_file
   * The cache_file is used to ensure referencial transparency (see the spec
   * when talking about import in the core jsonnet spec).
   *)
  within_an_object : bool;
}

exception Error of string * Tok.t

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let error tk s =
  (* TODO? if Parse_info.is_fake tk ... *)
  raise (Error (s, tk))

let fk = Tok.unsafe_fake_tok ""
let fb x = (fk, x, fk)
let mk_str_literal (str, tk) = Str (None, DoubleQuote, (fk, [ (str, tk) ], fk))
let mk_array exprs = A (fk, Array exprs, fk)

let mk_DotAccess_std id : expr =
  let std_id = ("std", fk) in
  DotAccess (Id std_id, fk, id)

and expr_or_null v : expr =
  match v with
  | None -> L (Null fk)
  | Some e -> e

let freshvar =
  let store = ref 0 in
  fun () ->
    incr store;
    ("!tmp" ^ string_of_int !store, fk)

(*****************************************************************************)
(* Builtins *)
(*****************************************************************************)

(* todo? auto generate the right name in otarzan? *)
let desugar_string _env x = x
let desugar_list f env x = x |> Common.map (fun x -> f env x)

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

let rec desugar_expr trace env v : C.program =
  try desugar_expr_aux trace env v with
  | Failure "TODO" ->
      pr2 (spf "TODO: construct not handled:\n %s" (show_expr v));
      failwith "TODO:desugar"

and desugar_expr_aux trace env v : C.program =
  match v with
  | L v -> (C.L v, trace)
  | O v ->
      let v = desugar_obj_inside trace env v in
      (v, trace)
  | A v ->
      let v = desugar_arr_inside trace env v in
      v
  | Id v ->
      let v = (desugar_wrap desugar_string) env v in
      (C.Id v, trace)
  | IdSpecial (Dollar, tdollar) -> (C.Id ("$", tdollar), trace)
  | IdSpecial v ->
      let v = (desugar_wrap desugar_special) env v in
      (C.IdSpecial v, trace)
  | Local (v1, v2, v3, v4) ->
      let tlocal = desugar_tok env v1 in
      let binds = (desugar_list (desugar_bind trace)) env v2 in
      let tsemi = desugar_tok env v3 in
      let e = desugar_expr trace env v4 in
      (C.Local (tlocal, binds, tsemi, e), trace)
  (* no need to handle specially super.id, handled by desugar_special *)
  | DotAccess (v1, v2, v3) ->
      let e = desugar_expr trace env v1 in
      let tdot = desugar_tok env v2 in
      let id = desugar_ident env v3 in
      (C.ArrayAccess (e, (tdot, (C.L (mk_str_literal id), []), tdot)), trace)
  | ArrayAccess (v1, v2) ->
      let v1 = desugar_expr trace env v1 in
      let v2 = (desugar_bracket (desugar_expr trace)) env v2 in
      (C.ArrayAccess (v1, v2), trace)
  | SliceAccess (e, (l, (e1opt, e2opt, e3opt), r)) ->
      let e' = expr_or_null e1opt in
      let e'' = expr_or_null e2opt in
      let e''' = expr_or_null e3opt in
      let std_slice = mk_DotAccess_std ("slice", l) in
      desugar_expr trace env
        (Call (std_slice, (l, [ Arg e; Arg e'; Arg e''; Arg e''' ], r)))
  | Call (v1, v2) ->
      let v1 = desugar_expr trace env v1 in
      let v2 =
        (desugar_bracket (desugar_list (desugar_argument trace))) env v2
      in
      (C.Call (v1, v2), trace)
  | UnaryOp (v1, v2) ->
      let v1 = (desugar_wrap desugar_unary_op) env v1 in
      let v2 = desugar_expr trace env v2 in
      (C.UnaryOp (v1, v2), trace)
  | BinaryOp (v1, (NotEq, t), v3) ->
      desugar_expr trace env (UnaryOp ((UBang, t), BinaryOp (v1, (Eq, t), v3)))
  | BinaryOp (v1, (Eq, t), v3) ->
      let std_equals = mk_DotAccess_std ("equals", t) in
      desugar_expr trace env (Call (std_equals, (fk, [ Arg v1; Arg v3 ], fk)))
  | BinaryOp (v1, (Mod, t), v3) ->
      let std_equals = mk_DotAccess_std ("mod", t) in
      desugar_expr trace env (Call (std_equals, (fk, [ Arg v1; Arg v3 ], fk)))
  (* no need to handle specially e in super, handled by desugar_special *)
  | BinaryOp (e, (In, t), e') ->
      let std_objectHasEx = mk_DotAccess_std ("objectHasEx", t) in
      let true_ = L (Bool (true, fk)) in
      desugar_expr trace
        { env with within_an_object = true }
        (Call (std_objectHasEx, (fk, [ Arg e'; Arg e; Arg true_ ], fk)))
  (* general case *)
  | BinaryOp (v1, v2, v3) ->
      let v1 = desugar_expr trace env v1 in
      let v2 = (desugar_wrap desugar_binary_op) env v2 in
      let v3 = desugar_expr trace env v3 in
      (C.BinaryOp (v1, v2, v3), trace)
  | AdjustObj (v1, v2) ->
      desugar_expr trace env (BinaryOp (v1, (Plus, fk), O v2))
  | If (tif, e, e', else_opt) ->
      let tif = desugar_tok env tif in
      let e = desugar_expr trace env e in
      let e' = desugar_expr trace env e' in
      let e'' =
        match else_opt with
        | Some (_telse, e'') -> desugar_expr trace env e''
        | None -> (C.L (Null fk), trace)
      in
      (C.If (tif, e, e', e''), trace)
  | Lambda v ->
      let v = desugar_function_definition trace env v in
      (C.Lambda v, trace)
  | I v -> desugar_import trace env v
  | Assert ((tassert, e, None), tsemi, e') ->
      let assert_failed_str = mk_str_literal ("Assertion failed", tassert) in
      desugar_expr trace env
        (Assert ((tassert, e, Some (fk, L assert_failed_str)), tsemi, e'))
  | Assert ((tassert, e, Some (tcolon, e')), _tsemi, e'') ->
      desugar_expr trace env
        (If (tassert, e, e'', Some (tcolon, Error (fk, e'))))
  | Error (v1, v2) ->
      let v1 = desugar_tok env v1 in
      let v2 = desugar_expr trace env v2 in
      (C.Error (v1, v2), trace)
  | ParenExpr v ->
      let _, e, _ = (desugar_bracket (desugar_expr trace)) env v in
      e
  | Ellipsis tk
  | DeepEllipsis (tk, _, _) ->
      error tk "Ellipsis can appear only in semgrep patterns"

and desugar_special _env v =
  match v with
  | Self -> C.Self
  | Super -> C.Super
  | Dollar -> assert false

and desugar_argument trace env v =
  match v with
  | Arg v ->
      let v = desugar_expr trace env v in
      C.Arg v
  | NamedArg (v1, v2, v3) ->
      let v1 = desugar_ident env v1 in
      let v2 = desugar_tok env v2 in
      let v3 = desugar_expr trace env v3 in
      C.NamedArg (v1, v2, v3)

and desugar_unary_op _env v = v

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

and desugar_arr_inside trace env (l, v, r) : C.expr_with_trace =
  let l = desugar_tok env l in
  let r = desugar_tok env r in
  match v with
  | Array v ->
      let xs = (desugar_list (desugar_expr trace)) env v in
      (C.Array (l, xs, r), trace)
  | ArrayComp (expr, for_comp, rest_comp) ->
      desugar_comprehension trace env expr (CompFor for_comp :: rest_comp)

(* Strictly speaking, the semantics say you're supposed to desugar as an
 * expression in tandem with desugaring the comprehension.
 * However, these return two different types in our desugaring process.
 * So we can't do that.
 * I hope that applying a single outer-level desugar_expr is equivalent
 * to interleaving it.
 *)
and desugar_comprehension_helper trace env e comps : AST_jsonnet.expr =
  match comps with
  | CompIf (tok, e') :: rest ->
      let empty_else = Some (fk, mk_array []) in
      let inner_exp =
        match rest with
        | [] -> mk_array [ e ]
        | __else__ -> desugar_comprehension_helper trace env e rest
      in
      If (tok, e', inner_exp, empty_else)
  | CompFor (_, x, _, e') :: rest ->
      let std_join (l, r) =
        Call (mk_DotAccess_std ("join", fk), fb [ Arg l; Arg r ])
      in
      let std_mk_array (length, f) =
        Call (mk_DotAccess_std ("makeArray", fk), fb [ Arg length; Arg f ])
      in
      let std_length array =
        Call (mk_DotAccess_std ("length", fk), fb [ Arg array ])
      in
      let arr = freshvar () in
      let f =
        let inner_exp =
          match rest with
          | [] -> mk_array [ e ]
          | __else__ -> desugar_comprehension_helper trace env e rest
        in
        let i = freshvar () in
        Lambda
          {
            f_tok = fk;
            f_params = fb [ P (i, None) ];
            f_body =
              Local
                ( fk,
                  [ B (x, fk, ArrayAccess (Id arr, Id i |> fb)) ],
                  fk,
                  inner_exp );
          }
      in
      Local
        ( fk,
          [ B (arr, fk, e') ],
          fk,
          std_join (mk_array [], std_mk_array (std_length (Id arr), f)) )
  | [] -> failwith "impossible: Empty array comprehension"

and desugar_comprehension trace env expr comps : C.program =
  desugar_expr trace env (desugar_comprehension_helper trace env expr comps)

(* The desugaring of method was already done at parsing time,
 * so no need to handle id with parameters here. See AST_jsonnet.bind comment.
 *)
and desugar_bind trace env v =
  match v with
  | B (v1, v2, v3) ->
      let id = desugar_ident env v1 in
      let teq = desugar_tok env v2 in
      let e = desugar_expr trace env v3 in
      C.B (id, teq, e)

and desugar_function_definition trace env { f_tok; f_params; f_body } =
  let f_tok = desugar_tok env f_tok in
  let f_params =
    desugar_bracket (desugar_list (desugar_parameter trace)) env f_params
  in
  let f_body = desugar_expr trace env f_body in
  { C.f_tok; f_params; f_body }

and desugar_parameter trace env v =
  match v with
  | P (v1, v2) -> (
      let id = desugar_ident env v1 in
      match v2 with
      | Some (v1, v2) ->
          let teq = desugar_tok env v1 in
          let e = desugar_expr trace env v2 in
          C.P (id, teq, e)
      | None ->
          C.P
            ( id,
              fk,
              ( C.Error
                  (fk, (C.L (mk_str_literal ("Parameter not bound", fk)), trace)),
                trace ) ))
  | ParamEllipsis tk ->
      error tk "ParamEllipsis can appear only in semgrep patterns"

and desugar_obj_inside trace env (l, v, r) : C.expr =
  let l = desugar_tok env l in
  let r = desugar_tok env r in
  match v with
  | Object v ->
      let binds, asserts, fields =
        v
        |> Common.partition_either3 (function
             | OLocal (_tlocal, x) -> Left3 x
             | OEllipsis tk ->
                 error tk "OEllipsis can appear only in semgrep patterns"
             | OAssert x -> Middle3 x
             | OField x -> Right3 x)
      in
      let binds =
        if env.within_an_object then binds
        else binds @ [ B (("$", fk), fk, IdSpecial (Self, fk)) ]
      in
      let asserts' =
        asserts
        |> Common.map (fun assert_ ->
               desugar_assert_ trace env (assert_, binds))
      in
      let fields' =
        fields
        |> Common.map (fun field -> desugar_field trace env (field, binds))
      in
      let obj = C.Object (asserts', fields') in
      if env.within_an_object then
        C.Local
          ( fk,
            [
              C.B (("$outerself", fk), fk, (C.IdSpecial (C.Self, fk), trace));
              C.B (("$outersuper", fk), fk, (C.IdSpecial (C.Super, fk), trace));
            ],
            fk,
            (C.O (l, obj, r), trace) )
      else C.O (l, obj, r)
  | ObjectComp _vTODO -> C.ExprTodo (("ObjectComp", l), O (l, v, r))

and desugar_assert_ trace (env : env) (v : assert_ * bind list) : C.obj_assert =
  let (tassert, e, opt), binds = v in
  match opt with
  | None ->
      let assert_failed_str = mk_str_literal ("Assertion failed", tassert) in
      desugar_assert_ trace env
        ((tassert, e, Some (fk, L assert_failed_str)), binds)
  | Some (_tcolon, e') ->
      let if_expr =
        If (tassert, e, L (Null fk), Some (tassert, Error (fk, e')))
      in
      let final_expr = Local (fk, binds, fk, if_expr) in
      ( tassert,
        desugar_expr trace { env with within_an_object = true } final_expr )

and desugar_field trace (env : env) (v : field * bind list) : C.field =
  let { fld_name; fld_attr; fld_hidden; fld_value = e' }, binds = v in
  let fld_name_desugared = desugar_field_name trace env fld_name in
  let fld_hidden = (desugar_wrap desugar_hidden) env fld_hidden in
  let fld_value =
    desugar_expr trace
      { env with within_an_object = true }
      (Local (fk, binds, fk, e'))
  in
  match fld_attr with
  | None -> { C.fld_name = fld_name_desugared; fld_hidden; fld_value }
  | Some (PlusField _) ->
      let name =
        match fld_name with
        | FId ident ->
            let id = desugar_ident env ident in
            let str = mk_str_literal id in
            L str
        | FStr str -> L (Str str)
        | FDynamic (_, p, _) -> p
      in

      let index = desugar_expr trace env name in
      let obj = C.IdSpecial (Super, fk) in

      let rhs = C.ArrayAccess ((obj, trace), (fk, index, fk)) in

      let new_fld_value = C.BinaryOp ((rhs, trace), (C.Plus, fk), fld_value) in
      {
        C.fld_name = fld_name_desugared;
        fld_hidden;
        fld_value = (new_fld_value, trace);
      }

and desugar_field_name trace env v =
  match v with
  | FId v ->
      let id = desugar_ident env v in
      let str = mk_str_literal id in
      C.FExpr (fk, (C.L str, trace), fk)
  | FStr v -> C.FExpr (fk, (C.L (Str v), trace), fk)
  | FDynamic v ->
      let l, v, r = (desugar_bracket (desugar_expr trace)) env v in
      C.FExpr (l, v, r)

and desugar_hidden _env v = v

and desugar_import trace env v : C.expr_with_trace =
  match v with
  | Import (tk, str_) ->
      (* TODO: keep history of import, use tk *)
      let str, _tk = string_of_string_ str_ in
      let expr, env =
        match env.import_callback env.base str with
        | None ->
            let final_path = Filename.concat env.base str in
            if not (Sys.file_exists final_path) then
              error tk (spf "file does not exist: %s" final_path);
            let ast = Parse_jsonnet.parse_program (Fpath.v final_path) in
            let env = { env with base = Filename.dirname final_path } in
            (ast, env)
        | Some ast ->
            (* TODO? let the import callback adjust base? *)
            (ast, env)
      in
      (* recurse *)
      desugar_expr (tk :: trace) env expr
  (* TODO? could also have import_callback on ImportStr!
   * so could fetch network data from our policies
   *)
  | ImportStr (tk, str_) ->
      let str, _tk = string_of_string_ str_ in
      let final_path = Filename.concat env.base str in
      if not (Sys.file_exists final_path) then
        error tk (spf "file does not exist: %s" final_path);
      let s = Common.read_file final_path in
      (C.L (mk_str_literal (s, tk)), trace)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let desugar_expr_profiled env e = desugar_expr env e [@@profiling]

let desugar_program ?(import_callback = default_callback) ?(use_std = true)
    (file : Fpath.t) (e : program) : C.program =
  let env =
    {
      within_an_object = false;
      base = Filename.dirname !!file;
      import_callback;
    }
  in
  let e =
    if use_std then
      let std = Std_jsonnet.get_std_jsonnet () in
      (* 'local std = e_std; e' *)
      Local (fk, [ B (("std", fk), fk, std) ], fk, e)
    else e
  in
  let core = desugar_expr_profiled [] env e in
  core
