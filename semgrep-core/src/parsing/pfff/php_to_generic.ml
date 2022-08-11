(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common
open Ast_php
module G = AST_generic
module H = AST_generic_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Ast_php to AST_generic.
 *
 * See AST_generic.ml for more information.
 *
 * TODO: convert some Assign in VarDef like Python_to_generic.ml
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let id x = x
let option = Option.map
let list = List.map
let bool = id
let string = id

(* raise AST_generic.Error *)
let error = AST_generic.error
let fake = AST_generic.fake
let fb = AST_generic.fake_bracket

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let info x = x

let wrap _of_a (v1, v2) =
  let v1 = _of_a v1 and v2 = info v2 in
  (v1, v2)

let bracket of_a (t1, x, t2) = (info t1, of_a x, info t2)
let ident v = wrap string v
let var v = wrap string v
let qualified_ident v = list ident v

let name_of_qualified_ident xs =
  let xs = qualified_ident xs in
  H.name_of_ids xs

let name v = qualified_ident v
let fixOp x = H.conv_incr x

let binaryOp (x, t) =
  match x with
  | BinaryConcat -> Left (G.Concat, t)
  | CombinedComparison -> Left (G.Cmp, t)
  | ArithOp op -> Left (H.conv_op op, t)

let unaryOp x = x

let modifierbis = function
  | Public -> G.Public
  | Private -> G.Private
  | Protected -> G.Protected
  | Static -> G.Static
  | Abstract -> G.Abstract
  | Final -> G.Final
  | Async -> G.Async

let ptype (x, t) =
  match x with
  | BoolTy -> G.ty_builtin ("bool", t)
  | IntTy -> G.ty_builtin ("int", t)
  | DoubleTy -> G.ty_builtin ("double", t)
  | StringTy -> G.ty_builtin ("string", t)
  (* TODO: TyArray of gen? *)
  | ArrayTy -> G.ty_builtin ("array", t)
  | ObjectTy -> G.ty_builtin ("object", t)

let list_expr_to_opt xs =
  match xs with
  | [] -> None
  | [ e ] -> Some e
  | x :: xs -> Some (G.Seq (x :: xs) |> G.e)

let for_var xs = xs |> List.map (fun e -> G.ForInitExpr e)

let rec stmt_aux = function
  | Expr (v1, t) ->
      let v1 = expr v1 in
      [ G.ExprStmt (v1, t) |> G.s ]
  | Block v1 ->
      let v1 = bracket (list stmt) v1 in
      [ G.Block v1 |> G.s ]
  | If (t, v1, v2, v3) ->
      let v1 = expr v1 and v2 = stmt v2 and v3 = stmt v3 in
      [ G.If (t, G.Cond v1, v2, Some (* TODO *) v3) |> G.s ]
  | Switch (t, v1, v2) ->
      let v1 = expr v1
      and v2 = list case v2 |> List.map (fun x -> G.CasesAndBody x) in
      [ G.Switch (t, Some (G.Cond v1), v2) |> G.s ]
  | While (t, v1, v2) ->
      let v1 = expr v1 and v2 = stmt v2 in
      [ G.While (t, G.Cond v1, v2) |> G.s ]
  | Do (t, v1, v2) ->
      let v1 = stmt v1 and v2 = expr v2 in
      [ G.DoWhile (t, v1, v2) |> G.s ]
  | For (t, v1, v2, v3, v4) ->
      let v1 = list expr v1
      and v2 = list expr v2
      and v3 = list expr v3
      and v4 = stmt v4 in
      [
        G.For
          ( t,
            G.ForClassic (for_var v1, list_expr_to_opt v2, list_expr_to_opt v3),
            v4 )
        |> G.s;
      ]
  | Foreach (t, v1, t2, v2, v3) ->
      let v1 = expr v1 and v2 = foreach_pattern v2 and v3 = stmt v3 in
      [ G.For (t, G.ForEach (v2, t2, v1), v3) |> G.s ]
  | Return (t, v1) ->
      let v1 = option expr v1 in
      [ G.Return (t, v1, G.sc) |> G.s ]
  | Break (t, v1) -> [ G.Break (t, opt_expr_to_label_ident v1, G.sc) |> G.s ]
  | Continue (t, v1) ->
      [ G.Continue (t, opt_expr_to_label_ident v1, G.sc) |> G.s ]
  | Label (id, _, v1) ->
      let v1 = stmt v1 in
      [ G.Label (ident id, v1) |> G.s ]
  | Goto (t, id) -> [ G.Goto (t, ident id, G.sc) |> G.s ]
  | Try (t, v1, v2, v3) ->
      let v1 = stmt v1 and v2 = list catch v2 and v3 = finally v3 in
      [ G.Try (t, v1, v2, v3) |> G.s ]
  | ClassDef v1 ->
      let ent, def = class_def v1 in
      [ G.DefStmt (ent, G.ClassDef def) |> G.s ]
  | FuncDef v1 ->
      let ent, def = func_def v1 in
      [ G.DefStmt (ent, G.FuncDef def) |> G.s ]
  | ConstantDef v1 ->
      let ent, def = constant_def v1 in
      [ G.DefStmt (ent, G.VarDef def) |> G.s ]
  | TypeDef v1 ->
      let ent, def = type_def v1 in
      [ G.DefStmt (ent, G.TypeDef def) |> G.s ]
  | NamespaceDef (t, v1, (_t1, v2, t2)) ->
      let v1 = qualified_ident v1 and v2 = list stmt v2 in
      [ G.DirectiveStmt (G.Package (t, v1) |> G.d) |> G.s ]
      @ v2
      @ [ G.DirectiveStmt (G.PackageEnd t2 |> G.d) |> G.s ]
  | NamespaceUse (t, v1, v2) -> (
      let v1 = qualified_ident v1 in
      match v2 with
      | Some x ->
          [
            G.DirectiveStmt
              (G.ImportAs (t, G.DottedName v1, Some (alias x)) |> G.d)
            |> G.s;
          ]
      (* A use declaration such as `use A\B\C;` brings `C` into scope as `C` *)
      | None -> (
          match List.rev v1 with
          | name :: path ->
              [
                G.DirectiveStmt
                  (G.ImportFrom (t, G.DottedName (List.rev path), name, None)
                  |> G.d)
                |> G.s;
              ]
          | [] -> raise Impossible))
  | StaticVars (t, v1) ->
      v1
      |> list (fun (v1, v2) ->
             let v1 = var v1 and v2 = option expr v2 in
             let attrs = [ G.KeywordAttr (G.Static, t) ] in
             let ent = G.basic_entity v1 ~attrs in
             let def = { G.vinit = v2; vtype = None } in
             G.DefStmt (ent, G.VarDef def) |> G.s)
  | Global (t, v1) ->
      v1
      |> List.map (fun e ->
             match e with
             | Id [ id ] ->
                 let ent = G.basic_entity id in
                 G.DefStmt (ent, G.UseOuterDecl t) |> G.s
             | _ ->
                 let e = expr e in
                 G.OtherStmt (G.OS_GlobalComplex, [ G.E e ]) |> G.s)

and alias x =
  let x = ident x in
  (x, G.empty_id_info ())

and stmt x = G.stmt1 (stmt_aux x)

and opt_expr_to_label_ident = function
  | None -> G.LNone
  | Some e -> (
      match e with
      | Int (Some i, tok) -> G.LInt (i, tok)
      | Id [ label ] -> G.LId label
      | _ ->
          let e = expr e in
          G.LDynamic e)

and case = function
  | Case (t, v1, v2) ->
      let v1 = expr v1 and v2 = list stmt v2 in
      ([ G.Case (t, H.expr_to_pattern v1) ], G.stmt1 v2)
  | Default (t, v1) ->
      let v1 = list stmt v1 in
      ([ G.Default t ], G.stmt1 v1)

and catch (t, v1, v2, v3) =
  let v1 = hint_type v1 and v2 = var v2 and v3 = stmt v3 in
  let exn = G.CatchParam (G.param_of_type v1 ~pname:(Some v2)) in
  (t, exn, v3)

(* a list of finally??? php ... *)
and finally (v : finally list) =
  let xs = list (fun (t, xs) -> (t, stmt xs)) v in
  match xs with
  | [] -> None
  | (t, x) :: xs -> Some (t, G.stmt1 (x :: List.map snd xs))

and expr e : G.expr =
  (match e with
  | DeepEllipsis x -> G.DeepEllipsis (bracket expr x)
  | Ellipsis t -> G.Ellipsis t
  | Bool v1 ->
      let v1 = wrap id v1 in
      G.L (G.Bool v1)
  | Int v1 ->
      let v1 = wrap id v1 in
      G.L (G.Int v1)
  | Double v1 ->
      let v1 = wrap id v1 in
      G.L (G.Float v1)
  | String v1 ->
      let v1 = wrap string v1 in
      G.L (G.String v1)
  | Id v1 ->
      let v1 = name_of_qualified_ident v1 in
      G.N v1
  | IdSpecial v1 -> special v1
  (* unify Id and Var, finally *)
  | Var v1 ->
      let v1 = var v1 in
      G.N (G.Id (v1, G.empty_id_info ()))
  | Array_get (v1, (t1, Some v2, t2)) ->
      let v1 = expr v1 and v2 = expr v2 in
      G.ArrayAccess (v1, (t1, v2, t2))
  (* $var[] = ... used to be handled in the Assign caller, but there are still
   * other complex uses of $var[] in other contexts such as
   * $var[][] = ... where we must generate something.
   *)
  | Array_get (v1, (t1, None, _)) ->
      let v1 = expr v1 in
      G.OtherExpr (("ArrayAppend", t1), [ G.E v1 ])
  | Obj_get (v1, t, Id [ v2 ]) ->
      let v1 = expr v1 and v2 = ident v2 in
      G.DotAccess (v1, t, G.FN (G.Id (v2, G.empty_id_info ())))
  | Obj_get (v1, _tdot, Ellipsis tdots) ->
      let v1 = expr v1 in
      G.DotAccessEllipsis (v1, tdots)
  | Obj_get (v1, t, v2) ->
      let v1 = expr v1 and v2 = expr v2 in
      G.DotAccess (v1, t, G.FDynamic v2)
  | Class_get (v1, t, Id [ v2 ]) ->
      let v1 = expr v1 and v2 = ident v2 in
      G.DotAccess (v1, t, G.FN (G.Id (v2, G.empty_id_info ())))
  | Class_get (v1, t, v2) ->
      let v1 = expr v1 and v2 = expr v2 in
      G.DotAccess (v1, t, G.FDynamic v2)
  | New (v0, v1, v2) ->
      let v1 = expr v1 and v2 = list argument v2 in
      let t = H.expr_to_type v1 in
      G.New (v0, t, fb v2)
  | NewAnonClass (_tTODO, args, cdef) ->
      let _ent, cdef = class_def cdef in
      let args = list argument args in
      let anon_class = G.AnonClass cdef |> G.e in
      G.Call (anon_class, fb args)
  | InstanceOf (t, v1, v2) ->
      let v1 = expr v1 and v2 = expr v2 in
      G.Call
        (G.IdSpecial (G.Instanceof, t) |> G.e, fb ([ v1; v2 ] |> List.map G.arg))
  (* v[] = 1 --> v <append>= 1.
   * update: because we must generate an OE_ArrayAppend in other contexts,
   * this prevents the simple pattern '$x[]' to be matched in an Assign
   * context, hence the commented code below.
   *
   * | Assign ((Array_get(v1, (_, None, _)), t, v3)) ->
   *   let v1 = expr v1
   *   and v3 = expr v3
   *   in
   *   G.AssignOp (v1, (G.Append, t), v3)
   *
   * TODO: Some of those Assign are really VarDef. Do like in
   * Python_to_generic.ml
   *)
  | Assign (v1, t, v3) ->
      let v1 = expr v1 and v3 = expr v3 in
      G.Assign (v1, t, v3)
  | AssignOp (v1, v2, v3) -> (
      let v2 = binaryOp v2 and v1 = expr v1 and v3 = expr v3 in
      match v2 with
      | Left (op, t) -> G.AssignOp (v1, (op, t), v3)
      | Right (special, t) ->
          (* todo: should introduce intermediate var *)
          G.Assign
            ( v1,
              t,
              G.Call (G.IdSpecial (special, t) |> G.e, fb [ G.Arg v1; G.Arg v3 ])
              |> G.e ))
  | List v1 ->
      let v1 = bracket (list expr) v1 in
      G.Container (G.List, v1)
  | Arrow (v1, t, v2) ->
      let v1 = expr v1 and v2 = expr v2 in
      (G.keyval v1 t v2).e
  | Ref (t, v1) ->
      let v1 = expr v1 in
      G.Ref (t, v1)
  | Unpack v1 ->
      let v1 = expr v1 in
      G.Call
        (G.IdSpecial (G.Spread, fake "...") |> G.e, G.fake_bracket [ G.Arg v1 ])
  | Call (v1, v2) ->
      let v1 = expr v1 and v2 = bracket (list argument) v2 in
      G.Call (v1, v2)
  | Throw (t, v1) ->
      let v1 = expr v1 in
      let st = G.Throw (t, v1, G.sc) |> G.s in
      G.StmtExpr st
  | Infix ((v1, t), v2) ->
      let v1 = fixOp v1 and v2 = expr v2 in
      G.Call (G.IdSpecial (G.IncrDecr (v1, G.Prefix), t) |> G.e, fb [ G.Arg v2 ])
  | Postfix ((v1, t), v2) ->
      let v1 = fixOp v1 and v2 = expr v2 in
      G.Call
        (G.IdSpecial (G.IncrDecr (v1, G.Postfix), t) |> G.e, fb [ G.Arg v2 ])
  | Binop (v1, v2, v3) -> (
      let v2 = binaryOp v2 and v1 = expr v1 and v3 = expr v3 in
      match v2 with
      | Left (op, t) ->
          G.Call (G.IdSpecial (G.Op op, t) |> G.e, fb [ G.Arg v1; G.Arg v3 ])
      | Right x -> G.Call (G.IdSpecial x |> G.e, fb [ G.Arg v1; G.Arg v3 ]))
  | Unop ((v1, t), v2) ->
      let v1 = unaryOp v1 and v2 = expr v2 in
      G.Call (G.IdSpecial (G.Op (H.conv_op v1), t) |> G.e, fb [ G.Arg v2 ])
  | Guil (t, v1, _) ->
      let v1 = list expr v1 in
      G.Call
        ( G.IdSpecial (G.ConcatString G.InterpolatedConcat, t) |> G.e,
          fb (v1 |> List.map G.arg) )
  | ConsArray v1 ->
      let v1 = bracket (list array_value) v1 in
      G.Container (G.Array, v1)
  | CondExpr (v1, v2, v3) ->
      let v1 = expr v1 and v2 = expr v2 and v3 = expr v3 in
      G.Conditional (v1, v2, v3)
  | Cast (v1, v2) ->
      let v1 = ptype v1 and v2 = expr v2 in
      G.Cast (v1, fake ":", v2)
  | Lambda v1 -> (
      let tok = snd v1.f_name in
      match v1 with
      | {
       f_kind = lambdakind, t;
       f_ref = _;
       m_modifiers = _;
       f_name = _;
       l_uses;
       f_attrs = [];
       f_params = ps;
       f_return_type = rett;
       f_body = body;
      } ->
          let _lusesTODO =
            list
              (fun (v1, v2) ->
                let _v1 = bool v1 and _v2 = var v2 in
                ())
              l_uses
          in
          let lambdakind =
            match lambdakind with
            | AnonLambda -> G.LambdaKind
            | ShortLambda -> G.Arrow
            | _ -> error tok "unsupported lambda variant"
          in

          let body = stmt body in
          let ps = parameters ps in
          let rett = option hint_type rett in
          (* TODO: transform l_uses in UseOuterDecl preceding body *)
          G.Lambda
            {
              G.fparams = ps;
              frettype = rett;
              fbody = G.FBStmt body;
              fkind = (lambdakind, t);
            }
      | _ -> error tok "TODO: Lambda")
  | Match (tok, e, matches) ->
      let e = expr e in
      let matches = Common.map match_ matches in
      G.StmtExpr (G.Switch (tok, Some (G.Cond e), matches) |> G.s))
  |> G.e

and match_ = function
  | MCase (cases, e) ->
      let cases =
        Common.map
          (fun case ->
            let case = expr case in
            (* TODO extend G.case_of_pat_and_expr to handle multiple cases? *)
            G.Case (G.fake "case", H.expr_to_pattern case))
          cases
      in
      let e = expr e in
      G.CasesAndBody (cases, G.ExprStmt (e, G.sc) |> G.s)
  | MDefault (tok, e) ->
      let e = expr e in
      G.CasesAndBody ([ G.Default tok ], G.ExprStmt (e, G.sc) |> G.s)

and argument = function
  | Arg e -> expr e |> G.arg
  | ArgRef (tok, e) -> G.Ref (tok, expr e) |> G.e |> G.arg
  | ArgUnpack (tok, e) -> G.special (Spread, tok) [ expr e ] |> G.arg
  | ArgLabel (label, _tok, e) -> G.ArgKwd (label, expr e)

and special (spec, tok) =
  match spec with
  | This -> G.IdSpecial (G.This, tok)
  | Self -> G.IdSpecial (G.Self, tok)
  | Parent -> G.IdSpecial (G.Parent, tok)
  | FuncLike Empty -> G.N (G.Id (("empty", tok), G.empty_id_info ()))
  | FuncLike Eval -> G.IdSpecial (G.Eval, tok)
  | FuncLike Exit -> G.N (G.Id (("exit", tok), G.empty_id_info ()))
  | FuncLike Isset -> G.IdSpecial (G.Defined, tok)
  | FuncLike Unset -> G.N (G.Id (("unset", tok), G.empty_id_info ()))

and foreach_pattern v =
  let v = expr v in
  H.expr_to_pattern v

and array_value v = expr v

and hint_type = function
  | Hint v1 ->
      let v1 = name v1 in
      G.TyN (name_of_qualified_ident v1) |> G.t
  | HintArray t -> G.ty_builtin ("array", t)
  | HintQuestion (t, v1) ->
      let v1 = hint_type v1 in
      G.TyQuestion (v1, t) |> G.t
  | HintTuple (t1, v1, t2) ->
      let v1 = list hint_type v1 in
      G.TyTuple (t1, v1, t2) |> G.t
  | HintCallback (v1, v2) ->
      let v1 = list hint_type v1 and v2 = option hint_type v2 in
      let params = v1 |> List.map (fun x -> G.Param (G.param_of_type x)) in
      let fret =
        match v2 with
        | Some t -> t
        | None -> G.ty_builtin ("void", fake "void")
      in
      G.TyFun (params, fret) |> G.t
  | HintTypeConst (_, tok, _) ->
      G.OtherType (("HintTypeConst not supported, facebook-ext", tok), [])
      |> G.t
  | HintVariadic (tok, _) ->
      G.OtherType (("HintVariadic not supported", tok), []) |> G.t

and class_name v = hint_type v

and func_def
    {
      f_name;
      f_kind;
      f_params;
      f_return_type;
      f_ref;
      m_modifiers;
      l_uses;
      f_attrs;
      f_body;
    } =
  let id = ident f_name in
  let fkind = function_kind f_kind in
  let params = parameters f_params in
  let fret = option hint_type f_return_type in
  let _is_refTODO = bool f_ref in
  let modifiers =
    list modifier m_modifiers |> List.map (fun m -> G.KeywordAttr m)
  in
  (* todo: transform in UseOuterDecl before first body stmt *)
  let _lusesTODO =
    list
      (fun (v1, v2) ->
        let _v1 = bool v1 and _v2 = var v2 in
        ())
      l_uses
  in
  let attrs = list attribute f_attrs in
  let body = stmt f_body in
  let ent = G.basic_entity id ~attrs:(modifiers @ attrs) in
  let def =
    { G.fparams = params; frettype = fret; fbody = G.FBStmt body; fkind }
  in
  (ent, def)

and function_kind (kind, t) =
  ( (match kind with
    | Function -> G.Function
    | AnonLambda -> G.LambdaKind
    | ShortLambda -> G.Arrow
    | Method -> G.Method),
    t )

and parameters x = list parameter x

and parameter x =
  match x with
  | ParamClassic x -> parameter_classic x
  | ParamEllipsis t -> G.ParamEllipsis t

and parameter_classic { p_type; p_ref; p_name; p_default; p_attrs; p_variadic }
    =
  let p_type = option hint_type p_type in
  let p_name = var p_name in
  let p_default = option expr p_default in
  let p_attrs = list attribute p_attrs in
  let pclassic =
    {
      G.pname = Some p_name;
      ptype = p_type;
      pdefault = p_default;
      pattrs = p_attrs;
      pinfo = G.empty_id_info ();
    }
  in
  match (p_variadic, p_ref) with
  | None, None -> G.Param pclassic
  | _, Some tok -> G.OtherParam (("Ref", tok), [ G.Pa (G.Param pclassic) ])
  | Some tok, None -> G.ParamRest (tok, pclassic)

and modifier v = wrap modifierbis v

and attribute v =
  match v with
  | Id xs ->
      let name = name_of_qualified_ident xs in
      G.NamedAttr (fake "@", name, fb [])
  | Call (Id xs, args) ->
      let name = name_of_qualified_ident xs in
      let args = bracket (list argument) args in
      G.NamedAttr (fake "@", name, args)
  | _ -> raise Impossible

(* see ast_php_build.ml *)
and constant_def { cst_name; cst_body; cst_tok = tok } =
  let id = ident cst_name in
  let body = expr cst_body in
  let attr = [ G.KeywordAttr (G.Const, tok) ] in
  let ent = G.basic_entity id ~attrs:attr in
  (ent, { G.vinit = Some body; vtype = None })

and enum_type _tok { e_base; e_constraint } =
  let t = hint_type e_base in
  let _ = option hint_type e_constraint in
  t

and class_def
    {
      c_name;
      c_kind;
      c_modifiers;
      c_extends;
      c_implements;
      c_uses;
      c_enum_type;
      c_attrs;
      c_constants;
      c_variables;
      c_methods;
      c_braces = t1, (), t2;
    } =
  let tok = snd c_name in

  let id = ident c_name in
  let kind, class_attrs = class_kind c_kind in
  let extends = option class_parent c_extends in
  let implements = list class_name c_implements in
  let uses = list class_name c_uses in

  let _enum = option (enum_type tok) c_enum_type in

  let modifiers =
    list modifier c_modifiers |> List.map (fun m -> G.KeywordAttr m)
  in
  let attrs = list attribute c_attrs in

  let csts = list constant_def c_constants in
  let vars = list class_var c_variables in
  let methods = list method_def c_methods in

  let fields =
    (csts |> List.map (fun (ent, var) -> (ent, G.VarDef var)))
    @ (vars |> List.map (fun (ent, var) -> (ent, G.VarDef var)))
    @ (methods |> List.map (fun (ent, var) -> (ent, G.FuncDef var)))
  in

  let ent = G.basic_entity id ~attrs:(attrs @ modifiers @ class_attrs) in
  let def =
    {
      G.ckind = kind;
      cextends = extends |> Option.to_list;
      cimplements = implements;
      cmixins = uses;
      cparams = [];
      cbody = (t1, fields |> List.map (fun def -> G.fld def), t2);
    }
  in
  (ent, def)

and class_parent x : G.class_parent =
  let x = class_name x in
  (x, None)

and class_kind (x, t) =
  match x with
  | Class -> ((G.Class, t), [])
  | Interface -> ((G.Interface, t), [])
  | Trait -> ((G.Trait, t), [])
  | Enum -> ((G.Class, t), [ G.KeywordAttr (G.EnumClass, t) ])

and class_var
    {
      cv_name = cname;
      cv_type = ctype;
      cv_value = cvalue;
      cv_modifiers = cmodifiers;
    } =
  let id = var cname in
  let typ = option hint_type ctype in
  let value = option expr cvalue in
  let modifiers =
    list modifier cmodifiers |> List.map (fun m -> G.KeywordAttr m)
  in
  let ent = G.basic_entity id ~attrs:modifiers in
  let def = { G.vtype = typ; vinit = value } in
  (ent, def)

and method_def v = func_def v

and type_def { t_name; t_kind } =
  let id = ident t_name in
  let kind = type_def_kind (snd t_name) t_kind in
  let ent = G.basic_entity id in
  (ent, { G.tbody = kind })

and type_def_kind _tok = function
  | Alias v1 ->
      let v1 = hint_type v1 in
      G.AliasType v1

and program v = list stmt v

let partial v =
  match v with
  | PartialIf (t, e) ->
      let e = expr e in
      G.PartialIf (t, e)

let any = function
  | Program v1 ->
      let v1 = program v1 in
      G.Ss v1
  | Stmt v1 ->
      let v1 = stmt v1 in
      G.S v1
  | Expr2 v1 ->
      let v1 = expr v1 in
      G.E v1
  | Param v1 ->
      let v1 = parameter v1 in
      G.Pa v1
  | Partial v1 -> G.Partial (partial v1)
