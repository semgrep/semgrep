(* Yoann Padioleau
 *
 * Copyright (C) 2020 Semgrep Inc.
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
open Either_
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
let list = List_.map
let bool = id
let string = id

(* raise AST_generic.Error *)
let error = AST_generic.error
let fake = AST_generic.fake
let fb = Tok.unsafe_fake_bracket

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

(* Note, Funtions and Classes are case insensitive, variables are case
 * sensitive. *)
let name_of_qualified_ident ~case_insensitive xs =
  let xs = qualified_ident xs in
  H.name_of_ids ~case_insensitive xs

let name v = qualified_ident v
let fixOp x = x

let binaryOp (x, t) =
  match x with
  | BinaryConcat -> Left (G.Concat, t)
  | CombinedComparison -> Left (G.Cmp, t)
  | ArithOp op -> Left (op, t)

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

let for_var xs = xs |> List_.map (fun e -> G.ForInitExpr e)

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
      and v2 = list case v2 |> List_.map (fun x -> G.CasesAndBody x) in
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
      [ G.Try (t, v1, v2, None, v3) |> G.s ]
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
                  (G.ImportFrom
                     ( t,
                       G.DottedName (List.rev path),
                       [ H.mk_import_from_kind name None ] )
                  |> G.d)
                |> G.s;
              ]
          | [] -> raise Impossible))
  | StaticVars (t, v1) ->
      v1
      |> list (fun (v1, v2) ->
             let v1 = var v1 and v2 = option expr v2 in
             let attrs = [ G.KeywordAttr (G.Static, t) ] in
             let ent = G.basic_entity v1 ~case_insensitive:false ~attrs in
             let def = { G.vinit = v2; vtype = None; vtok = G.no_sc } in
             G.DefStmt (ent, G.VarDef def) |> G.s)
  | Global (t, v1) ->
      v1
      |> List_.map (fun e ->
             match e with
             | Id [ id ] ->
                 let ent = G.basic_entity ~case_insensitive:false id in
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
      | Int ((Some _, tok) as pi) -> (
          match Parsed_int.to_int_opt pi with
          | None ->
              let e = expr e in
              G.LDynamic e
          | Some i -> G.LInt (i, tok))
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
  let exn = G.CatchParam (G.param_of_type v1 ~pname:v2) in
  (t, exn, v3)

(* a list of finally??? php ... *)
and finally (v : finally list) =
  let xs = list (fun (t, xs) -> (t, stmt xs)) v in
  match xs with
  | [] -> None
  | (t, x) :: xs -> Some (t, G.stmt1 (x :: List_.map snd xs))

and expr e : G.expr =
  match e with
  | DeepEllipsis x -> G.DeepEllipsis (bracket expr x) |> G.e
  | Ellipsis t -> G.Ellipsis t |> G.e
  | Bool v1 ->
      let v1 = wrap id v1 in
      G.L (G.Bool v1) |> G.e
  | Int v1 -> G.L (G.Int v1) |> G.e
  | Double v1 ->
      let v1 = wrap id v1 in
      G.L (G.Float v1) |> G.e
  | String v1 ->
      let v1 = wrap string v1 in
      G.L (G.String (fb v1)) |> G.e
  | Id v1 ->
      let v1 = name_of_qualified_ident ~case_insensitive:true v1 in
      G.N v1 |> G.e
  | IdSpecial v1 -> special v1
  (* unify Id and Var, finally *)
  | Var v1 ->
      let v1 = var v1 in
      G.N (G.Id (v1, G.empty_id_info ())) |> G.e
  | Array_get (v1, (t1, Some v2, t2)) ->
      let v1 = expr v1 and v2 = expr v2 in
      G.ArrayAccess (v1, (t1, v2, t2)) |> G.e
  (* $var[] = ... used to be handled in the Assign caller, but there are still
   * other complex uses of $var[] in other contexts such as
   * $var[][] = ... where we must generate something.
   *)
  | Array_get (v1, (t1, None, _)) ->
      let v1 = expr v1 in
      G.OtherExpr (("ArrayAppend", t1), [ G.E v1 ]) |> G.e
  | Obj_get (v1, t, Id [ v2 ]) ->
      let v1 = expr v1 and v2 = ident v2 in
      G.DotAccess (v1, t, G.FN (G.Id (v2, G.empty_id_info ()))) |> G.e
  | Obj_get (v1, _tdot, Ellipsis tdots) ->
      let v1 = expr v1 in
      G.DotAccessEllipsis (v1, tdots) |> G.e
  | Obj_get (v1, t, v2) ->
      let v1 = expr v1 and v2 = expr v2 in
      G.DotAccess (v1, t, G.FDynamic v2) |> G.e
  | Class_get (v1, t, Id [ v2 ]) ->
      let v1 = expr v1 and v2 = ident v2 in
      G.DotAccess (v1, t, G.FN (G.Id (v2, G.empty_id_info ()))) |> G.e
  | Class_get (v1, t, v2) ->
      let v1 = expr v1 and v2 = expr v2 in
      G.DotAccess (v1, t, G.FDynamic v2) |> G.e
  | New (v0, v1, v2) ->
      let v1 = expr v1 and v2 = list argument v2 in
      let t = H.expr_to_type v1 in
      G.New (v0, t, G.empty_id_info (), fb v2) |> G.e
  | NewAnonClass (_tTODO, args, cdef) ->
      let _ent, cdef = class_def cdef in
      let args = list argument args in
      let anon_class = G.AnonClass cdef |> G.e in
      G.Call (anon_class, fb args) |> G.e
  | InstanceOf (t, v1, v2) ->
      let v1 = expr v1 and v2 = expr v2 in
      G.Call
        ( G.IdSpecial (G.Instanceof, t) |> G.e,
          fb ([ v1; v2 ] |> List_.map G.arg) )
      |> G.e
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
      G.Assign (v1, t, v3) |> G.e
  | AssignOp (v1, v2, v3) -> (
      let v2 = binaryOp v2 and v1 = expr v1 and v3 = expr v3 in
      match v2 with
      | Left (op, t) -> G.AssignOp (v1, (op, t), v3) |> G.e
      | Right (special, t) ->
          (* todo: should introduce intermediate var *)
          G.Assign
            ( v1,
              t,
              G.Call (G.IdSpecial (special, t) |> G.e, fb [ G.Arg v1; G.Arg v3 ])
              |> G.e )
          |> G.e)
  | List v1 ->
      let v1 = bracket (list expr) v1 in
      G.Container (G.List, v1) |> G.e
  | Arrow (v1, t, v2) ->
      let v1 = expr v1 and v2 = expr v2 in
      G.keyval v1 t v2
  | Ref (t, v1) ->
      let v1 = expr v1 in
      G.Ref (t, v1) |> G.e
  | Unpack v1 ->
      let v1 = expr v1 in
      G.Call
        ( G.IdSpecial (G.Spread, fake "...") |> G.e,
          Tok.unsafe_fake_bracket [ G.Arg v1 ] )
      |> G.e
  | Call (v1, v2) ->
      let v1 = expr v1 and v2 = bracket (list argument) v2 in
      G.Call (v1, v2) |> G.e
  | Throw (t, v1) ->
      let v1 = expr v1 in
      let st = G.Throw (t, v1, G.sc) |> G.s in
      G.StmtExpr st |> G.e
  | Infix ((v1, t), v2) ->
      let v1 = fixOp v1 and v2 = expr v2 in
      G.Call (G.IdSpecial (G.IncrDecr (v1, G.Prefix), t) |> G.e, fb [ G.Arg v2 ])
      |> G.e
  | Postfix ((v1, t), v2) ->
      let v1 = fixOp v1 and v2 = expr v2 in
      G.Call
        (G.IdSpecial (G.IncrDecr (v1, G.Postfix), t) |> G.e, fb [ G.Arg v2 ])
      |> G.e
  | Binop (v1, v2, v3) -> (
      let v2 = binaryOp v2 and v1 = expr v1 and v3 = expr v3 in
      match v2 with
      | Left (op, t) ->
          G.Call (G.IdSpecial (G.Op op, t) |> G.e, fb [ G.Arg v1; G.Arg v3 ])
          |> G.e
      | Right x ->
          G.Call (G.IdSpecial x |> G.e, fb [ G.Arg v1; G.Arg v3 ]) |> G.e)
  | Unop ((v1, t), v2) ->
      let v1 = unaryOp v1 and v2 = expr v2 in
      G.Call (G.IdSpecial (G.Op v1, t) |> G.e, fb [ G.Arg v2 ]) |> G.e
  | Guil (l, xs, r) -> (
      let xs = list expr xs in
      match xs with
      (* ugly: sgrep-ext: for foo("$VAR") we don't want to
       * generate an InterpolatedConcat because it's actually not an
       * interpolation ($VAR is interpreted as a metavar, not a PHP var)
       *)
      | [ ({ e = G.L (G.String (_, (str, _), _)); _ } as e) ]
        when AST_generic.is_metavar_name str ->
          e
      | _else_ ->
          let ys = xs |> List_.map (fun x -> Either_.Middle3 x) in
          G.interpolated (l, ys, r))
  | ConsArray v1 ->
      let v1 = bracket (list array_value) v1 in
      G.Container (G.Array, v1) |> G.e
  | CondExpr (v1, v2, v3) ->
      let v1 = expr v1 and v2 = expr v2 and v3 = expr v3 in
      G.Conditional (v1, v2, v3) |> G.e
  | Cast (v1, v2) ->
      let v1 = ptype v1 and v2 = expr v2 in
      G.Cast (v1, fake ":", v2) |> G.e
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
              G.fparams = fb ps;
              frettype = rett;
              fbody = G.FBStmt body;
              fkind = (lambdakind, t);
            }
          |> G.e
      | _ -> error tok "TODO: Lambda")
  | Match (tok, e, matches) ->
      let e = expr e in
      let matches = List_.map match_ matches in
      G.StmtExpr (G.Switch (tok, Some (G.Cond e), matches) |> G.s) |> G.e

and match_ = function
  | MCase (cases, e) ->
      let cases =
        List_.map
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
  | This -> G.IdSpecial (G.This, tok) |> G.e
  | Self -> G.IdSpecial (G.Self, tok) |> G.e
  | Parent -> G.IdSpecial (G.Parent, tok) |> G.e
  | FuncLike Empty -> G.N (G.Id (("empty", tok), G.empty_id_info ())) |> G.e
  | FuncLike Eval -> G.IdSpecial (G.Eval, tok) |> G.e
  | FuncLike Exit -> G.N (G.Id (("exit", tok), G.empty_id_info ())) |> G.e
  | FuncLike Isset -> G.IdSpecial (G.Defined, tok) |> G.e
  | FuncLike Unset -> G.N (G.Id (("unset", tok), G.empty_id_info ())) |> G.e

and foreach_pattern v =
  let v = expr v in
  H.expr_to_pattern v

and array_value v = expr v

and hint_type = function
  | Hint v1 ->
      let v1 = name v1 in
      G.TyN (name_of_qualified_ident ~case_insensitive:true v1) |> G.t
  | HintArray t -> G.ty_builtin ("array", t)
  | HintQuestion (t, v1) ->
      let v1 = hint_type v1 in
      G.TyQuestion (v1, t) |> G.t
  | HintTuple (t1, v1, t2) ->
      let v1 = list hint_type v1 in
      G.TyTuple (t1, v1, t2) |> G.t
  | HintCallback (v1, v2) ->
      let v1 = list hint_type v1 and v2 = option hint_type v2 in
      let params = v1 |> List_.map (fun x -> G.Param (G.param_of_type x)) in
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
    list modifier m_modifiers |> List_.map (fun m -> G.KeywordAttr m)
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
  let ent =
    G.basic_entity id ~attrs:(modifiers @ attrs) ~case_insensitive:true
  in
  let def =
    { G.fparams = fb params; frettype = fret; fbody = G.FBStmt body; fkind }
  in
  (ent, def)

and function_kind (kind, t) =
  ( (match kind with
    | Function -> G.Function
    | AnonLambda -> G.LambdaKind
    | ShortLambda -> G.Arrow
    | Method -> G.Method),
    t )

and parameters x : G.parameter list = list parameter x

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

(* TODO: attributes are probably case-insensitive because they refer
   to class names. This need to be verified. *)
and attribute v =
  match v with
  | Id xs ->
      let name = name_of_qualified_ident ~case_insensitive:true xs in
      G.NamedAttr (fake "@", name, fb [])
  | Call (Id xs, args) ->
      let name = name_of_qualified_ident ~case_insensitive:true xs in
      let args = bracket (list argument) args in
      G.NamedAttr (fake "@", name, args)
  | _ -> raise Impossible

(* see ast_php_build.ml *)
and constant_def { cst_name; cst_body; cst_tok = tok } =
  let id = ident cst_name in
  let body = expr cst_body in
  let attr = [ G.KeywordAttr (G.Const, tok) ] in
  let ent = G.basic_entity id ~case_insensitive:false ~attrs:attr in
  (ent, { G.vinit = Some body; vtype = None; vtok = G.no_sc })

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
    list modifier c_modifiers |> List_.map (fun m -> G.KeywordAttr m)
  in
  let attrs = list attribute c_attrs in

  let csts = list constant_def c_constants in
  let vars = list class_var c_variables in
  let methods = list method_def c_methods in

  let fields =
    (csts |> List_.map (fun (ent, var) -> (ent, G.VarDef var)))
    @ (vars |> List_.map (fun (ent, var) -> (ent, G.VarDef var)))
    @ (methods |> List_.map (fun (ent, var) -> (ent, G.FuncDef var)))
  in

  let ent =
    G.basic_entity id
      ~attrs:(attrs @ modifiers @ class_attrs)
      ~case_insensitive:true
  in
  let def =
    {
      G.ckind = kind;
      cextends = extends |> Option.to_list;
      cimplements = implements;
      cmixins = uses;
      cparams = fb [];
      cbody = (t1, fields |> List_.map (fun def -> G.fld def), t2);
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
    list modifier cmodifiers |> List_.map (fun m -> G.KeywordAttr m)
  in
  let ent = G.basic_entity id ~case_insensitive:false ~attrs:modifiers in
  let def = { G.vtype = typ; vinit = value; vtok = G.no_sc } in
  (ent, def)

and method_def v = func_def v

and type_def { t_name; t_kind } =
  let id = ident t_name in
  let kind = type_def_kind (snd t_name) t_kind in
  let ent = G.basic_entity ~case_insensitive:true id in
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
