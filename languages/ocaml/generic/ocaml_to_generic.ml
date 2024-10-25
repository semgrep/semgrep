(* Yoann Padioleau
 *
 * Copyright (C) 2019, 2020, 2024 Semgrep Inc.
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
open AST_ocaml
module G = AST_generic
module H = AST_generic_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Ast_ml to AST_generic.
 *
 * See AST_generic.ml for more information.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let id x = x
let option = Option.map
let list = List_.map
let string = id
let bool = id

(*
let _int = id
let _error = G.error
 *)

(* TODO: each use of this is usually the sign of a todo to improve
 * AST_generic.ml or ast_ml.ml *)
let fake = G.fake
let fb = Tok.unsafe_fake_bracket
let add_attrs ent attrs = { ent with G.attrs }

let mk_var_or_func tlet params tret body =
  (* coupling: with stmt() and what is generated for simple expressions *)
  match (params, body.G.s) with
  | [], G.OtherStmt (G.OS_ExprStmt2, [ G.E { e = G.Lambda def; _ } ]) ->
      G.FuncDef def
  | [], G.OtherStmt (G.OS_ExprStmt2, [ G.E e ]) ->
      G.VarDef { G.vinit = Some e; vtype = tret; vtok = G.no_sc }
  (* this is useful only for Ellipsis and DeepEllipsis for now, but
   * it could handle more later.
   *)
  | [], G.ExprStmt (x, sc) ->
      G.VarDef { G.vinit = Some x; vtype = tret; vtok = Some sc }
  | [], _st ->
      G.VarDef
        { G.vinit = Some (G.stmt_to_expr body); vtype = tret; vtok = G.no_sc }
  | _ :: _, _body ->
      G.FuncDef
        {
          G.fparams = fb params;
          frettype = tret;
          fkind = (G.Function, tlet);
          (* TODO? maybe generate FBExpr when we can? *)
          fbody = G.FBStmt body;
        }

let defs_of_bindings tlet attrs xs =
  xs
  |> list (function
       | Either.Left (ent, params, tret, body, lattrs) ->
           let ent = add_attrs ent (attrs @ lattrs) in
           G.DefStmt (ent, mk_var_or_func tlet params tret body) |> G.s
       | Either.Right (pat, e) ->
           (* TODO no attrs *)
           let exp = G.LetPattern (pat, e) |> G.e in
           G.exprstmt exp)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let info x = x
let tok v = info v

let wrap _of_a (v1, v2) =
  let v1 = _of_a v1 and v2 = info v2 in
  (v1, v2)

let bracket of_a (t1, x, t2) = (info t1, of_a x, info t2)

let rec ident v = wrap string v
and name (v1, v2) = H.name_of_ids (v1 @ [ v2 ])

and name_ (v1, v2) =
  let v1 = qualifier v1 and v2 = ident v2 in
  name (v1, v2)

and dotted_ident_of_name (v1, v2) =
  let v1 = qualifier v1 and v2 = ident v2 in
  v1 @ [ v2 ]

and module_name (v1, v2) =
  let v1 = qualifier v1 and v2 = ident v2 in
  v1 @ [ v2 ]

and qualifier v = list ident v
and todo_category v = ident v

and type_ x =
  let tk = type_kind x in
  tk |> G.t

and type_kind = function
  | TyEllipsis v1 ->
      let v1 = tok v1 in
      G.TyEllipsis v1
  | TyName v1 ->
      let v1 = name_ v1 in
      G.TyN v1
  | TyVar v1 ->
      let v1 = ident v1 in
      G.TyVar v1
  | TyAny v1 ->
      let v1 = tok v1 in
      G.TyAny v1
  | TyFunction (v1, v2) ->
      let v1 = type_ v1 and v2 = type_ v2 in
      G.TyFun ([ G.Param (G.param_of_type v1) ], v2)
  | TyApp (v1, v2) ->
      let l, v1, r = bracket (list type_) v1 and v2 = name v2 in
      G.TyApply (G.TyN v2 |> G.t, (l, v1 |> list (fun t -> G.TA t), r))
  | TyTuple v1 ->
      let v1 = list type_ v1 in
      G.TyTuple (fb v1)
  | TyTodo (t, v1) ->
      let t = todo_category t in
      let v1 = list type_ v1 in
      G.OtherType (t, list (fun x -> G.T x) v1)

and expr_body e : G.stmt = stmt e

and stmt e : G.stmt =
  match e with
  (* alt: Could be a G.Conditional, but then forced to define
   * an else branch which leads to some useless-else FP with ocamllint rules*)
  | If (t, v1, v2, v3) ->
      let v1 = expr v1
      and v2 = stmt v2
      and v3 =
        match v3 with
        | None -> None
        | Some x -> Some (stmt x)
      in
      G.If (t, G.Cond v1, v2, v3) |> G.s
  (* alt: could be a G.Seq expr *)
  | Sequence (l, v1, r) ->
      let v1 = list stmt v1 in
      G.Block (l, v1, r) |> G.s
  | Try (t, v1, v2) ->
      let v1 = stmt v1 and v2 = list match_case v2 in
      let catches =
        v2
        |> list (fun (pat, e) ->
               (fake "catch", G.CatchPattern pat, G.exprstmt e))
      in
      G.Try (t, v1, catches, None, None) |> G.s
  | While (t, v1, v2) ->
      let v1 = expr v1 and v2 = stmt v2 in
      G.While (t, G.Cond v1, v2) |> G.s
  | For (t, v1, v2, v3, v4, v5) ->
      let v1 = ident v1
      and v2 = expr v2
      and tok, nextop, condop = for_direction v3
      and v4 = expr v4
      and v5 = stmt v5 in
      let ent = G.basic_entity v1 in
      let var = { G.vinit = Some v2; vtype = None; vtok = G.no_sc } in
      let n = G.N (G.Id (v1, G.empty_id_info ())) |> G.e in
      let next =
        G.AssignOp (n, (nextop, tok), G.L (G.Int (Some 1L, tok)) |> G.e) |> G.e
      in
      let cond =
        G.Call (G.IdSpecial (G.Op condop, tok) |> G.e, fb [ G.Arg n; G.Arg v4 ])
        |> G.e
      in
      let header =
        G.ForClassic ([ G.ForInitVar (ent, var) ], Some cond, Some next)
      in
      G.For (t, header, v5) |> G.s
  | Match (t, v1, v2) ->
      let v1 = expr v1
      and v2 = list (fun a -> a |> match_case |> G.case_of_pat_and_expr) v2 in
      G.Switch (t, Some (G.Cond v1), v2) |> G.s
  | e -> (
      let e = expr e in
      (* bugfix: I was using 'G.exprstmt e' before, but then a pattern
       * like useless-else as 'if $E then $E1 else ()' will actually
       * match code like 'if foo then 1 else fail ()' because of
       * the special code in Generic_vs_generic around ExprStmt where
       * we use m_expr_deep to look deep for (), which in this case
       * will also match 'fail ()'. So better not use ExprStmt and
       * use a separate construct for now.
       * alt: disable ExprStmt magic for OCaml in Generic_vs_generic.
       *
       * update: there are cases though where we want to generate an
       * ExprStmt for Ellipsis otherwise Generic_vs_generic will not work.
       *)
      match e.G.e with
      | G.Ellipsis _
      | G.DeepEllipsis _ ->
          G.exprstmt e
      | _ -> G.OtherStmt (G.OS_ExprStmt2, [ G.E e ]) |> G.s)

and option_expr_to_ctor_arguments v =
  match v with
  | None -> fb []
  | Some (ParenExpr (l, Tuple xs, r)) ->
      let xs = list expr xs in
      (l, xs, r)
  (* we want to keep those ParenExpr in a Constructor context *)
  | Some (ParenExpr (l, e, r)) ->
      let e = expr e in
      (l, [ e ], r)
  | Some e ->
      let e = expr e in
      fb [ e ]

and expr e =
  match e with
  | LetOpen (tlet, n, _tin, e) ->
      let dotted = module_name n in
      let e = expr e in
      G.LocalImportAll (G.DottedName dotted, tlet, e) |> G.e
  | LocalOpen (n, tdot, e) ->
      let dotted = module_name n in
      let e = expr e in
      G.LocalImportAll (G.DottedName dotted, tdot, e) |> G.e
  | Obj { o_tok; o_body } ->
      let cdef =
        G.
          {
            ckind = (Object, o_tok);
            cextends = [];
            cimplements = [];
            cmixins = [];
            cparams = fb [];
            cbody = fb (list class_field o_body);
          }
      in
      G.AnonClass cdef |> G.e
  | ParenExpr (l, e, r) -> (
      let e = expr e in
      match e.G.e with
      (* replace fake brackets with real one *)
      | G.Container (G.Tuple, (_, xs, _)) ->
          G.Container (G.Tuple, (l, xs, r)) |> G.e
      | _kind ->
          AST_generic_helpers.set_e_range l r e;
          e)
  | TypedExpr (v1, v2, v3) -> (
      let v1 = expr v1 in
      let v2 = tok v2 in
      let v3 = type_ v3 in
      match v1.G.e with
      | G.N (G.Id (id, _idinfo)) when AST_generic.is_metavar_name (fst id) ->
          G.TypedMetavar (id, v2, v3) |> G.e
      | _ -> G.Cast (v3, v2, v1) |> G.e)
  | Ellipsis v1 ->
      let v1 = tok v1 in
      G.Ellipsis v1 |> G.e
  | DeepEllipsis (v1, v2, v3) ->
      let v1 = tok v1 in
      let v2 = expr v2 in
      let v3 = tok v3 in
      G.DeepEllipsis (v1, v2, v3) |> G.e
  | L v1 ->
      let v1 = literal v1 in
      G.L v1 |> G.e
  | Name v1 -> G.N (name v1) |> G.e
  | Constructor (v1, v2) ->
      let v1 = name v1 in
      let v2 = option_expr_to_ctor_arguments v2 in
      G.Constructor (v1, v2) |> G.e
  | PolyVariant ((v0, v1), v2) ->
      let v0 = tok v0 in
      let v1 = ident v1 in
      let v2 = option_expr_to_ctor_arguments v2 in
      let name = H.name_of_ids [ ("`", v0); v1 ] in
      (* TODO: introduce a new construct in AST_generic instead? *)
      G.Constructor (name, v2) |> G.e
  | Tuple v1 ->
      let v1 = (list expr) v1 in
      (* the fake brackets might be replaced in the caller if there
       * was a ParenExpr around
       *)
      G.Container (G.Tuple, fb v1) |> G.e
  | List v1 ->
      let v1 = bracket (list expr) v1 in
      G.Container (G.List, v1) |> G.e
  | Prefix (v1, v2) ->
      let v1 = wrap string v1 and v2 = expr v2 in
      G.Call (G.N (G.Id (v1, G.empty_id_info ())) |> G.e, fb [ G.Arg v2 ])
      |> G.e
  (* todo? convert some v2 in IdSpecial Op? *)
  | Infix (v1, v2, v3) ->
      let v1 = expr v1 and v3 = expr v3 in
      G.Call
        (G.N (G.Id (v2, G.empty_id_info ())) |> G.e, fb [ G.Arg v1; G.Arg v3 ])
      |> G.e
  | Call (v1, v2) ->
      let v1 = expr v1 and v2 = list argument v2 in
      G.Call (v1, fb v2) |> G.e
  | RefAccess (v1, v2) ->
      let v1 = tok v1 and v2 = expr v2 in
      G.DeRef (v1, v2) |> G.e
  | RefAssign (v1, v2, v3) ->
      let v1 = expr v1 and v2 = tok v2 and v3 = expr v3 in
      G.Assign (G.DeRef (v2, v1) |> G.e, v2, v3) |> G.e
  | FieldAccess (v1, vtok, v2) ->
      let v1 = expr v1 in
      let vtok = tok vtok in
      let v2 = name v2 in
      G.DotAccess (v1, vtok, G.FN v2) |> G.e
  | FieldAssign (v1, t1, v2, t2, v3) ->
      let v1 = expr v1 and v3 = expr v3 in
      let t1 = tok t1 in
      let t2 = tok t2 in
      let v2 = name v2 in
      G.Assign (G.DotAccess (v1, t1, G.FN v2) |> G.e, t2, v3) |> G.e
  | Record (v1, v2) -> (
      let v1 = option expr v1
      and v2 =
        bracket
          (list (fun (v1, v2) ->
               let v2 = expr v2 in
               match v1 with
               | [], id ->
                   let id = ident id in
                   G.basic_field id (Some v2) None
               | _ ->
                   let n = name v1 in
                   let ent = { G.name = G.EN n; attrs = []; tparams = None } in
                   let def =
                     G.VarDef
                       { G.vinit = Some v2; vtype = None; vtok = G.no_sc }
                   in
                   G.fld (ent, def)))
          v2
      in
      let obj = G.Record v2 |> G.e in
      match v1 with
      | None -> obj
      | Some e -> G.OtherExpr (("With", G.fake ""), [ G.E e; G.E obj ]) |> G.e)
  | New (v1, v2) ->
      let v1 = tok v1 and v2 = name v2 in
      G.New (v1, G.TyN v2 |> G.t, G.empty_id_info (), fb []) |> G.e
  | ObjAccess (v1, t, v2) ->
      let v1 = expr v1 and v2 = ident v2 in
      let t = tok t in
      G.DotAccess (v1, t, G.FN (G.Id (v2, G.empty_id_info ()))) |> G.e
  | LetIn (tlet, v1, v2, v3) ->
      let _v1 = rec_opt v1 in
      let v2 = list let_binding v2 in
      let v3 = expr v3 in
      let defs = defs_of_bindings tlet [] v2 in
      let st = G.Block (fb (defs @ [ G.exprstmt v3 ])) |> G.s in
      let x = G.stmt_to_expr st in
      x.G.e |> G.e
  | Fun (t, v1, v2) ->
      let v1 = list parameter v1 and v2 = expr v2 in
      let def =
        {
          G.fparams = fb v1;
          frettype = None;
          fkind = (G.Function, t);
          fbody = G.FBExpr v2;
        }
      in
      G.Lambda def |> G.e
  | Function (t, xs) ->
      let xs = list (fun a -> a |> match_case |> G.case_of_pat_and_expr) xs in
      let id = G.implicit_param_id t in
      let params = [ G.Param (G.param_of_id id) ] in
      let body_stmt =
        G.Switch
          (t, Some (G.Cond (G.N (AST_generic_helpers.name_of_id id) |> G.e)), xs)
        |> G.s
      in
      G.Lambda
        {
          G.fparams = fb params;
          frettype = None;
          fkind = (G.Function, t);
          fbody = G.FBStmt body_stmt;
        }
      |> G.e
  | ExprTodo (t, xs) ->
      let t = todo_category t in
      let xs = list expr xs in
      G.OtherExpr (t, list (fun x -> G.E x) xs) |> G.e
  | If _
  | Try _
  | For _
  | While _
  | Sequence _
  | Match _ ->
      let s = stmt e in
      let x = G.stmt_to_expr s in
      x.G.e |> G.e

and literal = function
  | Int v1 -> G.Int v1
  | Float v1 ->
      let v1 = wrap id v1 in
      G.Float v1
  | Char v1 ->
      let v1 = wrap string v1 in
      G.Char v1
  | String v1 ->
      let v1 = wrap string v1 in
      G.String (fb v1)
  | Bool v1 ->
      let v1 = wrap bool v1 in
      G.Bool v1
  | Unit (v1, v2) ->
      let v1 = tok v1 in
      let v2 = tok v2 in
      let t = Tok.combine_toks v1 [ v2 ] in
      G.Unit t

and argument = function
  | Arg v1 ->
      let v1 = expr v1 in
      G.Arg v1
  | ArgKwd (v1, v2) ->
      let v1 = ident v1 and v2 = expr v2 in
      G.ArgKwd (v1, v2)
  | ArgQuestion (v1, v2) ->
      let v1 = ident v1 and v2 = expr v2 in
      G.OtherArg (("ArgQuestion", snd v1), [ G.I v1; G.E v2 ])
  | ArgTodo t ->
      let t = todo_category t in
      G.OtherArg (t, [])

and match_case (v1, (v3, _t, v2)) : G.pattern * G.expr =
  let v1 = pattern v1 and v2 = expr v2 and v3 = option expr v3 in
  match v3 with
  | None -> (v1, v2)
  | Some x -> (G.PatWhen (v1, x), v2)

and for_direction = function
  | To v1 ->
      let v1 = tok v1 in
      (v1, G.Plus, G.LtE)
  | Downto v1 ->
      let v1 = tok v1 in
      (v1, G.Minus, G.GtE)

and rec_opt v =
  match v with
  | None -> []
  | Some t -> [ G.KeywordAttr (G.Recursive, t) ]

and pattern = function
  | PatEllipsis v1 ->
      let v1 = tok v1 in
      G.PatEllipsis v1
  | PatVar v1 ->
      let v1 = ident v1 in
      G.PatId (v1, G.empty_id_info ())
  | PatLiteral v1 ->
      let v1 = literal v1 in
      G.PatLiteral v1
  | PatConstructor (v1, v2) ->
      let v1 = name v1 and v2 = option pattern v2 in
      G.PatConstructor (v1, Option.to_list v2)
  | PatPolyVariant ((v0, v1), v2) ->
      let v0 = tok v0 in
      let v1 = ident v1 in
      let v2 = option pattern v2 in
      let name = H.name_of_ids [ ("`", v0); v1 ] in
      G.PatConstructor (name, Option.to_list v2)
  | PatConsInfix (v1, v2, v3) ->
      let v1 = pattern v1 and v2 = tok v2 and v3 = pattern v3 in
      let n = H.name_of_ids [ ("::", v2) ] in
      G.PatConstructor (n, [ v1; v3 ])
  | PatTuple v1 ->
      let v1 = bracket (list pattern) v1 in
      G.PatTuple v1
  | PatList v1 ->
      let v1 = bracket (list pattern) v1 in
      G.PatList v1
  | PatUnderscore v1 ->
      let v1 = tok v1 in
      G.PatWildcard v1
  | PatRecord v1 ->
      let v1 =
        bracket
          (list (fun (v1, v2) ->
               let v1 = dotted_ident_of_name v1 and v2 = pattern v2 in
               (v1, v2)))
          v1
      in
      G.PatRecord v1
  | PatAs (v1, v2) ->
      let v1 = pattern v1 and v2 = ident v2 in
      G.PatAs (v1, (v2, G.empty_id_info ()))
  | PatDisj (v1, v2) ->
      let v1 = pattern v1 and v2 = pattern v2 in
      G.PatDisj (v1, v2)
  | PatTyped (v1, _, v2) ->
      let v1 = pattern v1 and v2 = type_ v2 in
      G.PatTyped (v1, v2)
  | PatTodo (t, xs) ->
      let t = todo_category t in
      let xs = list pattern xs in
      G.OtherPat (t, list (fun x -> G.P x) xs)

and let_binding = function
  | LetClassic v1 ->
      let v1 = let_def v1 in
      Either.Left v1
  | LetPattern (v1, v2) -> (
      let v1 = pattern v1 and v2 = expr v2 in
      match v1 with
      | G.PatTyped (G.PatId (id, _idinfo), ty) ->
          let ent = G.basic_entity id in
          (match ent.G.name with
          | G.EN (G.Id (_, idinfo)) ->
              (* less: abusing id_type? Do we asume id_info is populated
               * by further static analysis (naming/typing)? But the info
               * is here, and this can be used in semgrep too to express
               * a form of TypedMetavar, so let's abuse it for now.
               *)
              idinfo.G.id_type := Some ty
          | _ -> raise Impossible);
          Either.Left (ent, [], None, G.exprstmt v2, [])
      | _ -> Either.Right (v1, v2))

and let_def { lname; lparams; lrettype; lbody; lattrs } =
  let v1 = ident lname in
  let v2 = list parameter lparams in
  let v3 = option type_ lrettype in
  let (v4 : G.stmt) = expr_body lbody in
  let v5 = attributes lattrs in
  let ent = G.basic_entity v1 in
  (ent, v2, v3, v4, v5)

and parameter = function
  | Param v -> (
      let v = pattern v in
      match v with
      | G.PatEllipsis t -> G.ParamEllipsis t
      | G.PatId (id, _idinfo) -> G.Param (G.param_of_id id)
      | G.PatTyped (G.PatId (id, _idinfo), ty) ->
          G.Param { (G.param_of_id id) with G.ptype = Some ty }
      | _ -> G.ParamPattern v)
  | ParamTodo x -> G.OtherParam (x, [])

and class_field (fld : class_field) : G.field =
  match fld with
  | Method { m_name; m_tok; m_params; m_rettype; m_body } ->
      let id = ident m_name in
      let ent = G.basic_entity id in
      let fparams = fb (list parameter m_params) in
      let frettype = option type_ m_rettype in
      let fbody =
        match m_body with
        | None -> G.FBNothing
        | Some e ->
            let e = expr e in
            G.FBExpr e
      in
      let fdef = G.{ fkind = (Method, m_tok); fparams; frettype; fbody } in
      G.fld (ent, G.FuncDef fdef)
  | InstanceVar { inst_tok = _; inst_name; inst_type; inst_expr } ->
      let id = ident inst_name in
      let ent = G.basic_entity id in
      let vinit = option expr inst_expr in
      let vtype = option type_ inst_type in
      let def = G.{ vinit; vtype; vtok = G.no_sc } in
      G.fld (ent, G.VarDef def)
  | CfldTodo todok ->
      let st = G.OtherStmt (G.OS_Todo, [ G.TodoK todok ]) |> G.s in
      G.F st

and type_declaration x =
  match x with
  | TyDecl { tname; tparams; tbody } ->
      let id = ident tname in
      let tparams = option (bracket (list type_parameter)) tparams in
      let tbody = type_def_kind tbody in
      let entity = { (G.basic_entity id) with G.tparams } in
      let def = { G.tbody } in
      Either.Left (entity, def)
  | TyDeclTodo categ -> Either.Right categ

and type_parameter v =
  match v with
  | TyParam v -> G.tparam_of_id (ident v)
  | TyParamTodo (s, t) -> G.OtherTypeParam ((s, t), [])

and type_def_kind = function
  | AbstractType -> G.AbstractType (fake "")
  | TdTodo categ -> G.OtherTypeKind (categ, [])
  | CoreType v1 ->
      let v1 = type_ v1 in
      G.AliasType v1
  | AlgebraicType v1 ->
      let v1 =
        list
          (fun (v1, v2) ->
            let v1 = ident v1 and v2 = list type_ v2 in
            G.OrConstructor (v1, v2))
          v1
      in
      G.OrType v1
  | RecordType v1 ->
      let v1 =
        bracket
          (list (fun (id, ty, mut_opt) ->
               let id = ident id
               and ty = type_ ty
               and mut_opt = option tok mut_opt in
               let ent =
                 G.basic_entity id
                   ~attrs:
                     (match mut_opt with
                     | Some tok -> [ G.attr G.Mutable tok ]
                     | None -> [])
               in
               G.fld
                 ( ent,
                   G.FieldDefColon
                     { G.vinit = None; vtype = Some ty; vtok = G.no_sc } )))
          v1
      in
      G.AndType v1

and module_declaration { mname; mbody } =
  let v1 = ident mname in
  let v2 = module_expr mbody in
  (G.basic_entity v1, { G.mbody = v2 })

and module_expr = function
  | ModuleName v1 ->
      let v1 = dotted_ident_of_name v1 in
      G.ModuleAlias v1
  | ModuleStruct v1 ->
      let v1 = list item v1 |> List_.flatten in
      G.ModuleStruct (None, v1)
  | ModuleTodo (t, xs) ->
      let t = todo_category t in
      let xs = list module_expr xs in
      G.OtherModule (t, list (fun x -> G.ModDk x) xs)

and attributes xs = list attribute xs

and attribute x =
  match x with
  | NamedAttr (t1, (dotted, xs), t2) ->
      let args =
        xs
        |> List_.filter_map (function
             | { i = TopExpr e; iattrs = [] } ->
                 let e = expr e in
                 Some (G.Arg e)
             | _ -> None)
      in
      let name = H.name_of_ids dotted in
      G.NamedAttr (t1, name, (t2, args, t2))

and class_binding (c_tok : Tok.t) (binding : class_binding) : G.definition =
  let { c_name; c_tparams; c_params; c_body } = binding in
  let id = ident c_name in
  let tparams = option (bracket (list type_parameter)) c_tparams in
  let ent = G.basic_entity ?tparams id in
  let cparams = fb (list parameter c_params) in
  let cbody =
    match c_body with
    | None -> fb []
    | Some x -> (
        match x with
        | ClTodo (todok, _xs) ->
            let st = G.OtherStmt (G.OS_Todo, [ G.TodoK todok ]) |> G.s in
            fb [ G.F st ]
        | ClObj { o_tok = _; o_body } ->
            let flds = list class_field o_body in
            fb flds)
  in
  let cdef =
    G.
      {
        ckind = (Class, c_tok);
        cextends = [];
        cimplements = [];
        cmixins = [];
        cparams;
        cbody;
      }
  in
  (ent, G.ClassDef cdef)

and item { i; iattrs } =
  let attrs = attributes iattrs in
  match i with
  | Class (c_tok, xs) ->
      xs
      |> list (fun x ->
             let def = class_binding c_tok x in
             G.DefStmt def |> G.s)
  | TopExpr e ->
      let e = expr e in
      [ G.exprstmt e ]
  | Type (_t, v1) ->
      let xs = list type_declaration v1 in
      xs
      |> List_.map (function
           | Either.Left (ent, def) ->
               (* add attrs to all mutual type decls *)
               let ent = add_attrs ent attrs in
               G.DefStmt (ent, G.TypeDef def) |> G.s
           | Either.Right categ ->
               G.OtherStmt (G.OS_Todo, [ G.TodoK categ ]) |> G.s)
  | Exception (_t, v1, v2) ->
      let v1 = ident v1 and v2 = list type_ v2 in
      let ent = G.basic_entity v1 ~attrs in
      let def = G.Exception (v1, v2) in
      [ G.DefStmt (ent, G.TypeDef { G.tbody = def }) |> G.s ]
  | External (texternal, v1, v2, v3) ->
      let id = ident v1 and ty = type_ v2 and _strs = list (wrap string) v3 in
      let attrs = [ G.KeywordAttr (G.Extern, texternal) ] @ attrs in
      let ent = G.basic_entity id ~attrs in
      let def = G.Signature { sig_tok = texternal; sig_type = ty } in
      [ G.DefStmt (ent, def) |> G.s ]
  | Open (t, v1) ->
      let v1 = module_name v1 in
      let dir =
        { G.d = G.ImportAll (t, G.DottedName v1, fake "*"); d_attrs = attrs }
      in
      [ G.DirectiveStmt dir |> G.s ]
  | Val (tval, v1, v2) ->
      let id = ident v1 and ty = type_ v2 in
      let ent = G.basic_entity id ~attrs in
      let def = G.Signature { sig_tok = tval; sig_type = ty } in
      [ G.DefStmt (ent, def) |> G.s ]
  | Let (tlet, v1, v2) ->
      let _v1 = rec_opt v1 and v2 = list let_binding v2 in
      let defs = defs_of_bindings tlet attrs v2 in
      defs
  | Module (_t, v1) ->
      let ent, def = module_declaration v1 in
      let ent = add_attrs ent attrs in
      [ G.DefStmt (ent, G.ModuleDef def) |> G.s ]
  | ItemTodo (t, xs) ->
      let t = todo_category t in
      let xs = list item xs |> List_.flatten in
      [
        G.OtherStmt
          ( G.OS_Todo,
            [ G.TodoK t ]
            @ List_.map (fun x -> G.S x) xs
            @ List_.map (fun x -> G.At x) attrs )
        |> G.s;
      ]

and program xs = List.concat_map item xs

and partial = function
  | PartialIf (t, e) ->
      let e = expr e in
      G.Partial (G.PartialIf (t, e))
  | PartialMatch (t, e) ->
      let e = expr e in
      G.Partial (G.PartialMatch (t, e))
  | PartialTry (t, e) ->
      let e = expr e in
      let res =
        match e.G.e with
        | G.StmtExpr s -> G.PartialTry (t, s)
        | _ -> G.PartialTry (t, G.exprstmt e)
      in
      G.Partial res
  | PartialLetIn (tlet, recopt, xs, _tinTODO) -> (
      let _recopt = rec_opt recopt in
      let xs = list let_binding xs in
      let defs = defs_of_bindings tlet [] xs in
      (* in Ast_ml those let are partials, but in the generic AST
       * we convert 'let x = a in b' in a sequence of VarDef and expr,
       * so those PartialLetIn are converted in a simple statement pattern
       *)
      match defs with
      | [] -> raise Impossible
      | [ x ] -> G.S x
      | xs -> G.Ss xs)

and any = function
  | E x -> (
      let x = expr x in
      match x.G.e with
      | G.StmtExpr s -> G.S s
      | _ -> G.E x)
  | I x -> (
      match item x with
      | [] -> raise Impossible
      | xs -> G.Ss xs)
  | T x ->
      let x = type_ x in
      G.T x
  | P x ->
      let x = pattern x in
      G.P x
  | MC mc ->
      let pat, e = match_case mc in
      let case = G.case_of_pat_and_expr (pat, e) in
      G.Partial (G.PartialSwitchCase case)
  | Id x ->
      let x = ident x in
      G.I x
  | Pr xs ->
      let xs = program xs in
      G.Ss xs
  | Partial x ->
      let x = partial x in
      x
