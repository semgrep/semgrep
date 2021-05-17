(* Yoann Padioleau
 *
 * Copyright (C) 2019, 2020 r2c
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
open Ast_ml
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

let option = Common.map_opt

let list = List.map

let string = id

let bool = id

let int = id

let error = G.error

(* TODO: each use of this is usually the sign of a todo to improve
 * AST_generic.ml or ast_ml.ml *)
let fake = G.fake

let add_attrs ent attrs = { ent with G.attrs }

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

and name_ (v1, v2) =
  let v1 = qualifier v1 and v2 = ident v2 in
  (v2, { G.empty_name_info with G.name_qualifier = Some (G.QDots v1) })

and name (v1, v2) = H.name_of_ids (v1 @ [ v2 ])

and dotted_ident_of_name (v1, v2) =
  let v1 = qualifier v1 and v2 = ident v2 in
  v1 @ [ v2 ]

and module_name (v1, v2) =
  let v1 = qualifier v1 and v2 = ident v2 in
  v1 @ [ v2 ]

and qualifier v = list ident v

and todo_category v = ident v

and type_ = function
  | TyEllipsis v1 ->
      let v1 = tok v1 in
      G.TyEllipsis v1
  (* TODO *)
  | TyName v1 ->
      let v1 = name_ v1 in
      G.TyN (G.IdQualified (v1, G.empty_id_info ()))
  | TyVar v1 ->
      let v1 = ident v1 in
      G.TyVar v1
  | TyAny v1 ->
      let v1 = tok v1 in
      G.TyAny v1
  | TyFunction (v1, v2) ->
      let v1 = type_ v1 and v2 = type_ v2 in
      G.TyFun ([ G.ParamClassic (G.param_of_type v1) ], v2)
  | TyApp (v1, v2) ->
      let v1 = list type_ v1 and v2 = dotted_ident_of_name v2 in
      G.TyNameApply (v2, v1 |> List.map (fun t -> G.TypeArg t))
  | TyTuple v1 ->
      let v1 = list type_ v1 in
      G.TyTuple (G.fake_bracket v1)
  | TyTodo (t, v1) ->
      let t = todo_category t in
      let v1 = list type_ v1 in
      G.OtherType (G.OT_Todo, G.TodoK t :: List.map (fun x -> G.T x) v1)

and expr_body e : G.stmt = stmt e

and stmt e : G.stmt =
  match e with
  (* alt: Could be a G.Conditional, but then forced to define
   * an else branch which leads to some useless-else FP with ocamllint rules*)
  | If (t, v1, v2, v3) ->
      let v1 = expr v1
      and v2 = stmt v2
      and v3 = match v3 with None -> None | Some x -> Some (stmt x) in
      G.If (t, v1, v2, v3) |> G.s
  (* alt: could be a G.Seq expr *)
  | Sequence v1 ->
      let v1 = list stmt v1 in
      G.Block (G.fake_bracket v1) |> G.s
  | Try (t, v1, v2) ->
      let v1 = stmt v1 and v2 = list match_case v2 in
      let catches =
        v2 |> List.map (fun (pat, e) -> (fake "catch", pat, G.exprstmt e))
      in
      G.Try (t, v1, catches, None) |> G.s
  | While (t, v1, v2) ->
      let v1 = expr v1 and v2 = stmt v2 in
      G.While (t, v1, v2) |> G.s
  | For (t, v1, v2, v3, v4, v5) ->
      let v1 = ident v1
      and v2 = expr v2
      and tok, nextop, condop = for_direction v3
      and v4 = expr v4
      and v5 = stmt v5 in
      let ent = G.basic_entity v1 [] in
      let var = { G.vinit = Some v2; vtype = None } in
      let n = G.N (G.Id (v1, G.empty_id_info ())) in
      let next = G.AssignOp (n, (nextop, tok), G.L (G.Int (Some 1, tok))) in
      let cond =
        G.Call
          (G.IdSpecial (G.Op condop, tok), G.fake_bracket [ G.Arg n; G.Arg v4 ])
      in
      let header =
        G.ForClassic ([ G.ForInitVar (ent, var) ], Some cond, Some next)
      in
      G.For (t, header, v5) |> G.s
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
      match e with
      | G.Ellipsis _ | G.DeepEllipsis _ -> G.exprstmt e
      | _ -> G.OtherStmt (G.OS_ExprStmt2, [ G.E e ]) |> G.s )

and expr e =
  match e with
  | TypedExpr (v1, v2, v3) -> (
      let v1 = expr v1 in
      let v3 = type_ v3 in
      match v1 with
      | G.N (G.Id (id, _idinfo)) when AST_generic_.is_metavar_name (fst id) ->
          G.TypedMetavar (id, v2, v3)
      | _ -> G.Cast (v3, v1) )
  | Ellipsis v1 ->
      let v1 = tok v1 in
      G.Ellipsis v1
  | DeepEllipsis (v1, v2, v3) ->
      let v1 = tok v1 in
      let v2 = expr v2 in
      let v3 = tok v3 in
      G.DeepEllipsis (v1, v2, v3)
  | L v1 ->
      let v1 = literal v1 in
      G.L v1
  | Name v1 -> G.N (name v1)
  | Constructor (v1, v2) ->
      let v1 = dotted_ident_of_name v1 and v2 = option expr v2 in
      G.Constructor (v1, Common.opt_to_list v2)
  | PolyVariant ((v0, v1), v2) ->
      let v0 = tok v0 in
      let v1 = ident v1 and v2 = option expr v2 in
      let dotted_ident = [ ("`", v0); v1 ] in
      (* TODO: introduce a new construct in AST_generic instead? *)
      G.Constructor (dotted_ident, Common.opt_to_list v2)
  | Tuple v1 ->
      let v1 = list expr v1 in
      G.Tuple (G.fake_bracket v1)
  | List v1 ->
      let v1 = bracket (list expr) v1 in
      G.Container (G.List, v1)
  | Prefix (v1, v2) ->
      let v1 = wrap string v1 and v2 = expr v2 in
      G.Call (G.N (G.Id (v1, G.empty_id_info ())), G.fake_bracket [ G.Arg v2 ])
  | Infix (v1, v2, v3) ->
      let v1 = expr v1 and v3 = expr v3 in
      G.Call
        ( G.N (G.Id (v2, G.empty_id_info ())),
          G.fake_bracket [ G.Arg v1; G.Arg v3 ] )
  | Call (v1, v2) ->
      let v1 = expr v1 and v2 = list argument v2 in
      G.Call (v1, G.fake_bracket v2)
  | RefAccess (v1, v2) ->
      let v1 = tok v1 and v2 = expr v2 in
      G.DeRef (v1, v2)
  | RefAssign (v1, v2, v3) ->
      let v1 = expr v1 and v2 = tok v2 and v3 = expr v3 in
      G.Assign (G.DeRef (v2, v1), v2, v3)
  | FieldAccess (v1, vtok, v2) ->
      let v1 = expr v1 in
      let vtok = tok vtok in
      let v2 = name v2 in
      G.DotAccess (v1, vtok, G.EN v2)
  | FieldAssign (v1, t1, v2, t2, v3) ->
      let v1 = expr v1 and v3 = expr v3 in
      let t1 = tok t1 in
      let t2 = tok t2 in
      let v2 = name v2 in
      G.Assign (G.DotAccess (v1, t1, G.EN v2), t2, v3)
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
                   let v1 = dotted_ident_of_name v1 in
                   let e =
                     G.OtherExpr (G.OE_RecordFieldName, [ G.Di v1; G.E v2 ])
                   in
                   let st = G.exprstmt e in
                   G.FieldStmt st))
          v2
      in
      let obj = G.Record v2 in
      match v1 with
      | None -> obj
      | Some e -> G.OtherExpr (G.OE_RecordWith, [ G.E e; G.E obj ]) )
  | New (v1, v2) ->
      let v1 = tok v1 and v2 = name v2 in
      G.Call (G.IdSpecial (G.New, v1), G.fake_bracket [ G.Arg (G.N v2) ])
  | ObjAccess (v1, t, v2) ->
      let v1 = expr v1 and v2 = ident v2 in
      let t = tok t in
      G.DotAccess (v1, t, G.EN (G.Id (v2, G.empty_id_info ())))
  | LetIn (tlet, v1, v2, v3) ->
      let _v1 = rec_opt v1 in
      let v2 = list let_binding v2 in
      let v3 = expr v3 in
      let defs =
        v2
        |> List.map (function
             | Left (ent, params, tret, body) ->
                 G.DefStmt (ent, mk_var_or_func tlet params tret body) |> G.s
             | Right (pat, e) ->
                 let exp = G.LetPattern (pat, e) in
                 G.exprstmt exp)
      in
      let st = G.Block (G.fake_bracket (defs @ [ G.exprstmt v3 ])) |> G.s in
      G.OtherExpr (G.OE_StmtExpr, [ G.S st ])
  | Fun (t, v1, v2) ->
      let v1 = list parameter v1 and v2 = expr v2 in
      let def =
        {
          G.fparams = v1;
          frettype = None;
          fkind = (G.Function, t);
          fbody = G.exprstmt v2;
        }
      in
      G.Lambda def
  | Function (t, xs) ->
      let xs = list match_case xs in
      let id = ("!_implicit_param!", t) in
      let params = [ G.ParamClassic (G.param_of_id id) ] in
      let body_exp = G.MatchPattern (G.N (G.Id (id, G.empty_id_info ())), xs) in
      let body_stmt = G.exprstmt body_exp in
      G.Lambda
        {
          G.fparams = params;
          frettype = None;
          fkind = (G.Function, t);
          fbody = body_stmt;
        }
  | Match (_t, v1, v2) ->
      let v1 = expr v1 and v2 = list match_case v2 in
      G.MatchPattern (v1, v2)
  | ExprTodo (t, xs) ->
      let t = todo_category t in
      let xs = list expr xs in
      G.OtherExpr (G.OE_Todo, G.TodoK t :: List.map (fun x -> G.E x) xs)
  | If _ | Try _ | For _ | While _ | Sequence _ ->
      let s = stmt e in
      G.OtherExpr (G.OE_StmtExpr, [ G.S s ])

and literal = function
  | Int v1 ->
      let v1 = wrap id v1 in
      G.Int v1
  | Float v1 ->
      let v1 = wrap id v1 in
      G.Float v1
  | Char v1 ->
      let v1 = wrap string v1 in
      G.Char v1
  | String v1 ->
      let v1 = wrap string v1 in
      G.String v1
  | Bool v1 ->
      let v1 = wrap bool v1 in
      G.Bool v1
  | Unit (v1, v2) ->
      let v1 = tok v1 in
      let v2 = tok v2 in
      let t = Parse_info.combine_infos v1 [ v2 ] in
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
      G.ArgOther (G.OA_ArgQuestion, [ G.I v1; G.E v2 ])

and match_case (v1, (v3, _t, v2)) =
  let v1 = pattern v1 and v2 = expr v2 and v3 = option expr v3 in
  match v3 with None -> (v1, v2) | Some x -> (G.PatWhen (v1, x), v2)

and for_direction = function
  | To v1 ->
      let v1 = tok v1 in
      (v1, G.Plus, G.LtE)
  | Downto v1 ->
      let v1 = tok v1 in
      (v1, G.Minus, G.GtE)

and rec_opt v =
  match v with None -> [] | Some t -> [ G.KeywordAttr (G.Recursive, t) ]

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
      let v1 = dotted_ident_of_name v1 and v2 = option pattern v2 in
      G.PatConstructor (v1, Common.opt_to_list v2)
  | PatPolyVariant ((v0, v1), v2) ->
      let v0 = tok v0 in
      let v1 = ident v1 in
      let v2 = option pattern v2 in
      let dotted_ident = [ ("`", v0); v1 ] in
      G.PatConstructor (dotted_ident, Common.opt_to_list v2)
  | PatConsInfix (v1, v2, v3) ->
      let v1 = pattern v1 and v2 = tok v2 and v3 = pattern v3 in
      let n = [ ("::", v2) ] in
      G.PatConstructor (n, [ v1; v3 ])
  | PatTuple v1 ->
      let v1 = list pattern v1 in
      G.PatTuple (G.fake_bracket v1)
  | PatList v1 ->
      let v1 = bracket (list pattern) v1 in
      G.PatList v1
  | PatUnderscore v1 ->
      let v1 = tok v1 in
      G.PatUnderscore v1
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
  | PatTyped (v1, v2) ->
      let v1 = pattern v1 and v2 = type_ v2 in
      G.PatTyped (v1, v2)
  | PatTodo (t, xs) ->
      let t = todo_category t in
      let xs = list pattern xs in
      G.OtherPat (G.OP_Todo, G.TodoK t :: List.map (fun x -> G.P x) xs)

and let_binding = function
  | LetClassic v1 ->
      let v1 = let_def v1 in
      Left v1
  | LetPattern (v1, v2) -> (
      let v1 = pattern v1 and v2 = expr v2 in
      match v1 with
      | G.PatTyped (G.PatId (id, _idinfo), ty) ->
          let ent = G.basic_entity id [] in
          ( match ent.G.name with
          | G.EN (G.Id (_, idinfo)) ->
              (* less: abusing id_type? Do we asume id_info is populated
               * by further static analysis (naming/typing)? But the info
               * is here, and this can be used in semgrep too to express
               * a form of TypedMetavar, so let's abuse it for now.
               *)
              idinfo.G.id_type := Some ty
          | _ -> raise Impossible );
          Left (ent, [], None, G.exprstmt v2)
      | _ -> Right (v1, v2) )

and let_def { lname; lparams; lrettype; lbody } =
  let v1 = ident lname in
  let v2 = list parameter lparams in
  let v3 = option type_ lrettype in
  let (v4 : G.stmt) = expr_body lbody in
  let ent = G.basic_entity v1 [] in
  (ent, v2, v3, v4)

and parameter = function
  | Param v -> (
      let v = pattern v in
      match v with
      | G.PatEllipsis t -> G.ParamEllipsis t
      | G.PatId (id, _idinfo) -> G.ParamClassic (G.param_of_id id)
      | G.PatTyped (G.PatId (id, _idinfo), ty) ->
          G.ParamClassic { (G.param_of_id id) with G.ptype = Some ty }
      | _ -> G.ParamPattern v )
  | ParamTodo t -> G.OtherParam (G.OPO_Todo, [ G.Tk t ])

and type_declaration { tname; tparams; tbody } =
  let v1 = ident tname in
  let v2 = list type_parameter tparams in
  let v3 = type_def_kind tbody in
  let entity = { (G.basic_entity v1 []) with G.tparams = v2 } in
  let def = { G.tbody = v3 } in
  (entity, def)

and type_parameter v = (ident v, [])

and type_def_kind = function
  | AbstractType -> G.OtherTypeKind (G.OTKO_AbstractType, [])
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
          (list (fun (v1, v2, v3) ->
               let v1 = ident v1 and v2 = type_ v2 and v3 = option tok v3 in
               let ent =
                 G.basic_entity v1
                   ( match v3 with
                   | Some tok -> [ G.attr G.Mutable tok ]
                   | None -> [] )
               in
               G.FieldStmt
                 ( G.DefStmt
                     (ent, G.FieldDefColon { G.vinit = None; vtype = Some v2 })
                 |> G.s )))
          v1
      in
      G.AndType v1

and module_declaration { mname; mbody } =
  let v1 = ident mname in
  let v2 = module_expr mbody in
  (G.basic_entity v1 [], { G.mbody = v2 })

and module_expr = function
  | ModuleName v1 ->
      let v1 = dotted_ident_of_name v1 in
      G.ModuleAlias v1
  | ModuleStruct v1 ->
      let v1 = list item v1 |> List.flatten in
      G.ModuleStruct (None, v1)
  | ModuleTodo (t, xs) ->
      let t = todo_category t in
      let xs = list module_expr xs in
      G.OtherModule (G.OMO_Todo, G.TodoK t :: List.map (fun x -> G.ModDk x) xs)

and attributes xs = list attribute xs

and attribute (t1, (dotted, xs), t2) =
  let args =
    xs
    |> Common.map_filter (function
         | { i = TopExpr e; iattrs = [] } ->
             let e = expr e in
             Some (G.Arg e)
         | _ -> None)
  in
  let name = H.name_of_ids dotted in
  G.NamedAttr (t1, name, (t2, args, t2))

and item { i; iattrs } =
  let attrs = attributes iattrs in
  match i with
  | TopExpr e ->
      let e = expr e in
      [ G.exprstmt e ]
  | Type (_t, v1) ->
      let xs = list type_declaration v1 in
      xs
      |> List.map (fun (ent, def) ->
             (* add attrs to all mutual type decls *)
             let ent = add_attrs ent attrs in
             G.DefStmt (ent, G.TypeDef def) |> G.s)
  | Exception (_t, v1, v2) ->
      let v1 = ident v1 and v2 = list type_ v2 in
      let ent = G.basic_entity v1 attrs in
      let def = G.Exception (v1, v2) in
      [ G.DefStmt (ent, G.TypeDef { G.tbody = def }) |> G.s ]
  | External (t, v1, v2, v3) ->
      let v1 = ident v1 and v2 = type_ v2 and _v3 = list (wrap string) v3 in
      let attrs = [ G.KeywordAttr (G.Extern, t) ] @ attrs in
      let ent = G.basic_entity v1 attrs in
      let def = G.Signature v2 in
      [ G.DefStmt (ent, def) |> G.s ]
  | Open (t, v1) ->
      let v1 = module_name v1 in
      (* no attrs here *)
      let dir = G.ImportAll (t, G.DottedName v1, fake "*") in
      [ G.DirectiveStmt dir |> G.s ]
  | Val (_t, v1, v2) ->
      let v1 = ident v1 and v2 = type_ v2 in
      let ent = G.basic_entity v1 attrs in
      let def = G.Signature v2 in
      [ G.DefStmt (ent, def) |> G.s ]
  | Let (tlet, v1, v2) ->
      let _v1 = rec_opt v1 and v2 = list let_binding v2 in
      v2
      |> List.map (function
           | Left (ent, params, tret, body) ->
               let ent = add_attrs ent attrs in
               G.DefStmt (ent, mk_var_or_func tlet params tret body) |> G.s
           | Right (pat, e) ->
               (* TODO no attrs *)
               let exp = G.LetPattern (pat, e) in
               G.exprstmt exp)
  | Module (_t, v1) ->
      let ent, def = module_declaration v1 in
      let ent = add_attrs ent attrs in
      [ G.DefStmt (ent, G.ModuleDef def) |> G.s ]
  | ItemTodo (t, xs) ->
      let t = todo_category t in
      let xs = list item xs |> List.flatten in
      [
        G.OtherStmt
          ( G.OS_Todo,
            [ G.TodoK t ]
            @ List.map (fun x -> G.S x) xs
            @ List.map (fun x -> G.At x) attrs )
        |> G.s;
      ]

and mk_var_or_func tlet params tret body =
  (* coupling: with stmt() and what is generated for simple expressions *)
  match (params, body.G.s) with
  | [], G.OtherStmt (G.OS_ExprStmt2, [ G.E (G.Lambda def) ]) -> G.FuncDef def
  | [], G.OtherStmt (G.OS_ExprStmt2, [ G.E e ]) ->
      G.VarDef { G.vinit = Some e; vtype = None }
  | _ ->
      G.FuncDef
        {
          G.fparams = params;
          frettype = tret;
          fkind = (G.Function, tlet);
          fbody = body;
        }

and program xs = List.map item xs |> List.flatten

and any = function
  | E x -> (
      let x = expr x in
      match x with
      | G.OtherExpr (G.OE_StmtExpr, [ G.S s ]) -> G.S s
      | _ -> G.E x )
  | I x -> (
      match item x with
      | [] -> raise Impossible
      | [ x ] -> G.S x
      | xs -> G.Ss xs )
  | T x ->
      let x = type_ x in
      G.T x
  | P x ->
      let x = pattern x in
      G.P x
  | Pr _x -> raise Todo
