(* Brandon Wu
 *
 * Copyright (C) 2019-2024 r2c
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
open AST_ql
module G = AST_generic
module H = AST_generic_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* AST_ql to AST_generic.
 *
 * See AST_generic.ml for more information.
 *
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let id x = x
let option = Option.map
let list = List_.map
let string = id
let bool = id
let unsafe_fake s = Tok.unsafe_fake_tok s
let fb = Tok.unsafe_fake_bracket

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let info x = x

let wrap of_a (v1, v2) =
  let v1 = of_a v1 and v2 = info v2 in
  (v1, v2)

let bracket of_a (t1, x, t2) = (info t1, of_a x, info t2)
let ident x = x

let rec name = function
  | Id id -> G.Id (ident id, G.empty_id_info ())
  | IdQualified x -> G.IdQualified (qualified_info x)

and qualified_info { name_last = v1, v2; name_middle } =
  let name_last = (ident v1, option type_arguments v2) in
  let name_middle =
    Some
      (G.QDots
         (list (fun (x, y) -> (ident x, option type_arguments y)) name_middle))
  in
  G.{ name_last; name_middle; name_top = None; name_info = G.empty_id_info () }

and type_argument (v1, pi_opt) =
  let v1 = TyN (name v1) |> G.t in
  match pi_opt with
  | None -> G.TA v1
  | Some ((_, t) as pi) ->
      G.OtherTypeArg
        (("TypeArgWithArity", t), [ G.T v1; G.E (G.L (G.Int pi) |> G.e) ])

and type_arguments x = Tok.unsafe_fake_bracket (list type_argument x)
and type_ x = G.TyN (name x) |> G.t
and name_of_type_ x = name x

and module_definition (_tok, id, _typarams, rhs) =
  let ent = G.basic_entity (ident id) in
  let def =
    match rhs with
    | ModuleBody stmts -> G.ModuleStruct (None, list stmt stmts)
    | ModuleAlias qual_info ->
        G.ModuleAlias
          (IdQualified (qualified_info qual_info) |> H.dotted_ident_of_name)
  in
  DefStmt (ent, ModuleDef { mbody = def }) |> G.s

and param_of_vardecl = function
  | VardeclInit (ty, id) ->
      let ty = type_ ty in
      let id = ident id in
      G.Param (G.param_of_id ~ptype:ty id)
  | VardeclEllipsis tok -> G.ParamEllipsis tok

and params_any_of_vardecls vardecls =
  G.Params (List_.map param_of_vardecl vardecls)

and predicate_definition (v1, v2, v3, v4) =
  let v1 = option type_ v1 in
  let ent = G.basic_entity (ident v2) in
  let fparams = List_.map param_of_vardecl v3 |> fb in
  let body =
    match v4 with
    | PredicateExpr None -> G.FBNothing
    | PredicateExpr (Some e) -> G.FBExpr (expr e)
    | PredicateAlias (qual_info, pi) ->
        G.FBExpr
          (G.OtherExpr
             ( ("PredicateAlias", snd pi),
               [ G.Ta (type_argument (IdQualified qual_info, Some pi)) ] )
          |> G.e)
  in
  G.DefStmt
    ( ent,
      FuncDef
        {
          fkind = (Function, unsafe_fake "");
          fparams;
          frettype = v1;
          fbody = body;
        } )
  |> G.s

and literal = function
  | Bool v1 ->
      let v1 = wrap bool v1 in
      G.L (G.Bool v1) |> G.e
  | Int v1 -> G.L (G.Int v1) |> G.e
  | String v1 ->
      let v1 = wrap string v1 in
      G.L (G.String (fb v1)) |> G.e
  | Float v1 -> G.L (G.Float v1) |> G.e

and direction (x, y) =
  match x with
  | Asc -> G.N (H.name_of_id ("asc", y)) |> G.e
  | Desc -> G.N (H.name_of_id ("desc", y)) |> G.e

and quantifier (x, y) =
  match x with
  | Forall -> G.N (H.name_of_id ("forall", y)) |> G.e
  | Exists -> G.N (H.name_of_id ("exists", y)) |> G.e
  | Forex -> G.N (H.name_of_id ("forex", y)) |> G.e

and expr (x : expr) =
  match x with
  | L x -> literal x
  | N x -> G.N (name x) |> G.e
  | IdSpecial x -> special x
  | Call (v1, _tyargs, v2) ->
      let v1 = expr v1 in
      let v2 = bracket (list argument) v2 in
      G.Call (v1, v2) |> G.e
  | Aggregation
      {
        expr = e;
        rank_exprs;
        body = l, { vardecls; formula; as_exprs; agg_orderbys }, r;
      } ->
      let rank_exprs = List_.map (fun x -> G.E (expr x)) rank_exprs in
      let formula =
        match formula with
        | None -> []
        | Some x -> [ G.Arg (expr x) ]
      in
      let as_exprs =
        List_.map
          (fun (x, y) ->
            match y with
            | None -> G.E (expr x)
            | Some y -> G.Anys [ G.E (expr x); G.I (ident y) ])
          as_exprs
      in
      let orderbys =
        List_.map
          (fun (x, y) ->
            let direction =
              match y with
              | None -> []
              | Some dir -> [ G.E (direction dir) ]
            in
            G.Anys ([ G.E (expr x) ] @ direction))
          agg_orderbys
      in
      G.Call
        ( expr e,
          ( l,
            [
              G.OtherArg (("RankExprs", unsafe_fake ""), rank_exprs);
              (* This is not _quite_ right.
                 An aggregation is really an expression which is introducing a
                 bunch of variables to range over, and returning a result. These
                 vardecls aren't _arguments_, they're _parameters_.
                 But, we don't really have constructs other than lambdas, functions, which
                 behave like this. So this is as close as we can get. *)
              G.OtherArg
                ( ("AggregateVardecls", unsafe_fake ""),
                  [ params_any_of_vardecls vardecls ] );
            ]
            @ formula
            @ [
                G.OtherArg (("AsExprs", unsafe_fake ""), as_exprs);
                G.OtherArg (("AggOrderBys", unsafe_fake ""), orderbys);
              ],
            r ) )
      |> G.e
  | Cast (v1, v2) ->
      let v1 = type_ v1 and v2 = expr v2 in
      G.Cast (v1, unsafe_fake "", v2) |> G.e
  | IfExp (v1, v2, v3) ->
      let v1 = expr v1 and v2 = expr v2 and v3 = expr v3 in
      G.Conditional (v1, v2, v3) |> G.e
  | BinOp (v1, (v2, tok), v3) ->
      let v1 = expr v1 and v2 = operator v2 and v3 = expr v3 in
      G.Call
        (G.IdSpecial (G.Op v2, tok) |> G.e, fb ([ v1; v3 ] |> List_.map G.arg))
      |> G.e
  | UnaryOp ((v1, tok), v2) ->
      let op = unaryop v1 and v2 = expr v2 in
      G.opcall (op, tok) [ v2 ]
  | Quantified (quant, (l, body, r)) ->
      let body =
        match body with
        | Bare e -> [ G.Arg (expr e) ]
        | Declared (vardecls, es) ->
            let vardecls_arg =
              G.OtherArg
                ( ("QuantifiedVardecls", unsafe_fake ""),
                  [ params_any_of_vardecls vardecls ] )
            in
            let other_args =
              match es with
              | None -> []
              | Some (e, None) -> [ G.Arg (expr e) ]
              | Some (e, Some e2) -> [ G.Arg (expr e); G.Arg (expr e2) ]
            in
            vardecls_arg :: other_args
      in
      G.Call (quantifier quant, (l, body, r)) |> G.e
  | Range (e1, tk, e2) -> G.opcall (Range, tk) [ expr e1; expr e2 ]
  | Set v1 ->
      let v1 = bracket (list expr) v1 in
      G.Container (G.Set, v1) |> G.e
  | ParenExpr (l, e, r) ->
      let e = expr e in
      H.set_e_range l r e;
      e
  | AnnotatedExpr (id1, id2, e) ->
      G.OtherExpr
        ( ("AnnotatedExpr", unsafe_fake ""),
          [ G.I (ident id1); G.I (ident id2); G.E (expr e) ] )
      |> G.e
  | DotAccess (v1, v2, v3) ->
      let lhs = expr v1 in
      let field =
        match v3 with
        | RhsPredicate (id, args) ->
            G.FDynamic
              (G.Call (N (name (Id id)) |> G.e, fb (arguments args)) |> G.e)
        | RhsCast ty ->
            G.FDynamic
              (G.OtherExpr (("RhsCast", unsafe_fake ""), [ G.T (type_ ty) ])
              |> G.e)
        | RhsSuper tok -> G.FDynamic (G.IdSpecial (G.Super, tok) |> G.e)
      in
      G.DotAccess (lhs, v2, field) |> G.e
  | Ellipsis x ->
      let x = info x in
      G.Ellipsis x |> G.e
  | MetavarEllipsis id -> G.N (H.name_of_id (ident id)) |> G.e
  | TypedMetavar (v1, v3) ->
      let v1 = type_ v1 in
      let v3 = ident v3 in
      G.TypedMetavar (v3, unsafe_fake "", v1) |> G.e
  | DeepEllipsis (v1, v2, v3) -> G.DeepEllipsis (v1, expr v2, v3) |> G.e

and argument = function
  | Arg x -> G.Arg (expr x)
  | ArgUnderscore tok -> G.OtherArg (("ArgUnderscore", tok), [])

and special (x, tk) =
  match x with
  | This -> G.IdSpecial (G.This, tk) |> G.e
  | Result -> G.N (H.name_of_id ("result", tk)) |> G.e
  | NoneId -> G.N (H.name_of_id ("none", tk)) |> G.e
  | Not -> G.IdSpecial (G.Op G.Not, tk) |> G.e

and arguments x = list argument x

and unaryop = function
  | UAdd -> G.Plus
  | USub -> G.Minus

and operator = function
  | In -> G.In
  | InstanceOf -> G.Is
  (* logical *)
  | And -> G.And
  | Or -> G.Or
  (* This is not a CMP, but there's nowhere else to put it.
     It works, because no where else should this really be used, so whatever.
  *)
  | Implies -> G.Cmp
  (* arithmetic *)
  | Add -> G.Plus
  | Sub -> G.Minus
  | Mult -> G.Mult
  | Div -> G.Div
  | Mod -> G.Mod
  (* comparison *)
  | Eq -> G.Eq
  | NotEq -> G.NotEq
  | Lt -> G.Lt
  | Gt -> G.Gt
  | LtEq -> G.LtE
  | GtEq -> G.GtE

and class_definition (v1, v2, v3) =
  let ent = G.basic_entity (ident v2) in
  match v3 with
  | ClassBody (extends, instancesof, stmts) ->
      G.DefStmt
        ( ent,
          ClassDef
            {
              G.ckind = (G.Class, v1);
              cextends = List_.map (fun x -> (type_ x, None)) extends;
              cimplements = list type_ instancesof;
              cbody = bracket (list (fun x -> G.F (stmt x))) stmts;
              cparams = fb [];
              cmixins = [];
            } )
      |> G.s
  | ClassAlias v1 ->
      let rhs = name_of_type_ v1 in
      DefStmt
        ( ent,
          VarDef { vinit = Some (G.N rhs |> G.e); vtype = None; vtok = None } )
      |> G.s

and select { from; where; select; sel_orderbys } =
  let from =
    match from with
    | None -> G.Anys []
    | Some (_tk, vardecls) -> params_any_of_vardecls vardecls
  in
  let where =
    match where with
    | None -> G.Anys []
    | Some e -> G.Anys [ G.E (expr e) ]
  in
  let select =
    G.Anys
      (List_.map
         (fun (e, i) ->
           let e = G.E (expr e) in
           match i with
           | None -> e
           | Some i -> G.Anys [ e; G.I (ident i) ])
         select)
  in
  let orderbys =
    G.Anys
      (List_.map
         (fun (x, y) ->
           let e = G.E (expr x) in
           match y with
           | None -> e
           | Some dir -> G.Anys [ e; G.E (direction dir) ])
         sel_orderbys)
  in
  G.OtherStmt (OS_Todo, [ from; where; select; orderbys ]) |> G.s

and vardecl = function
  | VardeclInit (v1, v2) ->
      let v1 = type_ v1 and v2 = ident v2 in
      G.DefStmt
        ( G.basic_entity v2,
          G.VarDef { vinit = None; vtype = Some v1; vtok = None } )
      |> G.s
  | VardeclEllipsis tok -> G.exprstmt (G.Ellipsis tok |> G.e)

and stmt = function
  | ClassDef v1 -> class_definition v1
  | ModuleDef v1 -> module_definition v1
  | PredicateDef v1 -> predicate_definition v1
  | VarDecl x -> vardecl x
  | NewType (v1, v2, v3) ->
      (* This is really a spicy kind of OrType, but with the constraint that certain constructors
         must fulfill a predicate on its input values to exist.

         Sort of like being able to write
         type my_type =
           | A of (a : int) when a > 0
           | B of (b : int list) when List.length b > 10
         in OCaml.

         The regular OrType doesn't have space for this, because we need to put the idents for every
         argument to the constructor, and also the predicate on the constructor.

         So let's just Other out. Why not.
      *)
      let ent = G.basic_entity (ident v2) in
      let elems =
        List_.map
          (fun (id, vardecls, expr_opt) ->
            let expr_opt =
              match expr_opt with
              | None -> []
              | Some e -> [ G.E (expr e) ]
            in
            G.Anys
              ([ G.I (ident id); params_any_of_vardecls vardecls ] @ expr_opt))
          v3
      in
      G.DefStmt
        (ent, G.TypeDef { tbody = OtherTypeKind (("CodeQLOrType", v1), elems) })
      |> G.s
  | TypeUnion (tok, v1, v2) ->
      (* A "type union" essentially lets you define a subset of a pre-existing algebraic datatype,
         by selecting some of the existing constructors.
         OrType is again inconvenient here, so we Other out.
      *)
      let ent = G.basic_entity (ident v1) in
      let elems = List_.map (fun x -> G.E (G.N (name_of_type_ x) |> G.e)) v2 in
      G.DefStmt
        ( ent,
          G.TypeDef { tbody = OtherTypeKind (("CodeQLTypeUnion", tok), elems) }
        )
      |> G.s
  | ImportAs (v1, v2, v3) ->
      let alias =
        match v3 with
        | None -> None
        | Some id -> Some (ident id, G.empty_id_info ())
      in
      DirectiveStmt
        (ImportAs (v1, DottedName (H.dotted_ident_of_name (name v2)), alias)
        |> G.d)
      |> G.s
  | Select v1 -> select v1
  | ExprStmt v1 -> G.exprstmt (expr v1)

let program v1 = list stmt v1

let any = function
  | Pr stmts -> G.Pr (list stmt stmts)
  | E e -> G.E (expr e)
