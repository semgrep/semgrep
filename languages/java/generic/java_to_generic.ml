(* Yoann Padioleau
 *
 * Copyright (C) 2019 Semgrep Inc.
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
open Ast_java
module G = AST_generic
module H = AST_generic_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Ast_java to AST_generic.
 *
 * See ast_generic.ml for more information.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let id x = x
let option = Option.map
let list = List_.map
let (string : string -> string) = id
let (bool : bool -> bool) = id
let (int : int -> int) = id
let error = AST_generic.error
let fake tok s = Tok.fake_tok tok s
let unsafe_fake s = Tok.unsafe_fake_tok s

(* todo: to remove at some point when Ast_java includes them directly *)
let fb = Tok.unsafe_fake_bracket

let id_of_entname = function
  | G.EN (Id (id, idinfo)) -> (id, idinfo)
  | G.EN _
  | G.EDynamic _
  | EPattern _
  | OtherEntity _ ->
      raise Impossible

let entity_to_param { G.name; attrs; tparams = _unused } t =
  let id, info = id_of_entname name in
  {
    G.pname = Some id;
    ptype = t;
    pattrs = attrs;
    pinfo = info;
    pdefault = None;
  }

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let info x = x
let tok v = info v

let wrap _of_a (v1, v2) =
  let v1 = _of_a v1 and v2 = info v2 in
  (v1, v2)

let bracket of_a (t1, x, t2) = (info t1, of_a x, info t2)
let list1 _of_a = list _of_a
let ident v = wrap string v
let qualified_ident v = list ident v

let rec typ = function
  | TBasic v1 ->
      let v1 = wrap string v1 in
      G.ty_builtin v1
  | TClass v1 -> class_type v1
  | TArray (t1, v1, t2) ->
      let v1 = typ v1 in
      G.TyArray ((t1, None, t2), v1) |> G.t
  | TVar t ->
      let t = info t in
      G.TyAny t |> G.t

and type_arguments v = bracket (list type_argument) v

and class_type v =
  let res =
    list1
      (fun (v1, v2) ->
        let v1 = ident v1 and v2 = option type_arguments v2 in
        (v1, v2))
      v
  in

  (* TODO: would like simply
   * G.TyN (H.name_of_ids_with_opt_typeargs res)
   * but got regressions on aliasing_type.java and misc_generic.java
   *)
  match List.rev res with
  | [] -> raise Impossible (* list1 *)
  | [ (id, None) ] -> G.TyN (G.Id (id, G.empty_id_info ())) |> G.t
  | [ (id, Some ts) ] ->
      G.TyApply (G.TyN (H.name_of_ids [ id ]) |> G.t, ts) |> G.t
  | (id, None) :: xs ->
      G.TyN
        (G.IdQualified
           {
             G.name_last = (id, None);
             name_top = None;
             name_middle = Some (G.QDots (List.rev xs));
             name_info = G.empty_id_info ();
           })
      |> G.t
  | (id, Some ts) :: xs ->
      G.TyApply
        (G.TyN (H.name_of_ids (List.rev (id :: List_.map fst xs))) |> G.t, ts)
      |> G.t

and type_argument = function
  | TArgument v1 ->
      let v1 = ref_type v1 in
      G.TA v1
  | TWildCard (v1, v2) ->
      let v2 =
        option
          (fun (v1, v2) ->
            let v1 = wrap bool v1 and v2 = ref_type v2 in
            (v1, v2))
          v2
      in
      G.TAWildcard (v1, v2)

and ref_type v = typ v

let type_parameter = function
  | TParamEllipsis v1 -> G.TParamEllipsis v1
  | TParam (v1, v2) ->
      let v1 = ident v1 and v2 = list ref_type v2 in
      G.tparam_of_id v1 ~tp_bounds:v2

let rec modifier (x, tok) =
  let s = Tok.content_of_tok tok in
  match x with
  | Public -> G.attr G.Public tok
  | Protected -> G.attr G.Protected tok
  | Private -> G.attr G.Private tok
  | Abstract -> G.attr G.Abstract tok
  | Static -> G.attr G.Static tok
  | Final -> G.attr G.Final tok
  | StrictFP -> G.unhandled_keywordattr (s, tok)
  | Transient -> G.unhandled_keywordattr (s, tok)
  | Volatile -> G.attr G.Volatile tok
  | Synchronized -> G.unhandled_keywordattr (s, tok)
  | Native -> G.unhandled_keywordattr (s, tok)
  | DefaultModifier -> G.attr G.DefaultImpl tok
  | Sealed -> G.attr G.SealedClass tok
  | NonSealed -> G.unhandled_keywordattr (s, tok)
  | Annotation v1 -> annotation v1

and modifiers v = list modifier v

and annotation (t, v1, v2) =
  let v1 = qualified_ident v1 in
  let xs =
    match v2 with
    | None -> fb []
    | Some x -> bracket annotation_element x
  in
  let name = H.name_of_ids v1 in
  G.NamedAttr (t, name, xs)

and annotation_element = function
  | AnnotArgValue v1 ->
      let v1 = element_value v1 in
      [ G.Arg v1 ]
  | AnnotArgPairInit v1 -> list annotation_pair v1
  | EmptyAnnotArg -> []

and element_value = function
  | AnnotExprInit v1 ->
      let v1 = expr v1 in
      v1
  | AnnotNestedAnnot v1 ->
      let v1 = annotation v1 in
      G.OtherExpr (("Annot", unsafe_fake ""), [ G.At v1 ]) |> G.e
  | AnnotArrayInit (t1, v1, t2) ->
      let v1 = list element_value v1 in
      G.Container (G.List, (t1, v1, t2)) |> G.e

and annotation_pair = function
  | AnnotPair (v1, v2) ->
      let v1 = ident v1 and v2 = element_value v2 in
      G.ArgKwd (v1, v2)
  | AnnotPairEllipsis v1 -> G.Arg (G.Ellipsis v1 |> G.e)

(* id_or_name_of_qualified_ident *)
and name v =
  let v = ident v in
  G.Id (v, G.empty_id_info ())

(*
  let res = list1
      (fun (v1, v2) ->
         let _v1TODO = list type_argument v1
         and v2 = ident v2 in (v2))
      v
  in
  (match List.rev res with
   | [] -> raise Impossible (* list1 *)
   | [name] -> G.Id (name, G.empty_id_info())
   | name::y::xs ->
       let name_info = { G.
                         name_typeargs = None; (* could be v1TODO above *)
                         name_qualifier = Some (G.QDots (List.rev (y::xs)));
                       } in
       G.IdQualified ((name, name_info), G.empty_id_info())
  )
*)
and literal = function
  | Int v1 -> G.Int v1
  | Float v1 ->
      let v1 = wrap id v1 in
      G.Float v1
  | String v1 ->
      let v1 = wrap string v1 in
      G.String (fb v1)
  | TextBlock v1 ->
      (* TODO: remove enclosing triple quotes? or do that in ast_java.ml? *)
      let v1 = wrap string v1 in
      G.String (fb v1)
  | Char v1 ->
      let v1 = wrap string v1 in
      G.Char v1
  | Null v1 ->
      let v1 = tok v1 in
      G.Null v1
  | Bool v1 ->
      let v1 = wrap bool v1 in
      G.Bool v1

and expr e =
  (match e with
  | This t -> G.IdSpecial (G.This, t)
  | ObjAccessEllipsis (v1, v2) ->
      let v1 = expr v1 in
      G.DotAccessEllipsis (v1, v2)
  | Ellipsis v1 ->
      let v1 = tok v1 in
      G.Ellipsis v1
  | DeepEllipsis v1 ->
      let v1 = bracket expr v1 in
      G.DeepEllipsis v1
  | NameId v1 -> G.N (name v1)
  | NameOrClassType v1 ->
      let ii = Ast_java.tok_of_name_or_class_type v1 in
      error ii "NameOrClassType should only appear in (ignored) annotations"
  | Literal v1 ->
      let v1 = literal v1 in
      G.L v1
  | ClassLiteral (v1, v2) ->
      let v1 = typ v1 in
      G.OtherExpr (("ClassLiteral", v2), [ G.T v1 ])
  | NewClass (v0, v1, (lp, v2, rp), v3) -> (
      let v1 = typ v1
      and v2 = list argument v2
      and v3 = option (bracket decls) v3 in
      match v3 with
      | None -> G.New (v0, v1, G.empty_id_info (), (lp, v2, rp))
      | Some decls ->
          let anonclass =
            G.AnonClass
              {
                G.ckind = (G.Class, v0);
                cextends = [ (v1, None) ];
                cimplements = [];
                cmixins = [];
                cparams = fb [];
                cbody = decls |> bracket (List_.map (fun x -> G.F x));
              }
            |> G.e
          in
          G.Call (anonclass, (lp, v2, rp)))
  | NewArray (v0, v1, v2, v3, v4) -> (
      let v1 = typ v1
      and v2 = list argument v2
      and v3 = int v3
      and v4 = option init v4 in
      let rec mk_array n =
        if n < 1 then raise Impossible;
        (* see parser_java.mly dims | dim_exprs rules *)
        if n =|= 1 then G.TyArray (fb None, v1) |> G.t
        else G.TyArray (fb None, mk_array (n - 1)) |> G.t
      in
      let t = mk_array (v3 + List.length v2) in
      match v4 with
      | None -> G.New (v0, t, G.empty_id_info (), fb v2)
      | Some e -> G.New (v0, t, G.empty_id_info (), fb (G.Arg e :: v2)))
  (* x.new Y(...) {...} *)
  | NewQualifiedClass (v0, _tok1, tok2, v2, v3, v4) ->
      let v0 = expr v0
      and v2 = typ v2
      and v3 = arguments v3
      and v4 = option (bracket decls) v4 in
      let anys =
        [ G.E v0; G.T v2 ]
        @ (v3 |> Tok.unbracket |> List_.map (fun arg -> G.Ar arg))
        @ (Option.to_list v4 |> List_.map Tok.unbracket |> List_.flatten
          |> List_.map (fun st -> G.S st))
      in
      G.OtherExpr (("NewQualifiedClass", tok2), anys)
  | MethodRef (v1, v2, v3, v4) ->
      let v1 = expr_or_type v1 in
      let v2 = tok v2 in
      let _v3TODO = option type_arguments v3 in
      let v4 = ident v4 in
      (* TODO? use G.GetRef? *)
      G.OtherExpr (("MethodRef", v2), [ v1; G.I v4 ])
  | Call (v1, v2) ->
      let v1 = expr v1 and v2 = arguments v2 in
      G.Call (v1, v2)
  | Dot (v1, t, v2) ->
      let v1 = expr v1 and t = info t and v2 = ident v2 in
      G.DotAccess (v1, t, G.FN (G.Id (v2, G.empty_id_info ())))
  | ArrayAccess (v1, v2) ->
      let v1 = expr v1 and v2 = bracket expr v2 in
      G.ArrayAccess (v1, v2)
  | Postfix (v1, (v2, tok)) ->
      let v1 = expr v1 and v2 = fix_op v2 in
      G.Call
        (G.IdSpecial (G.IncrDecr (v2, G.Postfix), tok) |> G.e, fb [ G.Arg v1 ])
  | Prefix ((v1, tok), v2) ->
      let v1 = fix_op v1 and v2 = expr v2 in
      G.Call
        (G.IdSpecial (G.IncrDecr (v1, G.Prefix), tok) |> G.e, fb [ G.Arg v2 ])
  | Unary (v1, v2) ->
      let v1, tok = v1 and v2 = expr v2 in
      G.Call (G.IdSpecial (G.Op v1, tok) |> G.e, fb [ G.Arg v2 ])
  | Infix (v1, (v2, tok), v3) ->
      let v1 = expr v1 and v2 = v2 and v3 = expr v3 in
      G.Call (G.IdSpecial (G.Op v2, tok) |> G.e, fb [ G.Arg v1; G.Arg v3 ])
  | Cast ((l, v1, _), v2) ->
      let v1 = list typ v1 and v2 = expr v2 in
      let t =
        Common2.foldl1 (fun acc e -> G.TyAnd (acc, fake l "&", e) |> G.t) v1
      in
      G.Cast (t, l, v2)
  | InstanceOf (v1, v2) ->
      let v1 = expr v1 and v2 = ref_type v2 in
      G.Call
        ( G.IdSpecial (G.Instanceof, unsafe_fake "instanceof") |> G.e,
          fb [ G.Arg v1; G.ArgType v2 ] )
  | Conditional (v1, v2, v3) ->
      let v1 = expr v1 and v2 = expr v2 and v3 = expr v3 in
      G.Conditional (v1, v2, v3)
  | Assign (v1, v2, v3) ->
      let v1 = expr v1 and v2 = info v2 and v3 = expr v3 in
      G.Assign (v1, v2, v3)
  | AssignOp (v1, (v2, tok), v3) ->
      let v1 = expr v1 and v3 = expr v3 in
      G.AssignOp (v1, (v2, tok), v3)
  | TypedMetavar (v1, v2) ->
      let v1 = ident v1 in
      let v2 = typ v2 in
      G.TypedMetavar (v1, Tok.fake_tok (snd v1) " ", v2)
  | Lambda (v1, t, v2) ->
      let fparams = parameters v1 in
      let v2 = stmt v2 in
      G.Lambda
        {
          G.fparams = fb fparams;
          frettype = None;
          fbody = G.FBStmt v2;
          fkind = (G.Arrow, t);
        }
  | SwitchE (v0, v1, v2) ->
      let v0 = info v0 in
      let v1 = expr v1
      and v2 =
        list
          (fun (v1, v2) ->
            let v1 = cases v1 and v2 = stmts v2 in
            (v1, G.stmt1 v2))
          v2
        |> List_.map (fun x -> G.CasesAndBody x)
      in
      let x = G.stmt_to_expr (G.Switch (v0, Some (Cond v1), v2) |> G.s) in
      x.G.e)
  |> G.e

and class_parent v : G.class_parent =
  let v = ref_type v in
  (v, None)

and expr_or_type = function
  | Left e -> G.E (expr e)
  | Right t -> G.T (typ t)

and argument v =
  let v = expr v in
  G.Arg v

and arguments v : G.argument list G.bracket = bracket (list argument) v
and fix_op v = v

and resource t (v : resource) : G.stmt =
  match v with
  | Left v ->
      let ent, v = var_with_init v in
      G.DefStmt (ent, G.VarDef v) |> G.s
  | Right e -> G.ExprStmt (expr e, t) |> G.s

and resources (_t1, v, t2) = list (resource t2) v

and stmt st =
  match stmt_aux st with
  | [] -> G.s (Block (Tok.unsafe_fake_bracket []))
  | [ st ] -> st
  | xs ->
      (* This should never happen in a context where we want
         a single statement. In Java, Blocks correspond with
         a new scope, so in `stmt_aux` we should explicitly
         create a Block when we want one. This means that if
         we reach this case, we do not want a new scope, and
         should not create a new Block. To avoid giving users
         errors when we don't need to (e.g. they just want to
         match) we'll create one anyway, but a good way to
         debug scoping issues is to put a failwith here. If you
         run into this, you probably instead want to flatten
         the statements *)
      let warning = "this-should-never-happen-and-could-be-a-scoping-problem" in
      G.s (Block (G.fake warning, xs, G.fake warning))

and stmt_aux st =
  match st with
  | EmptyStmt t -> [ G.Block (t, [], t) |> G.s ]
  | Block v1 ->
      let v1 = bracket stmts v1 in
      [ G.Block v1 |> G.s ]
  | Expr (v1, t) ->
      let v1 = expr v1 in
      [ G.ExprStmt (v1, t) |> G.s ]
  | If (t, v1, v2, v3) ->
      let v1 = expr v1 and v2 = stmt v2 and v3 = option stmt v3 in
      [ G.If (t, G.Cond v1, v2, v3) |> G.s ]
  | Switch (v0, v1, v2) ->
      let v0 = info v0 in
      let v1 = expr v1
      and v2 =
        list
          (fun (v1, v2) ->
            let v1 = cases v1 and v2 = stmts v2 in
            (v1, G.stmt1 v2))
          v2
        |> List_.map (fun x -> G.CasesAndBody x)
      in
      [ G.Switch (v0, Some (G.Cond v1), v2) |> G.s ]
  | While (t, v1, v2) ->
      let v1 = expr v1 and v2 = stmt v2 in
      [ G.While (t, G.Cond v1, v2) |> G.s ]
  | Do (t, v1, v2) ->
      let v1 = stmt v1 and v2 = expr v2 in
      [ G.DoWhile (t, v1, v2) |> G.s ]
  | For (t, v1, v2) ->
      let v1 = for_control t v1 and v2 = stmt v2 in
      [ G.For (t, v1, v2) |> G.s ]
  | Break (t, v1) ->
      let v1 = H.opt_to_label_ident v1 in
      [ G.Break (t, v1, G.sc) |> G.s ]
  | Continue (t, v1) ->
      let v1 = H.opt_to_label_ident v1 in
      [ G.Continue (t, v1, G.sc) |> G.s ]
  | Return (t, v1) ->
      let v1 = option expr v1 in
      [ G.Return (t, v1, G.sc) |> G.s ]
  | Label (v1, v2) ->
      let v1 = ident v1 and v2 = stmt v2 in
      [ G.Label (v1, v2) |> G.s ]
  | Sync (v0, v1, v2) ->
      let v1 = expr v1 and v2 = stmt v2 in
      [ G.OtherStmtWithStmt (G.OSWS_Block ("Sync", v0), [ G.E v1 ], v2) |> G.s ]
  | Try (t, v0, v1, v2, v3) -> (
      let v1 = stmt v1 and v2 = catches v2 and v3 = option tok_and_stmt v3 in
      let try_stmt = G.Try (t, v1, v2, None, v3) |> G.s in
      match v0 with
      | None -> [ try_stmt ]
      | Some r -> [ G.WithUsingResource (t, resources r, try_stmt) |> G.s ])
  | Throw (t, v1) ->
      let v1 = expr v1 in
      [ G.Throw (t, v1, G.sc) |> G.s ]
  | LocalVarList (vs, sc) ->
      vs
      |> List_.map (fun v1 ->
             let ent, v = var_with_init v1 in
             (ent, v))
      |> H.add_semicolon_to_last_var_def_and_convert_to_stmts sc
  | DeclStmt v1 -> [ decl v1 ]
  | DirectiveStmt v1 -> [ directive v1 ]
  | Assert (t, v1, v2) ->
      let v1 = expr v1 and v2 = option expr v2 in
      let es = v1 :: Option.to_list v2 in
      let args = es |> List_.map G.arg in
      [ G.Assert (t, fb args, G.sc) |> G.s ]

and tok_and_stmt (t, v) =
  let v = stmt v in
  (t, v)

and stmts v = list stmt_aux v |> List_.flatten

and case = function
  | Case (t, v1) ->
      let v1 = expr v1 in
      G.Case (t, H.expr_to_pattern v1)
  | Default t -> G.Default t

and cases v = list case v

and list_to_opt_seq = function
  | [] -> None
  | [ e ] -> Some e
  | xs -> Some (G.Seq xs |> G.e)

and for_control tok = function
  | ForEllipsis t -> G.ForEllipsis t
  | ForClassic (v1, v2, v3) ->
      let v1 = for_init v1 and v2 = list expr v2 and v3 = list expr v3 in
      G.ForClassic (v1, list_to_opt_seq v2, list_to_opt_seq v3)
  | Foreach (v1, v2) ->
      let ent, typ = var v1 and v2 = expr v2 in
      let id, _idinfo = id_of_entname ent.G.name in
      let patid = G.PatId (id, G.empty_id_info ()) in
      let pat =
        match typ with
        | Some t -> G.PatTyped (patid, t)
        | None -> error tok "TODO: Foreach without a type"
      in
      G.ForEach (pat, fake (snd id) "in", v2)

and for_init = function
  | ForInitVars v1 ->
      let v1 = list var_with_init v1 in
      v1 |> List_.map (fun (ent, v) -> G.ForInitVar (ent, v))
  | ForInitExprs v1 ->
      let v1 = list expr v1 in
      v1 |> List_.map (fun e -> G.ForInitExpr e)

and var { name; mods; type_ = xtyp } =
  let v1 = ident name in
  let v2 = modifiers mods in
  let v3 = option typ xtyp in
  (G.basic_entity v1 ~attrs:v2, v3)

and catch (tok, catch_exn, v2) =
  let v2 = stmt v2 in
  let exn =
    match catch_exn with
    | CatchParam (v1, _union_types) -> (
        let ent, typ = var v1 in
        let id, _idinfo = id_of_entname ent.G.name in
        match typ with
        | Some t -> G.CatchParam (G.param_of_type t ~pname:id)
        | None -> error tok "TODO: Catch without a type")
    | CatchEllipsis t -> G.CatchPattern (G.PatEllipsis t)
  in
  (tok, exn, v2)

and catches v = list catch v

and var_with_init { f_var; f_init } =
  let ent, t = var f_var in
  let init = option init f_init in
  (ent, { G.vinit = init; vtype = t; vtok = G.no_sc })

and init = function
  | ExprInit v1 ->
      let v1 = expr v1 in
      v1
  | ArrayInit v1 ->
      let v1 = bracket (list init) v1 in
      G.Container (G.Array, v1) |> G.e

and parameters v : G.parameter list = List_.map parameter_binding v

and parameter_binding = function
  | ParamClassic v
  | ParamReceiver v ->
      let ent, t = var v in
      G.Param (entity_to_param ent t)
  | ParamSpread (tk, v) ->
      let ent, t = var v in
      let p = entity_to_param ent t in
      G.ParamRest (tk, p)
  | ParamEllipsis t -> G.ParamEllipsis t

and method_decl ?cl_kind { m_var; m_formals; m_throws; m_body } =
  let ent, rett = var m_var in
  let fparams = parameters m_formals in
  let v3 = list typ m_throws in
  let v4 = stmt m_body in
  (* TODO: use fthrow field instead *)
  let throws =
    v3
    |> List_.map (fun t -> G.OtherAttribute (("Throw", G.fake ""), [ G.T t ]))
  in
  let fbody =
    match (cl_kind, v4) with
    | Some (Interface, _), { s = G.Block (_, [], _); _ } -> G.FBNothing
    | _ -> FBStmt v4
  in
  ( { ent with G.attrs = ent.G.attrs @ throws },
    {
      G.fparams = fb fparams;
      frettype = rett;
      fbody;
      fkind = (G.Method, G.fake "");
    } )

and field v = var_with_init v

and enum_decl { en_name; en_mods; en_impls; en_body } =
  let v1 = ident en_name in
  let v2 = modifiers en_mods in
  let v3 = list class_parent en_impls in
  let v4, v5 = en_body in
  let v4 = list enum_constant v4 |> List_.map G.fld in
  let v5 = decls v5 |> List_.map (fun st -> G.F st) in
  let ent = G.basic_entity v1 ~attrs:(G.attr EnumClass (snd v1) :: v2) in
  let cbody = fb (v4 @ v5) in
  let cdef =
    {
      G.ckind = (G.Class, snd v1);
      cextends = v3;
      cmixins = [];
      cimplements = [];
      cparams = fb [];
      cbody;
    }
  in
  (ent, cdef)

and enum_constant (v1, v2, v3) =
  let id = ident v1 in
  let v2 = option arguments v2 in
  let v3 = option class_body v3 in
  let ent = G.basic_entity id in
  let def = { G.ee_args = v2; ee_body = v3 } in
  (ent, G.EnumEntryDef def)

and class_body ?cl_kind (l, xs, r) : G.field list G.bracket =
  let xs = decls ?cl_kind xs |> List_.map (fun x -> G.F x) in
  (l, xs, r)

and class_decl
    {
      cl_name;
      cl_kind;
      cl_tparams;
      cl_mods;
      cl_extends;
      cl_impls;
      cl_body;
      cl_formals;
    } =
  let v1 = ident cl_name in
  let v2, more_attrs = class_kind_and_more cl_kind in
  let v3 =
    match cl_tparams with
    | [] -> None
    | xs -> Some (Tok.unsafe_fake_bracket (list type_parameter xs))
  in
  let v4 = modifiers cl_mods in
  let v5 = option class_parent cl_extends in
  let v6 = list ref_type cl_impls in
  let cparams = parameters cl_formals in
  let fields = class_body ~cl_kind cl_body in
  let ent = G.basic_entity v1 ~attrs:(more_attrs @ v4) ?tparams:v3 in
  let cdef =
    {
      G.ckind = v2;
      cextends = Option.to_list v5;
      cimplements = v6;
      cmixins = [];
      cparams = fb cparams;
      cbody = fields;
    }
  in
  (ent, cdef)

and class_kind_and_more (x, t) =
  match x with
  | ClassRegular -> ((G.Class, t), [])
  | Interface -> ((G.Interface, t), [])
  | AtInterface -> ((G.Interface, t), [ G.attr AnnotationClass t ])
  | Record -> ((G.Class, t), [ G.attr RecordClass t ])

and decl ?cl_kind decl : G.stmt =
  match decl with
  | Class v1 ->
      let ent, def = class_decl v1 in
      G.DefStmt (ent, G.ClassDef def) |> G.s
  | Method v1 ->
      let ent, def = method_decl ?cl_kind v1 in
      G.DefStmt (ent, G.FuncDef def) |> G.s
  | Field v1 ->
      let ent, def = field v1 in
      G.DefStmt (ent, G.VarDef def) |> G.s
  | Enum v1 ->
      let ent, def = enum_decl v1 in
      G.DefStmt (ent, G.ClassDef def) |> G.s
  | Init (v1, v2) -> (
      let st = stmt v2 in
      match v1 with
      | Some tstatic ->
          G.OtherStmtWithStmt (G.OSWS_Block ("Static", tstatic), [], st) |> G.s
      | None -> st)
  | DeclEllipsis v1 -> G.ExprStmt (G.Ellipsis v1 |> G.e, G.sc) |> G.s
  | DeclMetavarEllipsis v1 ->
      G.ExprStmt (G.N (Id (v1, G.empty_id_info ())) |> G.e, G.sc) |> G.s
  | EmptyDecl t -> G.Block (t, [], t) |> G.s
  | AnnotationTypeElementTodo t -> G.OtherStmt (G.OS_Todo, [ G.Tk t ]) |> G.s

and decls ?cl_kind v : G.stmt list = list (decl ?cl_kind) v

and import = function
  | ImportAll (t, xs, tok) -> G.ImportAll (t, G.DottedName xs, tok)
  | ImportFrom (t, xs, id) ->
      let id = ident id in
      G.ImportFrom (t, G.DottedName xs, [ H.mk_import_from_kind id None ])

and directive = function
  | Import (static, v2) ->
      let d_attrs =
        match static with
        | None -> []
        | Some t -> [ G.attr G.Static t ]
      in
      G.DirectiveStmt { G.d = import v2; d_attrs } |> G.s
  | Package (t, qu, _t2) ->
      let qu = qualified_ident qu in
      G.DirectiveStmt (G.Package (t, qu) |> G.d) |> G.s
  | ModuleTodo t -> G.OtherStmt (G.OS_Todo, [ G.Tk t ]) |> G.s

let program v = stmts v

let partial = function
  | PartialDecl x -> (
      let x = decl x in
      match x.G.s with
      | G.DefStmt def -> G.PartialDef def
      | _ -> failwith "unsupported PartialDecl")
  | PartialIf (v1, v2) ->
      let v2 = expr v2 in
      G.PartialIf (v1, v2)
  | PartialTry (v1, v2) ->
      let v2 = stmt v2 in
      G.PartialTry (v1, v2)
  | PartialFinally (v1, v2) ->
      let v2 = stmt v2 in
      G.PartialFinally (v1, v2)
  | PartialCatch v1 ->
      let v1 = catch v1 in
      G.PartialCatch v1

let any = function
  | AMod v1 ->
      let v1 = modifier v1 in
      G.At v1
  | Partial v1 ->
      let v1 = partial v1 in
      G.Partial v1
  | AIdent v1 ->
      let v1 = ident v1 in
      G.I v1
  | AExpr v1 ->
      let v1 = expr v1 in
      G.E v1
  | AStmt v1 ->
      let v1 = stmt v1 in
      G.S v1
  | AStmts v1 ->
      let v1 = List_.map stmt v1 in
      G.Ss v1
  | ATyp v1 ->
      let v1 = typ v1 in
      G.T v1
  | AVar v1 ->
      let ent, t = var v1 in
      G.Def (ent, G.VarDef { G.vtype = t; vinit = None; vtok = G.no_sc })
  | AInit v1 ->
      let v1 = init v1 in
      G.E v1
  | AMethod v1 ->
      let ent, def = method_decl v1 in
      G.Def (ent, G.FuncDef def)
  | AField v1 ->
      let ent, def = field v1 in
      G.Def (ent, G.VarDef def)
  | AClass v1 ->
      let ent, def = class_decl v1 in
      G.Def (ent, G.ClassDef def)
  | AProgram v1 ->
      let v1 = program v1 in
      G.Pr v1
