(* Yoann Padioleau
 *
 * Copyright (C) 2019 r2c
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

let option = Common.map_opt

let list = List.map

let (string : string -> string) = id

let (bool : bool -> bool) = id

let (int : int -> int) = id

let error = AST_generic.error

let fake s = Parse_info.fake_info s

(* todo: to remove at some point when Ast_java includes them directly *)
let fb = G.fake_bracket

let id_of_entname = function
  | G.EN (Id (id, idinfo)) -> (id, idinfo)
  | G.EN _ | G.EDynamic _ -> raise Impossible

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
      G.TyBuiltin v1
  | TClass v1 ->
      let v1 = class_type v1 in
      v1
  | TArray (t1, v1, t2) ->
      let v1 = typ v1 in
      G.TyArray ((t1, None, t2), v1)

and class_type v =
  let res =
    list1
      (fun (v1, v2) ->
        let v1 = ident v1 and _v2TODO = list type_argument v2 in
        v1)
      v
  in
  match List.rev res with
  | [] -> raise Impossible (* list1 *)
  | [ id ] -> G.TyN (G.Id (id, G.empty_id_info ()))
  | id :: xs ->
      let name_info =
        {
          G.name_typeargs = None;
          (* could be v1TODO above *)
          name_qualifier = Some (G.QDots (List.rev xs));
        }
      in
      G.TyN (G.IdQualified ((id, name_info), G.empty_id_info ()))

and type_argument = function
  | TArgument v1 ->
      let v1 = ref_type v1 in
      G.TypeArg v1
  | TWildCard (v1, v2) ->
      let v2 =
        option
          (fun (v1, v2) ->
            let v1 = wrap bool v1 and v2 = ref_type v2 in
            (v1, v2))
          v2
      in
      G.TypeWildcard (v1, v2)

and ref_type v = typ v

let type_parameter = function
  | TParam (v1, v2) ->
      let v1 = ident v1 and v2 = list ref_type v2 in
      (v1, v2 |> List.map (fun t -> G.Extends t))

let rec modifier (x, tok) =
  match x with
  | Public -> G.attr G.Public tok
  | Protected -> G.attr G.Protected tok
  | Private -> G.attr G.Private tok
  | Abstract -> G.attr G.Abstract tok
  | Static -> G.attr G.Static tok
  | Final -> G.attr G.Final tok
  | StrictFP -> G.OtherAttribute (G.OA_StrictFP, [])
  | Transient -> G.OtherAttribute (G.OA_Transient, [])
  | Volatile -> G.attr G.Volatile tok
  | Synchronized -> G.OtherAttribute (G.OA_Synchronized, [])
  | Native -> G.OtherAttribute (G.OA_Native, [])
  | DefaultModifier -> G.OtherAttribute (G.OA_Default, [])
  | Annotation v1 -> annotation v1

and modifiers v = list modifier v

and annotation (t, v1, v2) =
  let v1 = qualified_ident v1 in
  let xs =
    match v2 with None -> fb [] | Some x -> bracket annotation_element x
  in
  let name = H.name_of_ids v1 in
  G.NamedAttr (t, name, xs)

and type_arguments x = list type_argument x

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
      G.OtherExpr (G.OE_Annot, [ G.At v1 ])
  | AnnotArrayInit (t1, v1, t2) ->
      let v1 = list element_value v1 in
      G.Container (G.List, (t1, v1, t2))

and annotation_pair = function
  | AnnotPair (v1, v2) ->
      let v1 = ident v1 and v2 = element_value v2 in
      G.ArgKwd (v1, v2)
  | AnnotPairEllipsis v1 -> G.Arg (G.Ellipsis v1)

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
  | Int v1 ->
      let v1 = wrap id v1 in
      G.Int v1
  | Float v1 ->
      let v1 = wrap id v1 in
      G.Float v1
  | String v1 ->
      let v1 = wrap string v1 in
      G.String v1
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
  match e with
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
  | NameOrClassType _v1 ->
      let ii = Lib_parsing_java.ii_of_any (AExpr e) in
      error (List.hd ii)
        "NameOrClassType should only appear in (ignored) annotations"
  | Literal v1 ->
      let v1 = literal v1 in
      G.L v1
  | ClassLiteral (v1, v2) ->
      let v1 = typ v1 in
      G.OtherExpr (G.OE_ClassLiteral, [ G.T v1; G.Tk v2 ])
  | NewClass (v0, v1, (lp, v2, rp), v3) -> (
      let v1 = typ v1
      and v2 = list argument v2
      and v3 = option (bracket decls) v3 in
      match v3 with
      | None -> G.Call (G.IdSpecial (G.New, v0), (lp, G.ArgType v1 :: v2, rp))
      | Some decls ->
          let anonclass =
            G.AnonClass
              {
                G.ckind = (G.Class, v0);
                cextends = [ v1 ];
                cimplements = [];
                cmixins = [];
                cparams = [];
                cbody = decls |> bracket (List.map (fun x -> G.FieldStmt x));
              }
          in
          G.Call (G.IdSpecial (G.New, v0), (lp, G.Arg anonclass :: v2, rp)) )
  | NewArray (v0, v1, v2, v3, v4) -> (
      let v1 = typ v1
      and v2 = list argument v2
      and v3 = int v3
      and v4 = option init v4 in
      let rec mk_array n =
        if n < 1 then raise Impossible;
        (* see parser_java.mly dims | dim_exprs rules *)
        if n = 1 then G.TyArray (fb None, v1)
        else G.TyArray (fb None, mk_array (n - 1))
      in
      let t = mk_array (v3 + List.length v2) in
      match v4 with
      | None -> G.Call (G.IdSpecial (G.New, v0), fb (G.ArgType t :: v2))
      | Some e ->
          G.Call (G.IdSpecial (G.New, v0), fb (G.ArgType t :: G.Arg e :: v2)) )
  (* x.new Y(...) {...} *)
  | NewQualifiedClass (v0, _tok1, _tok2, v2, v3, v4) ->
      let v0 = expr v0
      and v2 = typ v2
      and v3 = arguments v3
      and v4 = option (bracket decls) v4 in
      let any =
        [ G.E v0; G.T v2 ]
        @ (v3 |> G.unbracket |> List.map (fun arg -> G.Ar arg))
        @ ( Common.opt_to_list v4 |> List.map G.unbracket |> List.flatten
          |> List.map (fun st -> G.S st) )
      in
      G.OtherExpr (G.OE_NewQualifiedClass, any)
  | MethodRef (v1, v2, v3, v4) ->
      let v1 = expr_or_type v1 in
      let v2 = tok v2 in
      let _v3 = type_arguments v3 in
      let v4 = ident v4 in
      G.OtherExpr (G.OE_Todo, [ v1; G.Tk v2; G.I v4 ])
  | Call (v1, v2) ->
      let v1 = expr v1 and v2 = arguments v2 in
      G.Call (v1, v2)
  | Dot (v1, t, v2) ->
      let v1 = expr v1 and t = info t and v2 = ident v2 in
      G.DotAccess (v1, t, G.EN (G.Id (v2, G.empty_id_info ())))
  | ArrayAccess (v1, v2) ->
      let v1 = expr v1 and v2 = bracket expr v2 in
      G.ArrayAccess (v1, v2)
  | Postfix (v1, (v2, tok)) ->
      let v1 = expr v1 and v2 = fix_op v2 in
      G.Call (G.IdSpecial (G.IncrDecr (v2, G.Postfix), tok), fb [ G.Arg v1 ])
  | Prefix ((v1, tok), v2) ->
      let v1 = fix_op v1 and v2 = expr v2 in
      G.Call (G.IdSpecial (G.IncrDecr (v1, G.Prefix), tok), fb [ G.Arg v2 ])
  | Unary (v1, v2) ->
      let v1, tok = v1 and v2 = expr v2 in
      G.Call (G.IdSpecial (G.Op (H.conv_op v1), tok), fb [ G.Arg v2 ])
  | Infix (v1, (v2, tok), v3) ->
      let v1 = expr v1 and v2 = v2 and v3 = expr v3 in
      G.Call (G.IdSpecial (G.Op (H.conv_op v2), tok), fb [ G.Arg v1; G.Arg v3 ])
  | Cast ((_, v1, _), v2) ->
      let v1 = list typ v1 and v2 = expr v2 in
      let t = Common2.foldl1 (fun acc e -> G.TyAnd (acc, fake "&", e)) v1 in
      G.Cast (t, v2)
  | InstanceOf (v1, v2) ->
      let v1 = expr v1 and v2 = ref_type v2 in
      G.Call
        ( G.IdSpecial (G.Instanceof, fake "instanceof"),
          fb [ G.Arg v1; G.ArgType v2 ] )
  | Conditional (v1, v2, v3) ->
      let v1 = expr v1 and v2 = expr v2 and v3 = expr v3 in
      G.Conditional (v1, v2, v3)
  | Assign (v1, v2, v3) ->
      let v1 = expr v1 and v2 = info v2 and v3 = expr v3 in
      G.Assign (v1, v2, v3)
  | AssignOp (v1, (v2, tok), v3) ->
      let v1 = expr v1 and v3 = expr v3 in
      G.AssignOp (v1, (H.conv_op v2, tok), v3)
  | TypedMetavar (v1, v2) ->
      let v1 = ident v1 in
      let v2 = typ v2 in
      G.TypedMetavar (v1, Parse_info.fake_info " ", v2)
  | Lambda (v1, t, v2) ->
      let v1 = parameters v1 in
      let v2 = stmt v2 in
      G.Lambda
        { G.fparams = v1; frettype = None; fbody = v2; fkind = (G.Arrow, t) }
  | SwitchE (v0, v1, v2) ->
      let v0 = info v0 in
      let v1 = expr v1
      and v2 =
        list
          (fun (v1, v2) ->
            let v1 = cases v1 and v2 = stmts v2 in
            (v1, G.stmt1 v2))
          v2
        |> List.map (fun x -> G.CasesAndBody x)
      in
      G.OtherExpr (G.OE_StmtExpr, [ G.S (G.Switch (v0, Some v1, v2) |> G.s) ])

and expr_or_type = function Left e -> G.E (expr e) | Right t -> G.T (typ t)

and argument v =
  let v = expr v in
  G.Arg v

and arguments v : G.argument list G.bracket = bracket (list argument) v

and fix_op v = H.conv_incr v

and stmt st =
  match st with
  | EmptyStmt t -> G.Block (t, [], t) |> G.s
  | Block v1 ->
      let v1 = bracket stmts v1 in
      G.Block v1 |> G.s
  | Expr (v1, t) ->
      let v1 = expr v1 in
      G.ExprStmt (v1, t) |> G.s
  | If (t, v1, v2, v3) ->
      let v1 = expr v1 and v2 = stmt v2 and v3 = option stmt v3 in
      G.If (t, v1, v2, v3) |> G.s
  | Switch (v0, v1, v2) ->
      let v0 = info v0 in
      let v1 = expr v1
      and v2 =
        list
          (fun (v1, v2) ->
            let v1 = cases v1 and v2 = stmts v2 in
            (v1, G.stmt1 v2))
          v2
        |> List.map (fun x -> G.CasesAndBody x)
      in
      G.Switch (v0, Some v1, v2) |> G.s
  | While (t, v1, v2) ->
      let v1 = expr v1 and v2 = stmt v2 in
      G.While (t, v1, v2) |> G.s
  | Do (t, v1, v2) ->
      let v1 = stmt v1 and v2 = expr v2 in
      G.DoWhile (t, v1, v2) |> G.s
  | For (t, v1, v2) ->
      let v1 = for_control t v1 and v2 = stmt v2 in
      G.For (t, v1, v2) |> G.s
  | Break (t, v1) ->
      let v1 = H.opt_to_label_ident v1 in
      G.Break (t, v1, G.sc) |> G.s
  | Continue (t, v1) ->
      let v1 = H.opt_to_label_ident v1 in
      G.Continue (t, v1, G.sc) |> G.s
  | Return (t, v1) ->
      let v1 = option expr v1 in
      G.Return (t, v1, G.sc) |> G.s
  | Label (v1, v2) ->
      let v1 = ident v1 and v2 = stmt v2 in
      G.Label (v1, v2) |> G.s
  | Sync (v1, v2) ->
      let v1 = expr v1 and v2 = stmt v2 in
      G.OtherStmt (G.OS_Sync, [ G.E v1; G.S v2 ]) |> G.s
  | Try (t, _v0TODO, v1, v2, v3) ->
      let v1 = stmt v1 and v2 = catches v2 and v3 = option tok_and_stmt v3 in
      G.Try (t, v1, v2, v3) |> G.s
  | Throw (t, v1) ->
      let v1 = expr v1 in
      G.Throw (t, v1, G.sc) |> G.s
  | LocalVar v1 ->
      let ent, v = var_with_init v1 in
      G.DefStmt (ent, G.VarDef v) |> G.s
  | DeclStmt v1 -> decl v1
  | DirectiveStmt v1 -> directive v1
  | Assert (t, v1, v2) ->
      let v1 = expr v1 and v2 = option expr v2 in
      G.Assert (t, v1, v2, G.sc) |> G.s

and tok_and_stmt (t, v) =
  let v = stmt v in
  (t, v)

and stmts v = list stmt v

and case = function
  | Case (t, v1) ->
      let v1 = expr v1 in
      G.Case (t, H.expr_to_pattern v1)
  | Default t -> G.Default t

and cases v = list case v

and list_to_opt_seq = function
  | [] -> None
  | [ e ] -> Some e
  | xs -> Some (G.Seq xs)

and for_control tok = function
  | ForEllipsis t -> G.ForEllipsis t
  | ForClassic (v1, v2, v3) ->
      let v1 = for_init v1 and v2 = list expr v2 and v3 = list expr v3 in
      G.ForClassic (v1, list_to_opt_seq v2, list_to_opt_seq v3)
  | Foreach (v1, v2) ->
      let ent, typ = var v1 and v2 = expr v2 in
      let id, _idinfo = id_of_entname ent.G.name in
      let pat =
        match typ with
        | Some t -> G.PatVar (t, Some (id, G.empty_id_info ()))
        | None -> error tok "TODO: Foreach without a type"
      in
      G.ForEach (pat, fake "in", v2)

and for_init = function
  | ForInitVars v1 ->
      let v1 = list var_with_init v1 in
      v1 |> List.map (fun (ent, v) -> G.ForInitVar (ent, v))
  | ForInitExprs v1 ->
      let v1 = list expr v1 in
      v1 |> List.map (fun e -> G.ForInitExpr e)

and var { name; mods; type_ = xtyp } =
  let v1 = ident name in
  let v2 = modifiers mods in
  let v3 = option typ xtyp in
  (G.basic_entity v1 v2, v3)

and catch (tok, (v1, _union_types), v2) =
  let ent, typ = var v1 in
  let id, _idinfo = id_of_entname ent.G.name in
  let v2 = stmt v2 in
  let pat =
    match typ with
    | Some t -> G.PatVar (t, Some (id, G.empty_id_info ()))
    | None -> error tok "TODO: Catch without a type"
  in
  (tok, pat, v2)

and catches v = list catch v

and var_with_init { f_var; f_init } =
  let ent, t = var f_var in
  let init = option init f_init in
  (ent, { G.vinit = init; vtype = t })

and init = function
  | ExprInit v1 ->
      let v1 = expr v1 in
      v1
  | ArrayInit v1 ->
      let v1 = bracket (list init) v1 in
      G.Container (G.Array, v1)

and parameters v = List.map parameter_binding v

and parameter_binding = function
  | ParamClassic v | ParamReceiver v ->
      let ent, t = var v in
      G.ParamClassic (entity_to_param ent t)
  | ParamSpread (tk, v) ->
      let ent, t = var v in
      let p = entity_to_param ent t in
      G.ParamRest (tk, p)
  | ParamEllipsis t -> G.ParamEllipsis t

and method_decl { m_var; m_formals; m_throws; m_body } =
  let ent, rett = var m_var in
  let v2 = parameters m_formals in
  let v3 = list typ m_throws in
  let v4 = stmt m_body in
  let throws =
    v3 |> List.map (fun t -> G.OtherAttribute (G.OA_AnnotThrow, [ G.T t ]))
  in
  ( { ent with G.attrs = ent.G.attrs @ throws },
    {
      G.fparams = v2;
      frettype = rett;
      fbody = v4;
      fkind = (G.Method, G.fake "");
    } )

and field v = var_with_init v

and enum_decl { en_name; en_mods; en_impls; en_body } =
  let v1 = ident en_name in
  let v2 = modifiers en_mods in
  let _v3TODO = list ref_type en_impls in
  let v4, v5 = en_body in
  let v4 = list enum_constant v4 in
  let _v5TODO = decls v5 in
  let ent = G.basic_entity v1 v2 in
  let tdef = { G.tbody = G.OrType v4 } in
  (ent, tdef)

and enum_constant (v1, v2, v3) =
  let v1 = ident v1 in
  let _v2TODO = option arguments v2 in
  let _v3TODO = option class_body v3 in
  G.OrConstructor (v1, [])

and class_body x = bracket decls x

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
  let v2 = class_kind cl_kind in
  let v3 = list type_parameter cl_tparams in
  let v4 = modifiers cl_mods in
  let v5 = option typ cl_extends in
  let v6 = list ref_type cl_impls in
  let cparams = parameters cl_formals in
  let v7 = class_body cl_body in
  let fields = v7 |> bracket (List.map (fun x -> G.FieldStmt x)) in
  let ent = { (G.basic_entity v1 v4) with G.tparams = v3 } in
  let cdef =
    {
      G.ckind = v2;
      cextends = Common.opt_to_list v5;
      cimplements = v6;
      cmixins = [];
      cparams;
      cbody = fields;
    }
  in
  (ent, cdef)

and class_kind (x, t) =
  ( ( match x with
    | ClassRegular -> G.Class
    | Interface -> G.Interface
    | AtInterface -> G.AtInterface
    | Record -> G.RecordClass ),
    t )

and decl decl =
  match decl with
  | Class v1 ->
      let ent, def = class_decl v1 in
      G.DefStmt (ent, G.ClassDef def) |> G.s
  | Method v1 ->
      let ent, def = method_decl v1 in
      G.DefStmt (ent, G.FuncDef def) |> G.s
  | Field v1 ->
      let ent, def = field v1 in
      G.DefStmt (ent, G.VarDef def) |> G.s
  | Enum v1 ->
      let ent, def = enum_decl v1 in
      G.DefStmt (ent, G.TypeDef def) |> G.s
  | Init (_v1TODO, v2) ->
      let v2 = stmt v2 in
      v2
  | DeclEllipsis v1 -> G.ExprStmt (G.Ellipsis v1, G.sc) |> G.s
  | EmptyDecl t -> G.Block (t, [], t) |> G.s
  | AnnotationTypeElementTodo t -> G.OtherStmt (G.OS_Todo, [ G.Tk t ]) |> G.s

and decls v = list decl v

and import = function
  | ImportAll (t, xs, tok) -> G.ImportAll (t, G.DottedName xs, tok)
  | ImportFrom (t, xs, id) ->
      let id = ident id in
      G.ImportFrom (t, G.DottedName xs, id, None)

and directive = function
  | Import (_vstatic, v2) -> G.DirectiveStmt (import v2) |> G.s
  | Package (t, qu, _t2) ->
      let qu = qualified_ident qu in
      G.DirectiveStmt (G.Package (t, qu)) |> G.s
  | ModuleTodo t -> G.OtherStmt (G.OS_Todo, [ G.Tk t ]) |> G.s

let program v = stmts v

let partial = function
  | PartialDecl x -> (
      let x = decl x in
      match x.G.s with
      | G.DefStmt def -> G.PartialDef def
      | _ -> failwith "unsupported PartialDecl" )

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
      let v1 = List.map stmt v1 in
      G.Ss v1
  | ATyp v1 ->
      let v1 = typ v1 in
      G.T v1
  | AVar v1 ->
      let ent, t = var v1 in
      G.Def (ent, G.VarDef { G.vtype = t; vinit = None })
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
