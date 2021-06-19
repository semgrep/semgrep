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
open Ast_js
module G = AST_generic
module H = AST_generic_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Ast_js to AST_generic.
 *
 * See ast_generic.ml for more information.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let id x = x

let option = Common.map_opt

let list = List.map

let bool = id

let string = id

let error = AST_generic.error

(* for the require -> import translation *)
exception ComplicatedCase

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* todo: you should not use that, just pass the token as-is,
 * they are the same in ast_js.ml and AST_generic.ml
 *)
let info x = x

let wrap _of_a (v1, v2) =
  let v1 = _of_a v1 and v2 = info v2 in
  (v1, v2)

let bracket of_a (t1, x, t2) = (info t1, of_a x, info t2)

let name v = wrap id v

let ident x = name x

let filename v = wrap string v

let label v = wrap string v

type special_result =
  | SR_Special of G.special wrap
  | SR_Other of G.other_expr_operator wrap
  | SR_Literal of G.literal
  | SR_NeedArgs of (G.expr list -> G.expr)

let special (x, tok) =
  match x with
  | UseStrict -> SR_Other (G.OE_UseStrict, tok)
  | Null -> SR_Literal (G.Null tok)
  | Undefined -> SR_Literal (G.Undefined tok)
  | This -> SR_Special (G.This, tok)
  | Super -> SR_Special (G.Super, tok)
  | Require -> SR_Other (G.OE_Require, tok) (* TODO: left up to include? *)
  | Exports -> SR_Other (G.OE_Exports, tok)
  | Module -> SR_Other (G.OE_Module, tok)
  | Define -> SR_Other (G.OE_Define, tok)
  | Arguments -> SR_Other (G.OE_Arguments, tok)
  | New -> SR_Special (G.New, tok)
  | NewTarget -> SR_Other (G.OE_NewTarget, tok)
  | Eval -> SR_Special (G.Eval, tok)
  | Seq -> SR_NeedArgs (fun args -> G.Seq args)
  | Typeof -> SR_Special (G.Typeof, tok)
  | Instanceof -> SR_Special (G.Instanceof, tok)
  | In -> SR_Special (G.Op G.In, tok)
  | Delete -> SR_Other (G.OE_Delete, tok)
  (* a kind of cast operator:
   * See https://stackoverflow.com/questions/7452341/what-does-void-0-mean
   *)
  | Void ->
      SR_NeedArgs
        (fun args ->
          match args with
          | [ e ] ->
              let tvoid = G.TyBuiltin ("void", tok) in
              G.Cast (tvoid, e)
          | _ -> error tok "Impossible: Too many arguments to Void")
  | Spread -> SR_Special (G.Spread, tok)
  | Yield ->
      SR_NeedArgs
        (fun args ->
          match args with
          | [] -> G.Yield (tok, None, false)
          | [ e ] -> G.Yield (tok, Some e, false)
          | _ -> error tok "Impossible: Too many arguments to Yield")
  | YieldStar -> SR_Other (G.OE_YieldStar, tok)
  | Await ->
      SR_NeedArgs
        (fun args ->
          match args with
          | [ e ] -> G.Await (tok, e)
          | _ -> error tok "Impossible: Too many arguments to Await")
  | Encaps has_tag_function ->
      if not has_tag_function then
        SR_NeedArgs
          (fun args ->
            G.Call
              ( G.IdSpecial (G.ConcatString G.InterpolatedConcat, tok),
                args |> List.map (fun e -> G.Arg e) |> G.fake_bracket ))
      else
        SR_NeedArgs
          (fun args ->
            match args with
            | [] -> raise Impossible
            | tag :: rest ->
                G.Call
                  ( tag,
                    G.fake_bracket
                      [
                        G.Arg
                          (G.Call
                             ( G.IdSpecial
                                 (* update: we don't use InterpolatedConcat
                                  * here anymore, to differentiate it from
                                  * the above case.
                                  *)
                                 (G.ConcatString G.TaggedTemplateLiteral, tok),
                               rest
                               |> List.map (fun e -> G.Arg e)
                               |> G.fake_bracket ));
                      ] ))
  | ArithOp op -> SR_Special (G.Op (H.conv_op op), tok)
  | IncrDecr v -> SR_Special (G.IncrDecr (H.conv_incdec v), tok)

(*
   This is used to expose an individual statement as a block of one statement,
   where a sequence of statements is allowed. This simplifies the task
   of the sgrep pattern matcher.
   TODO: check all the places where this wrapping is necessary.
   TODO: see if this is an issue with other languages besides javascript.
*)
let as_block stmt =
  match stmt.G.s with
  | G.Block _ -> stmt
  | _ -> G.Block (G.fake_bracket [ stmt ]) |> G.s

let rec property_name = function
  | PN v1 ->
      let v1 = name v1 in
      Left v1
  | PN_Computed v1 ->
      let v1 = expr v1 in
      Right v1

and xhp = function
  | XmlText v1 ->
      let v1 = string v1 in
      G.XmlText v1
  | XmlExpr v1 ->
      let v1 = bracket (option expr) v1 in
      G.XmlExpr v1
  | XmlXml v1 ->
      let v1 = xml v1 in
      G.XmlXml v1

and xml_attribute = function
  | XmlAttr (v1, t, v2) ->
      let v1 = ident v1 and v2 = xhp_attr v2 in
      G.XmlAttr (v1, t, v2)
  | XmlAttrExpr v ->
      let v = bracket expr v in
      G.XmlAttrExpr v
  | XmlEllipsis v1 -> G.XmlEllipsis v1

and xml { xml_kind = xml_tag; xml_attrs; xml_body } =
  let tag = xml_kind xml_tag in
  let attrs = list xml_attribute xml_attrs in
  let body = list xhp xml_body in
  { G.xml_kind = tag; xml_attrs = attrs; xml_body = body }

and xml_kind = function
  | XmlClassic (v0, v1, v2, v3) ->
      let v1 = ident v1 in
      G.XmlClassic (v0, v1, v2, v3)
  | XmlSingleton (v0, v1, v2) ->
      let v1 = ident v1 in
      XmlSingleton (v0, v1, v2)
  | XmlFragment (v1, v2) -> XmlFragment (v1, v2)

and xhp_attr v = expr v

and literal x =
  match x with
  | Bool v1 ->
      let v1 = wrap bool v1 in
      G.Bool v1
  | Num v1 ->
      let v1 = wrap string v1 in
      G.Float v1
  | String v1 ->
      let v1 = wrap string v1 in
      G.String v1
  | Regexp (v1, v2) ->
      let v1 = bracket (wrap string) v1 in
      let v2 = option (wrap string) v2 in
      G.Regexp (v1, v2)

and expr (x : expr) =
  match x with
  | ObjAccessEllipsis (v1, v2) ->
      let v1 = expr v1 in
      G.DotAccessEllipsis (v1, v2)
  (* not sure this is actually a valid JS/TS construct *)
  | Cast (v1, _v2, v3) ->
      let v1 = expr v1 in
      let v3 = type_ v3 in
      G.Cast (v3, v1)
  (* converting to Cast as it's mostly the same *)
  | TypeAssert (v1, _v2, v3) ->
      let v1 = expr v1 in
      let v3 = type_ v3 in
      G.Cast (v3, v1)
  | ExprTodo (v1, v2) ->
      let v2 = list expr v2 in
      G.OtherExpr (G.OE_Todo, G.TodoK v1 :: (v2 |> List.map (fun e -> G.E e)))
  | L x -> G.L (literal x)
  | Id v1 ->
      let v1 = name v1 in
      G.N (G.Id (v1, G.empty_id_info ()))
  | IdSpecial v1 -> (
      let x = special v1 in
      match x with
      | SR_Special v -> G.IdSpecial v
      | SR_NeedArgs _ ->
          error (snd v1) "Impossible: should have been matched in Call first"
      | SR_Literal l -> G.L l
      | SR_Other (x, tok) -> G.OtherExpr (x, [ G.Tk tok ]) )
  | Assign (v1, tok, v2) ->
      let v1 = expr v1 and v2 = expr v2 in
      let tok = info tok in
      G.Assign (v1, tok, v2)
  | ArrAccess (v1, v2) ->
      let v1 = expr v1 and v2 = bracket expr v2 in
      G.ArrayAccess (v1, v2)
  | Obj v1 ->
      let flds = obj_ v1 in
      G.Record flds
  | Ellipsis v1 ->
      let v1 = info v1 in
      G.Ellipsis v1
  | DeepEllipsis v1 ->
      let v1 = bracket expr v1 in
      G.DeepEllipsis v1
  | TypedMetavar (v1, tok, v2) ->
      let v1 = name v1 and v2 = type_ v2 in
      let tok = info tok in
      G.TypedMetavar (v1, tok, v2)
  | Class (v1, _v2TODO) ->
      let def, _more_attrsTODOEMPTY = class_ v1 in
      G.AnonClass def
  | ObjAccess (v1, t, v2) -> (
      let v1 = expr v1 in
      let v2 = property_name v2 in
      let t = info t in
      match v2 with
      | Left n -> G.DotAccess (v1, t, G.EN (G.Id (n, G.empty_id_info ())))
      | Right e -> G.DotAccess (v1, t, G.EDynamic e) )
  | Fun (v1, _v2TODO) ->
      let def, _more_attrs = fun_ v1 in
      (* todo? assert more_attrs = []? *)
      G.Lambda def
  | Apply (IdSpecial v1, v2) -> (
      let x = special v1 in
      let v2 = bracket (list expr) v2 in
      match x with
      | SR_Special v ->
          G.Call (G.IdSpecial v, bracket (List.map (fun e -> G.Arg e)) v2)
      | SR_Literal _ -> error (snd v1) "Weird: literal in call position"
      | SR_Other (x, tok) ->
          (* ex: NewTarget *)
          G.Call
            ( G.OtherExpr (x, [ G.Tk tok ]),
              bracket (List.map (fun e -> G.Arg e)) v2 )
      | SR_NeedArgs f -> f (G.unbracket v2) )
  | Apply (v1, v2) ->
      let v1 = expr v1 and v2 = bracket (list expr) v2 in
      G.Call (v1, bracket (List.map (fun e -> G.Arg e)) v2)
  | Arr v1 ->
      let v1 = bracket (list expr) v1 in
      G.Container (G.Array, v1)
  | Conditional (v1, v2, v3) ->
      let v1 = expr v1 and v2 = expr v2 and v3 = expr v3 in
      G.Conditional (v1, v2, v3)
  | Xml v1 ->
      let v1 = xml v1 in
      G.Xml v1

and stmt x =
  match x with
  | StmtTodo (v1, v2) ->
      let v2 = list any v2 in
      G.OtherStmt (G.OS_Todo, G.TodoK v1 :: v2) |> G.s
  | M v1 ->
      let v1 = module_directive v1 in
      G.DirectiveStmt v1 |> G.s
  | DefStmt v1 ->
      let v1 = definition v1 in
      G.DefStmt v1 |> G.s
  | Block v1 ->
      let v1 = bracket list_stmt v1 in
      G.Block v1 |> G.s
  | ExprStmt (v1, t) ->
      let v1 = expr v1 in
      G.ExprStmt (v1, t) |> G.s
  | If (t, v1, v2, v3) ->
      let v1 = expr v1 and v2 = stmt v2 and v3 = option stmt v3 in
      G.If (t, v1, v2, v3) |> G.s
  | Do (t, v1, v2) ->
      let v1 = stmt v1 and v2 = expr v2 in
      G.DoWhile (t, v1, v2) |> G.s
  | While (t, v1, v2) ->
      let v1 = expr v1 and v2 = stmt v2 in
      G.While (t, v1, v2) |> G.s
  | For (t, v1, v2) ->
      let v1 = for_header v1 and v2 = stmt v2 in
      G.For (t, v1, v2) |> G.s
  | Switch (v0, v1, v2) ->
      let v0 = info v0 in
      let v1 = expr v1
      and v2 = list case v2 |> List.map (fun x -> G.CasesAndBody x) in
      G.Switch (v0, Some v1, v2) |> G.s
  | Continue (t, v1, sc) ->
      let v1 = option label v1 in
      G.Continue (t, H.opt_to_label_ident v1, sc) |> G.s
  | Break (t, v1, sc) ->
      let v1 = option label v1 in
      G.Break (t, H.opt_to_label_ident v1, sc) |> G.s
  | Return (t, v1, sc) ->
      let v1 = option expr v1 in
      G.Return (t, v1, sc) |> G.s
  | Label (v1, v2) ->
      let v1 = label v1 and v2 = stmt v2 in
      G.Label (v1, v2) |> G.s
  | Throw (t, v1, sc) ->
      let v1 = expr v1 in
      G.Throw (t, v1, sc) |> G.s
  | Try (t, v1, v2, v3) ->
      let v1 = stmt v1
      and v2 = option catch_block v2
      and v3 = option tok_and_stmt v3 in
      G.Try (t, v1, Common.opt_to_list v2, v3) |> G.s
  | With (_v1, v2, v3) ->
      let e = expr v2 in
      let v3 = stmt v3 in
      G.OtherStmtWithStmt (G.OSWS_With, Some e, v3) |> G.s

and catch_block = function
  | BoundCatch (t, v1, v2) ->
      let v1 = H.expr_to_pattern (expr v1) and v2 = stmt v2 in
      (t, v1, v2)
  | UnboundCatch (t, v1) ->
      let v1 =
        stmt v1
        (* bugfix: reusing 't' to avoid NoTokenLocation error when
         * a semgrep patter like catch($ERR) matches an UnboundCatch. *)
      in
      (t, G.PatUnderscore t, v1)

and tok_and_stmt (t, v) =
  let v = stmt v in
  (t, v)

and for_header = function
  | ForClassic (v1, v2, v3) -> (
      let v2 = option expr v2 in
      let v3 = option expr v3 in
      match v1 with
      | Left vars ->
          let vars =
            vars
            |> List.map (fun x ->
                   let a, b = var_of_var x in
                   G.ForInitVar (a, b))
          in
          G.ForClassic (vars, v2, v3)
      | Right e ->
          let e = expr e in
          G.ForClassic ([ G.ForInitExpr e ], v2, v3) )
  | ForIn (v1, t, v2) ->
      let v2 = expr v2 in
      let pattern =
        match v1 with
        (* TODO: v_init is not always _NONE! when we use multivardef!!! *)
        | Left ({ name = id; attrs = _TODO }, { v_init = _NONE; _ }) ->
            G.PatId (id, G.empty_id_info ())
        | Right e ->
            let e = expr e in
            H.expr_to_pattern e
      in
      G.ForEach (pattern, t, v2)
  | ForOf (v1, t, v2) ->
      let v2 = expr v2 in
      let pattern =
        match v1 with
        (* TODO: v_init is not always _NONE! when we use multivardef!!! *)
        | Left ({ name = id; attrs = _TODO }, { v_init = _NONE; _ }) ->
            G.PatId (id, G.empty_id_info ())
        | Right e ->
            let e = expr e in
            H.expr_to_pattern e
      in
      let e = G.Call (G.IdSpecial (G.ForOf, t), G.fake_bracket [ G.Arg v2 ]) in
      G.ForEach (pattern, t, e)
  | ForEllipsis v1 -> G.ForEllipsis v1

and case = function
  | Case (t, v1, v2) ->
      let v1 = expr v1 and v2 = stmt v2 in
      ([ G.Case (t, H.expr_to_pattern v1) ], v2)
  | Default (t, v1) ->
      let v1 = stmt v1 in
      ([ G.Default t ], v1)

(* used to be an AST_generic.type_ with no conversion needed, but now that
 * we moved AST_generic.ml out of pfff, we need the boilerplate below
 *)
and type_ x =
  match x with
  | TyBuiltin id -> G.TyBuiltin (ident id)
  | TyName xs -> G.TyN (H.name_of_ids xs)
  | TyLiteral l ->
      let l = literal l in
      G.OtherType
        (G.OT_Todo, [ G.TodoK ("LitType", PI.fake_info ""); G.E (G.L l) ])
  | TyQuestion (tok, t) ->
      let t = type_ t in
      G.TyQuestion (t, tok)
  | TyArray (t, (lt, (), rt)) ->
      let t = type_ t in
      G.TyArray ((lt, None, rt), t)
  | TyTuple (lt, xs, rt) ->
      let xs = List.map tuple_type_member xs in
      G.TyTuple (lt, xs, rt)
  | TyFun (params, typ_opt) ->
      let params = List.map parameter_binding params in
      let rett =
        match typ_opt with
        | None -> G.TyBuiltin ("void", PI.fake_info "void")
        | Some t -> type_ t
      in
      G.TyFun (params, rett)
  | TyRecordAnon (lt, (), rt) -> G.TyRecordAnon (PI.fake_info "", (lt, [], rt))
  | TyOr (t1, tk, t2) ->
      let t1 = type_ t1 in
      let t2 = type_ t2 in
      G.TyOr (t1, tk, t2)
  | TyAnd (t1, tk, t2) ->
      let t1 = type_ t1 in
      let t2 = type_ t2 in
      G.TyAnd (t1, tk, t2)
  | TypeTodo (categ, xs) ->
      G.OtherType (G.OT_Todo, G.TodoK categ :: List.map any xs)

and tuple_type_member x =
  match x with
  | TyTupMember x -> type_ x
  | TyTupOpt (x, tok) -> TyQuestion (type_ x, tok)
  | TyTupRest (tok, x) -> TyRest (tok, type_ x)

and entity { name = n; attrs } =
  let n = name n in
  let attrs = list attribute attrs in
  G.basic_entity n attrs

and definition (ent, def) =
  let ent = entity ent in
  match def with
  | VarDef { v_kind = x_kind; v_init = x_init; v_type = ty } ->
      let v2 = var_kind x_kind in
      let ty = option type_ ty in
      let v3 = option expr x_init in
      ( { ent with G.attrs = v2 :: ent.G.attrs },
        G.VarDef { G.vinit = v3; G.vtype = ty } )
  | FuncDef def ->
      let def, more_attrs = fun_ def in
      ({ ent with G.attrs = ent.G.attrs @ more_attrs }, G.FuncDef def)
  | ClassDef def ->
      let def, more_attrs = class_ def in
      ({ ent with G.attrs = ent.G.attrs @ more_attrs }, G.ClassDef def)
  | DefTodo (v1, v2) ->
      let v2 = list any v2 in
      (ent, G.OtherDef (G.OD_Todo, G.TodoK v1 :: v2))

and var_of_var
    ({ name = x_name; attrs }, { v_kind = x_kind; v_init = x_init; v_type }) =
  let v1 = name x_name in
  let attrs = list attribute attrs in
  let v2 = var_kind x_kind in
  let ent = G.basic_entity v1 (v2 :: attrs) in
  let v3 = option expr x_init in
  let v_type = option type_ v_type in
  (ent, { G.vinit = v3; vtype = v_type })

and var_kind (x, tok) =
  match x with
  | Var -> G.attr G.Var tok
  | Let -> G.attr G.Let tok
  | Const -> G.attr G.Const tok

and function_definition x =
  let a, _attrs = fun_ x in
  (* ?? assert _attrs = []? *)
  a

and fun_ { f_kind; f_attrs = f_props; f_params; f_body; f_rettype } =
  let fkind = H.conv_function_kind f_kind in
  let v1 = list attribute f_props in
  let v2 = list parameter_binding f_params in
  let v3 = stmt f_body |> as_block in
  let frettype = option type_ f_rettype in
  ({ G.fparams = v2; frettype; fbody = v3; fkind }, v1)

and parameter_binding = function
  | ParamClassic x -> parameter x
  | ParamPattern x -> G.ParamPattern (pattern x)
  | ParamEllipsis x -> G.ParamEllipsis x

and pattern x =
  let x = expr x in
  H.expr_to_pattern x

and parameter x =
  match x with
  | { p_name; p_default; p_dots; p_type; p_attrs } -> (
      let v1 = name p_name in
      let pdefault = option expr p_default in
      let v3 = bool p_dots in
      let ptype = option type_ p_type in
      let pattrs = list attribute p_attrs in
      let pclassic =
        {
          G.pname = Some v1;
          pdefault;
          ptype;
          pattrs;
          pinfo = G.empty_id_info ();
        }
      in
      match v3 with
      | None -> G.ParamClassic pclassic
      | Some tok -> G.ParamRest (tok, pclassic) )

and argument x = expr x

and attribute = function
  | KeywordAttr x -> G.KeywordAttr (keyword_attribute x)
  | NamedAttr (t, ids, opt) ->
      let t1, args, t2 =
        match opt with Some x -> x | None -> G.fake_bracket []
      in
      let args = list argument args |> List.map G.arg in
      let name = H.name_of_ids ids in
      G.NamedAttr (t, name, (t1, args, t2))

and keyword_attribute (x, tok) =
  ( ( match x with
    (* methods *)
    | Get -> G.Getter
    | Set -> G.Setter
    | Generator -> G.Generator
    | Async -> G.Async
    (* fields *)
    | Static -> G.Static
    | Public -> G.Public
    | Private -> G.Private
    | Protected -> G.Protected
    | Readonly -> G.Const
    | Optional -> G.Optional
    | Abstract -> G.Abstract
    | NotNull -> G.NotNull ),
    tok )

and obj_ v = bracket (list property) v

and parent = function
  | Left e ->
      let e = expr e in
      H.expr_to_type e
  | Right t -> type_ t

and class_ { c_extends; c_implements; c_body; c_kind; c_attrs } =
  let cextends = list parent c_extends in
  let v2 = bracket (list property) c_body in
  let attrs = list attribute c_attrs in
  let cimplements = list type_ c_implements in
  ( {
      G.ckind = H.conv_class_kind c_kind;
      cextends;
      cimplements;
      cmixins = [];
      cparams = [];
      cbody = v2;
    },
    attrs )

and field_classic
    { fld_name = v1; fld_attrs = v2; fld_type = vt; fld_body = v3 } =
  let v1 = property_name v1 in
  let v2 = list attribute v2 in
  let vt = option type_ vt in
  let ent =
    match v1 with
    | Left n -> G.basic_entity n v2
    | Right e -> { G.name = G.EDynamic e; attrs = v2; tparams = [] }
  in
  match v3 with
  | Some (Fun (def, None)) ->
      let def, more_attrs = fun_ def in
      let _kind, tok = def.G.fkind in
      ( { ent with G.attrs = ent.G.attrs @ more_attrs },
        G.FuncDef { def with G.fkind = (G.Method, tok) } )
  | _ ->
      let v3 = option expr v3 in
      (ent, G.VarDef { G.vinit = v3; vtype = vt })

and property x =
  match x with
  | Field v1 ->
      let ent, def = field_classic v1 in
      G.FieldStmt (G.DefStmt (ent, def) |> G.s)
  | FieldColon v1 ->
      let ent, def = field_classic v1 in
      let def =
        match def with
        (* ugly: this is to prevent assignment to match object field
         * definitions in semgrep *)
        | G.VarDef x -> G.FieldDefColon x
        | _ -> def
      in
      G.FieldStmt (G.DefStmt (ent, def) |> G.s)
  | FieldSpread (t, v1) ->
      let v1 = expr v1 in
      G.FieldSpread (t, v1)
  | FieldEllipsis v1 -> G.FieldStmt (G.exprstmt (G.Ellipsis v1))
  | FieldPatDefault (v1, _v2, v3) ->
      let v1 = pattern v1 in
      let v3 = expr v3 in
      G.FieldStmt (G.exprstmt (G.LetPattern (v1, v3)))
  | FieldTodo (v1, v2) ->
      let v2 = stmt v2 in
      (* hmm, should use OtherStmtWithStmt ? *)
      G.FieldStmt (G.OtherStmt (G.OS_Todo, [ G.TodoK v1; G.S v2 ]) |> G.s)

and alias v1 =
  let v1 = name v1 in
  (v1, G.empty_id_info ())

and module_directive x =
  match x with
  | ReExportNamespace (v1, _v2, _v3, v4) ->
      let v4 = filename v4 in
      G.OtherDirective (G.OI_ReExportNamespace, [ G.Tk v1; G.Str v4 ])
  | Import (t, v1, v2, v3) ->
      let v1 = name v1 and v2 = option alias v2 and v3 = filename v3 in
      G.ImportFrom (t, G.FileName v3, v1, v2)
  | ModuleAlias (t, v1, v2) ->
      let v1 = alias v1 and v2 = filename v2 in
      G.ImportAs (t, G.FileName v2, Some v1)
  (* sgrep: we used to convert this in an OI_ImportEffect, but
   * we now want import "foo" to be used to match any form of import
   *)
  | ImportFile (t, v1) ->
      let v1 = name v1 in
      (* old: G.OtherDirective (G.OI_ImportEffect, [G.I v1]) *)
      G.ImportAs (t, G.FileName v1, None)
  | Export (t, v1) ->
      let v1 = name v1 in
      G.OtherDirective (G.OI_Export, [ G.Tk t; G.I v1 ])

and require_to_import_in_stmt_opt st =
  match st with
  (* ex: const { f1, f2 } = require("file"); *)
  | DefStmt
      ( { name = x, _; attrs = [] },
        VarDef
          {
            v_kind = _;
            v_type = _;
            v_init =
              Some
                (Assign
                  ( pattern,
                    _,
                    Apply
                      (IdSpecial (Require, treq), (_, [ L (String file) ], _))
                  ));
          } )
    when x =$= AST_generic_.special_multivardef_pattern -> (
      try
        match pattern with
        | Obj (_, xs, _) ->
            let ys =
              xs
              |> List.map (function
                   | FieldColon
                       {
                         fld_name = PN id1;
                         fld_body = Some (Id id2);
                         fld_attrs = [];
                         fld_type = None;
                       } ->
                       let alias_opt =
                         match (id1, id2) with
                         | (s1, _), (s2, _) when s1 =$= s2 -> None
                         | _ -> Some (id2, G.empty_id_info ())
                       in
                       G.DirectiveStmt
                         (G.ImportFrom (treq, G.FileName file, id1, alias_opt))
                       |> G.s
                   | _ -> raise ComplicatedCase)
            in
            (* we also keep the require() call in the AST so people using
             * semgrep patterns like 'require("foo")' still find those
             * requires in the target. The ImportFrom conversion is mostly
             * for Naming_AST to recognize those require and do the
             * right aliasing for them too.
             * alt: do the conversion in Naming_AST.ml instead?
             *)
            let orig = stmt st in
            Some (ys @ [ orig ])
        | _ -> raise ComplicatedCase
      with ComplicatedCase -> None )
  | _ -> None

and list_stmt xs =
  (* converting require() in import, so they can benefit from the
   * other goodies coming with import in semgrep (e.g., equivalence aliasing)
   *)
  xs
  |> List.map (fun st ->
         match require_to_import_in_stmt_opt st with
         | Some xs -> xs
         | None -> [ stmt st ])
  |> List.flatten

and program v = list_stmt v

and partial = function
  | PartialFunOrFuncDef (_v1, v2) ->
      let v2 = function_definition v2 in
      G.PartialLambdaOrFuncDef v2
  | PartialDef v1 ->
      let v1 = definition v1 in
      G.PartialDef v1
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
      let v1 = catch_block v1 in
      G.PartialCatch v1
  | PartialSingleField (v1, v2, v3) ->
      let v1 = wrap string v1 in
      let v2 = info v2 in
      let v3 = expr v3 in
      G.PartialSingleField (v1, v2, v3)

and any = function
  | Property v1 ->
      let v1 = property v1 in
      G.Fld v1
  | Expr v1 ->
      let v1 = expr v1 in
      G.E v1
  | Stmt v1 -> (
      match require_to_import_in_stmt_opt v1 with
      | Some xs -> G.Ss xs
      | None ->
          let v1 = stmt v1 in
          G.S v1 )
  | Stmts v1 ->
      let v1 = list_stmt v1 in
      G.Ss v1
  | Program v1 ->
      let v1 = program v1 in
      G.Pr v1
  | Pattern v1 ->
      let v1 = pattern v1 in
      G.P v1
  | Type v1 ->
      let v1 = type_ v1 in
      G.T v1
  | Partial v1 ->
      let v1 = partial v1 in
      G.Partial v1
  | Tk v1 -> G.Tk v1
