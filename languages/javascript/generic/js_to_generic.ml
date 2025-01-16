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
open Either_
open Ast_js
module G = AST_generic
module H = AST_generic_helpers
module Log = Log_parser_javascript.Log

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
let option = Option.map
let list = List_.map
let bool = id
let string = id
let error = AST_generic.error
let fb = Tok.unsafe_fake_bracket

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
  | SR_Other of G.todo_kind
  | SR_Literal of G.literal
  | SR_NeedArgs of (G.expr list -> G.expr_kind)
  | SR_Expr of G.expr_kind

let special (x, tok) =
  let other_expr s = SR_Other (s, tok) in
  match x with
  | UseStrict -> other_expr "UseStrict"
  | Null -> SR_Literal (G.Null tok)
  | Undefined -> SR_Literal (G.Undefined tok)
  | This -> SR_Special (G.This, tok)
  | Super -> SR_Special (G.Super, tok)
  | Require -> SR_Special (G.Require, tok)
  (* We could add a new IdSpecial to Semgrep and DeepSemgrep for `module` and
   * `exports` like we did for `Require`, but we only need to analyze CJS
   * exports in a couple places, so it doesn't seem as worth it. Require needs
   * special naming behavior in Semgrep and DeepSemgrep, special matching
   * behavior in Semgrep, and special treatment when extracting dependencies in
   * DeepSemgrep. In contrast, `module` and `exports` should only need to be
   * handled in one place in DeepSemgrep. *)
  | Exports -> SR_Expr (G.N (G.Id (("exports", tok), G.empty_id_info ())))
  | Module -> SR_Expr (G.N (G.Id (("module", tok), G.empty_id_info ())))
  | Define -> other_expr "Define"
  | Arguments -> other_expr "Arguments"
  | NewTarget -> other_expr "NewTarget"
  | Eval -> SR_Special (G.Eval, tok)
  | Seq -> SR_NeedArgs (fun args -> G.Seq args)
  | Typeof -> SR_Special (G.Typeof, tok)
  | Instanceof -> SR_Special (G.Instanceof, tok)
  | In -> SR_Special (G.Op G.In, tok)
  | Delete -> other_expr "Delete"
  (* a kind of cast operator:
   * See https://stackoverflow.com/questions/7452341/what-does-void-0-mean
   *)
  | Void ->
      SR_NeedArgs
        (fun args ->
          match args with
          | [ e ] ->
              let tvoid = G.ty_builtin ("void", tok) in
              G.Cast (tvoid, Tok.fake_tok tok ":", e)
          | _ -> error tok "Impossible: Too many arguments to Void")
  | Spread -> SR_Special (G.Spread, tok)
  | Yield ->
      SR_NeedArgs
        (fun args ->
          match args with
          | [] -> G.Yield (tok, None, false)
          | [ e ] -> G.Yield (tok, Some e, false)
          | _ -> error tok "Impossible: Too many arguments to Yield")
  | YieldStar -> other_expr "YieldStar"
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
              ( G.IdSpecial (G.ConcatString G.InterpolatedConcat, tok) |> G.e,
                args |> List_.map (fun e -> G.Arg e) |> fb ))
      else
        SR_NeedArgs
          (fun args ->
            match args with
            | [] -> raise Impossible
            | tag :: rest ->
                G.Call
                  ( tag,
                    fb
                      [
                        G.Arg
                          (G.Call
                             ( G.IdSpecial
                                 (* update: we don't use InterpolatedConcat
                                  * here anymore, to differentiate it from
                                  * the above case.
                                  *)
                                 (G.ConcatString G.TaggedTemplateLiteral, tok)
                               |> G.e,
                               rest |> List_.map (fun e -> G.Arg e) |> fb )
                          |> G.e);
                      ] ))
  | ArithOp op -> SR_Special (G.Op op, tok)
  | IncrDecr v -> SR_Special (G.IncrDecr v, tok)

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
  | _ -> G.Block (fb [ stmt ]) |> G.s

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
      (* TODO Correctly parse Foo.Bar into IdQualified *)
      let v1 = G.Id (ident v1, G.empty_id_info ()) in
      G.XmlClassic (v0, v1, v2, v3)
  | XmlSingleton (v0, v1, v2) ->
      (* TODO Correctly parse Foo.Bar into IdQualified *)
      let v1 = G.Id (ident v1, G.empty_id_info ()) in
      XmlSingleton (v0, v1, v2)
  | XmlFragment (v1, v2) -> XmlFragment (v1, v2)

and xhp_attr v = expr v

and literal x : G.literal =
  match x with
  | Bool v1 ->
      let v1 = wrap bool v1 in
      G.Bool v1
  | Num v1 ->
      let v1 = wrap string v1 in
      G.Float v1
  | String v1 ->
      let v1 = wrap string v1 in
      G.String (fb v1)
  | Regexp (v1, v2) ->
      let v1 = bracket (wrap string) v1 in
      let v2 = option (wrap string) v2 in
      G.Regexp (v1, v2)

and expr (x : expr) =
  match x with
  | ObjAccessEllipsis (v1, v2) ->
      let v1 = expr v1 in
      G.DotAccessEllipsis (v1, v2) |> G.e
  (* not sure this is actually a valid JS/TS construct *)
  | Cast (v1, v2, v3) ->
      let v1 = expr v1 in
      let v3 = type_ v3 in
      G.Cast (v3, v2, v1) |> G.e
  (* converting to Cast as it's mostly the same *)
  | TypeAssert (v1, v2, v3) ->
      let v1 = expr v1 in
      let v3 = type_ v3 in
      G.Cast (v3, v2, v1) |> G.e
  | ExprTodo (v1, v2) ->
      let v2 = list expr v2 in
      G.OtherExpr (v1, v2 |> List_.map (fun e -> G.E e)) |> G.e
  | ParenExpr (l, e, r) ->
      let e = expr e in
      H.set_e_range l r e;
      e
  | L x -> G.L (literal x) |> G.e
  | Id v1 ->
      let v1 = name v1 in
      G.N (G.Id (v1, G.empty_id_info ())) |> G.e
  | IdSpecial v1 ->
      (let x = special v1 in
       match x with
       | SR_Special v -> G.IdSpecial v
       | SR_NeedArgs _ ->
           error (snd v1) "Impossible: should have been matched in Call first"
       | SR_Literal l -> G.L l
       | SR_Other categ -> G.OtherExpr (categ, [])
       | SR_Expr e -> e)
      |> G.e
  | Assign (v1, tok, v2) ->
      let v1 = expr v1 and v2 = expr v2 in
      let tok = info tok in
      G.Assign (v1, tok, v2) |> G.e
  | ArrAccess (v1, v2) ->
      let v1 = expr v1 and v2 = bracket expr v2 in
      G.ArrayAccess (v1, v2) |> G.e
  | Obj v1 ->
      let flds = obj_ v1 in
      G.Record flds |> G.e
  | Ellipsis v1 ->
      let v1 = info v1 in
      G.Ellipsis v1 |> G.e
  | DeepEllipsis v1 ->
      let v1 = bracket expr v1 in
      G.DeepEllipsis v1 |> G.e
  | TypedMetavar (v1, tok, v2) ->
      let v1 = name v1 and v2 = type_ v2 in
      let tok = info tok in
      G.TypedMetavar (v1, tok, v2) |> G.e
  | Class (v1, _v2TODO) ->
      let def, _more_attrsTODOEMPTY = class_ v1 in
      G.AnonClass def |> G.e
  | ObjAccess (v1, t, v2) ->
      let e = expr v1 in
      let t, v1 =
        match t with
        | Dot, tok -> (info tok, e)
        | QuestDot, tok ->
            let t = info tok in
            ( t,
              G.Call (G.IdSpecial (G.Op G.Elvis, t) |> G.e, fb [ G.Arg e ])
              |> G.e )
      in
      let v2 = property_name v2 in
      (match v2 with
      | Left n -> G.DotAccess (v1, t, G.FN (G.Id (n, G.empty_id_info ())))
      | Right e -> G.DotAccess (v1, t, G.FDynamic e))
      |> G.e
  | Fun (v1, _v2TODO) ->
      let def, more_attrs = fun_ v1 in
      (* TODO: Include attrs in generic AST? Where? *)
      let e = G.Lambda def |> G.e in
      (* Since the attrs aren't included in the AST, at least update the range
       * to include them. See
       * https://github.com/semgrep/semgrep/issues/7353 *)
      let attrs_any = List_.map (fun attr -> G.At attr) more_attrs in
      H.set_e_range_with_anys (G.Dk (G.FuncDef def) :: attrs_any) e;
      e
  | Apply (IdSpecial v1, v2) ->
      let x = special v1 in
      let v2 = bracket (list expr) v2 in
      (match x with
      | SR_Special v ->
          G.Call (G.IdSpecial v |> G.e, bracket (List_.map G.arg) v2)
      | SR_Literal l ->
          Log.warn (fun m -> m "Weird: literal in call position");
          (* apparently there's code like (null)("fs"), no idea what that is *)
          G.Call (G.L l |> G.e, bracket (List_.map G.arg) v2)
      | SR_NeedArgs f -> f (Tok.unbracket v2)
      | SR_Other categ ->
          (* ex: NewTarget *)
          G.Call
            ( G.OtherExpr (categ, []) |> G.e,
              bracket (List_.map (fun e -> G.Arg e)) v2 )
      | SR_Expr e -> G.Call (e |> G.e, bracket (List_.map G.arg) v2))
      |> G.e
  | Apply (v1, v2) ->
      let v1 = expr v1 and v2 = bracket (list expr) v2 in
      G.Call (v1, bracket (List_.map (fun e -> G.Arg e)) v2) |> G.e
  | New (tok, e, args) ->
      let tok = info tok in
      let e = expr e in
      let args = bracket (list (fun arg -> G.Arg (expr arg))) args in
      G.New (tok, H.expr_to_type e, G.empty_id_info (), args) |> G.e
  | Arr v1 ->
      let v1 = bracket (list expr) v1 in
      G.Container (G.Array, v1) |> G.e
  | Conditional (v1, v2, v3) ->
      let v1 = expr v1 and v2 = expr v2 and v3 = expr v3 in
      G.Conditional (v1, v2, v3) |> G.e
  | Xml v1 ->
      let v1 = xml v1 in
      G.Xml v1 |> G.e

and stmt x =
  match x with
  | StmtTodo (v1, v2) ->
      let v2 = list any v2 in
      G.OtherStmt (G.OS_Todo, G.TodoK v1 :: v2) |> G.s
  | M v1 ->
      let v1 = module_directive v1 in
      G.DirectiveStmt (v1 |> G.d) |> G.s
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
      G.If (t, Cond v1, v2, v3) |> G.s
  | Do (t, v1, v2) ->
      let v1 = stmt v1 and v2 = expr v2 in
      G.DoWhile (t, v1, v2) |> G.s
  | While (t, v1, v2) ->
      let v1 = expr v1 and v2 = stmt v2 in
      G.While (t, G.Cond v1, v2) |> G.s
  | For (t, v1, v2) ->
      let v1 = for_header v1 and v2 = stmt v2 in
      G.For (t, v1, v2) |> G.s
  | Switch (v0, v1, v2) ->
      let v0 = info v0 in
      let v1 = expr v1 and v2 = list case v2 in
      G.Switch (v0, Some (G.Cond v1), v2) |> G.s
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
      G.Try (t, v1, Option.to_list v2, None, v3) |> G.s
  | With (_v1, v2, v3) ->
      let e = expr v2 in
      let v3 = stmt v3 in
      G.OtherStmtWithStmt (G.OSWS_With, [ G.E e ], v3) |> G.s

and catch_block = function
  | BoundCatch (t, v1, v2) ->
      let v1 = H.expr_to_pattern (expr v1) and v2 = stmt v2 in
      (t, G.CatchPattern v1, v2)
  | UnboundCatch (t, v1) ->
      let v1 =
        stmt v1
        (* bugfix: reusing 't' to avoid NoTokenLocation error when
         * a semgrep patter like catch($ERR) matches an UnboundCatch. *)
      in
      (t, G.CatchPattern (G.PatWildcard t), v1)

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
            |> List_.map (fun x ->
                   let a, b = var_of_var x in
                   G.ForInitVar (a, b))
          in
          G.ForClassic (vars, v2, v3)
      | Right e ->
          let e = expr e in
          G.ForClassic ([ G.ForInitExpr e ], v2, v3))
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
      let e =
        G.Call (G.IdSpecial (G.ForOf, t) |> G.e, fb [ G.Arg v2 ]) |> G.e
      in
      G.ForEach (pattern, t, e)
  | ForEllipsis v1 -> G.ForEllipsis v1

and case = function
  | Case (t, v1, v2) ->
      let v1 = expr v1 and v2 = stmt v2 in
      G.CasesAndBody ([ G.Case (t, H.expr_to_pattern v1) ], v2)
  | Default (t, v1) ->
      let v1 = stmt v1 in
      G.CasesAndBody ([ G.Default t ], v1)

(* used to be an AST_generic.type_ with no conversion needed, but now that
 * we moved AST_generic.ml out of pfff, we need the boilerplate below
 *)
and type_ x =
  match x with
  | TyBuiltin id -> G.ty_builtin (ident id)
  | TyName xs -> G.TyN (H.name_of_ids xs) |> G.t
  (* TODO: use TyExpr now? or special TyLiteral? *)
  | TyLiteral l ->
      let l = G.L (literal l) in
      G.OtherType (("LitType", Tok.unsafe_fake_tok ""), [ G.E (l |> G.e) ])
      |> G.t
  | TyQuestion (tok, t) ->
      let t = type_ t in
      G.TyQuestion (t, tok) |> G.t
  | TyArray (t, (lt, (), rt)) ->
      let t = type_ t in
      G.TyArray ((lt, None, rt), t) |> G.t
  | TyTuple (lt, xs, rt) ->
      let xs = List_.map tuple_type_member xs in
      G.TyTuple (lt, xs, rt) |> G.t
  | TyFun (params, typ_opt) ->
      let params = List_.map parameter_binding params in
      let rett =
        match typ_opt with
        | None -> G.ty_builtin ("void", Tok.unsafe_fake_tok "void")
        | Some t -> type_ t
      in
      G.TyFun (params, rett) |> G.t
  | TyRecordAnon (lt, properties, rt) ->
      G.TyRecordAnon
        ((G.Class, Tok.fake_tok lt ""), (lt, List_.map property properties, rt))
      |> G.t
  | TyOr (t1, tk, t2) ->
      let t1 = type_ t1 in
      let t2 = type_ t2 in
      G.TyOr (t1, tk, t2) |> G.t
  | TyAnd (t1, tk, t2) ->
      let t1 = type_ t1 in
      let t2 = type_ t2 in
      G.TyAnd (t1, tk, t2) |> G.t
  | TypeTodo (categ, xs) -> G.OtherType (categ, List_.map any xs) |> G.t

and tuple_type_member x =
  match x with
  | TyTupMember x -> type_ x
  | TyTupOpt (x, tok) -> G.TyQuestion (type_ x, tok) |> G.t
  | TyTupRest (tok, x) -> G.TyRest (tok, type_ x) |> G.t

and entity { name = n; attrs } =
  let n = name n in
  let attrs = list attribute attrs in
  G.basic_entity n ~attrs

and definition (ent, def) =
  let ent = entity ent in
  match def with
  | VarDef { v_kind = x_kind; v_init = x_init; v_type = ty } ->
      let v2 = var_kind x_kind in
      let ty = option type_ ty in
      let v3 = option expr x_init in
      ( { ent with G.attrs = v2 :: ent.G.attrs },
        G.VarDef { G.vinit = v3; vtype = ty; vtok = G.no_sc } )
  | FuncDef def ->
      let def, more_attrs = fun_ def in
      ({ ent with G.attrs = ent.G.attrs @ more_attrs }, G.FuncDef def)
  | ClassDef def ->
      let def, more_attrs = class_ def in
      ({ ent with G.attrs = ent.G.attrs @ more_attrs }, G.ClassDef def)
  | DefTodo (v1, v2) ->
      let v2 = list any v2 in
      (ent, G.OtherDef (v1, v2))

and var_of_var
    ({ name = x_name; attrs }, { v_kind = x_kind; v_init = x_init; v_type }) =
  let v1 = name x_name in
  let attrs = list attribute attrs in
  let v2 = var_kind x_kind in
  let ent = G.basic_entity v1 ~attrs:(v2 :: attrs) in
  let v3 = option expr x_init in
  let v_type = option type_ v_type in
  (ent, { G.vinit = v3; vtype = v_type; vtok = G.no_sc })

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
  let v1 = list attribute f_props in
  let v2 = bracket (list parameter_binding) f_params in
  let v3 = stmt f_body |> as_block in
  let frettype = option type_ f_rettype in
  ({ G.fparams = v2; frettype; fbody = G.FBStmt v3; fkind = f_kind }, v1)

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
      | None -> G.Param pclassic
      | Some tok -> G.ParamRest (tok, pclassic))

and argument x = expr x

and attribute = function
  | KeywordAttr x -> G.KeywordAttr (keyword_attribute x)
  | NamedAttr (t, ids, opt) ->
      let t1, args, t2 =
        match opt with
        | Some x -> x
        | None -> fb []
      in
      let args = list argument args |> List_.map G.arg in
      let name = H.name_of_ids ids in
      G.NamedAttr (t, name, (t1, args, t2))

and keyword_attribute (x, tok) =
  ( (match x with
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
    | Override -> G.Override
    | NotNull -> G.NotNull),
    tok )

and obj_ v = bracket (list property) v

and parent = function
  | Left e ->
      let e = expr e in
      H.expr_to_class_parent e
  | Right t -> (type_ t, None)

and class_ { c_extends; c_implements; c_body; c_kind; c_attrs } =
  let cextends = list parent c_extends in
  let v2 = bracket (list property) c_body in
  let attrs = list attribute c_attrs in
  let cimplements = list type_ c_implements in
  ( {
      G.ckind = c_kind;
      cextends;
      cimplements;
      cmixins = [];
      cparams = fb [];
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
    | Left n -> G.basic_entity n ~attrs:v2
    | Right e -> { G.name = G.EDynamic e; attrs = v2; tparams = None }
  in
  match v3 with
  | Some (Fun (def, None)) ->
      let def, more_attrs = fun_ def in
      let fkind, tok = def.G.fkind in
      let fkind =
        match fkind with
        | G.Function -> G.Method
        | x -> x
      in
      ( { ent with G.attrs = ent.G.attrs @ more_attrs },
        G.FuncDef { def with G.fkind = (fkind, tok) } )
  | _ ->
      let v3 = option expr v3 in
      (ent, G.VarDef { G.vinit = v3; vtype = vt; vtok = G.no_sc })

and property x =
  match x with
  | Field v1 ->
      let ent, def = field_classic v1 in
      G.fld (ent, def)
  | FieldColon v1 ->
      let ent, def = field_classic v1 in
      let def =
        match def with
        (* ugly: this is to prevent assignment to match object field
         * definitions in semgrep *)
        | G.VarDef x -> G.FieldDefColon x
        | _ -> def
      in
      G.fld (ent, def)
  | FieldSpread (t, v1) ->
      let v1 = expr v1 in
      let spec = (G.Spread, t) in
      let e = G.special spec [ v1 ] in
      let st = G.exprstmt e in
      G.F st
  | FieldEllipsis v1 -> G.field_ellipsis v1
  | FieldPatDefault (v1, _v2, v3) ->
      let v1 = pattern v1 in
      let v3 = expr v3 in
      G.F (G.exprstmt (G.LetPattern (v1, v3) |> G.e))
  | FieldTodo (v1, v2) ->
      let v2 = stmt v2 in
      (* hmm, should use OtherStmtWithStmt ? *)
      G.F (G.OtherStmt (G.OS_Todo, [ G.TodoK v1; G.S v2 ]) |> G.s)

and alias v1 =
  let v1 = name v1 in
  (v1, G.empty_id_info ())

and module_directive x =
  match x with
  | ReExportNamespace (v1, _v2, _opt_alias, _v3, v4) ->
      let v4 = filename v4 in
      G.OtherDirective (("ReExportNamespace", v1), [ G.Str (fb v4) ])
  | Import (t, v1, v2) ->
      let v1 =
        List_.map
          (fun (v1, v2) ->
            let v1 = name v1 and v2 = option name v2 in
            H.mk_import_from_kind v1 v2)
          v1
      in
      let v2 = filename v2 in
      G.ImportFrom (t, G.FileName v2, v1)
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
      G.OtherDirective (("Export", t), [ G.I v1 ])

and list_stmt xs =
  (* converting require() in import, so they can benefit from the
   * other goodies coming with import in semgrep (e.g., equivalence aliasing)
   *)
  xs |> List_.map (fun st -> [ stmt st ]) |> List_.flatten

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
  | PartialSwitchCase v1 ->
      let v1 = case v1 in
      G.PartialSwitchCase v1

and any = function
  | Property v1 ->
      let v1 = property v1 in
      G.Fld v1
  | Expr v1 ->
      let v1 = expr v1 in
      G.E v1
  | Stmt v1 ->
      let v1 = stmt v1 in
      G.S v1
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
