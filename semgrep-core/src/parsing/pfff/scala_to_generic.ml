(* Yoann Padioleau
 *
 * Copyright (C) 2021 r2c
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
open AST_scala
module G = AST_generic
module H = AST_generic_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* AST_scala to AST_generic.
 *
 * See AST_generic.ml for more information.
 *)

[@@@warning "-27-26-32"]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let name_of_id id = G.Id (id, G.empty_id_info ())

let name_of_ids ids = H.name_of_ids ids

let ids_of_name = function
  | G.Id (id, _) -> [ id ]
  | G.IdQualified ((id, info), _) -> (
      match info.G.name_qualifier with
      | None -> [ id ]
      | Some (G.QTop _) -> [ id ]
      | Some (G.QDots xs) -> xs @ [ id ]
      | Some (G.QExpr _) ->
          raise Impossible (* H.name_of_ids can't generate this *) )

let error = G.error

let todo _ = failwith "TODO"

let fake = G.fake

let fb = G.fake_bracket

let id x = x

let v_string = id

let v_int = id

let v_float = id

let v_bool = id

let v_list = List.map

let v_option = Common.map_opt

(*****************************************************************************)
(* Boilerplate *)
(*****************************************************************************)

(* generated by ocamltarzan with: camlp4o -o /tmp/yyy.ml -I pa/ pa_type_conv.cmo pa_visitor.cmo  pr_o.cmo /tmp/xxx.ml  *)

let v_tok v = v

let v_wrap _of_a (v1, v2) =
  let v1 = _of_a v1 and v2 = v_tok v2 in
  (v1, v2)

let v_bracket _of_a (v1, v2, v3) =
  let v1 = v_tok v1 and v2 = _of_a v2 and v3 = v_tok v3 in
  (v1, v2, v3)

let v_ident v = v_wrap v_string v

let v_op v = v_wrap v_string v

let v_varid v = v_wrap v_string v

let v_ident_or_wildcard v = v_ident v

let v_varid_or_wildcard v = v_ident v

let v_ident_or_this v = v_ident v

let v_dotted_ident v = v_list v_ident v

let v_qualified_ident v = v_dotted_ident v

let v_selectors v = v_dotted_ident v

let v_simple_ref = function
  | Id v1 ->
      let v1 = v_ident v1 in
      Left v1
  | This (v1, v2) ->
      let v1TODO = v_option v_ident v1 and v2 = v_tok v2 in
      Right (G.IdSpecial (G.This, v2))
  | Super (v1, v2, v3, v4) ->
      let v1TODO = v_option v_ident v1
      and v2 = v_tok v2
      and v3TODO = v_option (v_bracket v_ident) v3
      and v4 = v_ident v4 in
      let fld = G.EN (G.Id (v4, G.empty_id_info ())) in
      Right (G.DotAccess (G.IdSpecial (G.Super, v2), fake ".", fld))

let tok_of_simple_ref = function
  | Id (_, t) -> t
  | This (_, t) -> t
  | Super (_, t, _, _) -> t

let v_path (v1, v2) =
  let v1 = v_simple_ref v1 and v2 = v_selectors v2 in
  (v1, v2)

let v_stable_id v = v_path v

let rec v_import_selector (v1, v2) =
  let v1 = v_ident_or_wildcard v1 and v2 = v_option v_alias v2 in
  (v1, v2)

and v_alias (v1, v2) =
  let v1 = v_tok v1 and v2 = v_ident_or_wildcard v2 in
  (v1, v2)

let dotted_name_of_stable_id (v1, v2) =
  match v_simple_ref v1 with
  | Left id -> id :: v2
  | Right _ ->
      let tk = tok_of_simple_ref v1 in
      error tk "complex stable id not handled"

let rec v_import_expr tk (v1, v2) =
  let module_name = G.DottedName (dotted_name_of_stable_id v1) in
  let v2 = v_import_spec v2 in
  v2 tk module_name

and v_import_spec = function
  | ImportId v1 ->
      let v1 = v_ident v1 in
      fun tk path -> [ G.ImportFrom (tk, path, v1, None) ]
  | ImportWildcard v1 ->
      let v1 = v_tok v1 in
      fun tk path -> [ G.ImportAll (tk, path, v1) ]
  | ImportSelectors (_, v1, _) ->
      let v1 = (v_list v_import_selector) v1 in
      fun tk path ->
        v1
        |> List.map (fun (id, opt) ->
               let alias =
                 match opt with
                 | None -> None
                 | Some (_, id) -> Some (id, G.empty_id_info ())
               in
               G.ImportFrom (tk, path, id, alias))

let v_import (v1, v2) =
  let v1 = v_tok v1 in
  let v2 = v_list (v_import_expr v1) v2 in
  v2

let v_package (v1, v2) =
  let v1 = v_tok v1 and v2 = v_qualified_ident v2 in
  (v1, v2)

let rec v_literal = function
  | Int v1 ->
      let v1 = v_wrap (v_option v_int) v1 in
      Left (G.Int v1)
  | Float v1 ->
      let v1 = v_wrap (v_option v_float) v1 in
      Left (G.Float v1)
  | Char v1 ->
      let v1 = v_wrap v_string v1 in
      Left (G.Char v1)
  | String v1 ->
      let v1 = v_wrap v_string v1 in
      Left (G.String v1)
  | Bool v1 ->
      let v1 = v_wrap v_bool v1 in
      Left (G.Bool v1)
  | Null v1 ->
      let v1 = v_tok v1 in
      Left (G.Null v1)
  | Interpolated (v1, v2, v3) ->
      let v1 = v_ident v1 and v2 = v_list v_encaps v2 and v3 = v_tok v3 in
      let special = G.IdSpecial (G.ConcatString G.FString, snd v1) in
      let args =
        v2
        |> List.map (function
             | Left lit -> G.Arg (G.L lit)
             | Right e ->
                 let special = G.IdSpecial (G.InterpolatedElement, fake "") in
                 G.Arg (G.Call (special, fb [ G.Arg e ])))
      in
      Right (G.Call (special, (snd v1, args, v3)))

and v_encaps = function
  | EncapsStr v1 ->
      let v1 = v_wrap v_string v1 in
      Left (G.String v1)
  | EncapsDollarIdent v1 ->
      let v1 = v_ident v1 in
      let name = name_of_id v1 in
      Right (G.N name)
  | EncapsExpr v1 ->
      (* always a Block *)
      let v1 = v_expr v1 in
      Right v1

and todo_type msg any = G.OtherType (G.OT_Todo, G.TodoK (msg, fake msg) :: any)

and v_type_ = function
  | TyLiteral v1 -> (
      let v1 = v_literal v1 in
      match v1 with
      | Left lit -> todo_type "TyLiteralLit" [ G.E (G.L lit) ]
      | Right e -> todo_type "TyLiteralExpr" [ G.E e ] )
  | TyName v1 ->
      let xs = dotted_name_of_stable_id v1 in
      let name = name_of_ids xs in
      G.TyN name
  | TyProj (v1, v2, v3) ->
      let v1 = v_type_ v1 and v2 = v_tok v2 and v3 = v_ident v3 in
      todo_type "TyProj" [ G.T v1; G.I v3 ]
  | TyApplied (v1, v2) -> (
      let v1 = v_type_ v1 and v2 = v_bracket (v_list v_type_) v2 in
      let xs = G.unbracket v2 in
      let args = xs |> List.map (fun x -> G.TypeArg x) in
      match v1 with
      | G.TyN n ->
          let ids = ids_of_name n in
          G.TyNameApply (ids, args)
      | _ ->
          todo_type "TyAppliedComplex"
            (G.T v1 :: (xs |> List.map (fun x -> G.T x))) )
  | TyInfix (v1, v2, v3) ->
      let v1 = v_type_ v1 and v2 = v_ident v2 and v3 = v_type_ v3 in
      G.TyNameApply ([ v2 ], [ G.TypeArg v1; G.TypeArg v3 ])
  | TyFunction1 (v1, v2, v3) ->
      let v1 = v_type_ v1 and v2 = v_tok v2 and v3 = v_type_ v3 in
      G.TyFun ([ G.ParamClassic (G.param_of_type v1) ], v3)
  | TyFunction2 (v1, v2, v3) ->
      let v1 = v_bracket (v_list v_type_) v1
      and v2 = v_tok v2
      and v3 = v_type_ v3 in
      let ts =
        v1 |> G.unbracket
        |> List.map (fun t -> G.ParamClassic (G.param_of_type t))
      in
      G.TyFun (ts, v3)
  | TyTuple v1 ->
      let v1 = v_bracket (v_list v_type_) v1 in
      G.TyTuple v1
  | TyRepeated (v1, v2) ->
      let v1 = v_type_ v1 and v2 = v_tok v2 in
      todo_type "TyRepeated" [ G.T v1; G.Tk v2 ]
  | TyAnnotated (v1, v2) ->
      let v1 = v_type_ v1 and _v2TODO = v_list v_annotation v2 in
      v1
  | TyRefined (v1, v2) ->
      let v1 = v_option v_type_ v1 and lb, xs, rb = v_refinement v2 in
      error lb "TyRefined not handled"
  | TyExistential (v1, v2, v3) ->
      let v1 = v_type_ v1 and v2 = v_tok v2 and _v3TODO = v_refinement v3 in
      v1
  | TyWith (v1, v2, v3) ->
      let v1 = v_type_ v1 and v2 = v_tok v2 and v3 = v_type_ v3 in
      G.TyAnd (v1, v2, v3)
  | TyWildcard (v1, v2) ->
      let v1 = v_tok v1 and _v2TODO = v_type_bounds v2 in
      G.TyAny v1

and v_refinement v = v_bracket (v_list v_refine_stat) v

and v_refine_stat v = v_definition v

and v_type_bounds { supertype = v_supertype; subtype = v_subtype } =
  let arg1 =
    v_option
      (fun (v1, v2) ->
        let v1 = v_tok v1 and v2 = v_type_ v2 in
        ())
      v_supertype
  in
  let arg2 =
    v_option
      (fun (v1, v2) ->
        let v1 = v_tok v1 and v2 = v_type_ v2 in
        ())
      v_subtype
  in
  (arg1, arg2)

and v_ascription v = v_type_ v

and todo_pattern msg any = G.OtherPat (G.OP_Todo, G.TodoK (msg, fake msg) :: any)

and v_pattern = function
  | PatLiteral v1 -> (
      let v1 = v_literal v1 in
      match v1 with
      | Left lit -> G.PatLiteral lit
      | Right e -> todo_pattern "PatLiteralExpr" [ G.E e ] )
  | PatName v1 ->
      let ids = dotted_name_of_stable_id v1 in
      G.PatConstructor (ids, [])
  | PatTuple v1 ->
      let v1 = v_bracket (v_list v_pattern) v1 in
      G.PatTuple v1
  | PatVarid v1 ->
      let v1 = v_varid_or_wildcard v1 in
      G.PatId (v1, G.empty_id_info ())
  | PatTypedVarid (v1, v2, v3) ->
      let v1 = v_varid_or_wildcard v1 and v2 = v_tok v2 and v3 = v_type_ v3 in
      let p1 = G.PatId (v1, G.empty_id_info ()) in
      G.PatTyped (p1, v3)
  | PatBind (v1, v2, v3) ->
      let v1 = v_varid v1 and v2 = v_tok v2 and v3 = v_pattern v3 in
      G.PatAs (v3, (v1, G.empty_id_info ()))
  | PatApply (v1, v2, v3) ->
      let ids = dotted_name_of_stable_id v1 in
      let _v2TODO = v_option (v_bracket (v_list v_type_)) v2 in
      let v3 = v_option (v_bracket (v_list v_pattern)) v3 in
      let xs = match v3 with None -> [] | Some (_, xs, _) -> xs in
      G.PatConstructor (ids, xs)
  | PatInfix (v1, v2, v3) ->
      let v1 = v_pattern v1 and v2 = v_ident v2 and v3 = v_pattern v3 in
      G.PatConstructor ([ v2 ], [ v1; v3 ])
  | PatUnderscoreStar (v1, v2) ->
      let v1 = v_tok v1 and v2 = v_tok v2 in
      todo_pattern "PatUnderscoreStar" [ G.Tk v1; G.Tk v2 ]
  | PatDisj (v1, v2, v3) ->
      let v1 = v_pattern v1 and v2 = v_tok v2 and v3 = v_pattern v3 in
      G.PatDisj (v1, v3)

and v_expr = function
  | L v1 -> (
      let v1 = v_literal v1 in
      match v1 with Left lit -> G.L lit | Right e -> e )
  | Tuple v1 ->
      let v1 = v_bracket (v_list v_expr) v1 in
      todo ()
  | Name v1 ->
      let v1 = v_path v1 in
      todo ()
  | ExprUnderscore v1 ->
      let v1 = v_tok v1 in
      todo ()
  | InstanciatedExpr (v1, v2) ->
      let v1 = v_expr v1 and v2 = v_bracket (v_list v_type_) v2 in
      todo ()
  | TypedExpr (v1, v2, v3) ->
      let v1 = v_expr v1 and v2 = v_tok v2 and v3 = v_ascription v3 in
      todo ()
  | DotAccess (v1, v2, v3) ->
      let v1 = v_expr v1 and v2 = v_tok v2 and v3 = v_ident v3 in
      todo ()
  | Apply (v1, v2) ->
      let v1 = v_expr v1 and v2 = v_list v_arguments v2 in
      todo ()
  | Infix (v1, v2, v3) ->
      let v1 = v_expr v1 and v2 = v_ident v2 and v3 = v_expr v3 in
      todo ()
  | Prefix (v1, v2) ->
      let v1 = v_op v1 and v2 = v_expr v2 in
      todo ()
  | Postfix (v1, v2) ->
      let v1 = v_expr v1 and v2 = v_ident v2 in
      todo ()
  | Assign (v1, v2, v3) ->
      let v1 = v_lhs v1 and v2 = v_tok v2 and v3 = v_expr v3 in
      todo ()
  | Match (v1, v2, v3) ->
      let v1 = v_expr v1
      and v2 = v_tok v2
      and v3 = v_bracket v_case_clauses v3 in
      todo ()
  | Lambda v1 ->
      let v1 = v_function_definition v1 in
      todo ()
  | New (v1, v2) ->
      let v1 = v_tok v1 and v2 = v_template_definition v2 in
      todo ()
  | BlockExpr v1 ->
      let v1 = v_block_expr v1 in
      todo ()
  | S v1 ->
      let v1 = v_stmt v1 in
      todo ()

and v_lhs v = v_expr v

and v_arguments = function
  | Args v1 ->
      let v1 = v_bracket (v_list v_argument) v1 in
      ()
  | ArgBlock v1 ->
      let v1 = v_block_expr v1 in
      ()

and v_argument v = v_expr v

and v_case_clauses v = v_list v_case_clause v

and v_case_clause
    {
      casetoks = v_casetoks;
      casepat = v_casepat;
      caseguard = v_caseguard;
      casebody = v_casebody;
    } =
  let arg =
    match v_casetoks with
    | v1, v2 ->
        let v1 = v_tok v1 and v2 = v_tok v2 in
        ()
  in
  let arg = v_pattern v_casepat in
  let arg = v_option v_guard v_caseguard in
  let arg = v_block v_casebody in
  ()

and v_guard (v1, v2) =
  let v1 = v_tok v1 and v2 = v_expr v2 in
  ()

and v_block_expr v = v_bracket v_block_expr_kind v

and v_block_expr_kind = function
  | BEBlock v1 ->
      let v1 = v_block v1 in
      ()
  | BECases v1 ->
      let v1 = v_case_clauses v1 in
      ()

and v_stmt = function
  | Block v1 ->
      let v1 = v_bracket v_block v1 in
      ()
  | If (v1, v2, v3, v4) ->
      let v1 = v_tok v1
      and v2 = v_bracket v_expr v2
      and v3 = v_expr v3
      and v4 =
        v_option
          (fun (v1, v2) ->
            let v1 = v_tok v1 and v2 = v_expr v2 in
            ())
          v4
      in
      ()
  | While (v1, v2, v3) ->
      let v1 = v_tok v1 and v2 = v_bracket v_expr v2 and v3 = v_expr v3 in
      ()
  | DoWhile (v1, v2, v3, v4) ->
      let v1 = v_tok v1
      and v2 = v_expr v2
      and v3 = v_tok v3
      and v4 = v_bracket v_expr v4 in
      ()
  | For (v1, v2, v3) ->
      let v1 = v_tok v1
      and v2 = v_bracket v_enumerators v2
      and v3 = v_for_body v3 in
      ()
  | Return (v1, v2) ->
      let v1 = v_tok v1 and v2 = v_option v_expr v2 in
      ()
  | Try (v1, v2, v3, v4) ->
      let v1 = v_tok v1
      and v2 = v_expr v2
      and v3 = v_option v_catch_clause v3
      and v4 = v_option v_finally_clause v4 in
      ()
  | Throw (v1, v2) ->
      let v1 = v_tok v1 and v2 = v_expr v2 in
      ()

and v_enumerators v = v_list v_enumerator v

and v_enumerator = function
  | G v1 ->
      let v1 = v_generator v1 in
      ()
  | GIf v1 ->
      let v1 = v_list v_guard v1 in
      ()

and v_generator
    {
      genpat = v_genpat;
      gentok = v_gentok;
      genbody = v_genbody;
      genguards = v_genguards;
    } =
  let arg = v_pattern v_genpat in
  let arg = v_tok v_gentok in
  let arg = v_expr v_genbody in
  let arg = v_list v_guard v_genguards in
  ()

and v_for_body = function
  | Yield (v1, v2) ->
      let v1 = v_tok v1 and v2 = v_expr v2 in
      ()
  | NoYield v1 ->
      let v1 = v_expr v1 in
      ()

and v_catch_clause (v1, v2) =
  let v1 = v_tok v1 and v2 = v_expr v2 in
  ()

and v_finally_clause (v1, v2) =
  let v1 = v_tok v1 and v2 = v_expr v2 in
  ()

and v_block v = v_list v_block_stat v

and v_block_stat = function
  | D v1 ->
      let v1 = v_definition v1 in
      ()
  | I v1 ->
      let v1 = v_import v1 in
      ()
  | E v1 ->
      let v1 = v_expr v1 in
      ()
  | Package v1 ->
      let v1 = v_package v1 in
      ()
  | Packaging (v1, v2) ->
      let v1 = v_package v1 and v2 = v_bracket (v_list v_top_stat) v2 in
      ()

and v_template_stat v = v_block_stat v

and v_top_stat v = v_block_stat v

and v_modifier v = v_wrap v_modifier_kind v

and v_modifier_kind = function
  | Abstract -> ()
  | Final -> ()
  | Sealed -> ()
  | Implicit -> ()
  | Lazy -> ()
  | Private v1 ->
      let v1 = v_option (v_bracket v_ident_or_this) v1 in
      ()
  | Protected v1 ->
      let v1 = v_option (v_bracket v_ident_or_this) v1 in
      ()
  | Override -> ()
  | CaseClassOrObject -> ()
  | PackageObject -> ()
  | Val -> ()
  | Var -> ()

and v_annotation (v1, v2, v3) =
  let v1 = v_tok v1 and v2 = v_type_ v2 and v3 = v_list v_arguments v3 in
  ()

and v_attribute = function
  | A v1 ->
      let v1 = v_annotation v1 in
      ()
  | M v1 ->
      let v1 = v_modifier v1 in
      ()

and v_type_parameter
    {
      tpname = v_tpname;
      tpvariance = v_tpvariance;
      tpannots = v_tpannots;
      tpparams = v_tpparams;
      tpbounds = v_tpbounds;
      tpviewbounds = v_tpviewbounds;
      tpcolons = v_tpcolons;
    } =
  let arg = v_ident_or_wildcard v_tpname in
  let arg = v_option (v_wrap v_variance) v_tpvariance in
  let arg = v_list v_annotation v_tpannots in
  let arg = v_type_parameters v_tpparams in
  let arg = v_type_bounds v_tpbounds in
  let arg = v_list v_type_ v_tpviewbounds in
  let arg = v_list v_type_ v_tpcolons in
  ()

and v_variance = function Covariant -> () | Contravariant -> ()

and v_type_parameters v = v_option (v_bracket (v_list v_type_parameter)) v

and v_definition = function
  | DefEnt (v1, v2) ->
      let v1 = v_entity v1 and v2 = v_definition_kind v2 in
      ()
  | VarDefs v1 ->
      let v1 = v_variable_definitions v1 in
      ()

and v_variable_definitions
    {
      vpatterns = v_vpatterns;
      vattrs = v_vattrs;
      vtype = v_vtype;
      vbody = v_vbody;
    } =
  let arg = v_list v_pattern v_vpatterns in
  let arg = v_list v_attribute v_vattrs in
  let arg = v_option v_type_ v_vtype in
  let arg = v_option v_expr v_vbody in
  ()

and v_entity { name = v_name; attrs = v_attrs; tparams = v_tparams } =
  let arg = v_ident v_name in
  let arg = v_list v_attribute v_attrs in
  let arg = v_type_parameters v_tparams in
  ()

and v_definition_kind = function
  | FuncDef v1 ->
      let v1 = v_function_definition v1 in
      ()
  | TypeDef v1 ->
      let v1 = v_type_definition v1 in
      ()
  | Template v1 ->
      let v1 = v_template_definition v1 in
      ()

and v_function_definition
    {
      fkind = v_fkind;
      fparams = v_fparams;
      frettype = v_frettype;
      fbody = vfbody;
    } =
  let arg = v_wrap v_function_kind v_fkind in
  let arg = v_list v_bindings v_fparams in
  let arg = v_option v_type_ v_frettype in
  let arg = v_option v_fbody vfbody in
  ()

and v_function_kind = function LambdaArrow -> () | Def -> ()

and v_fbody = function
  | FBlock v1 ->
      let v1 = v_block_expr v1 in
      ()
  | FExpr (v1, v2) ->
      let v1 = v_tok v1 and v2 = v_expr v2 in
      ()

and v_bindings v = v_bracket (v_list v_binding) v

and v_binding
    {
      p_name = v_p_name;
      p_attrs = v_p_attrs;
      p_type = v_p_type;
      p_default = v_p_default;
    } =
  let arg = v_ident_or_wildcard v_p_name in
  let arg = v_list v_attribute v_p_attrs in
  let arg = v_option v_param_type v_p_type in
  let arg = v_option v_expr v_p_default in
  ()

and v_param_type = function
  | PT v1 ->
      let v1 = v_type_ v1 in
      ()
  | PTByNameApplication (v1, v2) ->
      let v1 = v_tok v1 and v2 = v_type_ v2 in
      ()
  | PTRepeatedApplication (v1, v2) ->
      let v1 = v_type_ v1 and v2 = v_tok v2 in
      ()

and v_template_definition
    {
      ckind = v_ckind;
      cparams = v_cparams;
      cparents = v_cparents;
      cbody = v_cbody;
    } =
  let arg = v_wrap v_template_kind v_ckind in
  let arg = v_list v_bindings v_cparams in
  let arg = v_template_parents v_cparents in
  let arg = v_option v_template_body v_cbody in
  ()

and v_template_parents { cextends = v_cextends; cwith = v_cwith } =
  let arg =
    v_option
      (fun (v1, v2) ->
        let v1 = v_type_ v1 and v2 = v_list v_arguments v2 in
        ())
      v_cextends
  in
  let arg = v_list v_type_ v_cwith in
  ()

and v_template_body v =
  v_bracket
    (fun (v1, v2) ->
      let v1 = v_option v_self_type v1 and v2 = v_block v2 in
      ())
    v

and v_self_type (v1, v2, v3) =
  let v1 = v_ident_or_this v1 and v2 = v_option v_type_ v2 and v3 = v_tok v3 in
  ()

and v_template_kind = function
  | Class -> ()
  | Trait -> ()
  | Object -> ()
  | Singleton -> ()

and v_type_definition { ttok = v_ttok; tbody = v_tbody } =
  let arg = v_tok v_ttok in
  let arg = v_type_definition_kind v_tbody in
  ()

and v_type_definition_kind = function
  | TDef (v1, v2) ->
      let v1 = v_tok v1 and v2 = v_type_ v2 in
      ()
  | TDcl v1 ->
      let v1 = v_type_bounds v1 in
      ()

let v_program v = v_list v_top_stat v

let v_any = function
  | Program v1 ->
      let v1 = v_program v1 in
      ()
  | Tk v1 ->
      let v1 = v_tok v1 in
      ()

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let program xs = failwith "TODO"

let any x = failwith "TODO"
