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
 *
 * TODO:
 * - see TODO, especially generators, This/Super class, etc.
 * - Scala can have multiple parameter lists or argument lists. Right now
 *   In Call position we fold, but for the parameters we flatten.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let fake = G.fake

let fb = G.fake_bracket

let id x = x

let v_string = id

let v_int = id

let v_float = id

let v_bool = id

let v_list = List.map

let v_option = Common.map_opt

let cases_to_lambda lb (cases : G.action list) : G.function_definition =
  let id = ("!hidden_scala_param!", lb) in
  let param = G.ParamClassic (G.param_of_id id) in
  let body = G.exprstmt (G.MatchPattern (G.N (H.name_of_id id), cases)) in
  {
    fkind = (G.BlockCases, lb);
    frettype = None;
    fparams = [ param ];
    fbody = body;
  }

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
      let _v1TODO = v_option v_ident v1 and v2 = v_tok v2 in
      Right (G.IdSpecial (G.This, v2))
  | Super (v1, v2, v3, v4) ->
      let _v1TODO = v_option v_ident v1
      and v2 = v_tok v2
      and _v3TODO = v_option (v_bracket v_ident) v3
      and v4 = v_ident v4 in
      let fld = G.EN (G.Id (v4, G.empty_id_info ())) in
      Right (G.DotAccess (G.IdSpecial (G.Super, v2), fake ".", fld))

(* TODO: should not use *)
let id_of_simple_ref = function
  | Id id -> id
  | This (_, t) -> ("this", t)
  | Super (_, t, _, _) -> ("super", t)

let v_path (v1, v2) =
  let v1 = v_simple_ref v1 and v2 = v_selectors v2 in
  (v1, v2)

let rec v_import_selector (v1, v2) =
  let v1 = v_ident_or_wildcard v1 and v2 = v_option v_alias v2 in
  (v1, v2)

and v_alias (v1, v2) =
  let v1 = v_tok v1 and v2 = v_ident_or_wildcard v2 in
  (v1, v2)

let v_dotted_name_of_stable_id (v1, v2) =
  let id = id_of_simple_ref v1 in
  id :: v2

let rec v_import_expr tk (v1, v2) =
  let module_name = G.DottedName (v_dotted_name_of_stable_id v1) in
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

let v_import (v1, v2) : G.directive list =
  let v1 = v_tok v1 in
  let v2 = v_list (v_import_expr v1) v2 in
  List.flatten v2

let v_package (v1, v2) =
  let v1 = v_tok v1 and v2 = v_qualified_ident v2 in
  (v1, v2)

let rec v_literal = function
  | Symbol (tquote, id) -> Left (G.Atom (tquote, id))
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
      let name = H.name_of_id v1 in
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
      | Right e -> todo_type "TyLiteralExpr" [ G.E e ])
  | TyName v1 ->
      let xs = v_dotted_name_of_stable_id v1 in
      let name = H.name_of_ids xs in
      G.TyN name
  | TyProj (v1, v2, v3) ->
      let v1 = v_type_ v1 and _v2 = v_tok v2 and v3 = v_ident v3 in
      todo_type "TyProj" [ G.T v1; G.I v3 ]
  | TyApplied (v1, v2) -> (
      let v1 = v_type_ v1 and v2 = v_bracket (v_list v_type_) v2 in
      let lp, xs, rp = v2 in
      let args = xs |> List.map (fun x -> G.TypeArg x) in
      match v1 with
      | G.TyN n -> G.TyApply (G.TyN n, (lp, args, rp))
      | _ ->
          todo_type "TyAppliedComplex"
            (G.T v1 :: (xs |> List.map (fun x -> G.T x))))
  | TyInfix (v1, v2, v3) ->
      let v1 = v_type_ v1 and v2 = v_ident v2 and v3 = v_type_ v3 in
      G.TyApply (G.TyN (H.name_of_ids [ v2 ]), fb [ G.TypeArg v1; G.TypeArg v3 ])
  | TyFunction1 (v1, v2, v3) ->
      let v1 = v_type_ v1 and _v2 = v_tok v2 and v3 = v_type_ v3 in
      G.TyFun ([ G.ParamClassic (G.param_of_type v1) ], v3)
  | TyFunction2 (v1, v2, v3) ->
      let v1 = v_bracket (v_list v_type_) v1
      and _v2 = v_tok v2
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
      let v1 = v_option v_type_ v1 and _lb, defs, _rb = v_refinement v2 in
      todo_type "TyRefined"
        ((match v1 with None -> [] | Some t -> [ G.T t ])
        @ (defs |> List.map (fun def -> G.Def def)))
  | TyExistential (v1, v2, v3) ->
      let v1 = v_type_ v1 in
      let _v2 = v_tok v2 in
      let _lb, defs, _rb = v_refinement v3 in
      todo_type "TyExistential" (G.T v1 :: (defs |> List.map (fun x -> G.Def x)))
  | TyWith (v1, v2, v3) ->
      let v1 = v_type_ v1 and v2 = v_tok v2 and v3 = v_type_ v3 in
      G.TyAnd (v1, v2, v3)
  | TyWildcard (v1, v2) ->
      let v1 = v_tok v1 and _v2TODO = v_type_bounds v2 in
      G.TyAny v1

and v_refinement v =
  v_bracket (fun xs -> v_list v_refine_stat xs |> List.flatten) v

and v_refine_stat v = v_definition v

and v_type_bounds { supertype = v_supertype; subtype = v_subtype } =
  let arg1 =
    v_option
      (fun (v1, v2) ->
        let v1 = v_tok v1 and v2 = v_type_ v2 in
        (v1, v2))
      v_supertype
  in
  let arg2 =
    v_option
      (fun (v1, v2) ->
        let v1 = v_tok v1 and v2 = v_type_ v2 in
        (v1, v2))
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
      | Right e -> todo_pattern "PatLiteralExpr" [ G.E e ])
  | PatName v1 ->
      let ids = v_dotted_name_of_stable_id v1 in
      G.PatConstructor (ids, [])
  | PatTuple v1 ->
      let v1 = v_bracket (v_list v_pattern) v1 in
      G.PatTuple v1
  | PatVarid v1 ->
      let v1 = v_varid_or_wildcard v1 in
      G.PatId (v1, G.empty_id_info ())
  | PatTypedVarid (v1, v2, v3) ->
      let v1 = v_varid_or_wildcard v1 and _v2 = v_tok v2 and v3 = v_type_ v3 in
      let p1 = G.PatId (v1, G.empty_id_info ()) in
      G.PatTyped (p1, v3)
  | PatBind (v1, v2, v3) ->
      let v1 = v_varid v1 and _v2 = v_tok v2 and v3 = v_pattern v3 in
      G.PatAs (v3, (v1, G.empty_id_info ()))
  | PatApply (v1, v2, v3) ->
      let ids = v_dotted_name_of_stable_id v1 in
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
      let v1 = v_pattern v1 and _v2 = v_tok v2 and v3 = v_pattern v3 in
      G.PatDisj (v1, v3)

and todo_expr msg any = G.OtherExpr (G.OE_Todo, G.TodoK (msg, fake msg) :: any)

and v_expr = function
  | Ellipsis v1 -> G.Ellipsis v1
  | DeepEllipsis v1 -> G.DeepEllipsis (v_bracket v_expr v1)
  | L v1 -> (
      let v1 = v_literal v1 in
      match v1 with Left lit -> G.L lit | Right e -> e)
  | Tuple v1 ->
      let v1 = v_bracket (v_list v_expr) v1 in
      G.Tuple v1
  | Name v1 ->
      let sref, ids = v_path v1 in
      let start =
        match sref with Left id -> G.N (H.name_of_id id) | Right e -> e
      in
      ids
      |> List.fold_left
           (fun acc fld -> G.DotAccess (acc, fake ".", G.EN (H.name_of_id fld)))
           start
  | ExprUnderscore v1 ->
      let v1 = v_tok v1 in
      todo_expr "ExprUnderscore" [ G.Tk v1 ]
  | InstanciatedExpr (v1, v2) ->
      let v1 = v_expr v1 and _, v2, _ = v_bracket (v_list v_type_) v2 in
      todo_expr "InstanciatedExpr" (G.E v1 :: List.map (fun t -> G.T t) v2)
  | TypedExpr (v1, v2, v3) ->
      let v1 = v_expr v1 and _v2 = v_tok v2 and v3 = v_ascription v3 in
      G.Cast (v3, v1)
  | DotAccess (v1, v2, v3) ->
      let v1 = v_expr v1 and v2 = v_tok v2 and v3 = v_ident v3 in
      let name = H.name_of_id v3 in
      G.DotAccess (v1, v2, G.EN name)
  | Apply (v1, v2) ->
      let v1 = v_expr v1 and v2 = v_list v_arguments v2 in
      v2 |> List.fold_left (fun acc xs -> G.Call (acc, xs)) v1
  | Infix (v1, v2, v3) ->
      let v1 = v_expr v1 and v2 = v_ident v2 and v3 = v_expr v3 in
      G.Call (G.N (H.name_of_id v2), fb [ G.Arg v1; G.Arg v3 ])
  | Prefix (v1, v2) ->
      let v1 = v_op v1 and v2 = v_expr v2 in
      G.Call (G.N (H.name_of_id v1), fb [ G.Arg v2 ])
  | Postfix (v1, v2) ->
      let v1 = v_expr v1 and v2 = v_ident v2 in
      G.Call (G.N (H.name_of_id v2), fb [ G.Arg v1 ])
  | Assign (v1, v2, v3) ->
      let v1 = v_lhs v1 and v2 = v_tok v2 and v3 = v_expr v3 in
      G.Assign (v1, v2, v3)
  | Match (v1, v2, v3) ->
      let v1 = v_expr v1
      and _v2 = v_tok v2
      and v3 = v_bracket v_case_clauses v3 in
      G.MatchPattern (v1, G.unbracket v3)
  | Lambda v1 ->
      let v1 = v_function_definition v1 in
      G.Lambda v1
  | New (v1, v2) ->
      let v1 = v_tok v1 and v2, argss = v_template_definition v2 in
      let cl = G.AnonClass v2 in
      let special = G.IdSpecial (G.New, v1) in
      let start = G.Call (special, fb [ G.Arg cl ]) in
      argss |> List.fold_left (fun acc args -> G.Call (acc, args)) start
  | BlockExpr v1 -> (
      let lb, kind, _rb = v_block_expr v1 in
      match kind with
      | Left stats -> expr_of_block stats
      | Right cases -> G.Lambda (cases_to_lambda lb cases))
  | S v1 ->
      let v1 = v_stmt v1 in
      G.OtherExpr (G.OE_StmtExpr, [ G.S v1 ])

(* alt: transform in a series of Seq? *)
and expr_of_block xs : G.expr =
  let st = G.Block (fb xs) |> G.s in
  G.OtherExpr (G.OE_StmtExpr, [ G.S st ])

and v_lhs v = v_expr v

and v_arguments = function
  | Args v1 ->
      let v1 = v_bracket (v_list v_argument) v1 in
      v1
  | ArgBlock v1 -> (
      let lb, kind, rb = v_block_expr v1 in
      match kind with
      | Left stats -> (lb, [ G.Arg (expr_of_block stats) ], rb)
      | Right cases -> (lb, [ G.Arg (G.Lambda (cases_to_lambda lb cases)) ], rb)
      )

and v_argument v =
  let v = v_expr v in
  G.Arg v

and v_case_clauses v = v_list v_case_clause v

and v_case_clause
    {
      casetoks = v_casetoks;
      casepat = v_casepat;
      caseguard = v_caseguard;
      casebody = v_casebody;
    } : G.action =
  let _icase, _iarrow =
    match v_casetoks with
    | v1, v2 ->
        let v1 = v_tok v1 and v2 = v_tok v2 in
        (v1, v2)
  in
  let pat = v_pattern v_casepat in
  let guardopt = v_option v_guard v_caseguard in
  let block = v_block v_casebody in
  let pat =
    match guardopt with None -> pat | Some (_t, e) -> PatWhen (pat, e)
  in
  (pat, expr_of_block block)

and v_guard (v1, v2) =
  let v1 = v_tok v1 and v2 = v_expr v2 in
  (v1, v2)

and v_block_expr v =
  let lb, xs, rb = v_bracket v_block_expr_kind v in
  (lb, xs, rb)

and v_block_expr_kind = function
  | BEBlock v1 ->
      let v1 = v_block v1 in
      Left v1
  | BECases v1 ->
      let v1 = v_case_clauses v1 in
      Right v1

and v_expr_for_stmt (e : expr) : G.stmt =
  match e with
  | S s -> v_stmt s
  | _ ->
      let e = v_expr e in
      G.ExprStmt (e, G.sc) |> G.s

and v_stmt = function
  | Block v1 ->
      let v1 = v_bracket v_block v1 in
      G.Block v1 |> G.s
  | If (v1, v2, v3, v4) ->
      let v1 = v_tok v1
      and v2 = v_bracket v_expr v2
      and v3 = v_expr_for_stmt v3
      and v4 =
        v_option
          (fun (v1, v2) ->
            let _v1 = v_tok v1 and v2 = v_expr_for_stmt v2 in
            v2)
          v4
      in
      G.If (v1, G.unbracket v2, v3, v4) |> G.s
  | While (v1, v2, v3) ->
      let v1 = v_tok v1
      and v2 = v_bracket v_expr v2
      and v3 = v_expr_for_stmt v3 in
      G.While (v1, G.unbracket v2, v3) |> G.s
  | DoWhile (v1, v2, v3, v4) ->
      let v1 = v_tok v1
      and v2 = v_expr_for_stmt v2
      and _v3 = v_tok v3
      and v4 = v_bracket v_expr v4 in
      G.DoWhile (v1, v2, G.unbracket v4) |> G.s
  | For (v1, v2, v3) ->
      let v1 = v_tok v1
      and _v2TODO = v_bracket v_enumerators v2
      and v3 = v_for_body v3 in
      let header = G.ForClassic ([], None, None) (* TODO *) in
      G.For (v1, header, v3) |> G.s
  | Return (v1, v2) ->
      let v1 = v_tok v1 and v2 = v_option v_expr v2 in
      G.Return (v1, v2, G.sc) |> G.s
  | Try (v1, v2, v3, v4) ->
      let v1 = v_tok v1
      and v2 = v_expr_for_stmt v2
      and v3 = v_option v_catch_clause v3
      and v4 = v_option v_finally_clause v4 in
      let catches = match v3 with None -> [] | Some xs -> xs in
      G.Try (v1, v2, catches, v4) |> G.s
  | Throw (v1, v2) ->
      let v1 = v_tok v1 and v2 = v_expr v2 in
      G.Throw (v1, v2, G.sc) |> G.s

and v_enumerators v = v_list v_enumerator v

and v_enumerator = function
  | G v1 ->
      let _v1TODO = v_generator v1 in
      ()
  | GIf v1 ->
      let _v1TODO = v_list v_guard v1 in
      ()

and v_generator
    {
      genpat = v_genpat;
      gentok = v_gentok;
      genbody = v_genbody;
      genguards = v_genguards;
    } =
  let _pat = v_pattern v_genpat in
  let _t = v_tok v_gentok in
  let _e = v_expr v_genbody in
  let _guards = v_list v_guard v_genguards in
  ()

and v_for_body = function
  | Yield (v1, v2) ->
      let v1 = v_tok v1 and v2 = v_expr v2 in
      let e = G.Yield (v1, Some v2, false) in
      G.exprstmt e
  | NoYield v1 ->
      let v1 = v_expr_for_stmt v1 in
      v1

and v_catch_clause (v1, v2) : G.catch list =
  let v1 = v_tok v1 in
  match v2 with
  | CatchCases (_lb, xs, _rb) ->
      let actions = List.map v_case_clause xs in
      actions
      |> List.map (fun (pat, e) ->
             (* todo? e was the result of expr_of_block, so maybe we
              * should revert because we want a stmt here with block_of_expr
              *)
             (fake "case", pat, G.exprstmt e))
  | CatchExpr e ->
      let e = v_expr e in
      let pat = G.PatUnderscore v1 in
      [ (v1, pat, G.exprstmt e) ]

and v_finally_clause (v1, v2) =
  let v1 = v_tok v1 and v2 = v_expr_for_stmt v2 in
  (v1, v2)

and v_block v = v_list v_block_stat v |> List.flatten

and v_block_stat x : G.item list =
  match x with
  | D v1 ->
      let v1 = v_definition v1 in
      v1 |> List.map (fun def -> G.DefStmt def |> G.s)
  | I v1 ->
      let v1 = v_import v1 in
      v1 |> List.map (fun dir -> G.DirectiveStmt dir |> G.s)
  | E v1 ->
      let v1 = v_expr_for_stmt v1 in
      [ v1 ]
  | Package v1 ->
      let ipak, ids = v_package v1 in
      [ G.DirectiveStmt (G.Package (ipak, ids)) |> G.s ]
  | Packaging (v1, (_lb, v2, rb)) ->
      let ipak, ids = v_package v1 in
      let xxs = v_list v_top_stat v2 in
      [ G.DirectiveStmt (G.Package (ipak, ids)) |> G.s ]
      @ List.flatten xxs
      @ [ G.DirectiveStmt (G.PackageEnd rb) |> G.s ]

and v_top_stat v = v_block_stat v

and v_modifier v : G.attribute =
  let kind, tok = v_wrap v_modifier_kind v in
  match kind with
  | Left kwd -> G.KeywordAttr (kwd, tok)
  | Right s -> G.OtherAttribute (G.OA_Todo, [ G.TodoK (s, tok) ])

and v_modifier_kind = function
  | Abstract -> Left G.Abstract
  | Final -> Left G.Final
  | Sealed -> Right "sealed"
  | Implicit -> Right "implicit"
  | Lazy -> Left G.Lazy
  | Private v1 ->
      let _v1TODO = v_option (v_bracket v_ident_or_this) v1 in
      Left G.Private
  | Protected v1 ->
      let _v1TODO = v_option (v_bracket v_ident_or_this) v1 in
      Left G.Protected
  | Override -> Left G.Override
  | CaseClassOrObject -> Left G.CaseClass
  | PackageObject -> Right "PackageObject"
  | Val -> Left G.Const
  | Var -> Left G.Mutable

and v_annotation (v1, v2, v3) : G.attribute =
  let v1 = v_tok v1 and v2 = v_type_ v2 and v3 = v_list v_arguments v3 in
  let args = v3 |> List.map G.unbracket |> List.flatten in
  match v2 with
  | TyN name -> G.NamedAttr (v1, name, fb args)
  | _ ->
      G.OtherAttribute
        (OA_Todo, [ G.TodoK ("AnnotationComplexType", v1); G.T v2; G.Args args ])

and v_attribute x : G.attribute =
  match x with
  | A v1 ->
      let v1 = v_annotation v1 in
      v1
  | M v1 ->
      let v1 = v_modifier v1 in
      v1

and v_type_parameter
    {
      tpname = v_tpname;
      tpvariance = v_tpvariance;
      tpannots = v_tpannots;
      tpparams = v_tpparams;
      tpbounds = v_tpbounds;
      tpviewbounds = v_tpviewbounds;
      tpcolons = v_tpcolons;
    } : G.type_parameter =
  let id = v_ident_or_wildcard v_tpname in
  let _argTODO = v_option (v_wrap v_variance) v_tpvariance in
  let _argTODO = v_list v_annotation v_tpannots in
  let _argTODO = v_type_parameters v_tpparams in
  let _argTODO = v_type_bounds v_tpbounds in
  let _argTODO = v_list v_type_ v_tpviewbounds in
  let _argTODO = v_list v_type_ v_tpcolons in
  let constraints = [] in
  (id, constraints)

and v_variance = function Covariant -> () | Contravariant -> ()

and v_type_parameters v : G.type_parameter list =
  match v with None -> [] | Some (_lb, xs, _rb) -> v_list v_type_parameter xs

and v_definition x : G.definition list =
  match x with
  | DefEnt (v1, v2) ->
      let v1 = v_entity v1 and v2 = v_definition_kind v2 in
      [ (v1, v2) ]
  | VarDefs v1 -> v_variable_definitions v1

and v_variable_definitions
    {
      vpatterns = v_vpatterns;
      vattrs = v_vattrs;
      vtype = v_vtype;
      vbody = v_vbody;
    } =
  let attrs = v_list v_attribute v_vattrs in
  let topt = v_option v_type_ v_vtype in
  let eopt = v_option v_expr v_vbody in
  v_vpatterns
  |> Common.map_filter (fun pat ->
         match pat with
         | PatVarid id | PatName (Id id, []) ->
             let ent = G.basic_entity id attrs in
             let vdef = { G.vinit = eopt; vtype = topt } in
             Some (ent, G.VarDef vdef)
         | _ ->
             pr2 "TODO pattern var";
             None)

and v_entity { name = v_name; attrs = v_attrs; tparams = v_tparams } =
  let v1 = v_ident v_name in
  let v2 = v_list v_attribute v_attrs in
  let v3 = v_type_parameters v_tparams in
  { name = G.EN (H.name_of_id v1); attrs = v2; tparams = v3 }

and v_definition_kind = function
  | FuncDef v1 ->
      let v1 = v_function_definition v1 in
      G.FuncDef v1
  | TypeDef v1 ->
      let v1 = v_type_definition v1 in
      G.TypeDef v1
  | Template v1 ->
      let v1, _argssTODO = v_template_definition v1 in
      G.ClassDef v1

and v_function_definition
    {
      fkind = v_fkind;
      fparams = v_fparams;
      frettype = v_frettype;
      fbody = vfbody;
    } =
  let kind = v_wrap v_function_kind v_fkind in
  let params = v_list v_bindings v_fparams in
  let tret = v_option v_type_ v_frettype in
  let fbody = v_option v_fbody vfbody in
  {
    fkind = kind;
    fparams = List.flatten params;
    (* TODO? *)
    frettype = tret;
    fbody = (match fbody with None -> G.empty_fbody | Some st -> st);
  }

and v_function_kind = function LambdaArrow -> G.Arrow | Def -> G.Method

and v_fbody = function
  | FBlock v1 -> (
      let lb, kind, rb = v_block_expr v1 in
      match kind with
      | Left stats -> G.Block (lb, stats, rb) |> G.s
      | Right cases ->
          let def = cases_to_lambda lb cases in
          G.exprstmt (G.Lambda def))
  | FExpr (v1, v2) ->
      let _v1 = v_tok v1 and v2 = v_expr_for_stmt v2 in
      v2

and v_bindings v = v_bracket (v_list v_binding) v |> G.unbracket

and v_binding v : G.parameter =
  match v with
  | ParamEllipsis t -> G.ParamEllipsis t
  | ParamClassic
      {
        p_name = v_p_name;
        p_attrs = v_p_attrs;
        p_type = v_p_type;
        p_default = v_p_default;
      } -> (
      let id = v_ident_or_wildcard v_p_name in
      let attrs = v_list v_attribute v_p_attrs in
      let default = v_option v_expr v_p_default in
      let pclassic =
        { (G.param_of_id id) with pattrs = attrs; pdefault = default }
      in
      match v_p_type with
      | None -> G.ParamClassic pclassic
      | Some (PT v1) ->
          let v1 = v_type_ v1 in
          G.ParamClassic { pclassic with ptype = Some v1 }
      | Some (PTByNameApplication (v1, v2)) ->
          let v1 = v_tok v1 and v2 = v_type_ v2 in
          G.ParamClassic
            {
              pclassic with
              ptype = Some v2;
              pattrs = G.KeywordAttr (G.Lazy, v1) :: pclassic.pattrs;
            }
      | Some (PTRepeatedApplication (v1, v2)) ->
          let v1 = v_type_ v1 and v2 = v_tok v2 in
          G.ParamRest (v2, { pclassic with ptype = Some v1 }))

and v_template_definition
    {
      ckind = v_ckind;
      cparams = v_cparams;
      cparents = v_cparents;
      cbody = v_cbody;
    } : G.class_definition * G.arguments bracket list =
  let ckind = v_wrap v_template_kind v_ckind in
  (* TODO? flatten? *)
  let cparams = v_list v_bindings v_cparams |> List.flatten in
  let (cextends, cmixins), argss = v_template_parents v_cparents in
  let body = v_option v_template_body v_cbody in
  let cbody =
    match body with
    | None -> G.empty_body
    | Some (lb, xs, rb) -> (lb, xs |> List.map (fun st -> G.FieldStmt st), rb)
  in
  ({ G.ckind; cextends; cmixins; cimplements = []; cparams; cbody }, argss)

and v_template_parents { cextends = v_cextends; cwith = v_cwith } =
  let parent, argss =
    match v_cextends with
    | None -> ([], [])
    | Some (v1, v2) ->
        let v1 = v_type_ v1 in
        let v2 = v_list v_arguments v2 in
        ([ v1 ], v2)
  in
  let v2 = v_list v_type_ v_cwith in
  ((parent, v2), argss)

and v_template_body v =
  v_bracket
    (fun (v1, v2) ->
      let _v1TODO = v_option v_self_type v1 and v2 = v_block v2 in
      v2)
    v

and v_self_type (v1, v2, v3) =
  let _v1 = v_ident_or_this v1
  and _v2 = v_option v_type_ v2
  and _v3 = v_tok v3 in
  ()

and v_template_kind = function
  | Class -> G.Class
  | Trait -> G.Trait
  | Object -> G.Object
  | Singleton -> G.Object

and v_type_definition { ttok = v_ttok; tbody = v_tbody } =
  let _tok = v_tok v_ttok in
  let arg = v_type_definition_kind v_tbody in
  { tbody = arg }

and v_type_definition_kind = function
  | TDef (v1, v2) ->
      let _v1 = v_tok v1 and v2 = v_type_ v2 in
      G.NewType v2
  | TDcl v1 ->
      let _v1TODO = v_type_bounds v1 in
      G.OtherTypeKind (G.OTKO_AbstractType, [])

let v_program v = v_list v_top_stat v |> List.flatten

let v_any = function
  | Pr v1 ->
      let v1 = v_program v1 in
      G.Ss v1
  | Tk v1 ->
      let v1 = v_tok v1 in
      G.Tk v1
  | Ex e -> (
      match v_expr_for_stmt e with
      | { G.s = G.ExprStmt (e, _); _ } -> G.E e
      | st -> G.S st)
  | Ss b -> (
      let xs = v_block b in
      match xs with [ s ] -> G.S s | xs -> G.Ss xs)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let program xs = v_program xs

let any x = v_any x
