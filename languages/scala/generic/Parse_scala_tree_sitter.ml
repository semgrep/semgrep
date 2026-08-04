(* Cooper Pierce
 *
 * Copyright (c) 2024-2026 Semgrep Inc.
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
open Fpath_.Operators
module CST = Tree_sitter_scala.CST
module H = Parse_tree_sitter_helpers
open AST_generic
module G = AST_generic
module H2 = AST_generic_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Scala parser using tree-sitter-lang/semgrep-scala and converting
 * directly to AST_generic.ml
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type context = Program | Pattern
type env = context H.env

let token = H.token
let str = H.str
let fb = Tok.unsafe_fake_bracket

let in_pattern env =
  match env.H.extra with
  | Program -> false
  | Pattern -> true

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-scala/Boilerplate.ml *)

(* ---------------------------------------------------------------------- *)
(* Token-level helpers *)
(* ---------------------------------------------------------------------- *)

let identifier (env : env) (x : CST.identifier) : ident =
  match x with
  | `Alpha_id tok -> str env tok
  | `Back_id tok -> str env tok
  | `Soft_id x -> (
      match x with
      | `Infix tok -> str env tok
      | `Inline tok -> str env tok
      | `Opaque tok -> str env tok
      | `Open tok -> str env tok
      | `Trac tok -> str env tok
      | `Tran tok -> str env tok
      | `End tok -> str env tok)

let type_identifier (env : env) (x : CST.type_identifier) : ident =
  match x with
  | `Id x -> identifier env x
  | `Op_id tok -> str env tok

let type_id_name (env : env) (x : CST.type_identifier) : name =
  let id = type_identifier env x in
  H2.name_of_id id

let package_identifier (env : env) ((v1, v2) : CST.package_identifier) :
    dotted_ident =
  let v1 = type_identifier env v1 in
  let v2 =
    List.map
      (fun (_tdot, v2) ->
        let v2 = type_identifier env v2 in
        v2)
      v2
  in
  v1 :: v2

(* ---------------------------------------------------------------------- *)
(* Stable identifiers -> names *)
(* ---------------------------------------------------------------------- *)

let rec anon_choice_type_id_4bf0d65_to_dotted_ident (env : env)
    (x : CST.anon_choice_type_id_4bf0d65) : dotted_ident =
  match x with
  | `Choice_id x -> [ type_identifier env x ]
  | `Stable_id x -> stable_identifier_to_dotted_ident env x

and stable_identifier_to_dotted_ident (env : env)
    ((v1, _v2, v3) : CST.stable_identifier) : dotted_ident =
  let prefix = anon_choice_type_id_4bf0d65_to_dotted_ident env v1 in
  let last = type_identifier env v3 in
  prefix @ [ last ]

let anon_choice_type_id_4bf0d65_to_name (env : env)
    (x : CST.anon_choice_type_id_4bf0d65) : name =
  let ids = anon_choice_type_id_4bf0d65_to_dotted_ident env x in
  H2.name_of_ids ids

let stable_type_identifier_to_name (env : env)
    ((v1, _v2, v3) : CST.stable_type_identifier) : name =
  let prefix = anon_choice_type_id_4bf0d65_to_dotted_ident env v1 in
  let last = type_identifier env v3 in
  H2.name_of_ids (prefix @ [ last ])

let anon_choice_type_id_ae98204_to_name (env : env)
    (x : CST.anon_choice_type_id_ae98204) : name =
  match x with
  | `Type_id x -> type_id_name env x
  | `Stable_type_id x -> stable_type_identifier_to_name env x

(* ---------------------------------------------------------------------- *)
(* Literals *)
(* ---------------------------------------------------------------------- *)

let boolean_literal (env : env) (x : CST.boolean_literal) =
  match x with
  | `True tok -> Bool (true, token env tok)
  | `False tok -> Bool (false, token env tok)

let map_string_ (env : env) (x : CST.string_) : expr =
  match x with
  | `Simple_str_start_rep_simple_str_middle_esc_seq_single_line_str_end
      (v1, v2, v3) ->
      (* The string content is split between the middle tokens and the end
         token. For a simple string like "hello", the middle list is empty
         and the content is in the end token (minus the closing quote).
         For strings with escapes, the content alternates between middle
         and escape tokens. *)
      let start_t = token env v1 in
      let end_s, end_t = str env v3 in
      (* Remove trailing quote from end token *)
      let end_content =
        if
          String.length end_s > 0
          && Char.equal (String.get end_s (String.length end_s - 1)) '"'
        then String.sub end_s 0 (String.length end_s - 1)
        else end_s
      in
      let parts =
        List.map
          (fun (mid, esc) ->
            let s, _t = str env mid in
            let e, _t = str env esc in
            s ^ e)
          v2
      in
      let full = String.concat "" parts ^ end_content in
      (* Combine start and end tokens for a token spanning the full string literal *)
      let tok = Tok.combine_toks start_t [ end_t ] in
      L (String (fb (full, tok))) |> G.e
  | `Simple_mult_str_start_mult_str_end (v1, v2) ->
      let start_t = token env v1 in
      let _s, end_t = str env v2 in
      let tok = Tok.combine_toks start_t [ end_t ] in
      L (String (fb ("", tok))) |> G.e

let non_null_literal (env : env) (x : CST.non_null_literal) : literal =
  match x with
  | `Int_lit tok ->
      let s, t = str env tok in
      Int (Parsed_int.parse (s, t))
  | `Floa_point_lit tok ->
      let s, t = str env tok in
      Float (float_of_string_opt s, t)
  | `Bool_lit x -> boolean_literal env x
  | `Char_lit tok ->
      let s, t = str env tok in
      Char (s, t)
  | `Str x -> (
      match x with
      | `Simple_str_start_rep_simple_str_middle_esc_seq_single_line_str_end
          (v1, v2, v3) ->
          let start_t = token env v1 in
          let end_s, end_t = str env v3 in
          let end_content =
            if
              String.length end_s > 0
              && Char.equal (String.get end_s (String.length end_s - 1)) '"'
            then String.sub end_s 0 (String.length end_s - 1)
            else end_s
          in
          let parts =
            List.map
              (fun (mid, esc) ->
                let s, _t = str env mid in
                let e, _t = str env esc in
                s ^ e)
              v2
          in
          let full = String.concat "" parts ^ end_content in
          let tok = Tok.combine_toks start_t [ end_t ] in
          String (fb (full, tok))
      | `Simple_mult_str_start_mult_str_end (v1, v2) ->
          let start_t = token env v1 in
          let _s, end_t = str env v2 in
          let tok = Tok.combine_toks start_t [ end_t ] in
          String (fb ("", tok)))

let literal (env : env) (x : CST.literal) : literal =
  match x with
  | `Non_null_lit x -> non_null_literal env x
  | `Null_lit tok -> Null (token env tok)

let literal_to_expr (env : env) (x : CST.literal) : expr =
  match x with
  | `Non_null_lit (`Str x) -> map_string_ env x
  | _ -> L (literal env x) |> G.e

(* ---------------------------------------------------------------------- *)
(* Modifiers *)
(* ---------------------------------------------------------------------- *)

let access_modifier (env : env) ((v1, _v2) : CST.access_modifier) : attribute =
  match v1 with
  | `Priv tok -> KeywordAttr (Private, token env tok)
  | `Prot tok -> KeywordAttr (Protected, token env tok)

let modifier (env : env)
    (x :
      [< `Abst of _
      | `Final of _
      | `Sealed of _
      | `Impl of _
      | `Lazy of _
      | `Over of _
      | `Access_modi of CST.access_modifier
      | `Inline_modi of _
      | `Infix_modi of _
      | `Into_modi of _
      | `Open_modi of _
      | `Trac_modi of _
      | `Tran_modi of _ ]) : attribute =
  match x with
  | `Abst tok -> KeywordAttr (Abstract, token env tok)
  | `Final tok -> KeywordAttr (Final, token env tok)
  | `Sealed tok -> G.attr SealedClass (token env tok)
  | `Impl tok -> G.unhandled_keywordattr (str env tok)
  | `Lazy tok -> G.unhandled_keywordattr (str env tok)
  | `Over tok -> KeywordAttr (Override, token env tok)
  | `Access_modi x -> access_modifier env x
  | `Inline_modi tok -> KeywordAttr (Inline, token env tok)
  | `Infix_modi tok -> G.unhandled_keywordattr (str env tok)
  | `Into_modi tok -> G.unhandled_keywordattr (str env tok)
  | `Open_modi tok -> G.unhandled_keywordattr (str env tok)
  | `Trac_modi tok -> G.unhandled_keywordattr (str env tok)
  | `Tran_modi tok -> G.unhandled_keywordattr (str env tok)

let modifiers (env : env) (xs : CST.modifiers) : attribute list =
  List.map (modifier env) xs

let modifiers_opt (env : env) (x : CST.modifiers option) : attribute list =
  match x with
  | Some xs -> modifiers env xs
  | None -> []

(* ---------------------------------------------------------------------- *)
(* Forward declarations *)
(* ---------------------------------------------------------------------- *)

(* The large group of mutually recursive functions below handles
   all the CST types that form cycles in the grammar. *)

let rec annotation (env : env) ((v1, v2, v3) : CST.annotation) : attribute =
  let at_tok = token env v1 in
  let ty_name = simple_type_to_name env v2 in
  let args =
    match v3 with
    | [] -> fb []
    | arg_lists ->
        let all_args =
          arg_lists
          |> List.map (fun al ->
              let _l, args, _r = arguments env al in
              args)
          |> List_.flatten
        in
        fb all_args
  in
  NamedAttr (at_tok, ty_name, args)

and annotations (env : env) (xs : CST.annotation list) : attribute list =
  List.map (annotation env) xs

and simple_type_to_name (env : env) (x : CST.simple_type) : name =
  match x with
  | `Type_id x -> type_id_name env x
  | `Stable_type_id x -> stable_type_identifier_to_name env x
  | `Gene_type (ty, targs) ->
      let n = simple_type_to_name env ty in
      let targs = type_arguments env targs in
      H2.add_type_args_to_name n targs
  | _ ->
      (* For complex types used as annotations, just use a fake name *)
      let t = Tok.unsafe_fake_tok "" in
      H2.name_of_id ("_", t)

(* ---------------------------------------------------------------------- *)
(* Types *)
(* ---------------------------------------------------------------------- *)

and type_ (env : env) (x : CST.type_) : type_ =
  match x with
  | `Func_type x -> function_type env x
  | `Comp_type x -> compound_type env x
  | `Infix_type x -> infix_type env x
  | `Match_type (v1, _v2, v3) ->
      let t = infix_type_choice env v1 in
      let _cases = indented_type_cases env v3 in
      (* Match types are Scala 3 specific, represent as OtherType *)
      OtherType (("MatchType", Tok.unsafe_fake_tok ""), [ G.T t ]) |> G.t
  | `Anno_type x -> annotated_type env x
  | `Lit_type x -> literal_type env x
  | `Stru_type x ->
      let _l, fields, _r = template_body_to_class_body env x in
      let field_anys = List.map (fun f -> G.Fld f) fields in
      OtherType (("StructuralType", Tok.unsafe_fake_tok ""), field_anys) |> G.t
  | `Type_lambda x -> type_lambda env x

and literal_type (env : env) (x : CST.literal_type) : type_ =
  let lit = non_null_literal env x in
  OtherType (("LiteralType", Tok.unsafe_fake_tok ""), [ G.E (L lit |> G.e) ])
  |> G.t

and annotated_type (env : env) (x : CST.annotated_type) : type_ =
  match x with
  | `Anno_type_ (ty, annos) ->
      let t = simple_type env ty in
      let attrs = annotations env annos in
      { t with t_attrs = attrs @ t.t_attrs }
  | `Simple_type x -> simple_type env x

and simple_type (env : env) (x : CST.simple_type) : type_ =
  match x with
  | `Gene_type (ty, targs) ->
      let ty = simple_type env ty in
      let targs = type_arguments env targs in
      TyApply (ty, targs) |> G.t
  | `Proj_type (ty, _hash, tid) ->
      let ty = simple_type env ty in
      let id = type_identifier env tid in
      OtherType (("ProjectedType", snd id), [ G.T ty; G.I id ]) |> G.t
  | `Tuple_type (v1, v2, v3, _v4, v5) ->
      let l = token env v1 in
      let first = type_ env v2 in
      let rest = List.map (fun (_comma, t) -> type_ env t) v3 in
      let r = token env v5 in
      TyTuple (l, first :: rest, r) |> G.t
  | `Named_tuple_type (v1, v2, v3, _v4, v5) ->
      let l = token env v1 in
      let first = name_and_type env v2 in
      let rest = List.map (fun (_comma, nt) -> name_and_type env nt) v3 in
      let r = token env v5 in
      let params = first :: rest in
      let fields =
        List.map
          (fun (id, ty) ->
            let ent = G.basic_entity id in
            let vdef = { vinit = None; vtype = Some ty; vtok = G.no_sc } in
            F (DefStmt (ent, VarDef vdef) |> G.s))
          params
      in
      TyRecordAnon ((Class, Tok.unsafe_fake_tok ""), (l, fields, r)) |> G.t
  | `Sing_type (v1, _v2, _v3) ->
      let n = anon_choice_type_id_4bf0d65_to_name env v1 in
      OtherType (("SingletonType", Tok.unsafe_fake_tok ""), [ G.Name n ]) |> G.t
  | `Stable_type_id x ->
      let n = stable_type_identifier_to_name env x in
      TyN n |> G.t
  | `Type_id x ->
      let n = type_id_name env x in
      TyN n |> G.t
  | `Appl_cons_type (tid, args) ->
      let n = type_id_name env tid in
      let _l, exprs, _r = arguments env args in
      (* applied constructor types (e.g., in extends clauses) *)
      let ty = TyN n |> G.t in
      let type_args =
        List.map
          (fun a ->
            match a with
            | Arg e -> TAExpr e
            | _ ->
                TAExpr
                  (G.e (OtherExpr (("ArgToExpr", Tok.unsafe_fake_tok ""), []))))
          exprs
      in
      TyApply (ty, fb type_args) |> G.t
  | `Wild tok ->
      let _t = token env tok in
      TyN (H2.name_of_id ("_", token env tok)) |> G.t

and name_and_type (env : env) ((v1, _v2, v3) : CST.name_and_type) :
    ident * type_ =
  let id = type_identifier env v1 in
  let ty = param_type env v3 in
  (id, ty)

and compound_type (env : env) (x : CST.compound_type) : type_ =
  match x with
  | `Anno_type_rep1_with_anno_type (v1, v2) ->
      let first = annotated_type env v1 in
      let rest = List.map (fun (_with_tok, at) -> annotated_type env at) v2 in
      let types = first :: rest in
      OtherType
        ( ("CompoundType", Tok.unsafe_fake_tok ""),
          List.map (fun t -> G.T t) types )
      |> G.t
  | `Anno_type_refi (v1, _v2) ->
      let ty = annotated_type env v1 in
      ty
  | `Anno_type_rep1_with_anno_type_refi (v1, v2, _v3) ->
      let first = annotated_type env v1 in
      let rest = List.map (fun (_with_tok, at) -> annotated_type env at) v2 in
      let types = first :: rest in
      OtherType
        ( ("CompoundType", Tok.unsafe_fake_tok ""),
          List.map (fun t -> G.T t) types )
      |> G.t

and infix_type (env : env) ((v1, v2, v3) : CST.infix_type) : type_ =
  let left = infix_type_choice env v1 in
  let op = type_identifier env v2 in
  let right = infix_type_choice env v3 in
  let op_ty = TyN (H2.name_of_id op) |> G.t in
  TyApply (op_ty, fb [ TA left; TA right ]) |> G.t

and infix_type_choice (env : env) (x : CST.infix_type_choice) : type_ =
  match x with
  | `Comp_type x -> compound_type env x
  | `Infix_type x -> infix_type env x
  | `Anno_type x -> annotated_type env x
  | `Lit_type x -> literal_type env x

and function_type (env : env) (x : CST.function_type) : type_ =
  match x with
  | `Type_params_arrow_then_type (_tparams, (_, ret_type)) ->
      (* Type parameters in function types are unusual; just return the result *)
      type_ env ret_type
  | `Param_types_arrow_then_type (ptypes, (_, ret_type)) ->
      let param_types = parameter_types_to_list env ptypes in
      let ret = type_ env ret_type in
      TyFun (List.map (fun t -> Param (G.param_of_type t)) param_types, ret)
      |> G.t

and parameter_types_to_list (env : env) (x : CST.parameter_types) : type_ list =
  match x with
  | `Anno_type x -> [ annotated_type env x ]
  | `LPAR_opt_choice_type_rep_COMMA_choice_type_opt_COMMA_RPAR (_l, opt, _r)
    -> (
      match opt with
      | None -> []
      | Some (first, rest, _trailing_comma) ->
          let first = param_type env first in
          let rest = List.map (fun (_comma, pt) -> param_type env pt) rest in
          first :: rest)
  | `Comp_type x -> [ compound_type env x ]
  | `Infix_type x -> [ infix_type env x ]

and param_type (env : env) (x : CST.param_type) : type_ =
  match x with
  | `Type x -> type_ env x
  | `Lazy_param_type (_arrow, t) ->
      let ty = type_ env t in
      OtherType (("LazyParamType", Tok.unsafe_fake_tok ""), [ G.T ty ]) |> G.t
  | `Repe_param_type (t, _star) ->
      let ty =
        match t with
        | `Type t -> type_ env t
        | `Lazy_param_type (_arrow, t) ->
            let inner = type_ env t in
            OtherType (("LazyParamType", Tok.unsafe_fake_tok ""), [ G.T inner ])
            |> G.t
      in
      OtherType (("RepeatedParamType", Tok.unsafe_fake_tok ""), [ G.T ty ])
      |> G.t

and type_arguments (env : env) ((v1, v2, v3, _v4, v5) : CST.type_arguments) :
    type_arguments =
  let l = token env v1 in
  let first = TA (type_ env v2) in
  let rest = List.map (fun (_comma, t) -> TA (type_ env t)) v3 in
  let r = token env v5 in
  (l, first :: rest, r)

and type_lambda (env : env) ((_v1, v2, v3, _v4, _v5, _v6, v7) : CST.type_lambda)
    : type_ =
  let _first_tp = type_parameter env v2 in
  let _rest_tp = List.map (fun (_comma, tp) -> type_parameter env tp) v3 in
  let body = type_ env v7 in
  (* Type lambdas ([X] =>> F[X]) are Scala 3 specific *)
  body

and indented_type_cases (env : env) ((_v1, v2, _v3) : CST.indented_type_cases) :
    (type_ * type_) list =
  List.map (type_case_clause env) v2

and type_case_clause (env : env) ((_v1, v2, (_, v4)) : CST.type_case_clause) :
    type_ * type_ =
  let lhs = infix_type_choice env v2 in
  let rhs = type_ env v4 in
  (lhs, rhs)

(* ---------------------------------------------------------------------- *)
(* Type parameters *)
(* ---------------------------------------------------------------------- *)

and type_parameter (env : env) ((v1, v2, v3, v4, _v5, _v6) : CST.type_parameter)
    : type_parameter =
  let id =
    match v1 with
    | `Wild tok -> str env tok
    | `Choice_id x -> type_identifier env x
  in
  let _tparams =
    match v2 with
    | Some x -> Some (type_parameters env x)
    | None -> None
  in
  let _lower =
    match v3 with
    | Some (_tok, t) -> Some (type_ env t)
    | None -> None
  in
  let upper =
    match v4 with
    | Some (_tok, t) -> [ type_ env t ]
    | None -> []
  in
  TP
    {
      tp_id = id;
      tp_attrs = [];
      tp_bounds = upper;
      tp_default = None;
      tp_variance = None;
    }

and type_parameters (env : env) ((v1, v2, v3, _v4, v5) : CST.type_parameters) :
    type_parameters =
  let l = token env v1 in
  let first = variant_type_parameter env v2 in
  let rest =
    List.map (fun (_comma, vtp) -> variant_type_parameter env vtp) v3
  in
  let r = token env v5 in
  (l, first :: rest, r)

and variant_type_parameter (env : env)
    ((_annos, v2) : CST.variant_type_parameter) : type_parameter =
  match v2 with
  | `Cova_type_param (_plus, tp) -> (
      match type_parameter env tp with
      | TP p ->
          TP { p with tp_variance = Some (Covariant, Tok.unsafe_fake_tok "+") }
      | other -> other)
  | `Cont_type_param (_minus, tp) -> (
      match type_parameter env tp with
      | TP p ->
          TP
            {
              p with
              tp_variance = Some (Contravariant, Tok.unsafe_fake_tok "-");
            }
      | other -> other)
  | `Type_param x -> type_parameter env x
  | `Type_lambda x ->
      let _ty = type_lambda env x in
      let t = Tok.unsafe_fake_tok "" in
      TP
        {
          tp_id = ("_", t);
          tp_attrs = [];
          tp_bounds = [];
          tp_default = None;
          tp_variance = None;
        }

(* ---------------------------------------------------------------------- *)
(* Expressions *)
(* ---------------------------------------------------------------------- *)

and expression (env : env) (x : CST.expression) : expr =
  match x with
  | `If_exp (_inline_opt, v2, v3, v4, v5) ->
      let if_tok = token env v2 in
      let cond = if_condition env v3 in
      let then_expr = indentable_expression_to_stmt env v4 in
      let else_opt =
        match v5 with
        | Some (_semi_opt, _else_tok, else_expr) ->
            Some (indentable_expression_to_stmt env else_expr)
        | None -> None
      in
      let if_stmt = If (if_tok, Cond cond, then_expr, else_opt) |> G.s in
      stmt_to_expr if_stmt
  | `Match_exp (_inline_opt, v2, v3, v4) ->
      let e = expression env v2 in
      let match_tok = token env v3 in
      let cases = match_body env v4 in
      let switch = Switch (match_tok, Some (Cond e), cases) |> G.s in
      stmt_to_expr switch
  | `Try_exp (v1, v2, v3, v4) ->
      let try_tok = token env v1 in
      let body = indentable_expression_to_stmt env v2 in
      let catches =
        match v3 with
        | Some x -> catch_clause env x
        | None -> []
      in
      let finally =
        match v4 with
        | Some (_fin_tok, e) ->
            Some
              ( Tok.unsafe_fake_tok "finally",
                indentable_expression_to_stmt env e )
        | None -> None
      in
      let try_stmt = Try (try_tok, body, catches, None, finally) |> G.s in
      stmt_to_expr try_stmt
  | `Assign_exp (v1, v2, v3) ->
      let lhs =
        match v1 with
        | `Prefix_exp x -> prefix_expression env x
        | `Simple_exp x -> simple_expression env x
      in
      let eq = token env v2 in
      let rhs = expression env v3 in
      Assign (lhs, eq, rhs) |> G.e
  | `Lambda_exp (_tparams_opt, v2, _arrow, v4) ->
      let params = lambda_params env v2 in
      let body = indentable_expression_to_stmt env v4 in
      let def =
        {
          fkind = (Arrow, Tok.unsafe_fake_tok "=>");
          fparams = fb params;
          frettype = None;
          fbody = FBStmt body;
        }
      in
      Lambda def |> G.e
  | `Post_exp x -> postfix_expression env x
  | `Ascr_exp (v1, v2, v3) -> (
      let e = postfix_expression_choice env v1 in
      let colon_tok = token env v2 in
      let ty_or_anno =
        match v3 with
        | `Choice_type pt -> Some (param_type env pt)
        | `Anno _a -> None
      in
      match (e.e, ty_or_anno) with
      | N (Id ((s, tok), _)), Some ty when AST_generic.is_metavar_name s ->
          TypedMetavar ((s, tok), colon_tok, ty) |> G.e
      | _, Some ty -> Cast (ty, colon_tok, e) |> G.e
      | _, None -> e)
  | `Infix_exp x -> infix_expression env x
  | `Prefix_exp x -> prefix_expression env x
  | `Ret_exp (v1, v2) ->
      let ret_tok = token env v1 in
      let e_opt = Option.map (expression env) v2 in
      let ret = Return (ret_tok, e_opt, G.sc) |> G.s in
      stmt_to_expr ret
  | `Throw_exp (v1, v2) ->
      let throw_tok = token env v1 in
      let e = expression env v2 in
      let throw = Throw (throw_tok, e, G.sc) |> G.s in
      stmt_to_expr throw
  | `While_exp x -> while_expression env x
  | `Do_while_exp (v1, v2, _v3, v4) ->
      let do_tok = token env v1 in
      let body = expression env v2 in
      let _lp, cond, _rp = parenthesized_expression env v4 in
      let do_while =
        DoWhile (do_tok, ExprStmt (body, G.sc) |> G.s, cond) |> G.s
      in
      stmt_to_expr do_while
  | `For_exp x -> for_expression env x
  | `Macro_body (_macro_tok, v2) -> anon_choice_infix_exp_dc476f6 env v2
  | `Simple_exp x -> simple_expression env x

and lambda_params (env : env)
    (x :
      [ `Bindis of CST.bindings
      | `Opt_impl_choice_id of _ * CST.type_identifier
      | `Wild of _ ]) : parameter list =
  match x with
  | `Bindis x -> bindings_to_params env x
  | `Opt_impl_choice_id (_impl, tid) ->
      let id = type_identifier env tid in
      [ Param (G.param_of_id id) ]
  | `Wild _tok -> [ Param (G.param_of_id ("_", Tok.unsafe_fake_tok "_")) ]

and bindings_to_params (env : env) ((_l, bindings_opt, _r) : CST.bindings) :
    parameter list =
  match bindings_opt with
  | None -> []
  | Some (first, rest, _trailing_comma) ->
      let first = binding_to_param env first in
      let rest = List.map (fun (_comma, b) -> binding_to_param env b) rest in
      first :: rest

and binding_to_param (env : env) ((v1, v2) : CST.binding) : parameter =
  let id =
    match v1 with
    | `Choice_id x -> type_identifier env x
    | `Wild tok -> str env tok
  in
  let ptype =
    match v2 with
    | Some (_colon, pt) -> Some (param_type env pt)
    | None -> None
  in
  Param (G.param_of_id id ?ptype)

and if_condition (env : env) (x : CST.if_condition) : expr =
  match x with
  | `Paren_exp x ->
      let _l, e, _r = parenthesized_expression env x in
      e
  | `Inde_exp_then (v1, _then_tok) -> indentable_expression env v1

and while_expression (env : env) (x : CST.while_expression) : expr =
  match x with
  | `While_paren_exp_exp (v1, v2, v3) ->
      let while_tok = token env v1 in
      let _l, cond, _r = parenthesized_expression env v2 in
      let body = expression env v3 in
      let wh =
        While (while_tok, Cond cond, ExprStmt (body, G.sc) |> G.s) |> G.s
      in
      stmt_to_expr wh
  | `While_inde_exp_do_inde_exp (v1, v2, _do_tok, v4) ->
      let while_tok = token env v1 in
      let cond = indentable_expression env v2 in
      let body = indentable_expression_to_stmt env v4 in
      let wh = While (while_tok, Cond cond, body) |> G.s in
      stmt_to_expr wh

and for_expression (env : env) (x : CST.for_expression) : expr =
  match x with
  | `For_choice_LPAR_enumes_RPAR_choice_exp (v1, v2, v3) ->
      let for_tok = token env v1 in
      let enums =
        match v2 with
        | `LPAR_enumes_RPAR (_l, e, _r) -> enumerators env e
        | `LCURL_enumes_RCURL (_l, e, _r) -> enumerators env e
      in
      let body_or_yield =
        match v3 with
        | `Exp e -> expression env e
        | `Yield_inde_exp (yield_tok, ie) ->
            let e = indentable_expression env ie in
            Yield (token env yield_tok, Some e, false) |> G.e
      in
      let header = MultiForEach enums in
      let for_stmt =
        For (for_tok, header, ExprStmt (body_or_yield, G.sc) |> G.s) |> G.s
      in
      stmt_to_expr for_stmt
  | `For_enumes_choice_do_inde_exp (v1, v2, v3) ->
      let for_tok = token env v1 in
      let enums = enumerators env v2 in
      let body =
        match v3 with
        | `Do_inde_exp (_do_tok, ie) -> indentable_expression env ie
        | `Yield_inde_exp (yield_tok, ie) ->
            let e = indentable_expression env ie in
            Yield (token env yield_tok, Some e, false) |> G.e
      in
      let header = MultiForEach enums in
      let for_stmt =
        For (for_tok, header, ExprStmt (body, G.sc) |> G.s) |> G.s
      in
      stmt_to_expr for_stmt

and enumerators (env : env) (x : CST.enumerators) : multi_for_each list =
  let raw =
    match x with
    | `Enum_rep_choice_SEMI_enum_opt_auto_semi (v1, v2, _v3) ->
        let first = enumerator env v1 in
        let rest = List.map (fun (_semi, e) -> enumerator env e) v2 in
        first @ List_.flatten rest
    | `Indent_enum_rep_choice_SEMI_enum_opt_auto_semi_outd
        (_indent, v2, v3, _v4, _outdent) ->
        let first = enumerator env v2 in
        let rest = List.map (fun (_semi, e) -> enumerator env e) v3 in
        first @ List_.flatten rest
  in
  (* Attach standalone guards to the preceding generator.
     In Scala, `for { x <- xs; if cond }` parses the guard as a
     separate enumerator (`Left`). We fold it into the preceding
     FE/FECond to match the pfff parser's representation. *)
  let rec attach_guards acc = function
    | [] -> List.rev acc
    | Either.Right mfe :: rest -> attach_guards (mfe :: acc) rest
    | Either.Left (if_tok, cond) :: rest -> (
        match acc with
        | FE fe :: acc_rest ->
            attach_guards (FECond (fe, if_tok, cond) :: acc_rest) rest
        | FECond (fe, old_if, old_cond) :: acc_rest ->
            let combined =
              Call
                ( N (H2.name_of_id ("&&", Tok.unsafe_fake_tok "&&")) |> G.e,
                  fb [ Arg old_cond; Arg cond ] )
              |> G.e
            in
            attach_guards (FECond (fe, old_if, combined) :: acc_rest) rest
        | _ -> attach_guards acc rest)
  in
  attach_guards [] raw

and enumerator (env : env) (x : CST.enumerator) :
    (Tok.t * expr, multi_for_each) Either.t list =
  match x with
  | `Choice_opt_case_choice_choice_choice_id_choice_LTDASH_exp_opt_guard inner
    -> (
      match inner with
      | `Opt_case_choice_choice_choice_id_choice_LTDASH_exp_opt_guard
          (_case_opt, v2, arrow, v4, guard_opt) -> (
          let pat = pattern env v2 in
          let arrow_tok =
            match arrow with
            | `LTDASH tok -> token env tok
            | `EQ tok -> token env tok
          in
          let e = expression env v4 in
          let fe = (pat, arrow_tok, e) in
          match guard_opt with
          | None -> [ Either.Right (FE fe) ]
          | Some g ->
              let cond = guard env g in
              [ Either.Right (FECond (fe, Tok.unsafe_fake_tok "if", cond)) ])
      | `Rep1_guard guards ->
          List.map
            (fun g ->
              let cond = guard env g in
              Either.Left (Tok.unsafe_fake_tok "if", cond))
            guards)
  | `Semg_ellips tok ->
      let t = token env tok in
      [ Either.Right (FEllipsis t) ]

and postfix_expression (env : env) ((v1, v2) : CST.postfix_expression) : expr =
  let e = anon_choice_infix_exp_dc476f6 env v1 in
  let op_id = type_identifier env v2 in
  let op_s, op_t = op_id in
  if String.equal op_s "*" then
    (* `bar*` is Scala's splat/spread syntax for varargs *)
    G.special (Spread, op_t) [ e ]
  else
    (* Other postfix operators like `value !` are method calls *)
    DotAccess (e, Tok.unsafe_fake_tok ".", FN (H2.name_of_id op_id)) |> G.e

and postfix_expression_choice (env : env) (x : CST.postfix_expression_choice) :
    expr =
  match x with
  | `Post_exp x -> postfix_expression env x
  | `Infix_exp x -> infix_expression env x
  | `Prefix_exp x -> prefix_expression env x
  | `Simple_exp x -> simple_expression env x

and anon_choice_infix_exp_dc476f6 (env : env)
    (x : CST.anon_choice_infix_exp_dc476f6) : expr =
  match x with
  | `Infix_exp x -> infix_expression env x
  | `Prefix_exp x -> prefix_expression env x
  | `Simple_exp x -> simple_expression env x

and infix_expression (env : env) ((v1, v2, v3) : CST.infix_expression) : expr =
  let lhs = anon_choice_infix_exp_dc476f6 env v1 in
  let op_id = type_identifier env v2 in
  let rhs =
    match v3 with
    | `Prefix_exp x -> prefix_expression env x
    | `Simple_exp x -> simple_expression env x
    | `COLON_colon_arg (_colon, ca) -> colon_argument_to_expr env ca
  in
  (* In Scala, `x op y` is syntactic sugar for `x.op(y)`, so we map it
   * to DotAccess to enable pattern matching with `$XS.map($F)` to match
   * both `xs.map(f)` and `xs map f`. *)
  let dot_access =
    DotAccess (lhs, Tok.unsafe_fake_tok ".", FN (H2.name_of_id op_id)) |> G.e
  in
  Call (dot_access, fb [ Arg rhs ]) |> G.e

and colon_argument_to_expr (env : env) ((_binder_opt, v2) : CST.colon_argument)
    : expr =
  match v2 with
  | `Inde_blk x -> indented_block_to_expr env x
  | `Inde_cases x -> indented_cases_to_expr env x

and prefix_expression (env : env) ((v1, v2) : CST.prefix_expression) : expr =
  let op, tok =
    match v1 with
    | `PLUS tok -> (Plus, token env tok)
    | `DASH tok -> (Minus, token env tok)
    | `BANG tok -> (Not, token env tok)
    | `TILDE tok -> (BitNot, token env tok)
  in
  let e = simple_expression env v2 in
  G.opcall (op, tok) [ e ]

and simple_expression (env : env) (x : CST.simple_expression) : expr =
  match x with
  | `Choice_id inner -> (
      match inner with
      | `Id x ->
          let id = identifier env x in
          let s, t = id in
          if in_pattern env && AST_generic.is_metavar_name s then
            N (H2.name_of_id id) |> G.e
          else if String.equal s "this" then
            N (IdSpecial ((This, t), G.empty_id_info ())) |> G.e
          else if String.equal s "super" then
            N (IdSpecial ((Super, t), G.empty_id_info ())) |> G.e
          else N (H2.name_of_id id) |> G.e
      | `Op_id tok ->
          let id = str env tok in
          N (H2.name_of_id id) |> G.e
      | `Choice_non_null_lit x -> literal_to_expr env x
      | `Inte_str_exp x -> interpolated_string_expression env x
      | `Unit (v1, v2) ->
          let l = token env v1 in
          let r = token env v2 in
          L (Unit (Tok.combine_toks l [ r ])) |> G.e
      | `Tuple_exp (v1, v2, v3, _v4, v5) ->
          let l = token env v1 in
          let first = expression env v2 in
          let rest = List.map (fun (_comma, e) -> expression env e) v3 in
          let r = token env v5 in
          Container (Tuple, (l, first :: rest, r)) |> G.e
      | `Wild tok ->
          let id = str env tok in
          if in_pattern env then N (H2.name_of_id id) |> G.e
          else N (H2.name_of_id id) |> G.e
      | `Blk_ x -> block_expr env x
      | `Splice_exp x -> splice_target env x
      | `Case_blk x -> case_block_to_expr env x
      | `Quote_exp x -> quote_target env x
      | `Inst_exp x -> instance_expression env x
      | `Paren_exp x ->
          let _l, e, _r = parenthesized_expression env x in
          e
      | `Field_exp (v1, v2, v3) ->
          let e = simple_expression env v1 in
          let dot = token env v2 in
          let fld = type_identifier env v3 in
          DotAccess (e, dot, FN (H2.name_of_id fld)) |> G.e
      | `Gene_func (v1, v2) -> (
          let e = expression env v1 in
          let l, targs, r = type_arguments env v2 in
          (* Special case: super[TraitName] is qualified super, not generic function *)
          match (e.e, targs) with
          | ( N (IdSpecial ((Super, _), _) as super_name),
              [ TA { t = TyN qual_name; _ } ] ) ->
              (* super[Trait] -> DotAccess(Id("Trait"), FN(IdSpecial(Super)))
                 to match pfff parser representation *)
              let qual_expr = N qual_name |> G.e in
              DotAccess (qual_expr, Tok.unsafe_fake_tok ".", FN super_name)
              |> G.e
          (* x.asInstanceOf[T] is a cast *)
          | ( DotAccess (receiver, dot, FN (Id (("asInstanceOf", _), _))),
              [ TA t ] ) ->
              Cast (t, dot, receiver) |> G.e
          | _ ->
              let type_args =
                List.map
                  (fun ta ->
                    match ta with
                    | TA t -> G.ArgType t
                    | TAExpr ex -> G.Arg ex
                    | TAWildcard _ ->
                        G.ArgType
                          (OtherType (("TAWildcard", Tok.unsafe_fake_tok ""), [])
                          |> G.t)
                    | OtherTypeArg _ ->
                        G.ArgType
                          (OtherType
                             (("OtherTypeArg", Tok.unsafe_fake_tok ""), [])
                          |> G.t))
                  targs
              in
              Call (e, (l, type_args, r)) |> G.e)
      | `Call_exp x -> call_expression env x)
  | `Semg_meta tok ->
      let id = str env tok in
      N (H2.name_of_id id) |> G.e
  | `Deep_exp (lop, e, rop) ->
      let l = token env lop in
      let inner = expression env e in
      let r = token env rop in
      DeepEllipsis (l, inner, r) |> G.e
  | `Semg_ellips_meta tok ->
      let id = str env tok in
      let tok_t = token env tok in
      DisjExpr (Ellipsis tok_t |> G.e, N (H2.name_of_id id) |> G.e) |> G.e
  | `Semg_ellips tok -> Ellipsis (token env tok) |> G.e
  | `Symb_lit (_quote, id) ->
      let s, t = identifier env id in
      L (Atom (t, (s, t))) |> G.e

and quote_target (env : env) (x : CST.quote_expression) : expr =
  let _quote_tok, body = x in
  match body with
  | `LCURL_opt_blk_RCURL x ->
      let e = block_expr env x in
      OtherExpr (("QuoteBlock", Tok.unsafe_fake_tok "'"), [ G.E e ]) |> G.e
  | `LBRACK_type_RBRACK (_l, ty, _r) ->
      let t = type_ env ty in
      OtherExpr (("QuoteType", Tok.unsafe_fake_tok "'"), [ G.T t ]) |> G.e
  | `Id x ->
      (* In Scala 2, 'foo is a symbol literal (Atom).
         In Scala 3, 'identifier is a quoted expression.
         We produce Atom for backward compatibility with Scala 2 code. *)
      let s, t = identifier env x in
      L (Atom (t, (s, t))) |> G.e

and splice_target (env : env) (x : CST.splice_expression) : expr =
  let _dollar_tok, body = x in
  match body with
  | `LCURL_blk_RCURL (_l, blk, _r) ->
      let stmts = block env blk in
      let l = Tok.unsafe_fake_tok "{" in
      let r = Tok.unsafe_fake_tok "}" in
      stmt_to_expr (Block (l, stmts, r) |> G.s)
  | `LBRACK_type_RBRACK (_l, ty, _r) ->
      let t = type_ env ty in
      OtherExpr (("SpliceType", Tok.unsafe_fake_tok "$"), [ G.T t ]) |> G.e
  | `Id x ->
      let id = identifier env x in
      N (H2.name_of_id id) |> G.e

and call_expression (env : env) (x : CST.call_expression) : expr =
  match x with
  | `Simple_exp_choice_args (v1, v2) -> (
      let func = simple_expression env v1 in
      let args =
        match v2 with
        | `Args x -> arguments env x
        | `Case_blk x ->
            let e = case_block_to_expr env x in
            fb [ Arg e ]
        | `Blk_ x ->
            let e = block_expr env x in
            fb [ Arg e ]
      in
      (* Foo.apply(args) -> new Foo(args) when Foo is capitalized *)
      match func.e with
      | DotAccess
          ( { e = N (Id (((s, _) as id), _)); _ },
            _dot,
            FN (Id (("apply", apply_tok), _)) )
        when String_.is_capitalized s ->
          New
            (apply_tok, TyN (H2.name_of_id id) |> G.t, G.empty_id_info (), args)
          |> G.e
      | _ -> Call (func, args) |> G.e)
  | `Post_exp_choice_COLON_colon_arg (v1, _colon, v3) ->
      let e = postfix_expression_choice env v1 in
      let arg = colon_argument_to_expr env v3 in
      Call (e, fb [ Arg arg ]) |> G.e

and instance_expression (env : env) (x : CST.instance_expression) : expr =
  match x with
  | `New_cons_app_temp_body (_v1, v2, v3) ->
      let ty, args = constructor_application_to_type_and_args env v2 in
      let cbody = template_body_to_class_body env v3 in
      let cdef =
        {
          ckind = (Object, Tok.unsafe_fake_tok "object");
          cextends = [ (ty, Some args) ];
          cimplements = [];
          cmixins = [];
          cparams = fb [];
          cbody;
        }
      in
      let cl = AnonClass cdef |> G.e in
      Call (cl, fb []) |> G.e
  | `New_temp_body (v1, v2) ->
      let new_tok = token env v1 in
      let ty = TyN (H2.name_of_id ("AnyRef", new_tok)) |> G.t in
      let cbody = template_body_to_class_body env v2 in
      let cdef =
        {
          ckind = (Object, Tok.unsafe_fake_tok "object");
          cextends = [ (ty, None) ];
          cimplements = [];
          cmixins = [];
          cparams = fb [];
          cbody;
        }
      in
      let cl = AnonClass cdef |> G.e in
      Call (cl, fb []) |> G.e
  | `New_cons_app (v1, v2) ->
      let new_tok = token env v1 in
      let ty, args = constructor_application_to_type_and_args env v2 in
      let init = G.empty_id_info () in
      New (new_tok, ty, init, args) |> G.e

and constructor_application_to_type_and_args (env : env)
    (x : CST.constructor_application) : type_ * arguments =
  match x with
  | `Anno_type x -> (annotated_type env x, fb [])
  | `Comp_type x -> (compound_type env x, fb [])
  | `Stru_type x ->
      let _l, fields, _r = template_body_to_class_body env x in
      let field_anys = List.map (fun f -> G.Fld f) fields in
      ( OtherType (("StructuralType", Tok.unsafe_fake_tok ""), field_anys) |> G.t,
        fb [] )
  | `Simple_type_args (ty, args) ->
      let ty = simple_type env ty in
      let args = arguments env args in
      (ty, args)
  | `Anno_type_args (at, args) ->
      let ty = annotated_type env at in
      let args = arguments env args in
      (ty, args)
  | `Comp_type_args (ct, args) ->
      let ty = compound_type env ct in
      let args = arguments env args in
      (ty, args)

and interpolated_string_expression (env : env)
    (x : CST.interpolated_string_expression) : expr =
  match x with
  | `Raw_str_start_raw_str (_start, raw) -> raw_string_to_expr env raw
  | `Id_inte_str (id, istr) ->
      let prefix, _tok = identifier env id in
      interpolated_string_to_expr ~prefix env istr

and raw_string_to_expr (env : env) (x : CST.raw_string) : expr =
  match x with
  | `Simple_str_start_rep_raw_str_middle_choice_dollar_esc_single_line_str_end
      (v1, v2, _v3) -> (
      let start_tok = token env v1 in
      let parts =
        List.map
          (fun (mid, interp) ->
            let s, t = str env mid in
            let str_part = L (String (fb (s, t))) |> G.e in
            let interp_part =
              match interp with
              | `Dollar_esc _tok -> L (String (fb ("$", start_tok))) |> G.e
              | `Interp x -> interpolation env x
            in
            [ str_part; interp_part ])
          v2
      in
      let all_parts = List_.flatten parts in
      match all_parts with
      | [] -> L (String (fb ("", start_tok))) |> G.e
      | [ e ] -> e
      | _ ->
          Call
            ( G.Special (ConcatString InterpolatedConcat, start_tok) |> G.e,
              fb (List.map (fun e -> Arg e) all_parts) )
          |> G.e)
  | `Simple_mult_str_start_rep_raw_str_mult_middle_choice_dollar_esc_mult_str_end
      (v1, v2, _v3) -> (
      let start_tok = token env v1 in
      let parts =
        List.map
          (fun (mid, interp) ->
            let s, t = str env mid in
            let str_part = L (String (fb (s, t))) |> G.e in
            let interp_part =
              match interp with
              | `Dollar_esc _tok -> L (String (fb ("$", start_tok))) |> G.e
              | `Interp x -> interpolation env x
            in
            [ str_part; interp_part ])
          v2
      in
      let all_parts = List_.flatten parts in
      match all_parts with
      | [] -> L (String (fb ("", start_tok))) |> G.e
      | [ e ] -> e
      | _ ->
          Call
            ( G.Special (ConcatString InterpolatedConcat, start_tok) |> G.e,
              fb (List.map (fun e -> Arg e) all_parts) )
          |> G.e)

and interpolated_string_to_expr ~prefix (env : env)
    (x : CST.interpolated_string) : expr =
  (* Use FString(prefix) to distinguish interpolated strings by their prefix
   * (e.g. 's"..."', 'f"..."', 'abcd"..."'). This allows patterns like
   * 'abcd"..."' to match only strings with prefix 'abcd' rather than all
   * interpolated strings. *)
  let concat_kind = FString prefix in
  match x with
  | `Imm_tok_dquot_rep_inte_str_middle_choice_dollar_esc_single_line_str_end
      (v1, v2, v3) -> (
      let start_tok = token env v1 in
      let parts =
        List.map
          (fun (mid, interp) ->
            let s, t = str env mid in
            let str_part = L (String (fb (s, t))) |> G.e in
            let interp_part =
              match interp with
              | `Dollar_esc _tok -> L (String (fb ("$", start_tok))) |> G.e
              | `Interp x ->
                  let e = interpolation env x in
                  Call
                    ( G.Special (InterpolatedElement, start_tok) |> G.e,
                      fb [ Arg e ] )
                  |> G.e
              | `Esc_seq tok ->
                  let s, t = str env tok in
                  L (String (fb (s, t))) |> G.e
            in
            [ str_part; interp_part ])
          v2
      in
      let all_parts = List_.flatten parts in
      (* Include trailing content from end token *)
      let end_s, end_t = str env v3 in
      let end_content =
        if
          String.length end_s > 0
          && Char.equal (String.get end_s (String.length end_s - 1)) '"'
        then String.sub end_s 0 (String.length end_s - 1)
        else end_s
      in
      let all_parts =
        if String.length end_content > 0 then
          all_parts @ [ L (String (fb (end_content, end_t))) |> G.e ]
        else all_parts
      in
      match all_parts with
      | [] ->
          Call (G.Special (ConcatString concat_kind, start_tok) |> G.e, fb [])
          |> G.e
      | _ ->
          Call
            ( G.Special (ConcatString concat_kind, start_tok) |> G.e,
              fb (List.map (fun e -> Arg e) all_parts) )
          |> G.e)
  | `Imm_tok_dquo_rep_inte_mult_str_middle_choice_dollar_esc_mult_str_end
      (v1, v2, v3) -> (
      let start_tok = token env v1 in
      let parts =
        List.map
          (fun (mid, interp) ->
            let s, t = str env mid in
            let str_part = L (String (fb (s, t))) |> G.e in
            let interp_part =
              match interp with
              | `Dollar_esc _tok -> L (String (fb ("$", start_tok))) |> G.e
              | `Interp x ->
                  let e = interpolation env x in
                  Call
                    ( G.Special (InterpolatedElement, start_tok) |> G.e,
                      fb [ Arg e ] )
                  |> G.e
            in
            [ str_part; interp_part ])
          v2
      in
      let all_parts = List_.flatten parts in
      (* Include trailing content from end token *)
      let end_s, end_t = str env v3 in
      let end_content =
        if String.length end_s > 0 then
          (* Remove trailing triple-quote if present *)
          let s = end_s in
          if
            String.length s >= 3
            && Char.equal (String.get s (String.length s - 1)) '"'
            && Char.equal (String.get s (String.length s - 2)) '"'
            && Char.equal (String.get s (String.length s - 3)) '"'
          then String.sub s 0 (String.length s - 3)
          else s
        else end_s
      in
      let all_parts =
        if String.length end_content > 0 then
          all_parts @ [ L (String (fb (end_content, end_t))) |> G.e ]
        else all_parts
      in
      match all_parts with
      | [] ->
          Call (G.Special (ConcatString concat_kind, start_tok) |> G.e, fb [])
          |> G.e
      | _ ->
          Call
            ( G.Special (ConcatString concat_kind, start_tok) |> G.e,
              fb (List.map (fun e -> Arg e) all_parts) )
          |> G.e)

and interpolation (env : env) ((_dollar, v2) : CST.interpolation) : expr =
  match v2 with
  | `Alia_interp_id tok ->
      let s, t = str env tok in
      (* In pattern mode, metavariable names like $VAR1 must keep the '$' prefix
       * so that the pattern matcher recognizes them as metavariables. The tree-sitter
       * tokenizer strips the leading '$' from the identifier, so we restore it here. *)
      let s =
        if in_pattern env && AST_generic.is_metavar_name ("$" ^ s) then "$" ^ s
        else s
      in
      N (H2.name_of_id (s, t)) |> G.e
  | `Blk_ x -> block_expr env x

and parenthesized_expression (env : env)
    ((v1, v2, v3) : CST.parenthesized_expression) : Tok.t * expr * Tok.t =
  let l = token env v1 in
  let e = expression env v2 in
  let r = token env v3 in
  (l, e, r)

and arguments (env : env) ((v1, v2, v3) : CST.arguments) : arguments =
  let l = token env v1 in
  let args =
    match v2 with
    | `Opt_exprs_in_parens opt -> (
        match opt with
        | Some x -> exprs_in_parens env x
        | None -> [])
    | `Using_exprs_in_parens (_using_tok, x) -> exprs_in_parens env x
  in
  let r = token env v3 in
  (l, args, r)

and exprs_in_parens (env : env) ((v1, v2, _v3) : CST.exprs_in_parens) :
    argument list =
  let first = Arg (expression env v1) in
  let rest = List.map (fun (_comma, e) -> Arg (expression env e)) v2 in
  first :: rest

and indentable_expression (env : env) (x : CST.indentable_expression) : expr =
  match x with
  | `Inde_blk x -> indented_block_to_expr env x
  | `Inde_cases x -> indented_cases_to_expr env x
  | `Exp x -> expression env x

and indentable_expression_to_stmt (env : env) (x : CST.indentable_expression) :
    stmt =
  match x with
  | `Inde_blk x ->
      let stmts = indented_block env x in
      let l = Tok.unsafe_fake_tok "{" in
      let r = Tok.unsafe_fake_tok "}" in
      Block (l, stmts, r) |> G.s
  | `Inde_cases x ->
      let e = indented_cases_to_expr env x in
      ExprStmt (e, G.sc) |> G.s
  | `Exp x -> (
      let e = expression env x in
      (* Avoid double-wrapping StmtExpr(s) in ExprStmt: when an expression is
       * itself a statement in disguise, return the inner statement directly.
       * This is important for e.g. `else if cond then ...` which parses as
       * ExprStmt(StmtExpr(If(...))), but we want just If(...) as a statement
       * so that implicit-return analysis can properly recurse into branches. *)
      match e.G.e with
      | G.StmtExpr s -> s
      | _ -> ExprStmt (e, G.sc) |> G.s)

and indented_block_to_expr (env : env) (x : CST.indented_block) : expr =
  let stmts = indented_block env x in
  let l = Tok.unsafe_fake_tok "{" in
  let r = Tok.unsafe_fake_tok "}" in
  (* Avoid wrapping a single expression in an extra Block + StmtExpr layer,
   * which would interfere with implicit-return analysis. For a single
   * expression statement, return the expression directly. *)
  match stmts with
  | [ { G.s = G.ExprStmt (e, _); _ } ] -> e
  | _ -> stmt_to_expr (Block (l, stmts, r) |> G.s)

and indented_block (env : env)
    ((_indent, v2, _outdent, _end_marker) : CST.indented_block) : stmt list =
  block env v2

and indented_cases_to_expr (env : env) (x : CST.indented_cases) : expr =
  let cases = indented_cases env x in
  let l = Tok.unsafe_fake_tok "{" in
  let _r = Tok.unsafe_fake_tok "}" in
  let hidden_param_name = "!hidden_scala_param!" in
  let partial_match =
    Lambda
      {
        fkind = (BlockCases, l);
        fparams =
          fb
            [
              Param
                (G.param_of_id ~fake:true
                   (hidden_param_name, Tok.unsafe_fake_tok ""));
            ];
        frettype = None;
        fbody =
          FBExpr
            (StmtExpr
               (Switch
                  ( l,
                    Some
                      (Cond
                         (N
                            (H2.name_of_id ~fake:true
                               (hidden_param_name, Tok.unsafe_fake_tok ""))
                         |> G.e)),
                    cases )
               |> G.s)
            |> G.e);
      }
  in
  partial_match |> G.e

and indented_cases (env : env) ((_indent, v2, _outdent) : CST.indented_cases) :
    case_and_body list =
  List.map (case_clause env) v2

and block_expr (env : env) ((v1, blk_opt, v3) : CST.block_) : expr =
  let l = token env v1 in
  let stmts =
    match blk_opt with
    | Some blk -> block env blk
    | None -> []
  in
  let r = token env v3 in
  stmt_to_expr (Block (l, stmts, r) |> G.s)

and block (env : env) ((v1, v2, _v3) : CST.block) : stmt list =
  let first = block_item env v1 in
  let rest = List.concat_map (fun (_semi, item) -> block_item env item) v2 in
  first @ rest

and block_item (env : env) (x : CST.anon_choice_exp_5763a53) : stmt list =
  match x with
  | `Exp x ->
      let e = expression env x in
      [ ExprStmt (e, G.sc) |> G.s ]
  | `Choice_choice_given_defi x -> definition env x
  | `End_marker _x -> []
  | `SEMI _tok -> []

and match_body (env : env)
    (x : [ `Case_blk of CST.case_block | `Inde_cases of CST.indented_cases ]) :
    case_and_body list =
  match x with
  | `Case_blk x -> case_block env x
  | `Inde_cases x -> indented_cases env x

and case_block (env : env) (x : CST.case_block) : case_and_body list =
  match x with
  | `LCURL_RCURL (_l, _r) -> []
  | `LCURL_rep1_case_clause_RCURL (_l, clauses, _r) ->
      List.map (case_clause env) clauses

and case_block_to_expr (env : env) (x : CST.case_block) : expr =
  let cases, l, r =
    match x with
    | `LCURL_RCURL (l, r) -> ([], token env l, token env r)
    | `LCURL_rep1_case_clause_RCURL (l, clauses, r) ->
        (List.map (case_clause env) clauses, token env l, token env r)
  in
  let hidden_param_name = "!hidden_scala_param!" in
  let partial_match =
    Lambda
      {
        fkind = (BlockCases, l);
        fparams =
          ( l,
            [
              Param
                (G.param_of_id ~fake:true
                   (hidden_param_name, Tok.unsafe_fake_tok ""));
            ],
            r );
        frettype = None;
        fbody =
          FBExpr
            (StmtExpr
               (Switch
                  ( l,
                    Some
                      (Cond
                         (N
                            (H2.name_of_id ~fake:true
                               (hidden_param_name, Tok.unsafe_fake_tok ""))
                         |> G.e)),
                    cases )
               |> G.s)
            |> G.e);
      }
  in
  partial_match |> G.e

and case_clause (env : env) ((_case_tok, v2, v3) : CST.case_clause) :
    case_and_body =
  let pat, guard = case_pattern env v2 in
  let body =
    match v3 with
    | Some blk ->
        let stmts = block env blk in
        let l = Tok.unsafe_fake_tok "{" in
        let r = Tok.unsafe_fake_tok "}" in
        Block (l, stmts, r) |> G.s
    | None ->
        Block (Tok.unsafe_fake_tok "{", [], Tok.unsafe_fake_tok "}") |> G.s
  in
  let cond =
    match guard with
    | Some guard_expr ->
        Case (Tok.unsafe_fake_tok "case", PatWhen (pat, guard_expr))
    | None -> Case (Tok.unsafe_fake_tok "case", pat)
  in
  CasesAndBody ([ cond ], body)

and case_pattern (env : env) ((v1, v2, _v3) : CST.case_pattern) :
    pattern * expr option =
  let pat = pattern env v1 in
  let guard =
    match v2 with
    | Some x -> Some (guard env x)
    | None -> None
  in
  (pat, guard)

and guard (env : env) ((_if_tok, v2) : CST.guard) : expr =
  postfix_expression_choice env v2

and expr_case_clause (env : env) ((_case_tok, v2, v3) : CST.expr_case_clause) :
    case_and_body =
  let pat, guard_opt = case_pattern env v2 in
  let body_expr = expression env v3 in
  let body = ExprStmt (body_expr, G.sc) |> G.s in
  let cond =
    match guard_opt with
    | Some g -> Case (Tok.unsafe_fake_tok "case", PatWhen (pat, g))
    | None -> Case (Tok.unsafe_fake_tok "case", pat)
  in
  CasesAndBody ([ cond ], body)

and catch_cases_of_case_and_body (cabs : case_and_body list) : catch list =
  (* Convert each case clause into an individual catch entry.
   * This is the Scala representation where each case in a catch block
   * becomes a separate CatchPattern catch. *)
  List.filter_map
    (fun cab ->
      match cab with
      | CasesAndBody ([ Case (_tok, pat) ], body) ->
          Some (Tok.unsafe_fake_tok "catch", CatchPattern pat, body)
      | _ -> None)
    cabs

and catch_clause (env : env) ((_catch_tok, v2) : CST.catch_clause) : catch list
    =
  match v2 with
  | `Inde_exp ie -> (
      (* Handle the different Scala catch forms:
       * - `catch { case ... }` → Exp(Case_blk): extract case clauses
       * - `catch\n  case ...` → Inde_cases: extract case clauses
       * - other expressions (e.g. `catch 4`): wildcard catch
       * For sgrep patterns, `catch { ... }` (plain block with ellipsis) uses
       * PatEllipsis to match any catch pattern. *)
      match ie with
      | `Inde_cases x -> (
          let cases = indented_cases env x in
          let catches = catch_cases_of_case_and_body cases in
          match catches with
          | [] ->
              let e = indented_cases_to_expr env x in
              let exn = CatchPattern (PatWildcard (Tok.unsafe_fake_tok "_")) in
              [ (Tok.unsafe_fake_tok "catch", exn, ExprStmt (e, G.sc) |> G.s) ]
          | _ :: _ -> catches)
      | `Exp x -> (
          match x with
          | `Simple_exp (`Choice_id (`Case_blk cb)) -> (
              (* `catch { case ... }` — Scala 2 syntax: a case_block expression *)
              let cases = case_block env cb in
              let catches = catch_cases_of_case_and_body cases in
              match catches with
              | [] ->
                  (* empty case_block `catch {}` or unhandled cases *)
                  let exn =
                    CatchPattern (PatEllipsis (Tok.unsafe_fake_tok "..."))
                  in
                  [
                    ( Tok.unsafe_fake_tok "catch",
                      exn,
                      Block
                        (Tok.unsafe_fake_tok "{", [], Tok.unsafe_fake_tok "}")
                      |> G.s );
                  ]
              | _ :: _ -> catches)
          | `Simple_exp (`Choice_id (`Blk_ _)) ->
              (* `catch { ... }` — plain block, used in sgrep patterns.
               * Use PatEllipsis so the pattern matches any catch clause, and
               * ExprStmt(Ellipsis) as the body to match any body via the
               * m_stmt ellipsis rule. *)
              let exn =
                CatchPattern (PatEllipsis (Tok.unsafe_fake_tok "..."))
              in
              let body =
                ExprStmt (Ellipsis (Tok.unsafe_fake_tok "...") |> G.e, G.sc)
                |> G.s
              in
              [ (Tok.unsafe_fake_tok "catch", exn, body) ]
          | _ ->
              let e = expression env x in
              let body = ExprStmt (e, G.sc) |> G.s in
              let exn = CatchPattern (PatWildcard (Tok.unsafe_fake_tok "_")) in
              [ (Tok.unsafe_fake_tok "catch", exn, body) ])
      | `Inde_blk _ ->
          let e = indentable_expression env ie in
          let body = ExprStmt (e, G.sc) |> G.s in
          let exn = CatchPattern (PatWildcard (Tok.unsafe_fake_tok "_")) in
          [ (Tok.unsafe_fake_tok "catch", exn, body) ])
  | `Expr_case_clause x -> (
      let cb = expr_case_clause env x in
      match cb with
      | CasesAndBody ([ Case (_tok, pat) ], body) ->
          let exn = CatchPattern pat in
          [ (Tok.unsafe_fake_tok "catch", exn, body) ]
      | _ ->
          let exn = CatchPattern (PatWildcard (Tok.unsafe_fake_tok "_")) in
          [
            ( Tok.unsafe_fake_tok "catch",
              exn,
              Block (Tok.unsafe_fake_tok "{", [], Tok.unsafe_fake_tok "}")
              |> G.s );
          ])

(* ---------------------------------------------------------------------- *)
(* Patterns *)
(* ---------------------------------------------------------------------- *)

and pattern (env : env) (x : CST.pattern) : pattern =
  match x with
  | `Choice_choice_id inner -> (
      match inner with
      | `Choice_id x ->
          let id = type_identifier env x in
          let s = fst id in
          if in_pattern env && AST_generic.is_metavar_name s then
            PatId (id, G.empty_id_info ())
          else if
            String.length s > 0
            && not (Char.equal (Char.lowercase_ascii s.[0]) s.[0])
          then
            (* In Scala, patterns starting with uppercase are stable identifiers *)
            PatConstructor (H2.name_of_id id, [])
          else PatId (id, G.empty_id_info ())
      | `Stable_id x ->
          let ids = stable_identifier_to_dotted_ident env x in
          let n = H2.name_of_ids ids in
          PatConstructor (n, [])
      | `Inte_str_exp x ->
          let e = interpolated_string_expression env x in
          OtherPat (("InterpolatedStringPat", Tok.unsafe_fake_tok ""), [ G.E e ])
      | `Capt_pat (v1, _at, v3) ->
          let id =
            match v1 with
            | `Choice_id x -> type_identifier env x
            | `Wild tok -> str env tok
          in
          let inner = pattern env v3 in
          PatAs (inner, (id, G.empty_id_info ()))
      | `Tuple_pat (v1, v2, v3, _v4, v5) ->
          let l = token env v1 in
          let first = pattern env v2 in
          let rest = List.map (fun (_comma, p) -> pattern env p) v3 in
          let r = token env v5 in
          PatTuple (l, first :: rest, r)
      | `Named_tuple_pat (v1, v2, v3, _v4, v5) ->
          let l = token env v1 in
          let first = named_pattern env v2 in
          let rest = List.map (fun (_comma, np) -> named_pattern env np) v3 in
          let r = token env v5 in
          PatTuple (l, first :: rest, r)
      | `Case_class_pat (v1, _v2, v3, _v4) ->
          let name = anon_choice_type_id_ae98204_to_name env v1 in
          let pats =
            match v3 with
            | `Opt_choice_choice_choice_id_rep_COMMA_choice_choice_choice_id_opt_COMMA
                opt -> (
                match opt with
                | Some (first, rest, _trailing) ->
                    let first = pattern env first in
                    let rest =
                      List.map (fun (_comma, p) -> pattern env p) rest
                    in
                    first :: rest
                | None -> [])
            | `Opt_named_pat_rep_COMMA_named_pat_opt_COMMA opt -> (
                match opt with
                | Some (first, rest, _trailing) ->
                    let first = named_pattern env first in
                    let rest =
                      List.map (fun (_comma, np) -> named_pattern env np) rest
                    in
                    first :: rest
                | None -> [])
          in
          PatConstructor (name, pats)
      | `Infix_pat (v1, v2, v3) ->
          let lhs = pattern env v1 in
          let op = type_identifier env v2 in
          let rhs = pattern env v3 in
          PatConstructor (H2.name_of_id op, [ lhs; rhs ])
      | `Alt_pat (v1, _pipe, v3) ->
          let p1 = pattern env v1 in
          let p3 = pattern env v3 in
          PatDisj (p1, p3)
      | `Typed_pat (v1, _colon, v3) ->
          let pat = pattern env v1 in
          let ty = type_ env v3 in
          PatTyped (pat, ty)
      | `Given_pat (_given_tok, v2) ->
          let ty = type_ env v2 in
          OtherPat (("GivenPat", Tok.unsafe_fake_tok ""), [ G.T ty ])
      | `Quote_exp x ->
          let e = quote_target env x in
          OtherPat (("QuotePat", Tok.unsafe_fake_tok ""), [ G.E e ])
      | `Choice_non_null_lit x ->
          let lit = literal env x in
          PatLiteral lit
      | `Wild tok ->
          let _t = token env tok in
          PatWildcard (token env tok)
      | `Repeat_pat (v1, _star) ->
          let pat = pattern env v1 in
          OtherPat (("RepeatPat", Tok.unsafe_fake_tok ""), [ G.P pat ]))
  | `Semg_ellips tok -> PatEllipsis (token env tok)

and named_pattern (env : env) ((_id, _eq, v3) : CST.named_pattern) : pattern =
  pattern env v3

(* ---------------------------------------------------------------------- *)
(* Definitions *)
(* ---------------------------------------------------------------------- *)

and definition (env : env) (x : CST.definition) : stmt list =
  match x with
  | `Semg_val_or_var_defi (meta_tok, pat_or_ids, ascription_opt, _eq, init) ->
      let id = str env meta_tok in
      let attrs = [ KeywordAttr (Const, Tok.unsafe_fake_tok "val") ] in
      val_or_var_def env attrs false pat_or_ids ascription_opt init
      |> List.map (fun s ->
          match s.s with
          | DefStmt (ent, def) ->
              DefStmt ({ ent with name = EN (H2.name_of_id id) }, def) |> G.s
          | _ -> s)
  | `Choice_given_defi x -> definition_kind env x

and definition_kind (env : env) x : stmt list =
  match x with
  | `Given_defi (v1, v2, _given_tok, v4, _given_sigs, v6) ->
      let attrs = annotations env v1 @ modifiers_opt env v2 in
      let name_opt =
        match v4 with
        | Some gc -> given_constructor_name env gc
        | None -> None
      in
      let ty_and_init =
        match v6 with
        | `Stru_inst (_cons_app, _sep, _body) -> (None, None)
        | `Anno_type_opt_EQ_inde_exp (at, init_opt) ->
            let ty = annotated_type env at in
            let init =
              match init_opt with
              | Some (_eq, ie) -> Some (indentable_expression env ie)
              | None -> None
            in
            (Some ty, init)
      in
      let id =
        match name_opt with
        | Some id -> id
        | None -> ("given", Tok.unsafe_fake_tok "given")
      in
      let ent = G.basic_entity id ~attrs in
      let vtype, vinit = ty_and_init in
      let vdef = { vinit; vtype; vtok = G.no_sc } in
      [ DefStmt (ent, VarDef vdef) |> G.s ]
  | `Exte_defi (_ext_tok, tparams_opt, given_conds, v4) ->
      let tparams =
        match tparams_opt with
        | Some tp ->
            Anys
              (List.map
                 (fun p -> Tp p)
                 (let _, ps, _ = type_parameters env tp in
                  ps))
        | None -> Anys []
      in
      let params =
        List.map
          (fun gc ->
            let ps = parameters_to_params env gc in
            Anys (List.map (fun p -> Pa p) ps))
          given_conds
      in
      let methods =
        match v4 with
        | `Exte_temp_body x -> extension_template_body env x
        | `Func_defi x -> [ function_definition env x ]
        | `Func_decl x -> [ function_declaration env x ]
      in
      let method_anys = Anys (List.map (fun s -> S s) methods) in
      [ OtherStmt (OS_Extension, (tparams :: params) @ [ method_anys ]) |> G.s ]
  | `Class_defi (v1, v2, v3, _class_tok, v5) ->
      let attrs = annotations env v1 @ modifiers_opt env v2 in
      let case_attr =
        match v3 with
        | Some tok -> [ G.attr RecordClass (token env tok) ]
        | None -> []
      in
      [
        class_definition_ env (attrs @ case_attr)
          (Class, Tok.unsafe_fake_tok "class")
          v5;
      ]
  | `Import_decl (v1, v2, v3) ->
      let import_tok = token env v1 in
      let first = namespace_expression_to_stmts env import_tok v2 in
      let rest =
        List.concat_map
          (fun (_comma, ns) -> namespace_expression_to_stmts env import_tok ns)
          v3
      in
      first @ rest
  | `Export_decl (v1, v2, v3) ->
      let export_tok = token env v1 in
      let first = namespace_expression_to_stmts env export_tok v2 in
      let rest =
        List.concat_map
          (fun (_comma, ns) -> namespace_expression_to_stmts env export_tok ns)
          v3
      in
      first @ rest
  | `Obj_defi (v1, v2, v3, _obj_tok, v5) ->
      let attrs = annotations env v1 @ modifiers_opt env v2 in
      let case_attr =
        match v3 with
        | Some tok -> [ G.attr RecordClass (token env tok) ]
        | None -> []
      in
      [ object_definition_ env (attrs @ case_attr) v5 ]
  | `Enum_defi (v1, _enum_tok, v3, v4, v5, v6) ->
      let attrs = annotations env v1 in
      let cname, tparams, _anno_opt, _access_opt, cparams_list = v3 in
      let id = type_identifier env cname in
      let tparams = Option.map (type_parameters env) tparams in
      let cparams =
        match cparams_list with
        | [] -> fb []
        | _ ->
            let all =
              List.concat_map
                (fun (_auto_semi_opt, cp) ->
                  let _, ps, _ = class_parameters env cp in
                  ps)
                cparams_list
            in
            fb all
      in
      let cextends, cmixins =
        match v4 with
        | Some x -> extends_clause env x
        | None -> ([], [])
      in
      let cimplements =
        match v5 with
        | Some (_derives_tok, first, rest) ->
            let names =
              anon_choice_type_id_ae98204_to_name env first
              :: List.map
                   (fun (_comma, n) ->
                     anon_choice_type_id_ae98204_to_name env n)
                   rest
            in
            List.map (fun n -> TyN n |> G.t) names
        | None -> []
      in
      let cbody = enum_body env v6 in
      let ent =
        G.basic_entity id
          ~attrs:(G.attr EnumClass (Tok.unsafe_fake_tok "enum") :: attrs)
          ?tparams
      in
      let cdef =
        {
          ckind = (Class, Tok.unsafe_fake_tok "class");
          cextends;
          cimplements;
          cmixins;
          cparams;
          cbody;
        }
      in
      [ DefStmt (ent, ClassDef cdef) |> G.s ]
  | `Trait_defi (v1, v2, _trait_tok, v4) ->
      let attrs = annotations env v1 @ modifiers_opt env v2 in
      [ class_definition_ env attrs (Trait, Tok.unsafe_fake_tok "trait") v4 ]
  | `Val_defi ((v1_annos, v1_mods, _val_tok), v2, v3, _eq, v5) ->
      let attrs = annotations env v1_annos @ modifiers_opt env v1_mods in
      val_or_var_def env attrs false v2 v3 v5
  | `Val_decl ((v1_annos, v1_mods, _val_tok), v2, v3, _colon, v5) ->
      let attrs = annotations env v1_annos @ modifiers_opt env v1_mods in
      val_or_var_decl env attrs false v2 v3 v5
  | `Var_defi ((v1_annos, v1_mods, _var_tok), v2, v3, _eq, v5) ->
      let attrs = annotations env v1_annos @ modifiers_opt env v1_mods in
      val_or_var_def env attrs true v2 v3 v5
  | `Var_decl ((v1_annos, v1_mods, _var_tok), v2, v3, _colon, v5) ->
      let attrs = annotations env v1_annos @ modifiers_opt env v1_mods in
      val_or_var_decl env attrs true v2 v3 v5
  | `Type_defi (v1, v2, opaque_opt, _type_tok, v5, v6) ->
      let opaque_attr =
        match opaque_opt with
        | Some tok -> [ G.OtherAttribute (("opaque", token env tok), []) ]
        | None -> []
      in
      let attrs = annotations env v1 @ modifiers_opt env v2 @ opaque_attr in
      let tc_name, tc_tparams, _lb, _ub, _cb = v5 in
      let id = type_identifier env tc_name in
      let tparams = Option.map (type_parameters env) tc_tparams in
      let tbody =
        match v6 with
        | Some (_eq, ty) ->
            let t = type_ env ty in
            NewType t
        | None -> AbstractType (Tok.unsafe_fake_tok "type")
      in
      let ent = G.basic_entity id ~attrs ?tparams in
      [ DefStmt (ent, TypeDef { tbody }) |> G.s ]
  | `Func_defi x -> [ function_definition env x ]
  | `Func_decl x -> [ function_declaration env x ]
  | `Pack_clause (v1, v2, v3) -> (
      let pkg_tok = token env v1 in
      let ids = package_identifier env v2 in
      let pkg_stmt = DirectiveStmt (Package (pkg_tok, ids) |> G.d) |> G.s in
      match v3 with
      | None -> [ pkg_stmt ]
      | Some body ->
          (* package p: ... body ... end p
           * Emit Package directive, then body stmts, then PackageEnd. *)
          let body_stmts = template_body_to_stmts env body in
          let end_tok = Tok.unsafe_fake_tok "}" in
          let pkg_end = DirectiveStmt (PackageEnd end_tok |> G.d) |> G.s in
          (pkg_stmt :: body_stmts) @ [ pkg_end ])
  | `Pack_obj (_pkg_tok, _obj_tok, v3) ->
      let tid, ext_opt, _derives_opt, body_opt = v3 in
      let id = type_identifier env tid in
      let cextends, cmixins =
        match ext_opt with
        | Some x -> extends_clause env x
        | None -> ([], [])
      in
      let cbody =
        match body_opt with
        | Some x -> definition_body env x
        | None -> fb []
      in
      let ent = G.basic_entity id in
      let cdef =
        {
          ckind = (Object, Tok.unsafe_fake_tok "object");
          cextends;
          cimplements = [];
          cmixins;
          cparams = fb [];
          cbody;
        }
      in
      [ DefStmt (ent, ClassDef cdef) |> G.s ]

and given_constructor_name (env : env)
    ((v1, _v2, _v3, _v4, _v5) : CST.given_constructor) : ident option =
  match v1 with
  | Some x -> Some (type_identifier env x)
  | None -> None

and val_or_var_def (env : env) (attrs : attribute list) (is_mutable : bool)
    (pat_or_ids : CST.anon_choice_pat_a6d147b)
    (ascription_opt : CST.self_type_ascription option)
    (init : CST.indentable_expression) : stmt list =
  let vtype =
    match ascription_opt with
    | Some (_colon, ty) -> Some (type_ env ty)
    | None -> None
  in
  let vinit = Some (indentable_expression env init) in
  let attrs =
    if is_mutable then attrs
    else KeywordAttr (Const, Tok.unsafe_fake_tok "val") :: attrs
  in
  match pat_or_ids with
  | `Choice_choice_choice_id pat -> (
      let p = pattern env pat in
      match p with
      | PatId (id, _info) ->
          let ent = G.basic_entity id ~attrs in
          let vdef = { vinit; vtype; vtok = G.no_sc } in
          [ DefStmt (ent, VarDef vdef) |> G.s ]
      | PatConstructor (name, []) ->
          (* Uppercase identifiers in val definitions are still definitions, not patterns *)
          let id =
            match name with
            | Id (id, _) -> id
            | _ -> ("_", Tok.unsafe_fake_tok "")
          in
          let ent = G.basic_entity id ~attrs in
          let vdef = { vinit; vtype; vtok = G.no_sc } in
          [ DefStmt (ent, VarDef vdef) |> G.s ]
      | _ ->
          (* Pattern-based val definition: val (a, b) = expr *)
          let e =
            match vinit with
            | Some e -> e
            | None -> L (Unit (Tok.unsafe_fake_tok "()")) |> G.e
          in
          [ ExprStmt (LetPattern (p, e) |> G.e, G.sc) |> G.s ])
  | `Idents (id1, _comma, id2, rest) ->
      let ids =
        identifier env id1 :: identifier env id2
        :: List.map (fun (_comma, id) -> identifier env id) rest
      in
      List.map
        (fun id ->
          let ent = G.basic_entity id ~attrs in
          let vdef = { vinit; vtype; vtok = G.no_sc } in
          DefStmt (ent, VarDef vdef) |> G.s)
        ids

and val_or_var_decl (env : env) (attrs : attribute list) (is_mutable : bool)
    (first_id : CST.type_identifier) (rest_ids : (_ * CST.type_identifier) list)
    (ty : CST.type_) : stmt list =
  let attrs =
    if is_mutable then attrs
    else KeywordAttr (Const, Tok.unsafe_fake_tok "val") :: attrs
  in
  let ids =
    type_identifier env first_id
    :: List.map (fun (_comma, tid) -> type_identifier env tid) rest_ids
  in
  let ty = type_ env ty in
  List.map
    (fun id ->
      let ent = G.basic_entity id ~attrs in
      let vdef = { vinit = None; vtype = Some ty; vtok = G.no_sc } in
      DefStmt (ent, VarDef vdef) |> G.s)
    ids

and function_definition (env : env) ((v1, v2) : CST.function_definition) : stmt
    =
  let ent, fparams, frettype, fattrs = function_declaration_ env v1 in
  let body =
    let e =
      match v2 with
      | `EQ_inde_exp (_eq, ie) -> indentable_expression env ie
      | `Blk_ x -> block_expr env x
    in
    (* Unwrap single-expression blocks in function bodies to match pfff
     * behavior. For `def f() = { expr }`, the braces produce a
     * StmtExpr(Block([ExprStmt(expr)])) wrapper that pfff's parser strips
     * via blockExprAsExpr. Without this, patterns like
     * `def $F(...) = { ... }` won't match targets parsed by pfff (which
     * lack the Block wrapper) because the pattern matcher requires
     * structural equality between StmtExpr and non-StmtExpr expressions. *)
    match e.G.e with
    | G.StmtExpr
        { G.s = G.Block (_, [ { G.s = G.ExprStmt (inner, _); _ } ], _); _ } ->
        FBExpr inner
    | _ -> FBExpr e
  in
  let fdef =
    {
      fkind = (Method, Tok.unsafe_fake_tok "def");
      fparams;
      frettype;
      fbody = body;
    }
  in
  let ent = { ent with attrs = ent.attrs @ fattrs } in
  DefStmt (ent, FuncDef fdef) |> G.s

and function_declaration (env : env) (x : CST.function_declaration) : stmt =
  let ent, fparams, frettype, fattrs = function_declaration_ env x in
  let fdef =
    {
      fkind = (Method, Tok.unsafe_fake_tok "def");
      fparams;
      frettype;
      fbody = FBDecl G.sc;
    }
  in
  let ent = { ent with attrs = ent.attrs @ fattrs } in
  DefStmt (ent, FuncDef fdef) |> G.s

and function_declaration_ (env : env)
    ((v1, v2, _def_tok, v4, v5) : CST.function_declaration_) :
    entity * parameters * type_ option * attribute list =
  let annos = annotations env v1 in
  let mods = modifiers_opt env v2 in
  let fc_name, fc_params_and_tparams, _auto_semi_opt = v4 in
  let id = type_identifier env fc_name in
  let params_acc = ref [] in
  let tparams_acc = ref None in
  List.iter
    (fun (_auto_semi_opt, pt) ->
      match pt with
      | `Params x ->
          let ps = parameters_to_params env x in
          params_acc := !params_acc @ ps
      | `Type_params x ->
          let tp = type_parameters env x in
          tparams_acc := Some tp)
    fc_params_and_tparams;
  let frettype =
    match v5 with
    | Some (_colon, ty) -> Some (type_ env ty)
    | None -> None
  in
  let fparams = fb !params_acc in
  let ent = G.basic_entity id ~attrs:(annos @ mods) ?tparams:!tparams_acc in
  (ent, fparams, frettype, [])

and parameters_to_params (env : env) (x : CST.parameters) : parameter list =
  match x with
  | `LPAR_opt_impl_opt_param_rep_COMMA_param_opt_COMMA_RPAR
      (_l, impl_opt, params_opt, _r) -> (
      let implicit_attr =
        match impl_opt with
        | Some tok -> [ G.OtherAttribute (("implicit", token env tok), []) ]
        | None -> []
      in
      let add_implicit_attr = function
        | Param p -> Param { p with pattrs = implicit_attr @ p.pattrs }
        | other -> other
      in
      match params_opt with
      | Some (first, rest, _trailing) ->
          let first = parameter env first in
          let rest = List.map (fun (_comma, p) -> parameter env p) rest in
          List.map add_implicit_attr (first :: rest)
      | None -> [])
  | `Using_params_clause (_l, using_tok, v3, _r) ->
      let using_attr = G.OtherAttribute (("using", token env using_tok), []) in
      let add_using_attr = function
        | Param p -> Param { p with pattrs = using_attr :: p.pattrs }
        | other -> other
      in
      let params =
        match v3 with
        | `Param_rep_COMMA_param_opt_COMMA (first, rest, _trailing) ->
            let first = parameter env first in
            let rest = List.map (fun (_comma, p) -> parameter env p) rest in
            first :: rest
        | `Choice_type_rep_COMMA_choice_type_opt_COMMA (first, rest, _trailing)
          ->
            let first = param_type env first in
            let rest = List.map (fun (_comma, pt) -> param_type env pt) rest in
            List.map (fun t -> Param (G.param_of_type t)) (first :: rest)
      in
      List.map add_using_attr params

and parameter (env : env) (x : CST.parameter) : parameter =
  match x with
  | `Rep_anno_opt_inline_modi_choice_id_COLON_choice_type_opt_EQ_exp
      (annos, inline_opt, v3, _colon, v5, v6) ->
      let anno_attrs = annotations env annos in
      let inline_attr =
        match inline_opt with
        | Some tok -> [ KeywordAttr (Inline, token env tok) ]
        | None -> []
      in
      let pattrs = anno_attrs @ inline_attr in
      let id = type_identifier env v3 in
      let is_repeated =
        match v5 with
        | `Repe_param_type _ -> true
        | _ -> false
      in
      let ptype = Some (param_type env v5) in
      let pdefault =
        match v6 with
        | Some (_eq, e) -> Some (expression env e)
        | None -> None
      in
      let p = { (G.param_of_id id ?ptype) with pdefault; pattrs } in
      if is_repeated then ParamRest (Tok.unsafe_fake_tok "*", p) else Param p
  | `Semg_ellips tok -> ParamEllipsis (token env tok)

and class_definition_ (env : env) (attrs : attribute list)
    (ckind : class_kind * Tok.t) (x : CST.class_definition_) : stmt =
  let ctor, ext_opt, derives_opt, body_opt = x in
  let cname, tparams, anno_opt, access_opt, cparams_list = ctor in
  let id = type_identifier env cname in
  let ctor_annot =
    match anno_opt with
    | Some (at_tok, ty, args_opt) ->
        let at = token env at_tok in
        let name = simple_type_to_name env ty in
        let args =
          match args_opt with
          | Some a -> arguments env a
          | None -> fb []
        in
        [ NamedAttr (at, name, args) ]
    | None -> []
  in
  let ctor_access =
    match access_opt with
    | Some am -> [ access_modifier env am ]
    | None -> []
  in
  let attrs = attrs @ ctor_annot @ ctor_access in
  let tparams = Option.map (type_parameters env) tparams in
  let cparams =
    match cparams_list with
    | [] -> fb []
    | _ ->
        let all =
          List.concat_map
            (fun (_auto_semi_opt, cp) ->
              let _, ps, _ = class_parameters env cp in
              ps)
            cparams_list
        in
        fb all
  in
  let cextends, cmixins =
    match ext_opt with
    | Some x -> extends_clause env x
    | None -> ([], [])
  in
  let cimplements =
    match derives_opt with
    | Some (_derives_tok, first, rest) ->
        let names =
          anon_choice_type_id_ae98204_to_name env first
          :: List.map
               (fun (_comma, n) -> anon_choice_type_id_ae98204_to_name env n)
               rest
        in
        List.map (fun n -> TyN n |> G.t) names
    | None -> []
  in
  let cbody =
    match body_opt with
    | Some x -> definition_body env x
    | None -> fb []
  in
  let ent = G.basic_entity id ~attrs ?tparams in
  let cdef = { ckind; cextends; cimplements; cmixins; cparams; cbody } in
  DefStmt (ent, ClassDef cdef) |> G.s

and object_definition_ (env : env) (attrs : attribute list)
    ((v1, v2, _v3, v4) : CST.object_definition_) : stmt =
  let id = type_identifier env v1 in
  let cextends, cmixins =
    match v2 with
    | Some x -> extends_clause env x
    | None -> ([], [])
  in
  let cbody =
    match v4 with
    | Some x -> definition_body env x
    | None -> fb []
  in
  let ent = G.basic_entity id ~attrs in
  let cdef =
    {
      ckind = (Object, Tok.unsafe_fake_tok "object");
      cextends;
      cimplements = [];
      cmixins;
      cparams = fb [];
      cbody;
    }
  in
  DefStmt (ent, ClassDef cdef) |> G.s

and class_parameters (env : env)
    ((_auto_semi, _l, impl_or_using_opt, params_opt, _r) : CST.class_parameters)
    : parameters =
  let extra_attr =
    match impl_or_using_opt with
    | Some (`Impl tok) -> [ G.OtherAttribute (("implicit", token env tok), []) ]
    | Some (`Using tok) -> [ G.OtherAttribute (("using", token env tok), []) ]
    | None -> []
  in
  let add_extra_attr = function
    | Param p -> Param { p with pattrs = extra_attr @ p.pattrs }
    | other -> other
  in
  match params_opt with
  | None -> fb []
  | Some (first, rest, _trailing) ->
      let first = class_parameter env first in
      let rest = List.map (fun (_comma, cp) -> class_parameter env cp) rest in
      fb (List.map add_extra_attr (first :: rest))

and class_parameter (env : env) (x : CST.class_parameter) : parameter =
  match x with
  | `Rep_anno_opt_modifs_opt_choice_val_choice_id_opt_COLON_choice_type_opt_EQ_exp
      (annos, mods, val_var_opt, v4, v5, v6) ->
      let anno_attrs = annotations env annos in
      let mod_attrs = modifiers_opt env mods in
      let val_var_attrs =
        match val_var_opt with
        | Some (`Val tok) -> [ KeywordAttr (Const, token env tok) ]
        | Some (`Var tok) -> [ KeywordAttr (Mutable, token env tok) ]
        | None -> []
      in
      let pattrs = anno_attrs @ mod_attrs @ val_var_attrs in
      let id = type_identifier env v4 in
      let ptype =
        match v5 with
        | Some (_colon, pt) -> Some (param_type env pt)
        | None -> None
      in
      let pdefault =
        match v6 with
        | Some (_eq, e) -> Some (expression env e)
        | None -> None
      in
      Param { (G.param_of_id id ?ptype) with pdefault; pattrs }
  | `Semg_ellips tok -> ParamEllipsis (token env tok)

and extends_clause (env : env) ((_extends_tok, v2, v3) : CST.extends_clause) :
    class_parent list * type_ list =
  let cextends, cmixins = constructor_applications env v2 in
  let _extra_args = Option.map (arguments env) v3 in
  (cextends, cmixins)

and constructor_applications (env : env) (x : CST.constructor_applications) :
    class_parent list * type_ list =
  match x with
  | `Cons_app_rep_COMMA_cons_app (first, rest) ->
      let first = constructor_application_to_parent env first in
      let rest =
        List.map
          (fun (_comma, ca) -> constructor_application_to_parent env ca)
          rest
      in
      (first :: rest, [])
  | `Cons_app_rep_with_cons_app (first, rest) ->
      let first = constructor_application_to_parent env first in
      let rest =
        List.map
          (fun (_with, ca) ->
            let ty, _args = constructor_application_to_type_and_args env ca in
            ty)
          rest
      in
      ([ first ], rest)

and constructor_application_to_parent (env : env)
    (x : CST.constructor_application) : class_parent =
  let ty, args = constructor_application_to_type_and_args env x in
  let _l, arg_list, _r = args in
  match arg_list with
  | [] -> (ty, None)
  | _ -> (ty, Some (fb arg_list))

and definition_body (env : env) ((_auto_semi, v2) : CST.definition_body) :
    field list bracket =
  template_body_to_class_body env v2

and template_body_to_class_body (env : env) (x : CST.template_body) :
    field list bracket =
  match x with
  | `Inde_temp_body (_colon, _indent, _self_type_opt, blk, _outdent) ->
      let stmts = block env blk in
      let fields = List.map (fun s -> F s) stmts in
      fb fields
  | `Braced_temp_body (v1, v2, v3) ->
      let l = token env v1 in
      let fields =
        match v2 with
        | Some x -> (
            match x with
            | `Braced_temp_body1 (_self_type_opt, blk) ->
                let stmts = block env blk in
                List.map (fun s -> F s) stmts
            | `Braced_temp_body2 (_indent_and_self, blk_opt, _outdent) -> (
                match blk_opt with
                | Some blk ->
                    let stmts = block env blk in
                    List.map (fun s -> F s) stmts
                | None -> []))
        | None -> []
      in
      let r = token env v3 in
      (l, fields, r)

and template_body_to_stmts (env : env) (x : CST.template_body) : stmt list =
  match x with
  | `Inde_temp_body (_colon, _indent, _self_type_opt, blk, _outdent) ->
      block env blk
  | `Braced_temp_body (_v1, v2, _v3) -> (
      match v2 with
      | Some x -> (
          match x with
          | `Braced_temp_body1 (_self_type_opt, blk) -> block env blk
          | `Braced_temp_body2 (_indent_and_self, blk_opt, _outdent) -> (
              match blk_opt with
              | Some blk -> block env blk
              | None -> []))
      | None -> [])

and enum_body (env : env) (x : CST.enum_body) : field list bracket =
  match x with
  | `COLON_indent_enum_blk_outd (_colon, _indent, blk, _outdent) ->
      let stmts = enum_block env blk in
      fb (List.map (fun s -> F s) stmts)
  | `LCURL_opt_enum_blk_RCURL (v1, v2, v3) ->
      let l = token env v1 in
      let stmts =
        match v2 with
        | Some blk -> enum_block env blk
        | None -> []
      in
      let r = token env v3 in
      (l, List.map (fun s -> F s) stmts, r)

and enum_block (env : env) ((v1, v2, _v3) : CST.enum_block) : stmt list =
  let first = enum_case_or_def env v1 in
  let rest =
    List.concat_map (fun (_semi, item) -> enum_case_or_def env item) v2
  in
  first @ rest

and enum_case_or_def (env : env) (x : CST.anon_choice_enum_case_defins_b7955e9)
    : stmt list =
  match x with
  | `Enum_case_defins (_annos, _case_tok, v3) -> (
      match v3 with
      | `Simple_enum_case_rep_COMMA_simple_enum_case (first, rest) ->
          let first = simple_enum_case env first in
          let rest =
            List.map (fun (_comma, sec) -> simple_enum_case env sec) rest
          in
          first :: rest
      | `Full_enum_case x -> [ full_enum_case env x ])
  | `Exp x ->
      let e = expression env x in
      [ ExprStmt (e, G.sc) |> G.s ]
  | `Choice_choice_given_defi x -> definition env x

and simple_enum_case (env : env) ((v1, _extends_opt) : CST.simple_enum_case) :
    stmt =
  let id = type_identifier env v1 in
  let ent = G.basic_entity id in
  let enum_entry = { ee_args = None; ee_body = None } in
  DefStmt (ent, EnumEntryDef enum_entry) |> G.s

and full_enum_case (env : env) ((v1, v2) : CST.full_enum_case) : stmt =
  let id = type_identifier env v1 in
  let tparams_opt, cparams_list, _extends_opt = v2 in
  let tparams = Option.map (type_parameters env) tparams_opt in
  let params =
    List.concat_map
      (fun cp ->
        let _l, ps, _r = class_parameters env cp in
        ps)
      cparams_list
  in
  let fake_tok = Tok.unsafe_fake_tok "Param" in
  let ee_args =
    match
      List.map
        (fun param -> G.OtherArg (("Param", fake_tok), [ G.Pa param ]))
        params
    with
    | [] -> None
    | args -> Some (fb args)
  in
  let ent = G.basic_entity id ?tparams in
  let enum_entry = { ee_args; ee_body = None } in
  DefStmt (ent, EnumEntryDef enum_entry) |> G.s

and extension_template_body (env : env) (x : CST.extension_template_body) :
    stmt list =
  match x with
  | `Indent_blk_outd (_indent, blk, _outdent) -> block env blk
  | `LCURL_opt_blk_RCURL (_l, blk_opt, _r) -> (
      match blk_opt with
      | Some blk -> block env blk
      | None -> [])

and namespace_expression_to_stmts (env : env) (import_tok : Tok.t)
    ((v1, v2, v3) : CST.namespace_expression) : stmt list =
  let first_id = type_identifier env v1 in
  let rest_ids = List.map (fun (_dot, tid) -> type_identifier env tid) v2 in
  let prefix = first_id :: rest_ids in
  match v3 with
  | None ->
      (* import a.b.c -> ImportFrom(a.b, c) *)
      let ident, module_name =
        match List.rev prefix with
        | [] -> failwith "impossible: empty import"
        | x :: xs -> (x, List.rev xs)
      in
      let dir =
        ImportFrom
          ( import_tok,
            DottedName module_name,
            [ H2.mk_import_from_kind ident None ] )
      in
      [ DirectiveStmt (dir |> G.d) |> G.s ]
  | Some (_dot, selector) -> (
      match selector with
      | `Name_wild x ->
          let _wild =
            match x with
            | `STAR tok -> token env tok
            | `X__ tok -> token env tok
            | `Given tok -> token env tok
          in
          let dir =
            ImportAll (import_tok, DottedName prefix, Tok.unsafe_fake_tok "*")
          in
          [ DirectiveStmt (dir |> G.d) |> G.s ]
      | `Name_selecs x -> namespace_selector_stmts env import_tok prefix x
      | `As_rena_id (tid, _as_tok, new_name) ->
          let id = type_identifier env tid in
          let alias =
            match new_name with
            | `Choice_id x -> Some (type_identifier env x)
            | `Wild _tok -> None
          in
          let dir =
            ImportFrom
              ( import_tok,
                DottedName prefix,
                [ H2.mk_import_from_kind id alias ] )
          in
          [ DirectiveStmt (dir |> G.d) |> G.s ])

and namespace_selector_stmts (env : env) (import_tok : Tok.t)
    (prefix : dotted_ident)
    ((_l, v2, v3, _trailing, _r) : CST.namespace_selectors) : stmt list =
  let all_selectors = v2 :: List.map snd v3 in
  List.map (namespace_selector_to_stmt env import_tok prefix) all_selectors

and namespace_selector_to_stmt (env : env) (import_tok : Tok.t)
    (prefix : dotted_ident) (x : CST.anon_choice_name_given_by_type_ca66fd5) :
    stmt =
  match x with
  | `Name_given_by_type (_given_tok, ty) ->
      (* import pkg.{given SomeType} -> OtherDirective, matching pfff parser *)
      let dir =
        OtherDirective
          (("ImportGiven", Tok.unsafe_fake_tok "given"), [ G.T (type_ env ty) ])
      in
      DirectiveStmt (dir |> G.d) |> G.s
  | `Name_wild _x ->
      (* import pkg.{*, ...} or import pkg.{_, ...} -> ImportAll *)
      let dir =
        ImportAll (import_tok, DottedName prefix, Tok.unsafe_fake_tok "*")
      in
      DirectiveStmt (dir |> G.d) |> G.s
  | `Choice_id x ->
      let id = type_identifier env x in
      let dir =
        ImportFrom
          (import_tok, DottedName prefix, [ H2.mk_import_from_kind id None ])
      in
      DirectiveStmt (dir |> G.d) |> G.s
  | `Arrow_rena_id (tid, _arrow, new_name) ->
      let id = type_identifier env tid in
      let alias =
        match new_name with
        | `Choice_id x -> Some (type_identifier env x)
        | `Wild _tok -> None
      in
      let dir =
        ImportFrom
          (import_tok, DottedName prefix, [ H2.mk_import_from_kind id alias ])
      in
      DirectiveStmt (dir |> G.d) |> G.s
  | `As_rena_id (tid, _as_tok, new_name) ->
      let id = type_identifier env tid in
      let alias =
        match new_name with
        | `Choice_id x -> Some (type_identifier env x)
        | `Wild _tok -> None
      in
      let dir =
        ImportFrom
          (import_tok, DottedName prefix, [ H2.mk_import_from_kind id alias ])
      in
      DirectiveStmt (dir |> G.d) |> G.s

(* ---------------------------------------------------------------------- *)
(* Helpers *)
(* ---------------------------------------------------------------------- *)

and stmt_to_expr (s : stmt) : expr = StmtExpr s |> G.e

(* ---------------------------------------------------------------------- *)
(* Top-level *)
(* ---------------------------------------------------------------------- *)

and top_level_definition (env : env) (x : CST.top_level_definition) : stmt list
    =
  match x with
  | `Choice_choice_choice_given_defi inner -> (
      match inner with
      | `Choice_choice_given_defi x -> definition env x
      | `End_marker _x -> []
      | `Exp x ->
          let e = expression env x in
          [ ExprStmt (e, G.sc) |> G.s ])
  | `Semg_exp (_tok, x) ->
      let e = expression env x in
      [ ExprStmt (e, G.sc) |> G.s ]
  | `Semg_stmt (_tok, x) -> (
      match x with
      | `Exp e -> [ ExprStmt (expression env e, G.sc) |> G.s ]
      | `Choice_choice_given_defi d -> definition env d)
  | `Semg_member_decl (_tok, x) -> (
      match x with
      | `Func_defi x -> [ function_definition env x ]
      | `Func_decl x -> [ function_declaration env x ]
      | `Val_defi ((v1_annos, v1_mods, _val_tok), v2, v3, _eq, v5) ->
          let attrs = annotations env v1_annos @ modifiers_opt env v1_mods in
          val_or_var_def env attrs false v2 v3 v5
      | `Val_decl ((v1_annos, v1_mods, _val_tok), v2, v3, _colon, v5) ->
          let attrs = annotations env v1_annos @ modifiers_opt env v1_mods in
          val_or_var_decl env attrs false v2 v3 v5
      | `Var_defi ((v1_annos, v1_mods, _var_tok), v2, v3, _eq, v5) ->
          let attrs = annotations env v1_annos @ modifiers_opt env v1_mods in
          val_or_var_def env attrs true v2 v3 v5
      | `Var_decl ((v1_annos, v1_mods, _var_tok), v2, v3, _colon, v5) ->
          let attrs = annotations env v1_annos @ modifiers_opt env v1_mods in
          val_or_var_decl env attrs true v2 v3 v5)

and compilation_unit (env : env) ((_shebang, v2) : CST.compilation_unit) : any =
  match v2 with
  | None -> Pr []
  | Some (first, rest, _semi_opt) ->
      let first_stmts = top_level_definition env first in
      let rest_stmts =
        List.concat_map (fun (_semi, tld) -> top_level_definition env tld) rest
      in
      Pr (Scala_to_generic.merge_chained_packages (first_stmts @ rest_stmts))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_scala.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = Program } in
      match compilation_unit env cst with
      | G.Pr xs -> xs
      | _ -> failwith "not a program")

let parse_pattern str =
  H.wrap_parser
    (fun () -> Tree_sitter_scala.Parse.string str)
    (fun cst _extras ->
      let file = Fpath.v "<pattern>" in
      let env =
        { H.file; conv = H.line_col_to_pos_pattern str; extra = Pattern }
      in
      compilation_unit env cst)
