(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
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
open Either
open Fpath_.Operators
module AST = Ast_java
module CST = Tree_sitter_java.CST
open Ast_java
module G = AST_generic
module H = Parse_tree_sitter_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Java parser using tree-sitter-lang/semgrep-java and converting
 * to ../ast//ast_java.ml
 *
 * The resulting AST can then be converted to the generic AST by using
 * ../generic/java_to_generic.ml
 *
 * TODO:
 *  - look at all the _xxx var in this file; Many of them should really
 *    be in the AST (annotations, certain punctuation tokens, etc.)
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type env = unit H.env

let fake = Tok.fake_tok
let token = H.token
let str = H.str
let option = Option.map

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-java/Boilerplate.ml *)

(**
   Boilerplate to be used as a template when mapping the java CST
   to another type of tree.
*)

let identifier (env : env) (tok : CST.identifier) = str env tok

(* pattern [a-zA-Z_]\w* *)

let floating_point_type (env : env) (x : CST.floating_point_type) =
  match x with
  | `Float tok -> TBasic ("float", token env tok) (* "float" *)
  | `Double tok -> TBasic ("double", token env tok)

(* "double" *)

let integral_type (env : env) (x : CST.integral_type) =
  match x with
  | `Byte tok -> TBasic (str env tok) (* "byte" *)
  | `Short tok -> TBasic (str env tok) (* "short" *)
  | `Int tok -> TBasic (str env tok) (* "int" *)
  | `Long tok -> TBasic (str env tok) (* "long" *)
  | `Char tok -> TBasic (str env tok)

(* "char" *)

let requires_modifier (env : env) (x : CST.requires_modifier) =
  match x with
  | `Tran tok -> token env tok (* "transitive" *)
  | `Static tok -> token env tok

(* "static" *)

let choice_open env f = function
  | `Open tok -> f env tok (* "open" *)
  | `Record tok -> f env tok (* "record" *)
  | `Module tok -> f env tok (* "module" *)

let id_extra env = function
  | `Id tok -> str env tok (* pattern [a-zA-Z_]\w* *)
  | `Choice_open x -> choice_open env str x

let rec qualifier_extra env = function
  | `Id tok -> [ str env tok ] (* pattern [a-zA-Z_]\w* *)
  | `Choice_open x -> [ choice_open env str x ]
  | `Scoped_id x -> scoped_identifier env x

and scoped_identifier (env : env) ((v1, v2, v3) : CST.scoped_identifier) :
    qualified_ident =
  let v1 = qualifier_extra env v1 in
  let _v2 = token env v2 (* "." *) in
  let v3 = str env v3 (* pattern [a-zA-Z_]\w* *) in
  v1 @ [ v3 ]

let inferred_parameters (env : env) ((v1, v2, v3, v4) : CST.inferred_parameters)
    =
  let v1 = token env v1 (* "(" *) in
  let v2 = id_extra env v2 (* pattern [a-zA-Z_]\w* *) in
  let v3 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = id_extra env v2 (* pattern [a-zA-Z_]\w* *) in
        v2)
      v3
  in
  let v4 = token env v4 (* ")" *) in
  (v1, v2 :: v3, v4)

let int_literal env tok =
  let s, t = str env tok in
  Parsed_int.parse_c_octal (s, t)

let multiline_string_fragment (env : env) (x : CST.multiline_string_fragment) =
  match x with
  | `Pat_98d585a x -> str env x
  | `Pat_9347a80_rep_pat_98d585a (v1, v2) ->
      (* pattern "\"[^\"]*" *)
      let s, t = str env v1 in
      let strs, toks =
        List_.map
          (fun x ->
            (* pattern "[^\"]+" *)
            str env x)
          v2
        |> Common2.unzip
      in
      (s ^ String.concat "" strs, Tok.combine_toks t toks)

let escape_sequence_ (env : env) (x : CST.escape_sequence_) =
  match x with
  | `Imm_tok_bslash_pat_36cdeeb x -> str env x
  | `Esc_seq tok -> (* escape_sequence *) str env tok

let string_literal env s =
  match s with
  | `Str_lit_ (quote1, contents, quote2) ->
      let tok1 = token env quote1 in
      let strs, toks =
        List_.map
          (function
            | `Str_frag x -> str env x
            | `Esc_seq x -> str env x)
          contents
        |> Common2.unzip
      in
      let tok2 = token env quote2 in
      (String.concat "" strs, Tok.combine_toks tok1 (toks @ [ tok2 ]))
  | `Mult_str_lit (quote1, contents, quote2) ->
      let tok1 = token env quote1 in
      let strs, toks =
        List_.map
          (function
            | `Mult_str_frag x -> multiline_string_fragment env x
            | `Esc_seq_ x -> escape_sequence_ env x)
          contents
        |> Common2.unzip
      in
      let tok2 = token env quote2 in
      (String.concat "" strs, Tok.combine_toks tok1 (toks @ [ tok2 ]))

let float_literal env tok =
  let s, t = str env tok in
  (float_of_string_opt s, t)

let literal (env : env) (x : CST.literal) =
  match x with
  | `Deci_int_lit tok -> Int (int_literal env tok) (* decimal_integer_literal *)
  | `Hex_int_lit tok -> Int (int_literal env tok) (* hex_integer_literal *)
  | `Octal_int_lit tok -> Int (int_literal env tok) (* octal_integer_literal *)
  | `Bin_int_lit tok -> Int (int_literal env tok) (* binary_integer_literal *)
  | `Deci_floa_point_lit tok ->
      Float (float_literal env tok) (* decimal_floating_point_literal *)
  | `Hex_floa_point_lit tok ->
      Float (float_literal env tok) (* hex_floating_point_literal *)
  | `True tok -> Bool (true, token env tok) (* "true" *)
  | `False tok -> Bool (false, token env tok) (* "false" *)
  | `Char_lit tok -> Char (str env tok) (* character_literal *)
  | `Str_lit s -> String (string_literal env s) (* string_literal *)
  | `Null_lit tok -> Null (token env tok)
(* "null" *)

(* TODO: add in AST *)
let module_directive (env : env) (v : CST.module_directive) : unit =
  match v with
  | `Requis_module_dire (v1, v2, v3, v4) ->
      let _v1 = token env v1 (* "requires" *) in
      let _v2 = List_.map (requires_modifier env) v2 in
      let _v3 = qualifier_extra env v3 in
      let _v4 = token env v4 (* ";" *) in
      ()
  | `Exports_module_dire (v1, v2, v3, v4) ->
      let _v1 = token env v1 (* "exports" *) in
      let _v2 = qualifier_extra env v2 in
      let _v3 =
        match v3 with
        | Some _TODO -> Some () (* "to" *)
        | None -> None
      in
      let _v4 = token env v4 (* ";" *) in
      ()
  | `Opens_module_dire (v1, v2, v3, v4) ->
      let _v1 = token env v1 (* "opens" *) in
      let _v2 = qualifier_extra env v2 in
      let _v3 =
        match v3 with
        | Some _ -> Some () (* "to" *)
        | None -> None
      in
      let _v4 = token env v4 (* ";" *) in
      ()
  | `Uses_module_dire (v1, v2, v3) ->
      let _v1 = token env v1 (* "uses" *) in
      let _v2 = qualifier_extra env v2 in
      let _v3 = token env v3 (* ";" *) in
      ()
  | `Provis_module_dire (v1, v2, v3, v4, v5, v6) ->
      let _v1 = token env v1 (* "provides" *) in
      let _v2 = qualifier_extra env v2 in
      let _v3 = token env v3 (* "with" *) in
      let _v4 = qualifier_extra env v4 in
      let _v5 =
        List_.map
          (fun (v1, v2) ->
            let _v1 = token env v1 (* "," *) in
            let v2 = qualifier_extra env v2 in
            v2)
          v5
      in
      let _v6 = token env v6 (* ";" *) in
      ()

(* TODO: add in AST *)
let module_body (env : env) ((v1, v2, v3) : CST.module_body) : unit =
  let _v1 = token env v1 (* "{" *) in
  let _v2 = List_.map (module_directive env) v2 in
  let _v3 = token env v3 (* "}" *) in
  ()

let rec expression (env : env) (x : CST.expression) =
  match x with
  | `Assign_exp (v1, v2, v3) -> (
      let v1 =
        match v1 with
        | `Id tok -> name_of_id env tok (* pattern [a-zA-Z_]\w* *)
        | `Choice_open x -> choice_open env name_of_id x
        | `Field_access x -> field_access env x
        | `Array_access x -> array_access env x
      in
      let v2 =
        match v2 with
        | `EQ tok -> (Left (), token env tok) (* "=" *)
        | `PLUSEQ tok -> (Right G.Plus, token env tok) (* "+=" *)
        | `DASHEQ tok -> (Right G.Minus, token env tok) (* "-=" *)
        | `STAREQ tok -> (Right G.Mult, token env tok) (* "*=" *)
        | `SLASHEQ tok -> (Right G.Div, token env tok) (* "/=" *)
        | `AMPEQ tok -> (Right G.BitAnd, token env tok) (* "&=" *)
        | `BAREQ tok -> (Right G.BitOr, token env tok) (* "|=" *)
        | `HATEQ tok -> (Right G.BitXor, token env tok) (* "^=" *)
        | `PERCEQ tok -> (Right G.Mod, token env tok) (* "%=" *)
        | `LTLTEQ tok -> (Right G.LSL, token env tok) (* "<<=" *)
        | `GTGTEQ tok -> (Right G.LSR, token env tok) (* ">>=" *)
        | `GTGTGTEQ tok -> (Right G.ASR, token env tok)
        (* ">>>=" *)
      in
      let v3 = expression env v3 in
      match v2 with
      | Left (), t -> Assign (v1, t, v3)
      | Right op, t -> AssignOp (v1, (op, t), v3))
  | `Bin_exp x -> binary_expression env x
  | `Inst_exp (v1, v2, v3, v4, _v5) ->
      let v1 = expression env v1 in
      let _v2 = token env v2 (* "instanceof" *) in
      (* TODO: use attrs *)
      let _attrs =
        match v3 with
        | Some _ -> Some "final"
        | None -> None
      in
      let v4 = type_ env v4 in
      (* TODO make identifier available to the AST*)
      InstanceOf (v1, v4)
  | `Lambda_exp (v1, v2, v3) ->
      let v1 =
        match v1 with
        | `Id tok ->
            let id = str env tok (* pattern [a-zA-Z_]\w* *) in
            [ ParamClassic (AST.entity_of_id id) ]
        | `Formal_params x -> formal_parameters env x
        | `Choice_open x ->
            let id = choice_open env str x in
            [ ParamClassic (AST.entity_of_id id) ]
        | `Infe_params x ->
            let _, xs, _ = inferred_parameters env x in
            xs |> List_.map (fun id -> ParamClassic (AST.entity_of_id id))
      in
      let v2 = token env v2 (* "->" *) in
      let v3 =
        match v3 with
        | `Exp x ->
            let x = expression env x in
            Expr (x, fake v2 ";")
        | `Blk x -> block env x
      in
      Lambda (v1, v2, v3)
  | `Tern_exp (v1, v2, v3, v4, v5) ->
      let v1 = expression env v1 in
      let _v2 = token env v2 (* "?" *) in
      let v3 = expression env v3 in
      let _v4 = token env v4 (* ":" *) in
      let v5 = expression env v5 in
      Conditional (v1, v3, v5)
  | `Update_exp x -> update_expression env x
  | `Prim_exp x -> primary_expression env x
  | `Un_exp x -> unary_expression env x
  | `Cast_exp (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = type_ env v2 in
      let v3 =
        List_.map
          (fun (v1, v2) ->
            let _v1 = token env v1 (* "&" *) in
            let v2 = type_ env v2 in
            v2)
          v3
      in
      let v4 = token env v4 (* ")" *) in
      let v5 = expression env v5 in
      Cast ((v1, v2 :: v3, v4), v5)
  | `Switch_exp (v1, v2, v3) ->
      let v1 = token env v1 (* "switch" *) in
      let v2 = parenthesized_expression env v2 in
      let v3 = switch_block env v3 in
      SwitchE (v1, v2, v3)

and binary_expression (env : env) (x : CST.binary_expression) =
  match x with
  | `Exp_GT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* ">" *) in
      let v3 = expression env v3 in
      Infix (v1, (G.Gt, v2), v3)
  | `Exp_LT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "<" *) in
      let v3 = expression env v3 in
      Infix (v1, (G.Lt, v2), v3)
  | `Exp_EQEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "==" *) in
      let v3 = expression env v3 in
      Infix (v1, (G.Eq, v2), v3)
  | `Exp_GTEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* ">=" *) in
      let v3 = expression env v3 in
      Infix (v1, (G.GtE, v2), v3)
  | `Exp_LTEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "<=" *) in
      let v3 = expression env v3 in
      Infix (v1, (G.LtE, v2), v3)
  | `Exp_BANGEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "!=" *) in
      let v3 = expression env v3 in
      Infix (v1, (G.NotEq, v2), v3)
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "&&" *) in
      let v3 = expression env v3 in
      Infix (v1, (G.And, v2), v3)
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "||" *) in
      let v3 = expression env v3 in
      Infix (v1, (G.Or, v2), v3)
  | `Exp_PLUS_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "+" *) in
      let v3 = expression env v3 in
      Infix (v1, (G.Plus, v2), v3)
  | `Exp_DASH_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "-" *) in
      let v3 = expression env v3 in
      Infix (v1, (G.Minus, v2), v3)
  | `Exp_STAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "*" *) in
      let v3 = expression env v3 in
      Infix (v1, (G.Mult, v2), v3)
  | `Exp_SLASH_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "/" *) in
      let v3 = expression env v3 in
      Infix (v1, (G.Div, v2), v3)
  | `Exp_AMP_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "&" *) in
      let v3 = expression env v3 in
      Infix (v1, (G.BitAnd, v2), v3)
  | `Exp_BAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "|" *) in
      let v3 = expression env v3 in
      Infix (v1, (G.BitOr, v2), v3)
  | `Exp_HAT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "^" *) in
      let v3 = expression env v3 in
      Infix (v1, (G.BitXor, v2), v3)
  | `Exp_PERC_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "%" *) in
      let v3 = expression env v3 in
      Infix (v1, (G.Mod, v2), v3)
  | `Exp_LTLT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "<<" *) in
      let v3 = expression env v3 in
      Infix (v1, (G.LSL, v2), v3)
  | `Exp_GTGT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* ">>" *) in
      let v3 = expression env v3 in
      Infix (v1, (G.LSR, v2), v3)
  | `Exp_GTGTGT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* ">>>" *) in
      let v3 = expression env v3 in
      Infix (v1, (G.ASR, v2), v3)

and unary_expression (env : env) (x : CST.unary_expression) =
  match x with
  | `PLUS_exp (v1, v2) ->
      let v1 = token env v1 (* "+" *) in
      let v2 = expression env v2 in
      Unary ((G.Plus, v1), v2)
  | `DASH_exp (v1, v2) ->
      let v1 = token env v1 (* "-" *) in
      let v2 = expression env v2 in
      Unary ((G.Minus, v1), v2)
  | `BANG_exp (v1, v2) ->
      let v1 = token env v1 (* "!" *) in
      let v2 = expression env v2 in
      Unary ((G.Not, v1), v2)
  | `TILDE_exp (v1, v2) ->
      let v1 = token env v1 (* "~" *) in
      let v2 = expression env v2 in
      Unary ((G.BitNot, v1), v2)

and update_expression (env : env) (x : CST.update_expression) =
  match x with
  | `Exp_PLUSPLUS (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "++" *) in
      Postfix (v1, (G.Incr, v2))
  | `Exp_DASHDASH (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "--" *) in
      Postfix (v1, (G.Decr, v2))
  | `PLUSPLUS_exp (v1, v2) ->
      let v1 = token env v1 (* "++" *) in
      let v2 = expression env v2 in
      Prefix ((G.Incr, v1), v2)
  | `DASHDASH_exp (v1, v2) ->
      let v1 = token env v1 (* "--" *) in
      let v2 = expression env v2 in
      Prefix ((G.Decr, v1), v2)

and basic_type_extra env = function
  | `Void_type tok -> TBasic (str env tok) (* "void" *)
  | `Inte_type x -> integral_type env x
  | `Floa_point_type x -> floating_point_type env x
  | `Bool_type tok -> TBasic (str env tok) (* "boolean" *)
  | `Id tok -> (
      let x = str env tok (* pattern [a-zA-Z_]\w* *) in
      match x with
      (* since Java 10 *)
      | "var", tk -> TVar tk
      | x -> TClass [ (x, None) ])
  | `Scoped_type_id x ->
      let x = scoped_type_identifier env x in
      TClass x
  | `Gene_type x ->
      let x = generic_type env x in
      TClass x

and name_of_id env tok =
  (*Name ([[], str env tok]) *)
  NameId (str env tok)

(* TODO: use a special at some point *)
and super env tok = name_of_id env tok
and super_id_field env tok = str env tok
and new_id env tok = str env tok

and primary_expression (env : env) (x : CST.primary_expression) =
  match x with
  | `Semg_ellips tok ->
      let tok = (* "..." *) token env tok in
      Ellipsis tok
  | `Semg_named_ellips tok -> name_of_id env tok
  | `Choice_lit x -> (
      match x with
      | `Lit x -> Literal (literal env x)
      | `Class_lit (v1, v2, v3) ->
          let v1 = unannotated_type env v1 in
          let _v2 = token env v2 (* "." *) in
          let v3 = token env v3 (* "class" *) in
          ClassLiteral (v1, v3)
      | `This tok -> This (token env tok) (* "this" *)
      | `Id tok -> name_of_id env tok (* pattern [a-zA-Z_]\w* *)
      | `Choice_open x -> choice_open env name_of_id x
      | `Paren_exp x -> parenthesized_expression env x
      | `Obj_crea_exp x -> object_creation_expression env x
      | `Field_access x -> (
          let e = field_access env x in
          (* TODO: this should not be needed if the issue below was fixed:
           * https://github.com/tree-sitter/tree-sitter-java/issues/121
           *)
          match e with
          | Dot (NameId id, _tdot, ("class", tclass)) ->
              ClassLiteral (TClass [ (id, None) ], tclass)
          | _ -> e)
      | `Array_access x -> array_access env x
      | `Meth_invo (v1, v2) ->
          let v1 =
            match v1 with
            | `Choice_id x ->
                let id = id_extra env x in
                NameId id
            | `Choice_prim_exp_DOT_opt_super_DOT_opt_type_args_choice_id
                (v1, v2, v3, v4, v5) ->
                let v1 =
                  match v1 with
                  | `Prim_exp x -> primary_expression env x
                  | `Super tok -> super env tok
                  (* "super" *)
                in
                let v2 = token env v2 (* "." *) in
                let v3 =
                  match v3 with
                  | Some (v1bis, v2bis) ->
                      let v1bis = super_id_field env v1bis (* "super" *) in
                      let v2bis = token env v2bis (* "." *) in
                      fun v5 -> Dot (Dot (v1, v2, v1bis), v2bis, v5)
                  | None -> fun v5 -> Dot (v1, v2, v5)
                in
                let _v4TODO = option (type_arguments env) v4 in
                let v5 = id_extra env v5 in
                v3 v5
          in
          let v2 = argument_list env v2 in
          Call (v1, v2)
      | `Meth_ref (v1, v2, v3, v4) ->
          let v1 =
            match v1 with
            | `Type x ->
                let t = type_ env x in
                Right t
            | `Prim_exp x -> Left (primary_expression env x)
            | `Super tok -> Left (super env tok)
            (* "super" *)
          in
          let v2 = token env v2 (* "::" *) in
          let v3 = option (type_arguments env) v3 in
          let v4 =
            match v4 with
            | `New tok -> new_id env tok (* "new" *)
            | `Id tok -> str env tok
            (* pattern [a-zA-Z_]\w* *)
          in
          MethodRef (v1, v2, v3, v4)
      | `Array_crea_exp (v1, _annotation, v3, v4) ->
          (*TODO add support for annotations *)
          let v1 = token env v1 (* "new" *) in
          let v3 = basic_type_extra env v3 in
          let exprs, dims, init =
            match v4 with
            | `Rep1_dimens_expr_opt_dimens (v1, v3) ->
                let v1 = List_.map (dimensions_expr env) v1 in
                let v3 =
                  match v3 with
                  | Some x -> dimensions env x
                  | None -> []
                in
                (v1, v3, None)
            | `Dimens_array_init (v1, v3) ->
                let v1 = dimensions env v1 in
                let v3 = array_initializer env v3 in
                ([], v1, Some (ArrayInit v3))
          in
          NewArray (v1, v3, exprs, List.length dims, init))

and dimensions_expr (env : env) ((v1, v2, v3, v4) : CST.dimensions_expr) =
  let _v1 = List_.map (annotation env) v1 in
  let _v2 = token env v2 (* "[" *) in
  let v3 = expression env v3 in
  let _v4 = token env v4 (* "]" *) in
  v3

and dimensions (env : env) (xs : CST.dimensions) =
  List_.map
    (fun (v1, v2, v3) ->
      let _v1 = List_.map (annotation env) v1 in
      let v2 = token env v2 (* "[" *) in
      let v3 = token env v3 (* "]" *) in
      (v2, (), v3))
    xs

and parenthesized_expression (env : env)
    ((v1, v2, v3) : CST.parenthesized_expression) =
  let _v1 = token env v1 (* "(" *) in
  let v2 = expression env v2 in
  let _v3 = token env v3 (* ")" *) in
  v2

and object_creation_expression (env : env) (x : CST.object_creation_expression)
    =
  match x with
  | `Unqu_obj_crea_exp x ->
      let tnew, _targsTODO, typ, args, body_opt =
        unqualified_object_creation_expression env x
      in
      NewClass (tnew, typ, args, body_opt)
  | `Prim_exp_DOT_unqu_obj_crea_exp (v1, v2, v3) ->
      let v1 = primary_expression env v1 in
      let v2 = token env v2 (* "." *) in
      let v3 = unqualified_object_creation_expression env v3 in
      let tnew, _targsTODO, typ, args, body_opt = v3 in
      NewQualifiedClass (v1, v2, tnew, typ, args, body_opt)

and unqualified_object_creation_expression (env : env)
    ((v1, v2, v3, v4, v5) : CST.unqualified_object_creation_expression) =
  let v1 = token env v1 (* "new" *) in
  let v2 = option (type_arguments env) v2 in
  let v3 = basic_type_extra env v3 in
  let v4 = argument_list env v4 in
  let v5 =
    match v5 with
    | Some x -> Some (class_body env x)
    | None -> None
  in
  (v1, v2, v3, v4, v5)

and field_access (env : env) ((v1, v2, v3, v4) : CST.field_access) =
  let v1 =
    match v1 with
    | `Prim_exp x -> primary_expression env x
    | `Super tok -> super env tok
    (* "super" *)
  in
  let v2 =
    match v2 with
    | Some (v1bis, v2bis) ->
        let v1bis = token env v1bis (* "." *) in
        let v2bis = super_id_field env v2bis (* "super" *) in
        fun v3 v4 -> Dot (Dot (v1, v1bis, v2bis), v3, v4)
    | None -> fun v3 v4 -> Dot (v1, v3, v4)
  in
  let v3 = token env v3 (* "." *) in

  let v4 =
    match v4 with
    | `Id tok -> str env tok (* pattern [a-zA-Z_]\w* *)
    | `Choice_open x -> choice_open env str x
    | `This tok -> str env tok
    (* "this" *)
  in
  v2 v3 v4

and array_access (env : env) ((v1, v2, v3, v4) : CST.array_access) =
  let v1 = primary_expression env v1 in
  let v2 = token env v2 (* "[" *) in
  let v3 = expression env v3 in
  let v4 = token env v4 (* "]" *) in
  ArrayAccess (v1, (v2, v3, v4))

and argument_list (env : env) ((v1, v2, v3) : CST.argument_list) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = expression env v1 in
        let v2 =
          List_.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = expression env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v3 = token env v3 (* ")" *) in
  (v1, v2, v3)

and type_arguments (env : env) ((v1, v2, v3) : CST.type_arguments) =
  let v1 = token env v1 (* "<" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 =
          match v1 with
          | `Type x -> TArgument (type_ env x)
          | `Wild x -> wildcard env x
        in
        let v2 =
          List_.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 =
                match v2 with
                | `Type x -> TArgument (type_ env x)
                | `Wild x -> wildcard env x
              in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let v3 = token env v3 (* ">" *) in
  (v1, v2, v3)

and wildcard (env : env) ((v1, v2, v3) : CST.wildcard) =
  let _v1 = List_.map (annotation env) v1 in
  let v2 = token env v2 (* "?" *) in
  let v3 =
    match v3 with
    | Some x -> Some (wildcard_bounds env x)
    | None -> None
  in
  TWildCard (v2, v3)

and wildcard_bounds (env : env) (x : CST.wildcard_bounds) =
  match x with
  | `Extends_type (v1, v2) ->
      let v1 = token env v1 (* "extends" *) in
      let v2 = type_ env v2 in
      ((false, v1), v2)
  | `Super_type (v1, v2) ->
      let v1 = token env v1 (* "super" *) in
      let v2 = type_ env v2 in
      ((true, v1), v2)

and stmt1 tok = function
  | [] -> EmptyStmt (Tok.fake_tok tok ";")
  | [ x ] -> x
  | xs -> Block (Tok.fake_bracket tok xs)

and statement (env : env) ~tok (x : CST.statement) : Ast_java.stmt =
  statement_aux env x |> stmt1 tok

and statement_aux (env : env) (x : CST.statement) : Ast_java.stmt list =
  match x with
  | `Semg_ellips tok ->
      let tok = (* "..." *) token env tok in
      [ Expr (Ellipsis tok, AST_generic.sc) ]
  | `Semg_named_ellips tok -> [ Expr (name_of_id env tok, AST_generic.sc) ]
  | `Choice_decl x -> (
      match x with
      | `Decl x -> [ declaration env x ]
      | `Exp_stmt (v1, v2) ->
          let v1 = expression env v1 in
          let v2 = token env v2 (* ";" *) in
          [ Expr (v1, v2) ]
      | `Labe_stmt (v1, v2, v3) ->
          let v1 = identifier env v1 (* pattern [a-zA-Z_]\w* *) in
          let v2 = token env v2 (* ":" *) in
          let v3 = statement env ~tok:v2 v3 in
          [ Label (v1, v3) ]
      | `If_stmt (v1, v2, v3, v4) ->
          let v1 = token env v1 (* "if" *) in
          let v2 = parenthesized_expression env v2 in
          let v3 = statement env ~tok:v1 v3 in
          let v4 =
            match v4 with
            | Some (v1, v2) ->
                let v1 = token env v1 (* "else" *) in
                let v2 = statement env ~tok:v1 v2 in
                Some v2
            | None -> None
          in
          [ If (v1, v2, v3, v4) ]
      | `While_stmt (v1, v2, v3) ->
          let v1 = token env v1 (* "while" *) in
          let v2 = parenthesized_expression env v2 in
          let v3 = statement env ~tok:v1 v3 in
          [ While (v1, v2, v3) ]
      | `For_stmt (v1, v2, v3, v4, v5, v6, v7, v8) ->
          let v1 = token env v1 (* "for" *) in
          let _v2 = token env v2 (* "(" *) in
          let v3 =
            match v3 with
            | `Local_var_decl x ->
                let xs, _sc = local_variable_declaration env x in
                ForInitVars xs
            | `Opt_exp_rep_COMMA_exp_SEMI (v1, v2) ->
                let v1 =
                  match v1 with
                  | Some (v1, v2) ->
                      let v1 = expression env v1 in
                      let v2 =
                        List_.map
                          (fun (v1, v2) ->
                            let _v1 = token env v1 (* "," *) in
                            let v2 = expression env v2 in
                            v2)
                          v2
                      in
                      v1 :: v2
                  | None -> []
                in
                let _v2 = token env v2 (* ";" *) in
                ForInitExprs v1
          in
          let v4 =
            match v4 with
            | Some x -> [ expression env x ]
            | None -> []
          in
          let _v5 = token env v5 (* ";" *) in
          let v6 =
            match v6 with
            | Some (v1, v2) ->
                let v1 = expression env v1 in
                let v2 =
                  List_.map
                    (fun (v1, v2) ->
                      let _v1 = token env v1 (* "," *) in
                      let v2 = expression env v2 in
                      v2)
                    v2
                in
                v1 :: v2
            | None -> []
          in
          let v7 = token env v7 (* ")" *) in
          let v8 = statement env ~tok:v7 v8 in
          [ For (v1, ForClassic (v3, v4, v6), v8) ]
      | `Enha_for_stmt (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
          let v1 = token env v1 (* "for" *) in
          let _v2 = token env v2 (* "(" *) in
          let v3 = modifiers_opt env v3 in
          let v4 = unannotated_type env v4 in
          let v5 = variable_declarator_id env v5 in
          let _v6 = token env v6 (* ":" *) in
          let v7 = expression env v7 in
          let v8 = token env v8 (* ")" *) in
          let v9 = statement env ~tok:v8 v9 in
          let vdef = canon_var v3 (Some v4) v5 in
          [ For (v1, Foreach (vdef, v7), v9) ]
      | `Blk x -> [ block env x ]
      | `SEMI tok ->
          let t = token env tok (* ";" *) in
          [ EmptyStmt t ]
      | `Assert_stmt x -> [ assert_statement env x ]
      | `Switch_exp (v1, v2, v3) ->
          let v1 = token env v1 (* "switch" *) in
          let v2 = parenthesized_expression env v2 in
          let v3 = switch_block env v3 in
          [ Switch (v1, v2, v3) ]
      | `Do_stmt (v1, v2, v3, v4, v5) ->
          let v1 = token env v1 (* "do" *) in
          let v2 = statement env ~tok:v1 v2 in
          let _v3 = token env v3 (* "while" *) in
          let v4 = parenthesized_expression env v4 in
          let _v5 = token env v5 (* ";" *) in
          [ Do (v1, v2, v4) ]
      | `Brk_stmt (v1, v2, v3) ->
          let v1 = token env v1 (* "break" *) in
          let v2 =
            match v2 with
            | Some tok -> Some (identifier env tok) (* pattern [a-zA-Z_]\w* *)
            | None -> None
          in
          let _v3 = token env v3 (* ";" *) in
          [ Break (v1, v2) ]
      | `Cont_stmt (v1, v2, v3) ->
          let v1 = token env v1 (* "continue" *) in
          let v2 =
            match v2 with
            | Some tok -> Some (identifier env tok) (* pattern [a-zA-Z_]\w* *)
            | None -> None
          in
          let _v3 = token env v3 (* ";" *) in
          [ Continue (v1, v2) ]
      | `Ret_stmt (v1, v2, v3) ->
          let v1 = token env v1 (* "return" *) in
          let v2 =
            match v2 with
            | Some x -> Some (expression env x)
            | None -> None
          in
          let _v3 = token env v3 (* ";" *) in
          [ Return (v1, v2) ]
      | `Yield_stmt (v1, v2, v3) ->
          let v1 = token env v1 (* "yield" *) in
          let v2 = Some (expression env v2) in
          let _v3 = token env v3 (* ";" *) in
          [ Return (v1, v2) ]
      | `Sync_stmt (v1, v2, v3) ->
          let v1 = token env v1 (* "synchronized" *) in
          let v2 = parenthesized_expression env v2 in
          let v3 = block env v3 in
          [ Sync (v1, v2, v3) ]
      | `Local_var_decl x ->
          let xs, sc = local_variable_declaration env x in
          [ LocalVarList (xs, sc) ]
      | `Throw_stmt (v1, v2, v3) ->
          let v1 = token env v1 (* "throw" *) in
          let v2 = expression env v2 in
          let _v3 = token env v3 (* ";" *) in
          [ Throw (v1, v2) ]
      | `Try_stmt (v1, v2, v3) ->
          let v1 = token env v1 (* "try" *) in
          let v2 = block env v2 in
          let v3 =
            match v3 with
            | `Rep1_catch_clause xs -> (List_.map (catch_clause env) xs, None)
            | `Rep_catch_clause_fina_clause (v1, v2) ->
                let v1 = List_.map (catch_clause env) v1 in
                let v2 = finally_clause env v2 in
                (v1, Some v2)
          in
          let v3a, v3b = v3 in
          [ Try (v1, None, v2, v3a, v3b) ]
      | `Try_with_resous_stmt (v1, v2, v3, v4, v5) ->
          let v1 = token env v1 (* "try" *) in
          let v2 = resource_specification env v2 in
          let v3 = block env v3 in
          let v4 = List_.map (catch_clause env) v4 in
          let v5 =
            match v5 with
            | Some x -> Some (finally_clause env x)
            | None -> None
          in
          [ Try (v1, Some v2, v3, v4, v5) ])

and block (env : env) ((v1, v2, v3) : CST.block) =
  let v1 = token env v1 (* "{" *) in
  let v2 = List_.map (statement env ~tok:v1) v2 in
  let v3 = token env v3 (* "}" *) in
  Block (v1, v2, v3)

and assert_statement (env : env) (x : CST.assert_statement) =
  match x with
  | `Assert_exp_SEMI (v1, v2, v3) ->
      let v1 = token env v1 (* "assert" *) in
      let v2 = expression env v2 in
      let _v3 = token env v3 (* ";" *) in
      Assert (v1, v2, None)
  | `Assert_exp_COLON_exp_SEMI (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "assert" *) in
      let v2 = expression env v2 in
      let _v3 = token env v3 (* ":" *) in
      let v4 = expression env v4 in
      let _v5 = token env v5 (* ";" *) in
      Assert (v1, v2, Some v4)

and switch_block (env : env) ((v1, v2, v3) : CST.switch_block) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    match v2 with
    | `Rep_switch_blk_stmt_group stmt_blocks ->
        List_.map
          (fun (cases, stmts) ->
            ( List.concat_map (fun (x, _) -> switch_label env x) cases,
              List_.map (statement env ~tok:v1) stmts ))
          stmt_blocks
    | `Rep_switch_rule rules ->
        List_.map
          (fun (label, _tok, stmt) ->
            ( switch_label env label,
              let s =
                match stmt with
                | `Exp_stmt x -> `Exp_stmt x
                | `Throw_stmt x -> `Throw_stmt x
                | `Blk x -> `Blk x
              in
              [ statement env ~tok:v1 (`Choice_decl s) ] ))
          rules
  in
  let _v3 = token env v3 (* "}" *) in
  v2

and switch_label (env : env) (x : CST.switch_label) =
  match x with
  | `Case_exp_rep_COMMA_exp (v1, v2, v3) ->
      let v1 = token env v1 (* "case" *) in
      let v2 = expression env v2 in
      let v3 =
        List_.map
          (fun (tok (* "," *), e) -> Case (token env tok, expression env e))
          v3
      in
      Case (v1, v2) :: v3
  | `Defa v1 ->
      let v1 = token env v1 (* "default" *) in
      [ Default v1 ]

and catch_clause (env : env) ((v1, v2, v3, v4, v5) : CST.catch_clause) =
  let v1 = token env v1 (* "catch" *) in
  let _v2 = token env v2 (* "(" *) in
  let v3 = catch_formal_parameter env v3 in
  let _v4 = token env v4 (* ")" *) in
  let v5 = block env v5 in
  (v1, v3, v5)

and catch_formal_parameter (env : env)
    ((v1, v2, v3) : CST.catch_formal_parameter) =
  let v1 = modifiers_opt env v1 in
  let vtyp, vothertyps = catch_type env v2 in
  let v3 = variable_declarator_id env v3 in
  let vdef = canon_var v1 (Some vtyp) v3 in
  CatchParam (vdef, vothertyps)

and catch_type (env : env) ((v1, v2) : CST.catch_type) =
  let v1 = unannotated_type env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "|" *) in
        let v2 = unannotated_type env v2 in
        v2)
      v2
  in
  (v1, v2)

and finally_clause (env : env) ((v1, v2) : CST.finally_clause) =
  let v1 = token env v1 (* "finally" *) in
  let v2 = block env v2 in
  (v1, v2)

and resource_specification (env : env)
    ((v1, v2, v3, v4, v5) : CST.resource_specification) =
  let v1 = token env v1 (* "(" *) in
  let v2 = resource env v2 in
  let v3 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* ";" *) in
        let v2 = resource env v2 in
        v2)
      v3
  in
  let _v4 =
    match v4 with
    | Some tok -> Some (token env tok) (* ";" *)
    | None -> None
  in
  let v5 = token env v5 (* ")" *) in
  (v1, v2 :: v3, v5)

and resource (env : env) (x : CST.resource) =
  match x with
  | `Opt_modifs_unan_type_var_decl_id_EQ_exp (v1, v2, v3, v4, v5) ->
      let v1 = modifiers_opt env v1 in
      let v2 = unannotated_type env v2 in
      let v3 = variable_declarator_id env v3 in
      let _v4 = token env v4 (* "=" *) in
      let v5 = expression env v5 in
      let vdef = canon_var v1 (Some v2) v3 in
      Left { f_var = vdef; f_init = Some (ExprInit v5) }
  | `Id tok ->
      let x = name_of_id env tok (* pattern [a-zA-Z_]\w* *) in
      Right x
  | `Field_access x -> Right (field_access env x)

and annotation (env : env) (x : CST.annotation) : Tok.t * annotation =
  match x with
  | `Marker_anno (v1, v2) ->
      let v1 = token env v1 (* "@" *) in
      let v2 = qualifier_extra env v2 in
      (v1, (v1, v2, None))
  | `Anno_ (v1, v2, v3) ->
      let v1 = token env v1 (* "@" *) in
      let v2 = qualifier_extra env v2 in
      let v3 = annotation_argument_list env v3 in
      (v1, (v1, v2, Some v3))

and annotation_argument_list (env : env)
    ((v1, v2, v3) : CST.annotation_argument_list) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | `Elem_value x -> AnnotArgValue (element_value env x)
    | `Opt_elem_value_pair_rep_COMMA_elem_value_pair opt -> (
        match opt with
        | Some (v1, v2) ->
            let v1 = AnnotPair (element_value_pair env v1) in
            let v2 =
              List_.map
                (fun (v1, v2) ->
                  let _v1 = token env v1 (* "," *) in
                  let v2 = AnnotPair (element_value_pair env v2) in
                  v2)
                v2
            in
            AnnotArgPairInit (v1 :: v2)
        | None -> EmptyAnnotArg)
  in
  let v3 = token env v3 (* ")" *) in
  (v1, v2, v3)

and element_value_pair (env : env) ((v1, v2, v3) : CST.element_value_pair) =
  let v1 = identifier env v1 (* pattern [a-zA-Z_]\w* *) in
  let _v2 = token env v2 (* "=" *) in
  let v3 = element_value env v3 in
  (v1, v3)

and element_value (env : env) (x : CST.element_value) =
  match x with
  | `Exp x -> AnnotExprInit (expression env x)
  | `Elem_value_array_init (v1, v2, v3, v4) ->
      let lbrace = token env v1 (* "{" *) in
      let rbrace = token env v4 (* "}" *) in
      let v2 =
        match v2 with
        | Some (v1, v2) ->
            let v1 = element_value env v1 in
            let v2 =
              List_.map
                (fun (v1, v2) ->
                  let _v1 = token env v1 (* "," *) in
                  let v2 = element_value env v2 in
                  v2)
                v2
            in
            v1 :: v2
        | None -> []
      in
      let _v3 =
        match v3 with
        | Some tok -> Some (token env tok) (* "," *)
        | None -> None
      in
      AnnotArrayInit (lbrace, v2, rbrace)
  | `Anno x ->
      let _tok, x = annotation env x in
      AnnotNestedAnnot x

and declaration (env : env) (x : CST.declaration) : AST.stmt =
  match x with
  | `Module_decl (v1, v2, v3, v4, v5) ->
      let _v1 = List_.map (annotation env) v1 in
      let _v2 =
        match v2 with
        | Some tok -> Some (token env tok) (* "open" *)
        | None -> None
      in
      let v3 = token env v3 (* "module" *) in
      let _v4 = qualifier_extra env v4 in
      let _v5 = module_body env v5 in
      DirectiveStmt (ModuleTodo v3)
  | `Pack_decl (v1, v2, v3, v4) ->
      let _v1 = List_.map (annotation env) v1 in
      let v2 = token env v2 (* "package" *) in
      let v3 = qualifier_extra env v3 in
      let v4 = token env v4 (* ";" *) in
      DirectiveStmt (Package (v2, v3, v4))
  | `Import_decl (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "import" *) in
      let v2 =
        match v2 with
        | Some tok -> Some (token env tok) (* "static" *)
        | None -> None
      in
      let v3 = qualifier_extra env v3 in
      let v4 =
        match v4 with
        | Some (v1bis, v2bis) ->
            let _v1bis = token env v1bis (* "." *) in
            let v2bis = token env v2bis (* "*" *) in
            ImportAll (v1, v3, v2bis)
        | None -> (
            match List.rev v3 with
            | [] -> raise Impossible
            | x :: xs -> ImportFrom (v1, List.rev xs, x))
      in
      let _v5 = token env v5 (* ";" *) in
      DirectiveStmt (Import (v2, v4))
  | `Class_decl x -> DeclStmt (Class (class_declaration env x))
  | `Inte_decl x -> DeclStmt (Class (interface_declaration env x))
  | `Record_decl x -> DeclStmt (Class (record_declaration env x))
  | `Anno_type_decl x -> DeclStmt (Class (annotation_type_declaration env x))
  | `Enum_decl x -> DeclStmt (Enum (enum_declaration env x))

and enum_declaration (env : env) ((v1, v2, v3, v4, v5) : CST.enum_declaration) :
    enum_decl =
  let v1 = modifiers_opt env v1 in
  let _v2 = token env v2 (* "enum" *) in
  let v3 = identifier env v3 (* pattern [a-zA-Z_]\w* *) in
  let v4 =
    match v4 with
    | Some x -> super_interfaces env x
    | None -> []
  in
  let v5 = enum_body env v5 in
  { en_name = v3; en_mods = v1; en_impls = v4; en_body = v5 }

and enum_body (env : env) ((v1, v2, v3, v4, v5) : CST.enum_body) =
  let _v1 = token env v1 (* "{" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = enum_constant env v1 in
        let v2 =
          List_.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = enum_constant env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let _v3 =
    match v3 with
    | Some tok -> Some (token env tok) (* "," *)
    | None -> None
  in
  let v4 =
    match v4 with
    | Some x -> enum_body_declarations env x
    | None -> []
  in
  let _v5 = token env v5 (* "}" *) in
  (v2, v4)

and record_declaration (env : env)
    ((v1, v2, v3, v4, v5, v6, v7) : CST.record_declaration) : class_decl =
  let v1 = modifiers_opt env v1 in
  let v2 = token env v2 (* "record" *) in
  let v3 = identifier env v3 (* pattern [a-zA-Z_]\w* *) in
  let v4 =
    match v4 with
    | Some x -> type_parameters env x
    | None -> []
  in
  let v5 = formal_parameters env v5 in
  let v6 =
    match v6 with
    | Some x -> super_interfaces env x
    | None -> []
  in
  let v7 = class_body env v7 in
  {
    cl_name = v3;
    cl_kind = (Record, v2);
    cl_tparams = v4;
    cl_mods = v1;
    cl_extends = None;
    cl_impls = v6;
    cl_formals = v5;
    cl_body = v7;
  }

and comp_cons_decl env (v1, v2, v3) =
  (* Compact constructors are a way to rewrite the default constructors for
     a record, a kind of metaprogramming. We won't be able to analyze it
     super meaningfully, but that should be fine.
  *)
  let v1 = modifiers_opt env v1 in
  let v2 = (* pattern [\p{L}_$][\p{L}\p{Nd}_$]* *) str env v2 in
  let ent = { name = v2; mods = v1; type_ = None } in
  let v3 = block env v3 in
  { m_var = ent; m_formals = []; m_throws = []; m_body = v3 }

and class_body_decl env (x : CST.class_body_declaration) =
  match x with
  | `Choice_field_decl x -> (
      match x with
      | `Record_decl x -> [ Class (record_declaration env x) ]
      | `Field_decl x -> field_declaration env x
      | `Meth_decl x -> [ Method (method_declaration env x) ]
      | `Comp_cons_decl (v1, v2, v3) ->
          [ Method (comp_cons_decl env (v1, v2, v3)) ]
      | `Class_decl x -> [ Class (class_declaration env x) ]
      | `Inte_decl x -> [ Class (interface_declaration env x) ]
      | `Anno_type_decl x -> [ Class (annotation_type_declaration env x) ]
      | `Enum_decl x -> [ Enum (enum_declaration env x) ]
      | `Blk x ->
          let x = block env x in
          [ Init (None, x) ]
      | `Static_init x -> [ static_initializer env x ]
      | `Cons_decl x -> [ Method (constructor_declaration env x) ]
      | `SEMI tok -> [ EmptyDecl (token env tok) (* ";" *) ])
  | `Semg_ellips tok -> [ DeclEllipsis (token env tok) ]
  | `Semg_named_ellips tok -> [ DeclMetavarEllipsis (str env tok) ]

and enum_body_declarations (env : env) ((v1, v2) : CST.enum_body_declarations) =
  let _v1 = token env v1 (* ";" *) in
  let v2 = List_.map (fun x -> class_body_decl env x) v2 in
  List_.flatten v2

and enum_constant (env : env) ((v1, v2, v3, v4) : CST.enum_constant) =
  let _v1 = modifiers_opt env v1 in
  let v2 = identifier env v2 (* pattern [a-zA-Z_]\w* *) in
  let v3 =
    match v3 with
    | Some x -> Some (argument_list env x)
    | None -> None
  in
  let v4 =
    match v4 with
    | Some x -> Some (class_body env x)
    | None -> None
  in
  (v2, v3, v4)

and class_declaration (env : env)
    ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.class_declaration) : class_decl =
  let v1 = modifiers_opt env v1 in
  let v2 = token env v2 (* "class" *) in
  let v3 = identifier env v3 (* pattern [a-zA-Z_]\w* *) in
  let v4 =
    match v4 with
    | Some x -> type_parameters env x
    | None -> []
  in
  let v5 =
    match v5 with
    | Some x -> Some (superclass env x)
    | None -> None
  in
  let v6 =
    match v6 with
    | Some x -> super_interfaces env x
    | None -> []
  in
  let _v7 =
    match v7 with
    | Some x -> map_permits env x
    | None -> []
  in
  let v8 = class_body env v8 in
  {
    cl_name = v3;
    cl_kind = (ClassRegular, v2);
    cl_tparams = v4;
    cl_mods = v1;
    cl_extends = v5;
    cl_impls = v6;
    cl_formals = [];
    cl_body = v8;
  }

and map_permits (env : env) ((v1, v2) : CST.permits) =
  let _v1 = (* "permits" *) token env v1 in
  let v2 = interface_type_list env v2 in
  v2

and modifiers_opt env xopt =
  match xopt with
  | None -> []
  | Some v1 -> modifiers env v1

and modifiers (env : env) (xs : CST.modifiers) =
  List_.map
    (fun x ->
      match x with
      | `Anno x ->
          let tok, annot = annotation env x in
          (Annotation annot, tok)
      | `Public tok -> (Public, token env tok) (* "public" *)
      | `Prot tok -> (Protected, token env tok) (* "protected" *)
      | `Priv tok -> (Private, token env tok) (* "private" *)
      | `Abst tok -> (Abstract, token env tok) (* "abstract" *)
      | `Static tok -> (Static, token env tok) (* "static" *)
      | `Final tok -> (Final, token env tok) (* "final" *)
      | `Stri tok -> (StrictFP, token env tok) (* "strictfp" *)
      | `Defa tok -> (DefaultModifier, token env tok) (* "default" *)
      | `Sync tok -> (Synchronized, token env tok) (* "synchronized" *)
      | `Native tok -> (Native, token env tok) (* "native" *)
      | `Tran tok -> (Transient, token env tok) (* "transient" *)
      | `Vola tok -> (Volatile, token env tok) (* "volatile" *)
      | `NonD tok -> (NonSealed, token env tok)
      | `Sealed tok -> (Sealed, token env tok))
    xs

and type_parameters (env : env) ((v1, v2, v3, v4) : CST.type_parameters) =
  let _v1 = token env v1 (* "<" *) in
  let v2 = type_parameter env v2 in
  let v3 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = type_parameter env v2 in
        v2)
      v3
  in
  let _v4 = token env v4 (* ">" *) in
  v2 :: v3

and type_parameter (env : env) ((v1, v2, v3) : CST.type_parameter) :
    type_parameter =
  let _v1 = List_.map (annotation env) v1 in
  let v2 = identifier env v2 (* pattern [a-zA-Z_]\w* *) in
  let v3 =
    match v3 with
    | Some x -> type_bound env x
    | None -> []
  in
  TParam (v2, v3)

and type_bound (env : env) ((v1, v2, v3) : CST.type_bound) =
  let _v1 = token env v1 (* "extends" *) in
  let v2 = type_ env v2 in
  let v3 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "&" *) in
        let v2 = type_ env v2 in
        v2)
      v3
  in
  v2 :: v3

and superclass (env : env) ((v1, v2) : CST.superclass) =
  let _v1 = token env v1 (* "extends" *) in
  let v2 = type_ env v2 in
  v2

and super_interfaces (env : env) ((v1, v2) : CST.super_interfaces) =
  let _v1 = token env v1 (* "implements" *) in
  let v2 = interface_type_list env v2 in
  v2

(* called just type_list now in grammar.js after 'permits' introduction *)
and interface_type_list (env : env) ((v1, v2) : CST.type_list) =
  let v1 = type_ env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = type_ env v2 in
        v2)
      v2
  in
  v1 :: v2

and class_body (env : env) ((v1, v2, v3) : CST.class_body) =
  let v1 = token env v1 (* "{" *) in
  let v2 = List_.map (fun x -> class_body_decl env x) v2 in
  let v3 = token env v3 (* "}" *) in
  (v1, List_.flatten v2, v3)

and static_initializer (env : env) ((v1, v2) : CST.static_initializer) =
  let v1 = token env v1 (* "static" *) in
  let v2 = block env v2 in
  Init (Some v1, v2)

and constructor_declaration (env : env)
    ((v1, v2, v3, v4) : CST.constructor_declaration) : constructor_decl =
  let v1 = modifiers_opt env v1 in
  let v2 = constructor_declarator env v2 in
  let v3 =
    match v3 with
    | Some x -> throws env x
    | None -> []
  in
  let _tparams, id, params = v2 in
  let vdef = { name = id; mods = v1; type_ = None } in
  let v4 = constructor_body env v4 in
  { m_var = vdef; m_formals = params; m_throws = v3; m_body = v4 }

and constructor_declarator (env : env)
    ((v1, v2, v3) : CST.constructor_declarator) =
  let v1 =
    match v1 with
    | Some x -> type_parameters env x
    | None -> []
  in
  let v2 = identifier env v2 (* pattern [a-zA-Z_]\w* *) in
  let v3 = formal_parameters env v3 in
  (v1, v2, v3)

and constructor_body (env : env) ((v1, v2, v3, v4) : CST.constructor_body) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    match v2 with
    | Some x -> [ explicit_constructor_invocation env x ]
    | None -> []
  in
  let v3 = List_.map (statement env ~tok:v1) v3 in
  let v4 = token env v4 (* "}" *) in
  Block (v1, v2 @ v3, v4)

and explicit_constructor_invocation (env : env)
    ((v1, v2, v3) : CST.explicit_constructor_invocation) =
  let v1 =
    match v1 with
    | `Opt_type_args_choice_this (v1, v2) ->
        let _v1TODO = option (type_arguments env) v1 in
        let v2 =
          match v2 with
          | `This tok -> This (token env tok) (* "this" *)
          | `Super tok -> super env tok
          (* "super" *)
        in
        v2
    | `Choice_prim_exp_DOT_opt_type_args_super (v1, v2, v3, v4) ->
        let v1 =
          match v1 with
          | `Prim_exp x -> primary_expression env x
        in
        let v2 = token env v2 (* "." *) in
        let _v3 = option (type_arguments env) v3 in
        let v4 = super_id_field env v4 (* "super" *) in
        Dot (v1, v2, v4)
  in
  let v2 = argument_list env v2 in
  let v3 = token env v3 (* ";" *) in
  Expr (Call (v1, v2), v3)

and field_declaration (env : env) ((v1, v2, v3, v4) : CST.field_declaration) =
  let v1 = modifiers_opt env v1 in
  let v2 = unannotated_type env v2 in
  let v3 = variable_declarator_list env v3 in
  let _v4 = token env v4 (* ";" *) in
  decls (fun x -> Field x) v1 v2 v3

and annotation_type_declaration (env : env)
    ((v1, v2, v3, v4) : CST.annotation_type_declaration) : class_decl =
  let v1 = modifiers_opt env v1 in
  let v2 = token env v2 (* "@interface" *) in
  let v3 = identifier env v3 (* pattern [a-zA-Z_]\w* *) in
  let v4 = annotation_type_body env v4 in
  {
    cl_mods = v1;
    cl_name = v3;
    cl_body = v4;
    cl_kind = (AtInterface, v2);
    cl_tparams = [];
    cl_extends = None;
    cl_impls = [];
    cl_formals = [];
  }

and annotation_type_body (env : env) ((v1, v2, v3) : CST.annotation_type_body) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    List_.map
      (fun x ->
        match x with
        | `Anno_type_elem_decl x ->
            [ annotation_type_element_declaration env x ]
        | `Cst_decl x -> constant_declaration env x
        | `Class_decl x -> [ Class (class_declaration env x) ]
        | `Inte_decl x -> [ Class (interface_declaration env x) ]
        | `Enum_decl x -> [ Enum (enum_declaration env x) ]
        | `Anno_type_decl x -> [ Class (annotation_type_declaration env x) ])
      v2
  in
  let v3 = token env v3 (* "}" *) in
  (v1, List_.flatten v2, v3)

and annotation_type_element_declaration (env : env)
    ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.annotation_type_element_declaration)
    =
  let _v1 = modifiers_opt env v1 in
  let _v2 = unannotated_type env v2 in
  let v3 = token env v3 (* pattern [a-zA-Z_]\w* *) in
  let _v4 = token env v4 (* "(" *) in
  let _v5 = token env v5 (* ")" *) in
  let _v6 =
    match v6 with
    | Some x -> dimensions env x
    | None -> []
  in
  let _v7 =
    match v7 with
    | Some x -> Some (default_value env x)
    | None -> None
  in
  let _v8 = token env v8 (* ";" *) in
  AnnotationTypeElementTodo v3

and default_value (env : env) ((v1, v2) : CST.default_value) =
  let v1 = token env v1 (* "default" *) in
  let v2 = element_value env v2 in
  ((DefaultModifier, v1), v2)

and interface_declaration (env : env)
    ((v1, v2, v3, v4, v5, v6, v7) : CST.interface_declaration) : class_decl =
  let v1 = modifiers_opt env v1 in
  let v2 = token env v2 (* "interface" *) in
  let v3 = identifier env v3 (* pattern [a-zA-Z_]\w* *) in
  let v4 =
    match v4 with
    | Some x -> type_parameters env x
    | None -> []
  in
  let v5 =
    match v5 with
    | Some x -> extends_interfaces env x
    | None -> []
  in
  let _v6TODO =
    match v6 with
    | Some x -> map_permits env x
    | None -> []
  in
  let v7 = interface_body env v7 in
  {
    cl_name = v3;
    cl_kind = (Interface, v2);
    cl_tparams = v4;
    cl_mods = v1;
    cl_extends = None;
    cl_impls = v5;
    cl_formals = [];
    cl_body = v7;
  }

and extends_interfaces (env : env) ((v1, v2) : CST.extends_interfaces) =
  let _v1 = token env v1 (* "extends" *) in
  let v2 = interface_type_list env v2 in
  v2

and interface_body (env : env) ((v1, v2, v3) : CST.interface_body) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    List_.map
      (fun x ->
        match x with
        | `Cst_decl x -> constant_declaration env x
        | `Enum_decl x -> [ Enum (enum_declaration env x) ]
        | `Meth_decl x -> [ Method (method_declaration env x) ]
        | `Class_decl x -> [ Class (class_declaration env x) ]
        | `Inte_decl x -> [ Class (interface_declaration env x) ]
        | `Record_decl x -> [ Class (record_declaration env x) ]
        | `Anno_type_decl x -> [ Class (annotation_type_declaration env x) ]
        | `SEMI tok -> [ EmptyDecl (token env tok) (* ";" *) ])
      v2
  in
  let v3 = token env v3 (* "}" *) in
  (v1, List_.flatten v2, v3)

and constant_declaration (env : env)
    ((v1, v2, v3, v4) : CST.constant_declaration) =
  let v1 = modifiers_opt env v1 in
  let v2 = unannotated_type env v2 in
  let v3 = variable_declarator_list env v3 in
  let _v4 = token env v4 (* ";" *) in
  decls (fun x -> Field x) v1 v2 v3

and variable_declarator_list (env : env)
    ((v1, v2) : CST.variable_declarator_list) =
  let v1 = variable_declarator env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = variable_declarator env v2 in
        v2)
      v2
  in
  v1 :: v2

and init_extra env = function
  | `Exp x -> ExprInit (expression env x)
  | `Array_init x -> ArrayInit (array_initializer env x)

and variable_declarator (env : env) ((v1, v2) : CST.variable_declarator) :
    var_decl_id * init option =
  let v1 = variable_declarator_id env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let _v1 = token env v1 (* "=" *) in
        let v2 = init_extra env v2 in
        Some v2
    | None -> None
  in
  (v1, v2)

and variable_declarator_id (env : env) ((v1, v2) : CST.variable_declarator_id) :
    var_decl_id =
  let v1 = id_extra env v1 in
  let v2 =
    match v2 with
    | Some x -> dimensions env x
    | None -> []
  in
  List.fold_left (fun acc _e -> ArrayDecl acc) (IdentDecl v1) v2

and array_initializer (env : env) ((v1, v2, v3, v4) : CST.array_initializer) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = init_extra env v1 in
        let v2 =
          List_.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 = init_extra env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let _v3_trailing =
    match v3 with
    | Some tok -> Some (token env tok) (* "," *)
    | None -> None
  in
  let v4 = token env v4 (* "}" *) in
  (v1, v2, v4)

and type_ (env : env) (x : CST.type_) : typ =
  match x with
  | `Unan_type x -> unannotated_type env x
  | `Anno_type (v1, v2) ->
      let _v1 = List_.map (annotation env) v1 in
      let v2 = unannotated_type env v2 in
      v2

and unannotated_type (env : env) (x : CST.unannotated_type) : typ =
  match x with
  | `Choice_void_type x -> basic_type_extra env x
  | `Array_type (v1, v2) ->
      let v1 = unannotated_type env v1 in
      let v2 = dimensions env v2 in
      List.fold_left (fun acc (t1, (), t2) -> TArray (t1, acc, t2)) v1 v2

and scoped_type_identifier (env : env)
    ((v1, v2, v3, v4) : CST.scoped_type_identifier) : class_type =
  let v1 =
    match v1 with
    | `Id tok ->
        let id = str env tok (* pattern [a-zA-Z_]\w* *) in
        [ (id, None) ]
    | `Scoped_type_id x -> scoped_type_identifier env x
    | `Gene_type x -> generic_type env x
  in
  let _v2 = token env v2 (* "." *) in
  let _v3 = List_.map (annotation env) v3 in
  let v4 = identifier env v4 (* pattern [a-zA-Z_]\w* *) in

  v1 @ [ (v4, None) ]

and generic_type (env : env) ((v1, v2) : CST.generic_type) : class_type =
  let v1 =
    match v1 with
    | `Id tok ->
        let id = str env tok (* pattern [a-zA-Z_]\w* *) in
        [ (id, None) ]
    | `Scoped_type_id x -> scoped_type_identifier env x
  in
  let v2 = type_arguments env v2 in
  match List.rev v1 with
  | [] -> raise Impossible
  | (x, None) :: xs -> List.rev xs @ [ (x, Some v2) ]
  | (_x, _) :: _xs -> raise Impossible

and method_header (env : env) ((v1, v2, v3, v4) : CST.method_header) =
  let v1 =
    match v1 with
    | Some (v1, v2) ->
        let v1 = type_parameters env v1 in
        let _v2 = List_.map (annotation env) v2 in
        v1
    | None -> []
  in
  let v2 = unannotated_type env v2 in
  let v3 = method_declarator env v3 in
  let v4 =
    match v4 with
    | Some x -> throws env x
    | None -> []
  in
  let id, params, dims = v3 in
  let t =
    List.fold_left (fun acc (t1, (), t2) -> TArray (t1, acc, t2)) v2 dims
  in
  (v1, t, id, params, v4)

and method_declarator (env : env) ((v1, v2, v3) : CST.method_declarator) =
  let v1 = id_extra env v1 in
  let v2 = formal_parameters env v2 in
  let v3 =
    match v3 with
    | Some x -> dimensions env x
    | None -> []
  in
  (v1, v2, v3)

and formal_parameters (env : env) ((v1, v2, v3, v4) : CST.formal_parameters) =
  let _v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | Some x -> [ receiver_parameter env x ]
    | None -> []
  in
  let v3 =
    match v3 with
    | Some (v1, v2) ->
        let v1 =
          match v1 with
          | `Formal_param x -> formal_parameter env x
          | `Spread_param x -> spread_parameter env x
        in
        let v2 =
          List_.map
            (fun (v1, v2) ->
              let _v1 = token env v1 (* "," *) in
              let v2 =
                match v2 with
                | `Formal_param x -> formal_parameter env x
                | `Spread_param x -> spread_parameter env x
              in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let _v4 = token env v4 (* ")" *) in
  v2 @ v3

and formal_parameter (env : env) (x : CST.formal_parameter) =
  match x with
  | `Opt_modifs_unan_type_var_decl_id (v1, v2, v3) ->
      let v1 = modifiers_opt env v1 in
      let v2 = unannotated_type env v2 in
      let v3 = variable_declarator_id env v3 in
      ParamClassic (AST.canon_var v1 (Some v2) v3)
  | `Semg_ellips tok ->
      let tok = (* "..." *) token env tok in
      ParamEllipsis tok
  | `Semg_named_ellips tok ->
      ParamClassic (AST.canon_var [] None (IdentDecl (str env tok)))

and receiver_parameter (env : env) ((v1, v2, v3, v4) : CST.receiver_parameter) =
  let _v1 = List_.map (annotation env) v1 in
  let v2 = unannotated_type env v2 in
  let _v3 =
    match v3 with
    | Some (v1, v2) ->
        (* TODO *)
        let _v1 = token env v1 (* pattern [a-zA-Z_]\w* *) in
        let _v2 = token env v2 (* "." *) in
        ()
    | None -> ()
  in
  (* TODO: build special AST for that? what is a receiver_parameter? *)
  let v4 = str env v4 (* "this" *) in
  ParamReceiver (AST.canon_var [] (Some v2) (IdentDecl v4))

and spread_parameter (env : env) ((v1, v2, v3, v4) : CST.spread_parameter) =
  let v1 = modifiers_opt env v1 in
  let v2 = unannotated_type env v2 in
  let v3 = token env v3 (* "..." *) in
  let v4 = variable_declarator env v4 in
  let vdef, _init_optTODO = v4 in
  ParamSpread (v3, AST.canon_var v1 (Some v2) vdef)

and throws (env : env) ((v1, v2, v3) : CST.throws) : typ list =
  let _v1 = token env v1 (* "throws" *) in
  let v2 = type_ env v2 in
  let v3 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = token env v1 (* "," *) in
        let v2 = type_ env v2 in
        v2)
      v3
  in
  v2 :: v3

and local_variable_declaration (env : env)
    ((v1, v2, v3, v4) : CST.local_variable_declaration) :
    var_with_init list * sc =
  let v1 = modifiers_opt env v1 in
  let v2 = unannotated_type env v2 in
  let v3 = variable_declarator_list env v3 in
  let sc = token env v4 (* ";" *) in
  (decls (fun x -> x) v1 v2 v3, sc)

and method_declaration (env : env) ((v1, v2, v3) : CST.method_declaration) =
  let v1 = modifiers_opt env v1 in
  let v2 = method_header env v2 in
  let v3 =
    match v3 with
    | `Blk x -> block env x
    | `SEMI tok -> EmptyStmt (token env tok)
    (* ";" *)
  in
  let _tparams, t, id, params, throws = v2 in
  { (AST.method_header v1 t (IdentDecl id, params) throws) with m_body = v3 }

let partials (env : env) (x : CST.partials) =
  match x with
  | `Part_meth (v1, v2) ->
      let v1 = modifiers_opt env v1 in
      let v2 = method_header env v2 in
      let v3 = EmptyStmt (Tok.unsafe_fake_tok "") in
      let _tparams, t, id, params, throws = v2 in
      PartialDecl
        (Method
           {
             (AST.method_header v1 t (IdentDecl id, params) throws) with
             m_body = v3;
           })

let program (env : env) (file : Fpath.t) (x : CST.program) =
  match x with
  | `Rep_stmt xs ->
      let tok = Tok.first_tok_of_file file in
      AProgram (List_.map (statement env ~tok) xs)
  | `Cons_decl x -> AStmt (DeclStmt (Method (constructor_declaration env x)))
  | `Exp x -> AExpr (expression env x)
  | `Partis x -> Partial (partials env x)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_java.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      match program env file cst with
      | AProgram xs -> xs
      | _ -> failwith "not a program")

let parse_pattern str =
  H.wrap_parser
    (fun () -> Tree_sitter_java.Parse.string str)
    (fun cst _extras ->
      let file = Fpath.v "<pattern>" in
      let env = { H.file; conv = H.line_col_to_pos_pattern str; extra = () } in
      program env file cst)
