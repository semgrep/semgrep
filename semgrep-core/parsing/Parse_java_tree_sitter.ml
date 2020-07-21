(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
open Common
module AST = Ast_java
module CST = Tree_sitter_java.CST
module PI = Parse_info
open Ast_java
module G = AST_generic
module H = Parse_tree_sitter_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Java parser using ocaml-tree-sitter-lang/java and converting
 * to pfff/lang_java/parsing/ast_java.ml
 *
 * The resulting AST can then be converted to the generic AST by using
 * pfff/lang_java/analyze/java_to_generic.ml
 *
 * TODO:
 *  - look at all the _xxx var in this file; Many of them should really
 *    be in the AST (annotations, certain punctuation tokens, etc.)
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type env = H.env
let fake = G.fake
let token = H.token
let str = H.str

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying ocaml-tree-sitter-lang/java/Boilerplate.ml *)

(**
   Boilerplate to be used as a template when mapping the java CST
   to another type of tree.
*)

let identifier (env : env) (tok : CST.identifier) =
  str env tok (* pattern [a-zA-Z_]\w* *)


let floating_point_type (env : env) (x : CST.floating_point_type) =
  (match x with
  | `Floa_point_type_float tok -> TBasic ("float", token env tok) (* "float" *)
  | `Floa_point_type_doub tok -> TBasic ("double", token env tok) (* "double" *)
  )

let integral_type (env : env) (x : CST.integral_type) =
  (match x with
  | `Inte_type_byte tok -> TBasic (str env tok) (* "byte" *)
  | `Inte_type_short tok -> TBasic (str env tok) (* "short" *)
  | `Inte_type_int tok -> TBasic (str env tok) (* "int" *)
  | `Inte_type_long tok -> TBasic (str env tok) (* "long" *)
  | `Inte_type_char tok -> TBasic (str env tok) (* "char" *)
  )


let requires_modifier (env : env) (x : CST.requires_modifier) =
  (match x with
  | `Requis_modi_tran tok -> token env tok (* "transitive" *)
  | `Requis_modi_stat tok -> token env tok (* "static" *)
  )

let id_extra env = function
  | `Id tok -> str env tok (* pattern [a-zA-Z_]\w* *)
  | `Choice_open x ->
      (match x with
      | `Open tok -> str env tok (* "open" *)
      | `Modu tok -> str env tok (* "module" *)
      )


let rec qualifier_extra env = function
  | `Id tok -> [str env tok] (* pattern [a-zA-Z_]\w* *)
  | `Choice_open x ->
      (match x with
      | `Open tok -> [str env tok] (* "open" *)
      | `Modu tok -> [str env tok] (* "module" *)
      )
  | `Scop_id x -> scoped_identifier env x

and scoped_identifier (env : env) ((v1, v2, v3) : CST.scoped_identifier) : qualified_ident =
  let v1 = qualifier_extra env v1 in
  let _v2 = token env v2 (* "." *) in
  let v3 = str env v3 (* pattern [a-zA-Z_]\w* *) in
  v1 @ [v3]

let inferred_parameters (env : env) ((v1, v2, v3, v4) : CST.inferred_parameters) =
  let v1 = token env v1 (* "(" *) in
  let v2 = str env v2 (* pattern [a-zA-Z_]\w* *) in
  let v3 =
    List.map (fun (v1, v2) ->
      let _v1 = token env v1 (* "," *) in
      let v2 = str env v2 (* pattern [a-zA-Z_]\w* *) in
      v2
    ) v3
  in
  let v4 = token env v4 (* ")" *) in
  v1, v2::v3, v4

let literal (env : env) (x : CST.literal) =
  (match x with
  | `Lit_deci_int_lit tok ->
      Int (str env tok) (* decimal_integer_literal *)
  | `Lit_hex_int_lit tok ->
      Int (str env tok) (* hex_integer_literal *)
  | `Lit_octal_int_lit tok ->
      Int (str env tok) (* octal_integer_literal *)
  | `Lit_bin_int_lit tok ->
      Int (str env tok) (* binary_integer_literal *)
  | `Lit_deci_floa_point_lit tok ->
      Float (str env tok) (* decimal_floating_point_literal *)
  | `Lit_hex_floa_point_lit tok ->
      Float (str env tok) (* hex_floating_point_literal *)
  | `Lit_true tok -> Bool (true, token env tok) (* "true" *)
  | `Lit_false tok -> Bool (false, token env tok) (* "false" *)
  | `Lit_char_lit tok -> Char (str env tok) (* character_literal *)
  | `Lit_str_lit tok -> String (str env tok) (* string_literal *)
  | `Lit_null_lit tok -> Null (token env tok) (* "null" *)
  )

let module_directive (env : env) ((v1, v2) : CST.module_directive) =
  let _v1 =
    (match v1 with
    | `Requis_rep_requis_modi_choice_id (v1, v2, v3) ->
        let _v1 = token env v1 (* "requires" *) in
        let _v2 = List.map (requires_modifier env) v2 in
        let _v3 = qualifier_extra env v3 in
        ()
    | `Expors_choice_id_opt_to_opt_choice_id_rep_COMMA_choice_id (v1, v2, v3, v4, v5) ->
        let _v1 = token env v1 (* "exports" *) in
        let _v2 = qualifier_extra env v2 in
        let _v3 =
          (match v3 with
          | Some tok -> Some (token env tok) (* "to" *)
          | None -> None)
        in
        let _v4 =
          (match v4 with
          | Some x -> Some (qualifier_extra env x)
          | None -> None
          )
        in
        let _v5 =
          List.map (fun (v1, v2) ->
            let _v1 = token env v1 (* "," *) in
            let v2 = qualifier_extra env v2 in
            v2
          ) v5
        in
        ()
    | `Opens_choice_id_opt_to_opt_choice_id_rep_COMMA_choice_id (v1, v2, v3, v4, v5) ->
        let _v1 = token env v1 (* "opens" *) in
        let _v2 = qualifier_extra env v2 in
        let _v3 =
          (match v3 with
          | Some tok -> Some (token env tok) (* "to" *)
          | None -> None)
        in
        let _v4 =
          (match v4 with
          | Some x -> Some (qualifier_extra env x)
          | None -> None)
        in
        let _v5 =
          List.map (fun (v1, v2) ->
            let _v1 = token env v1 (* "," *) in
            let v2 = qualifier_extra env v2 in
            v2
          ) v5
        in
        ()
    | `Uses_choice_id (v1, v2) ->
        let _v1 = token env v1 (* "uses" *) in
        let _v2 = qualifier_extra env v2 in
        ()
    | `Provis_choice_id_with_choice_id_rep_COMMA_choice_id (v1, v2, v3, v4, v5) ->
        let _v1 = token env v1 (* "provides" *) in
        let _v2 = qualifier_extra env v2 in
        let _v3 = token env v3 (* "with" *) in
        let _v4 = qualifier_extra env v4 in
        let _v5 =
          List.map (fun (v1, v2) ->
            let _v1 = token env v1 (* "," *) in
            let v2 = qualifier_extra env v2 in
            v2
          ) v5
        in
        ()
    )
  in
  let _v2 = token env v2 (* ";" *) in
  ()

let module_body (env : env) ((v1, v2, v3) : CST.module_body) =
  let _v1 = token env v1 (* "{" *) in
  let _v2 = List.map (module_directive env) v2 in
  let _v3 = token env v3 (* "}" *) in
  ()




let rec expression (env : env) (x : CST.expression) =
  (match x with
  | `Exp_assign_exp (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | `Id tok -> name_of_id env tok (* pattern [a-zA-Z_]\w* *)
        | `Choice_open x ->
            (match x with
            | `Open tok -> name_of_id env tok (* "open" *)
            | `Modu tok -> name_of_id env tok (* "module" *)
            )
        | `Field_acce x -> field_access env x
        | `Array_acce x -> array_access env x
        )
      in
      let v2 =
        (match v2 with
        | `EQ tok -> Left (), token env tok (* "=" *)
        | `PLUSEQ tok -> Right G.Plus, token env tok (* "+=" *)
        | `DASHEQ tok -> Right G.Minus, token env tok (* "-=" *)
        | `STAREQ tok -> Right G.Mult, token env tok (* "*=" *)
        | `SLASHEQ tok -> Right G.Div, token env tok (* "/=" *)
        | `AMPEQ tok -> Right G.BitAnd, token env tok (* "&=" *)
        | `BAREQ tok -> Right G.BitOr, token env tok (* "|=" *)
        | `HATEQ tok -> Right G.BitXor, token env tok (* "^=" *)
        | `PERCEQ tok -> Right G.Mod, token env tok (* "%=" *)
        | `LTLTEQ tok -> Right G.LSL, token env tok (* "<<=" *)
        | `GTGTEQ tok -> Right G.LSR, token env tok (* ">>=" *)
        | `GTGTGTEQ tok -> Right G.ASR, token env tok (* ">>>=" *)
        )
      in
      let v3 = expression env v3 in
      (match v2 with
      | Left (), t -> Assign (v1, t, v3)
      | Right op, t -> AssignOp (v1, (op, t), v3)
      )
  | `Exp_bin_exp x -> binary_expression env x
  | `Exp_inst_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let _v2 = token env v2 (* "instanceof" *) in
      let v3 = type_ env v3 in
      InstanceOf (v1, v3)
  | `Exp_lamb_exp (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | `Id tok ->
            let id = str env tok (* pattern [a-zA-Z_]\w* *) in
            [ParamClassic (AST.entity_of_id id)]
        | `Form_params x -> formal_parameters env x
        | `Infe_params x ->
                let (_, xs, _) = inferred_parameters env x in
                xs |> List.map (fun id -> ParamClassic (AST.entity_of_id id))
        )
      in
      let _v2 = token env v2 (* "->" *) in
      let v3 =
        (match v3 with
        | `Exp x -> let x = expression env x in
                Expr (x, fake ";")
        | `Blk x -> block env x
        )
      in
      Lambda (v1, v3)

  | `Exp_tern_exp (v1, v2, v3, v4, v5) ->
      let v1 = expression env v1 in
      let _v2 = token env v2 (* "?" *) in
      let v3 = expression env v3 in
      let _v4 = token env v4 (* ":" *) in
      let v5 = expression env v5 in
      Conditional (v1, v3, v5)
  | `Exp_upda_exp x -> update_expression env x
  | `Exp_prim x -> primary env x
  | `Exp_un_exp x -> unary_expression env x
  | `Exp_cast_exp (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = type_ env v2 in
      let v3 =
        List.map (fun (v1, v2) ->
          let _v1 = token env v1 (* "&" *) in
          let v2 = type_ env v2 in
          v2
        ) v3
      in
      let v4 = token env v4 (* ")" *) in
      let v5 = expression env v5 in
      Cast ((v1, v2::v3, v4), v5)
  )


and binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Bin_exp_exp_GT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* ">" *) in
      let v3 = expression env v3 in
      Infix (v1, (G.Gt, v2), v3)
  | `Bin_exp_exp_LT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "<" *) in
      let v3 = expression env v3 in
      Infix (v1, (G.Lt, v2), v3)
  | `Bin_exp_exp_EQEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "==" *) in
      let v3 = expression env v3 in
      Infix (v1, (G.Eq, v2), v3)
  | `Bin_exp_exp_GTEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* ">=" *) in
      let v3 = expression env v3 in
      Infix (v1, (G.GtE, v2), v3)
  | `Bin_exp_exp_LTEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "<=" *) in
      let v3 = expression env v3 in
      Infix (v1, (G.LtE, v2), v3)
  | `Bin_exp_exp_BANGEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "!=" *) in
      let v3 = expression env v3 in
      Infix (v1, (G.NotEq, v2), v3)
  | `Bin_exp_exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "&&" *) in
      let v3 = expression env v3 in
      Infix (v1, (G.And, v2), v3)
  | `Bin_exp_exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "||" *) in
      let v3 = expression env v3 in
      Infix (v1, (G.Or, v2), v3)
  | `Bin_exp_exp_PLUS_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "+" *) in
      let v3 = expression env v3 in
      Infix (v1, (G.Plus, v2), v3)
  | `Bin_exp_exp_DASH_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "-" *) in
      let v3 = expression env v3 in
      Infix (v1, (G.Minus, v2), v3)
  | `Bin_exp_exp_STAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "*" *) in
      let v3 = expression env v3 in
      Infix (v1, (G.Mult, v2), v3)
  | `Bin_exp_exp_SLASH_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "/" *) in
      let v3 = expression env v3 in
      Infix (v1, (G.Div, v2), v3)
  | `Bin_exp_exp_AMP_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "&" *) in
      let v3 = expression env v3 in
      Infix (v1, (G.BitAnd, v2), v3)
  | `Bin_exp_exp_BAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "|" *) in
      let v3 = expression env v3 in
      Infix (v1, (G.BitOr, v2), v3)
  | `Bin_exp_exp_HAT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "^" *) in
      let v3 = expression env v3 in
      Infix (v1, (G.BitXor, v2), v3)
  | `Bin_exp_exp_PERC_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "%" *) in
      let v3 = expression env v3 in
      Infix (v1, (G.Mod, v2), v3)
  | `Bin_exp_exp_LTLT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "<<" *) in
      let v3 = expression env v3 in
      Infix (v1, (G.LSL, v2), v3)
  | `Bin_exp_exp_GTGT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* ">>" *) in
      let v3 = expression env v3 in
      Infix (v1, (G.LSR, v2), v3)
  | `Bin_exp_exp_GTGTGT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* ">>>" *) in
      let v3 = expression env v3 in
      Infix (v1, (G.ASR, v2), v3)
  )


and unary_expression (env : env) (x : CST.unary_expression) =
  (match x with
  | `Un_exp_PLUS_exp (v1, v2) ->
      let v1 = token env v1 (* "+" *) in
      let v2 = expression env v2 in
      Unary ((G.Plus, v1), v2)
  | `Un_exp_DASH_exp (v1, v2) ->
      let v1 = token env v1 (* "-" *) in
      let v2 = expression env v2 in
      Unary ((G.Minus, v1), v2)
  | `Un_exp_BANG_exp (v1, v2) ->
      let v1 = token env v1 (* "!" *) in
      let v2 = expression env v2 in
      Unary ((G.Not, v1), v2)
  | `Un_exp_TILDE_exp (v1, v2) ->
      let v1 = token env v1 (* "~" *) in
      let v2 = expression env v2 in
      Unary ((G.BitNot, v1), v2)
  )


and update_expression (env : env) (x : CST.update_expression) =
  (match x with
  | `Exp_PLUSPLUS (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "++" *) in
      Postfix(v1, (G.Incr, v2))
  | `Exp_DASHDASH (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "--" *) in
      Postfix(v1, (G.Decr, v2))
  | `PLUSPLUS_exp (v1, v2) ->
      let v1 = token env v1 (* "++" *) in
      let v2 = expression env v2 in
      Prefix ((G.Incr, v1), v2)
  | `DASHDASH_exp (v1, v2) ->
      let v1 = token env v1 (* "--" *) in
      let v2 = expression env v2 in
      Prefix ((G.Decr, v1), v2)
  )

and basic_type_extra env = function
  | `Void_type tok -> TBasic (str env tok) (* "void" *)
  | `Inte_type x -> integral_type env x
  | `Floa_point_type x -> floating_point_type env x
  | `Bool_type tok -> TBasic (str env tok) (* "boolean" *)
  | `Id tok ->
          let x = str env tok (* pattern [a-zA-Z_]\w* *) in
          TClass [x, []]
  | `Scop_type_id x ->
      let x = scoped_type_identifier env x in
      TClass x
  | `Gene_type x ->
      let x = generic_type env x in
      TClass x

and name_of_id env tok =
  Name ([[], str env tok])

(* TODO: use a special at some point *)
and this env tok =
  name_of_id env tok
and super env tok =
  name_of_id env tok
and super_id_field env tok =
  str env tok
and this_id_field env tok =
  str env tok
and new_id env tok =
  str env tok



and primary (env : env) (x : CST.primary) =
  (match x with
  | `Prim_lit x -> Literal (literal env x)
  | `Prim_class_lit (v1, v2, v3) ->
      let v1 = unannotated_type env v1 in
      let _v2 = token env v2 (* "." *) in
      let _v3 = token env v3 (* "class" *) in
      ClassLiteral v1
  | `Prim_this tok -> this env tok (* "this" *)
  | `Prim_id tok -> name_of_id env tok (* pattern [a-zA-Z_]\w* *)

  | `Prim_choice_open x ->
      (match x with
      | `Open tok -> name_of_id env tok (* "open" *)
      | `Modu tok -> name_of_id env tok (* "module" *)
      )
  | `Prim_paren_exp x -> parenthesized_expression env x
  | `Prim_obj_crea_exp x ->
      object_creation_expression env x
  | `Prim_field_acce x -> field_access env x
  | `Prim_array_acce x -> array_access env x
  | `Prim_meth_invo (v1, v2) ->
      let v1 =
        (match v1 with
        | `Choice_id x ->
                let id = id_extra env x in
                Name [[], id]
        | `Choice_prim_DOT_opt_super_DOT_opt_type_args_choice_id (v1, v2, v3, v4, v5) ->
            let v1 =
              (match v1 with
              | `Prim x -> primary env x
              | `Super tok -> super env tok (* "super" *)
              )
            in
            let v2 = token env v2 (* "." *) in
            let v3 =
              (match v3 with
              | Some (v1bis, v2bis) ->
                  let v1bis = super_id_field env v1bis (* "super" *) in
                  let v2bis = token env v2bis (* "." *) in
                  (fun v5 ->
                     Dot (Dot (v1, v2, v1bis), v2bis, v5))

              | None -> (fun v5 -> Dot (v1, v2, v5))
              )
            in
            let _v4 =
              (match v4 with
              | Some x -> type_arguments env x
              | None -> [])
            in
            let v5 = id_extra env v5 in
            v3 v5
        )
      in
      let v2 = argument_list env v2 in
      Call (v1, v2)
  | `Prim_meth_ref (v1, v2, v3, v4) ->
      let v1 =
        (match v1 with
        | `Type x -> let t = type_ env x in
                Right t
        | `Prim x -> Left (primary env x)
        | `Super tok -> Left (super env tok) (* "super" *)
        )
      in
      let v2 = token env v2 (* "::" *) in
      let v3 =
        (match v3 with
        | Some x -> type_arguments env x
        | None -> [])
      in
      let v4 =
        (match v4 with
        | `New tok -> new_id env tok (* "new" *)
        | `Id tok -> str env tok (* pattern [a-zA-Z_]\w* *)
        )
      in
      MethodRef (v1, v2, v3, v4)
  | `Prim_array_crea_exp (v1, v2, v3) ->
      let v1 = token env v1 (* "new" *) in
      let v2 = basic_type_extra env v2 in
      let (exprs, dims, init) =
        (match v3 with
        | `Rep1_dimens_expr_opt_dimens (v1, v2) ->
            let v1 = List.map (dimensions_expr env) v1 in
            let v2 =
              (match v2 with
              | Some x -> dimensions env x
              | None -> [])
            in
            v1, v2, None
        | `Dimens_array_init (v1, v2) ->
            let v1 = dimensions env v1 in
            let v2 = array_initializer env v2 in
            [], v1, Some (ArrayInit v2)
        )
      in
      NewArray (v1, v2, exprs, List.length dims, init)
  )


and dimensions_expr (env : env) ((v1, v2, v3, v4) : CST.dimensions_expr) =
  let _v1 = List.map (annotation env) v1 in
  let _v2 = token env v2 (* "[" *) in
  let v3 = expression env v3 in
  let _v4 = token env v4 (* "]" *) in
  v3

and dimensions (env : env) (xs : CST.dimensions) =
  List.map (fun (v1, v2, v3) ->
    let _v1 = List.map (annotation env) v1 in
    let v2 = token env v2 (* "[" *) in
    let v3 = token env v3 (* "]" *) in
    v2, (), v3
  ) xs


and parenthesized_expression (env : env) ((v1, v2, v3) : CST.parenthesized_expression) =
  let _v1 = token env v1 (* "(" *) in
  let v2 = expression env v2 in
  let _v3 = token env v3 (* ")" *) in
  v2


and object_creation_expression (env : env) (x : CST.object_creation_expression) =
  (match x with
  | `Obj_crea_exp_unqu_obj_crea_exp x ->
      let (tnew, _targsTODO, typ, args, body_opt) =
            unqualified_object_creation_expression env x in
      NewClass (tnew, typ, args, body_opt)

  | `Obj_crea_exp_prim_DOT_unqu_obj_crea_exp (v1, v2, v3) ->
      let v1 = primary env v1 in
      let v2 = token env v2 (* "." *) in
      let v3 =
        unqualified_object_creation_expression env v3
      in
      let (tnew, _targsTODO, typ, args, body_opt) = v3 in
      NewQualifiedClass (v1, v2, tnew, typ, args, body_opt)
  )


and unqualified_object_creation_expression (env : env) ((v1, v2, v3, v4, v5) : CST.unqualified_object_creation_expression) =
  let v1 = token env v1 (* "new" *) in
  let v2 =
    (match v2 with
    | Some x -> type_arguments env x
    | None -> [])
  in
  let v3 = basic_type_extra env v3 in
  let v4 = argument_list env v4 in
  let v5 =
    (match v5 with
    | Some x -> Some (class_body env x)
    | None -> None)
  in
  (v1, v2, v3, v4, v5)


and field_access (env : env) ((v1, v2, v3, v4) : CST.field_access) =
  let v1 =
    (match v1 with
    | `Prim x -> primary env x
    | `Super tok -> super env tok (* "super" *)
    )
  in
  let v2 =
    match v2 with
    | Some (v1bis, v2bis) ->
        let v1bis = token env v1bis (* "." *) in
        let v2bis = super_id_field env v2bis (* "super" *) in
        (fun v3 v4 -> Dot (Dot (v1, v1bis, v2bis), v3, v4))
    | None -> (fun v3 v4 -> Dot (v1, v3, v4))
  in
  let v3 = token env v3 (* "." *) in

  let v4 =
    (match v4 with
    | `Id tok -> str env tok (* pattern [a-zA-Z_]\w* *)
    | `Choice_open x ->
        (match x with
        | `Open tok -> str env tok (* "open" *)
        | `Modu tok -> str env tok (* "module" *)
        )
    | `This tok -> this_id_field env tok (* "this" *)
    )
  in
  v2 v3 v4


and array_access (env : env) ((v1, v2, v3, v4) : CST.array_access) =
  let v1 = primary env v1 in
  let _v2 = token env v2 (* "[" *) in
  let v3 = expression env v3 in
  let _v4 = token env v4 (* "]" *) in
  ArrayAccess (v1, v3)


and argument_list (env : env) ((v1, v2, v3) : CST.argument_list) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = expression env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let _v1 = token env v1 (* "," *) in
            let v2 = expression env v2 in
            v2
          ) v2
        in
        v1::v2
    | None -> [])
  in
  let v3 = token env v3 (* ")" *) in
  (v1, v2, v3)


and type_arguments (env : env) ((v1, v2, v3) : CST.type_arguments) =
  let _v1 = token env v1 (* "<" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 =
          (match v1 with
          | `Type x -> TArgument (type_ env x)
          | `Wild x -> TQuestion (wildcard env x)
          )
        in
        let v2 =
          List.map (fun (v1, v2) ->
            let _v1 = token env v1 (* "," *) in
            let v2 =
              (match v2 with
              | `Type x -> TArgument (type_ env x)
              | `Wild x -> TQuestion (wildcard env x)
              )
            in
            v2
          ) v2
        in
        v1::v2
    | None -> [])
  in
  let _v3 = token env v3 (* ">" *) in
  v2


and wildcard (env : env) ((v1, v2, v3) : CST.wildcard) =
  let _v1 = List.map (annotation env) v1 in
  let _v2 = token env v2 (* "?" *) in
  let v3 =
    (match v3 with
    | Some x -> Some (wildcard_bounds env x)
    | None -> None)
  in
  v3


and wildcard_bounds (env : env) (x : CST.wildcard_bounds) =
  (match x with
  | `Wild_bounds_extens_type (v1, v2) ->
      let _v1 = token env v1 (* "extends" *) in
      let v2 = type_ env v2 in
      false, v2
  | `Wild_bounds_super_type (v1, v2) ->
      let _v1 = token env v1 (* "super" *) in
      let v2 = type_ env v2 in
      true, v2
  )



and stmt1 = function
 | [] -> EmptyStmt (G.fake ";")
 | [x] -> x
 | xs -> Block (G.fake_bracket xs)

and statement (env : env) (x : CST.statement) : Ast_java.stmt =
  statement_aux env x |> stmt1

and statement_aux env x : Ast_java.stmt list =
  (match x with
  | `Stmt_decl x -> [declaration env x]
  | `Stmt_exp_stmt (v1, v2) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* ";" *) in
      [Expr (v1, v2)]
  | `Stmt_labe_stmt (v1, v2, v3) ->
      let v1 = identifier env v1 (* pattern [a-zA-Z_]\w* *) in
      let _v2 = token env v2 (* ":" *) in
      let v3 = statement env v3 in
      [Label (v1, v3)]
  | `Stmt_if_stmt (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "if" *) in
      let v2 = parenthesized_expression env v2 in
      let v3 = statement env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) ->
            let _v1 = token env v1 (* "else" *) in
            let v2 = statement env v2 in
            Some v2
        | None -> None)
      in
      [If (v1, v2, v3, v4)]
  | `Stmt_while_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "while" *) in
      let v2 = parenthesized_expression env v2 in
      let v3 = statement env v3 in
      [While (v1, v2, v3)]
  | `Stmt_for_stmt (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 = token env v1 (* "for" *) in
      let _v2 = token env v2 (* "(" *) in
      let v3 =
        (match v3 with
        | `Local_var_decl x ->
             let xs = local_variable_declaration env x in
              ForInitVars xs
        | `Opt_exp_rep_COMMA_exp_SEMI (v1, v2) ->
            let v1 =
              (match v1 with
              | Some (v1, v2) ->
                  let v1 = expression env v1 in
                  let v2 =
                    List.map (fun (v1, v2) ->
                      let _v1 = token env v1 (* "," *) in
                      let v2 = expression env v2 in
                      v2
                    ) v2
                  in
                  v1::v2
              | None -> [])
            in
            let _v2 = token env v2 (* ";" *) in
            ForInitExprs v1
        )
      in
      let v4 =
        (match v4 with
        | Some x -> [expression env x]
        | None -> [])
      in
      let _v5 = token env v5 (* ";" *) in
      let v6 =
        (match v6 with
        | Some (v1, v2) ->
            let v1 = expression env v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let _v1 = token env v1 (* "," *) in
                let v2 = expression env v2 in
                v2
              ) v2
            in
            v1::v2
        | None -> [])
      in
      let _v7 = token env v7 (* ")" *) in
      let v8 = statement env v8 in
      [For (v1, ForClassic (v3, v4, v6), v8)]
  | `Stmt_enha_for_stmt (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      let v1 = token env v1 (* "for" *) in
      let _v2 = token env v2 (* "(" *) in
      let v3 =
        (match v3 with
        | Some x -> modifiers env x
        | None -> [])
      in
      let v4 = unannotated_type env v4 in
      let v5 = variable_declarator_id env v5 in
      let _v6 = token env v6 (* ":" *) in
      let v7 = expression env v7 in
      let _v8 = token env v8 (* ")" *) in
      let v9 = statement env v9 in
      let vdef = canon_var v3 (Some v4) v5 in
      [For (v1, Foreach (vdef, v7), v9)]

  | `Stmt_blk x -> [block env x]
  | `Stmt_SEMI tok ->
        let t = token env tok (* ";" *) in [EmptyStmt t]
  | `Stmt_asse_stmt x ->
        [assert_statement env x]
  | `Stmt_swit_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "switch" *) in
      let v2 = parenthesized_expression env v2 in
      let v3 = switch_block env v3 in
      [Switch (v1, v2, v3)]
  | `Stmt_do_stmt (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "do" *) in
      let v2 = statement env v2 in
      let _v3 = token env v3 (* "while" *) in
      let v4 = parenthesized_expression env v4 in
      let _v5 = token env v5 (* ";" *) in
      [Do (v1, v2, v4)]
  | `Stmt_brk_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "break" *) in
      let v2 =
        (match v2 with
        | Some tok -> Some (identifier env tok) (* pattern [a-zA-Z_]\w* *)
        | None -> None)
      in
      let _v3 = token env v3 (* ";" *) in
      [Break (v1, v2)]
  | `Stmt_cont_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "continue" *) in
      let v2 =
        (match v2 with
        | Some tok -> Some (identifier env tok) (* pattern [a-zA-Z_]\w* *)
        | None -> None)
      in
      let _v3 = token env v3 (* ";" *) in
      [Continue (v1, v2)]
  | `Stmt_ret_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "return" *) in
      let v2 =
        (match v2 with
        | Some x -> Some (expression env x)
        | None -> None)
      in
      let _v3 = token env v3 (* ";" *) in
      [Return (v1, v2)]
  | `Stmt_sync_stmt (v1, v2, v3) ->
      let _v1 = token env v1 (* "synchronized" *) in
      let v2 = parenthesized_expression env v2 in
      let v3 = block env v3 in
      [Sync (v2, v3)]
  | `Stmt_local_var_decl x ->
      let xs = local_variable_declaration env x in
      xs |> List.map (fun x -> LocalVar x)
  | `Stmt_throw_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "throw" *) in
      let v2 = expression env v2 in
      let _v3 = token env v3 (* ";" *) in
      [Throw (v1, v2)]
  | `Stmt_try_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "try" *) in
      let v2 = block env v2 in
      let v3 =
        (match v3 with
        | `Rep1_catch_clau xs -> List.map (catch_clause env) xs, None
        | `Rep_catch_clau_fina_clau (v1, v2) ->
            let v1 = List.map (catch_clause env) v1 in
            let v2 = finally_clause env v2 in
            v1, Some v2
        )
      in
      let (v3a, v3b) = v3 in
      [Try (v1, None, v2, v3a, v3b)]
  | `Stmt_try_with_resous_stmt (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "try" *) in
      let v2 = resource_specification env v2 in
      let v3 = block env v3 in
      let v4 = List.map (catch_clause env) v4 in
      let v5 =
        (match v5 with
        | Some x -> Some (finally_clause env x)
        | None -> None)
      in
      [Try (v1, Some v2, v3, v4, v5)]
  )


and block (env : env) ((v1, v2, v3) : CST.block) =
  let v1 = token env v1 (* "{" *) in
  let v2 = List.map (statement env) v2 in
  let v3 = token env v3 (* "}" *) in
  Block (v1, v2, v3)


and assert_statement (env : env) (x : CST.assert_statement) =
  (match x with
  | `Asse_stmt_asse_exp_SEMI (v1, v2, v3) ->
      let v1 = token env v1 (* "assert" *) in
      let v2 = expression env v2 in
      let _v3 = token env v3 (* ";" *) in
      Assert (v1, v2, None)
  | `Asse_stmt_asse_exp_COLON_exp_SEMI (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "assert" *) in
      let v2 = expression env v2 in
      let _v3 = token env v3 (* ":" *) in
      let v4 = expression env v4 in
      let _v5 = token env v5 (* ";" *) in
      Assert (v1, v2, Some v4)
  )


and switch_block (env : env) ((v1, v2, v3) : CST.switch_block) =
  let _v1 = token env v1 (* "{" *) in
  let v2 =
    List.map (fun x ->
      (match x with
      | `Swit_label x -> Left (switch_label env x)
      | `Stmt x -> Right (statement env x)
      )
    ) v2
  in
  let _v3 = token env v3 (* "}" *) in
  let rec aux acc_cases acc_stmts xs =
    match xs with
    | [] -> [List.rev acc_cases, List.rev acc_stmts]
    | Left (case)::xs ->
        if acc_stmts <> []
        then
          let before = (List.rev acc_cases, List.rev acc_stmts) in
          let after = aux [case] [] xs in
          before::after
        else
          aux (case::acc_cases) acc_stmts xs
     | Right (st)::xs ->
        aux acc_cases (st::acc_stmts) xs
   in
   aux [] [] v2


and switch_label (env : env) (x : CST.switch_label) =
  (match x with
  | `Swit_label_case_exp_COLON (v1, v2, v3) ->
      let v1 = token env v1 (* "case" *) in
      let v2 = expression env v2 in
      let _v3 = token env v3 (* ":" *) in
      Case (v1, v2)
  | `Swit_label_defa_COLON (v1, v2) ->
      let v1 = token env v1 (* "default" *) in
      let _v2 = token env v2 (* ":" *) in
      Default v1
  )


and catch_clause (env : env) ((v1, v2, v3, v4, v5) : CST.catch_clause) =
  let v1 = token env v1 (* "catch" *) in
  let _v2 = token env v2 (* "(" *) in
  let v3 = catch_formal_parameter env v3 in
  let _v4 = token env v4 (* ")" *) in
  let v5 = block env v5 in
  (v1, v3, v5)


and catch_formal_parameter (env : env) ((v1, v2, v3) : CST.catch_formal_parameter) =
  let v1 =
    (match v1 with
    | Some x -> modifiers env x
    | None -> [])
  in
  let (vtyp, vothertyps) = catch_type env v2 in
  let v3 = variable_declarator_id env v3 in
  let vdef = canon_var v1 (Some vtyp) v3 in
  vdef, vothertyps


and catch_type (env : env) ((v1, v2) : CST.catch_type) =
  let v1 = unannotated_type env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let _v1 = token env v1 (* "|" *) in
      let v2 = unannotated_type env v2 in
      v2
    ) v2
  in
  v1, v2


and finally_clause (env : env) ((v1, v2) : CST.finally_clause) =
  let v1 = token env v1 (* "finally" *) in
  let v2 = block env v2 in
  v1, v2


and resource_specification (env : env) ((v1, v2, v3, v4, v5) : CST.resource_specification) =
  let v1 = token env v1 (* "(" *) in
  let v2 = resource env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let _v1 = token env v1 (* ";" *) in
      let v2 = resource env v2 in
      v2
    ) v3
  in
  let _v4 =
    (match v4 with
    | Some tok -> Some (token env tok) (* ";" *)
    | None -> None)
  in
  let v5 = token env v5 (* ")" *) in
  v1, (v2::v3), v5


and resource (env : env) (x : CST.resource) =
  (match x with
  | `Reso_opt_modifs_unan_type_var_decl_id_EQ_exp (v1, v2, v3, v4, v5) ->
      let v1 =
        (match v1 with
        | Some x -> modifiers env x
        | None -> [])
      in
      let v2 = unannotated_type env v2 in
      let v3 = variable_declarator_id env v3 in
      let _v4 = token env v4 (* "=" *) in
      let v5 = expression env v5 in
      let vdef = canon_var v1 (Some v2) v3 in
      Left { f_var = vdef; f_init = Some (ExprInit v5) }
  | `Reso_id tok ->
        let x = name_of_id env tok (* pattern [a-zA-Z_]\w* *) in
        Right x
  | `Reso_field_acce x ->
     Right (field_access env x)
  )


and annotation (env : env) (x : CST.annotation) : tok * annotation =
  (match x with
  | `Anno_mark_anno (v1, v2) ->
      let v1 = token env v1 (* "@" *) in
      let v2 = qualifier_extra env v2 in
      v1, (v1, (v2 |> List.map (fun id -> Id id)), None)
  | `Anno_anno_ (v1, v2, v3) ->
      let v1 = token env v1 (* "@" *) in
      let v2 = qualifier_extra env v2 in
      let v3 = annotation_argument_list env v3 in
      v1, (v1, (v2 |> List.map (fun id -> Id id)), Some v3)
  )


and annotation_argument_list (env : env) ((v1, v2, v3) : CST.annotation_argument_list) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | `Elem_value x -> AnnotArgValue (element_value env x)
    | `Opt_elem_value_pair_rep_COMMA_elem_value_pair opt ->
        (match opt with
        | Some (v1, v2) ->
            let v1 = element_value_pair env v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let _v1 = token env v1 (* "," *) in
                let v2 = element_value_pair env v2 in
                v2
              ) v2
            in
            AnnotArgPairInit (v1::v2)
        | None -> EmptyAnnotArg)
    )
  in
  let v3 = token env v3 (* ")" *) in
  v1, v2, v3


and element_value_pair (env : env) ((v1, v2, v3) : CST.element_value_pair) =
  let v1 = identifier env v1 (* pattern [a-zA-Z_]\w* *) in
  let _v2 = token env v2 (* "=" *) in
  let v3 = element_value env v3 in
  v1, v3


and element_value (env : env) (x : CST.element_value) =
  (match x with
  | `Exp x -> AnnotExprInit (expression env x)
  | `Elem_value_array_init (v1, v2, v3, v4) ->
      let _v1 = token env v1 (* "{" *) in
      let v2 =
        AnnotArrayInit (match v2 with
        | Some (v1, v2) ->
            let v1 = element_value env v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let _v1 = token env v1 (* "," *) in
                let v2 = element_value env v2 in
                v2
              ) v2
            in
            v1::v2
        | None -> [])
      in
      let _v3 =
        (match v3 with
        | Some tok -> Some (token env tok) (* "," *)
        | None -> None)
      in
      let _v4 = token env v4 (* "}" *) in
      v2
  | `Anno x ->
        let (_tok, x) = annotation env x in
        AnnotNestedAnnot x
  )


and declaration (env : env) (x : CST.declaration) : AST.stmt =
  (match x with
  | `Modu_decl (v1, v2, v3, v4, v5) ->
      let _v1 = List.map (annotation env) v1 in
      let _v2 =
        (match v2 with
        | Some tok -> Some (token env tok) (* "open" *)
        | None -> None)
      in
      let v3 = token env v3 (* "module" *) in
      let _v4 = qualifier_extra env v4 in
      let _v5 = module_body env v5 in
      DirectiveStmt (ModuleTodo v3)
  | `Pack_decl (v1, v2, v3, v4) ->
      let _v1 = List.map (annotation env) v1 in
      let v2 = token env v2 (* "package" *) in
      let v3 = qualifier_extra env v3 in
      let v4 = token env v4 (* ";" *) in
      DirectiveStmt (Package (v2, v3, v4))
  | `Impo_decl (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "import" *) in
      let v2 =
        (match v2 with
        | Some tok ->
                let _t = token env tok (* "static" *) in
                true
        | None -> false)
      in
      let v3 = qualifier_extra env v3 in
      let v4 =
        (match v4 with
        | Some (v1bis, v2bis) ->
            let _v1bis = token env v1bis (* "." *) in
            let v2bis = token env v2bis (* "*" *) in
            ImportAll (v1, v3, v2bis)
        | None ->
            (match List.rev v3 with
            | [] -> raise Impossible
            | x::xs ->
              ImportFrom (v1, List.rev xs, x)
            )
          )
      in
      let _v5 = token env v5 (* ";" *) in
      DirectiveStmt (Import (v2, v4))
  | `Class_decl x ->
        DeclStmt (Class (class_declaration env x))
  | `Inte_decl x ->
        DeclStmt (Class (interface_declaration env x))
  | `Anno_type_decl x ->
        DeclStmt (AnnotationType (annotation_type_declaration env x))
  | `Enum_decl x ->
        DeclStmt (Enum (enum_declaration env x))
  )


and enum_declaration (env : env) ((v1, v2, v3, v4, v5) : CST.enum_declaration)
 : enum_decl =
  let v1 =
    (match v1 with
    | Some x -> modifiers env x
    | None -> [])
  in
  let _v2 = token env v2 (* "enum" *) in
  let v3 = identifier env v3 (* pattern [a-zA-Z_]\w* *) in
  let v4 =
    (match v4 with
    | Some x -> super_interfaces env x
    | None -> [])
  in
  let v5 = enum_body env v5 in
  { en_name = v3; en_mods = v1; en_impls = v4; en_body = v5 }


and enum_body (env : env) ((v1, v2, v3, v4, v5) : CST.enum_body) =
  let _v1 = token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = enum_constant env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let _v1 = token env v1 (* "," *) in
            let v2 = enum_constant env v2 in
            v2
          ) v2
        in
        v1::v2
    | None -> [])
  in
  let _v3 =
    (match v3 with
    | Some tok -> Some (token env tok) (* "," *)
    | None -> None)
  in
  let v4 =
    (match v4 with
    | Some x -> enum_body_declarations env x
    | None -> [])
  in
  let _v5 = token env v5 (* "}" *) in
  v2, v4


and class_body_decl env = function
  | `Field_decl x -> field_declaration env x
  | `Meth_decl x -> [Method (method_declaration env x)]
  | `Class_decl x -> [Class (class_declaration env x)]
  | `Inte_decl x -> [Class (interface_declaration env x)]
  | `Anno_type_decl x -> [AnnotationType (annotation_type_declaration env x)]
  | `Enum_decl x -> [Enum (enum_declaration env x)]
  | `Blk x -> let x = block env x in
          [Init (false, x)]
  | `Stat_init x -> [static_initializer env x]
  | `Cons_decl x -> [Method (constructor_declaration env x)]
  | `SEMI tok -> [EmptyDecl (token env tok) (* ";" *)]

and enum_body_declarations (env : env) ((v1, v2) : CST.enum_body_declarations) =
  let _v1 = token env v1 (* ";" *) in
  let v2 = List.map (fun x -> class_body_decl env x) v2
  in
  List.flatten v2


and enum_constant (env : env) ((v1, v2, v3, v4) : CST.enum_constant) =
  let _v1 =
    (match v1 with
    | Some x -> modifiers env x
    | None -> [])
  in
  let v2 = identifier env v2 (* pattern [a-zA-Z_]\w* *) in
  let v3 =
    (match v3 with
    | Some x -> Some (argument_list env x)
    | None -> None)
  in
  let v4 =
    (match v4 with
    | Some x -> Some (class_body env x)
    | None -> None)
  in
  v2, v3, v4


and class_declaration (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.class_declaration) : class_decl =
  let v1 =
    (match v1 with
    | Some x -> modifiers env x
    | None -> [])
  in
  let _v2 = token env v2 (* "class" *) in
  let v3 = identifier env v3 (* pattern [a-zA-Z_]\w* *) in
  let v4 =
    (match v4 with
    | Some x -> type_parameters env x
    | None -> [])
  in
  let v5 =
    (match v5 with
    | Some x -> Some (superclass env x)
    | None -> None)
  in
  let v6 =
    (match v6 with
    | Some x -> super_interfaces env x
    | None -> [])
  in
  let v7 = class_body env v7 in
  { cl_name = v3; cl_kind = ClassRegular; cl_tparams = v4;
    cl_mods = v1; cl_extends = v5; cl_impls = v6;
    cl_body = v7 }


and modifiers (env : env) (xs : CST.modifiers) =
  List.map (fun x ->
    (match x with
    | `Anno x ->
            let tok, annot = annotation env x in
            Annotation annot, tok
    | `Publ tok -> Public, token env tok (* "public" *)
    | `Prot tok -> Protected, token env tok (* "protected" *)
    | `Priv tok -> Private, token env tok (* "private" *)
    | `Abst tok -> Abstract, token env tok (* "abstract" *)
    | `Stat tok -> Static, token env tok (* "static" *)
    | `Final tok -> Final, token env tok (* "final" *)
    | `Stri tok -> StrictFP, token env tok (* "strictfp" *)
    | `Defa tok -> DefaultModifier, token env tok (* "default" *)
    | `Sync tok -> Synchronized, token env tok (* "synchronized" *)
    | `Nati tok -> Native, token env tok (* "native" *)
    | `Tran tok -> Transient, token env tok (* "transient" *)
    | `Vola tok -> Volatile, token env tok (* "volatile" *)
    )
  ) xs


and type_parameters (env : env) ((v1, v2, v3, v4) : CST.type_parameters) =
  let _v1 = token env v1 (* "<" *) in
  let v2 = type_parameter env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let _v1 = token env v1 (* "," *) in
      let v2 = type_parameter env v2 in
      v2
    ) v3
  in
  let _v4 = token env v4 (* ">" *) in
  v2::v3


and type_parameter (env : env) ((v1, v2, v3) : CST.type_parameter) : type_parameter =
  let _v1 = List.map (annotation env) v1 in
  let v2 = identifier env v2 (* pattern [a-zA-Z_]\w* *) in
  let v3 =
    (match v3 with
    | Some x -> type_bound env x
    | None -> [])
  in
  TParam (v2, v3)


and type_bound (env : env) ((v1, v2, v3) : CST.type_bound) =
  let _v1 = token env v1 (* "extends" *) in
  let v2 = type_ env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let _v1 = token env v1 (* "&" *) in
      let v2 = type_ env v2 in
      v2
    ) v3
  in
  v2::v3


and superclass (env : env) ((v1, v2) : CST.superclass) =
  let _v1 = token env v1 (* "extends" *) in
  let v2 = type_ env v2 in
  v2


and super_interfaces (env : env) ((v1, v2) : CST.super_interfaces) =
  let _v1 = token env v1 (* "implements" *) in
  let v2 = interface_type_list env v2 in
  v2


and interface_type_list (env : env) ((v1, v2) : CST.interface_type_list) =
  let v1 = type_ env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let _v1 = token env v1 (* "," *) in
      let v2 = type_ env v2 in
      v2
    ) v2
  in
  v1::v2


and class_body (env : env) ((v1, v2, v3) : CST.class_body) =
  let v1 = token env v1 (* "{" *) in
  let v2 = List.map (fun x -> class_body_decl env x) v2 in
  let v3 = token env v3 (* "}" *) in
  v1, List.flatten v2, v3


and static_initializer (env : env) ((v1, v2) : CST.static_initializer) =
  let _v1 = token env v1 (* "static" *) in
  let v2 = block env v2 in
  Init (true, v2)


and constructor_declaration (env : env) ((v1, v2, v3, v4) : CST.constructor_declaration) : constructor_decl =
  let v1 =
    (match v1 with
    | Some x -> modifiers env x
    | None -> [])
  in
  let v2 = constructor_declarator env v2 in
  let v3 =
    (match v3 with
    | Some x -> throws env x
    | None -> [])
  in
  let (_tparams, id, params) = v2 in
  let vdef = { name = id; mods = v1; type_ = None } in
  let v4 = constructor_body env v4 in
  { m_var = vdef; m_formals = params; m_throws = v3; m_body = v4 }


and constructor_declarator (env : env) ((v1, v2, v3) : CST.constructor_declarator) =
  let v1 =
    (match v1 with
    | Some x -> type_parameters env x
    | None -> [])
  in
  let v2 = identifier env v2 (* pattern [a-zA-Z_]\w* *) in
  let v3 = formal_parameters env v3 in
  v1, v2, v3


and constructor_body (env : env) ((v1, v2, v3, v4) : CST.constructor_body) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some x -> [explicit_constructor_invocation env x]
    | None -> [])
  in
  let v3 = List.map (statement env) v3 in
  let v4 = token env v4 (* "}" *) in
  Block (v1, v2 @ v3, v4)


and explicit_constructor_invocation (env : env) ((v1, v2, v3) : CST.explicit_constructor_invocation) =
  let v1 =
    (match v1 with
    | `Opt_type_args_choice_this (v1, v2) ->
        let _v1 =
          (match v1 with
          | Some x -> type_arguments env x
          | None -> [])
        in
        let v2 =
          (match v2 with
          | `This tok -> this env tok (* "this" *)
          | `Super tok -> super env tok (* "super" *)
          )
        in
        v2
    | `Choice_prim_DOT_opt_type_args_super (v1, v2, v3, v4) ->
        let v1 =
          (match v1 with
          | `Prim x -> primary env x
          )
        in
        let v2 = token env v2 (* "." *) in
        let _v3 =
          (match v3 with
          | Some x -> type_arguments env x
          | None -> [])
        in
        let v4 = super_id_field env v4 (* "super" *) in
        Dot (v1, v2, v4)
    )
  in
  let v2 = argument_list env v2 in
  let v3 = token env v3 (* ";" *) in
  Expr (Call (v1, v2), v3)


and field_declaration (env : env) ((v1, v2, v3, v4) : CST.field_declaration) =
  let v1 =
    (match v1 with
    | Some x -> modifiers env x
    | None -> [])
  in
  let v2 = unannotated_type env v2 in
  let v3 = variable_declarator_list env v3 in
  let _v4 = token env v4 (* ";" *) in
  decls (fun x -> Field x) v1 v2 v3


and annotation_type_declaration (env : env) ((v1, v2, v3, v4) : CST.annotation_type_declaration) : annotation_type_decl =
  let v1 =
    (match v1 with
    | Some x -> modifiers env x
    | None -> [])
  in
  let v2 = token env v2 (* "@interface" *) in
  let v3 = identifier env v3 (* pattern [a-zA-Z_]\w* *) in
  let v4 = annotation_type_body env v4 in
  { an_tok = v2; an_mods = v1; an_name = v3; an_body = v4 }


and annotation_type_body (env : env) ((v1, v2, v3) : CST.annotation_type_body) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    List.map (fun x ->
      (match x with
      | `Anno_type_elem_decl x ->
          [annotation_type_element_declaration env x]
      | `Cst_decl x -> constant_declaration env x
      | `Class_decl x -> [Class (class_declaration env x)]
      | `Inte_decl x -> [Class (interface_declaration env x)]
      | `Anno_type_decl x ->
              [AnnotationType (annotation_type_declaration env x)]
      )
    ) v2
  in
  let v3 = token env v3 (* "}" *) in
  v1, List.flatten v2, v3


and annotation_type_element_declaration (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.annotation_type_element_declaration) =
  let _v1 =
    (match v1 with
    | Some x -> modifiers env x
    | None -> [])
  in
  let _v2 = unannotated_type env v2 in
  let v3 = token env v3 (* pattern [a-zA-Z_]\w* *) in
  let _v4 = token env v4 (* "(" *) in
  let _v5 = token env v5 (* ")" *) in
  let _v6 =
    (match v6 with
    | Some x -> dimensions env x
    | None -> [])
  in
  let _v7 =
    (match v7 with
    | Some x -> Some (default_value env x)
    | None -> None)
  in
  let _v8 = token env v8 (* ";" *) in
  AnnotationTypeElementTodo v3


and default_value (env : env) ((v1, v2) : CST.default_value) =
  let v1 = token env v1 (* "default" *) in
  let v2 = element_value env v2 in
  (DefaultModifier, v1), v2


and interface_declaration (env : env) ((v1, v2, v3, v4, v5, v6) : CST.interface_declaration) : class_decl =
  let v1 =
    (match v1 with
    | Some x -> modifiers env x
    | None -> [])
  in
  let _v2 = token env v2 (* "interface" *) in
  let v3 = identifier env v3 (* pattern [a-zA-Z_]\w* *) in
  let v4 =
    (match v4 with
    | Some x -> type_parameters env x
    | None -> [])
  in
  let v5 =
    (match v5 with
    | Some x -> extends_interfaces env x
    | None -> [])
  in
  let v6 = interface_body env v6 in
  { cl_name = v3; cl_kind = Interface; cl_tparams = v4;
    cl_mods = v1; cl_extends = None; cl_impls = v5;
    cl_body = v6 }


and extends_interfaces (env : env) ((v1, v2) : CST.extends_interfaces) =
  let _v1 = token env v1 (* "extends" *) in
  let v2 = interface_type_list env v2 in
  v2


and interface_body (env : env) ((v1, v2, v3) : CST.interface_body) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    List.map (fun x ->
      (match x with
      | `Cst_decl x -> constant_declaration env x
      | `Enum_decl x -> [Enum (enum_declaration env x)]
      | `Meth_decl x -> [Method (method_declaration env x)]
      | `Class_decl x -> [Class (class_declaration env x)]
      | `Inte_decl x -> [Class (interface_declaration env x)]
      | `Anno_type_decl x ->
              [AnnotationType (annotation_type_declaration env x)]
      | `SEMI tok -> [EmptyDecl (token env tok) (* ";" *)]
      )
    ) v2
  in
  let v3 = token env v3 (* "}" *) in
  v1, List.flatten v2, v3


and constant_declaration (env : env) ((v1, v2, v3, v4) : CST.constant_declaration) =
  let v1 =
    (match v1 with
    | Some x -> modifiers env x
    | None -> [])
  in
  let v2 = unannotated_type env v2 in
  let v3 = variable_declarator_list env v3 in
  let _v4 = token env v4 (* ";" *) in
  decls (fun x -> Field x) v1 v2 v3



and variable_declarator_list (env : env) ((v1, v2) : CST.variable_declarator_list) =
  let v1 = variable_declarator env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let _v1 = token env v1 (* "," *) in
      let v2 = variable_declarator env v2 in
      v2
    ) v2
  in
  v1::v2

and init_extra env = function
  | `Exp x -> ExprInit (expression env x)
  | `Array_init x -> ArrayInit (array_initializer env x)


and variable_declarator (env : env) ((v1, v2) : CST.variable_declarator) : var_decl_id * init option =
  let v1 = variable_declarator_id env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let _v1 = token env v1 (* "=" *) in
        let v2 = init_extra env v2 in
        Some v2
    | None -> None)
  in
  v1, v2


and variable_declarator_id (env : env) ((v1, v2) : CST.variable_declarator_id)
 : var_decl_id =
  let v1 = id_extra env v1 in
  let v2 =
    (match v2 with
    | Some x -> dimensions env x
    | None -> [])
  in
  List.fold_left (fun acc _e -> ArrayDecl acc) (IdentDecl v1) v2

and array_initializer (env : env) ((v1, v2, v3, v4) : CST.array_initializer) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = init_extra env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let _v1 = token env v1 (* "," *) in
            let v2 = init_extra env v2 in
            v2
          ) v2
        in
        v1::v2
    | None -> [])
  in
  let _v3_trailing =
    (match v3 with
    | Some tok -> Some (token env tok) (* "," *)
    | None -> None)
  in
  let v4 = token env v4 (* "}" *) in
  v1, v2, v4


and type_ (env : env) (x : CST.type_) : typ =
  (match x with
  | `Type_unan_type x -> unannotated_type env x
  | `Type_anno_type (v1, v2) ->
      let _v1 = List.map (annotation env) v1 in
      let v2 = unannotated_type env v2 in
      v2
  )


and unannotated_type (env : env) (x : CST.unannotated_type) : typ =
  (match x with
  | `Unan_type_choice_void_type x ->
        basic_type_extra env x
  | `Unan_type_array_type (v1, v2) ->
      let v1 = unannotated_type env v1 in
      let v2 = dimensions env v2 in
      List.fold_left (fun acc _e -> TArray acc) v1 v2
  )


and scoped_type_identifier (env : env) ((v1, v2, v3, v4) : CST.scoped_type_identifier) : class_type =
  let v1 =
    (match v1 with
    | `Id tok ->
          let id = str env tok (* pattern [a-zA-Z_]\w* *) in
          [id, []]
    | `Scop_type_id x -> scoped_type_identifier env x
    | `Gene_type x -> generic_type env x
    )
  in
  let _v2 = token env v2 (* "." *) in
  let _v3 = List.map (annotation env) v3 in
  let v4 = identifier env v4 (* pattern [a-zA-Z_]\w* *) in

  v1 @ [v4, []]


and generic_type (env : env) ((v1, v2) : CST.generic_type) : class_type =
  let v1 =
    (match v1 with
    | `Id tok ->
          let id = str env tok (* pattern [a-zA-Z_]\w* *) in
          [id, []]
    | `Scop_type_id x -> scoped_type_identifier env x
    )
  in
  let v2 = type_arguments env v2 in
  (match List.rev v1 with
  | [] -> raise Impossible
  | (x, [])::xs ->
        List.rev xs @ [x, v2]
  | (_x, _)::_xs ->
        raise Impossible
  )

and method_header (env : env) ((v1, v2, v3, v4) : CST.method_header) =
  let v1 =
    (match v1 with
    | Some (v1, v2) ->
        let v1 = type_parameters env v1 in
        let _v2 = List.map (annotation env) v2 in
        v1
    | None -> [])
  in
  let v2 = unannotated_type env v2 in
  let v3 = method_declarator env v3 in
  let v4 =
    (match v4 with
    | Some x -> throws env x
    | None -> [])
  in
  let (id, params, dims) = v3 in
  let t = List.fold_left (fun acc _e -> TArray acc) v2 dims in
  v1, t, id, params, v4


and method_declarator (env : env) ((v1, v2, v3) : CST.method_declarator) =
  let v1 = id_extra env v1 in
  let v2 = formal_parameters env v2 in
  let v3 =
    (match v3 with
    | Some x -> dimensions env x
    | None -> [])
  in
  v1, v2, v3


and formal_parameters (env : env) ((v1, v2, v3, v4) : CST.formal_parameters) =
  let _v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | Some x -> [receiver_parameter env x]
    | None -> [])
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) ->
        let v1 =
          (match v1 with
          | `Form_param x -> formal_parameter env x
          | `Spre_param x -> spread_parameter env x
          )
        in
        let v2 =
          List.map (fun (v1, v2) ->
            let _v1 = token env v1 (* "," *) in
            let v2 =
              (match v2 with
              | `Form_param x -> formal_parameter env x
              | `Spre_param x -> spread_parameter env x
              )
            in
            v2
          ) v2
        in
        v1::v2
    | None -> [])
  in
  let _v4 = token env v4 (* ")" *) in
  v2 @ v3


and formal_parameter (env : env) ((v1, v2, v3) : CST.formal_parameter) =
  let v1 =
    (match v1 with
    | Some x -> modifiers env x
    | None -> [])
  in
  let v2 = unannotated_type env v2 in
  let v3 = variable_declarator_id env v3 in
  ParamClassic (AST.canon_var v1 (Some v2) v3)


and receiver_parameter (env : env) ((v1, v2, v3, v4) : CST.receiver_parameter) =
  let _v1 = List.map (annotation env) v1 in
  let v2 = unannotated_type env v2 in
  let _v3 =
    (match v3 with
    | Some (v1, v2) ->
        (* TODO *)
        let _v1 = token env v1 (* pattern [a-zA-Z_]\w* *) in
        let _v2 = token env v2 (* "." *) in
        ()

    | None -> ())
  in
  let v4 = this_id_field env v4 (* "this" *) in
  ParamReceiver (AST.canon_var [] (Some v2) (IdentDecl v4))


and spread_parameter (env : env) ((v1, v2, v3, v4) : CST.spread_parameter) =
  let v1 =
    (match v1 with
    | Some x -> modifiers env x
    | None -> [])
  in
  let v2 = unannotated_type env v2 in
  let v3 = token env v3 (* "..." *) in
  let v4 = variable_declarator env v4 in
  let (vdef, _init_optTODO) = v4 in
  ParamSpread (v3, AST.canon_var v1 (Some v2) vdef)


and throws (env : env) ((v1, v2, v3) : CST.throws) : typ list =
  let _v1 = token env v1 (* "throws" *) in
  let v2 = type_ env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let _v1 = token env v1 (* "," *) in
      let v2 = type_ env v2 in
      v2
    ) v3
  in
  v2::v3


and local_variable_declaration (env : env) ((v1, v2, v3, v4) : CST.local_variable_declaration) : var_with_init list =
  let v1 =
    (match v1 with
    | Some x -> modifiers env x
    | None -> [])
  in
  let v2 = unannotated_type env v2 in
  let v3 = variable_declarator_list env v3 in
  let _v4 = token env v4 (* ";" *) in
  decls (fun x -> x) v1 v2 v3


and method_declaration (env : env) ((v1, v2, v3) : CST.method_declaration) =
  let v1 =
    (match v1 with
    | Some x -> modifiers env x
    | None -> [])
  in
  let v2 = method_header env v2 in
  let v3 =
    (match v3 with
    | `Blk x -> block env x
    | `SEMI tok -> EmptyStmt (token env tok) (* ";" *)
    )
  in
  let (_tparams, t, id, params, throws) = v2 in
  { (AST.method_header v1 t (IdentDecl id, params) throws)
    with m_body = v3 }



let program (env : env) (xs : CST.program) =
  List.map (statement env) xs

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse file =
  let ast =
    Parallel.backtrace_when_exn := false;
    Parallel.invoke Tree_sitter_java.Parse.file file ()
  in
  let env = { H.file; conv = H.line_col_to_pos file } in
  let _x = program env ast in
  raise Todo
