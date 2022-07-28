(* Yoann Padioleau
 *
 * Copyright (c) 2022 R2C
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
module CST = Tree_sitter_julia.CST
module H = Parse_tree_sitter_helpers
module G = AST_generic
open AST_generic
module H2 = AST_generic_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Julia parser using tree-sitter-lang/semgrep-julia and converting
 * directly to AST_generic.ml
 *
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type env = unit H.env

let token = H.token
let str = H.str
(*
let fb = G.fake_bracket
let sc tok = PI.sc tok
 *)

let todo (_env : env) _ = failwith "not implemented"
let stmts_of_exprs xs = xs |> Common.map G.exprstmt

let map_trailing_comma env v =
  match v with
  | Some tok -> Some ((* "," *) token env tok)
  | None -> None

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)

(* This was started by copying tree-sitter-lang/semgrep-julia/Boilerplate.ml *)

(**
   Boilerplate to be used as a template when mapping the julia CST
   to another type of tree.
*)

let map_pat_684220d (env : env) (tok : CST.pat_684220d) =
  (* pattern "[^'\\\\]" *) token env tok

let map_pat_a25c544 (env : env) (tok : CST.pat_a25c544) =
  (* pattern [0-9]|([0-9][0-9_]*[0-9]) *) token env tok

let map_tok_0b_pat_1c3450e (env : env) (tok : CST.tok_0b_pat_1c3450e) =
  (* tok_0b_pat_1c3450e *) token env tok

let map_imm_tok_lbrack (env : env) (tok : CST.imm_tok_lbrack) =
  (* "[" *) token env tok

let map_identifier (env : env) (tok : CST.identifier) : ident =
  (* identifier *)
  str env tok

let map_tok_0x_pat_50ed65e (env : env) (tok : CST.tok_0x_pat_50ed65e) =
  (* tok_0x_pat_50ed65e *) token env tok

let map_tok_0o_pat_c83427c (env : env) (tok : CST.tok_0o_pat_c83427c) =
  (* tok_0o_pat_c83427c *) token env tok

let map_terminator (env : env) (x : CST.terminator) : tok =
  match x with
  | `LF tok -> (* "\n" *) token env tok
  | `SEMI tok -> (* ";" *) token env tok

let map_terminator_opt env v =
  match v with
  | Some x -> Some (map_terminator env x)
  | None -> None

let map_imm_tok_colon (env : env) (tok : CST.imm_tok_colon) =
  (* ":" *) token env tok

let rec map_anon_choice_id_f1f5a37 (env : env) (x : CST.anon_choice_id_f1f5a37)
    : name =
  match x with
  | `Id tok ->
      let id = map_identifier env tok in
      todo env id
  | `Scoped_id x -> map_scoped_identifier env x

and map_scoped_identifier (env : env) ((v1, v2, v3) : CST.scoped_identifier) =
  let v1 =
    match v1 with
    | Some x -> map_anon_choice_id_f1f5a37 env x
    | None -> todo env ()
  in
  let v2 = (* "." *) token env v2 in
  let v3 = map_identifier env v3 in
  todo env (v1, v2, v3)

let map_anon_choice_str_content_no_interp_24ac4f9 (env : env)
    (x : CST.anon_choice_str_content_no_interp_24ac4f9) : string wrap =
  match x with
  | `Str_content_no_interp tok -> (* string_content_no_interp *) str env tok
  | `Esc_seq tok -> (* escape_sequence *) str env tok

let map_integer_literal (env : env) (x : CST.integer_literal) =
  match x with
  | `Tok_0b_pat_1c3450e x -> map_tok_0b_pat_1c3450e env x
  | `Tok_0o_pat_c83427c x -> map_tok_0o_pat_c83427c env x
  | `Tok_0x_pat_50ed65e x -> map_tok_0x_pat_50ed65e env x
  | `Pat_a25c544 x -> map_pat_a25c544 env x

let map_operator (env : env) (x : CST.operator) : ident =
  match x with
  | `Comp_op tok -> (* comparison_operator *) str env tok
  | `Dotty_op tok -> (* dotty_operator *) str env tok
  | `Plus_op tok -> (* plus_operator *) str env tok
  | `Times_op tok -> (* times_operator *) str env tok
  | `Rati_op tok -> (* rational_operator *) str env tok
  | `Bits_op tok -> (* bitshift_operator *) str env tok
  | `Power_op tok -> (* power_operator *) str env tok
  | `Un_op tok -> (* unary_operator *) str env tok

let map_macro_identifier (env : env) ((v1, v2) : CST.macro_identifier) =
  let v1 = (* "@" *) token env v1 in
  let v2 =
    match v2 with
    | `Id tok ->
        let id = map_identifier env tok in
        todo env id
    | `Op x ->
        let id = map_operator env x in
        todo env id
    | `DOT tok ->
        let tdot = (* "." *) token env tok in
        todo env tdot
  in
  todo env (v1, v2)

let map_anon_choice_id_8e9135e (env : env) (x : CST.anon_choice_id_8e9135e) =
  match x with
  | `Id tok -> map_identifier env tok
  | `Macro_id x -> map_macro_identifier env x

let map_anon_choice_id_1ca53ff (env : env) (x : CST.anon_choice_id_1ca53ff) =
  match x with
  | `Id tok ->
      let id = map_identifier env tok in
      todo env id
  | `Scoped_id x -> map_scoped_identifier env x
  | `Sele_import (v1, v2, v3, v4) ->
      let v1 = map_anon_choice_id_f1f5a37 env v1 in
      let v2 = map_imm_tok_colon env v2 in
      let v3 = map_anon_choice_id_8e9135e env v3 in
      let v4 =
        Common.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_id_8e9135e env v2 in
            v2)
          v4
      in
      todo env (v1, v2, v3, v4)

let rec map_anon_choice_arg_list_38b50f0 (env : env)
    (x : CST.anon_choice_arg_list_38b50f0) : argument list bracket =
  match x with
  | `Arg_list x -> map_argument_list env x
  | `Gene_exp x -> map_generator_expression env x

and map_anon_choice_exp_44a61a9 (env : env) (x : CST.anon_choice_exp_44a61a9) :
    expr =
  match x with
  | `Exp x -> map_expression env x
  | `Assign_exp x -> map_assignment_expression env x

and map_anon_choice_exp_91c2553 (env : env) (x : CST.anon_choice_exp_91c2553) :
    argument =
  match x with
  | `Exp x ->
      let e = map_expression env x in
      Arg e
  | `Named_field x ->
      let fld = map_named_field env x in
      todo env fld

and map_anon_choice_exp_b71eb95 (env : env) (x : CST.anon_choice_exp_b71eb95) :
    expr =
  match x with
  | `Exp x -> map_expression env x
  | `Assign_exp x -> map_assignment_expression env x
  | `Bare_tuple_exp x -> map_bare_tuple_expression env x

and map_anon_choice_exp_d189641 (env : env) (x : CST.anon_choice_exp_d189641) :
    expr =
  match x with
  | `Exp x -> map_expression env x
  | `Bare_tuple_exp x -> map_bare_tuple_expression env x

and map_anon_choice_id_0836987 (env : env) (x : CST.anon_choice_id_0836987) =
  match x with
  | `Id tok ->
      let id = map_identifier env tok in
      todo env id
  | `Spread_param (v1, v2) ->
      let v1 = map_identifier env v1 in
      let v2 = (* "..." *) token env v2 in
      todo env (v1, v2)
  | `Opt_param (v1, v2, v3) ->
      let v1 =
        match v1 with
        | `Id tok ->
            let id = map_identifier env tok in
            todo env id
        | `Typed_param x -> map_typed_parameter env x
      in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Typed_param x -> map_typed_parameter env x

and map_anon_choice_id_b4fe4b3 (env : env) (x : CST.anon_choice_id_b4fe4b3) =
  match x with
  | `Id tok ->
      let id = map_identifier env tok in
      todo env id
  | `Para_id x -> map_parameterized_identifier env x

and map_anon_choice_id_e9e133c (env : env) (x : CST.anon_choice_id_e9e133c) =
  match x with
  | `Id tok ->
      let id = map_identifier env tok in
      todo env id
  | `Cons_param (v1, v2, v3) ->
      let v1 = map_identifier env v1 in
      let v2 = (* "<:" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)

and map_anon_choice_str_content_838a78d (env : env)
    (x : CST.anon_choice_str_content_838a78d) =
  match x with
  | `Str_content tok -> Left3 ((* string_content *) str env tok)
  | `Str_interp (v1, v2) ->
      let v1 = (* "$" *) token env v1 in
      let v2 =
        match v2 with
        | `Id tok ->
            let id = map_identifier env tok in
            todo env id
        | `LPAR_exp_RPAR (v1, v2, v3) ->
            let v1 = (* "(" *) token env v1 in
            let v2 = map_expression env v2 in
            let v3 = (* ")" *) token env v3 in
            todo env (v1, v2, v3)
      in
      todo env (v1, v2)
  | `Esc_seq tok -> Left3 ((* escape_sequence *) str env tok)

and map_anon_exp_rep_COMMA_exp_0bb260c (env : env)
    ((v1, v2) : CST.anon_exp_rep_COMMA_exp_0bb260c) : expr list =
  let v1 = map_expression env v1 in
  let v2 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_expression env v2 in
        v2)
      v2
  in
  v1 :: v2

and map_argument_list (env : env) ((v1, v2, v3, v4, v5) : CST.argument_list) :
    argument list bracket =
  let l = (* "(" *) token env v1 in
  let args =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_exp_91c2553 env v1 in
        let v2 =
          Common.map
            (fun (v1, v2) ->
              let _v1 = (* "," *) token env v1 in
              let v2 = map_anon_choice_exp_91c2553 env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let kwdargs =
    match v3 with
    | Some (v1, v2, v3) ->
        let _v1 = (* ";" *) token env v1 in
        let v2 = map_named_field env v2 in
        let v3 =
          Common.map
            (fun (v1, v2) ->
              let _v1 = (* "," *) token env v1 in
              let v2 = map_named_field env v2 in
              v2)
            v3
        in
        v2 :: v3
    | None -> []
  in
  let _v4 = map_trailing_comma env v4 in
  let r = (* ")" *) token env v5 in
  (l, args @ kwdargs, r)

and map_assignment_expression (env : env)
    ((v1, v2, v3) : CST.assignment_expression) : expr =
  let v1 = map_anon_choice_exp_d189641 env v1 in
  let v2 =
    match v2 with
    | `Assign_op tok -> (* assign_operator *) token env tok
    | `EQ tok -> (* "=" *) token env tok
  in
  let v3 = map_anon_choice_exp_b71eb95 env v3 in
  todo env (v1, v2, v3)

and map_bare_tuple_expression (env : env) ((v1, v2) : CST.bare_tuple_expression)
    : expr =
  let v1 = map_expression env v1 in
  let v2 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_expression env v2 in
        v2)
      v2
  in
  todo env (v1, v2)

and map_binary_expression (env : env) (x : CST.binary_expression) : expr =
  match x with
  | `Exp_power_op_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* power_operator *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_rati_op_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* rational_operator *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_bits_op_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* bitshift_operator *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_times_op_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* times_operator *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_choice_PLUS_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `PLUS tok -> (* "+" *) token env tok
        | `Plus_op tok -> (* plus_operator *) token env tok
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_dotty_op_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* dotty_operator *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_arrow_op_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* arrow_operator *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_LTBAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<|" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BARGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "|>" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_choice_in_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `In tok -> (* "in" *) token env tok
        | `Isa tok -> (* "isa" *) token env tok
        | `Comp_op tok -> (* comparison_operator *) token env tok
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)

and map_catch_clause (env : env) ((v1, v2, v3, v4) : CST.catch_clause) =
  let v1 = (* "catch" *) token env v1 in
  let v2 =
    match v2 with
    | Some tok ->
        let id = map_identifier env tok in
        todo env id
    | None -> todo env ()
  in
  let v3 = map_terminator_opt env v3 in
  let v4 = map_source_file env v4 in
  todo env (v1, v2, v3, v4)

and map_comprehension_clause (env : env) ((v1, v2) : CST.comprehension_clause) =
  let v1 = map_for_clause env v1 in
  let v2 =
    Common.map
      (fun x ->
        match x with
        | `For_clause x -> map_for_clause env x
        | `If_clause x -> map_if_clause env x)
      v2
  in
  todo env (v1, v2)

and map_definition (env : env) (x : CST.definition) : definition =
  match x with
  | `Abst_defi (v1, v2, v3, v4, v5, v6) ->
      let v1 = (* "abstract" *) token env v1 in
      let v2 = (* "type" *) token env v2 in
      let v3 = map_identifier env v3 in
      let v4 =
        match v4 with
        | Some x -> map_type_parameter_list env x
        | None -> todo env ()
      in
      let v5 =
        match v5 with
        | Some x -> map_subtype_clause env x
        | None -> todo env ()
      in
      let v6 = (* "end" *) token env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Prim_defi (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = (* "primitive" *) token env v1 in
      let v2 = (* "type" *) token env v2 in
      let v3 = map_identifier env v3 in
      let v4 =
        match v4 with
        | Some x -> map_type_parameter_list env x
        | None -> todo env ()
      in
      let v5 =
        match v5 with
        | Some x -> map_subtype_clause env x
        | None -> todo env ()
      in
      let v6 = map_pat_a25c544 env v6 in
      let v7 = (* "end" *) token env v7 in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `Struct_defi (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 =
        match v1 with
        | Some tok -> (* "mutable" *) token env tok
        | None -> todo env ()
      in
      let v2 = (* "struct" *) token env v2 in
      let v3 = map_identifier env v3 in
      let v4 =
        match v4 with
        | Some x -> map_type_parameter_list env x
        | None -> todo env ()
      in
      let v5 =
        match v5 with
        | Some x -> map_subtype_clause env x
        | None -> todo env ()
      in
      let v6 = map_source_file env v6 in
      let v7 = (* "end" *) token env v7 in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `Module_defi (v1, v2, v3, v4) ->
      let v1 = (* "module" *) token env v1 in
      let v2 = map_identifier env v2 in
      let v3 = map_source_file env v3 in
      let v4 = (* "end" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Func_defi (v1, v2, v3, v4, v5, v6) ->
      let v1 = (* "function" *) token env v1 in
      let v2 = map_identifier env v2 in
      let v3 =
        match v3 with
        | Some x -> map_type_parameter_list env x
        | None -> todo env ()
      in
      let v4 = map_parameter_list env v4 in
      let v5 = map_source_file env v5 in
      let v6 = (* "end" *) token env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Macro_defi (v1, v2, v3, v4, v5) ->
      let v1 = (* "macro" *) token env v1 in
      let v2 =
        match v2 with
        | `Id tok ->
            let id = map_identifier env tok in
            todo env id
        | `Op x -> map_operator env x
      in
      let v3 = map_parameter_list env v3 in
      let v4 = map_source_file env v4 in
      let v5 = (* "end" *) token env v5 in
      todo env (v1, v2, v3, v4, v5)

and map_do_clause (env : env) ((v1, v2, v3) : CST.do_clause) =
  let v1 = (* "do" *) token env v1 in
  let v2 = map_expression_list env v2 in
  let v3 = (* "end" *) token env v3 in
  todo env (v1, v2, v3)

and map_else_clause (env : env) ((v1, v2) : CST.else_clause) =
  let v1 = (* "else" *) token env v1 in
  let v2 = map_source_file env v2 in
  todo env (v1, v2)

and map_elseif_clause (env : env) ((v1, v2, v3, v4) : CST.elseif_clause) =
  let v1 = (* "elseif" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = map_terminator_opt env v3 in
  let v4 = map_source_file env v4 in
  todo env (v1, v2, v3, v4)

and map_expression (env : env) (x : CST.expression) : expr =
  match x with
  | `Choice_if_stmt x ->
      let st = map_statement env x in
      todo env st
  | `Choice_abst_defi x ->
      let def = map_definition env x in
      todo env def
  | `Typed_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `COLONCOLON tok -> (* "::" *) token env tok
        | `LTCOLON tok -> (* "<:" *) token env tok
      in
      let v3 = map_anon_choice_id_b4fe4b3 env v3 in
      todo env (v1, v2, v3)
  | `Comp_exp (v1, v2, v3) ->
      let v1 = (* "begin" *) token env v1 in
      let v2 = map_expression_list env v2 in
      let v3 = (* "end" *) token env v3 in
      todo env (v1, v2, v3)
  | `Pair_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "=>" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `COLON tok ->
      let t = (* ":" *) token env tok in
      todo env t
  | `Macro_exp (v1, v2) ->
      let v1 = map_macro_identifier env v1 in
      let v2 =
        match v2 with
        | Some x -> (
            match x with
            | `Imme_paren_arg_list (v1, v2) ->
                let v1 = (* immediate_paren *) token env v1 in
                let v2 = map_argument_list env v2 in
                todo env (v1, v2)
            | `Macro_arg_list x -> map_macro_argument_list env x)
        | None -> todo env ()
      in
      todo env (v1, v2)
  | `Un_exp x -> map_unary_expression env x
  | `Bin_exp x -> map_binary_expression env x
  | `Tern_exp (v1, v2, v3, v4, v5) ->
      let v1 = map_expression env v1 in
      let v2 = (* "?" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ":" *) token env v4 in
      let v5 = map_expression env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Gene_exp x ->
      let e = map_generator_expression env x in
      todo env e
  | `Func_exp x ->
      let fdef = map_function_expression env x in
      todo env fdef
  | `Coef_exp (v1, v2) ->
      let v1 =
        match v1 with
        | `Pat_a25c544 x -> map_pat_a25c544 env x
        | `Float_lit tok -> (* float_literal *) token env tok
      in
      let v2 =
        match v2 with
        | `Paren_exp x -> map_parenthesized_expression env x
        | `Id tok ->
            let id = map_identifier env tok in
            todo env id
      in
      todo env (v1, v2)
  | `Spread_exp x -> map_spread_expression env x
  | `Range_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Quote_exp (v1, v2) ->
      let v1 = (* ":" *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Interp_exp (v1, v2) ->
      let v1 = (* "$" *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Prim_exp x -> map_primary_expression env x
  | `Lit x -> map_literal env x
  | `Op x ->
      let op = map_operator env x in
      todo env op

and map_expression_list (env : env) ((v1, v2, v3) : CST.expression_list) :
    expr list =
  let v1 = map_anon_choice_exp_b71eb95 env v1 in
  let v2 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = map_terminator env v1 in
        let v2 = map_anon_choice_exp_b71eb95 env v2 in
        v2)
      v2
  in
  let _v3 = map_terminator_opt env v3 in
  v1 :: v2

and map_field_expression (env : env) ((v1, v2, v3) : CST.field_expression) :
    expr =
  let v1 = map_primary_expression env v1 in
  let v2 = (* "." *) token env v2 in
  let v3 = map_identifier env v3 in
  todo env (v1, v2, v3)

and map_finally_clause (env : env) ((v1, v2, v3) : CST.finally_clause) =
  let v1 = (* "finally" *) token env v1 in
  let v2 = map_terminator_opt env v2 in
  let v3 = map_source_file env v3 in
  todo env (v1, v2, v3)

and map_for_binding (env : env) ((v1, v2, v3) : CST.for_binding) =
  let v1 =
    match v1 with
    | `Id tok ->
        let id = map_identifier env tok in
        todo env id
    | `Tuple_exp x -> map_tuple_expression env x
  in
  let v2 =
    match v2 with
    | `In tok -> (* "in" *) token env tok
    | `EQ tok -> (* "=" *) token env tok
    | `UNKUNKUNK tok -> (* "\226\136\136" *) token env tok
  in
  let v3 = map_expression env v3 in
  todo env (v1, v2, v3)

and map_for_clause (env : env) ((v1, v2, v3) : CST.for_clause) =
  let v1 = (* "for" *) token env v1 in
  let v2 = map_for_binding env v2 in
  let v3 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_for_binding env v2 in
        v2)
      v3
  in
  todo env (v1, v2, v3)

and map_function_expression (env : env) (x : CST.function_expression) :
    function_definition =
  match x with
  | `Func_param_list_choice_exp_end (v1, v2, v3, v4) ->
      let v1 = (* "function" *) token env v1 in
      let v2 = map_parameter_list env v2 in
      let v3 = map_anon_choice_exp_44a61a9 env v3 in
      let v4 = (* "end" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Choice_id_DASHGT_choice_exp (v1, v2, v3) ->
      let v1 =
        match v1 with
        | `Id tok ->
            let id = map_identifier env tok in
            todo env id
        | `Param_list x -> map_parameter_list env x
      in
      let v2 = (* "->" *) token env v2 in
      let v3 = map_anon_choice_exp_44a61a9 env v3 in
      todo env (v1, v2, v3)

and map_generator_expression (env : env)
    ((v1, v2, v3, v4) : CST.generator_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = map_comprehension_clause env v3 in
  let v4 = (* ")" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_if_clause (env : env) ((v1, v2) : CST.if_clause) =
  let v1 = (* "if" *) token env v1 in
  let v2 = map_expression env v2 in
  todo env (v1, v2)

and map_keyword_parameters (env : env) ((v1, v2, v3) : CST.keyword_parameters) =
  let v1 = (* ";" *) token env v1 in
  let v2 = map_anon_choice_id_0836987 env v2 in
  let v3 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_anon_choice_id_0836987 env v2 in
        v2)
      v3
  in
  todo env (v1, v2, v3)

and map_literal (env : env) (x : CST.literal) : expr =
  match x with
  | `Int_lit x ->
      let i = map_integer_literal env x in
      todo env i
  | `Float_lit tok ->
      let f = (* float_literal *) token env tok in
      todo env f
  | `Char_lit (v1, v2, v3) ->
      let v1 = (* "'" *) token env v1 in
      let v2 =
        match v2 with
        | `Esc_seq tok -> (* escape_sequence *) token env tok
        | `Pat_684220d x -> map_pat_684220d env x
      in
      let v3 = (* "'" *) token env v3 in
      todo env (v1, v2, v3)
  | `Str_lit (v1, v2, v3) ->
      let l = (* string_start *) token env v1 in
      let xs = Common.map (map_anon_choice_str_content_838a78d env) v2 in
      let r = (* string_end *) token env v3 in
      G.interpolated (l, xs, r)
  | `Cmd_lit (v1, v2, v3) ->
      let v1 = (* command_start *) token env v1 in
      let v2 = Common.map (map_anon_choice_str_content_838a78d env) v2 in
      let v3 = (* command_end *) token env v3 in
      todo env (v1, v2, v3)
  | `Pref_str_lit (v1, v2, v3, v4) ->
      let v1 = map_identifier env v1 in
      let v2 = (* immediate_string_start *) token env v2 in
      let v3 =
        Common.map (map_anon_choice_str_content_no_interp_24ac4f9 env) v3
      in
      let v4 = (* string_end *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Pref_cmd_lit (v1, v2, v3, v4) ->
      let v1 = map_identifier env v1 in
      let v2 = (* immediate_command_start *) token env v2 in
      let v3 =
        Common.map (map_anon_choice_str_content_no_interp_24ac4f9 env) v3
      in
      let v4 = (* command_end *) token env v4 in
      todo env (v1, v2, v3, v4)

and map_macro_argument_list (env : env) (xs : CST.macro_argument_list) =
  Common.map (map_expression env) xs

and map_matrix_row (env : env) (xs : CST.matrix_row) =
  Common.map (map_expression env) xs

and map_named_field (env : env) ((v1, v2, v3) : CST.named_field) =
  let v1 = map_identifier env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_expression env v3 in
  todo env (v1, v2, v3)

and map_parameter_list (env : env) ((v1, v2, v3, v4) : CST.parameter_list) :
    parameter list bracket =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_id_0836987 env v1 in
        let v2 =
          Common.map
            (fun (v1, v2) ->
              let _v1 = (* "," *) token env v1 in
              let v2 = map_anon_choice_id_0836987 env v2 in
              v2)
            v2
        in
        todo env (v1, v2)
    | None -> todo env ()
  in
  let v3 =
    match v3 with
    | Some x -> map_keyword_parameters env x
    | None -> todo env ()
  in
  let v4 = (* ")" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_parameterized_identifier (env : env)
    ((v1, v2) : CST.parameterized_identifier) =
  let v1 =
    match v1 with
    | `Id tok ->
        let id = map_identifier env tok in
        todo env id
    | `Field_exp x -> map_field_expression env x
  in
  let v2 = map_type_argument_list env v2 in
  todo env (v1, v2)

and map_parenthesized_expression (env : env)
    ((v1, v2, v3) : CST.parenthesized_expression) : expr =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | `Exp_list x ->
        let xs = map_expression_list env x in
        todo env xs
    | `Spread_exp x -> map_spread_expression env x
  in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

and map_primary_expression (env : env) (x : CST.primary_expression) : expr =
  match x with
  | `Id tok ->
      let id = map_identifier env tok in
      N (H2.name_of_id id) |> G.e
  | `Array_exp (v1, v2, v3, v4) ->
      let v1 = (* "[" *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> map_anon_exp_rep_COMMA_exp_0bb260c env x
        | None -> []
      in
      let v3 = map_trailing_comma env v3 in
      let v4 = (* "]" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Array_comp_exp (v1, v2, v3, v4) ->
      let v1 = (* "[" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = map_comprehension_clause env v3 in
      let v4 = (* "]" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Matrix_exp (v1, v2, v3, v4) ->
      let v1 = (* "[" *) token env v1 in
      let v2 =
        match v2 with
        | Some (v1, v2) ->
            let v1 = map_matrix_row env v1 in
            let v2 =
              Common.map
                (fun (v1, v2) ->
                  let v1 = (* ";" *) token env v1 in
                  let v2 = map_matrix_row env v2 in
                  todo env (v1, v2))
                v2
            in
            todo env (v1, v2)
        | None -> todo env ()
      in
      let v3 =
        match v3 with
        | Some tok -> (* ";" *) token env tok
        | None -> todo env ()
      in
      let v4 = (* "]" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Call_exp (v1, v2, v3, v4) ->
      let e1 =
        match v1 with
        | `Prim_exp x -> map_primary_expression env x
        | `Op x ->
            let op = map_operator env x in
            todo env op
      in
      let _l = (* immediate_paren *) token env v2 in
      let l, args, r = map_anon_choice_arg_list_38b50f0 env v3 in
      let _do_optTODO =
        match v4 with
        | Some x -> Some (map_do_clause env x)
        | None -> None
      in
      Call (e1, (l, args, r)) |> G.e
  | `Field_exp x ->
      let fld = map_field_expression env x in
      todo env fld
  | `Paren_exp x -> map_parenthesized_expression env x
  | `Subs_exp (v1, v2, v3, v4, v5) ->
      let v1 =
        match v1 with
        | `Prim_exp x -> map_primary_expression env x
        | `Lit x -> map_literal env x
      in
      let v2 = map_imm_tok_lbrack env v2 in
      let v3 =
        match v3 with
        | Some x -> map_anon_exp_rep_COMMA_exp_0bb260c env x
        | None -> []
      in
      let v4 = map_trailing_comma env v4 in
      let v5 = (* "]" *) token env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Para_id x ->
      let pid = map_parameterized_identifier env x in
      todo env pid
  | `Tuple_exp x -> map_tuple_expression env x
  | `Broa_call_exp (v1, v2, v3, v4, v5) ->
      let v1 = map_primary_expression env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = (* immediate_paren *) token env v3 in
      let v4 = map_anon_choice_arg_list_38b50f0 env v4 in
      let v5 =
        match v5 with
        | Some x -> map_do_clause env x
        | None -> todo env ()
      in
      todo env (v1, v2, v3, v4, v5)

and map_source_file (env : env) (opt : CST.source_file) : expr list =
  match opt with
  | Some x ->
      let xs = map_expression_list env x in
      xs
  | None -> []

and map_spread_expression (env : env) ((v1, v2) : CST.spread_expression) : expr
    =
  let v1 = map_expression env v1 in
  let v2 = (* "..." *) token env v2 in
  todo env (v1, v2)

and map_statement (env : env) (x : CST.statement) : stmt =
  match x with
  | `If_stmt (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = (* "if" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = map_terminator_opt env v3 in
      let v4 = map_source_file env v4 in
      let v5 = Common.map (map_elseif_clause env) v5 in
      let v6 =
        match v6 with
        | Some x -> map_else_clause env x
        | None -> todo env ()
      in
      let v7 = (* "end" *) token env v7 in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `Try_stmt (v1, v2, v3, v4, v5) ->
      let v1 = (* "try" *) token env v1 in
      let v2 = map_source_file env v2 in
      let v3 =
        match v3 with
        | Some x -> map_catch_clause env x
        | None -> todo env ()
      in
      let v4 =
        match v4 with
        | Some x -> map_finally_clause env x
        | None -> todo env ()
      in
      let v5 = (* "end" *) token env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `For_stmt (v1, v2, v3, v4, v5, v6) ->
      let v1 = (* "for" *) token env v1 in
      let v2 = map_for_binding env v2 in
      let v3 =
        Common.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_for_binding env v2 in
            v2)
          v3
      in
      let v4 = map_terminator_opt env v4 in
      let v5 = map_source_file env v5 in
      let v6 = (* "end" *) token env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `While_stmt (v1, v2, v3, v4, v5) ->
      let v1 = (* "while" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = map_terminator_opt env v3 in
      let v4 = map_source_file env v4 in
      let v5 = (* "end" *) token env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Let_stmt (v1, v2, v3, v4, v5, v6) ->
      let v1 = (* "let" *) token env v1 in
      let v2 = map_variable_declaration env v2 in
      let v3 =
        Common.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_variable_declaration env v2 in
            v2)
          v3
      in
      let v4 = map_terminator_opt env v4 in
      let v5 = map_source_file env v5 in
      let v6 = (* "end" *) token env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Const_stmt (v1, v2, v3) ->
      let v1 = (* "const" *) token env v1 in
      let v2 = map_variable_declaration env v2 in
      let v3 =
        Common.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_variable_declaration env v2 in
            v2)
          v3
      in
      todo env (v1, v2, v3)
  | `Quote_stmt (v1, v2, v3) ->
      let v1 = (* "quote" *) token env v1 in
      let v2 = map_source_file env v2 in
      let v3 = (* "end" *) token env v3 in
      todo env (v1, v2, v3)
  | `Brk_stmt tok ->
      let tbreak = (* "break" *) token env tok in
      todo env tbreak
  | `Cont_stmt tok ->
      let tcont = (* "continue" *) token env tok in
      todo env tcont
  | `Ret_stmt (v1, v2) ->
      let v1 = (* "return" *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> map_anon_choice_exp_d189641 env x
        | None -> todo env ()
      in
      todo env (v1, v2)
  | `Import_stmt (v1, v2, v3) ->
      let v1 =
        match v1 with
        | `Using tok -> (* "using" *) token env tok
        | `Import tok -> (* "import" *) token env tok
      in
      let v2 = map_anon_choice_id_1ca53ff env v2 in
      let v3 =
        Common.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_id_1ca53ff env v2 in
            v2)
          v3
      in
      todo env (v1, v2, v3)
  | `Export_stmt (v1, v2, v3) ->
      let v1 = (* "export" *) token env v1 in
      let v2 = map_identifier env v2 in
      let v3 =
        Common.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_identifier env v2 in
            v2)
          v3
      in
      todo env (v1, v2, v3)

and map_subtype_clause (env : env) ((v1, v2) : CST.subtype_clause) =
  let v1 = (* "<:" *) token env v1 in
  let v2 = map_expression env v2 in
  todo env (v1, v2)

and map_tuple_expression (env : env) ((v1, v2, v3) : CST.tuple_expression) :
    expr =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | `Opt_COMMA opt -> (
        match opt with
        | Some tok -> (* "," *) token env tok
        | None -> todo env ())
    | `Choice_exp_COMMA (v1, v2) ->
        let v1 = map_anon_choice_exp_91c2553 env v1 in
        let v2 = (* "," *) token env v2 in
        todo env (v1, v2)
    | `Choice_exp_rep1_COMMA_choice_exp_opt_COMMA (v1, v2, v3) ->
        let v1 = map_anon_choice_exp_91c2553 env v1 in
        let v2 =
          Common.map
            (fun (v1, v2) ->
              let _v1 = (* "," *) token env v1 in
              let v2 = map_anon_choice_exp_91c2553 env v2 in
              v2)
            v2
        in
        let v3 = map_trailing_comma env v3 in
        todo env (v1, v2, v3)
  in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

and map_type_argument_list (env : env)
    ((v1, v2, v3, v4) : CST.type_argument_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    match v2 with
    | `Exp x -> map_expression env x
  in
  let v3 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 =
          match v2 with
          | `Exp x -> map_expression env x
        in
        v2)
      v3
  in
  let v4 = (* "}" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_type_parameter_list (env : env)
    ((v1, v2, v3, v4) : CST.type_parameter_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 = map_anon_choice_id_e9e133c env v2 in
  let v3 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_anon_choice_id_e9e133c env v2 in
        v2)
      v3
  in
  let v4 = (* "}" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_typed_parameter (env : env) ((v1, v2, v3) : CST.typed_parameter) =
  let v1 = map_identifier env v1 in
  let v2 = (* "::" *) token env v2 in
  let v3 = map_anon_choice_id_b4fe4b3 env v3 in
  todo env (v1, v2, v3)

and map_unary_expression (env : env) (x : CST.unary_expression) : expr =
  match x with
  | `Un_op_exp (v1, v2) ->
      let v1 = (* unary_operator *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Exp_SQUOT (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = (* "'" *) token env v2 in
      todo env (v1, v2)

and map_variable_declaration (env : env) ((v1, v2) : CST.variable_declaration) =
  let v1 = map_identifier env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = (* "=" *) token env v1 in
        let v2 = map_expression env v2 in
        todo env (v1, v2)
    | None -> todo env ()
  in
  todo env (v1, v2)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_julia.Parse.file file)
    (fun cst ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      try
        let xs = map_source_file env cst in
        stmts_of_exprs xs
      with
      (* TODO: to delete once todo() has been removed *)
      | Failure "not implemented" as exn ->
          H.debug_sexp_cst_after_error (CST.sexp_of_source_file cst);
          raise exn)

let parse_pattern str =
  H.wrap_parser
    (fun () -> Tree_sitter_julia.Parse.string str)
    (fun cst ->
      let file = "<pattern>" in
      let env = { H.file; conv = Hashtbl.create 0; extra = () } in
      match map_source_file env cst with
      | [ e ] -> G.E e
      | es -> G.Ss (stmts_of_exprs es))
