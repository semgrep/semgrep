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
module CST = Tree_sitter_jsonnet.CST
module H = Parse_tree_sitter_helpers
module PI = Parse_info
open AST_generic
module G = AST_generic
module H2 = AST_generic_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Jsonnet parser using tree-sitter-lang/semgrep-jsonnet and converting
 * directly to AST_generic.ml
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type env = unit H.env

(**
   Boilerplate to be used as a template when mapping the jsonnet CST
   to another type of tree.
*)

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

(* TODO: remove at some point *)
[@@@warning "-32"]

let token (env : env) (_tok : Tree_sitter_run.Token.t) =
  failwith "not implemented"

let todo (env : env) _ = failwith "not implemented"
let map_number (env : env) (tok : CST.number) = (* number *) token env tok

let map_id (env : env) (tok : CST.id) =
  (* pattern [_a-zA-Z][_a-zA-Z0-9]* *) token env tok

let map_binaryop (env : env) (x : CST.binaryop) =
  match x with
  | `STAR tok -> (* "*" *) token env tok
  | `SLASH tok -> (* "/" *) token env tok
  | `PERC tok -> (* "%" *) token env tok
  | `PLUS tok -> (* "+" *) token env tok
  | `DASH tok -> (* "-" *) token env tok
  | `LTLT tok -> (* "<<" *) token env tok
  | `GTGT tok -> (* ">>" *) token env tok
  | `LT tok -> (* "<" *) token env tok
  | `LTEQ tok -> (* "<=" *) token env tok
  | `GT tok -> (* ">" *) token env tok
  | `GTEQ tok -> (* ">=" *) token env tok
  | `EQEQ tok -> (* "==" *) token env tok
  | `BANGEQ tok -> (* "!=" *) token env tok
  | `AMP tok -> (* "&" *) token env tok
  | `HAT tok -> (* "^" *) token env tok
  | `BAR tok -> (* "|" *) token env tok
  | `AMPAMP tok -> (* "&&" *) token env tok
  | `BARBAR tok -> (* "||" *) token env tok

let map_string_start (env : env) (tok : CST.string_start) =
  (* string_start *) token env tok

let map_string_content (env : env) (tok : CST.string_content) =
  (* string_content *) token env tok

let map_unaryop (env : env) (x : CST.unaryop) =
  match x with
  | `DASH tok -> (* "-" *) token env tok
  | `PLUS tok -> (* "+" *) token env tok
  | `BANG tok -> (* "!" *) token env tok
  | `TILDE tok -> (* "~" *) token env tok

let map_imm_tok_prec_p1_pat_59587ce (env : env)
    (tok : CST.imm_tok_prec_p1_pat_59587ce) =
  (* pattern "[^\\\\'\\n]+" *) token env tok

let map_string_end (env : env) (tok : CST.string_end) =
  (* string_end *) token env tok

let map_escape_sequence (env : env) (tok : CST.escape_sequence) =
  (* escape_sequence *) token env tok

let map_imm_tok_prec_p1_pat_c7f65b4 (env : env)
    (tok : CST.imm_tok_prec_p1_pat_c7f65b4) =
  (* pattern "[^\\\\\"\\n]+" *) token env tok

let map_h (env : env) (x : CST.h) =
  match x with
  | `COLON tok -> (* ":" *) token env tok
  | `COLONCOLON tok -> (* "::" *) token env tok
  | `COLONCOLONCOLON tok -> (* ":::" *) token env tok

let map_str_single (env : env) (xs : CST.str_single) =
  Common.map
    (fun x ->
      match x with
      | `Imm_tok_prec_p1_pat_59587ce x -> map_imm_tok_prec_p1_pat_59587ce env x
      | `Esc_seq tok -> (* escape_sequence *) token env tok)
    xs

let map_str_double (env : env) (xs : CST.str_double) =
  Common.map
    (fun x ->
      match x with
      | `Imm_tok_prec_p1_pat_c7f65b4 x -> map_imm_tok_prec_p1_pat_c7f65b4 env x
      | `Esc_seq tok -> (* escape_sequence *) token env tok)
    xs

let map_string_ (env : env) (x : CST.string_) =
  match x with
  | `Opt_AT_single_single (v1, v2, v3) ->
      let v1 =
        match v1 with
        | Some tok -> (* "@" *) token env tok
        | None -> todo env ()
      in
      let v2 = (* "'" *) token env v2 in
      let v3 = (* "'" *) token env v3 in
      todo env (v1, v2, v3)
  | `Opt_AT_single_str_single_single (v1, v2, v3, v4) ->
      let v1 =
        match v1 with
        | Some tok -> (* "@" *) token env tok
        | None -> todo env ()
      in
      let v2 = (* "'" *) token env v2 in
      let v3 = map_str_single env v3 in
      let v4 = (* "'" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Opt_AT_double_double (v1, v2, v3) ->
      let v1 =
        match v1 with
        | Some tok -> (* "@" *) token env tok
        | None -> todo env ()
      in
      let v2 = (* "\"" *) token env v2 in
      let v3 = (* "\"" *) token env v3 in
      todo env (v1, v2, v3)
  | `Opt_AT_double_str_double_double (v1, v2, v3, v4) ->
      let v1 =
        match v1 with
        | Some tok -> (* "@" *) token env tok
        | None -> todo env ()
      in
      let v2 = (* "\"" *) token env v2 in
      let v3 = map_str_double env v3 in
      let v4 = (* "\"" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Opt_AT_str_start_str_content_str_end (v1, v2, v3, v4) ->
      let v1 =
        match v1 with
        | Some tok -> (* "@" *) token env tok
        | None -> todo env ()
      in
      let v2 = (* string_start *) token env v2 in
      let v3 = (* string_content *) token env v3 in
      let v4 = (* string_end *) token env v4 in
      todo env (v1, v2, v3, v4)

let rec map_args (env : env) (x : CST.args) =
  match x with
  | `Expr_opt_COMMA_expr_opt_COMMA_named_arg_opt_COMMA (v1, v2, v3, v4) ->
      let v1 = map_document env v1 in
      let v2 =
        match v2 with
        | Some (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_document env v2 in
            todo env (v1, v2)
        | None -> todo env ()
      in
      let v3 =
        match v3 with
        | Some (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_named_argument env v2 in
            todo env (v1, v2)
        | None -> todo env ()
      in
      let v4 =
        match v4 with
        | Some tok -> (* "," *) token env tok
        | None -> todo env ()
      in
      todo env (v1, v2, v3, v4)
  | `Named_arg_opt_COMMA_named_arg_opt_COMMA (v1, v2, v3) ->
      let v1 = map_named_argument env v1 in
      let v2 =
        match v2 with
        | Some (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_named_argument env v2 in
            todo env (v1, v2)
        | None -> todo env ()
      in
      let v3 =
        match v3 with
        | Some tok -> (* "," *) token env tok
        | None -> todo env ()
      in
      todo env (v1, v2, v3)

and map_assert_ (env : env) ((v1, v2, v3) : CST.assert_) =
  let v1 = (* "assert" *) token env v1 in
  let v2 = map_document env v2 in
  let v3 =
    match v3 with
    | Some (v1, v2) ->
        let v1 = (* ":" *) token env v1 in
        let v2 = map_document env v2 in
        todo env (v1, v2)
    | None -> todo env ()
  in
  todo env (v1, v2, v3)

and map_bind (env : env) (x : CST.bind) =
  match x with
  | `Id_EQ_expr x -> map_named_argument env x
  | `Id_LPAR_opt_params_RPAR_EQ_expr (v1, v2, v3, v4, v5, v6) ->
      let v1 = (* pattern [_a-zA-Z][_a-zA-Z0-9]* *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        match v3 with
        | Some x -> map_params env x
        | None -> todo env ()
      in
      let v4 = (* ")" *) token env v4 in
      let v5 = (* "=" *) token env v5 in
      let v6 = map_document env v6 in
      todo env (v1, v2, v3, v4, v5, v6)

and map_compspec (env : env) (xs : CST.compspec) =
  Common.map
    (fun x ->
      match x with
      | `Fors x -> map_forspec env x
      | `Ifspec x -> map_ifspec env x)
    xs

and map_document (env : env) (x : CST.document) : expr = map_expr env x

and map_expr (env : env) (x : CST.expr) : expr =
  match x with
  | `Null tok -> (* "null" *) token env tok
  | `True tok -> (* "true" *) token env tok
  | `False tok -> (* "false" *) token env tok
  | `Self tok -> (* "self" *) token env tok
  | `Dollar tok -> (* "$" *) token env tok
  | `Num tok -> (* number *) token env tok
  | `Super tok -> (* "super" *) token env tok
  | `Str x -> map_string_ env x
  | `LCURL_opt_choice_member_rep_COMMA_member_opt_COMMA_RCURL (v1, v2, v3) ->
      let v1 = (* "{" *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> map_objinside env x
        | None -> todo env ()
      in
      let v3 = (* "}" *) token env v3 in
      todo env (v1, v2, v3)
  | `LBRACK_opt_expr_rep_COMMA_expr_opt_COMMA_RBRACK (v1, v2, v3) ->
      let v1 = (* "[" *) token env v1 in
      let v2 =
        match v2 with
        | Some (v1, v2, v3) ->
            let v1 = map_document env v1 in
            let v2 =
              Common.map
                (fun (v1, v2) ->
                  let v1 = (* "," *) token env v1 in
                  let v2 = map_document env v2 in
                  todo env (v1, v2))
                v2
            in
            let v3 =
              match v3 with
              | Some tok -> (* "," *) token env tok
              | None -> todo env ()
            in
            todo env (v1, v2, v3)
        | None -> todo env ()
      in
      let v3 = (* "]" *) token env v3 in
      todo env (v1, v2, v3)
  | `LBRACK_expr_opt_COMMA_fors_opt_comp_RBRACK (v1, v2, v3, v4, v5, v6) ->
      let v1 = (* "[" *) token env v1 in
      let v2 = map_document env v2 in
      let v3 =
        match v3 with
        | Some tok -> (* "," *) token env tok
        | None -> todo env ()
      in
      let v4 = map_forspec env v4 in
      let v5 =
        match v5 with
        | Some x -> map_compspec env x
        | None -> todo env ()
      in
      let v6 = (* "]" *) token env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Expr_DOT_id (v1, v2, v3) ->
      let v1 = map_document env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = (* pattern [_a-zA-Z][_a-zA-Z0-9]* *) token env v3 in
      todo env (v1, v2, v3)
  | `Expr_LBRACK_opt_expr_opt_COLON_opt_expr_opt_COLON_opt_expr_RBRACK
      (v1, v2, v3, v4, v5) ->
      let v1 = map_document env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 =
        match v3 with
        | Some x -> map_document env x
        | None -> todo env ()
      in
      let v4 =
        match v4 with
        | Some (v1, v2, v3) ->
            let v1 = (* ":" *) token env v1 in
            let v2 =
              match v2 with
              | Some x -> map_document env x
              | None -> todo env ()
            in
            let v3 =
              match v3 with
              | Some (v1, v2) ->
                  let v1 = (* ":" *) token env v1 in
                  let v2 =
                    match v2 with
                    | Some x -> map_document env x
                    | None -> todo env ()
                  in
                  todo env (v1, v2)
              | None -> todo env ()
            in
            todo env (v1, v2, v3)
        | None -> todo env ()
      in
      let v5 = (* "]" *) token env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Super_DOT_id (v1, v2, v3) ->
      let v1 = (* "super" *) token env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = (* pattern [_a-zA-Z][_a-zA-Z0-9]* *) token env v3 in
      todo env (v1, v2, v3)
  | `Super_LBRACK_expr_RBRACK (v1, v2, v3, v4) ->
      let v1 = (* "super" *) token env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 = map_document env v3 in
      let v4 = (* "]" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Expr_LPAR_opt_args_RPAR (v1, v2, v3, v4) ->
      let v1 = map_document env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        match v3 with
        | Some x -> map_args env x
        | None -> todo env ()
      in
      let v4 = (* ")" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Id tok -> (* pattern [_a-zA-Z][_a-zA-Z0-9]* *) token env tok
  | `Local_bind (v1, v2, v3, v4, v5) ->
      let v1 = (* "local" *) token env v1 in
      let v2 = map_bind env v2 in
      let v3 =
        Common.map
          (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_bind env v2 in
            todo env (v1, v2))
          v3
      in
      let v4 = (* ";" *) token env v4 in
      let v5 = map_document env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `If_expr_then_expr_opt_else_expr (v1, v2, v3, v4, v5) ->
      let v1 = (* "if" *) token env v1 in
      let v2 = map_document env v2 in
      let v3 = (* "then" *) token env v3 in
      let v4 = map_document env v4 in
      let v5 =
        match v5 with
        | Some (v1, v2) ->
            let v1 = (* "else" *) token env v1 in
            let v2 = map_document env v2 in
            todo env (v1, v2)
        | None -> todo env ()
      in
      todo env (v1, v2, v3, v4, v5)
  | `Expr_bina_expr (v1, v2, v3) ->
      let v1 = map_document env v1 in
      let v2 = map_binaryop env v2 in
      let v3 = map_document env v3 in
      todo env (v1, v2, v3)
  | `Unar_expr (v1, v2) ->
      let v1 = map_unaryop env v1 in
      let v2 = map_document env v2 in
      todo env (v1, v2)
  | `Expr_LCURL_choice_member_rep_COMMA_member_opt_COMMA_RCURL (v1, v2, v3, v4)
    ->
      let v1 = map_document env v1 in
      let v2 = (* "{" *) token env v2 in
      let v3 = map_objinside env v3 in
      let v4 = (* "}" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Anon_func (v1, v2, v3, v4, v5) ->
      let v1 = (* "function" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        match v3 with
        | Some x -> map_params env x
        | None -> todo env ()
      in
      let v4 = (* ")" *) token env v4 in
      let v5 = map_document env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Assert_SEMI_expr (v1, v2, v3) ->
      let v1 = map_assert_ env v1 in
      let v2 = (* ";" *) token env v2 in
      let v3 = map_document env v3 in
      todo env (v1, v2, v3)
  | `Import (v1, v2) ->
      let v1 = (* "import" *) token env v1 in
      let v2 = map_string_ env v2 in
      todo env (v1, v2)
  | `Impo (v1, v2) ->
      let v1 = (* "importstr" *) token env v1 in
      let v2 = map_string_ env v2 in
      todo env (v1, v2)
  | `Expr_error (v1, v2) ->
      let v1 = (* "error" *) token env v1 in
      let v2 = map_document env v2 in
      todo env (v1, v2)
  | `Expr_in_super (v1, v2, v3) ->
      let v1 = map_document env v1 in
      let v2 = (* "in" *) token env v2 in
      let v3 = (* "super" *) token env v3 in
      todo env (v1, v2, v3)
  | `LPAR_expr_RPAR (v1, v2, v3) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = map_document env v2 in
      let v3 = (* ")" *) token env v3 in
      todo env (v1, v2, v3)

and map_field (env : env) (x : CST.field) =
  match x with
  | `Fiel_opt_PLUS_choice_COLON_expr (v1, v2, v3, v4) ->
      let v1 = map_fieldname env v1 in
      let v2 =
        match v2 with
        | Some tok -> (* "+" *) token env tok
        | None -> todo env ()
      in
      let v3 = map_h env v3 in
      let v4 = map_document env v4 in
      todo env (v1, v2, v3, v4)
  | `Fiel_LPAR_opt_params_RPAR_choice_COLON_expr (v1, v2, v3, v4, v5, v6) ->
      let v1 = map_fieldname env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        match v3 with
        | Some x -> map_params env x
        | None -> todo env ()
      in
      let v4 = (* ")" *) token env v4 in
      let v5 = map_h env v5 in
      let v6 = map_document env v6 in
      todo env (v1, v2, v3, v4, v5, v6)

and map_fieldname (env : env) (x : CST.fieldname) =
  match x with
  | `Id tok -> (* pattern [_a-zA-Z][_a-zA-Z0-9]* *) token env tok
  | `Str x -> map_string_ env x
  | `LBRACK_expr_RBRACK (v1, v2, v3) ->
      let v1 = (* "[" *) token env v1 in
      let v2 = map_document env v2 in
      let v3 = (* "]" *) token env v3 in
      todo env (v1, v2, v3)

and map_forspec (env : env) ((v1, v2, v3, v4) : CST.forspec) =
  let v1 = (* "for" *) token env v1 in
  let v2 = (* pattern [_a-zA-Z][_a-zA-Z0-9]* *) token env v2 in
  let v3 = (* "in" *) token env v3 in
  let v4 = map_document env v4 in
  todo env (v1, v2, v3, v4)

and map_ifspec (env : env) ((v1, v2) : CST.ifspec) =
  let v1 = (* "if" *) token env v1 in
  let v2 = map_document env v2 in
  todo env (v1, v2)

and map_member (env : env) (x : CST.member) =
  match x with
  | `Objl x -> map_objlocal env x
  | `Assert x -> map_assert_ env x
  | `Field x -> map_field env x

and map_named_argument (env : env) ((v1, v2, v3) : CST.named_argument) =
  let v1 = (* pattern [_a-zA-Z][_a-zA-Z0-9]* *) token env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_document env v3 in
  todo env (v1, v2, v3)

and map_objinside (env : env) (x : CST.objinside) =
  match x with
  | `Member_rep_COMMA_member_opt_COMMA (v1, v2, v3) ->
      let v1 = map_member env v1 in
      let v2 =
        Common.map
          (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_member env v2 in
            todo env (v1, v2))
          v2
      in
      let v3 =
        match v3 with
        | Some tok -> (* "," *) token env tok
        | None -> todo env ()
      in
      todo env (v1, v2, v3)
  | `Rep_objl_COMMA_LBRACK_expr_RBRACK_COLON_expr_rep_COMMA_objl_opt_COMMA_fors_opt_comp
      (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) ->
      let v1 =
        Common.map
          (fun (v1, v2) ->
            let v1 = map_objlocal env v1 in
            let v2 = (* "," *) token env v2 in
            todo env (v1, v2))
          v1
      in
      let v2 = (* "[" *) token env v2 in
      let v3 = map_document env v3 in
      let v4 = (* "]" *) token env v4 in
      let v5 = (* ":" *) token env v5 in
      let v6 = map_document env v6 in
      let v7 =
        Common.map
          (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_objlocal env v2 in
            todo env (v1, v2))
          v7
      in
      let v8 =
        match v8 with
        | Some tok -> (* "," *) token env tok
        | None -> todo env ()
      in
      let v9 = map_forspec env v9 in
      let v10 =
        match v10 with
        | Some x -> map_compspec env x
        | None -> todo env ()
      in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10)

and map_objlocal (env : env) ((v1, v2) : CST.objlocal) =
  let v1 = (* "local" *) token env v1 in
  let v2 = map_bind env v2 in
  todo env (v1, v2)

and map_param (env : env) ((v1, v2) : CST.param) =
  let v1 = (* pattern [_a-zA-Z][_a-zA-Z0-9]* *) token env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2) ->
        let v1 = (* "=" *) token env v1 in
        let v2 = map_document env v2 in
        todo env (v1, v2)
    | None -> todo env ()
  in
  todo env (v1, v2)

and map_params (env : env) ((v1, v2, v3) : CST.params) =
  let v1 = map_param env v1 in
  let v2 =
    Common.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_param env v2 in
        todo env (v1, v2))
      v2
  in
  let v3 =
    match v3 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ()
  in
  todo env (v1, v2, v3)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_jsonnet.Parse.file file)
    (fun cst ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      try
        let _e = map_document env cst in
        raise Todo
      with
      (* TODO: to delete once todo() has been removed *)
      | Failure "not implemented" as exn ->
          H.debug_sexp_cst_after_error (CST.sexp_of_document cst);
          raise exn)

let parse_pattern str =
  H.wrap_parser
    (fun () -> Tree_sitter_jsonnet.Parse.string str)
    (fun cst ->
      let file = "<pattern>" in
      let env = { H.file; conv = Hashtbl.create 0; extra = () } in
      let _e = map_document env cst in
      raise Todo)
