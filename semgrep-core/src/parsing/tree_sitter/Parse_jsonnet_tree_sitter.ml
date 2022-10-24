(* Yoann Padioleau
 *
 * Copyright (c) 2022 R2C
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
module CST = Tree_sitter_jsonnet.CST
module H = Parse_tree_sitter_helpers
module PI = Parse_info
open AST_jsonnet
module J = AST_jsonnet

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Jsonnet parser using tree-sitter-lang/semgrep-jsonnet and converting
 * to AST_jsonnet.ml
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type env = unit H.env

let token = H.token
let str = H.str
let todo (_env : env) _ = failwith "not implemented"
let later (_env : env) _ = failwith "not implemented"

let trailing_comma env v =
  match v with
  | Some tok -> Some ((* "," *) token env tok)
  | None -> None

let tat_optional env v =
  match v with
  | Some tok -> Some ((* "@" *) token env tok)
  | None -> None

(**
   Boilerplate to be used as a template when mapping the jsonnet CST
   to another type of tree.
*)

let map_binaryop (env : env) (x : CST.binaryop) : binary_op wrap =
  match x with
  | `STAR tok -> (Mult, (* "*" *) token env tok)
  | `SLASH tok -> (Div, (* "/" *) token env tok)
  | `PERC tok -> (Mod, (* "%" *) token env tok)
  | `PLUS tok -> (Plus, (* "+" *) token env tok)
  | `DASH tok -> (Minus, (* "-" *) token env tok)
  | `LTLT tok -> (LSL, (* "<<" *) token env tok)
  | `GTGT tok -> (LSR, (* ">>" *) token env tok)
  | `LT tok -> (Lt, (* "<" *) token env tok)
  | `LTEQ tok -> (LtE, (* "<=" *) token env tok)
  | `GT tok -> (Gt, (* ">" *) token env tok)
  | `GTEQ tok -> (GtE, (* ">=" *) token env tok)
  | `EQEQ tok -> (Eq, (* "==" *) token env tok)
  | `BANGEQ tok -> (NotEq, (* "!=" *) token env tok)
  | `AMP tok -> (BitAnd, (* "&" *) token env tok)
  | `HAT tok -> (BitXor, (* "^" *) token env tok)
  | `BAR tok -> (BitOr, (* "|" *) token env tok)
  | `AMPAMP tok -> (And, (* "&&" *) token env tok)
  | `BARBAR tok -> (Or, (* "||" *) token env tok)

let map_unaryop (env : env) (x : CST.unaryop) : unary_op wrap =
  match x with
  | `DASH tok -> (UMinus, (* "-" *) token env tok)
  | `PLUS tok -> (UPlus, (* "+" *) token env tok)
  | `BANG tok -> (UBang, (* "!" *) token env tok)
  | `TILDE tok -> (UTilde, (* "~" *) token env tok)

let map_imm_tok_prec_p1_pat_59587ce (env : env)
    (tok : CST.imm_tok_prec_p1_pat_59587ce) =
  (* pattern "[^\\\\'\\n]+" *) token env tok

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
      let _tat = tat_optional env v1 in
      let v2 = (* "'" *) token env v2 in
      let v3 = (* "'" *) token env v3 in
      later env (v1, v2, v3)
  | `Opt_AT_single_str_single_single (v1, v2, v3, v4) ->
      let _tat = tat_optional env v1 in
      let v2 = (* "'" *) token env v2 in
      let v3 = map_str_single env v3 in
      let v4 = (* "'" *) token env v4 in
      later env (v1, v2, v3, v4)
  | `Opt_AT_double_double (v1, v2, v3) ->
      let _tat = tat_optional env v1 in
      let v2 = (* "\"" *) token env v2 in
      let v3 = (* "\"" *) token env v3 in
      later env (v1, v2, v3)
  | `Opt_AT_double_str_double_double (v1, v2, v3, v4) ->
      let _tat = tat_optional env v1 in
      let v2 = (* "\"" *) token env v2 in
      let v3 = map_str_double env v3 in
      let v4 = (* "\"" *) token env v4 in
      later env (v1, v2, v3, v4)
  | `Opt_AT_str_start_str_content_str_end (v1, v2, v3, v4) ->
      let _tat = tat_optional env v1 in
      let v2 = (* string_start *) token env v2 in
      let v3 = (* string_content *) token env v3 in
      let v4 = (* string_end *) token env v4 in
      later env (v1, v2, v3, v4)

let rec map_args (env : env) (x : CST.args) : argument list =
  match x with
  | `Expr_opt_COMMA_expr_opt_COMMA_named_arg_opt_COMMA (v1, v2, v3, v4) ->
      let v1 = map_document env v1 in
      let v2 =
        match v2 with
        | Some (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_document env v2 in
            [ Arg v2 ]
        | None -> []
      in
      let v3 =
        match v3 with
        | Some (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_named_argument env v2 in
            [ v2 ]
        | None -> []
      in
      let _v4 = trailing_comma env v4 in
      Arg v1 :: (v2 @ v3)
  | `Named_arg_opt_COMMA_named_arg_opt_COMMA (v1, v2, v3) ->
      let v1 = map_named_argument env v1 in
      let v2 =
        match v2 with
        | Some (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_named_argument env v2 in
            [ v2 ]
        | None -> []
      in
      let _v3 = trailing_comma env v3 in
      v1 :: v2

and map_assert_ (env : env) ((v1, v2, v3) : CST.assert_) : expr =
  let tassert = (* "assert" *) token env v1 in
  let e = map_document env v2 in
  let es =
    match v3 with
    | Some (v1, v2) ->
        let _tcolon = (* ":" *) token env v1 in
        let v2 = map_document env v2 in
        [ v2 ]
    | None -> []
  in
  TodoExpr (("Assert", tassert), e :: es) |> J.e

and map_bind (env : env) (x : CST.bind) : local_binding =
  match x with
  | `Id_EQ_expr x ->
      let id, teq, e = map_named_argument_bis env x in
      later env (id, teq, e)
  | `Id_LPAR_opt_params_RPAR_EQ_expr (v1, v2, v3, v4, v5, v6) ->
      let id = (* pattern [_a-zA-Z][_a-zA-Z0-9]* *) str env v1 in
      let lpar = (* "(" *) token env v2 in
      let ps =
        match v3 with
        | Some x -> map_params env x
        | None -> []
      in
      let rpar = (* ")" *) token env v4 in
      let teq = (* "=" *) token env v5 in
      let e = map_document env v6 in
      later env (id, lpar, ps, rpar, teq, e)

and map_compspec (env : env) (xs : CST.compspec) =
  Common.map
    (fun x ->
      match x with
      | `Fors x -> map_forspec env x
      | `Ifspec x -> map_ifspec env x)
    xs

and map_document (env : env) (x : CST.document) : program = map_expr env x

and map_expr_opt env v =
  match v with
  | Some x -> Some (map_expr env x)
  | None -> None

and map_expr (env : env) (x : CST.expr) : expr =
  match x with
  | `Null tok -> L (Null ((* "null" *) token env tok)) |> J.e
  | `True tok ->
      let t = (* "true" *) token env tok in
      L (Bool (true, t)) |> J.e
  | `False tok ->
      let t = (* "false" *) token env tok in
      L (Bool (false, t)) |> J.e
  | `Self tok ->
      let t = (* "self" *) token env tok in
      IdSpecial (Self, t) |> J.e
  | `Dollar tok ->
      let t = (* "$" *) token env tok in
      IdSpecial (Dollar, t) |> J.e
  | `Num tok ->
      let s = (* number *) str env tok in
      L (Number s) |> J.e
  | `Super tok ->
      let t = (* "super" *) token env tok in
      IdSpecial (Super, t) |> J.e
  | `Str x -> map_string_ env x
  | `LCURL_opt_choice_member_rep_COMMA_member_opt_COMMA_RCURL (v1, v2, v3) ->
      let lc = (* "{" *) token env v1 in
      let xs =
        match v2 with
        | Some x -> map_objinside env x
        | None -> []
      in
      let rc = (* "}" *) token env v3 in
      Object (lc, xs, rc) |> J.e
  | `LBRACK_opt_expr_rep_COMMA_expr_opt_COMMA_RBRACK (v1, v2, v3) ->
      let lb = (* "[" *) token env v1 in
      let xs =
        match v2 with
        | Some (v1, v2, v3) ->
            let v1 = map_document env v1 in
            let v2 =
              Common.map
                (fun (v1, v2) ->
                  let _v1 = (* "," *) token env v1 in
                  let v2 = map_document env v2 in
                  v2)
                v2
            in
            let _v3 = trailing_comma env v3 in
            v1 :: v2
        | None -> []
      in
      let rb = (* "]" *) token env v3 in
      Array (lb, xs, rb) |> J.e
  | `LBRACK_expr_opt_COMMA_fors_opt_comp_RBRACK (v1, v2, v3, v4, v5, v6) ->
      let lb = (* "[" *) token env v1 in
      let e = map_document env v2 in
      let _v3 = trailing_comma env v3 in
      let _v4TODO = map_forspec env v4 in
      let _v5TODO =
        match v5 with
        | Some x -> map_compspec env x
        | None -> []
      in
      let _rb = (* "]" *) token env v6 in
      TodoExpr (("ArrayComprehension", lb), [ e ]) |> J.e
  | `Expr_DOT_id (v1, v2, v3) ->
      let e = map_document env v1 in
      let tdot = (* "." *) token env v2 in
      let fld = (* pattern [_a-zA-Z][_a-zA-Z0-9]* *) str env v3 in
      DotAccess (e, tdot, fld) |> J.e
  | `Expr_LBRACK_opt_expr_opt_COLON_opt_expr_opt_COLON_opt_expr_RBRACK
      (v1, v2, v3, v4, v5) -> (
      let e = map_document env v1 in
      let lb = (* "[" *) token env v2 in
      let idx_opt = map_expr_opt env v3 in
      let slice_opt =
        match v4 with
        | Some (v1, v2, v3) ->
            let v1 = (* ":" *) token env v1 in
            let v2 = map_expr_opt env v2 in
            let v3 =
              match v3 with
              | Some (v1, v2) ->
                  let v1 = (* ":" *) token env v1 in
                  let v2 = map_expr_opt env v2 in
                  Some (v1, v2)
              | None -> None
            in
            Some (v1, v2, v3)
        | None -> None
      in
      let rb = (* "]" *) token env v5 in
      match (idx_opt, slice_opt) with
      | Some idx, None -> ArrayAccess (e, (lb, idx, rb)) |> J.e
      | _else_ -> TodoExpr (("SliceAccess", lb), []) |> J.e)
  | `Super_DOT_id (v1, v2, v3) ->
      let tsuper = (* "super" *) token env v1 in
      let tdot = (* "." *) token env v2 in
      let fld = (* pattern [_a-zA-Z][_a-zA-Z0-9]* *) str env v3 in
      DotAccess (IdSpecial (Super, tsuper) |> J.e, tdot, fld) |> J.e
  | `Super_LBRACK_expr_RBRACK (v1, v2, v3, v4) ->
      let tsuper = (* "super" *) token env v1 in
      let lb = (* "[" *) token env v2 in
      let idx = map_document env v3 in
      let rb = (* "]" *) token env v4 in
      ArrayAccess (IdSpecial (Super, tsuper) |> J.e, (lb, idx, rb)) |> J.e
  | `Expr_LPAR_opt_args_RPAR (v1, v2, v3, v4) ->
      let e = map_document env v1 in
      let lp = (* "(" *) token env v2 in
      let args =
        match v3 with
        | Some x -> map_args env x
        | None -> []
      in
      let rp = (* ")" *) token env v4 in
      Call (e, (lp, args, rp)) |> J.e
  | `Id tok ->
      let id = (* pattern [_a-zA-Z][_a-zA-Z0-9]* *) str env tok in
      Id id |> J.e
  | `Local_bind (v1, v2, v3, v4, v5) ->
      let tlocal = (* "local" *) token env v1 in
      let bind = map_bind env v2 in
      let binds =
        Common.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_bind env v2 in
            v2)
          v3
      in
      let tsemi = (* ";" *) token env v4 in
      let e = map_document env v5 in
      LocalBind (tlocal, bind :: binds, tsemi, e) |> J.e
  | `If_expr_then_expr_opt_else_expr (v1, v2, v3, v4, v5) ->
      let tif = (* "if" *) token env v1 in
      let cond = map_document env v2 in
      let _tthen = (* "then" *) token env v3 in
      let then_ = map_document env v4 in
      let else_opt =
        match v5 with
        | Some (v1, v2) ->
            let telse = (* "else" *) token env v1 in
            let e = map_document env v2 in
            Some (telse, e)
        | None -> None
      in
      If (tif, cond, then_, else_opt) |> J.e
  | `Expr_bina_expr (v1, v2, v3) ->
      let v1 = map_document env v1 in
      let v2 = map_binaryop env v2 in
      let v3 = map_document env v3 in
      BinaryOp (v1, v2, v3) |> J.e
  | `Unar_expr (v1, v2) ->
      let v1 = map_unaryop env v1 in
      let v2 = map_document env v2 in
      UnaryOp (v1, v2) |> J.e
  | `Expr_LCURL_choice_member_rep_COMMA_member_opt_COMMA_RCURL (v1, v2, v3, v4)
    ->
      let e = map_document env v1 in
      let lc = (* "{" *) token env v2 in
      let _flds = map_objinside env v3 in
      let _rc = (* "}" *) token env v4 in
      TodoExpr (("CurlyExprObj", lc), [ e ]) |> J.e
  | `Anon_func (v1, v2, v3, v4, v5) ->
      let tfunc = (* "function" *) token env v1 in
      let lp = (* "(" *) token env v2 in
      let params =
        match v3 with
        | Some x -> map_params env x
        | None -> []
      in
      let rp = (* ")" *) token env v4 in
      let body = map_document env v5 in
      let fdef = { ftok = tfunc; fparams = (lp, params, rp); fbody = body } in
      Lambda fdef |> J.e
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
      let lp = (* "(" *) token env v1 in
      let e = map_document env v2 in
      let rp = (* ")" *) token env v3 in
      ParenExpr (lp, e, rp) |> J.e

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

and map_fieldname (env : env) (x : CST.fieldname) : fieldname =
  match x with
  | `Id tok ->
      let id = (* pattern [_a-zA-Z][_a-zA-Z0-9]* *) str env tok in
      todo env id
  | `Str x ->
      let s = map_string_ env x in
      todo env s
  | `LBRACK_expr_RBRACK (v1, v2, v3) ->
      let lb = (* "[" *) token env v1 in
      let idx = map_document env v2 in
      let rb = (* "]" *) token env v3 in
      todo env (lb, idx, rb)

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

and map_member (env : env) (x : CST.member) : field =
  match x with
  | `Objl x ->
      let x = map_objlocal env x in
      todo env x
  | `Assert x ->
      let x = map_assert_ env x in
      todo env x
  | `Field x ->
      let x = map_field env x in
      todo env x

and map_named_argument (env : env) ((v1, v2, v3) : CST.named_argument) :
    argument =
  let a, b, c = map_named_argument_bis env (v1, v2, v3) in
  NamedArg (a, b, c)

and map_named_argument_bis (env : env) ((v1, v2, v3) : CST.named_argument) =
  let v1 = (* pattern [_a-zA-Z][_a-zA-Z0-9]* *) str env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_document env v3 in
  (v1, v2, v3)

and map_objinside (env : env) (x : CST.objinside) : field list =
  match x with
  | `Member_rep_COMMA_member_opt_COMMA (v1, v2, v3) ->
      let v1 = map_member env v1 in
      let v2 =
        Common.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_member env v2 in
            v2)
          v2
      in
      let _v3 = trailing_comma env v3 in
      v1 :: v2
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
      let v8 = trailing_comma env v8 in
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

and map_param (env : env) ((v1, v2) : CST.param) : parameter =
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

and map_params (env : env) ((v1, v2, v3) : CST.params) : parameter list =
  let v1 = map_param env v1 in
  let v2 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_param env v2 in
        v2)
      v2
  in
  let _v3 = trailing_comma env v3 in
  v1 :: v2

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_jsonnet.Parse.file file)
    (fun cst ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      try
        let e = map_document env cst in
        e
      with
      (* TODO: to delete once todo() has been removed *)
      | Failure "not implemented" as exn ->
          let e = Exception.catch exn in
          H.debug_sexp_cst_after_error (CST.sexp_of_document cst);
          Exception.reraise e)

let parse_pattern str =
  H.wrap_parser
    (fun () -> Tree_sitter_jsonnet.Parse.string str)
    (fun cst ->
      let file = "<pattern>" in
      let env = { H.file; conv = Hashtbl.create 0; extra = () } in
      let _e = map_document env cst in
      raise Todo)
