(* Yoann Padioleau
 *
 * Copyright (c) 2022-2023 r2c
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
open Fpath_.Operators
module CST = Tree_sitter_jsonnet.CST
module H = Parse_tree_sitter_helpers
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

let map_id (env : env) (x : CST.id) : ident =
  match x with
  | `Id tok -> (* pattern [_a-zA-Z][_a-zA-Z0-9]* *) str env tok
  | `Semg_meta tok -> str env tok

let map_unaryop (env : env) (x : CST.unaryop) : unary_op wrap =
  match x with
  | `DASH tok -> (UMinus, (* "-" *) token env tok)
  | `PLUS tok -> (UPlus, (* "+" *) token env tok)
  | `BANG tok -> (UBang, (* "!" *) token env tok)
  | `TILDE tok -> (UTilde, (* "~" *) token env tok)

let map_imm_tok_prec_p1_pat_59587ce (env : env)
    (tok : CST.imm_tok_prec_p1_pat_59587ce) : string wrap =
  (* pattern "[^\\\\'\\n]+" *) str env tok

let map_imm_tok_prec_p1_pat_c7f65b4 (env : env)
    (tok : CST.imm_tok_prec_p1_pat_c7f65b4) : string wrap =
  (* pattern "[^\\\\\"\\n]+" *) str env tok

let map_h (env : env) (x : CST.h) : hidden wrap =
  match x with
  | `COLON tok -> (Visible, (* ":" *) token env tok)
  | `COLONCOLON tok -> (Hidden, (* "::" *) token env tok)
  | `COLONCOLONCOLON tok -> (ForcedVisible, (* ":::" *) token env tok)

let map_str_single (env : env) (xs : CST.str_single) : string_content =
  List_.map
    (fun x ->
      match x with
      | `Imm_tok_prec_p1_pat_59587ce x -> map_imm_tok_prec_p1_pat_59587ce env x
      | `Esc_seq tok -> (* escape_sequence *) str env tok)
    xs

let map_str_double (env : env) (xs : CST.str_double) =
  List_.map
    (fun x ->
      match x with
      | `Imm_tok_prec_p1_pat_c7f65b4 x -> map_imm_tok_prec_p1_pat_c7f65b4 env x
      | `Esc_seq tok -> (* escape_sequence *) str env tok)
    xs

let map_string_ (env : env) (x : CST.string_) : string_ =
  match x with
  | `Opt_AT_single_single (v1, v2, v3) ->
      let tat = tat_optional env v1 in
      let v2 = (* "'" *) token env v2 in
      let v3 = (* "'" *) token env v3 in
      (tat, SingleQuote, (v2, [], v3))
  | `Opt_AT_single_str_single_single (v1, v2, v3, v4) ->
      let tat = tat_optional env v1 in
      let v2 = (* "'" *) token env v2 in
      let v3 = map_str_single env v3 in
      let v4 = (* "'" *) token env v4 in
      (tat, SingleQuote, (v2, v3, v4))
  | `Opt_AT_double_double (v1, v2, v3) ->
      let tat = tat_optional env v1 in
      let v2 = (* "\"" *) token env v2 in
      let v3 = (* "\"" *) token env v3 in
      (tat, DoubleQuote, (v2, [], v3))
  | `Opt_AT_double_str_double_double (v1, v2, v3, v4) ->
      let tat = tat_optional env v1 in
      let v2 = (* "\"" *) token env v2 in
      let v3 = map_str_double env v3 in
      let v4 = (* "\"" *) token env v4 in
      (tat, DoubleQuote, (v2, v3, v4))
  | `Opt_AT_str_start_str_content_str_end (v1, v2, v3, v4) ->
      let tat = tat_optional env v1 in
      let s1, t1 = (* string_start *) str env v2 in
      let content = (* string_content *) str env v3 in
      let s2, t2 = (* string_end *) str env v4 in
      let kind =
        match (s1, s2) with
        (* not sure why the Opt_AT_single_str_single_single does not cover this case *)
        | "'", "'" -> SingleQuote
        (* not sure why the Opt_AT_double_double does not cover this case *)
        | "\"", "\"" -> DoubleQuote
        | "|||", "|||" -> TripleBar
        | x, y when x = y ->
            raise
              (Parsing_error.Other_error
                 (spf "unrecognized string delimiter: %s" x, t1))
        | x, y ->
            raise
              (Parsing_error.Other_error
                 (spf "unmatched string delimiter: '%s' and '%s'" x y, t1))
      in
      (tat, kind, (t1, [ content ], t2))

let rec map_args (env : env) (x : CST.args) : argument list =
  match x with
  | `Expr_rep_COMMA_expr_rep_COMMA_named_arg_opt_COMMA (v1, v2, v3, v4) ->
      let v1 = map_document env v1 in
      let v2 =
        v2
        |> List_.map (fun (v1, v2) ->
               let _v1 = (* "," *) token env v1 in
               let v2 = map_document env v2 in
               Arg v2)
      in
      let v3 =
        v3
        |> List_.map (fun (v1, v2) ->
               let _v1 = (* "," *) token env v1 in
               let v2 = map_named_argument env v2 in
               v2)
      in
      let _v4 = trailing_comma env v4 in
      Arg v1 :: (v2 @ v3)
  | `Named_arg_rep_COMMA_named_arg_opt_COMMA (v1, v2, v3) ->
      let v1 = map_named_argument env v1 in
      let v2 =
        v2
        |> List_.map (fun (v1, v2) ->
               let _v1 = (* "," *) token env v1 in
               let v2 = map_named_argument env v2 in
               v2)
      in
      let _v3 = trailing_comma env v3 in
      v1 :: v2

and map_assert_ (env : env) ((v1, v2, v3) : CST.assert_) : assert_ =
  let tassert = (* "assert" *) token env v1 in
  let e = map_document env v2 in
  let eopt =
    match v3 with
    | Some (v1, v2) ->
        let tcolon = (* ":" *) token env v1 in
        let v2 = map_document env v2 in
        Some (tcolon, v2)
    | None -> None
  in
  (tassert, e, eopt)

and map_bind (env : env) (x : CST.bind) : bind =
  match x with
  | `Id_EQ_expr x ->
      let id, teq, e = map_named_argument_bis env x in
      B (id, teq, e)
  | `Id_LPAR_opt_params_RPAR_EQ_expr (v1, v2, v3, v4, v5, v6) ->
      let id = map_id env v1 in
      let lpar = (* "(" *) token env v2 in
      let ps =
        match v3 with
        | Some x -> map_params env x
        | None -> []
      in
      let rpar = (* ")" *) token env v4 in
      let teq = (* "=" *) token env v5 in
      let e = map_document env v6 in
      let def = { f_tok = lpar; f_params = (lpar, ps, rpar); f_body = e } in
      B (id, teq, Lambda def)

and map_compspec (env : env) (xs : CST.compspec) : for_or_if_comp list =
  List_.map
    (fun x ->
      match x with
      | `Fors x ->
          let forspec = map_forspec env x in
          CompFor forspec
      | `Ifspec x ->
          let ifspec = map_ifspec env x in
          CompIf ifspec)
    xs

and map_document (env : env) (x : CST.document) : program = map_expr env x

and map_expr_opt env v =
  match v with
  | Some x -> Some (map_expr env x)
  | None -> None

and map_expr (env : env) (x : CST.expr) : expr =
  match x with
  | `Semg_ellips v1 -> Ellipsis (token env v1)
  | `Deep_ellips (v1, v2, v3) ->
      let l = token env v1 in
      let e = map_expr env v2 in
      let r = token env v3 in
      DeepEllipsis (l, e, r)
  | `Choice_null x -> map_expr_origin env x

and map_expr_origin (env : env) x : expr =
  match x with
  | `Null tok -> L (Null ((* "null" *) token env tok))
  | `True tok ->
      let t = (* "true" *) token env tok in
      L (Bool (true, t))
  | `False tok ->
      let t = (* "false" *) token env tok in
      L (Bool (false, t))
  | `Self tok ->
      let t = (* "self" *) token env tok in
      IdSpecial (Self, t)
  | `Dollar tok ->
      let t = (* "$" *) token env tok in
      IdSpecial (Dollar, t)
  | `Num tok ->
      let s = (* number *) str env tok in
      L (Number s)
  | `Str x ->
      let s = map_string_ env x in
      L (Str s)
  | `LCURL_opt_choice_member_rep_COMMA_member_opt_COMMA_RCURL (v1, v2, v3) ->
      let lc = (* "{" *) token env v1 in
      let inside =
        match v2 with
        | Some x -> map_objinside env x
        | None -> Object []
      in
      let rc = (* "}" *) token env v3 in
      O (lc, inside, rc)
  | `LBRACK_opt_expr_rep_COMMA_expr_opt_COMMA_RBRACK (v1, v2, v3) ->
      let lb = (* "[" *) token env v1 in
      let xs =
        match v2 with
        | Some (v1, v2, v3) ->
            let v1 = map_document env v1 in
            let v2 =
              List_.map
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
      A (lb, Array xs, rb)
  | `LBRACK_expr_opt_COMMA_fors_opt_comp_RBRACK (v1, v2, v3, v4, v5, v6) ->
      let lb = (* "[" *) token env v1 in
      let e = map_document env v2 in
      let _v3 = trailing_comma env v3 in
      let forspec = map_forspec env v4 in
      let comps =
        match v5 with
        | Some x -> map_compspec env x
        | None -> []
      in
      let rb = (* "]" *) token env v6 in
      A (lb, ArrayComp (e, forspec, comps), rb)
  | `Expr_DOT_id (v1, v2, v3) ->
      let e = map_document env v1 in
      let tdot = (* "." *) token env v2 in
      let fld = map_id env v3 in
      DotAccess (e, tdot, fld)
  | `Expr_LBRACK_opt_expr_opt_COLON_opt_expr_opt_COLON_opt_expr_RBRACK
      (v1, v2, v3, v4, v5) -> (
      let e = map_document env v1 in
      let lb = (* "[" *) token env v2 in
      let idx_opt = map_expr_opt env v3 in
      let slice_opt =
        match v4 with
        | Some (v1, v2, v3) ->
            let _tcolon = (* ":" *) token env v1 in
            let e2_opt = map_expr_opt env v2 in
            let e3_opt =
              match v3 with
              | Some (v1, v2) ->
                  let _tcolon = (* ":" *) token env v1 in
                  let e3_opt = map_expr_opt env v2 in
                  e3_opt
              | None -> None
            in
            Some (e2_opt, e3_opt)
        | None -> None
      in
      let rb = (* "]" *) token env v5 in
      match (idx_opt, slice_opt) with
      | Some idx, None -> ArrayAccess (e, (lb, idx, rb))
      | None, None -> SliceAccess (e, (lb, (None, None, None), rb))
      | e1_opt, Some (e2_opt, e3_opt) ->
          SliceAccess (e, (lb, (e1_opt, e2_opt, e3_opt), rb)))
  | `Super_DOT_id (v1, v2, v3) ->
      let tsuper = (* "super" *) token env v1 in
      let tdot = (* "." *) token env v2 in
      let fld = map_id env v3 in
      DotAccess (IdSpecial (Super, tsuper), tdot, fld)
  | `Super_LBRACK_expr_RBRACK (v1, v2, v3, v4) ->
      let tsuper = (* "super" *) token env v1 in
      let lb = (* "[" *) token env v2 in
      let idx = map_document env v3 in
      let rb = (* "]" *) token env v4 in
      ArrayAccess (IdSpecial (Super, tsuper), (lb, idx, rb))
  | `Expr_LPAR_opt_args_RPAR_opt_tail (v1, v2, v3, v4, v5) ->
      let e = map_document env v1 in
      let lp = (* "(" *) token env v2 in
      let args =
        match v3 with
        | Some x -> map_args env x
        | None -> []
      in
      let rp = (* ")" *) token env v4 in
      let _tailstrictTODO = Option.map (token env) v5 in
      Call (e, (lp, args, rp))
  | `Id tok ->
      let id = map_id env tok in
      Id id
  | `Local_bind (v1, v2, v3, v4, v5) ->
      let tlocal = (* "local" *) token env v1 in
      let bind = map_bind env v2 in
      let binds =
        List_.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_bind env v2 in
            v2)
          v3
      in
      let tsemi = (* ";" *) token env v4 in
      let e = map_document env v5 in
      Local (tlocal, bind :: binds, tsemi, e)
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
      If (tif, cond, then_, else_opt)
  | `Bin_expr v1 -> map_binary_expr env v1
  | `Unar_expr (v1, v2) ->
      let v1 = map_unaryop env v1 in
      let v2 = map_document env v2 in
      UnaryOp (v1, v2)
  | `Expr_LCURL_choice_member_rep_COMMA_member_opt_COMMA_RCURL (v1, v2, v3, v4)
    ->
      let e = map_document env v1 in
      let lc = (* "{" *) token env v2 in
      let flds = map_objinside env v3 in
      let rc = (* "}" *) token env v4 in
      AdjustObj (e, (lc, flds, rc))
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
      let fdef =
        { f_tok = tfunc; f_params = (lp, params, rp); f_body = body }
      in
      Lambda fdef
  | `Assert_SEMI_expr (v1, v2, v3) ->
      let assert_ = map_assert_ env v1 in
      let tsemi = (* ";" *) token env v2 in
      let e3 = map_document env v3 in
      Assert (assert_, tsemi, e3)
  | `Import (v1, v2) ->
      let timport = (* "import" *) token env v1 in
      let s = map_string_ env v2 in
      I (Import (timport, s))
  | `Impo (v1, v2) ->
      let t = (* "importstr" *) token env v1 in
      let s = map_string_ env v2 in
      I (ImportStr (t, s))
  | `Expr_error (v1, v2) ->
      let t = (* "error" *) token env v1 in
      let e = map_document env v2 in
      Error (t, e)
  | `Expr_in_super (v1, v2, v3) ->
      let e = map_document env v1 in
      let tin = (* "in" *) token env v2 in
      let tsuper = (* "super" *) token env v3 in
      BinaryOp (e, (In, tin), IdSpecial (Super, tsuper))
  | `LPAR_expr_RPAR (v1, v2, v3) ->
      let lp = (* "(" *) token env v1 in
      let e = map_document env v2 in
      let rp = (* ")" *) token env v3 in
      ParenExpr (lp, e, rp)

(* TODO? in? *)
and map_binary_expr (env : env) (x : CST.binary_expr) : expr =
  match x with
  | `Expr_choice_STAR_expr (v1, v2, v3) ->
      let v1 = map_document env v1 in
      let v2 =
        match v2 with
        | `STAR tok -> (Mult, (* "*" *) token env tok)
        | `SLASH tok -> (Div, (* "/" *) token env tok)
        | `PERC tok -> (Mod, (* "%" *) token env tok)
      in
      let v3 = map_document env v3 in
      BinaryOp (v1, v2, v3)
  | `Expr_choice_PLUS_expr (v1, v2, v3) ->
      let v1 = map_document env v1 in
      let v2 =
        match v2 with
        | `PLUS tok -> (Plus, (* "+" *) token env tok)
        | `DASH tok -> (Minus, (* "-" *) token env tok)
      in
      let v3 = map_document env v3 in
      BinaryOp (v1, v2, v3)
  | `Expr_choice_LTLT_expr (v1, v2, v3) ->
      let v1 = map_document env v1 in
      let v2 =
        match v2 with
        | `LTLT tok -> (LSL, (* "<<" *) token env tok)
        | `GTGT tok -> (LSR, (* ">>" *) token env tok)
      in
      let v3 = map_document env v3 in
      BinaryOp (v1, v2, v3)
  | `Expr_choice_LT_expr (v1, v2, v3) ->
      let v1 = map_document env v1 in
      let v2 =
        match v2 with
        | `LT tok -> (Lt, (* "<" *) token env tok)
        | `LTEQ tok -> (LtE, (* "<=" *) token env tok)
        | `GT tok -> (Gt, (* ">" *) token env tok)
        | `GTEQ tok -> (GtE, (* ">=" *) token env tok)
      in
      let v3 = map_document env v3 in
      BinaryOp (v1, v2, v3)
  | `Expr_choice_EQEQ_expr (v1, v2, v3) ->
      let v1 = map_document env v1 in
      let v2 =
        match v2 with
        | `EQEQ tok -> (Eq, (* "==" *) token env tok)
        | `BANGEQ tok -> (NotEq, (* "!=" *) token env tok)
      in
      let v3 = map_document env v3 in
      BinaryOp (v1, v2, v3)
  | `Expr_AMP_expr (v1, v2, v3) ->
      let v1 = map_document env v1 in
      let v2 = (BitAnd, (* "&" *) token env v2) in
      let v3 = map_document env v3 in
      BinaryOp (v1, v2, v3)
  | `Expr_HAT_expr (v1, v2, v3) ->
      let v1 = map_document env v1 in
      let v2 = (BitXor, (* "^" *) token env v2) in
      let v3 = map_document env v3 in
      BinaryOp (v1, v2, v3)
  | `Expr_BAR_expr (v1, v2, v3) ->
      let v1 = map_document env v1 in
      let v2 = (BitOr, (* "|" *) token env v2) in
      let v3 = map_document env v3 in
      BinaryOp (v1, v2, v3)
  | `Expr_AMPAMP_expr (v1, v2, v3) ->
      let v1 = map_document env v1 in
      let v2 = (And, (* "&&" *) token env v2) in
      let v3 = map_document env v3 in
      BinaryOp (v1, v2, v3)
  | `Expr_BARBAR_expr (v1, v2, v3) ->
      let v1 = map_document env v1 in
      let v2 = (Or, (* "||" *) token env v2) in
      let v3 = map_document env v3 in
      BinaryOp (v1, v2, v3)

and map_field (env : env) (x : CST.field) : obj_member =
  match x with
  | `Semg_ellips v1 ->
      let tdots = token env v1 in
      OEllipsis tdots
  | `Choice_fiel_opt_PLUS_choice_COLON_expr y -> (
      match y with
      | `Fiel_opt_PLUS_choice_COLON_expr (v1, v2, v3, v4) ->
          let fld = map_fieldname env v1 in
          let plusopt =
            match v2 with
            | Some tok -> Some (PlusField ((* "+" *) token env tok))
            | None -> None
          in
          let h = map_h env v3 in
          let e = map_document env v4 in
          OField
            {
              fld_name = fld;
              fld_attr = plusopt;
              fld_hidden = h;
              fld_value = e;
            }
      | `Fiel_LPAR_opt_params_RPAR_choice_COLON_expr (v1, v2, v3, v4, v5, v6) ->
          let fld = map_fieldname env v1 in
          let lp = (* "(" *) token env v2 in
          let params =
            match v3 with
            | Some x -> map_params env x
            | None -> []
          in
          let rp = (* ")" *) token env v4 in
          let h = map_h env v5 in
          let e = map_document env v6 in
          let fdef = { f_tok = lp; f_params = (lp, params, rp); f_body = e } in
          OField
            {
              fld_name = fld;
              fld_attr = None;
              fld_hidden = h;
              fld_value = Lambda fdef;
            })

and map_fieldname (env : env) (x : CST.fieldname) : field_name =
  match x with
  | `Id tok ->
      let id = map_id env tok in
      FId id
  | `Str x ->
      let s = map_string_ env x in
      FStr s
  | `LBRACK_expr_RBRACK (v1, v2, v3) ->
      let lb = (* "[" *) token env v1 in
      let idx = map_document env v2 in
      let rb = (* "]" *) token env v3 in
      FDynamic (lb, idx, rb)

and map_forspec (env : env) ((v1, v2, v3, v4) : CST.forspec) : for_comp =
  let tfor = (* "for" *) token env v1 in
  let id = map_id env v2 in
  let tin = (* "in" *) token env v3 in
  let e = map_document env v4 in
  (tfor, id, tin, e)

and map_ifspec (env : env) ((v1, v2) : CST.ifspec) : if_comp =
  let tif = (* "if" *) token env v1 in
  let e = map_document env v2 in
  (tif, e)

and map_member (env : env) (x : CST.member) : obj_member =
  match x with
  | `Objl x ->
      let x = map_objlocal env x in
      OLocal x
  | `Assert x ->
      let x = map_assert_ env x in
      OAssert x
  | `Field x ->
      let x = map_field env x in
      x

and map_named_argument (env : env) ((v1, v2, v3) : CST.named_argument) :
    argument =
  let a, b, c = map_named_argument_bis env (v1, v2, v3) in
  NamedArg (a, b, c)

and map_named_argument_bis (env : env) ((v1, v2, v3) : CST.named_argument) =
  let v1 = map_id env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_document env v3 in
  (v1, v2, v3)

and map_objinside (env : env) (x : CST.objinside) : obj_inside =
  match x with
  | `Member_rep_COMMA_member_opt_COMMA (v1, v2, v3) ->
      let v1 = map_member env v1 in
      let v2 =
        List_.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_member env v2 in
            v2)
          v2
      in
      let _v3 = trailing_comma env v3 in
      Object (v1 :: v2)
  | `Rep_objl_COMMA_LBRACK_expr_RBRACK_COLON_expr_rep_COMMA_objl_opt_COMMA_fors_opt_comp
      (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) ->
      let oc_locals1 =
        List_.map
          (fun (v1, v2) ->
            let v1 = map_objlocal env v1 in
            let _v2 = (* "," *) token env v2 in
            v1)
          v1
      in
      let lb = (* "[" *) token env v2 in
      let fldname = map_document env v3 in
      let rb = (* "]" *) token env v4 in
      let tcolon = (* ":" *) token env v5 in
      let fldval = map_document env v6 in
      let oc_locals2 =
        List_.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_objlocal env v2 in
            v2)
          v7
      in
      let _ = trailing_comma env v8 in
      let forspec = map_forspec env v9 in
      let comps =
        match v10 with
        | Some x -> map_compspec env x
        | None -> []
      in
      ObjectComp
        {
          oc_locals1;
          oc_comp = (((lb, fldname, rb), tcolon, fldval), forspec, comps);
          oc_locals2;
        }

and map_objlocal (env : env) ((v1, v2) : CST.objlocal) =
  let tlocal = (* "local" *) token env v1 in
  let bind = map_bind env v2 in
  (tlocal, bind)

and map_param (env : env) (x : CST.param) : parameter =
  match x with
  | `Semg_ellips v1 -> ParamEllipsis (token env v1)
  | `Id_opt_EQ_expr (v1, v2) ->
      let id = map_id env v1 in
      let default_opt =
        match v2 with
        | Some (v1, v2) ->
            let v1 = (* "=" *) token env v1 in
            let v2 = map_document env v2 in
            Some (v1, v2)
        | None -> None
      in
      P (id, default_opt)

and map_params (env : env) ((v1, v2, v3) : CST.params) : parameter list =
  let v1 = map_param env v1 in
  let v2 =
    List_.map
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
    (fun () -> Tree_sitter_jsonnet.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      let e = map_document env cst in
      e)

let parse_pattern str =
  H.wrap_parser
    (fun () -> Tree_sitter_jsonnet.Parse.string str)
    (fun cst _extras ->
      (* TODO: use Origin!! *)
      let file = Fpath.v "<pattern>" in
      let env = { H.file; conv = H.line_col_to_pos_pattern str; extra = () } in
      let e = map_document env cst in
      E e)
