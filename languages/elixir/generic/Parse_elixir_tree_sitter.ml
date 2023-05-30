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
module CST = Tree_sitter_elixir.CST
open AST_elixir
module G = AST_generic
module H = Parse_tree_sitter_helpers
module H2 = AST_generic_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Elixir parser using tree-sitter-lang/semgrep-elixir and converting to
 * AST_elixir.ml
 *
 * references:
 * - https://hexdocs.pm/elixir/syntax-reference.html
 * - https://github.com/elixir-lang/tree-sitter-elixir/blob/main/docs/parser.md
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type env = unit H.env

let token = H.token
let str = H.str
let fb = Tok.unsafe_fake_bracket

(* helper to factorize code *)
let map_trailing_comma env v1 : Tok.t option =
  match v1 with
  | Some tok -> Some ((* "," *) token env tok)
  | None -> None

let map_before_unary_op_opt env v1 =
  match v1 with
  | Some tok -> Some ((* before_unary_op *) token env tok)
  | None -> None

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-elixir/Boilerplate.ml *)

(* less: actually can contain '.' *)
let map_alias (env : env) (x : CST.alias) : alias = str env x

let map_boolean (env : env) (x : CST.boolean) : bool wrap =
  match x with
  | `True tok -> (true, (* "true" *) token env tok)
  | `False tok -> (false, (* "false" *) token env tok)

let map_anon_choice_PLUS_8019319 (env : env) (x : CST.anon_choice_PLUS_8019319)
    : ident =
  match x with
  | `PLUS tok -> (* "+" *) str env tok
  | `DASH tok -> (* "-" *) str env tok
  | `BANG tok -> (* "!" *) str env tok
  | `HAT tok -> (* "^" *) str env tok
  | `TILDETILDETILDE tok -> (* "~~~" *) str env tok
  | `Not tok -> (* "not" *) str env tok

let map_terminator (env : env) (x : CST.terminator) : Tok.t list =
  match x with
  | `Rep_pat_509ec78_SEMI (v1, v2) ->
      let v1 = Common.map (token env (* pattern \r?\n *)) v1 in
      let v2 = (* ";" *) token env v2 in
      v1 @ [ v2 ]
  | `Rep1_pat_509ec78 xs -> Common.map (token env (* pattern \r?\n *)) xs

(* helper to factorize code *)
let map_terminator_opt env v1 : Tok.t list option =
  match v1 with
  | Some x -> Some (map_terminator env x)
  | None -> None

let map_identifier (env : env) (x : CST.identifier) : ident =
  match x with
  | `Choice_pat_cf9c6c3 y -> (
      match y with
      | `Pat_cf9c6c3 tok ->
          (* pattern [_\p{Ll}\p{Lm}\p{Lo}\p{Nl}\u1885\u1886\u2118\u212E\u309B\u309C][\p{ID_Continue}]*[?!]? *)
          str env tok
      (* TODO: part of elixir! not a semgrep construct! *)
      | `DOTDOTDOT tok -> (* "..." *) str env tok)
  | `Semg_meta tok -> str env tok

let map_identifier_or_ellipsis (env : env) (x : CST.identifier) : expr =
  match x with
  | `Choice_pat_cf9c6c3 y -> (
      match y with
      | `Pat_cf9c6c3 tok ->
          (* pattern [_\p{Ll}\p{Lm}\p{Lo}\p{Nl}\u1885\u1886\u2118\u212E\u309B\u309C][\p{ID_Continue}]*[?!]? *)
          let id = str env tok in
          N (H2.name_of_id id) |> G.e
      (* TODO: part of elixir! not a semgrep construct! *)
      | `DOTDOTDOT tok ->
          let tk = (* "..." *) token env tok in
          Ellipsis tk |> G.e)
  | `Semg_meta tok ->
      let id = str env tok in
      N (H2.name_of_id id) |> G.e

let map_quoted_xxx (env : env) (v1, v2, v3) : string wrap bracket =
  let l = (* "/" or another one *) token env v1 in
  let xs =
    Common.map
      (fun x ->
        match x with
        | `Quoted_content_slash tok
        | `Quoted_content_here_double tok
        | `Quoted_content_single tok
        | `Quoted_content_bar tok
        | `Quoted_content_curl tok
        | `Quoted_content_here_single tok
        | `Quoted_content_square tok
        | `Quoted_content_paren tok
        | `Quoted_content_double tok
        | `Quoted_content_angle tok ->
            str env tok
        | `Esc_seq tok -> (* escape_sequence *) str env tok)
      v2
  in
  let r = (* "/" or another one *) token env v3 in
  G.string_ (l, xs, r)

let map_quoted_i_xxx map_interpolation (env : env) (v1, v2, v3) : expr =
  let v1 = (* "<" or another one *) token env v1 in
  let v2 =
    Common.map
      (fun x ->
        match x with
        | `Quoted_content_i_angle tok
        | `Quoted_content_i_bar tok
        | `Quoted_content_i_curl tok
        | `Quoted_content_i_double tok
        | `Quoted_content_i_here_double tok
        | `Quoted_content_i_here_single tok
        | `Quoted_content_i_paren tok
        | `Quoted_content_i_single tok
        | `Quoted_content_i_slash tok
        | `Quoted_content_i_square tok ->
            Left3 (* quoted_content_i_angle *) (str env tok)
        | `Interp x ->
            let l, e, r = map_interpolation env x in
            Right3 (l, Some e, r)
        | `Esc_seq tok -> Left3 ((* escape_sequence *) str env tok))
      v2
  in
  let v3 = (* ">" or another one *) token env v3 in
  G.interpolated (v1, v2, v3)

let map_quoted_slash (env : env) ((v1, v2, v3) : CST.quoted_slash) =
  map_quoted_xxx env (v1, v2, v3)

let map_quoted_heredoc_double (env : env)
    ((v1, v2, v3) : CST.quoted_heredoc_double) =
  map_quoted_xxx env (v1, v2, v3)

let map_quoted_single (env : env) ((v1, v2, v3) : CST.quoted_single) =
  map_quoted_xxx env (v1, v2, v3)

let map_quoted_angle (env : env) ((v1, v2, v3) : CST.quoted_angle) =
  map_quoted_xxx env (v1, v2, v3)

let map_quoted_double (env : env) ((v1, v2, v3) : CST.quoted_double) =
  map_quoted_xxx env (v1, v2, v3)

let map_quoted_parenthesis (env : env) ((v1, v2, v3) : CST.quoted_parenthesis) =
  map_quoted_xxx env (v1, v2, v3)

let map_quoted_square (env : env) ((v1, v2, v3) : CST.quoted_square) =
  map_quoted_xxx env (v1, v2, v3)

let map_quoted_heredoc_single (env : env)
    ((v1, v2, v3) : CST.quoted_heredoc_single) =
  map_quoted_xxx env (v1, v2, v3)

let map_quoted_curly (env : env) ((v1, v2, v3) : CST.quoted_curly) =
  map_quoted_xxx env (v1, v2, v3)

let map_quoted_bar (env : env) ((v1, v2, v3) : CST.quoted_bar) =
  map_quoted_xxx env (v1, v2, v3)

let map_operator_identifier (env : env) (x : CST.operator_identifier) : ident =
  match x with
  | `AMP tok -> (* "&" *) str env tok
  | `Choice_PLUS x -> map_anon_choice_PLUS_8019319 env x
  | `AT tok -> (* "@" *) str env tok
  | `LTDASH tok -> (* "<-" *) str env tok
  | `BSLASHBSLASH tok -> (* "\\\\" *) str env tok
  | `When tok -> (* "when" *) str env tok
  | `COLONCOLON tok -> (* "::" *) str env tok
  | `BAR tok -> (* "|" *) str env tok
  | `EQ tok -> (* "=" *) str env tok
  | `BARBAR tok -> (* "||" *) str env tok
  | `BARBARBAR tok -> (* "|||" *) str env tok
  | `Or tok -> (* "or" *) str env tok
  | `AMPAMP tok -> (* "&&" *) str env tok
  | `AMPAMPAMP tok -> (* "&&&" *) str env tok
  | `And tok -> (* "and" *) str env tok
  | `EQEQ tok -> (* "==" *) str env tok
  | `BANGEQ tok -> (* "!=" *) str env tok
  | `EQTILDE tok -> (* "=~" *) str env tok
  | `EQEQEQ tok -> (* "===" *) str env tok
  | `BANGEQEQ tok -> (* "!==" *) str env tok
  | `LT tok -> (* "<" *) str env tok
  | `GT tok -> (* ">" *) str env tok
  | `LTEQ tok -> (* "<=" *) str env tok
  | `GTEQ tok -> (* ">=" *) str env tok
  | `BARGT tok -> (* "|>" *) str env tok
  | `LTLTLT tok -> (* "<<<" *) str env tok
  | `GTGTGT tok -> (* ">>>" *) str env tok
  | `LTLTTILDE tok -> (* "<<~" *) str env tok
  | `TILDEGTGT tok -> (* "~>>" *) str env tok
  | `LTTILDE tok -> (* "<~" *) str env tok
  | `TILDEGT tok -> (* "~>" *) str env tok
  | `LTTILDEGT tok -> (* "<~>" *) str env tok
  | `LTBARGT tok -> (* "<|>" *) str env tok
  | `In tok -> (* "in" *) str env tok
  | `Not_in tok -> (* not_in *) str env tok
  | `HATHAT tok -> (* "^^" *) str env tok
  | `PLUSPLUS tok -> (* "++" *) str env tok
  | `DASHDASH tok -> (* "--" *) str env tok
  | `PLUSPLUSPLUS tok -> (* "+++" *) str env tok
  | `DASHDASHDASH tok -> (* "---" *) str env tok
  | `DOTDOT tok -> (* ".." *) str env tok
  | `LTGT tok -> (* "<>" *) str env tok
  | `STAR tok -> (* "*" *) str env tok
  | `SLASH tok -> (* "/" *) str env tok
  | `STARSTAR tok -> (* "**" *) str env tok
  | `DASHGT tok -> (* "->" *) str env tok
  | `DOT tok -> (* "." *) str env tok

let rec map_after_block (env : env) ((v1, v2, v3) : CST.after_block) =
  let v1 = (* "after" *) str env v1 in
  let _v2 = map_terminator_opt env v2 in
  let v3 =
    match v3 with
    | Some x ->
        map_anon_choice_choice_stab_clause_rep_term_choice_stab_clause_b295119
          env x
    | None -> Left []
  in
  (v1, v3)

and map_anon_choice_choice_stab_clause_rep_term_choice_stab_clause_b295119
    (env : env)
    (x : CST.anon_choice_choice_stab_clause_rep_term_choice_stab_clause_b295119)
    : body_or_clauses =
  match x with
  | `Choice_stab_clause_rep_term_choice_stab_clause (v1, v2) ->
      let v1 =
        match v1 with
        | `Stab_clause x -> map_stab_clause env x
      in
      let v2 = Common.map (map_anon_term_choice_stab_clause_70647b7 env) v2 in
      Right (v1 :: v2)
  | `Choice_exp_rep_term_choice_exp_opt_term (v1, v2, v3) ->
      let v1 =
        match v1 with
        | `Exp x -> map_expression env x
      in
      let v2 = Common.map (map_anon_term_choice_exp_996111b env) v2 in
      let _v3 = map_terminator_opt env v3 in
      Left (v1 :: v2)

and map_anon_choice_exp_0094635 (env : env) (x : CST.anon_choice_exp_0094635) =
  match x with
  | `Exp x ->
      let e = map_expression env x in
      Left e
  | `Keywos x ->
      let xs = map_keywords env x in
      Right xs

and map_anon_choice_quoted_i_double_d7d5f65 (env : env)
    (x : CST.anon_choice_quoted_i_double_d7d5f65) : expr =
  match x with
  | `Quoted_i_double x -> map_quoted_i_double env x
  | `Quoted_i_single x -> map_quoted_i_single env x

and map_anon_exp_rep_COMMA_exp_opt_COMMA_keywos_041d82e (env : env)
    ((v1, v2, v3) : CST.anon_exp_rep_COMMA_exp_opt_COMMA_keywos_041d82e) :
    argument list =
  let v1 = map_expression env v1 in
  let v2 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_expression env v2 in
        v2)
      v2
  in
  let v3 =
    match v3 with
    | Some (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_keywords env v2 in
        v2
    | None -> []
  in
  Elixir_to_generic.args_of_exprs_and_keywords (v1 :: v2) v3

and map_anon_opt_opt_nl_before_do_do_blk_3eff85f (env : env)
    (opt : CST.anon_opt_opt_nl_before_do_do_blk_3eff85f) : do_block option =
  match opt with
  | Some (v1, v2) ->
      let _v1 =
        match v1 with
        | Some tok -> Some ((* newline_before_do *) token env tok)
        | None -> None
      in
      let v2 = map_do_block env v2 in
      Some v2
  | None -> None

and map_anon_term_choice_exp_996111b (env : env)
    ((v1, v2) : CST.anon_term_choice_exp_996111b) : expr =
  let _v1 = map_terminator env v1 in
  let v2 =
    match v2 with
    | `Exp x -> map_expression env x
  in
  v2

and map_anon_term_choice_stab_clause_70647b7 (env : env)
    ((v1, v2) : CST.anon_term_choice_stab_clause_70647b7) : stab_clause =
  let _v1 = map_terminator env v1 in
  let v2 =
    match v2 with
    | `Stab_clause x -> map_stab_clause env x
  in
  v2

and map_anonymous_call (env : env) ((v1, v2) : CST.anonymous_call) : call =
  let e, tdot = map_anonymous_dot env v1 in
  let args = map_call_arguments_with_parentheses env v2 in
  let anon_fld = G.FDynamic (G.OtherExpr (("AnonDotField", tdot), []) |> G.e) in
  let e = G.DotAccess (e, tdot, anon_fld) |> G.e in
  Elixir_to_generic.mk_call_parens e args None

and map_anonymous_dot (env : env) ((v1, v2) : CST.anonymous_dot) =
  let v1 = map_expression env v1 in
  let v2 = (* "." *) token env v2 in
  (v1, v2)

and map_atom (env : env) (x : CST.atom) : expr =
  match x with
  | `Atom_ tok ->
      (* less: should remove the leading ':' from x *)
      let x = (* atom_ *) str env tok in
      G.L (G.Atom (G.fake ":", x)) |> G.e
  | `Quoted_atom (v1, v2) ->
      let t = (* quoted_atom_start *) token env v1 in
      let str = map_anon_choice_quoted_i_double_d7d5f65 env v2 in
      G.OtherExpr (("AtomExpr", t), [ E str ]) |> G.e

and map_binary_operator (env : env) (x : CST.binary_operator) : expr =
  match x with
  | `Exp_choice_LTDASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `LTDASH tok -> (* "<-" *) str env tok
        (* TODO: default parameter syntax *)
        | `BSLASHBSLASH tok -> (* "\\\\" *) str env tok
      in
      let v3 = map_expression env v3 in
      Elixir_to_generic.binary_call v1 (Left v2) v3
  | `Exp_when_choice_exp (v1, v2, v3) ->
      let e1 = map_expression env v1 in
      let twhen = (* "when" *) token env v2 in
      let e_or_kwds = map_anon_choice_exp_0094635 env v3 in
      OtherExpr
        ( ("When", twhen),
          [ E e1; E (Elixir_to_generic.expr_of_e_or_kwds e_or_kwds) ] )
      |> G.e
  | `Exp_COLONCOLON_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "::" *) str env v2 in
      let v3 = map_expression env v3 in
      Elixir_to_generic.binary_call v1 (Left v2) v3
  | `Exp_BAR_choice_exp (v1, v2, v3) ->
      let e1 = map_expression env v1 in
      (* join operator (=~ "::" in OCaml, comes from Prolog/Erlang) *)
      let tbar = (* "|" *) token env v2 in
      let e_or_kwds = map_anon_choice_exp_0094635 env v3 in
      G.OtherExpr
        ( ("Join", tbar),
          [ G.E e1; G.E (Elixir_to_generic.expr_of_e_or_kwds e_or_kwds) ] )
      |> G.e
  | `Exp_EQGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      (* less: used in Maps, should convert in 'pair'? *)
      let v2 = (* "=>" *) str env v2 in
      let v3 = map_expression env v3 in
      Elixir_to_generic.binary_call v1 (Left v2) v3
  | `Exp_EQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      Assign (v1, v2, v3) |> G.e
  | `Exp_choice_BARBAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `BARBAR tok -> Right (G.Or, (* "||" *) token env tok)
        | `BARBARBAR tok -> Left ((* "|||" *) str env tok)
        | `Or tok -> Left ((* "or" *) str env tok)
      in
      let v3 = map_expression env v3 in
      Elixir_to_generic.binary_call v1 v2 v3
  | `Exp_choice_AMPAMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `AMPAMP tok -> Right (G.And, (* "&&" *) token env tok)
        | `AMPAMPAMP tok -> Left ((* "&&&" *) str env tok)
        | `And tok -> Left ((* "and" *) str env tok)
      in
      let v3 = map_expression env v3 in
      Elixir_to_generic.binary_call v1 v2 v3
  | `Exp_choice_EQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `EQEQ tok -> Right (G.Eq, (* "==" *) token env tok)
        | `BANGEQ tok -> Right (G.NotEq, (* "!=" *) token env tok)
        | `EQTILDE tok -> Right (G.RegexpMatch, (* "=~" *) token env tok)
        | `EQEQEQ tok -> Right (G.PhysEq, (* "===" *) token env tok)
        | `BANGEQEQ tok -> Right (G.NotPhysEq, (* "!==" *) token env tok)
      in
      let v3 = map_expression env v3 in
      Elixir_to_generic.binary_call v1 v2 v3
  | `Exp_choice_LT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `LT tok -> (* "<" *) (G.Lt, token env tok)
        | `GT tok -> (* ">" *) (G.Gt, token env tok)
        | `LTEQ tok -> (* "<=" *) (G.LtE, token env tok)
        | `GTEQ tok -> (* ">=" *) (G.GtE, token env tok)
      in
      let v3 = map_expression env v3 in
      Elixir_to_generic.binary_call v1 (Right v2) v3
  | `Exp_choice_BARGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `BARGT tok -> (* "|>" *) str env tok
        | `LTLTLT tok -> (* "<<<" *) str env tok
        | `GTGTGT tok -> (* ">>>" *) str env tok
        | `LTLTTILDE tok -> (* "<<~" *) str env tok
        | `TILDEGTGT tok -> (* "~>>" *) str env tok
        | `LTTILDE tok -> (* "<~" *) str env tok
        | `TILDEGT tok -> (* "~>" *) str env tok
        | `LTTILDEGT tok -> (* "<~>" *) str env tok
        | `LTBARGT tok -> (* "<|>" *) str env tok
      in
      let v3 = map_expression env v3 in
      Elixir_to_generic.binary_call v1 (Left v2) v3
  | `Exp_choice_in_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `In tok -> (* "in" *) (G.In, token env tok)
        | `Not_in tok -> (* not_in *) (G.NotIn, token env tok)
      in
      let v3 = map_expression env v3 in
      Elixir_to_generic.binary_call v1 (Right v2) v3
  | `Exp_HATHATHAT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "^^^" *) str env v2 in
      let v3 = map_expression env v3 in
      Elixir_to_generic.binary_call v1 (Left v2) v3
  | `Exp_SLASHSLASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "//" *) str env v2 in
      let v3 = map_expression env v3 in
      Elixir_to_generic.binary_call v1 (Left v2) v3
  | `Exp_choice_PLUSPLUS_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `PLUSPLUS tok -> (* "++" *) str env tok
        | `DASHDASH tok -> (* "--" *) str env tok
        | `PLUSPLUSPLUS tok -> (* "+++" *) str env tok
        | `DASHDASHDASH tok -> (* "---" *) str env tok
        | `DOTDOT tok -> (* ".." *) str env tok
        | `LTGT tok -> (* "<>" *) str env tok
      in
      let v3 = map_expression env v3 in
      Elixir_to_generic.binary_call v1 (Left v2) v3
  | `Exp_choice_PLUS_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `PLUS tok -> (G.Plus, (* "+" *) token env tok)
        | `DASH tok -> (G.Minus, (* "-" *) token env tok)
      in
      let v3 = map_expression env v3 in
      Elixir_to_generic.binary_call v1 (Right v2) v3
  | `Exp_choice_STAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `STAR tok -> (G.Mult, (* "*" *) token env tok)
        | `SLASH tok -> (G.Div, (* "/" *) token env tok)
      in
      let v3 = map_expression env v3 in
      Elixir_to_generic.binary_call v1 (Right v2) v3
  | `Exp_STARSTAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "**" *) (G.Pow, token env v2) in
      let v3 = map_expression env v3 in
      Elixir_to_generic.binary_call v1 (Right v2) v3
  | `Op_id_SLASH_int (v1, v2, v3) ->
      let id = map_operator_identifier env v1 in
      let tslash = (* "/" *) token env v2 in
      let s, t = (* integer *) str env v3 in
      let e = G.L (G.Int (int_of_string_opt s, t)) |> G.e in
      G.OtherExpr (("OpSlashInt", tslash), [ G.I id; G.E e ]) |> G.e

and map_body (env : env) ((v1, v2, v3, v4) : CST.body) : body =
  let _v1 = map_terminator_opt env v1 in
  let v2 = map_expression env v2 in
  let v3 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = map_terminator env v1 in
        let v2 = map_expression env v2 in
        v2)
      v3
  in
  let _v4 = map_terminator_opt env v4 in
  v2 :: v3

and map_call (env : env) (x : CST.call) : call =
  match x with
  | `Call_with_parens_b98484c x -> map_call_without_parentheses env x
  | `Call_with_parens_403315d x -> map_call_with_parentheses env x

and map_call_arguments_with_parentheses (env : env)
    ((v1, v2, v3) : CST.call_arguments_with_parentheses) : argument list bracket
    =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> map_call_arguments_with_trailing_separator env x
    | None -> []
  in
  let v3 = (* ")" *) token env v3 in
  (v1, v2, v3)

and map_call_arguments_with_parentheses_immediate (env : env)
    ((v1, v2, v3) : CST.call_arguments_with_parentheses_immediate) :
    argument list bracket =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> map_call_arguments_with_trailing_separator env x
    | None -> []
  in
  let v3 = (* ")" *) token env v3 in
  (v1, v2, v3)

and map_call_arguments_with_trailing_separator (env : env)
    (x : CST.call_arguments_with_trailing_separator) : argument list =
  match x with
  | `Exp_rep_COMMA_exp_opt_COMMA_keywos_with_trai_sepa (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        Common.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_expression env v2 in
            v2)
          v2
      in
      let v3 =
        match v3 with
        | Some (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_keywords_with_trailing_separator env v2 in
            v2
        | None -> []
      in
      Elixir_to_generic.args_of_exprs_and_keywords (v1 :: v2) v3
  | `Keywos_with_trai_sepa x ->
      let xs = map_keywords_with_trailing_separator env x in
      Elixir_to_generic.args_of_exprs_and_keywords [] xs

and map_call_arguments_without_parentheses (env : env)
    (x : CST.call_arguments_without_parentheses) : argument list =
  match x with
  | `Exp_rep_COMMA_exp_opt_COMMA_keywos x ->
      map_anon_exp_rep_COMMA_exp_opt_COMMA_keywos_041d82e env x
  | `Keywos x ->
      let xs = map_keywords env x in
      Elixir_to_generic.args_of_exprs_and_keywords [] xs

and map_call_with_parentheses (env : env) (x : CST.call_with_parentheses) : call
    =
  match x with
  | `Local_call_with_parens x -> map_local_call_with_parentheses env x
  | `Remote_call_with_parens x -> map_remote_call_with_parentheses env x
  | `Anon_call x -> map_anonymous_call env x
  | `Double_call (v1, v2, v3) ->
      let call1 : call =
        match v1 with
        | `Local_call_with_parens x -> map_local_call_with_parentheses env x
        | `Remote_call_with_parens x -> map_remote_call_with_parentheses env x
        | `Anon_call x -> map_anonymous_call env x
      in
      let args = map_call_arguments_with_parentheses env v2 in
      let blopt = map_anon_opt_opt_nl_before_do_do_blk_3eff85f env v3 in
      Elixir_to_generic.mk_call_parens call1 args blopt

and map_call_without_parentheses (env : env) (x : CST.call_without_parentheses)
    : call =
  match x with
  | `Local_call_with_parens (v1, v2, v3) ->
      let id = map_identifier env v1 in
      let args = map_call_arguments_without_parentheses env v2 in
      let blopt = map_anon_opt_opt_nl_before_do_do_blk_3eff85f env v3 in
      let e = G.N (H2.name_of_id id) |> G.e in
      Elixir_to_generic.mk_call_no_parens e args blopt
  | `Local_call_just_do_blk (v1, v2) ->
      let id = map_identifier env v1 in
      let bl = map_do_block env v2 in
      let e = G.N (H2.name_of_id id) |> G.e in
      Elixir_to_generic.mk_call_no_parens e [] (Some bl)
  | `Remote_call_with_parens (v1, v2, v3) ->
      let e = map_remote_dot env v1 in
      let args =
        match v2 with
        | Some x -> map_call_arguments_without_parentheses env x
        | None -> []
      in
      let blopt = map_anon_opt_opt_nl_before_do_do_blk_3eff85f env v3 in
      Elixir_to_generic.mk_call_no_parens e args blopt

and map_capture_expression (env : env) (x : CST.capture_expression) : expr =
  match x with
  | `LPAR_exp_RPAR (v1, v2, v3) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* ")" *) token env v3 in
      AST_generic_helpers.set_e_range v1 v3 v2;
      v2
  | `Exp x -> map_expression env x

and map_catch_block (env : env) ((v1, v2, v3) : CST.catch_block) =
  let v1 = (* "catch" *) str env v1 in
  let _v2 = map_terminator_opt env v2 in
  let v3 =
    match v3 with
    | Some x ->
        map_anon_choice_choice_stab_clause_rep_term_choice_stab_clause_b295119
          env x
    | None -> Left []
  in
  (v1, v3)

and map_charlist (env : env) (x : CST.charlist) : expr =
  match x with
  | `Quoted_i_single x -> map_quoted_i_single env x
  | `Quoted_i_here_single x -> map_quoted_i_heredoc_single env x

and map_do_block (env : env) ((v1, v2, v3, v4, v5) : CST.do_block) : do_block =
  let tdo = (* "do" *) token env v1 in
  let _ = map_terminator_opt env v2 in
  let xs =
    match v3 with
    | Some x ->
        map_anon_choice_choice_stab_clause_rep_term_choice_stab_clause_b295119
          env x
    | None -> Left []
  in
  let extras =
    Common.map
      (fun x ->
        match x with
        | `After_blk x -> map_after_block env x
        | `Rescue_blk x -> map_rescue_block env x
        | `Catch_blk x -> map_catch_block env x
        | `Else_blk x -> map_else_block env x)
      v4
  in
  let tend = (* "end" *) token env v5 in
  (tdo, (xs, extras), tend)

and map_dot (env : env) ((v1, v2, v3) : CST.dot) : expr =
  let e = map_expression env v1 in
  let tdot = (* "." *) token env v2 in
  let v3 =
    match v3 with
    | `Alias tok ->
        let al = map_alias env tok in
        DotAccess (e, tdot, FN (H2.name_of_id al)) |> G.e
    | `Tuple x ->
        let l, xs, r = map_tuple env x in
        let tuple = Container (Tuple, (l, xs, r)) |> G.e in
        DotAccess (e, tdot, FDynamic tuple) |> G.e
  in
  v3

and map_else_block (env : env) ((v1, v2, v3) : CST.else_block) =
  let v1 = (* "else" *) str env v1 in
  let _v2 = map_terminator_opt env v2 in
  let v3 =
    match v3 with
    | Some x ->
        map_anon_choice_choice_stab_clause_rep_term_choice_stab_clause_b295119
          env x
    | None -> Left []
  in
  (v1, v3)

and map_expression (env : env) (x : CST.expression) : expr =
  match x with
  (* semgrep-ext: *)
  | `Deep_ellips (v1, v2, v3) ->
      let l = token env v1 in
      let e = map_expression env v2 in
      let r = token env v3 in
      DeepEllipsis (l, e, r) |> G.e
  | `Blk (v1, v2, v3, v4) ->
      let l = (* "(" *) token env v1 in
      let _ = map_terminator_opt env v2 in
      let xs =
        match v3 with
        | Some x ->
            map_anon_choice_choice_stab_clause_rep_term_choice_stab_clause_b295119
              env x
        | None -> Left []
      in
      let r = (* ")" *) token env v4 in
      let blk : block = (l, xs, r) in
      Elixir_to_generic.expr_of_block blk
  | `Id x -> map_identifier_or_ellipsis env x
  | `Alias tok ->
      let al = map_alias env tok in
      (* less: should return a Constructor instead? *)
      N (H2.name_of_id al) |> G.e
  | `Int tok ->
      let s, t = (* integer *) str env tok in
      L (Int (int_of_string_opt s, t)) |> G.e
  | `Float tok ->
      let s, t = (* float *) str env tok in
      L (Float (float_of_string_opt s, t)) |> G.e
  | `Char_a87deb0 tok ->
      let x = (* pattern \?(.|\\.) *) str env tok in
      L (Char x) |> G.e
  | `Bool x ->
      let b = map_boolean env x in
      L (Bool b) |> G.e
  | `Nil tok ->
      let nil = (* "nil" *) token env tok in
      L (Null nil) |> G.e
  | `Atom x -> map_atom env x
  | `Str x -> map_string_ env x
  | `Char_a593f90 x -> map_charlist env x
  | `Sigil (v1, v2, v3) ->
      let ttilde = (* "~" *) token env v1 in
      let letter, any =
        match v2 with
        | `Imm_tok_pat_0db2d54_choice_quoted_i_double (v1, v2) ->
            let lower = (* pattern [a-z] *) str env v1 in
            let quoted =
              match v2 with
              | `Quoted_i_double x -> map_quoted_i_double env x
              | `Quoted_i_single x -> map_quoted_i_single env x
              | `Quoted_i_here_single x -> map_quoted_i_heredoc_single env x
              | `Quoted_i_here_double x -> map_quoted_i_heredoc_double env x
              | `Quoted_i_paren x -> map_quoted_i_parenthesis env x
              | `Quoted_i_curl x -> map_quoted_i_curly env x
              | `Quoted_i_square x -> map_quoted_i_square env x
              | `Quoted_i_angle x -> map_quoted_i_angle env x
              | `Quoted_i_bar x -> map_quoted_i_bar env x
              | `Quoted_i_slash x -> map_quoted_i_slash env x
            in
            (lower, G.E quoted)
        | `Imm_tok_pat_562b724_choice_quoted_double (v1, v2) ->
            let upper = (* pattern [A-Z] *) str env v1 in
            let quoted =
              match v2 with
              | `Quoted_double x -> map_quoted_double env x
              | `Quoted_single x -> map_quoted_single env x
              | `Quoted_here_single x -> map_quoted_heredoc_single env x
              | `Quoted_here_double x -> map_quoted_heredoc_double env x
              | `Quoted_paren x -> map_quoted_parenthesis env x
              | `Quoted_curl x -> map_quoted_curly env x
              | `Quoted_square x -> map_quoted_square env x
              | `Quoted_angle x -> map_quoted_angle env x
              | `Quoted_bar x -> map_quoted_bar env x
              | `Quoted_slash x -> map_quoted_slash env x
            in
            (upper, Str quoted)
      in
      let idopt =
        match v3 with
        | Some tok -> [ G.I ((* pattern [a-zA-Z0-9]+ *) str env tok) ]
        | None -> []
      in
      G.OtherExpr (("Sigil", ttilde), [ G.I letter; any ] @ idopt) |> G.e
  | `List (v1, v2, v3) ->
      let l = (* "[" *) token env v1 in
      let xs =
        match v2 with
        | Some x -> map_items_with_trailing_separator env x
        | None -> []
      in
      let r = (* "]" *) token env v3 in
      G.Container (G.List, (l, xs, r)) |> G.e
  | `Tuple x ->
      let l, xs, r = map_tuple env x in
      G.Container (G.Tuple, (l, xs, r)) |> G.e
  | `Bits (v1, v2, v3) ->
      let l = (* "<<" *) token env v1 in
      let xs =
        match v2 with
        | Some x -> map_items_with_trailing_separator env x
        | None -> []
      in
      let r = (* ">>" *) token env v3 in
      G.OtherExpr
        (("ContainerBits", l), (xs |> Common.map (fun e -> G.E e)) @ [ G.Tk r ])
      |> G.e
  | `Map (v1, v2, v3, v4, v5) -> (
      let tpercent = (* "%" *) token env v1 in
      let struct_opt =
        match v2 with
        | Some x -> Some (map_struct_ env x)
        | None -> None
      in
      let l = (* "{" *) token env v3 in
      let xs =
        match v4 with
        | Some x -> map_items_with_trailing_separator env x
        | None -> []
      in
      let r = (* "}" *) token env v5 in
      let container = G.Container (G.Dict, (l, xs, r)) |> G.e in
      match struct_opt with
      | None -> container
      | Some (Left id) ->
          let n = H2.name_of_id id in
          let ty = G.TyN n |> G.t in
          G.New (tpercent, ty, G.empty_id_info (), (l, Common.map G.arg xs, r))
          |> G.e
      | Some (Right e) -> G.Call (e, (l, Common.map G.arg xs, r)) |> G.e)
  | `Un_op x -> map_unary_operator env x
  | `Bin_op x -> map_binary_operator env x
  | `Dot x -> map_dot env x
  | `Call x -> map_call env x
  (* semantic: transformed in Access.get/2 *)
  | `Access_call (v1, v2, v3, v4) ->
      let v1 = map_expression env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* "]" *) token env v4 in
      ArrayAccess (v1, (v2, v3, v4)) |> G.e
  | `Anon_func (v1, v2, v3, v4, v5) ->
      let tfn = (* "fn" *) token env v1 in
      let _v2 = map_terminator_opt env v2 in
      let x = map_stab_clause env v3 in
      let xs =
        Common.map
          (fun (v1, v2) ->
            let _v1 = map_terminator env v1 in
            let v2 = map_stab_clause env v2 in
            v2)
          v4
      in
      let _v5TODO = (* "end" *) token env v5 in
      let clauses = x :: xs in
      let fdef =
        Elixir_to_generic.stab_clauses_to_function_definition tfn clauses
      in
      let fdef = { fdef with fkind = (LambdaKind, tfn) } in
      Lambda fdef |> G.e

and map_interpolation (env : env) ((v1, v2, v3) : CST.interpolation) :
    expr bracket =
  let v1 = (* "#{" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* "}" *) token env v3 in
  (v1, v2, v3)

and map_items_with_trailing_separator (env : env)
    (v1 : CST.items_with_trailing_separator) : item list =
  match v1 with
  | `Exp_rep_COMMA_exp_opt_COMMA (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        Common.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_expression env v2 in
            v2)
          v2
      in
      let _v3 = map_trailing_comma env v3 in
      v1 :: v2
  | `Opt_exp_rep_COMMA_exp_COMMA_keywos_with_trai_sepa (v1, v2) ->
      let v1 =
        match v1 with
        | Some (v1, v2, v3) ->
            let v1 = map_expression env v1 in
            let v2 =
              Common.map
                (fun (v1, v2) ->
                  let _v1 = (* "," *) token env v1 in
                  let v2 = map_expression env v2 in
                  v2)
                v2
            in
            let _v3 = (* "," *) token env v3 in
            v1 :: v2
        | None -> []
      in
      let xs = map_keywords_with_trailing_separator env v2 in
      Elixir_to_generic.items_of_exprs_and_keywords v1 xs

and map_keyword (env : env) (x : CST.keyword) : keyword =
  match x with
  | `Kw_ tok ->
      (* todo: remove suffix ':' *)
      let x = (* keyword_ *) str env tok in
      L (String (fb x)) |> G.e
  | `Quoted_kw (v1, v2) ->
      let v1 = map_anon_choice_quoted_i_double_d7d5f65 env v1 in
      let _v2TODO = (* pattern :\s *) token env v2 in
      v1

and map_keywords (env : env) ((v1, v2) : CST.keywords) : pair list =
  let v1 = map_pair env v1 in
  let v2 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_pair env v2 in
        v2)
      v2
  in
  v1 :: v2

and map_keywords_with_trailing_separator (env : env)
    ((v1, v2, v3) : CST.keywords_with_trailing_separator) =
  let v1 = map_pair env v1 in
  let v2 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_pair env v2 in
        v2)
      v2
  in
  let _v3 = map_trailing_comma env v3 in
  v1 :: v2

and map_local_call_with_parentheses (env : env)
    ((v1, v2, v3) : CST.local_call_with_parentheses) =
  let id = map_identifier env v1 in
  let args = map_call_arguments_with_parentheses_immediate env v2 in
  let blopt = map_anon_opt_opt_nl_before_do_do_blk_3eff85f env v3 in
  let e = N (H2.name_of_id id) |> G.e in
  Elixir_to_generic.mk_call_parens e args blopt

and map_pair (env : env) ((v1, v2) : CST.pair) : pair =
  let v1 = map_keyword env v1 in
  let v2 = map_expression env v2 in
  (v1, v2)

and map_quoted_i_angle (env : env) ((v1, v2, v3) : CST.quoted_i_angle) =
  map_quoted_i_xxx map_interpolation env (v1, v2, v3)

and map_quoted_i_bar (env : env) ((v1, v2, v3) : CST.quoted_i_bar) =
  map_quoted_i_xxx map_interpolation env (v1, v2, v3)

and map_quoted_i_curly (env : env) ((v1, v2, v3) : CST.quoted_i_curly) =
  map_quoted_i_xxx map_interpolation env (v1, v2, v3)

and map_quoted_i_double (env : env) ((v1, v2, v3) : CST.quoted_i_double) =
  map_quoted_i_xxx map_interpolation env (v1, v2, v3)

and map_quoted_i_heredoc_double (env : env)
    ((v1, v2, v3) : CST.quoted_i_heredoc_double) =
  map_quoted_i_xxx map_interpolation env (v1, v2, v3)

and map_quoted_i_heredoc_single (env : env)
    ((v1, v2, v3) : CST.quoted_i_heredoc_single) =
  map_quoted_i_xxx map_interpolation env (v1, v2, v3)

and map_quoted_i_parenthesis (env : env)
    ((v1, v2, v3) : CST.quoted_i_parenthesis) =
  map_quoted_i_xxx map_interpolation env (v1, v2, v3)

and map_quoted_i_single (env : env) ((v1, v2, v3) : CST.quoted_i_single) =
  map_quoted_i_xxx map_interpolation env (v1, v2, v3)

and map_quoted_i_slash (env : env) ((v1, v2, v3) : CST.quoted_i_slash) =
  map_quoted_i_xxx map_interpolation env (v1, v2, v3)

and map_quoted_i_square (env : env) ((v1, v2, v3) : CST.quoted_i_square) =
  map_quoted_i_xxx map_interpolation env (v1, v2, v3)

and map_remote_call_with_parentheses (env : env)
    ((v1, v2, v3) : CST.remote_call_with_parentheses) =
  let e = map_remote_dot env v1 in
  let args = map_call_arguments_with_parentheses_immediate env v2 in
  let blopt = map_anon_opt_opt_nl_before_do_do_blk_3eff85f env v3 in
  Elixir_to_generic.mk_call_parens e args blopt

and map_remote_dot (env : env) ((v1, v2, v3) : CST.remote_dot) : expr =
  let e = map_expression env v1 in
  let tdot = (* "." *) token env v2 in
  let fld : G.field_name =
    match v3 with
    | `Id x ->
        let id = map_identifier env x in
        FN (H2.name_of_id id)
    | `Choice_and x ->
        FN
          (H2.name_of_id
             (match x with
             | `And tok -> (* "and" *) str env tok
             | `In tok -> (* "in" *) str env tok
             | `Not tok -> (* "not" *) str env tok
             | `Or tok -> (* "or" *) str env tok
             | `When tok -> (* "when" *) str env tok
             | `True tok -> (* "true" *) str env tok
             | `False tok -> (* "false" *) str env tok
             | `Nil tok -> (* "nil" *) str env tok
             | `After tok -> (* "after" *) str env tok
             | `Catch tok -> (* "catch" *) str env tok
             | `Do tok -> (* "do" *) str env tok
             | `Else tok -> (* "else" *) str env tok
             | `End tok -> (* "end" *) str env tok
             | `Fn tok -> (* "fn" *) str env tok
             | `Rescue tok -> (* "rescue" *) str env tok))
    | `Op_id x ->
        let id = map_operator_identifier env x in
        FN (H2.name_of_id id)
    | `Quoted_i_double x ->
        (* TODO: could detect of the map_quoted_i_xxx is a constant string *)
        let x = map_quoted_i_double env x in
        FDynamic x
    | `Quoted_i_single x ->
        let x = map_quoted_i_single env x in
        FDynamic x
  in
  DotAccess (e, tdot, fld) |> G.e

and map_rescue_block (env : env) ((v1, v2, v3) : CST.rescue_block) =
  let v1 = (* "rescue" *) str env v1 in
  let _v2 = map_terminator_opt env v2 in
  let v3 =
    match v3 with
    | Some x ->
        map_anon_choice_choice_stab_clause_rep_term_choice_stab_clause_b295119
          env x
    | None -> Left []
  in
  (v1, v3)

and map_stab_clause (env : env) ((v1, v2, v3) : CST.stab_clause) : stab_clause =
  let v1 =
    match v1 with
    | Some x -> map_stab_clause_left env x
    | None -> ([], None)
  in
  let v2 = (* "->" *) token env v2 in
  let v3 =
    match v3 with
    | Some x -> map_body env x
    | None -> []
  in
  (v1, v2, v3)

and map_stab_clause_arguments_with_parentheses (env : env)
    ((v1, v2, v3) : CST.stab_clause_arguments_with_parentheses) :
    argument list bracket =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> map_stab_clause_arguments_without_parentheses env x
    | None -> []
  in
  let v3 = (* ")" *) token env v3 in
  (v1, v2, v3)

and map_stab_clause_arguments_without_parentheses (env : env)
    (x : CST.stab_clause_arguments_without_parentheses) : argument list =
  match x with
  | `Exp_rep_COMMA_exp_opt_COMMA_keywos x ->
      let xs = map_anon_exp_rep_COMMA_exp_opt_COMMA_keywos_041d82e env x in
      xs
  | `Keywos x ->
      let xs = map_keywords env x in
      Elixir_to_generic.args_of_exprs_and_keywords [] xs

(* we would prefer pattern list, but elixir is more general *)
and map_stab_clause_left (env : env) (x : CST.stab_clause_left) :
    argument list * (Tok.t * expr) option =
  match x with
  | `Stab_clause_args_with_parens_bf4a580 x ->
      let _, xs, _ = map_stab_clause_arguments_with_parentheses env x in
      (xs, None)
  | `Stab_clause_args_with_parens_with_guard_9d9f341 (v1, v2, v3) ->
      let _, xs, _ = map_stab_clause_arguments_with_parentheses env v1 in
      let twhen = (* "when" *) token env v2 in
      let e = map_expression env v3 in
      (xs, Some (twhen, e))
  | `Stab_clause_args_with_parens_a52ef95 x ->
      (map_stab_clause_arguments_without_parentheses env x, None)
  | `Stab_clause_args_with_parens_with_guard_cfbae3b (v1, v2, v3) ->
      let xs = map_stab_clause_arguments_without_parentheses env v1 in
      let twhen = (* "when" *) token env v2 in
      let e = map_expression env v3 in
      (xs, Some (twhen, e))

and map_string_ (env : env) (x : CST.string_) : expr =
  match x with
  | `Quoted_i_double x -> map_quoted_i_double env x
  | `Quoted_i_here_double x -> map_quoted_i_heredoc_double env x

and map_struct_ (env : env) (x : CST.struct_) : (ident, expr) either =
  match x with
  | `Alias tok ->
      let al = map_alias env tok in
      Left al
  | `Atom x ->
      let at = map_atom env x in
      Right at
  | `Id x ->
      (* less: map_identifier_or_ellipsis? *)
      let id = map_identifier env x in
      Left id
  | `Un_op x ->
      let op = map_unary_operator env x in
      Right op
  | `Dot x ->
      let d = map_dot env x in
      Right d
  | `Call_with_parens x ->
      let call = map_call_with_parentheses env x in
      Right call

and map_tuple (env : env) ((v1, v2, v3) : CST.tuple) =
  let l = (* "{" *) token env v1 in
  let xs =
    match v2 with
    | Some x -> map_items_with_trailing_separator env x
    | None -> []
  in
  let r = (* "}" *) token env v3 in
  (l, xs, r)

and map_unary_operator (env : env) (x : CST.unary_operator) : expr =
  match x with
  | `Opt_before_un_op_AMP_capt_exp (v1, v2, v3) ->
      let _v1 = map_before_unary_op_opt env v1 in
      (* TODO: fn shortcut syntax *)
      let tand = (* "&" *) token env v2 in
      let e = map_capture_expression env v3 in
      OtherExpr (("Shortcut", tand), [ E e ]) |> G.e
  | `Opt_before_un_op_choice_PLUS_exp (v1, v2, v3) ->
      let _v1 = map_before_unary_op_opt env v1 in
      let id = map_anon_choice_PLUS_8019319 env v2 in
      let e2 = map_expression env v3 in
      let e1 = N (H2.name_of_id id) |> G.e in
      Elixir_to_generic.mk_call_no_parens e1 [ G.arg e2 ] None
  | `Opt_before_un_op_AT_exp (v1, v2, v3) ->
      let _v1 = map_before_unary_op_opt env v1 in
      (* TODO: attributes *)
      let tat = (* "@" *) token env v2 in
      let e = map_expression env v3 in
      OtherExpr (("AttrExpr", tat), [ E e ]) |> G.e
  | `Opt_before_un_op_AMP_int (v1, v2, v3) ->
      let _v1 = map_before_unary_op_opt env v1 in
      (* TODO: fn shortcut syntax *)
      let tand = (* "&" *) token env v2 in
      let s, t = (* integer *) str env v3 in
      let e = L (Int (int_of_string_opt s, t)) |> G.e in
      OtherExpr (("Shortcut", tand), [ E e ]) |> G.e

let map_source (env : env) ((v1, v2) : CST.source) : body =
  let _v1 = map_terminator_opt env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2, v3) ->
        let v1 = map_expression env v1 in
        let v2 =
          Common.map
            (fun (v1, v2) ->
              let _v1 = map_terminator env v1 in
              let v2 = map_expression env v2 in
              v2)
            v2
        in
        let _v3 = map_terminator_opt env v3 in
        v1 :: v2
    | None -> []
  in
  v2

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_elixir.Parse.file file)
    (fun cst ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      let es = map_source env cst in
      Elixir_to_generic.body_to_stmts es)

let parse_pattern str =
  H.wrap_parser
    (fun () -> Tree_sitter_elixir.Parse.string str)
    (fun cst ->
      let file = "<pattern>" in
      let env = { H.file; conv = Hashtbl.create 0; extra = () } in
      let es = map_source env cst in
      G.Ss (Elixir_to_generic.body_to_stmts es))
