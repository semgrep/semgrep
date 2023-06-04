(* Yoann Padioleau
 *
 * Copyright (c) 2022-2023 Semgrep Inc.
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

(* TODO: remove! *)
[@@@warning "-26"]

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
(* Helpers *)
(*****************************************************************************)
type env = unit H.env

let token = H.token
let str = H.str
let _fb = Tok.unsafe_fake_bracket
let fk = Tok.unsafe_fake_tok

(* helper to factorize code *)
let map_trailing_comma env v1 : Tok.t option =
  match v1 with
  | Some tok -> Some ((* "," *) token env tok)
  | None -> None

let map_before_unary_op_opt env v1 =
  match v1 with
  | Some tok -> Some ((* before_unary_op *) token env tok)
  | None -> None

let args_of_exprs_and_keywords exprs kwds = (exprs, kwds)
let binary_call e1 op e2 = BinaryOp (e1, op, e2)
let expr_of_block blk = Block blk
let mk_call_parens _e _tdot _args _xxx = failwith "TODO"
let mk_call_no_parens _id_or_remote _args _blopt = failwith "TODO"
let items_of_exprs_and_keywords es kwds = (es, kwds)
let body_to_program xs = xs

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
    : operator wrap =
  match x with
  | `PLUS tok -> (* "+" *) (O G.Plus, token env tok)
  | `DASH tok -> (* "-" *) (O G.Minus, token env tok)
  | `BANG tok -> (* "!" *) (O G.Not, token env tok)
  | `HAT tok -> (* "^" *) (OPin, token env tok)
  | `TILDETILDETILDE tok -> (* "~~~" *) (OOther "~~~", token env tok)
  | `Not tok -> (* "not" *) (OStrictNot, token env tok)

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
          Id (str env tok)
      (* part of elixir! not a semgrep construct! *)
      | `DOTDOTDOT tok -> (* "..." *) IdEllipsis (token env tok))
  | `Semg_meta tok -> IdMetavar (str env tok)

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

let map_quoted_i_xxx f_map_interpolation (env : env) (v1, v2, v3) =
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
            Left (* quoted_content_i_angle *) (str env tok)
        | `Interp x ->
            let l, e, r = f_map_interpolation env x in
            Right (l, e, r)
        | `Esc_seq tok -> Left ((* escape_sequence *) str env tok))
      v2
  in
  let v3 = (* ">" or another one *) token env v3 in
  (v1, v2, v3)

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

let map_operator_identifier (env : env) (x : CST.operator_identifier) :
    operator wrap =
  match x with
  | `AMP tok -> (* "&" *) (OCapture, token env tok)
  | `Choice_PLUS x -> map_anon_choice_PLUS_8019319 env x
  | `AT tok -> (* "@" *) (OModuleAttr, token env tok)
  | `LTDASH tok -> (* "<-" *) (OLeftArrow, token env tok)
  | `BSLASHBSLASH tok -> (* "\\\\" *) (ODefault, token env tok)
  | `When tok -> (* "when" *) (OWhen, token env tok)
  | `COLONCOLON tok -> (* "::" *) (OType, token env tok)
  | `BAR tok -> (* "|" *) (OCons, token env tok)
  | `EQ tok -> (* "=" *) (OMatch, token env tok)
  | `BARBAR tok -> (* "||" *) (O G.Or, token env tok)
  | `BARBARBAR tok -> (* "|||" *) (OOther "|||", token env tok)
  | `Or tok -> (* "or" *) (OStrictOr, token env tok)
  | `AMPAMP tok -> (* "&&" *) (O G.And, token env tok)
  | `AMPAMPAMP tok -> (* "&&&" *) (OOther "&&&", token env tok)
  | `And tok -> (* "and" *) (OStrictAnd, token env tok)
  | `EQEQ tok -> (* "==" *) (O G.Eq, token env tok)
  | `BANGEQ tok -> (* "!=" *) (O G.NotEq, token env tok)
  | `EQTILDE tok -> (* "=~" *) (O G.RegexpMatch, token env tok)
  | `EQEQEQ tok -> (* "===" *) (O G.PhysEq, token env tok)
  | `BANGEQEQ tok -> (* "!==" *) (O G.NotPhysEq, token env tok)
  | `LT tok -> (* "<" *) (O G.Lt, token env tok)
  | `GT tok -> (* ">" *) (O G.Gt, token env tok)
  | `LTEQ tok -> (* "<=" *) (O G.LtE, token env tok)
  | `GTEQ tok -> (* ">=" *) (O G.GtE, token env tok)
  | `BARGT tok -> (* "|>" *) (OPipeline, token env tok)
  | `LTLTLT tok -> (* "<<<" *) (O G.LSL, token env tok)
  | `GTGTGT tok -> (* ">>>" *) (O G.LSR, token env tok)
  | `LTLTTILDE tok -> (* "<<~" *) (OOther "<<~", token env tok)
  | `TILDEGTGT tok -> (* "~>>" *) (OOther "~>>", token env tok)
  | `LTTILDE tok -> (* "<~" *) (OOther "<~", token env tok)
  | `TILDEGT tok -> (* "~>" *) (OOther "~>", token env tok)
  | `LTTILDEGT tok -> (* "<~>" *) (OOther "<~>", token env tok)
  | `LTBARGT tok -> (* "<|>" *) (OOther "<|>", token env tok)
  | `In tok -> (* "in" *) (O G.In, token env tok)
  | `Not_in tok -> (* not_in *) (O G.NotIn, token env tok)
  | `HATHAT tok -> (* "^^" *) (OOther "^^", token env tok)
  | `PLUSPLUS tok -> (* "++" *) (OOther "++", token env tok)
  | `DASHDASH tok -> (* "--" *) (OOther "--", token env tok)
  | `PLUSPLUSPLUS tok -> (* "+++" *) (OOther "+++", token env tok)
  | `DASHDASHDASH tok -> (* "---" *) (OOther "---", token env tok)
  | `DOTDOT tok -> (* ".." *) (O G.Range, token env tok)
  | `LTGT tok -> (* "<>" *) (OOther "<>", token env tok)
  | `STAR tok -> (* "*" *) (O G.Mult, token env tok)
  | `SLASH tok -> (* "/" *) (O G.Div, token env tok)
  | `STARSTAR tok -> (* "**" *) (O G.Pow, token env tok)
  | `DASHGT tok -> (* "->" *) (ORightArrow, token env tok)
  | `DOT tok -> (* "." *) (ODot, token env tok)

let rec map_after_block (env : env) ((v1, v2, v3) : CST.after_block) :
    exn_clause_kind wrap * body_or_clauses =
  let v1 = (After, (* "after" *) token env v1) in
  let _v2 = map_terminator_opt env v2 in
  let v3 =
    match v3 with
    | Some x ->
        map_anon_choice_choice_stab_clause_rep_term_choice_stab_clause_b295119
          env x
    | None -> Clauses []
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
      Clauses (v1 :: v2)
  | `Choice_exp_rep_term_choice_exp_opt_term (v1, v2, v3) ->
      let v1 =
        match v1 with
        | `Exp x -> map_expression env x
      in
      let v2 = Common.map (map_anon_term_choice_exp_996111b env) v2 in
      let _v3 = map_terminator_opt env v3 in
      Body (v1 :: v2)

and map_anon_choice_exp_0094635 (env : env) (x : CST.anon_choice_exp_0094635) :
    expr_or_kwds =
  match x with
  | `Exp x ->
      let e = map_expression env x in
      E e
  | `Keywos x ->
      let xs = map_keywords env x in
      Kwds xs

and map_anon_choice_quoted_i_double_d7d5f65 (env : env)
    (x : CST.anon_choice_quoted_i_double_d7d5f65) : quoted =
  match x with
  | `Quoted_i_double x -> map_quoted_i_double env x
  | `Quoted_i_single x -> map_quoted_i_single env x

and map_anon_exp_rep_COMMA_exp_opt_COMMA_keywos_041d82e (env : env)
    ((v1, v2, v3) : CST.anon_exp_rep_COMMA_exp_opt_COMMA_keywos_041d82e) :
    arguments =
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
  args_of_exprs_and_keywords (v1 :: v2) v3

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
  mk_call_parens e (Some tdot) args None

and map_anonymous_dot (env : env) ((v1, v2) : CST.anonymous_dot) =
  let v1 = map_expression env v1 in
  let v2 = (* "." *) token env v2 in
  (v1, v2)

and map_atom (env : env) (x : CST.atom) : atom =
  match x with
  | `Atom_ tok ->
      (* less: should remove the leading ':' from x *)
      let x = (* atom_ *) str env tok in
      (fk ":", X x)
  | `Quoted_atom (v1, v2) ->
      let t = (* quoted_atom_start *) token env v1 in
      let x = map_anon_choice_quoted_i_double_d7d5f65 env v2 in
      (t, Quoted x)

and map_binary_operator (env : env) (x : CST.binary_operator) : expr =
  match x with
  | `Exp_choice_LTDASH_exp (v1, v2, v3) -> (
      let v1 = map_expression env v1 in
      let v3 = map_expression env v3 in
      match v2 with
      | `LTDASH tok ->
          let t = (* "<-" *) token env tok in
          binary_call v1 (OLeftArrow, t) v3
      (* default parameter syntax *)
      | `BSLASHBSLASH tok ->
          let t = (* "\\\\" *) token env tok in
          binary_call v1 (ODefault, t) v3)
  | `Exp_when_choice_exp (v1, v2, v3) ->
      let e1 = map_expression env v1 in
      let twhen = (* "when" *) token env v2 in
      let e_or_kwds = map_anon_choice_exp_0094635 env v3 in
      When (e1, twhen, e_or_kwds)
  | `Exp_COLONCOLON_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "::" *) token env v2 in
      let v3 = map_expression env v3 in
      binary_call v1 (OType, v2) v3
  | `Exp_BAR_choice_exp (v1, v2, v3) ->
      let e1 = map_expression env v1 in
      (* join operator (=~ "::" in OCaml, comes from Prolog/Erlang) *)
      let tbar = (* "|" *) token env v2 in
      let e_or_kwds = map_anon_choice_exp_0094635 env v3 in
      Join (e1, tbar, e_or_kwds)
  | `Exp_EQGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      (* less: used in Maps, should convert in 'pair'? *)
      let v2 = (* "=>" *) token env v2 in
      let v3 = map_expression env v3 in
      binary_call v1 (ORightArrow, v2) v3
  | `Exp_EQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      binary_call v1 (OMatch, v2) v3
  | `Exp_choice_BARBAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `BARBAR tok -> (O G.Or, (* "||" *) token env tok)
        | `BARBARBAR tok -> (OOther "|||", (* "|||" *) token env tok)
        | `Or tok -> (OStrictOr, (* "or" *) token env tok)
      in
      let v3 = map_expression env v3 in
      binary_call v1 v2 v3
  | `Exp_choice_AMPAMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `AMPAMP tok -> (O G.And, (* "&&" *) token env tok)
        | `AMPAMPAMP tok -> (OOther "&&&", (* "&&&" *) token env tok)
        | `And tok -> (OStrictAnd, (* "and" *) token env tok)
      in
      let v3 = map_expression env v3 in
      binary_call v1 v2 v3
  | `Exp_choice_EQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `EQEQ tok -> (O G.Eq, (* "==" *) token env tok)
        | `BANGEQ tok -> (O G.NotEq, (* "!=" *) token env tok)
        | `EQTILDE tok -> (O G.RegexpMatch, (* "=~" *) token env tok)
        | `EQEQEQ tok -> (O G.PhysEq, (* "===" *) token env tok)
        | `BANGEQEQ tok -> (O G.NotPhysEq, (* "!==" *) token env tok)
      in
      let v3 = map_expression env v3 in
      binary_call v1 v2 v3
  | `Exp_choice_LT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `LT tok -> (* "<" *) (O G.Lt, token env tok)
        | `GT tok -> (* ">" *) (O G.Gt, token env tok)
        | `LTEQ tok -> (* "<=" *) (O G.LtE, token env tok)
        | `GTEQ tok -> (* ">=" *) (O G.GtE, token env tok)
      in
      let v3 = map_expression env v3 in
      binary_call v1 v2 v3
  | `Exp_choice_BARGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `BARGT tok -> (* "|>" *) (OPipeline, token env tok)
        | `LTLTLT tok -> (* "<<<" *) (OOther "<<<", token env tok)
        | `GTGTGT tok -> (* ">>>" *) (OOther ">>>", token env tok)
        | `LTLTTILDE tok -> (* "<<~" *) (OOther "<<~", token env tok)
        | `TILDEGTGT tok -> (* "~>>" *) (OOther "~>>", token env tok)
        | `LTTILDE tok -> (* "<~" *) (OOther "<~", token env tok)
        | `TILDEGT tok -> (* "~>" *) (OOther "~>", token env tok)
        | `LTTILDEGT tok -> (* "<~>" *) (OOther "<~>", token env tok)
        | `LTBARGT tok -> (* "<|>" *) (OOther "<|>", token env tok)
      in
      let v3 = map_expression env v3 in
      binary_call v1 v2 v3
  | `Exp_choice_in_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `In tok -> (* "in" *) (O G.In, token env tok)
        | `Not_in tok -> (* not_in *) (O G.NotIn, token env tok)
      in
      let v3 = map_expression env v3 in
      binary_call v1 v2 v3
  | `Exp_HATHATHAT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (OOther "^^^", (* "^^^" *) token env v2) in
      let v3 = map_expression env v3 in
      binary_call v1 v2 v3
  | `Exp_SLASHSLASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (OOther "//", (* "//" *) token env v2) in
      let v3 = map_expression env v3 in
      binary_call v1 v2 v3
  | `Exp_choice_PLUSPLUS_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `PLUSPLUS tok -> (OOther "++", (* "++" *) token env tok)
        | `DASHDASH tok -> (OOther "--", (* "--" *) token env tok)
        | `PLUSPLUSPLUS tok -> (OOther "+++", (* "+++" *) token env tok)
        | `DASHDASHDASH tok -> (OOther "---", (* "---" *) token env tok)
        | `DOTDOT tok -> (O G.Range, (* ".." *) token env tok)
        | `LTGT tok -> (OOther "<>", (* "<>" *) token env tok)
      in
      let v3 = map_expression env v3 in
      binary_call v1 v2 v3
  | `Exp_choice_PLUS_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `PLUS tok -> (O G.Plus, (* "+" *) token env tok)
        | `DASH tok -> (O G.Minus, (* "-" *) token env tok)
      in
      let v3 = map_expression env v3 in
      binary_call v1 v2 v3
  | `Exp_choice_STAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `STAR tok -> (O G.Mult, (* "*" *) token env tok)
        | `SLASH tok -> (O G.Div, (* "/" *) token env tok)
      in
      let v3 = map_expression env v3 in
      binary_call v1 v2 v3
  | `Exp_STARSTAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "**" *) (O G.Pow, token env v2) in
      let v3 = map_expression env v3 in
      binary_call v1 v2 v3
  | `Op_id_SLASH_int (v1, v2, v3) ->
      let op = map_operator_identifier env v1 in
      let tslash = (* "/" *) token env v2 in
      let s, t = (* integer *) str env v3 in
      OpArity (op, tslash, (int_of_string_opt s, t))

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
    ((v1, v2, v3) : CST.call_arguments_with_parentheses) : arguments bracket =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> map_call_arguments_with_trailing_separator env x
    | None -> ([], [])
  in
  let v3 = (* ")" *) token env v3 in
  (v1, v2, v3)

and map_call_arguments_with_parentheses_immediate (env : env)
    ((v1, v2, v3) : CST.call_arguments_with_parentheses_immediate) :
    arguments bracket =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> map_call_arguments_with_trailing_separator env x
    | None -> ([], [])
  in
  let v3 = (* ")" *) token env v3 in
  (v1, v2, v3)

and map_call_arguments_with_trailing_separator (env : env)
    (x : CST.call_arguments_with_trailing_separator) : arguments =
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
      args_of_exprs_and_keywords (v1 :: v2) v3
  | `Keywos_with_trai_sepa x ->
      let xs = map_keywords_with_trailing_separator env x in
      args_of_exprs_and_keywords [] xs

and map_call_arguments_without_parentheses (env : env)
    (x : CST.call_arguments_without_parentheses) : arguments =
  match x with
  | `Exp_rep_COMMA_exp_opt_COMMA_keywos x ->
      map_anon_exp_rep_COMMA_exp_opt_COMMA_keywos_041d82e env x
  | `Keywos x ->
      let xs = map_keywords env x in
      args_of_exprs_and_keywords [] xs

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
      mk_call_parens call1 None args blopt

and map_call_without_parentheses (env : env) (x : CST.call_without_parentheses)
    : call =
  match x with
  | `Local_call_with_parens (v1, v2, v3) ->
      let id = map_identifier env v1 in
      let args = map_call_arguments_without_parentheses env v2 in
      let blopt = map_anon_opt_opt_nl_before_do_do_blk_3eff85f env v3 in
      mk_call_no_parens (Left id) args blopt
  | `Local_call_just_do_blk (v1, v2) ->
      let id = map_identifier env v1 in
      let bl = map_do_block env v2 in
      mk_call_no_parens (Left id) [] (Some bl)
  | `Remote_call_with_parens (v1, v2, v3) ->
      let rdot = map_remote_dot env v1 in
      let args : arguments =
        match v2 with
        | Some x -> map_call_arguments_without_parentheses env x
        | None -> ([], [])
      in
      let blopt = map_anon_opt_opt_nl_before_do_do_blk_3eff85f env v3 in
      mk_call_no_parens (Right rdot) args blopt

and map_capture_expression (env : env) (x : CST.capture_expression) =
  match x with
  | `LPAR_exp_RPAR (v1, v2, v3) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* ")" *) token env v3 in
      fun tand -> ShortLambda (tand, (v1, v2, v3))
  | `Exp x ->
      let e = map_expression env x in
      fun tand -> Capture (tand, e)

and map_catch_block (env : env) ((v1, v2, v3) : CST.catch_block) =
  let v1 = (Catch, (* "catch" *) token env v1) in
  let _v2 = map_terminator_opt env v2 in
  let v3 =
    match v3 with
    | Some x ->
        map_anon_choice_choice_stab_clause_rep_term_choice_stab_clause_b295119
          env x
    | None -> Clauses []
  in
  (v1, v3)

and map_charlist (env : env) (x : CST.charlist) =
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
    | None -> Clauses []
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
  match v3 with
  | `Alias tok ->
      let al = map_alias env tok in
      DotAlias (e, tdot, al)
  | `Tuple x ->
      let l, xs, r = map_tuple env x in
      DotTuple (e, tdot, (l, xs, r))

and map_else_block (env : env) ((v1, v2, v3) : CST.else_block) =
  let v1 = (Else, (* "else" *) token env v1) in
  let _v2 = map_terminator_opt env v2 in
  let v3 =
    match v3 with
    | Some x ->
        map_anon_choice_choice_stab_clause_rep_term_choice_stab_clause_b295119
          env x
    | None -> Clauses []
  in
  (v1, v3)

and map_expression (env : env) (x : CST.expression) : expr =
  match x with
  (* semgrep-ext: *)
  | `Deep_ellips (v1, v2, v3) ->
      let l = token env v1 in
      let e = map_expression env v2 in
      let r = token env v3 in
      DeepEllipsis (l, e, r)
  | `Blk (v1, v2, v3, v4) ->
      let l = (* "(" *) token env v1 in
      let _ = map_terminator_opt env v2 in
      let xs =
        match v3 with
        | Some x ->
            map_anon_choice_choice_stab_clause_rep_term_choice_stab_clause_b295119
              env x
        | None -> Clauses []
      in
      let r = (* ")" *) token env v4 in
      let blk : block = (l, xs, r) in
      expr_of_block blk
  | `Id x ->
      let id = map_identifier env x in
      I id
  | `Alias x ->
      let al = map_alias env x in
      Alias al
  | `Int tok ->
      let s, t = (* integer *) str env tok in
      L (G.Int (int_of_string_opt s, t))
  | `Float tok ->
      let s, t = (* float *) str env tok in
      L (G.Float (float_of_string_opt s, t))
  | `Char_a87deb0 tok ->
      let x = (* pattern \?(.|\\.) *) str env tok in
      L (G.Char x)
  | `Bool x ->
      let b = map_boolean env x in
      L (G.Bool b)
  | `Nil tok ->
      let nil = (* "nil" *) token env tok in
      L (G.Null nil)
  | `Atom x ->
      let a = map_atom env x in
      A a
  | `Str x ->
      let x = map_string_ env x in
      String x
  | `Char_a593f90 x ->
      let x = map_charlist env x in
      Charlist x
  | `Sigil (v1, v2, v3) ->
      let ttilde = (* "~" *) token env v1 in
      let sigil_kind =
        match v2 with
        | `Imm_tok_pat_0db2d54_choice_quoted_i_double (v1, v2) ->
            let lower, t = (* pattern [a-z] *) str env v1 in
            let quoted : quoted =
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
            Lower ((String.get lower 0, t), quoted)
        | `Imm_tok_pat_562b724_choice_quoted_double (v1, v2) ->
            let upper, t = (* pattern [A-Z] *) str env v1 in
            let str : string wrap bracket =
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
            Upper ((String.get upper 0, t), str)
      in
      let idopt =
        match v3 with
        | Some tok -> Some ((* pattern [a-zA-Z0-9]+ *) str env tok)
        | None -> None
      in
      Sigil (ttilde, sigil_kind, idopt)
  | `List (v1, v2, v3) ->
      let l = (* "[" *) token env v1 in
      let xs =
        match v2 with
        | Some x -> map_items_with_trailing_separator env x
        | None -> ([], [])
      in
      let r = (* "]" *) token env v3 in
      List (l, xs, r)
  | `Tuple x ->
      let l, xs, r = map_tuple env x in
      Tuple (l, xs, r)
  | `Bits (v1, v2, v3) ->
      let l = (* "<<" *) token env v1 in
      let xs =
        match v2 with
        | Some x -> map_items_with_trailing_separator env x
        | None -> ([], [])
      in
      let r = (* ">>" *) token env v3 in
      Bits (l, xs, r)
  | `Map (v1, v2, v3, v4, v5) ->
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
        | None -> ([], [])
      in
      let r = (* "}" *) token env v5 in
      Map (tpercent, struct_opt, (l, xs, r))
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
      ArrayAccess (v1, (v2, v3, v4))
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
      let tend = (* "end" *) token env v5 in
      let clauses = x :: xs in
      Lambda (tfn, clauses, tend)

and map_interpolation (env : env) ((v1, v2, v3) : CST.interpolation) :
    expr bracket =
  let v1 = (* "#{" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* "}" *) token env v3 in
  (v1, v2, v3)

and map_items_with_trailing_separator (env : env)
    (v1 : CST.items_with_trailing_separator) : items =
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
      items_of_exprs_and_keywords (v1 :: v2) []
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
      items_of_exprs_and_keywords v1 xs

and map_keyword (env : env) (x : CST.keyword) : keyword =
  match x with
  | `Kw_ tok ->
      (* todo: remove suffix ':' *)
      let x = (* keyword_ *) str env tok in
      (X x, fk ":")
  | `Quoted_kw (v1, v2) ->
      let v1 = map_anon_choice_quoted_i_double_d7d5f65 env v1 in
      let tcolon = (* pattern :\s *) token env v2 in
      (Quoted v1, tcolon)

and map_keywords (env : env) ((v1, v2) : CST.keywords) : keywords =
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
    ((v1, v2, v3) : CST.local_call_with_parentheses) : call =
  let id = map_identifier env v1 in
  let args = map_call_arguments_with_parentheses_immediate env v2 in
  let blopt = map_anon_opt_opt_nl_before_do_do_blk_3eff85f env v3 in
  mk_call_parens (I id) None args blopt

and map_pair (env : env) ((v1, v2) : CST.pair) : pair =
  let v1 = map_keyword env v1 in
  let v2 = map_expression env v2 in
  (v1, v2)

and map_quoted_i_angle (env : env) ((v1, v2, v3) : CST.quoted_i_angle) =
  let x = map_quoted_i_xxx map_interpolation env (v1, v2, v3) in
  failwith "TODO"

and map_quoted_i_bar (env : env) ((v1, v2, v3) : CST.quoted_i_bar) =
  let x = map_quoted_i_xxx map_interpolation env (v1, v2, v3) in
  failwith "TODO"

and map_quoted_i_curly (env : env) ((v1, v2, v3) : CST.quoted_i_curly) =
  let x = map_quoted_i_xxx map_interpolation env (v1, v2, v3) in
  failwith "TODO"

and map_quoted_i_double (env : env) ((v1, v2, v3) : CST.quoted_i_double) =
  let x = map_quoted_i_xxx map_interpolation env (v1, v2, v3) in
  failwith "TODO"

and map_quoted_i_heredoc_double (env : env)
    ((v1, v2, v3) : CST.quoted_i_heredoc_double) =
  let x = map_quoted_i_xxx map_interpolation env (v1, v2, v3) in
  failwith "TODO"

and map_quoted_i_heredoc_single (env : env)
    ((v1, v2, v3) : CST.quoted_i_heredoc_single) =
  let x = map_quoted_i_xxx map_interpolation env (v1, v2, v3) in
  failwith "TODO"

and map_quoted_i_parenthesis (env : env)
    ((v1, v2, v3) : CST.quoted_i_parenthesis) =
  let x = map_quoted_i_xxx map_interpolation env (v1, v2, v3) in
  failwith "TODO"

and map_quoted_i_single (env : env) ((v1, v2, v3) : CST.quoted_i_single) =
  let x = map_quoted_i_xxx map_interpolation env (v1, v2, v3) in
  failwith "TODO"

and map_quoted_i_slash (env : env) ((v1, v2, v3) : CST.quoted_i_slash) =
  let x = map_quoted_i_xxx map_interpolation env (v1, v2, v3) in
  failwith "TODO"

and map_quoted_i_square (env : env) ((v1, v2, v3) : CST.quoted_i_square) =
  let x = map_quoted_i_xxx map_interpolation env (v1, v2, v3) in
  failwith "TODO"

and map_remote_call_with_parentheses (env : env)
    ((v1, v2, v3) : CST.remote_call_with_parentheses) : call =
  let e, tdot, fld = map_remote_dot env v1 in
  let args = map_call_arguments_with_parentheses_immediate env v2 in
  let blopt = map_anon_opt_opt_nl_before_do_do_blk_3eff85f env v3 in
  (* Elixir_to_generic.mk_call_parens e args blopt *)
  failwith "TODO"

and map_remote_dot (env : env) ((v1, v2, v3) : CST.remote_dot) : remote_dot =
  let e = map_expression env v1 in
  let tdot = (* "." *) token env v2 in
  let fld : ident_or_operator or_quoted =
    match v3 with
    | `Id x ->
        let id = map_identifier env x in
        X (Left id)
    (* keywords are ok in a remote_dot context, no ambiguity *)
    | `Choice_and x ->
        X
          (Left
             (Id
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
                | `Rescue tok -> (* "rescue" *) str env tok)))
    | `Op_id x ->
        let id = map_operator_identifier env x in
        X (Right id)
    | `Quoted_i_double x ->
        (* TODO: could detect of the map_quoted_i_xxx is a constant string *)
        let x = map_quoted_i_double env x in
        Quoted x
    | `Quoted_i_single x ->
        let x = map_quoted_i_single env x in
        Quoted x
  in
  (e, tdot, fld)

and map_rescue_block (env : env) ((v1, v2, v3) : CST.rescue_block) =
  let v1 = (Rescue (* "rescue" *), token env v1) in
  let _v2 = map_terminator_opt env v2 in
  let v3 =
    match v3 with
    | Some x ->
        map_anon_choice_choice_stab_clause_rep_term_choice_stab_clause_b295119
          env x
    | None -> Clauses []
  in
  (v1, v3)

and map_stab_clause (env : env) ((v1, v2, v3) : CST.stab_clause) : stab_clause =
  let v1 =
    match v1 with
    | Some x -> map_stab_clause_left env x
    | None -> (([], []), None)
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
    arguments bracket =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> map_stab_clause_arguments_without_parentheses env x
    | None -> ([], [])
  in
  let v3 = (* ")" *) token env v3 in
  (v1, v2, v3)

and map_stab_clause_arguments_without_parentheses (env : env)
    (x : CST.stab_clause_arguments_without_parentheses) : arguments =
  match x with
  | `Exp_rep_COMMA_exp_opt_COMMA_keywos x ->
      let xs = map_anon_exp_rep_COMMA_exp_opt_COMMA_keywos_041d82e env x in
      xs
  | `Keywos x ->
      let xs = map_keywords env x in
      args_of_exprs_and_keywords [] xs

(* we would prefer pattern list, but elixir is more general *)
and map_stab_clause_left (env : env) (x : CST.stab_clause_left) :
    arguments * (Tok.t * expr) option =
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

and map_string_ (env : env) (x : CST.string_) : quoted =
  match x with
  | `Quoted_i_double x -> map_quoted_i_double env x
  | `Quoted_i_here_double x -> map_quoted_i_heredoc_double env x

and map_struct_ (env : env) (x : CST.struct_) : struct_ =
  match x with
  | `Alias tok ->
      let al = map_alias env tok in
      Alias al
  | `Atom x ->
      let at = map_atom env x in
      A at
  | `Id x ->
      let id = map_identifier env x in
      I id
  | `Un_op x -> map_unary_operator env x
  | `Dot x -> map_dot env x
  | `Call_with_parens x ->
      let call = map_call_with_parentheses env x in
      Call call

and map_tuple (env : env) ((v1, v2, v3) : CST.tuple) : items bracket =
  let l = (* "{" *) token env v1 in
  let xs =
    match v2 with
    | Some x -> map_items_with_trailing_separator env x
    | None -> ([], [])
  in
  let r = (* "}" *) token env v3 in
  (l, xs, r)

and map_unary_operator (env : env) (x : CST.unary_operator) : expr =
  match x with
  | `Opt_before_un_op_AMP_capt_exp (v1, v2, v3) ->
      let _v1 = map_before_unary_op_opt env v1 in
      (* TODO: fn shortcut syntax *)
      let tand = (* "&" *) token env v2 in
      let f = map_capture_expression env v3 in
      f tand
  | `Opt_before_un_op_choice_PLUS_exp (v1, v2, v3) ->
      let _v1 = map_before_unary_op_opt env v1 in
      let op = map_anon_choice_PLUS_8019319 env v2 in
      let e = map_expression env v3 in
      UnaryOp (op, e)
  | `Opt_before_un_op_AT_exp (v1, v2, v3) ->
      let _v1 = map_before_unary_op_opt env v1 in
      let tat = (* "@" *) token env v2 in
      let e = map_expression env v3 in
      ModuleVarAccess (tat, e)
  | `Opt_before_un_op_AMP_int (v1, v2, v3) ->
      let _v1 = map_before_unary_op_opt env v1 in
      let tand = (* "&" *) token env v2 in
      let s, t = (* integer *) str env v3 in
      let e = (int_of_string_opt s, t) in
      PlaceHolder (tand, e)

let map_source (env : env) ((v1, v2) : CST.source) : program =
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
      body_to_program es)

let parse_pattern str =
  H.wrap_parser
    (fun () -> Tree_sitter_elixir.Parse.string str)
    (fun cst ->
      let file = "<pattern>" in
      let env = { H.file; conv = Hashtbl.create 0; extra = () } in
      let es = map_source env cst in
      Pr (body_to_program es))
